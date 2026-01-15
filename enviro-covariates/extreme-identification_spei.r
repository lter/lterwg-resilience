## --------------------------------------------------------------- ##
# Climate Extreme Identification (SPEI)
## --------------------------------------------------------------- ##

# Load needed libraries
librarian::shelf(tidyverse, googledrive, supportR)

# Make needed folder(s)
dir.create(file.path("data", "environment"), showWarnings = F, recursive = T)
dir.create(file.path("data", "harmonized_data"), showWarnings = F, recursive = T)

# Clear environment
rm(list = ls()); gc()

## ------------------------------------- ##
# Download SPEI Data ----
## ------------------------------------- ##
# Created by "exploratory/download_SPEI-site.R"

# Identify files in drive
drive_spei <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1JtFMD4IAizjNGd0wbLLZIBdgqnk4YR97")) %>% 
  dplyr::filter(stringr::str_detect(string = name, pattern = ".csv"))

# Check that worked
drive_spei

# Download locally
## Uncomment to re-download
# purrr::walk2(.x = drive_spei$id, .y = drive_spei$name,
#              .f = ~ googledrive::drive_download(file = .x, overwrite = T,
#                                                 path = file.path("data", "environment", .y)))

## ------------------------------------- ##
# Preparatory SPEI Wrangling ----
## ------------------------------------- ##

# Identify desired SPEI file
spei_file <- "SPEI03.csv"

# Read in one of those files
spei_v1 <- read.csv(file.path("data", "environment", spei_file))

# Check structure
dplyr::glimpse(spei_v1)

# Do some wrangling to this object
spei_v2 <- spei_v1 %>% 
  # Rotate to long format
  tidyr::pivot_longer(cols = -date, names_to = "site", values_to = "SPEI") %>% 
  # Drop missing values
  dplyr::filter(!is.na(SPEI)) %>% 
  # Make sure all dates have the same day number (matters for a conditional in 'diff_windows')
  dplyr::mutate(date = gsub(pattern = "-15", replacement = "-16", x = date)) %>% 
  # Make dates 'real' dates
  dplyr::mutate(date = as.Date(date))

# Re-check structure
dplyr::glimpse(spei_v2)

## ------------------------------------- ##
# Identify Extremes Events ----
## ------------------------------------- ##

# Make a list for storing outputs
extreme_list <- list()

# What percentile counts as 'extreme'?
ext_perc <- 0.994

# Iterate across sites in data
for(focal_site in sort(unique(spei_v2$site))){
  # focal_site <- "CPER"
  
  # Progress message
  message("Identifying whiplash events for '", focal_site, "'")
  
  # Subset data to only focal site
  spei_sub <- dplyr::filter(spei_v2, site == focal_site)

  # Identify the extreme(s)
  ext_df <- spei_sub %>% 
    dplyr::mutate(
      # Single extreme events
      single.extreme = ifelse(SPEI > as.numeric(quantile(x = SPEI, probs = ext_perc)),
        yes = T, no = F),
      # Double extreme events
      double.extreme = ifelse(single.extreme == T & dplyr::lag(single.extreme) == T,
        yes = T, no = F),
      # Diagnostic
      timestep.difference = as.numeric(date) - as.numeric(dplyr::lag(date)))

  # Subset to only extreme events
  ext_only <- ext_df %>% 
    dplyr::filter(single.extreme == T | double.extreme == T)
  
  # Add to the list
  extreme_list[[focal_site]] <- ext_df }

# Unlist the list and do minor wrangling
extreme_v01 <- extreme_list %>% 
  purrr::list_rbind(x = .)

# Check structure of last iteration of loop
dplyr::glimpse(extreme_v01)

## ------------------------------------- ##
# Export ----
## ------------------------------------- ##

# Make a final object
extreme_v99 <- extreme_v01

# Check structure
dplyr::glimpse(extreme_v99)

# Check out the extreme events
extreme_v99 %>% 
  dplyr::filter(single.extreme == T | double.extreme == T)

# Export locally
write.csv(x = extreme_v99, na = '', row.names = F,
  file = file.path("data", "environment", "climate-extremes_spei.csv"))

# End ----
