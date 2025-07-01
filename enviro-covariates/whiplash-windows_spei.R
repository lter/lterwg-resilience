## --------------------------------------------------------------- ##
# Whiplash Moving Windows Analysis Workflow
## --------------------------------------------------------------- ##
# Script author(s): Nick J Lyon, 

# Purpose
## Perform whiplash moving windows analysis à la Swain et al. 2025
## Paper linked here: https://escholarship.org/uc/item/9t78v9sr
## Note there are some deviations but the workflow is inspired by them

## ------------------------------------- ##
# Housekeeping ----
## ------------------------------------- ##

# Load needed libraries
librarian::shelf(tidyverse, googledrive, supportR)

# Make needed folder(s)
dir.create(file.path("data", "environment"), showWarnings = F, recursive = T)
dir.create(file.path("data", "harmonized_data"), showWarnings = F, recursive = T)
dir.create(file.path("data", "diagnostic", "whiplash"), showWarnings = F, recursive = T)
dir.create(file.path("graphs", "diagnostic", "whiplash"), showWarnings = F, recursive = T)

# Clear environment
rm(list = ls()); gc()

# Load needed custom function(s)
purrr::walk(.x = dir(path = file.path("tools"), pattern = "fxn_"),
            .f = ~ source(file.path("tools", .x)))

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
# Download ANPP Data ----
## ------------------------------------- ##

# This is non-critical to actual whiplash identification
## But will be nice context for the diagnostic plots

drive_anpp <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/13Ymkrr-kRLDmpaj1jwwVOnOSmEYnF-dJ")) %>% 
  dplyr::filter(stringr::str_detect(string = name, pattern = ".csv")) %>% 
  dplyr::filter(name == "02_anpp_filter1.csv")

# Check that worked
drive_anpp

# Download just the relevant file
## Uncomment to re-download
# googledrive::drive_download(file = drive_anpp$id, overwrite = T,
#                             path = file.path("data", "harmonized_data", drive_anpp$name))

## ------------------------------------- ##
# Preparatory ANPP Wrangling ----
## ------------------------------------- ##

# Read that in
anpp_v1 <- read.csv(file.path("data", "harmonized_data", drive_anpp$name))

# Check structure
dplyr::glimpse(anpp_v1)

# For this script, we only want the max/min years
anpp_extent <- c(min(anpp_v1$year, na.rm = T),
                 max(anpp_v1$year, na.rm = T))

# Check that out
anpp_extent

## ------------------------------------- ##
# Identify Whiplash Events ----
## ------------------------------------- ##

# Iterate across sites in data
for(focal_site in sort(unique(spei_v2$site))){
  # focal_site <- "CPER"
  
  # Progress message
  message("Identifying whiplash events for '", focal_site, "'")
  
  # Subset data to only focal site
  spei_sub <- dplyr::filter(spei_v2, site == focal_site)
  
  # Identify whiplash events
  whiplash_df <- id_whiplash(df = spei_sub, date_col = "date", enviro_col = "SPEI",
                             window_size = 3, ref_period = c(1940, 1980),
                             whiplash_perc = c(0.006, 0.994), quiet = FALSE) %>% 
    # Add a column for which site this data is from
    dplyr::mutate(site = focal_site, .before = dplyr::everything())

  # Export this locally  
  write.csv(x = whiplash_df, row.names = F, na = '',
            file = file.path("data", "diagnostic", "whiplash", 
                             paste0(focal_site, "_whiplash-data.csv")))
  
  # Get some simpler dataframes for ease of diagnostic plotting
  ## Whiplashes only
  whiplash_only <- dplyr::filter(whiplash_df, !is.na(whiplash))
  ## Data that are not whiplashes
  nonwhip_only <- dplyr::filter(whiplash_df, is.na(whiplash))
  
  # Make a graph of the distribution of differences + whiplash events
  ggplot() +
    geom_violin(data = whiplash_df, aes(y = SPEI_diff, x = site)) +
    geom_jitter(data = nonwhip_only, aes(y = SPEI_diff, x = site), 
                width = 0.2, alpha = 0.3) +
    geom_jitter(data = whiplash_only, aes(y = SPEI_diff, x = site), 
                color = "red", width = 0.2) +
    geom_hline(yintercept = unique(whiplash_df$whiplash_thresh_upper), linetype = 2, 
               color = "blue", linewidth = 0.5) +
    geom_hline(yintercept = unique(whiplash_df$whiplash_thresh_lower), linetype = 2, 
               color = "blue", linewidth = 0.5) +
    labs(x = "Site", y = "SPEI Differences") +
    supportR::theme_lyon()
    
  # Export this locally
  ggsave(filename = file.path("graphs", "diagnostic", "whiplash", 
                              paste0(focal_site, "_whiplash-violin.png")),
         width = 5, height = 5, units = "in")
  
  # Now make a scatterplot of the differences over time
  ggplot(whiplash_df, aes(x = date, y = SPEI_diff)) +
    geom_point() + 
    geom_point(data = whiplash_only, aes(x = date, y = SPEI_diff), color = "red") +
    geom_hline(yintercept = unique(whiplash_df$whiplash_thresh_upper), linetype = 2, 
               color = "blue", linewidth = 0.5) +
    geom_hline(yintercept = unique(whiplash_df$whiplash_thresh_lower), linetype = 2, 
               color = "blue", linewidth = 0.5) +
    # Annotate ANPP coverage
    annotate("rect", ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "gray45",
             xmin = as.Date(x = paste0(min(anpp_extent), "-01-01")),
             xmax = as.Date(x = paste0(max(anpp_extent), "-12-31"))) +
    geom_text(label = "ANPP Coverage", size = 4, 
              y = max(whiplash_df$SPEI_diff) - (max(whiplash_df$SPEI_diff) * 0.025),
              x = as.Date(paste0(ceiling((max(anpp_extent) - min(anpp_extent)) / 2) + min(anpp_extent), "-01-01"))) +
    # Final cosmetic stuff
    labs(x = "Date", y = "SPEI Differences") +
    supportR::theme_lyon()
  
  # Export that locally
  ggsave(filename = file.path("graphs", "diagnostic", "whiplash", 
                              paste0(focal_site, "_whiplash-scatter.png")),
         width = 7, height = 5, units = "in")
  
  # Now make a time series of the actual data (i.e., not the differences)
  ggplot(whiplash_df, aes(x = date, y = SPEI)) +
    geom_path() +
    geom_point(size = 0.5, color = "black") + 
    geom_point(data = whiplash_only, aes(x = date, y = SPEI),
               color = "red", size = 2) +
    # geom_point(data = whiplash_only, aes(x = prior_date, y = prior_SPEI),
    #            color = "blue", size = 2) +
    annotate("rect", ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "gray45",
             xmin = as.Date(x = paste0(min(anpp_extent), "-01-01")),
             xmax = as.Date(x = paste0(max(anpp_extent), "-12-31"))) +
    geom_text(label = "ANPP Coverage", size = 4, 
              y = max(whiplash_df$SPEI) - (max(whiplash_df$SPEI) * 0.005),
              x = as.Date(paste0(ceiling((max(anpp_extent) - min(anpp_extent)) / 2) + min(anpp_extent), "-01-01"))) +
    labs(x = "Date", y = "SPEI") +
    supportR::theme_lyon() +
    theme(legend.position = "none")
  
  # Export locally
  ggsave(filename = file.path("graphs", "diagnostic", "whiplash", 
                              paste0(focal_site, "_whiplash-time-series.png")),
         width = 9, height = 5, units = "in")
  
}

# Check structure of last iteration of loop
dplyr::glimpse(whiplash_df)
## view(whiplash_df)

## ------------------------------------- ##
# Upload Outputs to Drive ----
## ------------------------------------- ##

# Want to upload data to Drive?
upload_data <- FALSE

# Upload data if desired
if(upload_data == T){
  purrr::walk(.x = dir(path = file.path("data", "diagnostic", "whiplash")),
              .f = ~ googledrive::drive_upload(media = file.path("data", "diagnostic", 
                                                                 "whiplash", .x),
                                               overwrite = T, 
                                               googledrive::as_id("https://drive.google.com/drive/u/0/folders/179rWnh1171GTEhb-IbTQh7gUvaJ8IOAG")))
}

# What about *graphs*?
upload_graphs <- FALSE

# Upload graphs if desired
if(upload_graphs == T){
  purrr::walk(.x = dir(path = file.path("graphs", "diagnostic", "whiplash")),
              .f = ~ googledrive::drive_upload(media = file.path("graphs", "diagnostic", 
                                                                 "whiplash", .x),
                                               overwrite = T, 
                                               googledrive::as_id("https://drive.google.com/drive/u/0/folders/1Ffv8V7Fo6KrmJM9vO-IFR70pgryRF1qn")))
}

# End ----
