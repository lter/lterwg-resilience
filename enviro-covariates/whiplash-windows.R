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
librarian::shelf(tidyverse, googledrive)

# Make needed folder(s)
dir.create(file.path("data", "environment"), showWarnings = F, recursive = T)

# Clear environment
rm(list = ls()); gc()

## ------------------------------------- ##
# Download SPEI CSVs ----
## ------------------------------------- ##
# Created by "exploratory/download_SPEI-site.R"

# Identify files in drive
drive_spei <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1JtFMD4IAizjNGd0wbLLZIBdgqnk4YR97")) %>% 
  dplyr::filter(stringr::str_detect(string = name, pattern = ".csv"))

# Check that worked
drive_spei

# Download locally
purrr::walk2(.x = drive_spei$id, .y = drive_spei$name,
             .f = ~ googledrive::drive_download(file = .x, overwrite = T,
                                                path = file.path("data", "environment", .y)))

## ------------------------------------- ##
# Preparatory Wrangling ----
## ------------------------------------- ##

# Read in one of those files
spei3_v1 <- read.csv(file.path("data", "environment", "SPEI03.csv"))

# Check structure
dplyr::glimpse(spei3_v1)

# Do some wrangling to this object
spei3_v2 <- spei3_v1 %>% 
  # Rotate to long format
  tidyr::pivot_longer(cols = -date, names_to = "site", values_to = "SPEI") %>% 
  # Drop missing values
  dplyr::filter(!is.na(SPEI)) %>% 
  # Make dates 'real' dates
  dplyr::mutate(date = as.Date(date))

# Re-check structure
dplyr::glimpse(spei3_v2)

## ------------------------------------- ##
# Whiplash Exploration ----
## ------------------------------------- ##

# First, need to figure out whiplash event ID process

# Make a simpler dataframe
test_df <- spei3_v2 %>% 
  dplyr::filter(site == "CAF")

# Check structure
dplyr::glimpse(test_df)

# Determine upper/lower thresholds of SPEI (for what constitutes "whiplash")
upper_perc <- 0.994
lower_perc <- 0.006

# Calculate values at those thresholds
upper_thresh <- as.numeric(quantile(x = test_df$SPEI, probs = upper_perc))
lower_thresh <- as.numeric(quantile(x = test_df$SPEI, probs = lower_perc))

# Identify window size
window_size <- 3

# Output list
whiplash_list <- list()

# Loop across dates in data
for(focal_date in test_df$date){
  ## focal_date <- as.Date("1901-10-16")

  # Coerce to real date & overwrite existing object (should be one already...)
  focal_date <- as.Date(focal_date)
  
  # Processing message  
  message("Identify whiplash events in window relative to ", focal_date)
  
  # Grab SPEI at that time point
  focal_spei <- dplyr::filter(test_df, date == focal_date)$SPEI
  
  # Loop across window size amount of prior months
  for(months_prior in window_size:1){
    ## months_prior <- 3
    
    # Identify date at number of months prior to focal date
    prior_date <- focal_date - months(months_prior)
    
    # Identify SPEI at that date
    prior_spei <- dplyr::filter(test_df, date == prior_date)$SPEI
    
    # If no date exists, the difference is NA
    if(length(prior_spei) == 0){
      diff_spei <- NA_real_
      # Otherwise, calculate difference
    } else { diff_spei <- focal_spei - prior_spei }
    
    # Assemble dataframe variant of output
    diff_out <- data.frame("date" = focal_date,
                           "spei" = focal_spei,
                           "prior_date" = prior_date,
                           "spei_diff" = diff_spei)
    
    # Add to output list
    whiplash_list[[paste0(focal_date, "_", months_prior)]] <- diff_out
    
  } # Close prior months relative to focal date loop
} # Close focal date loop

# Process outputs
whiplash_df <- whiplash_list %>% 
  purrr::list_rbind(x = .) %>% 
  # Drop NA SPEI differences (where window exceeds available date range)
  dplyr::filter(!is.na(spei_diff)) %>% 
  # Identify maximum/minimum per date
  dplyr::group_by(date, spei) %>% 
  dplyr::summarize(diff_max = max(spei_diff, na.rm = T),
                   diff_min = min(spei_diff, na.rm = T)) %>% 
  dplyr::ungroup() %>% 
  # Identify whiplash events
  dplyr::mutate(whiplash = ifelse(diff_max >= upper_thresh | 
                                    diff_min <= lower_thresh,
                                  yes = "whiplash", no = NA)) %>% 
  # Identify direction of whiplash
  dplyr::mutate(whiplash_direction = dplyr::case_when(
    diff_max >= upper_thresh ~ "max",
    diff_min <= lower_thresh ~ "min",
    T ~ NA))

# Check structure of result
dplyr::glimpse(whiplash_df)
## view(whiplash_df)

# Any whiplash events?
whiplash_df %>% 
  dplyr::filter(whiplash == "whiplash")





## ------------------------------------- ##
# Whiplash Windows ----
## ------------------------------------- ##




# End ----
