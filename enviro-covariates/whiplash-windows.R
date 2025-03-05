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

# Mostly clear environment
rm(list = setdiff(ls(), c("spei3_v2", "spei3_v1"))); gc()

# Make a simpler dataframe
test_df <- spei3_v2 %>% 
  dplyr::filter(site == "CAF")

# Check structure
dplyr::glimpse(test_df)

# Identify window size
window_size <- 3

# Output list
diff_list <- list()

# Loop across dates in data
for(focal_date in test_df$date){
# for(focal_date in as.Date("1901-10-16")){
  ## focal_date <- as.Date("1901-10-16")
  
  # Coerce to real date & overwrite existing object (should be one already...)
  focal_date <- as.Date(focal_date)
  
  # Processing message  
  message("Identify environmental differences in window relative to ", focal_date)
  
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
      prior_spei <- NA_real_
      # Otherwise, calculate difference
    } else { diff_spei <- focal_spei - prior_spei }
    
    # Assemble dataframe variant of output
    diff_out <- data.frame("date" = focal_date,
                           "spei" = focal_spei,
                           "prior_date" = prior_date,
                           "prior_spei" = prior_spei,
                           "spei_diff" = diff_spei)
    
    # Add to output list
    diff_list[[paste0(focal_date, "_", months_prior)]] <- diff_out
    
  } # Close prior months relative to focal date loop
} # Close focal date loop

# Process outputs
diff_df <- diff_list %>% 
  purrr::list_rbind(x = .) %>% 
  # Drop NA SPEI differences (where window exceeds available date range)
  dplyr::filter(!is.na(spei_diff))


# Check that out
diff_df
## view(diff_df)

# Determine upper/lower thresholds of SPEI (for what constitutes "whiplash")
upper_perc <- 0.994
lower_perc <- 0.006

# Calculate values at those thresholds
upper_thresh <- as.numeric(quantile(x = diff_df$spei_diff, probs = upper_perc))
lower_thresh <- as.numeric(quantile(x = diff_df$spei_diff, probs = lower_perc))

# Exploratory histogram
ggplot(diff_df, aes(x = spei_diff)) +
  geom_histogram(bins = 45, color = "white", fill = "gray33") +
  geom_vline(xintercept = upper_thresh, linetype = 2, 
             color = "blue", linewidth = 0.5) +
  geom_vline(xintercept = lower_thresh, linetype = 2, 
             color = "blue", linewidth = 0.5) +
  supportR::theme_lyon()

# geom_histogram()# More processing
diff_out <- diff_df %>% 
  # Identify maximum/minimum per date
  dplyr::group_by(date, spei) %>% 
  dplyr::summarize(diff_max = max(spei_diff, na.rm = T),
                   diff_min = min(spei_diff, na.rm = T),
                   .groups = "keep") %>% 
  dplyr::ungroup() %>% 
  # Identify whiplash events
  dplyr::mutate(whiplash = ifelse(diff_max >= upper_thresh | 
                                    diff_min <= lower_thresh,
                                  yes = "whiplash", no = NA)) %>% 
  # Identify direction of whiplash
  dplyr::mutate(whiplash_direction = dplyr::case_when(
    diff_max >= upper_thresh ~ "max",
    diff_min <= lower_thresh ~ "min",
    T ~ NA)) %>% 
  # Simplify max/min SPEI diff to just the difference that actually crosses the threshold
  dplyr::mutate(spei_diff = dplyr::case_when(
    diff_max >= upper_thresh ~ diff_max,
    diff_min <= lower_thresh ~ diff_min,
    T ~ NA), .after = spei) %>% 
  dplyr::select(-dplyr::starts_with("diff_"))

# Check structure of result
dplyr::glimpse(diff_out)
## view(diff_out)

# Any whiplash events?
whiplash_only <- diff_out %>% 
  dplyr::filter(whiplash == "whiplash")

whiplash_only

# Make another histogram
ggplot() +
  geom_histogram(data = diff_df, aes(x = spei_diff),
                 bins = 45, color = "white", fill = "gray33") +
  geom_histogram(data = whiplash_only, aes(x = spei_diff),
                 bins = 50, color = "white", fill = "red") +
  geom_vline(xintercept = upper_thresh, linetype = 2, 
             color = "blue", linewidth = 0.5) +
  geom_vline(xintercept = lower_thresh, linetype = 2, 
             color = "blue", linewidth = 0.5) +
  supportR::theme_lyon()

# More exploratory graphing
ggplot(diff_df, aes(x = date, y = spei_diff)) +
  geom_point() + 
  geom_point(data = whiplash_only, aes(x = date, y = spei_diff), color = "red") +
  geom_hline(yintercept = upper_thresh, linetype = 2, 
             color = "blue", linewidth = 0.5) +
  geom_hline(yintercept = lower_thresh, linetype = 2, 
             color = "blue", linewidth = 0.5) +
  supportR::theme_lyon()

## ------------------------------------- ##
# Whiplash Function Development ----
## ------------------------------------- ##

# Mostly clear environment
rm(list = setdiff(ls(), c("spei3_v2", "spei3_v1"))); gc()

# Make a simpler dataframe
test_df <- spei3_v2 %>% 
  dplyr::filter(site == "CAF")

# Check structure
dplyr::glimpse(test_df)

# Define function
find_whiplash <- function(data = NULL, date_col = "date", enviro_col = "SPEI",
                          upper_perc = 0.994, lower_perc = 0.006, 
                          window_size = 3, quiet = FALSE){
  
  # Error / warning checks
  ## TBD, will return!
  # Error for non-dataframe 'data'
  # Errors for non-character 'date_col'/'enviro_col'
  # Errors for no columns matching provided 'date_col'/'enviro_col'
  # Error if 'date_col' isn't a date-format vector
  # Error if 'enviro_col' isn't a number-format vector
  # Error for more than one entry to any argument
  # Error for non-integer 'window_size'
  # Warning for non-logical 'quiet'
  
  data = test_df
  date_col = "date"
  enviro_col = "SPEI"
  upper_perc = 0.994
  lower_perc = 0.006
  window_size = 3
  quiet = F
  
  # Calculate enviromental threshold values at user-defined percentiles
  upper_thresh <- as.numeric(quantile(x = data[[{{enviro_col}}]], 
                                      probs = upper_perc))
  lower_thresh <- as.numeric(quantile(x = data[[{{enviro_col}}]], 
                                      probs = lower_perc))
  
  # List for storing outputs
  whiplash_list <- list()
  
  # Loop across dates in provided data
  for(focal_date in data[[{{date_col}}]]){
    ## focal_date <- as.Date("1983-03-16")
    
    # Coerce to real date & overwrite existing object (should be one already...)
    focal_date <- as.Date(focal_date)
    
    # If 'quiet' isn't false, return processing message
    message("Identifying whiplash events relative to ", focal_date)
    
    # Grab SPEI at that time point
    focal_enviro <- dplyr::filter(data, date == focal_date)[[{{enviro_col}}]]
    
    # Loop across window size amount of prior months
    for(months_prior in window_size:1){
      ## months_prior <- 3
      
      # Identify date at number of months prior to focal date
      prior_date <- focal_date - months(months_prior)
      
      # Identify SPEI at that date
      prior_enviro <- dplyr::filter(data, date == prior_date)[[{{enviro_col}}]]
      
      # If no date exists, the difference is NA
      if(length(prior_enviro) == 0){
        diff_enviro <- NA_real_
        prior_enviro <- NA_real_
        # Otherwise, calculate difference
      } else { diff_enviro <- focal_enviro - prior_enviro }
      
      # Assemble dataframe variant of output
      month_out <- data.frame("date" = focal_date, 
                              "enviro" = focal_enviro,
                              "prior_date" = prior_date,
                              "prior_enviro" = prior_enviro,
                              "enviro_diff" = diff_enviro) 
      
      # Add to output list
      whiplash_list[[paste0(focal_date, "_", months_prior)]] <- month_out
      
    } # Close prior months relative to focal date loop
  } # Close focal date loop
  
  # Coarse processing of output
  whiplash_df <- whiplash_list %>% 
    # Unlist to dataframe
    purrr::list_rbind(x = .) %>% 
    # Drop NA enviro differences (where window exceeds available date range)
    dplyr::filter(!is.na(enviro_diff))
  
  # Do some summarization of that
  whiplash_out <- whiplash_df %>% 
    # Identify maximum/minimum per date
    dplyr::group_by(dplyr::across(dplyr::all_of(c("date", "enviro")))) %>% 
    dplyr::summarize(diff_max = max(enviro_diff, na.rm = T),
                     diff_min = min(enviro_diff, na.rm = T),
                     .groups = "keep") %>% 
    dplyr::ungroup() %>% 
    # Identify whiplash events
    dplyr::mutate(whiplash = ifelse(diff_max >= upper_thresh | 
                                      diff_min <= lower_thresh,
                                    yes = "whiplash", no = NA)) %>% 
    # Identify direction of whiplash
    dplyr::mutate(whiplash_direction = dplyr::case_when(
      diff_max >= upper_thresh ~ "max",
      diff_min <= lower_thresh ~ "min",
      T ~ NA)) %>% 
    # Simplify max/min SPEI diff to just the difference that actually crosses the threshold
    dplyr::mutate(enviro_diff = dplyr::case_when(
      diff_max >= upper_thresh ~ diff_max,
      diff_min <= lower_thresh ~ diff_min,
      T ~ NA), .after = enviro) %>% 
    dplyr::select(-dplyr::starts_with("diff_"))
  
  
  # max_date = ifelse(enviro - prior_enviro == enviro_diff,
  #                   yes = paste(prior_date, collapse = "; "), 
  #                   no = NA_Date_),
  # min_date = ifelse(enviro - prior_enviro == enviro_diff,
  #                   yes = paste(prior_date, collapse = "; "),
  #                   no = NA_Date_),
  # 
  
  # DELETE ME (vvv) ----
  
  whiplash_df %>% head()
  
  dplyr::glimpse(whiplash_df)
  
  
  whiplash_out %>% 
    dplyr::filter(whiplash == "whiplash") %>% 
    glimpse()
  
  
  # DELETE ME (^^^)----
  
  # %>% 
  #   # Rename two columns
  #   supportR::safe_rename(data = ., 
  #                         bad_names = c("X", "Y", "prior_X", "Y_diff"),
  #                         good_names = c(date_col, enviro_col, 
  #                                        paste0("prior_", date_col),
  #                                        paste0(enviro_col, "_diff")))
  
  
}

# Invoke function








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
