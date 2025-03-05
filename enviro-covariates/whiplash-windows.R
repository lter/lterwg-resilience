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
# Window Function Development ----
## ------------------------------------- ##

# Mostly clear environment
rm(list = setdiff(ls(), c("spei3_v2", "spei3_v1"))); gc()

# Make a simpler dataframe
test_df <- spei3_v2 %>% 
  dplyr::filter(site == "CAF")

# Check structure
dplyr::glimpse(test_df)

# Define function
diff_windows <- function(df = NULL, date_col = "date", enviro_col = "SPEI",
                             window_size = 3, quiet = FALSE){
  
  # Error for non-dataframe 'df' or missing columns
  if("data.frame" %in% class(df) != T || any(c(date_col, enviro_col) %in% names(df)) != T )
    stop("'df' must be dataframe-like and have column names exactly matching 'date_col' and 'enviro_col'")
  
  # Errors for 'date_col' argument
  if(is.character(date_col) != T || date_col %in% names(df) != T || length(date_col) != 1 || class(df[[date_col]]) != "Date")
    stop("'date_col' must be a length-one character vector that exactly matches a column name in 'df' containing date information")
  
  # Errors for 'enviro_col' argument
  if(is.character(enviro_col) != T || enviro_col %in% names(df) != T || length(enviro_col) != 1 || any(c("numeric", "integer") %in% class(df[[enviro_col]])) != T)
    stop("'enviro_col' must be a length-one character vector that exactly matches a column name in 'df' containing numeric information")
  
  # Errors for 'window_size'
  if(length(window_size) != 1 || all(c("numeric", "integer") %in% class(window_size) != T) || window_size <= 0 || window_size - floor(window_size) != 0)
    stop("'window_size' must be a single integer greater than 0")
  
  # Warning for non-logical 'quiet'
  if(is.logical(quiet) != T){
    warning("'quiet' must be a logical. Defaulting to 'FALSE'")
    quiet <- FALSE }
  
  # Make a list for storing outputs
  diff_list <- list()
  
  # Loop across dates in df
  for(focal_date in df[[{{date_col}}]]){
    
    # Coerce to real date & overwrite existing object (should be one already...)
    focal_date <- as.Date(focal_date)
    
    # If 'quiet' isn't false, return a processing message  
    if(quiet != TRUE){
      message("Identifying environmental differences in window relative to ", focal_date) }
    
    # Grab enviro variable at that time point
    focal_enviro <- dplyr::filter(df, !!as.symbol(date_col) == focal_date)[[{{enviro_col}}]]
    
    # Loop across window size amount of prior months
    for(months_prior in window_size:1){
      ## months_prior <- 3
      
      # Identify date at number of months prior to focal date
      prior_date <- focal_date - months(months_prior)
      
      # Identify enviro at that date
      prior_enviro <- dplyr::filter(df, !!as.symbol(date_col) == prior_date)[[{{enviro_col}}]]
      
      # If no date exists...
      if(length(prior_enviro) == 0){
        ## ...Fill it and the difference with NA
        prior_enviro <- NA_real_
        diff_enviro <- NA_real_
        
        ## Otherwise, calculate true difference
      } else { diff_enviro <- focal_enviro - prior_enviro }
      
      # Assemble dataframe variant of output
      diff_out <- data.frame("date" = focal_date,
                             "enviro" = focal_enviro,
                             "prior_date" = prior_date,
                             "prior_enviro" = prior_enviro,
                             "enviro_diff" = diff_enviro)
      
      # Add to output list
      diff_list[[paste0(focal_date, "_", months_prior)]] <- diff_out
      
    } # Close prior months relative to focal date loop
  } # Close focal date loop
  
  # Unlist output & drop NA enviro differences
  ## (Where window exceeds available date range)
  diff_df <- purrr::list_rbind(x = diff_list) %>% 
    dplyr::filter(!is.na(enviro_diff))
  
  # Rename this to better match inputs
  diff_out <- supportR::safe_rename(data = diff_df,
                                    bad_names = c("date", "enviro", 
                                                  "prior_date", "prior_enviro",
                                                  "enviro_diff"),
                                    good_names = c(date_col, enviro_col,
                                                   paste0("prior_", date_col),
                                                   paste0("prior_", enviro_col),
                                                   paste0(enviro_col, "_diff")))
  
  
  # Return that to user
  return(diff_out)
  
}

# Invoke function
test_out <- diff_windows(df = test_df, date_col = "date", enviro_col = "SPEI",
                         window_size = 3, quiet = F)

# Check structure
dplyr::glimpse(test_out)

# Identify whiplash percentile thresholds
upper_perc = 0.994
lower_perc = 0.006

# Calculate enviromental threshold values at user-defined percentiles
upper_thresh <- as.numeric(quantile(x = test_out$SPEI_diff, probs = upper_perc))
lower_thresh <- as.numeric(quantile(x = test_out$SPEI_diff, probs = lower_perc))

# Make an exploratory graph
ggplot(test_out, aes(x = SPEI_diff)) +
  geom_histogram(bins = 45, color = "white", fill = "gray33") +
  geom_vline(xintercept = upper_thresh, linetype = 2, 
             color = "blue", linewidth = 0.5) +
  geom_vline(xintercept = lower_thresh, linetype = 2, 
             color = "blue", linewidth = 0.5) +
  labs(x = "SPEI Differences (from Windows)", y = "Frequency") +
  supportR::theme_lyon()

# Identify whiplash events now that window differences are known
whiplash_df <- test_out %>% 
  # Identify maximum/minimum per date
  dplyr::group_by(date, SPEI) %>% 
  dplyr::summarize(diff_max = max(SPEI_diff, na.rm = T),
                   diff_min = min(SPEI_diff, na.rm = T),
                   .groups = "keep") %>% 
  dplyr::ungroup() %>% 
  # Identify whiplash events (non-inclusive of threshold value)
  dplyr::mutate(whiplash = ifelse(diff_max > upper_thresh | 
                                    diff_min < lower_thresh,
                                  yes = "whiplash", no = NA)) %>% 
  # Identify direction of whiplash
  dplyr::mutate(whiplash_direction = dplyr::case_when(
    diff_max > upper_thresh ~ "max",
    diff_min < lower_thresh ~ "min",
    T ~ NA)) %>% 
  # Simplify max/min SPEI diff to just the difference that actually crosses the threshold
  dplyr::mutate(SPEI_diff = dplyr::case_when(
    diff_max >= upper_thresh ~ diff_max,
    diff_min <= lower_thresh ~ diff_min,
    T ~ NA), .after = SPEI) %>% 
  dplyr::select(-dplyr::starts_with("diff_"))

# Check structure of result
dplyr::glimpse(whiplash_df)
## view(whiplash_df)

# Any whiplash events?
(whiplash_only <- dplyr::filter(whiplash_df, whiplash == "whiplash") )

# Make another histogram
ggplot() +
  geom_histogram(data = test_out, aes(x = SPEI_diff),
                 bins = 45, color = "white", fill = "gray33") +
  geom_histogram(data = whiplash_only, aes(x = SPEI_diff),
                 bins = 50, color = "white", fill = "red") +
  geom_vline(xintercept = upper_thresh, linetype = 2, 
             color = "blue", linewidth = 0.5) +
  geom_vline(xintercept = lower_thresh, linetype = 2, 
             color = "blue", linewidth = 0.5) +
  labs(x = "SPEI Differences (from Windows)", y = "Frequency") +
  supportR::theme_lyon()

# More exploratory graphing
ggplot(test_out, aes(x = date, y = SPEI_diff)) +
  geom_point() + 
  geom_point(data = whiplash_only, aes(x = date, y = SPEI_diff), color = "red") +
  geom_hline(yintercept = upper_thresh, linetype = 2, 
             color = "blue", linewidth = 0.5) +
  geom_hline(yintercept = lower_thresh, linetype = 2, 
             color = "blue", linewidth = 0.5) +
  labs(x = "Date", y = "SPEI Differences") +
  supportR::theme_lyon()

# End ----
