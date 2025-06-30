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
dir.create(file.path("graphs", "explore"), showWarnings = F, recursive = T)

# Clear environment
rm(list = ls()); gc()

# Load needed custom function(s)
purrr::walk(.x = dir(path = file.path("tools"), pattern = "fxn_"),
            .f = ~ source(file.path("tools", .x)))

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

# Now, get a 1940-80 version of this
spei3_4080 <- spei3_v2 %>% 
  # Filter to desired date range
  dplyr::filter(year(date) >= 1940 & year(date) <= 1980)

# Re-check structure
dplyr::glimpse(spei3_v2)

## ------------------------------------- ##
# Window Function Development ----
## ------------------------------------- ##

# Make a simpler dataframe for both
test_df <- dplyr::filter(spei3_v2, site == "CPER")
test_4080 <- dplyr::filter(spei3_4080, site == "CPER")

# Check structure
dplyr::glimpse(test_df)

# Invoke function for both data objects
test_out <- diff_windows(df = test_df, date_col = "date", enviro_col = "SPEI",
                         window_size = 3, quiet = F)
test_4080_out <- diff_windows(df = test_4080, date_col = "date", enviro_col = "SPEI",
                              window_size = 3, quiet = F)

# Check structure
dplyr::glimpse(test_out)

# Identify whiplash percentile thresholds
upper_perc <- 0.994
lower_perc <- 0.006

# Calculate enviromental threshold values at user-defined percentiles
## NOTE: Swain et al used 40-80 values to identify thresholds for longer record
upper_thresh <- as.numeric(quantile(x = test_4080_out$SPEI_diff, probs = upper_perc))
lower_thresh <- as.numeric(quantile(x = test_4080_out$SPEI_diff, probs = lower_perc))

# Identify whiplash events now that window differences are known
## NOTE: Here we're using the full record
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

# Export locally
ggsave(filename = file.path("graphs", "explore", "whiplash_demo-histogram.png"),
       width = 5, height = 5, units = "in")

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

# Export locally
ggsave(filename = file.path("graphs", "explore", "whiplash_demo-scatter.png"),
       width = 5, height = 5, units = "in")

# Yet more exploratory graphing
ggplot(whiplash_df, aes(x = date, y = SPEI)) +
  geom_path() +
  geom_point(size = 0.5, color = "black") + 
  geom_point(data = whiplash_only, aes(x = date, y = SPEI),
             color = "red", size = 2) +
  labs(x = "Date", y = "SPEI") +
  supportR::theme_lyon() +
  theme(legend.position = "none")

# Export locally
ggsave(filename = file.path("graphs", "explore", "whiplash_demo-time-series.png"),
       width = 12, height = 6, units = "in")

# End ----
