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
## Uncomment to re-download
# purrr::walk2(.x = drive_spei$id, .y = drive_spei$name,
#              .f = ~ googledrive::drive_download(file = .x, overwrite = T,
#                                                 path = file.path("data", "environment", .y)))

## ------------------------------------- ##
# Preparatory Wrangling ----
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
  # Make dates 'real' dates
  dplyr::mutate(date = as.Date(date))

# Re-ceck structure
dplyr::glimpse(spei_v2)

## ------------------------------------- ##
# Identify Whiplash Events ----
## ------------------------------------- ##

# Subset to one site
spei_sub <- dplyr::filter(spei_v2, site == "CPER")

# Invoke function
whiplash_df <- id_whiplash(df = spei_sub, date_col = "date", enviro_col = "SPEI",
                           window_size = 3, ref_period = c(1940, 1980),
                           whiplash_perc = c(0.006, 0.994), quiet = FALSE)

# Check structure
glimpse(whiplash_df)
## View(whiplash_df)

# Get a simpler dataframe of only whiplash events
whiplash_only <- dplyr::filter(whiplash_df, !is.na(whiplash))

# Make another histogram
ggplot() +
  geom_histogram(data = whiplash_df, aes(x = SPEI_diff),
                 bins = 45, color = "white", fill = "gray33") +
  geom_histogram(data = whiplash_only, aes(x = SPEI_diff),
                 bins = 50, color = "white", fill = "red") +
  geom_vline(xintercept = unique(whiplash_df$whiplash_thresh_upper), linetype = 2, 
             color = "blue", linewidth = 0.5) +
  geom_vline(xintercept = unique(whiplash_df$whiplash_thresh_lower), linetype = 2, 
             color = "blue", linewidth = 0.5) +
  labs(x = "SPEI Differences (from Windows)", y = "Frequency") +
  supportR::theme_lyon()

# Export locally
ggsave(filename = file.path("graphs", "explore", "whiplash_demo-histogram.png"),
       width = 5, height = 5, units = "in")

# More exploratory graphing
ggplot(whiplash_df, aes(x = date, y = SPEI_diff)) +
  geom_point() + 
  geom_point(data = whiplash_only, aes(x = date, y = SPEI_diff), color = "red") +
  geom_hline(yintercept = unique(whiplash_df$whiplash_thresh_upper), linetype = 2, 
             color = "blue", linewidth = 0.5) +
  geom_hline(yintercept = unique(whiplash_df$whiplash_thresh_lower), linetype = 2, 
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
