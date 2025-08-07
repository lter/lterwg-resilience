## ----------------------------------------------------------------- ##
# Resilience Management - ANPP Exploration Graphs
## ----------------------------------------------------------------- ##
# Authors: Olivia Hajek

# Purpose
## Understand the mean and variability, etc of ANPP data

## --------------------------------------- ##
# Housekeeping ----
## --------------------------------------- ##

# Load libraries
librarian::shelf(tidyverse, googledrive, supportR)

# Make needed folder(s)
dir.create(file.path("data"), showWarnings = F)
dir.create(file.path("data", "harmonized_data"), showWarnings = F)
dir.create(file.path("data", "environment"), showWarnings = F)

# Clear environment + collect garbage
rm(list = ls()); gc()

# Identify relevant tidy file
focal_name <- "anpp_wyr_merged.csv"

# Download harmonized data file
googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/13Ymkrr-kRLDmpaj1jwwVOnOSmEYnF-dJ")) %>% 
  dplyr::filter(name == focal_file) %>% 
  googledrive::drive_download(file = .$id, overwrite = T,
                              path = file.path("data", "harmonized_data", .$name))

# Read in harmonized data
tidy_v1 <- read.csv(file = file.path("data", "harmonized_data", focal_file))

# Check structure
dplyr::glimpse(tidy_v1)


