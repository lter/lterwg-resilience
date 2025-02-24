## ----------------------------------------------------------------- ##
# Resilience Management - Wrangling Workflow
## ----------------------------------------------------------------- ##
# Authors: Nick J Lyon, ...

# Purpose
## 

## --------------------------------------- ##
# Housekeeping ----
## --------------------------------------- ##

# Load libraries
librarian::shelf(tidyverse, googledrive)

# Make needed folder(s)
dir.create(file.path("data"), showWarnings = F)
dir.create(file.path("data", "tidy"), showWarnings = F)

# Clear environment + collect garbage
rm(list = ls()); gc()

# Identify desired file
focal_file <- "02_resilience_wrangled.csv"

# Download harmonized data file
googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/13Ymkrr-kRLDmpaj1jwwVOnOSmEYnF-dJ")) %>% 
  dplyr::filter(name == focal_file) %>% 
  googledrive::drive_download(file = .$id, overwrite = T,
                              path = file.path("data", "tidy", .$name))

# Read in harmonized data
sub_v1 <- read.csv(file = file.path("data", "tidy", focal_file))

# Check structure
dplyr::glimpse(sub_v1)

## ------------------------------------------- ##
# Calculate Needed Metrics ----
## ------------------------------------------- ##

# Some subset operations require certain aggregated metrics to evaluate

# TBD; will return!
sub_v2 <- sub_v1


# Check structure
dplyr::glimpse(sub_v2)

## ------------------------------------------- ##
# Export ----
## ------------------------------------------- ##

# Final pre-export tweaks
sub_v99 <- sub_v2

# Check structure
dplyr::glimpse(sub_v99)

# Make nice output name
focal_output <- "03_resilience_filtered.csv"

# Export locally
write.csv(x = sub_v99, row.names = F, na = '', file = file.path("data", "tidy", focal_output))

# Upload to Drive
googledrive::drive_upload(media = file.path("data", "tidy", focal_output), overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/13Ymkrr-kRLDmpaj1jwwVOnOSmEYnF-dJ"))

# End ----
