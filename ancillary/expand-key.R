## ----------------------------------------------------------------- ##
# Resilience Management - Harmonization Workflow
## ----------------------------------------------------------------- ##
# Authors: Nick J Lyon, ...

# Purpose
## Ingest all raw data files and harmonize them into a single data table
## "Harmonize" = combine comparable columns and standardize column names

## --------------------------------------- ##
# Housekeeping ----
## --------------------------------------- ##

# Load libraries
librarian::shelf(tidyverse, lter/ltertools, googledrive)

# Make needed folder(s)
dir.create(file.path("data"), showWarnings = F)
dir.create(file.path("data", "raw"), showWarnings = F)
dir.create(file.path("data", "tidy"), showWarnings = F)

# Clear environment + collect garbage
rm(list = ls()); gc()

## ------------------------------------------- ##
# Download Raw Data ----
## ------------------------------------------- ##

# NOTE
## This script assumes (1) access to the "LTER-WG_Resilience-Management" Shared Drive (2) authentication with R
## For more information on authentication, see the following tutorial:
### https://lter.github.io/scicomp/tutorial_googledrive-pkg.html

# Identify wanted files
files_drive <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1zI1KYBlROyBZSgjSEYmVjsIfCmRPpUPq")) %>% 
  dplyr::filter(stringr::str_detect(string = .$name, pattern = "\\.csv"))

# Did that work?
files_drive

# Identify local files
files_local <- dir(path = file.path("data", "raw"))
files_local

# Overwrite local data files?
update <- FALSE

# Identify desired files
if(update == T) {
  files_wanted <- files_drive 
} else {
  files_wanted <- files_drive %>%
    dplyr::filter(!name %in% files_local)
}

# Download them!
purrr::walk2(.x = files_wanted$id, .y = files_wanted$name,
             .f = ~ googledrive::drive_download(file = .x, overwrite = T,
                                                path = file.path("data", "raw", .y)))

# Clear environment + collect garbage
rm(list = ls()); gc()

## ------------------------------------------- ##
# Download Data Key ----
## ------------------------------------------- ##

# Grab the data key
key_drive <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1Ty7QX7vyvD797eKJzMWbr8AwIo-GyBFO")) %>% 
  dplyr::filter(name == "resilience_data-key")

# Did that work?
key_drive

# Download the data key
googledrive::drive_download(file = key_drive$id, overwrite = T, type = "csv",
                            path = file.path("data", key_drive$name))

# Clear environment + collect garbage
rm(list = ls()); gc()

## ------------------------------------------- ##
# Download Harmonized Data ----
## ------------------------------------------- ##

# Grab the harmonized data
tidy_drive <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/13Ymkrr-kRLDmpaj1jwwVOnOSmEYnF-dJ")) %>% 
  dplyr::filter(name == "01_resilience_harmonized.csv")

# Did that work?
tidy_drive

# Download the harmonized data
googledrive::drive_download(file = tidy_drive$id, overwrite = T,
                            path = file.path("data", "tidy", tidy_drive$name))

# Clear environment + collect garbage
rm(list = ls()); gc()

## ------------------------------------------- ##
# Identify New Rows for Key ----
## ------------------------------------------- ##

# Read in key
res_key <- read.csv(file = file.path("data", "resilience_data-key.csv")) 

# Check structure
dplyr::glimpse(res_key)

# Read in harmonized data
res_df <- read.csv(file = file.path("data", "tidy", "01_resilience_harmonized.csv"))

# Check structure
dplyr::glimpse(res_df)

# What do we need to add to the data key to harmonize the new raw data?
key_addition <- ltertools::expand_key(key = res_key, raw_folder = file.path("data", "raw"),
                                      harmonized_df = res_df, data_format = "csv")

# How's that look?
dplyr::glimpse(key_addition)
## tibble::view(key_addition)

## ------------------------------------------- ##
# Export ----
## ------------------------------------------- ##

# Export locally
write.csv(x = key_addition, na = '', row.names = F,
          file = file.path("data", paste(Sys.Date(), "_new-rows-for-key_DELETE-AFTER-USE.csv")))

# NOTE TO PERSON RUNNING CODE
## Here's what you should do next:
## 1. Open the data key GoogleSheet file
## 2. Scroll to bottom (i.e., end of currently filled-out section)
## 3. Open the CSV you just exported above
## 4. Copy/paste all of the two columns in that into the end of the data key
## 5. Delete the CSV once you've copy/pasted the content into the GoogleSheet





# End ----
