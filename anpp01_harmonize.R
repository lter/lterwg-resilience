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
librarian::shelf(tidyverse, lter/ltertools, googledrive, supportR)

# Make needed folder(s)
dir.create(file.path("data"), showWarnings = F)
dir.create(file.path("data", "pre_processed_data"), showWarnings = F)
dir.create(file.path("data", "harmonized_data"), showWarnings = F)

# Clear environment + collect garbage
rm(list = ls()); gc()

## ------------------------------------------- ##
# Download Data ----
## ------------------------------------------- ##

# NOTE
## This script assumes (1) access to the "LTER-WG_Resilience-Management" Shared Drive (2) authentication with R
## For more information on authentication, see the following tutorial:
### https://lter.github.io/scicomp/tutorial_googledrive-pkg.html

# Identify wanted files
files_drive <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1Sw-CdVIsCNvnS3laPn1a90WHoZsEoMif")) %>% 
  dplyr::filter(stringr::str_detect(string = .$name, pattern = "\\.csv"))

# Did that work?
files_drive

# Identify local files
files_local <- dir(path = file.path("data", "pre_processed_data"))
files_local

# Overwrite local data files?
update <- TRUE

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
                                                path = file.path("data", "pre_processed_data", .y)))

# #make the key - we need to just do this once.
# key<-begin_key(raw_folder = file.path("data", "pre-processed_data"))
# 
# 
# Grab the data key
key_drive <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/1/folders/1Ty7QX7vyvD797eKJzMWbr8AwIo-GyBFO")) %>%
  dplyr::filter(name == "resilience_data_key")

# Did that work?
key_drive

# Download the data key
googledrive::drive_download(file = key_drive$id, overwrite = T, type = "csv",
                            path = file.path("data", key_drive$name))

## ------------------------------------------- ##
# Harmonize! ----
## ------------------------------------------- ##

# Read in data key
key <- read.csv(file = file.path("data", "resilience_data_key.csv")) %>%
  filter(source != "drives_siteinfo.csv")

# Check that looks roughly right
dplyr::glimpse(key)

# Perform harmonization
combo_v1 <- ltertools::harmonize(key = key, raw_folder = file.path("data", "pre_processed_data"),
                                 data_format = "csv", quiet = F)

# Check that structure out
dplyr::glimpse(combo_v1)


#check date format
str(combo_v1$date)

#format both date columns as date
combo_v1$date <- as.Date(combo_v1$date, format = "%Y-%m-%d")

combo_v1$date_m.d.yyyy <- as.Date(combo_v1$date_m.d.yyyy, format = "%Y-%m-%d")


#count how many actual date values there are 
sum(!is.na(combo_v1$date))
# 37484

#count how many actual date values there are in the other date column
sum(!is.na(combo_v1$date_m.d.yyyy))
# 6467

#merge date columns
combo_v1 %>%
  mutate(merged_dates = coalesce(date, date_m.d.yyyy)) -> combo_v1

#count how many actual date values there are in the merged dates
sum(!is.na(combo_v1$merged_dates))
# 43951

combo_v1 %>% 
  select(- c(date, date_m.d.yyyy, month)) %>%  #remove the old incomplete month column
  rename(date = merged_dates) %>% 
  mutate(month = format(date, "%m"))-> combo_v1 #pull month from complete date column

## ------------------------------------------- ##
# Export ----
## ------------------------------------------- ##

# Final pre-export tweaks
combo_v99 <- combo_v1

# Check structure
dplyr::glimpse(combo_v99)

# Export locally
write.csv(x = combo_v99, row.names = F, na = '',
          file = file.path("data", "harmonized_data", "01_resilience_harmonized.csv"))

# Upload to Drive
googledrive::drive_upload(media = file.path("data", "harmonized_data", "01_resilience_harmonized.csv"), overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/13Ymkrr-kRLDmpaj1jwwVOnOSmEYnF-dJ"))

# 
# # Upload to Drive New Key
# googledrive::drive_upload(media = file.path("data", "resilience_data-key.csv"), overwrite = T,
#                           path = googledrive::as_id("https://drive.google.com/drive/folders/1Ty7QX7vyvD797eKJzMWbr8AwIo-GyBFO"))

# End ----
