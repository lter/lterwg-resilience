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
dir.create(file.path("data", "tidy"), showWarnings = F)

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
key <- read.csv(file = file.path("data", "resilience_data_key.csv"))

# Check that looks roughly right
dplyr::glimpse(key)

# Perform harmonization
combo_v1 <- ltertools::harmonize(key = key, raw_folder = file.path("data", "pre_processed_data"),
                                 data_format = "csv", quiet = F)

# Check that structure out
dplyr::glimpse(combo_v1)

## ------------------------------------------- ##
# QUICK QC ---- DATES AND ANPP UNITS
## ------------------------------------------- ##
combo_v2 <- combo_v1 %>%
  # ANPP - convert KG/ha to g/m2
  dplyr::mutate(anpp_g_m2 = ifelse(is.na(anpp_g_m2), anpp_kg_ha/10, anpp_g_m2))%>%
  # fix dates and extract year
  dplyr::mutate(date = lubridate::as_date(date, format="%m/%d/%Y"), 
                date_m.d.yyy = lubridate::as_date(date_m.d.yyyy, format= "%m-%d-%Y"), 
                date_m.d.yyy2 = lubridate::as_date(date_m.d.yyyy, format= "%Y-%m-%d")) %>%
  #make a single date column 
  dplyr::mutate(dates =coalesce(date, date_m.d.yyy, date_m.d.yyy2))%>%
  # add year
  dplyr::mutate(year = ifelse(is.na(year), lubridate::year(dates), year))

## ------------------------------------------- ##
#  MERGE ANNUAL PRECIPITATION WITH ANPP DATA
## ------------------------------------------- ##

## --------------------------------------- ##
# Download Precip Data ----
## --------------------------------------- ##

# Identify files from Drive
drive_precip <- googledrive::drive_ls(path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/16KZhR5CGu7YDze72Y2-LaNEdGNC39Kcf"))

# Look good?
drive_precip

# Download locally
purrr::walk2(.x = drive_precip$id, .y = drive_precip$name,
             .f = ~ googledrive::drive_download(file = .x, overwrite = T,
                                                path = file.path("data", "environment", .y)))

### Read in precipitation data
ppt <- read.csv(file = file.path("data", "environment", "precip_annual-summary.csv"))

##Make sure all site IDs are capitalized
combo_v2$site <- ifelse(combo_v2$network=="LTER", toupper(combo_v2$site), combo_v2$site)

## Join
combo_ppt <- left_join(combo_v2, ppt, by=c("network", "site","year"))

## ------------------------------------------- ##
# Export ----
## ------------------------------------------- ##

# Final pre-export tweaks
combo_v99 <- combo_ppt

# Check structure
dplyr::glimpse(combo_v99)

# Export locally
write.csv(x = combo_v99, row.names = F, na = '',
          file = file.path("data", "tidy", "01_resilience_harmonized.csv"))

# Upload to Drive
googledrive::drive_upload(media = file.path("data", "tidy", "01_resilience_harmonized.csv"), overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/13Ymkrr-kRLDmpaj1jwwVOnOSmEYnF-dJ"))


# Upload to Drive New Key
googledrive::drive_upload(media = file.path("data", "resilience_data-key.csv"), overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/folders/1Ty7QX7vyvD797eKJzMWbr8AwIo-GyBFO"))

# End ----
