## --------------------------------------------------------------- ##
# Iowa State Univ Drainage Project Data Pre-Processing
## --------------------------------------------------------------- ##
# Script author(s): Nick J Lyon, 

# Purpose
## Pre-process ISU Drainage Project data to be ready for inclusion in the harmonization workflow

## ------------------------------------- ##
# Housekeeping ----
## ------------------------------------- ##

# Load needed libraries
librarian::shelf(tidyverse, googledrive, ltertools)

# Make needed folder(s)
source(file.path("00_create_data_folder.R"))

# Clear environment
rm(list = ls()); gc()

# Identify network
net_name <- "ISU Drainage"

## ------------------------------------- ##
# Download Data ----
## ------------------------------------- ##

# If the data don't exist locally, download 'em
if("td_20250827225852.xlsx" %in% dir(path = file.path("data", "raw_data")) != T){
  
  # Identify data in Drive
  drive_cscap <- googledrive::drive_ls(path = googledrive::as_id("https://drive.google.com/drive/folders/1iyCpiv06PdcVzU99VPuN-FSDwAkwv-jJ")) %>% 
    dplyr::filter(stringr::str_detect(string = name, pattern = "td_"))
  
  # Check that worked
  drive_cscap
  
  # Download it
  purrr::walk(.x = drive_cscap$id, .y = drive_cscap$name,
              .f = ~ googledrive::drive_download(file = .x, overwrite = T,
                                                 path = file.path("data", "raw_data", .y)))
}

# Do the same for the 'treatment table year' GoogleSheet
if("treatment_table_year.csv" %in% dir(path = file.path("data")) != T){
  
  # Identify data in Drive
  drive_trttab <- googledrive::drive_ls(path = googledrive::as_id("https://drive.google.com/drive/folders/1Ty7QX7vyvD797eKJzMWbr8AwIo-GyBFO")) %>% 
    dplyr::filter(name == "treatment_table_year")
  
  # Check that worked
  drive_trttab
  
  # Download it
  purrr::walk(.x = drive_trttab$id, .y = drive_trttab$name,
              .f = ~ googledrive::drive_download(file = .x, overwrite = T, type = "csv",
                                                 path = file.path("data", .y)))
}

## ------------------------------------- ##
# Load & Tidy Data ----
## ------------------------------------- ##

# Read data
isu_v1 <- readxl::read_xlsx(path = file.path("data", "raw_data", "td_20250827225852.xlsx"),
                            sheet = "Agronomic")

# Check structure
dplyr::glimpse(isu_v1)

# Begin processing
isu_v2 <- isu_v1 %>% 
  # Ditch bad header rows
  dplyr::filter(!siteid %in% c("description", "units")) %>% 
  # Replace null values with true NAs
  dplyr::mutate(dplyr::across(.cols = dplyr::everything(),
                              .fns = ~ ifelse(. == -999, yes = NA, no = .))) %>% 
  # Make numeric columns into real numbers
  dplyr::mutate(dplyr::across(.cols = dplyr::ends_with(c("biomass", "date")),
                              .fns = ~ as.numeric(.))) %>% 
  # Fix stupid Excel date issue
  dplyr::mutate(date = as.Date(date, origin = "1899-12-30"))

# Check structure
dplyr::glimpse(isu_v2)

## ------------------------------------- ##
# Process Data ----
## ------------------------------------- ##

# Now that the data are in tidy form, we can do the real processing
isu_v3 <- isu_v2 %>% 
  # Keep only rows that have either whole plant or forage biomass
  dplyr::filter(!is.na(whole_plant_biomass) | !is.na(forage_biomass)) %>% 
  # Ditch any columns that are completely empty
  dplyr::select(-dplyr::where(fn = ~ all(is.na(.)))) %>% 
  # Add network name
  dplyr::mutate(network = net_name, .before = dplyr::everything()) 

# Check structure
dplyr::glimpse(isu_v3)

## ------------------------------------- ##
# Extract Treatment Table Info ----
## ------------------------------------- ##

# We also want some information for the 'treatment_table_year' file
isutrt_v1 <- isu_v3 %>% 
  # Pare down to only some columns
  dplyr::select(network:year) %>% 
  # Keep only unique rows
  dplyr::distinct()
  
# Check structure
dplyr::glimpse(isutrt_v1)

# Read in the 'planting' sheet of the data file
plant_v1 <- readxl::read_xlsx(path = file.path("data", "raw_data", "td_20250827225852.xlsx"),
                              sheet = "Planting")

# Check structure
dplyr::glimpse(plant_v1)

# Grab just what we need
plant_v2 <- plant_v1 %>% 
  # Ditch bad header rows
  dplyr::filter(!siteid %in% c("description", "units")) %>% 
  # Pare down columns
  dplyr::select(siteid, plotid, year_calendar, cashcrop, date) %>% 
  # Rename slightly
  dplyr::rename(crop = cashcrop,
                year = year_calendar,
                date_plant = date)

# Check structure
dplyr::glimpse(plant_v2)

# Read in the 'harvesting' sheet of the data file
harvest_v1 <- readxl::read_xlsx(path = file.path("data", "raw_data", "td_20250827225852.xlsx"),
                                sheet = "Harvesting")

# Check structure
dplyr::glimpse(harvest_v1)

# Grab just what we need
harvest_v2 <- harvest_v1 %>% 
  # Ditch bad header rows
  dplyr::filter(!siteid %in% c("description", "units")) %>% 
  # Pare down columns
  dplyr::select(siteid, plotid, year_calendar, cashcrop, date) %>% 
  # Rename slightly
  dplyr::rename(crop = cashcrop,
                year = year_calendar,
                date_harvest = date)

# Check structure
dplyr::glimpse(harvest_v2)

# Combine these various treatment tables into one
isutrt_v2 <- isutrt_v1 %>% 
  dplyr::left_join(y = plant_v2, by = c("siteid", "plotid", "year", "crop")) %>% 
  dplyr::left_join(y = harvest_v2, by = c("siteid", "plotid", "year", "crop")) %>% 
  # Make numeric columns true numbers
  dplyr::mutate(dplyr::across(.cols = dplyr::contains(c("date", "year")),
                              .fns = as.numeric)) %>% 
  # Fix stupid Excel date issue
  dplyr::mutate(dplyr::across(.cols = dplyr::starts_with("date"),
                              .fns = ~ as.Date(., origin = "1899-12-30"))) %>% 
  # Make dates into characters
  dplyr::mutate(dplyr::across(.cols = dplyr::starts_with("date"),
                              .fns = as.character)) %>% 
  # Rename some of these columns
  dplyr::rename(site = siteid,
                treatment = plotid)
  
# Check structure
dplyr::glimpse(isutrt_v2)

# Load the full treatment table that others have been filling out
trttab <- read.csv(file = file.path("data", "treatment_table_year.csv"))

# Check structure
dplyr::glimpse(trttab)

# Bind this network's treatment info to the bottom of this
isutrt_v3 <- dplyr::bind_rows(trttab, isutrt_v2) %>% 
  # And filter to just this network
  dplyr::filter(network == net_name)
## This process creates all the empty columns in the right order for easy copy/pasting

# Check structure
dplyr::glimpse(isutrt_v3)

## ------------------------------------- ##
# Extract Site Coordinates ----
## ------------------------------------- ##

# Read in the site coordinate info too
coords_v1 <- readxl::read_xlsx(path = file.path("data", "raw_data", "td_20250827225852.xlsx"),
                               sheet = "Sites")

# Check structure
dplyr::glimpse(coords_v1)

# Get just the NW coordinates for this site
coords_v2 <- coords_v1 %>% 
  # Ditch bad header rows
  dplyr::filter(!siteid %in% c("description", "units")) %>% 
  # Make coordinates into numbers
  dplyr::mutate(dplyr::across(.cols = ends_with("itude"),
                              .fns = ~ as.numeric(.))) %>% 
  # Keep only desired columns
  dplyr::select(siteid, longitude, latitude) %>% 
  # Rename 'uniqeid' column
  dplyr::rename(site = siteid) %>% 
  # Add network name
  dplyr::mutate(network = net_name, .before = dplyr::everything()) 

# Check structure
dplyr::glimpse(coords_v2)

## ------------------------------------- ##
# Export ----
## ------------------------------------- ##

# Final structure check (of ANPP data)
dplyr::glimpse(isu_v3)

# Export to the pre-processed data folder
write.csv(x = isu_v3, na = '', row.names = F,
          file = file.path("data", "pre_processed_data", "isu-drainage_pre_process.csv"))

# Upload this to the Drive
googledrive::drive_upload(media = file.path("data", "pre_processed_data", "isu-drainage_pre_process.csv"), overwrite = T, path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1Sw-CdVIsCNvnS3laPn1a90WHoZsEoMif"))

# Final structure check of treatment info
dplyr::glimpse(isutrt_v3)

# Also export treatment info
write.csv(x = isutrt_v3, na = '', row.names = F,
          file = file.path("data", "isu-drainage_treatments.csv"))

# Final structure check of coordinates
dplyr::glimpse(coords_v2)

# Also export coordinate info
write.csv(x = coords_v2, na = '', row.names = F,
          file = file.path("data", "isu-drainage_coords.csv"))

## ------------------------------------- ##
# Prep Data Key ----
## ------------------------------------- ##

# For the ANPP data to be easily included in the harmonization workflow,
## We need the start of the data key for this file

# Begin the data key
isu_key <- ltertools::begin_key(raw_folder = file.path("data", "pre_processed_data")) %>% 
  # Filter to only this data file (in case this code is ever run with other stuff in that folder)
  dplyr::filter(source == "isu-drainage_pre_process.csv")

# Check structure
dplyr::glimpse(isu_key)

# Export
## Commenting out because it's unlikely this will be needed again
# write.csv(x = isu_key, na = '', row.names = F,
#           file = file.path("data", "isu_key-fragment.csv"))

# End ----
