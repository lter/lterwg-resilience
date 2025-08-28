## --------------------------------------------------------------- ##
# Sustainable Corn CAP Data Pre-Processing
## --------------------------------------------------------------- ##
# Script author(s): Nick J Lyon, 

# Purpose
## Pre-process Sustainable Corn CAP data to be ready for inclusion in the harmonization workflow

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
net_name <- "CSCAP"

## ------------------------------------- ##
# Download Data ----
## ------------------------------------- ##

# If the data don't exist locally, download 'em
if("cscap_20250827212711.xlsx" %in% dir(path = file.path("data", "raw_data")) != T){
  
  # Identify data in Drive
  drive_cscap <- googledrive::drive_ls(path = googledrive::as_id("https://drive.google.com/drive/folders/1iyCpiv06PdcVzU99VPuN-FSDwAkwv-jJ")) %>% 
    dplyr::filter(stringr::str_detect(string = name, pattern = "cscap_"))
  
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
cap_v1 <- readxl::read_xlsx(path = file.path("data", "raw_data", "cscap_20250827212711.xlsx"),
                            sheet = "Agronomic")

# Check structure
dplyr::glimpse(cap_v1)

# Begin processing
cap_v2 <- cap_v1 %>% 
  # Fix column names
  dplyr::rename(corn_veg_biomass_kgha = AGR04,
                soy_veg_biomass_kgha = AGR05,
                cover_crop_biomass_kgha = AGR07,
                corn_grain_biomass_kgha = AGR33,
                soy_grain_biomass_kgha = AGR34) %>% 
  # Ditch bad header rows
  dplyr::filter(!uniqueid %in% c("description", "units")) %>% 
  # Replace null values with true NAs
  dplyr::mutate(dplyr::across(.cols = dplyr::ends_with("_kgha"),
                              .fns = ~ ifelse(. == -999, yes = NA, no = .))) %>% 
  # Make numeric columns into real numbers
  dplyr::mutate(dplyr::across(.cols = dplyr::ends_with("_kgha"),
                              .fns = ~ as.numeric(.)))

# Check structure
dplyr::glimpse(cap_v2)

## ------------------------------------- ##
# Process Data ----
## ------------------------------------- ##

# Now that the data are in tidy form, we can do the real processing
cap_v3 <- cap_v2 %>% 
  # Keep only rows that have both veg and grain biomass for one of the crop types
  dplyr::filter((!is.na(corn_veg_biomass_kgha) & !is.na(corn_grain_biomass_kgha)) |
                  (!is.na(soy_veg_biomass_kgha) & !is.na(soy_grain_biomass_kgha))) %>% 
  # Flip to long format
  tidyr::pivot_longer(cols = dplyr::starts_with(c("corn_", "soy_")),
                      names_to = "names", values_to = "biomass_kgha") %>% 
  # Remove NAs this introduces in the biomass column
  dplyr::filter(!is.na(biomass_kgha)) %>% 
  # Make a crop column
  dplyr::mutate(crop = ifelse(stringr::str_detect(string = names, pattern = "corn"),
                              yes = "corn", no = "soybean"),
                .after = year) %>% 
  # Sum veg and grain biomass (i.e., within new crop column)
  dplyr::group_by(uniqueid, plotid, year, crop, cover_crop_biomass_kgha) %>% 
  dplyr::summarize(anpp_kgha = sum(biomass_kgha, na.rm = T),
                   .groups = "keep") %>% 
  dplyr::ungroup() %>% 
  # Add network name
  dplyr::mutate(network = net_name, .before = dplyr::everything()) 

# Check structure
dplyr::glimpse(cap_v3)

## ------------------------------------- ##
# Extract Treatment Table Info ----
## ------------------------------------- ##

# We also want some information for the 'treatment_table_year' file
captrt_v1 <- cap_v3 %>% 
  # Make cover crop just yes/no
  dplyr::mutate(cover_crop = ifelse(!is.na(cover_crop_biomass_kgha),
                                    yes = 1, no = 0)) %>% 
  # Ditch ANPP column
  dplyr::select(-anpp_kgha) %>% 
  # Arrange by site/plot/year
  dplyr::arrange(uniqueid, plotid, year)

# Check structure
dplyr::glimpse(captrt_v1)

# Read in the 'Field Operations' sheet of the data file
ops_v1 <- readxl::read_xlsx(path = file.path("data", "raw_data", "cscap_20250827212711.xlsx"),
                            sheet = "Field Operations")

# Check structure
dplyr::glimpse(ops_v1)

# Grab just what we want from that sheet
ops_v2 <- ops_v1 %>% 
  # Ditch bad header rows
  dplyr::filter(!uniqueid %in% c("description", "units")) %>% 
  # Keep only operations that relate to corn/soy
  dplyr::filter(stringr::str_detect(string = operation, pattern = "corn") |
                  stringr::str_detect(string = operation, pattern = "soy")) %>% 
  # Pare down to only needed columns
  dplyr::select(uniqueid, cropyear, operation, date) %>% 
  # Fix stupid Excel date issue
  dplyr::mutate(date = as.Date(as.numeric(date), origin = "1899-12-30")) %>% 
  # Separate 'operation' into crop versus event
  tidyr::separate_wider_delim(cols = operation, delim = "_", names = c("event", "crop")) %>% 
  # Drop non-unique rows
  dplyr::distinct() %>% 
  # NOTE JUDGEMENT CALL HERE (vvv)
  # There are two plant/harvest dates for corn at SERF from 2012-15
  # We need there to be one so I'm taking the first
  dplyr::group_by(uniqueid, cropyear, crop, event) %>% 
  dplyr::summarize(date = dplyr::first(date),
                   .groups = "keep") %>% 
  dplyr::ungroup() %>% 
  # NOTE JUDGEMENT CALL HERE (^^^)
  # Reshape wide
  tidyr::pivot_wider(names_from = event, values_from = date)

# Check structure
dplyr::glimpse(ops_v2)

# Combine these pieces of metadata
captrt_v2 <- captrt_v1 %>% 
  # Attach harvest/planting dates
  dplyr::left_join(y = ops_v2, by = c("uniqueid", "year" = "cropyear", "crop")) %>% 
  # Rename columns slightly
  dplyr::rename(site = uniqueid, 
                date_harvest = harvest,
                date_plant = plant,
                cover_crop_biomass = cover_crop_biomass_kgha) %>% 
  # Combine site and plot to make 'treatment
  dplyr::mutate(treatment = paste0(site, "_", plotid), .after = site) %>% 
  # Ditch old plot
  dplyr::select(-plotid) %>% 
  # Add network name
  dplyr::mutate(network = net_name, .before = dplyr::everything()) %>% 
  # Make date back into a character
  dplyr::mutate(dplyr::across(.cols = dplyr::starts_with("date"),
                              .fns = as.character))

# Check structure
dplyr::glimpse(captrt_v2)

# Load the full treatment table that others have been filling out
trttab <- read.csv(file = file.path("data", "treatment_table_year.csv"))

# Check structure
dplyr::glimpse(trttab)

# Bind this network's treatment info to the bottom of this
captrt_v3 <- dplyr::bind_rows(trttab, captrt_v2) %>% 
  # And filter to just this network
  dplyr::filter(network == net_name)
## This process creates all the empty columns in the right order for easy copy/pasting

# Check structure
dplyr::glimpse(captrt_v3)

## ------------------------------------- ##
# Extract Site Coordinates ----
## ------------------------------------- ##

# Read in the site coordinate info too
coords_v1 <- readxl::read_xlsx(path = file.path("data", "raw_data", "cscap_20250827212711.xlsx"),
                               sheet = "Site Metadata")

# Check structure
dplyr::glimpse(coords_v1)

# Get just the NW coordinates for this site
coords_v2 <- coords_v1 %>% 
  # Ditch bad header rows
  dplyr::filter(!uniqueid %in% c("description", "units")) %>% 
  # Rename wanted columns
  dplyr::rename(longitude = `NW Lon`,
                latitude = `NW Lat`) %>% 
  # Make coordinates into numbers
  dplyr::mutate(dplyr::across(.cols = ends_with("itude"),
                              .fns = ~ as.numeric(.))) %>% 
  # Keep only desired columns
  dplyr::select(uniqueid:latitude) %>% 
  # Rename 'uniqeid' column
  dplyr::rename(site = uniqueid) %>% 
  # Add network name
  dplyr::mutate(network = net_name, .before = dplyr::everything()) 

# Check structure
dplyr::glimpse(coords_v2)

## ------------------------------------- ##
# Export ----
## ------------------------------------- ##

# Final structure check (of ANPP data)
dplyr::glimpse(cap_v3)

# Export to the pre-processed data folder
write.csv(x = cap_v3, na = '', row.names = F,
          file = file.path("data", "pre_processed_data", "cscap_pre_process.csv"))

# Final structure check of treatment info
dplyr::glimpse(captrt_v3)

# Also export treatment info
write.csv(x = captrt_v3, na = '', row.names = F,
          file = file.path("data", "cscap_treatments.csv"))

# Final structure check of coordinates
dplyr::glimpse(coords_v2)

# Also export coordinate info
write.csv(x = coords_v2, na = '', row.names = F,
          file = file.path("data", "cscap_coords.csv"))

## ------------------------------------- ##
# Prep Data Key ----
## ------------------------------------- ##

# For the ANPP data to be easily included in the harmonization workflow,
## We need the start of the data key for this file

# Begin the data key
cscap_key <- ltertools::begin_key(raw_folder = file.path("data", "pre_processed_data")) %>% 
  # Filter to only this data file (in case this code is ever run with other stuff in that folder)
  dplyr::filter(source == "cscap_pre_process.csv")

# Check structure
dplyr::glimpse(cscap_key)

# Export
## Commenting out because it's unlikely this will be needed again
# write.csv(x = cscap_key, na = '', row.names = F,
#           file = file.path("data", "cscap_key-fragment.csv"))

# End ----
