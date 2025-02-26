## ------------------------------------------------------ ##
# Pre-Processing - NutNet
## ------------------------------------------------------ ##
# Author(s): Nick J Lyon, ...

# Purpose
## 

## -------------------------------------------- ## 
# Housekeeping ----
## -------------------------------------------- ## 

# Load needed libraries
librarian::shelf(tidyverse, ltertools, googledrive)

# Make needed folder(s)
dir.create(file.path("data"), showWarnings = F)
dir.create(file.path("data", "raw"), showWarnings = F)
dir.create(file.path("data", "pre_processed_data"), showWarnings = F)

# Clear environment
rm(list = ls()); gc()

## -------------------------------------------- ## 
# Download Raw Data ----
## -------------------------------------------- ## 

# Identify raw files in Drive
drive_raw <- googledrive::drive_ls(path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1zI1KYBlROyBZSgjSEYmVjsIfCmRPpUPq"))

# Check that worked
drive_raw

# Download them
purrr::walk2(.x = drive_raw$id, .y = drive_raw$name,
             .f = ~ googledrive::drive_download(file = .x, overwrite = T,
                                                path = file.path("data", "raw", .y)))

## -------------------------------------------- ## 
# Pre-Process "NutNet_Fay_et_al_2025.csv" ----
## -------------------------------------------- ## 

# Read in relevant file
nn_fay <- read.csv(file = file.path("data", "raw", "NutNet_Fay_et_al_2025.csv"))

# Check structure
dplyr::glimpse(nn_fay)

# Do wanted pre-processing
nn_fay_pp <- nn_fay %>% 
  # Separate country information
  dplyr::mutate(country = stringr::str_sub(string = site_code,
                                           start = nchar(site_code) - 1,
                                           end = nchar(site_code)),
                .before = site_code) %>% 
  # Subset to only desired countries
  dplyr::filter(country %in% c("us", "ca")) %>% 
  # Add network column
  dplyr::mutate(network = "NutNet", .before = dplyr::everything())
  
# Re-check structure
dplyr::glimpse(nn_fay_pp)
## view(nn_fay_pp)

# Export locally
write.csv(x = nn_fay_pp, na = '', row.names = F,
          file = file.path("data", "pre_processed_data", "nutnet_fay_2025_pre-process.csv"))

## -------------------------------------------- ## 
# Upload Pre-Processed Data ----
## -------------------------------------------- ## 

# Identify local files
( local_pp <- dir(path = file.path("data", "pre_processed_data")) )

# Upload them
purrr::walk(.x = local_pp,
            .f = ~ googledrive::drive_upload(media = file.path("data", "pre_processed_data", .x), overwrite = T, path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1Sw-CdVIsCNvnS3laPn1a90WHoZsEoMif")))


# End ----
