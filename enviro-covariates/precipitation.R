## ----------------------------------------------------------------- ##
# Resilience Management - Precipitation Aggregation Workflow
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
dir.create(file.path("data", "environment"), showWarnings = F)

# Clear environment + collect garbage
rm(list = ls()); gc()

## --------------------------------------- ##
# Download Precip Data ----
## --------------------------------------- ##

# Identify files from Drive
drive_precip <- googledrive::drive_ls(path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1x4titHOkl65dUBE6HJHTVE31iO01ZBbt"))

# Look good?
drive_precip

# Download locally
purrr::walk2(.x = drive_precip$id, .y = drive_precip$name,
             .f = ~ googledrive::drive_download(file = .x, overwrite = T,
                                                path = file.path("data", "environment", .y)))

## --------------------------------------- ##
# Combine Precip ----
## --------------------------------------- ##

# Read in these files
ppt_v1 <- drive_precip$name %>% 
  purrr::map(.x = ., .f = ~ read.csv(file = file.path("data", "environment", .))) %>% 
  purrr::list_rbind(x = .) %>% 
  # Do some minor column renaming too
  dplyr::rename(site = site_id,
                network = project_id) %>% 
  dplyr::mutate(network = ifelse(is.na(network), yes = "NutNet", no = network))

# Check structure
dplyr::glimpse(ppt_v1)

## --------------------------------------- ##
# Aggregate Precip - Annual ----
## --------------------------------------- ##

# Begin with annual aggregation
ppt_annual <- ppt_v1 %>% 
  dplyr::group_by(network, site, year) %>% 
  dplyr::summarize(annual_precip_mm = mean(precip, na.rm = T),
                   .groups = "keep") %>% 
  dplyr::ungroup()

# Check structure
dplyr::glimpse(ppt_annual)

## --------------------------------------- ##
# Export ----
## --------------------------------------- ##

# Export locally
write.csv(x = ppt_annual, na = '', row.names = F,
          file = file.path("data", "environment", "precip_annual-means.csv"))

# End ----
