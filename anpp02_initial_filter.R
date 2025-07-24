## ----------------------------------------------------------------- ##
# Resilience Management - Baseline ANPP Filter Workflow
## ----------------------------------------------------------------- ##
# Authors: Nick J Lyon, Olivia Hajek, ...

# Purpose
## remove unwanted treatments and sites
## Unwanted treatments - primarily LTAR
## Non-USA sites from Nut-Net

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
# Download Harmonized Data ----
## ------------------------------------------- ##

# NOTE
## This script assumes (1) access to the "LTER-WG_Resilience-Management" Shared Drive (2) authentication with R
## For more information on authentication, see the following tutorial:
### https://lter.github.io/scicomp/tutorial_googledrive-pkg.html

# Identify desired file
focal_file <- "01_resilience_harmonized.csv"

# Download harmonized data file
googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/1/folders/13Ymkrr-kRLDmpaj1jwwVOnOSmEYnF-dJ")) %>% 
  dplyr::filter(name == focal_file) %>% 
  googledrive::drive_download(file = .$id, overwrite = T,
                              path = file.path("data", "harmonized_data", .$name))

# Read in harmonized data
harm_anpp <- read.csv(file = file.path("data", "harmonized_data", focal_file))

# Check structure
dplyr::glimpse(harm_anpp)

## ------------------------------------------- ##
#Filter Baseline ANPP Data ----
## ------------------------------------------- ##

#################################
# 1) Remove non-USA NutNet sites
#################################
##already done in pre-processing

#################################
# 2) Remove unwanted LTAR treatments (irrigated, mid-season harvest)
#################################

# Grab excluded treatmetn file
excl_trt_drive <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/1/folders/1Ty7QX7vyvD797eKJzMWbr8AwIo-GyBFO")) %>%
  dplyr::filter(name == "excluded_treatments")

# Did that work?
excl_trt_drive

# Download the excluded treatmetn file
googledrive::drive_download(file = excl_trt_drive$id, overwrite = T, type = "csv",
                            path = file.path("data", excl_trt_drive$name))

# Read in excluded treatment file
excl_trt <- read.csv(file = file.path("data", "excluded_treatments.csv"))

# Remove treatments found in the excl_trt file

harm_anpp.3 <- harm_anpp%>%
  filter(!treatment %in% excl_trt$treatment)

#################################
# 3) Remove unpublished LTAR datasets
#################################

# Grab excluded treatmetn file
pub_trt_ltar <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/1/folders/1Ty7QX7vyvD797eKJzMWbr8AwIo-GyBFO")) %>%
  dplyr::filter(name == "LTAR_published_trts.csv")

# Did that work?
pub_trt_ltar 

# Download the excluded treatmetn file
googledrive::drive_download(file = pub_trt_ltar$id, overwrite = T, type = "csv",
                            path = file.path("data", pub_trt_ltar$name))

# Read in excluded treatment file
pub_trt2 <- read.csv(file = file.path("data", "LTAR_published_trts.csv"))

# Remove treatments that aren't published
str(harm_anpp.3)
pubtrt <- pub_trt2$treatments
ltar <- harm_anpp.3 %>%
  dplyr::filter(network=="LTAR") %>%
  dplyr::filter(treatment %in% pub_trt2$treatments)

# Create dataset without LTAR 
nutnetlter <- harm_anpp.3 %>%
  dplyr::filter(network %in% c("LTER", "lter","NutNet", "LTAR and LTER") | site == "CPER")

harm_anpp.4 <- rbind(ltar, nutnetlter)

## ------------------------------------------- ##
# Export ----
## ------------------------------------------- ##

# Final pre-export tweaks
harm_anpp.99 <- harm_anpp.4

# Check structure
dplyr::glimpse(harm_anpp.99)

# Export locally
write.csv(x = harm_anpp.99, row.names = F, na = '',
          file = file.path("data", "harmonized_data", "02_anpp_filter1.csv"))

# Upload to Drive
googledrive::drive_upload(media = file.path("data", "harmonized_data", "02_anpp_filter1.csv"), overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/13Ymkrr-kRLDmpaj1jwwVOnOSmEYnF-dJ"))


# End ----

