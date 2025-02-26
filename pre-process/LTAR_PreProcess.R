###Pre-Processing for LTAR Data

##load libraries
library(tidyverse)
library(googledrive)

## Make needed folder(s)
dir.create(file.path("data"), showWarnings = F)
dir.create(file.path("data", "raw"), showWarnings = F)

# Identify wanted files
files_drive <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1Sw-CdVIsCNvnS3laPn1a90WHoZsEoMif")) %>% 
  dplyr::filter(stringr::str_detect(string = .$name, pattern = "\\.csv"))

# Did that work?
print(files_drive, n=30)

##ABS_RCREC
# ABS_UF
# CAF
# CPER
# ECB_E1
# ECB_D2
# ECB_C1
# GB_wbs
# GB_los
# GB_mbs
# JER
# KBS
# LCB
# NH
# NP_C
# NP_R
# PRHPA
# SP
# TG
# UCB
# UMRB
