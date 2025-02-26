###Pre-Processing for LTAR Data

##load libraries
library(tidyverse)
library(googledrive)

# Make needed folder(s)
dir.create(file.path("data"), showWarnings = F)
dir.create(file.path("data", "raw"), showWarnings = F)
dir.create(file.path("data", "pre_processed_data"), showWarnings = F)

# Identify wanted files
files_drive <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/1/folders/1zI1KYBlROyBZSgjSEYmVjsIfCmRPpUPq")) %>% 
  dplyr::filter(stringr::str_detect(string = .$name, pattern = "\\.csv"))

# Did that work?
print(files_drive, n=40)

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

# ABS_UF
# Add site and network name
abs <- read.csv("./data/raw/ABS_UF_BIR_NIFA_ANPP_Betsey.csv")
abs$site <- "abs_ltar"

write.csv(abs, "./data/pre_processed_data/ABS_UF_BIR_NIFA_ANPP_Betsey.csv")

rm(list = ls()); gc()

# CAF
# Add site and network name
caf <- read.csv("./data/raw/CAF_DET_20231218_MeasHarvestFraction.csv")
caf$site <- "caf_ltar"

write.csv(caf, "./data/pre_processed_data/CAF_DET_20231218_MeasHarvestFraction.csv")

rm(list = ls()); gc()

# CPER
cper <- read.csv("./data/raw/CPER_LTNPP_MeasGrazingPlants.csv")
cper$site <- "cper_ltar"

write.csv(cper, "./data/pre_processed_data/CPER_LTNPP_MeasGrazingPlants.csv")

rm(list = ls()); gc()

# ECB


# GB
# KBS
kbs <- read.csv("./data/raw/KBS_ANPP.csv")
kbs$site <- "kbs_ltar"

write.csv(kbs, "./data/pre_processed_data/KBS_ANPP.csv")

rm(list = ls()); gc()

# LCB

# NH
nh <- read.csv("./data/raw/UMRB_MeasHarvestFrac_v2.csv")
nh$site <- "nh_ltar"

write.csv(nh, "./data/pre_processed_data/UMRB_MeasHarvestFrac_v2.csv")

rm(list = ls()); gc()

# NP_C
caf <- read.csv("./data/raw/NPMA_MeasHarvestFraction.csv")
caf$site <- "caf_ltar"

write.csv(abs, "./data/pre_processed_data/NPMA_MeasHarvestFraction.csv")

rm(list = ls()); gc()

# NP_R
np_r <- read.csv("./data/raw/NP_InOut_MeasGrazingPlants.csv")
np_r$site <- "np_r_ltar"

write.csv(np_r, "./data/pre_processed_data/NP_InOut_MeasGrazingPlants.csv")

rm(list = ls()); gc()

# PRHPA NEMERREN
prhpa.nem <- read.csv("./data/raw/PRHPA_NEMERREM_MeasHarvestFraction.csv")
prhpa.nem$site <- "prhpa_ltar"

write.csv(prhpa.nem, "./data/pre_processed_data/PRHPA_NEMERREM_MeasHarvestFraction.csv")

rm(list = ls()); gc()

# PRHPA NEMELTCRS
prhpa.nemel <- read.csv("./data/raw/PRHPA_NEMELTCRS_MeasResidueMgnt.csv")
prhpa.nemel$site <- "prhpa_ltar"

write.csv(prhpa.nemel, "./data/pre_processed_data/PRHPA_NEMELTCRS_MeasResidueMgnt.csv")

rm(list = ls()); gc()

# SP
sp <- read.csv("./data/raw/SP_RotGraz_MeasGrazingPlants_OLH.csv")
sp$site <- "sp_ltar"

write.csv(sp, "./data/pre_processed_data/SP_RotGraz_MeasGrazingPlants_OLH.csv")

rm(list = ls()); gc()

# TG
tg <- read.csv("./data/raw/TG_GSWRL_LTBE_MeasHarvestFractionv2.csv")
tg$site <- "tg_ltar"

write.csv(tg, "./data/pre_processed_data/TG_GSWRL_LTBE_MeasHarvestFractionv2.csv")

rm(list = ls()); gc()

# UCB - Not doing now

# UMRB
umrb <- read.csv("./data/raw/TG_GSWRL_LTBE_MeasHarvestFractionv2.csv")
umrb$site <- "umrb_ltar"

write.csv(umrb, "./data/pre_processed_data/TG_GSWRL_LTBE_MeasHarvestFractionv2.csv")

rm(list = ls()); gc()

