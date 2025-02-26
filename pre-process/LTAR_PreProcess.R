###Pre-Processing for LTAR Data

##load libraries
library(tidyverse)
library(googledrive)
library(stringr)

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
abs$site_ID <- "ABS_UF"
abs$network <- "LTAR"

write.csv(abs, "./data/pre_processed_data/ABS_UF_BIR_NIFA_ANPP_Betsey.csv", row.names=FALSE)

rm(list = ls()); gc()

# CAF
# Add site and network name
caf <- read.csv("./data/raw/CAF_DET_20231218_MeasHarvestFraction.csv")
caf$site_ID <- "CAF"
caf$network <- "LTAR"

##Select only AGB
caf2 <- caf %>%
  filter(Plant.Fraction == "Aboveground biomass")

write.csv(caf, "./data/pre_processed_data/CAF_DET_20231218_MeasHarvestFraction.csv", row.names=FALSE)

rm(list = ls()); gc()

# CPER
# Add site and network name
cper <- read.csv("./data/raw/CPER_LTNPP_MeasGrazingPlants.csv")
cper$site_ID <- "CPER"
cper$network <- "LTAR"

# Filter out shrubs (SHRB) and sub-shrubs (SS)
cper2 <- cper %>%
  filter(!Functional.Groups %in% c("SS", "SHRB"))

# Summing by functional groups
cper

write.csv(cper2, "./data/pre_processed_data/CPER_LTNPP_MeasGrazingPlants.csv", row.names=FALSE)

rm(list = ls()); gc()

# ECB
# Add site and network name
ecb <- read.csv("./data/raw/ECB_MeasHarvestFractionv2.csv")

ecb$site_ID <- ecb$Unit.ID
ecb$network <- "LTAR"

##Fix treatment names (ECB_B1bau - ECB_E1BAU)
##Although, we won't use this treatment because it only has grain yield
ecb$Treatment.ID <- ifelse(ecb$Treatment.ID =="ECB_B1BAU", "ECB_E1BAU", ecb$Treatment.ID)

write.csv(ecb, "./data/pre_processed_data/ECB_MeasHarvestFractionv2.csv", row.names=FALSE)

rm(list = ls()); gc()

# GB
# Add site and network name
gb <- read.csv("./data/raw/GB_MeasGrazingPlants_04242024.csv")

# Update treatment ID
gb$Treatment.ID <- substring(gb$Unit.ID,1,6)

# Make site name the treatment ID name too 
# Site/treatment is between thses different vegetation types
gb$site_ID <- gb$Treatment.ID
gb$network <- "LTAR"

# Remove litter from the calculation file
gb2 <- gb %>%
  filter(Functional.Groups!="litter")

write.csv(gb2, "./data/pre_processed_data/GB_MeasGrazingPlants_04242024.csv", row.names=FALSE)

rm(list = ls()); gc()

# KBS
# Add site and network name
kbs <- read.csv("./data/raw/KBS_ANPP.csv")
kbs$site <- "KBS_LTAR"

# BIOMASS IS IN G/M2
# Update error in treatment name (KBS_T21 should be T2)
kbs$Treatment.ID <- ifelse(kbs$Treatment.ID =="KBS_T21", "KBS_T2", kbs$Treatment.ID)

write.csv(kbs, "./data/pre_processed_data/KBS_ANPP.csv", row.names=FALSE)

rm(list = ls()); gc()

# LCB - will add/do later

# NH
# Add site and network name
nh <- read.csv("./data/raw/UMRB_MeasHarvestFrac_v2.csv")
nh$site <- "NH_LTAR"

write.csv(nh, "./data/pre_processed_data/UMRB_MeasHarvestFrac_v2.csv", row.names=FALSE)

rm(list = ls()); gc()

# NP_C
# Add site and network name
np_c <- read.csv("./data/raw/NPMA_MeasHarvestFraction.csv")
np_c$site <- "NP_C_LTAR"

write.csv(np_c, "./data/pre_processed_data/NPMA_MeasHarvestFraction.csv", row.names=FALSE)

rm(list = ls()); gc()

# NP_R
np_r <- read.csv("./data/raw/NP_InOut_MeasGrazingPlants.csv")
np_r$site <- "NP_R_LTAR"

write.csv(np_r, "./data/pre_processed_data/NP_InOut_MeasGrazingPlants.csv", row.names=FALSE)

rm(list = ls()); gc()

# PRHPA NEMERREN
prhpa.nem <- read.csv("./data/raw/PRHPA_NEMERREM_MeasHarvestFraction.csv")
prhpa.nem$site <- "PRHPA_LTAR"

write.csv(prhpa.nem, "./data/pre_processed_data/PRHPA_NEMERREM_MeasHarvestFraction.csv", row.names=FALSE)

rm(list = ls()); gc()

# PRHPA NEMELTCRS
prhpa.nemel <- read.csv("./data/raw/PRHPA_NEMELTCRS_MeasResidueMgnt.csv")
prhpa.nemel$site <- "PRHPA_LTAR"

write.csv(prhpa.nemel, "./data/pre_processed_data/PRHPA_NEMELTCRS_MeasResidueMgnt.csv", row.names=FALSE)

rm(list = ls()); gc()

# SP
sp <- read.csv("./data/raw/SP_RotGraz_MeasGrazingPlants_OLH.csv")
sp$site <- "SP_LTAR"

write.csv(sp, "./data/pre_processed_data/SP_RotGraz_MeasGrazingPlants_OLH.csv", row.names=FALSE)

rm(list = ls()); gc()

# TG
tg <- read.csv("./data/raw/TG_GSWRL_LTBE_MeasHarvestFractionv2.csv")
tg$site <- "TG_LTAR"

write.csv(tg, "./data/pre_processed_data/TG_GSWRL_LTBE_MeasHarvestFractionv2.csv", row.names=FALSE)

rm(list = ls()); gc()

# UCB - Not doing now

# UMRB
umrb <- read.csv("./data/raw/UMRB_AMES_IAKFT_MeasHarvestFraction.csv")
umrb$site <- "UMRB_LTAR"

write.csv(umrb, "./data/pre_processed_data/UMRB_AMES_IAKFT_MeasHarvestFraction.csv", row.names=FALSE)

rm(list = ls()); gc()

# Move the pre_processed_data folder up to the drive
files_upload <- dir(path = file.path("data", "pre_processed_data"))
files_upload <- data.frame(files_upload)


purrr::walk2(.x = file.path("data", "pre_processed_data", files_upload$files_upload),
             .f = ~ googledrive::drive_upload(media = .x, overwrite = T,
                                              path = googledrive::as_id("https://drive.google.com/drive/u/1/folders/1nPqsPO5oxzMoMCLF4USSrZc56ZbtZHND")))
             
googledrive::drive_upload(media = file.path("data", "pre_processed_data", files_upload$files_upload ), overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/u/1/folders/1nPqsPO5oxzMoMCLF4USSrZc56ZbtZHND"))

