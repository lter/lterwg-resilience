## ------------------------------------------------------ ##
# Pre-Processing - LTAR
## ------------------------------------------------------ ##
# Author(s): Olivia Hajek, Nick J Lyon, ...

# Purpose
## Do needed pre-processing for (some) LTAR files
## See sub-sections below for information on needed alterations for each file

## -------------------------------------------- ## 
# Housekeeping ----
## -------------------------------------------- ## 

# Load needed libraries
librarian::shelf(tidyverse, googledrive, supportR)

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
# Pre-Process "ABS_UF" ----
## -------------------------------------------- ## 

# Needed pre-processing:
## Add network / site ID

# Read in data
absuf_raw <- read.csv(file = file.path("data", "raw", "ABS_UF_BIR_NIFA_ANPP_Betsey.csv"))

# Check structure
dplyr::glimpse(absuf_raw)

# Make needed repairs
absuf_pp <- absuf_raw %>% 
  # Drop columns that are entirely NA
  dplyr::select(-dplyr::where(fn = ~ all(is.na(.) | nchar(.) == 0))) %>% 
  # Add desired column(s)
  dplyr::mutate(network = "LTAR", site_ID = "ABS_UF",
                .before = dplyr::everything())

# Check for gained/lost columns
supportR::diff_check(old = names(absuf_raw), new = names(absuf_pp))

# Re-check structure
dplyr::glimpse(absuf_pp)

# Export locally
write.csv(x = absuf_pp, na = '', row.names = F,
          file = file.path("data", "pre_processed_data", "LTAR_abs-uf_pre-process.csv"))

# Clear environment
rm(list = ls()); gc()

## -------------------------------------------- ## 
# Pre-Process "CAF" ----
## -------------------------------------------- ## 

# Needed pre-processing:
## CAF - using "plant fraction" and "Frac.Dr.Matt", make a column for grain_kg_ha and anpp_kg_ha based on plant fraction and the biomass value

# Read in file
caf_raw <- read.csv(file = file.path("data", "raw", "CAF_DET_20231218_MeasHarvestFraction.csv"))

# Check structure
dplyr::glimpse(caf_raw)

# Do needed repairs
caf_pp <- caf_raw %>% 
  # Drop columns that are entirely NA
  dplyr::select(-dplyr::where(fn = ~ all(is.na(.) | nchar(.) == 0))) %>% 
  # Add desired column(s)
  dplyr::mutate(network = "LTAR", site_ID = "CAF",
                .before = dplyr::everything()) %>% 
  # Drop unwanted column(s)
  dplyr::select(-Frac.Dry.Matt.STD.kg.ha) %>% 
  # Drop missing plant fractions / dry matt values
  dplyr::filter(!is.na(Plant.Fraction) & !is.na(Frac.Dry.Matt.kg.ha)) %>% 
  # Pivot biomass to wide format
  tidyr::pivot_wider(names_from = Plant.Fraction, 
                     values_from = Frac.Dry.Matt.kg.ha) %>% 
  # Make desired new column(s)
  dplyr::rename(grain_kg_ha = Grain,
                anpp_kg_ha = `Aboveground biomass`)

# What columns are lost/gained
supportR::diff_check(old = names(caf_raw), new = names(caf_pp))

# Re-check structure
dplyr::glimpse(caf_pp)

# Export locally
write.csv(x = caf_pp, na = '', row.names = F,
          file = file.path("data", "pre_processed_data", "LTAR_caf_pre-process.csv"))

# Clear environment / collect garbage
rm(list = ls()); gc()

## -------------------------------------------- ## 
# Pre-Process "CPER" ----
## -------------------------------------------- ## 

# Needed pre-processing:
## sum Above.Gr.Bio by Unit_ID and year/date

# Read in data
cper_raw <- read.csv(file = file.path("data", "raw", "CPER_LTNPP_MeasGrazingPlants.csv"))

# Check structure
dplyr::glimpse(cper_raw)

# Make needed repairs
cper_pp <- cper_raw %>% 
  # Drop columns that are entirely NA
  dplyr::select(-dplyr::where(fn = ~ all(is.na(.) | nchar(.) == 0))) %>% 
  # Add desired column(s)
  dplyr::mutate(network = "LTAR", site_ID = "CPER",
                .before = dplyr::everything()) %>% 
  # Filter out unwanted groups
  dplyr::filter(!Functional.Groups %in% c("SS", "SHRB")) %>% 
  # Sum biomass within relevant categorical columns
  dplyr::group_by(dplyr::across(dplyr::all_of(setdiff(x = names(.), 
                                        y = c("Growth.Stage", "Functional.Groups", "AboveGr.Bio.kg.ha..dry."))))) %>% 
  dplyr::summarize(biomass_kg_ha = sum(AboveGr.Bio.kg.ha..dry., na.rm = T)) %>% 
  dplyr::ungroup()

# Check for gained/lost columns
supportR::diff_check(old = names(cper_raw), new = names(cper_pp))

# Re-check structure
dplyr::glimpse(cper_pp)

# Export locally
write.csv(x = cper_pp, na = '', row.names = F,
          file = file.path("data", "pre_processed_data", "LTAR_cper_pre-process.csv"))

# Clear environment
rm(list = ls()); gc()

## -------------------------------------------- ## 
# Pre-Process "ECB" ----
## -------------------------------------------- ## 

# Needed pre-processing:
## ECB -  Grain + Stems for Wheat and Aboveground biomass for Wheat, Aboveground biomass is cron (use column H - Frac.Dry.Matter)

# Read in data
ecb_raw <- read.csv(file = file.path("data", "raw", "ECB_MeasHarvestFractionv2.csv"))

# Check structure
dplyr::glimpse(ecb_raw)

# Make needed repairs
ecb_pp <- ecb_raw %>% 
  # Drop columns that are entirely NA
  dplyr::select(-dplyr::where(fn = ~ all(is.na(.) | nchar(.) == 0))) %>% 
  # Add desired column(s)
  dplyr::mutate(network = "LTAR", site_ID = "ECB",
                .before = dplyr::everything()) %>% 
  # Handle some treatment ID weirdness
  dplyr::mutate(Treatment.ID = ifelse(Treatment.ID == "ECB_B1BAU",
                                      # Note change from "_B" to "_E"
                                      yes = "ECB_E1BAU",
                                      no = Treatment.ID))

# Check for gained/lost columns
supportR::diff_check(old = names(ecb_raw), new = names(ecb_pp))

# Re-check structure
dplyr::glimpse(ecb_pp)

# Export locally
write.csv(x = ecb_pp, na = '', row.names = F,
          file = file.path("data", "pre_processed_data", "LTAR_ecb_pre-process.csv"))

# Clear environment
rm(list = ls()); gc()

## -------------------------------------------- ## 
# Pre-Process "GB" ----
## -------------------------------------------- ## 

# Needed pre-processing:
## sum Above.Gr.Bio by unit ID and year/date

# Read in data
gb_raw <- read.csv(file = file.path("data", "raw", "GB_MeasGrazingPlants_04242024.csv"))

# Check structure
dplyr::glimpse(gb_raw)

# Make needed repairs
gb_pp <- gb_raw %>% 
  # Drop columns that are entirely NA
  dplyr::select(-dplyr::where(fn = ~ all(is.na(.) | nchar(.) == 0))) %>% 
  # Add desired column(s)
  dplyr::mutate(network = "LTAR", site_ID = "GB",
                .before = dplyr::everything()) %>% 
  # Tweak treatment ID column
  dplyr::mutate(Treatment.ID = stringr::str_sub(string = Unit.ID, 
                                                start = 1, end = 6)) %>% 
  # Drop unwanted column(s)
  dplyr::select(-Surface.Litter.kg.ha..dry.) %>% 
  # Drop unwanted functional groups
  dplyr::filter(Functional.Groups != "litter") %>% 
  # Sum through unit IDs
  dplyr::group_by(dplyr::across(dplyr::all_of(setdiff(x = names(.), y = c("Functional.Groups", "AboveGr.Bio.kg.ha..dry."))))) %>% 
  dplyr::summarize(biomass_kg_ha = sum(AboveGr.Bio.kg.ha..dry., na.rm = T)) %>% 
  dplyr::ungroup()

# Check for gained/lost columns
supportR::diff_check(old = names(gb_raw), new = names(gb_pp))

# Re-check structure
dplyr::glimpse(gb_pp)

# Export locally
write.csv(x = gb_pp, na = '', row.names = F,
          file = file.path("data", "pre_processed_data", "LTAR_gb_pre-process.csv"))

# Clear environment
rm(list = ls()); gc()

## -------------------------------------------- ## 
# Pre-Process "KBS" ----
## -------------------------------------------- ## 

# Needed pre-processing:
## Using frac.dry.matter, make anpp_g_m2 for those with WHOLE as plant fraction and grain_kg_ha for SEED
## For plots unit_id with multiple crops (mostly grasses), sum across the whole plants. Don’t think this will be relevant for the crops. 

# Read in data
kbs_raw <- read.csv(file = file.path("data", "raw", "KBS_ANPP.csv"))

# Check structure
dplyr::glimpse(kbs_raw)

# Make needed repairs
kbs_pp <- kbs_raw %>% 
  # Drop columns that are entirely NA
  dplyr::select(-dplyr::where(fn = ~ all(is.na(.) | nchar(.) == 0))) %>% 
  # Add desired column(s)
  dplyr::mutate(network = "LTAR", site_ID = "KBS",
                .before = dplyr::everything()) %>% 
  # Drop unwanted column(s)
  dplyr::select(-Frac.Moist..) %>% 
  # Fix one broken treatment column
  dplyr::mutate(Treatment.ID = ifelse(Treatment.ID =="KBS_T21", 
                                      yes = "KBS_T2", no = Treatment.ID)) %>% 
  # Identify instances of more than one crop
  dplyr::group_by(Treatment.ID, Sampling.Date) %>% 
  dplyr::mutate(crop = ifelse(length(unique(Crop)) > 1,
                              yes = "MIXED CROPS",
                              no = Crop),
                .after = Crop) %>% 
  dplyr::ungroup() %>% 
  # Summarize through new crop column
  dplyr::group_by(dplyr::across(dplyr::all_of(setdiff(x = names(.),
                                                      y = c("Crop", "Frac.Dry.Matt.kg.ha"))))) %>% 
  dplyr::summarize(biomass_kg_ha = sum(Frac.Dry.Matt.kg.ha, na.rm = T)) %>% 
  dplyr::ungroup() %>% 
  # Rotate types of biomass to wide format
  tidyr::pivot_wider(names_from = Plant.Fraction,
                     values_from = biomass_kg_ha) %>% 
  # Get desired grain vs. total biomass
  dplyr::rename(biomass_kg_ha = WHOLE,
                grain_kg_ha = SEED) %>% 
  # Drop unwanted columns
  dplyr::select(-STOVER, -LITTER, -`STOVER-NONLEAF`, -STOV_VEG, -STOV_REP)
  
# Check for gained/lost columns
supportR::diff_check(old = names(kbs_raw), new = names(kbs_pp))

# Re-check structure
dplyr::glimpse(kbs_pp)

# Export locally
write.csv(x = kbs_pp, na = '', row.names = F,
          file = file.path("data", "pre_processed_data", "LTAR_kbs_pre-process.csv"))

# Clear environment
rm(list = ls()); gc()

## -------------------------------------------- ## 
# Pre-Process "LCB" ----
## -------------------------------------------- ## 

# LCB - will add/do later

## -------------------------------------------- ## 
# Pre-Process "NH" ----
## -------------------------------------------- ## 

# Needed pre-processing:
## Add network / site ID
## use plant fraction and put it as either grain or full anppp

# Read in data
nh_raw <- read.csv(file = file.path("data", "raw", "UMRB_MeasHarvestFrac_v2.csv"))

# Check structure
dplyr::glimpse(nh_raw)

# Make needed repairs
nh_pp <- nh_raw %>% 
  # Drop columns that are entirely NA
  dplyr::select(-dplyr::where(fn = ~ all(is.na(.) | nchar(.) == 0))) %>% 
  # Add desired column(s)
  dplyr::mutate(network = "LTAR", site_ID = "NH",
                .before = dplyr::everything()) %>% 
  # Fill missing "growth stages"
  dplyr::mutate(Growth.Stage = ifelse(nchar(Growth.Stage) == 0,
                                      yes = "Combine Harvest", no = Growth.Stage)) %>% 
  # Summarize across duplicates
  dplyr::group_by(dplyr::across(dplyr::all_of(setdiff(x = names(.), y = c("Frac.Dry.Matt.kg.ha"))))) %>%
  dplyr::summarize(biomass = sum(Frac.Dry.Matt.kg.ha, na.rm = T)) %>%
  dplyr::ungroup() %>%
  # Rotate to wide format
  tidyr::pivot_wider(names_from = Plant.Fraction, 
                     values_from = biomass) %>% 
  # Rename resulting columns
  dplyr::rename(anpp_kg_ha = `Aboveground biomass`,
                grain_kg_ha = Grain)

# Check for gained/lost columns
supportR::diff_check(old = names(nh_raw), new = names(nh_pp))

# Re-check structure
dplyr::glimpse(nh_pp)

# Export locally
write.csv(x = nh_pp, na = '', row.names = F,
          file = file.path("data", "pre_processed_data", "LTAR_nh_pre-process.csv"))

# Clear environment
rm(list = ls()); gc()

## -------------------------------------------- ## 
# Pre-Process "NP_C" ----
## -------------------------------------------- ## 

# Needed pre-processing:
## For the same year and unit id, sum "Stover (all non-grain biomass)" and grain to get total ANPP, but ANPP can also be the aboveground biomass
## Maintain grain values in their own column

# Read in data
npc_raw <- read.csv(file = file.path("data", "raw", "NPMA_MeasHarvestFraction.csv"))

# Check structure
dplyr::glimpse(npc_raw)

# Make needed repairs
npc_pp <- npc_raw %>% 
  # Drop columns that are entirely NA
  dplyr::select(-dplyr::where(fn = ~ all(is.na(.) | nchar(.) == 0))) %>% 
  # Add desired column(s)
  dplyr::mutate(network = "LTAR", site_ID = "NP_C",
                .before = dplyr::everything()) %>% 
  # Remove missing plant fractions / biomass
  dplyr::filter(nchar(Plant.Fraction) != 0 & !is.na(Frac.Dry.Matt.kg.ha)) %>%
  # Pivot wider
  tidyr::pivot_wider(names_from = Plant.Fraction,
                     values_from = Frac.Dry.Matt.kg.ha) %>% 
  # Rename / consolidate resulting columns
  dplyr::rename(grain_kg_ha = Grain) %>% 
  dplyr::mutate(anpp_kg_ga = dplyr::case_when(
                  !is.na(`Aboveground biomass`) ~ `Aboveground biomass`,
                  !is.na(grain_kg_ha) & !is.na(`Stover (all non-grain biomass)`) ~ grain_kg_ha + `Stover (all non-grain biomass)`,
                  is.na(grain_kg_ha) & !is.na(`Stover (all non-grain biomass)`) ~ `Stover (all non-grain biomass)`,
                  !is.na(grain_kg_ha) & is.na(`Stover (all non-grain biomass)`) ~ grain_kg_ha,
                  T ~ NA)) %>% 
  # Drop superseded columns
  dplyr::select(-`Stover (all non-grain biomass)`, -`Aboveground biomass`)
  
# Check for gained/lost columns
supportR::diff_check(old = names(npc_raw), new = names(npc_pp))

# Re-check structure
dplyr::glimpse(npc_pp)

# Export locally
write.csv(x = npc_pp, na = '', row.names = F,
          file = file.path("data", "pre_processed_data", "LTAR_np-c_pre-process.csv"))

# Clear environment
rm(list = ls()); gc()

## -------------------------------------------- ## 
# Pre-Process "NP_R" ----
## -------------------------------------------- ## 

# Needed pre-processing:
## 

# Read in data
xx_raw <- read.csv(file = file.path("data", "raw", "xx.csv"))

# Check structure
dplyr::glimpse(xx_raw)

# Make needed repairs
xx_pp <- xx_raw %>% 
  # Drop columns that are entirely NA
  dplyr::select(-dplyr::where(fn = ~ all(is.na(.) | nchar(.) == 0))) %>% 
  # Add desired column(s)
  dplyr::mutate(network = "LTAR", site_ID = "XX",
                .before = dplyr::everything())

# Check for gained/lost columns
supportR::diff_check(old = names(xx_raw), new = names(xx_pp))

# Re-check structure
dplyr::glimpse(xx_pp)

# Export locally
write.csv(x = xx_pp, na = '', row.names = F,
          file = file.path("data", "pre_processed_data", "LTAR_xx_pre-process.csv"))

# Clear environment
rm(list = ls()); gc()

# NP_R
np_r <- read.csv("./data/raw/NP_InOut_MeasGrazingPlants.csv")
np_r$site <- "NP_R_LTAR"

write.csv(np_r, "./data/pre_processed_data/NP_InOut_MeasGrazingPlants.csv", row.names=FALSE)

rm(list = ls()); gc()


## -------------------------------------------- ## 
# Pre-Process "PRHPA_NEMERREN" ----
## -------------------------------------------- ## 

# Needed pre-processing:
## 

# Read in data
xx_raw <- read.csv(file = file.path("data", "raw", "xx.csv"))

# Check structure
dplyr::glimpse(xx_raw)

# Make needed repairs
xx_pp <- xx_raw %>% 
  # Drop columns that are entirely NA
  dplyr::select(-dplyr::where(fn = ~ all(is.na(.) | nchar(.) == 0))) %>% 
  # Add desired column(s)
  dplyr::mutate(network = "LTAR", site_ID = "XX",
                .before = dplyr::everything())

# Check for gained/lost columns
supportR::diff_check(old = names(xx_raw), new = names(xx_pp))

# Re-check structure
dplyr::glimpse(xx_pp)

# Export locally
write.csv(x = xx_pp, na = '', row.names = F,
          file = file.path("data", "pre_processed_data", "LTAR_xx_pre-process.csv"))

# Clear environment
rm(list = ls()); gc()

# PRHPA NEMERREN
prhpa.nem <- read.csv("./data/raw/PRHPA_NEMERREM_MeasHarvestFraction.csv")
prhpa.nem$site <- "PRHPA_LTAR"

write.csv(prhpa.nem, "./data/pre_processed_data/PRHPA_NEMERREM_MeasHarvestFraction.csv", row.names=FALSE)

rm(list = ls()); gc()

## -------------------------------------------- ## 
# Pre-Process "PRHPA_NEMELTCRS" ----
## -------------------------------------------- ## 

# Needed pre-processing:
## 

# Read in data
xx_raw <- read.csv(file = file.path("data", "raw", "xx.csv"))

# Check structure
dplyr::glimpse(xx_raw)

# Make needed repairs
xx_pp <- xx_raw %>% 
  # Drop columns that are entirely NA
  dplyr::select(-dplyr::where(fn = ~ all(is.na(.) | nchar(.) == 0))) %>% 
  # Add desired column(s)
  dplyr::mutate(network = "LTAR", site_ID = "XX",
                .before = dplyr::everything())

# Check for gained/lost columns
supportR::diff_check(old = names(xx_raw), new = names(xx_pp))

# Re-check structure
dplyr::glimpse(xx_pp)

# Export locally
write.csv(x = xx_pp, na = '', row.names = F,
          file = file.path("data", "pre_processed_data", "LTAR_xx_pre-process.csv"))

# Clear environment
rm(list = ls()); gc()

# PRHPA NEMELTCRS
prhpa.nemel <- read.csv("./data/raw/PRHPA_NEMELTCRS_MeasResidueMgnt.csv")
prhpa.nemel$site <- "PRHPA_LTAR"

write.csv(prhpa.nemel, "./data/pre_processed_data/PRHPA_NEMELTCRS_MeasResidueMgnt.csv", row.names=FALSE)

rm(list = ls()); gc()

## -------------------------------------------- ## 
# Pre-Process "SP" ----
## -------------------------------------------- ## 

# Needed pre-processing:
## 

# Read in data
xx_raw <- read.csv(file = file.path("data", "raw", "xx.csv"))

# Check structure
dplyr::glimpse(xx_raw)

# Make needed repairs
xx_pp <- xx_raw %>% 
  # Drop columns that are entirely NA
  dplyr::select(-dplyr::where(fn = ~ all(is.na(.) | nchar(.) == 0))) %>% 
  # Add desired column(s)
  dplyr::mutate(network = "LTAR", site_ID = "XX",
                .before = dplyr::everything())

# Check for gained/lost columns
supportR::diff_check(old = names(xx_raw), new = names(xx_pp))

# Re-check structure
dplyr::glimpse(xx_pp)

# Export locally
write.csv(x = xx_pp, na = '', row.names = F,
          file = file.path("data", "pre_processed_data", "LTAR_xx_pre-process.csv"))

# Clear environment
rm(list = ls()); gc()

# SP
sp <- read.csv("./data/raw/SP_RotGraz_MeasGrazingPlants_OLH.csv")
sp$site <- "SP_LTAR"

write.csv(sp, "./data/pre_processed_data/SP_RotGraz_MeasGrazingPlants_OLH.csv", row.names=FALSE)

rm(list = ls()); gc()

## -------------------------------------------- ## 
# Pre-Process "TG" ----
## -------------------------------------------- ## 

# Needed pre-processing:
## 

# Read in data
xx_raw <- read.csv(file = file.path("data", "raw", "xx.csv"))

# Check structure
dplyr::glimpse(xx_raw)

# Make needed repairs
xx_pp <- xx_raw %>% 
  # Drop columns that are entirely NA
  dplyr::select(-dplyr::where(fn = ~ all(is.na(.) | nchar(.) == 0))) %>% 
  # Add desired column(s)
  dplyr::mutate(network = "LTAR", site_ID = "XX",
                .before = dplyr::everything())

# Check for gained/lost columns
supportR::diff_check(old = names(xx_raw), new = names(xx_pp))

# Re-check structure
dplyr::glimpse(xx_pp)

# Export locally
write.csv(x = xx_pp, na = '', row.names = F,
          file = file.path("data", "pre_processed_data", "LTAR_xx_pre-process.csv"))

# Clear environment
rm(list = ls()); gc()

# TG
tg <- read.csv("./data/raw/TG_GSWRL_LTBE_MeasHarvestFractionv2.csv")
tg$site <- "TG_LTAR"

write.csv(tg, "./data/pre_processed_data/TG_GSWRL_LTBE_MeasHarvestFractionv2.csv", row.names=FALSE)

rm(list = ls()); gc()

## -------------------------------------------- ## 
# Pre-Process "UCB" ----
## -------------------------------------------- ## 

# UCB - Not doing now

## -------------------------------------------- ## 
# Pre-Process "UMRB" ----
## -------------------------------------------- ## 

# Needed pre-processing:
## 

# Read in data
xx_raw <- read.csv(file = file.path("data", "raw", "xx.csv"))

# Check structure
dplyr::glimpse(xx_raw)

# Make needed repairs
xx_pp <- xx_raw %>% 
  # Drop columns that are entirely NA
  dplyr::select(-dplyr::where(fn = ~ all(is.na(.) | nchar(.) == 0))) %>% 
  # Add desired column(s)
  dplyr::mutate(network = "LTAR", site_ID = "XX",
                .before = dplyr::everything())

# Check for gained/lost columns
supportR::diff_check(old = names(xx_raw), new = names(xx_pp))

# Re-check structure
dplyr::glimpse(xx_pp)

# Export locally
write.csv(x = xx_pp, na = '', row.names = F,
          file = file.path("data", "pre_processed_data", "LTAR_xx_pre-process.csv"))

# Clear environment
rm(list = ls()); gc()

# UMRB
umrb <- read.csv("./data/raw/UMRB_AMES_IAKFT_MeasHarvestFraction.csv")
umrb$site <- "UMRB_LTAR"

write.csv(umrb, "./data/pre_processed_data/UMRB_AMES_IAKFT_MeasHarvestFraction.csv", row.names=FALSE)

rm(list = ls()); gc()

## -------------------------------------------- ## 
# Upload Pre-Processed Data ----
## -------------------------------------------- ## 

# Identify local files
( local_pp <- dir(path = file.path("data", "pre_processed_data")) )

# Upload them
purrr::walk(.x = local_pp,
            .f = ~ googledrive::drive_upload(media = file.path("data", "pre_processed_data", .x), overwrite = T, path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1Sw-CdVIsCNvnS3laPn1a90WHoZsEoMif")))

# End ----

## -------------------------------------------- ## 
# Pre-Process TEMPLATE ----
## -------------------------------------------- ## 
# Duplicate / customize this as needed for new data files

# Needed pre-processing:
## 

# Read in data
xx_raw <- read.csv(file = file.path("data", "raw", "xx.csv"))

# Check structure
dplyr::glimpse(xx_raw)

# Make needed repairs
xx_pp <- xx_raw %>% 
  # Drop columns that are entirely NA
  dplyr::select(-dplyr::where(fn = ~ all(is.na(.) | nchar(.) == 0))) %>% 
  # Add desired column(s)
  dplyr::mutate(network = "LTAR", site_ID = "XX",
                .before = dplyr::everything())

# Check for gained/lost columns
supportR::diff_check(old = names(xx_raw), new = names(xx_pp))

# Re-check structure
dplyr::glimpse(xx_pp)

# Export locally
write.csv(x = xx_pp, na = '', row.names = F,
          file = file.path("data", "pre_processed_data", "LTAR_xx_pre-process.csv"))

# Clear environment
rm(list = ls()); gc()
