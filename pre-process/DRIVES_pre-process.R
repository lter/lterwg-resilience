## Katherine Muller, 2026-07-07
## This processes the publicly available portions of the DRIVES database into a form
## (hopefully) usable for this synthesis.

# load data:-----
#devtools::install_github("DRIVES-Project/drivesR")
library(drivesR)# package for working with DRIVES database tables.
library(tidyverse)

## loads a list of dataframes called dbpub
load(file.path("data","additional_data","directus_public_dblist_withCan_2026-07-07.Rdata"))

## get data dictionaries (doesn't require an API key)
dictlist <- import_dictionary_tables()

## get yield data with planting and harvest dates. 
## This puts multiple crop fractions (grain, straw, etc.) in different columns
yields <- harmonize_yields_planting_harvest(dbpub)

## get treatment identifiers to merge with yields
unittrt <- harmonize_treatments_units(dbpub, maxyear = 2022) %>% select(unit_id,year,treatmentID1, treatmentID2)

yields <- left_join(yields, unittrt, by = c("unit_id", "harvest_year" = "year"))
## remove rows that don't match up with a treatment (pre-experiment uniformity yield data)
yields <- filter(yields, !is.na(yields$treatmentID1))

yields <- ungroup(yields)

# Site info table:------------
## filter for sites that are within the public yield data
sitedf <- dbpub$site_info %>% filter(site_id %in% unique(yields$site_id))
sitedf$network <- "DRIVES"
sitedf <- relocate(sitedf, network, .before = 1)

#write.csv(sitedf, file = file.path("data","pre_processed_data","drives_siteinfo.csv"),row.names = FALSE,na = "")


# Treatment info table: -------

## gives one row per year.
trtdf <- harmonize_treatments(dbpub)
## collapse so entry phases are in a string. 
trtdf <- trtdf %>% group_by(treatmentID1, year) %>%
  mutate(entryPhases = paste(sort(unique(entryPhase)),collapse=",")) %>%
  select(-entryPhase) %>%
  relocate(entryPhases, .after = "rotation_id")


## summarize it so there's one row per combination of treatment levels. 
groupcols <- c("site_id","treatmentID1",#"treatmentID2",
  names(trtdf)[(which(names(trtdf)=="year")+1):ncol(trtdf)])

trtdf2 <- trtdf %>% 
                group_by(across(all_of(groupcols))) %>%
                summarize(start_year = min(year, na.rm=TRUE),
                          end_year = max(year, na.rm=TRUE),
                          .groups = "drop") %>%
  relocate(start_year, end_year, .after = treatmentID1)
  
  
# I think it's helpful to show the entry phases. 
# remove sites and treatments that have no yield data in the public dataset. 
sitestoremove <- setdiff(trtdf$site_id, yields$site_id)

trtsansyield <- yields %>% summarize(.by = c(treatmentID1), noYield = all(is.na(dry_yield_kg_ha_f1))) %>%
  filter(noYield == TRUE) %>% .[["treatmentID1"]]

trtdf2 <- trtdf2 %>% filter(!site_id %in% sitestoremove) %>% filter(!treatmentID1 %in% trtsansyield)
# sites with contrasting N rates have that info in a different column. condense for simplicity.
table(is.na(trtdf2$`N rate`), is.na(trtdf2$`N fertility`))# mutually exclusive.
nraterows <- which(!is.na(trtdf2$`N rate`))
trtdf2$`N fertility`[nraterows] <- trtdf2$`N rate`[nraterows]

# treatment columns that apply to excluded sites
emptycols <- names(trtdf2)[which(apply(trtdf2,2,function(x){all(is.na(x))}))]

trtdf3 <- select(trtdf2, -all_of(c("N rate",emptycols)))

# good enough. 
#write.csv(trtdf3, file = file.path("data","pre_processed_data","drives_treatmentinfo.csv"),row.names = FALSE,na = "")


# Process yield data--------

## Make a TF column for whether the observation can be used for ANPP.
## also add TF for grain.
## For now, ignore cases that could be imputed--I just want rough estimates
yields$hasANPP <- FALSE
yields$hasGrain <- FALSE

## First condition: measured fraction is aboveground biomass and yield is not missing 
yields$hasANPP[which(grepl("biomass",yields$measured_fraction_f1) & !is.na(yields$dry_yield_kg_ha_f1))] <- TRUE

# for grain, it's straightforward.
yields$hasGrain[which(yields$measured_fraction_f1=="grain" & !is.na(yields$dry_yield_kg_ha_f1))] <- TRUE

## Second condition: first fraction is grain and the second fraction is
# stover, straw, or silage 
# and it has yield data for both
#(there was one site where they measured silage and grain in the same plots)
table(yields$measured_fraction_f1, yields$measured_fraction_f2)
table(yields$measured_fraction_f1, yields$measured_fraction_f3)## only tomatoes at CA had a third measured fraction.

grainplus <- yields$measured_fraction_f1== "grain" & yields$measured_fraction_f2 %in% c("straw","stover") &
  !is.na(yields$dry_yield_kg_ha_f1) & !is.na(yields$dry_yield_kg_ha_f2)

yields$hasANPP[which(grainplus)] <- TRUE

## there are a few years in WICST when they harvested silage from microplots, but the date is different, 
# so it's easier just to exclude. 


# Calculate ANPP----------
## (currently in kg/ha)
yields <- yields %>% 
  mutate(ANPP_kgha = sum(dry_yield_kg_ha_f1,dry_yield_kg_ha_f2, na.rm = TRUE)) %>%
  mutate(ANPP_kgha = ifelse(hasANPP, ANPP_kgha, NA))

## add in a grain-specific yield column
yields$grainyield_kgha <- ifelse(yields$hasGrain, yields$dry_yield_kg_ha_f1, NA)


## trim off unecessary columns ant put in a better order
## Right now the harmonization function is omitting harvest dates for crops with multiple harvests. 
# this is a bug I need to fix. But most of the crops you want will have one harvest date anyway.
colsIwant <- c("site_id" ,"unit_id" ,"planting_year","harvest_year" ,"expected_crop_id" ,"actual_crop_id" ,"treatmentID1" ,"treatmentID2" ,"hasANPP" ,"hasGrain" ,"ANPP_kgha" ,"grainyield_kgha","stand_year" ,"num_harvests" ,"rotation_phase" ,"cover_crop" , "measured_fraction_f1" ,"measured_fraction_f2"  ,"removed_from_field_tf_f1" ,"removed_from_field_tf_f2"  ,"dry_yield_kg_ha_f1" ,"dry_yield_kg_ha_f2" ,"harvest_date_1_f1" ,"planting_date" )

yields2 <- yields %>% select(all_of(colsIwant))

## rename a couple
yields2 <- yields2 %>% rename("cover_crop_tf" = "cover_crop")

## trim off big batches of rows--------
## treatments with no yield data. 
trtsummary <- yields2 %>% ungroup() %>% summarize(.by= c(site_id,treatmentID1),
                                    noYield = all(is.na(dry_yield_kg_ha_f1)))

table(trtsummary$site_id, trtsummary$noYield)

yields3 <- yields2 %>% filter(!treatmentID1 %in% trtsummary$treatmentID1[which(trtsummary$noYield==TRUE)])


## remove rows for cover crops that weren't measured.
table(yields3$num_harvests, yields3$measured_fraction_f1)
table( yields3$measured_fraction_f1, is.na(yields3$dry_yield_kg_ha_f1))

yields3 <- yields3 %>% filter(measured_fraction_f1 != "none")

## add a more general crop identifier for aggregrating.
yields3$aggregate_crop <- yields3$actual_crop_id
yields3$aggregate_crop[which(yields3$actual_crop_id %in% c("corn","silage corn"))] <- "corn"
yields3$aggregate_crop[which(yields3$actual_crop_id %in% c("spring triticale","spring oats","spring barley","spring wheat","spring small grain"))] <- "spring small grain" 
yields3$aggregate_crop[which(yields3$actual_crop_id %in% c("winter wheat","winter cereal rye","winter small grain"))] <- "winter small grain" 
yields3$aggregate_crop[which(yields3$actual_crop_id %in% c("annual grass-legume mix","annual legume-only mix"))] <- "winter legume cover crop"
yields3$aggregate_crop[which(yields3$actual_crop_id=="sorghum")] <- "sorghum"
yields3$aggregate_crop[which(yields3$actual_crop_id=="soybean")] <- "soybean"
yields3$aggregate_crop[which(yields3$actual_crop_id %in% c("alfalfa","alfalfa mix","perennial mix","red clover"))] <- "perennial forage legume" 

yields3 <- relocate(yields3, aggregate_crop, .before = actual_crop_id)

#write.csv(yields3, file.path("data","pre_processed_data","drives_anpp_pre_process.csv"), row.names=FALSE, na = "")


# Olivia is amending this slightly to fix the ANPP
file2<-'drives_anpp_pre_process.csv'
googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1Sw-CdVIsCNvnS3laPn1a90WHoZsEoMif")) %>% 
  dplyr::filter(name == file2) %>% 
  googledrive::drive_download(file = .$id, overwrite = T,
                              path = file.path("data", "pre_processed_data", .$name))

##STEP 1: Read in the file
drives <- read.csv(file = file.path("data", "pre_processed_data", file2)) 

# fix the anpp
glimpse(drives)
drives

unique(drives$measured_fraction_f1)
unique(drives$measured_fraction_f2)
drives.2 <- drives %>%
  mutate(anpp_kgha = case_when(
    hasANPP == T ~ dry_yield_kg_ha_f1, 
    measured_fraction_f2 %in% c("straw", "stover", "silage") ~ dry_yield_kg_ha_f1 + dry_yield_kg_ha_f2,
    TRUE ~ NA)) %>%
  select(-ANPP_kgha) %>%
  rename(ANPP_kgha = anpp_kgha)

# Export locally
write.csv(x = drives.2, row.names = F, na = '',
          file = file.path("data", "pre_processed_data", "drives_anpp_pre_process.csv"))

# Upload to Drive
googledrive::drive_upload(media = file.path("data", "pre_processed_data", "drives_anpp_pre_process.csv"), overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1Sw-CdVIsCNvnS3laPn1a90WHoZsEoMif"))


# Quick understanding of the drive treatments, crop names, and time lengths to exclude
glimpse(drives.2)
unique(drives.2$actual_crop_id)

drives.summary <- drives.2%>%
  mutate(grain_data = ifelse(grainyield_kgha > 0, 1, 0), anpp_data = ifelse(ANPP_kgha>0, 1, 0)) %>%
  mutate(grain_data = ifelse(is.na(grain_data), 0, grain_data), anpp_data = ifelse(is.na(anpp_data), 0, anpp_data)) %>%
  filter(!site_id %in% c("NELITCSE", "MIKBSLFL", "NEMLTCRS")) %>%
  group_by(site_id, treatmentID1, actual_crop_id) %>%
  summarize(min_year = min(harvest_year), max_year = max(harvest_year), n=length(unique(harvest_year)), anpp_pres = sum(anpp_data), 
            grain_pres = sum(grain_data))%>%
  ungroup()%>%
  filter(actual_crop_id %in% c("corn", "soybean", "winter wheat", "spring wheat")) %>%
  filter(n>4)



