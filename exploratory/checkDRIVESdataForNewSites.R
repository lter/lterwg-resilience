## Katherine Muller, 2026-07-07

# Taking an inventory of the publicly available portions of the DRIVES database for usable crop data. 
############
# Criteria:
## 1) at least 5 site-years per crop
## 2) ANPP can be calculated from measured data (no estimation from harvest index)
## 2) Not irrigated

## Sites that overlap with Laurie's additional data:
# OHTVD_Wo and OHTVD_Ho are OH>HOYTVILLE.LTR, OH>WOOSTER.LTR  
# Sites that overlap with 
# CSCAP	HOYTVILLE.LTR	HOYTVILLE.LTR
# CSCAP	WOOSTER.LTR	WOOSTER.LTR
# NEMLTCRS is the same as the current PRHPA 

### objective:
# get a table of the number of sites and site years by crop with the range 

# load data:
#devtools::install_github("DRIVES-Project/drivesR")
library(drivesR)# package for working with DRIVES database tables.
library(tidyverse)


## loads a list of dataframes called dbpub
load(file.path("data","additional_data","directus_public_dblist_withCan_2026-07-07.Rdata"))


## Get an inventory of what fractions were measured for each crop and rotation. 
rotationphases <- dbpub$rotation_phases %>% # keeping the original in the list and doing the changes on a new copy
  mutate(across(
    starts_with("crop_fractions_"),
    ~ .x %>%
      str_remove_all("^\\{|\\}$") %>% # remove curly brackets
      str_split(",") # converts to character list.
  )) %>%
  pivot_longer(cols = starts_with("crop_fractions_"),
               names_to = c("removed","measured"),
               names_prefix = "crop_fractions_",
               names_sep = "_",
               values_to = "crop_fraction") %>%
  mutate(removed = removed == "removed",
         measured = measured == "measured") %>% # change to T/F
  unnest(crop_fraction, keep_empty = TRUE) %>%
  filter(crop_fraction != "none") # remove placeholder "none"


rotationcropmeasurements <- rotationphases %>% 
                                  filter(is_a_mix_component == FALSE &# remove components of mixtures
                                           measured == TRUE) %>%# remove fractions that were never measured
                                  summarize(.by= c(rotation_id,phase, granular_phase,crop_id),
                                            measuredFractions = paste(sort(crop_fraction), collapse=";"))

table(rotationcropmeasurements$crop_id, rotationcropmeasurements$measuredFractions)


## get yield data with treatment info
yields <- harmonize_yields_treatments(dbpub,crop_fractions_as_columns = TRUE)

# get a T/F for whether the observation can be used for ANPP. --------
## For now, ignore cases that could be imputed--I just want rough estimates
yields$hasANPP <- FALSE

## First condition: measured fraction is aboveground biomass and yield is not missing 
yields$hasANPP[which(grepl("biomass",yields$measured_fraction_1) & !is.na(yields$dry_yield_kg_ha_1))] <- TRUE

## Second condition: first fraction is grain and the second fraction is
# stover, straw, or silage 
# and it has yield data for both
#(there was one site where they measured silage and grain in the same plots)
table(yields$measured_fraction_1, yields$measured_fraction_2)
table(yields$measured_fraction_1, yields$measured_fraction_3)## only tomatoes at CA had a third measured fraction.

grainplus <- yields$measured_fraction_1== "grain" & yields$measured_fraction_2 %in% c("straw","stover","silage") &
                    !is.na(yields$dry_yield_kg_ha_1) & !is.na(yields$dry_yield_kg_ha_2)

yields$hasANPP[which(grainplus)] <- TRUE

table(yields$actual_crop_id, yields$hasANPP)

yields$removeRow[which(!yields$hasANPP)] <- TRUE

## remove irrigated treatments--------
table(yields$site_id,yields$irrigation)
## a few sites are rarely irrigated to save the experiment, but
# they can count as non-irrigated for this
## Only CATCE has irrigated
with(filter(yields, site_id == "CATCE"), table(actual_crop_id, irrigation))

## wheat systems have irrigated and non-irrigated treatmetns. Corn/tomato systems are irrigated. 
yields$removeRow[which(yields$irrigation=="irrigated")] <- TRUE

## Remove unrealistic zero-fertilizer control treatments---------
zeroNrows <- (yields$`N fertility` == "none" & yields$`cover crop`=="no cover crop") |## CATCE rainfed wheat control
              (yields$`N rate`== "zero N") # Applies to treatments at CanONLTRT_Ri and NEMLTCRS

# doublecheck. 
unique(yields$site_id[which(zeroNrows)])# OK
yields$removeRow[which(zeroNrows)] <- TRUE


## get an inventory of site-years and duration by crop (irrespective of treatment context)------------
yieldForANPP <- yields %>% filter(removeRow==FALSE)
## get a more aggregated crop. 
unique(yieldForANPP$actual_crop_id)
yieldForANPP$aggregatedCrop <- NA
yieldForANPP$aggregatedCrop[which(yieldForANPP$actual_crop_id %in% c("corn","silage corn"))] <- "corn"
yieldForANPP$aggregatedCrop[which(yieldForANPP$actual_crop_id %in% c("spring triticale","spring oats","spring barley","winter cereal rye"))] <- "other small grain" 
yieldForANPP$aggregatedCrop[which(yieldForANPP$actual_crop_id %in% c("spring wheat","winter wheat"))] <- "wheat" 
yieldForANPP$aggregatedCrop[which(yieldForANPP$actual_crop_id %in% c("annual grass-legume mix","annual legume-only mix"))] <- "winter legume cover crop"
yieldForANPP$aggregatedCrop[which(yieldForANPP$actual_crop_id=="sorghum")] <- "sorghum"
yieldForANPP$aggregatedCrop[which(yieldForANPP$actual_crop_id=="soybean")] <- "soybean"
yieldForANPP$aggregatedCrop[which(yieldForANPP$actual_crop_id %in% c("alfalfa","alfalfa mix","perennial mix","red clover"))] <- "perennial forage legume" 

# check all are accounted for
table(yieldForANPP$actual_crop_id, is.na(yieldForANPP$aggregatedCrop))# OK

cropsummarybysite <- yieldForANPP %>% ungroup() %>%
  summarise(.by = c(site_id, aggregatedCrop),
          yearsWithData = length(unique(harvest_year)))

#View(cropsummarybysite)
## use this to remove site/crop combos with <5 years of data.
for(i in 1:nrow(cropsummarybysite)){
  myrow <- cropsummarybysite[i,]
  if(myrow$yearsWithData < 5){
    nixrows <- yieldForANPP$site_id== myrow$site_id & yieldForANPP$aggregatedCrop== myrow$aggregatedCrop
    yieldForANPP$removeRow[nixrows] <- TRUE
  }
}


yieldForANPP2 <- yieldForANPP %>% filter(removeRow ==FALSE)

## calculate anpp. 
yieldForANPP2 <- yieldForANPP2 %>% 
  mutate(ANPP = sum(dry_yield_kg_ha_1, dry_yield_kg_ha_1, na.rm=TRUE))


## Get water year annual precipitation----------
library(lubridate)
## this only goes to the max year of the data, but it's OK for a rough approximation
dailyweatherdf <- dbpub$weather_daily %>% filter(site_id %in% unique(yieldForANPP2$site_id) & year >= 1979 )
dailyweatherdf$month <- lubridate::month(as.Date(dailyweatherdf$date))

wyr_ppt_allyrs <- dailyweatherdf %>%
  mutate(w_yr = ifelse(month > 9, year + 1, year) )%>%
  group_by(w_yr, site_id)%>%
  dplyr::summarise(wyr_ppt  = sum(precip_mm, na.rm = T) )


wyr_map <- wyr_ppt_allyrs %>% ungroup() %>% summarize(.by = site_id, wyr_ppt_mean = mean(wyr_ppt, na.rm = TRUE))




## Get final inventory: by site
cropsummarybysite <- yieldForANPP2 %>% ungroup() %>%
  summarize(.by = c(site_id, aggregatedCrop), 
            numyears = length(unique(harvest_year))) %>%
  left_join(wyr_map, by = "site_id")

#View(cropsummarybysite) 
# some sites are missing weather data--that's a mistake--will be fixed.
# the missing ones should have comparable MAP to sites that aren't missing. 

# inventory--all drives sites.
cropsummary <- cropsummarybysite %>% 
  summarize(.by = c(aggregatedCrop),
            numSites = length(unique(site_id)),
            minSiteYears = min(numyears),
            maxSiteYears = max(numyears),
            minMAP = min(wyr_ppt_mean, na.rm=TRUE),
            maxMAP = max(wyr_ppt_mean, na.rm=TRUE))

cropsummary







