## assumes computer is in sync with google drive
library(supportR)
library(tidyverse)

source("ancillary/google_drive_urls.R")
soilvars <- read.csv("data/pre_processed_data/soilvars.csv")
daymetannual <- read.csv("data/pre_processed_data/daymet_annual_weather.csv")
mswepannual <- read.csv("data/pre_processed_data/mswep_annual.csv")
range(daymetannual$year) #1980-2023
range(mswepannual$year)# 1979-2025
siteinfo <- read.csv("data/SAMPLEsite_summary_info.csv")
siteinfo <- select(siteinfo, -start_yr, -end_yr)
setdiff(siteinfo$site_id, soilvars$site_id)# consistent

## NRCS uses 1991-2020 for climate normals
## I'll use mean for simplicity.
mat <- daymetannual %>% filter(year %in% 1991:2020) %>%
            group_by(site_id,network) %>%
            summarize(mat_degc = round(mean(mean_tmean_degC),1) )
map <- mswepannual %>% filter(year %in% 1991:2020) %>%
            group_by(site_id, network) %>%
            summarize(map_mm = round(mean(precip_mmyear),1))
soildf <- soilvars %>% select(site_id, network,hydgrp, taxorder,taxsuborder,muname)
soildf <- soildf %>% rename_with(~paste0("soil_",.x) ,.cols = hydgrp:muname )
soildf$soil_comment <- NA
soildf$soil_comment[which(soildf$site_id== "koffler.ca")] <- "Canadian site absent from NRCS soil survey"
soildf$soil_comment[which(soildf$site_id== "sedg.us")] <- "Missing basic soil info" 
soildf$soil_comment[which(soildf$site_id=="sage.us")] <- "Missing hydrogrp"

outdf <- siteinfo %>% 
          left_join(mat) %>%
          left_join(map) %>%
          left_join(soildf)
View(outdf)
#write.csv(outdf, file.path("data","harmonized_data","site_summary_info.csv"),row.names = FALSE, na = "")
# googledrive::drive_upload(media = file.path("data", "harmonized_data","site_summary_info.csv"), overwrite = T,
#                           path = googledrive::as_id(dir.harmonized_data))
