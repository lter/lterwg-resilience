#Calculate Aridity Index
#Aridity index defined as long-term mean precipitation over 
#potential evapotranspiration (P/PET)

#load packages
library(tidyverse)
library(SPEI)

#download climate data and site info (see weatherSiteInitialFigure.R)
#merge climate and site tables
climate_site <- left_join(site_info, maw) %>%
  mutate(PET = SPEI::thornthwaite(climate_site$mean_tmean_degC, climate_site$latitude, na.rm = FALSE))#calculate PET

