#Calculate Aridity Index
#Aridity index defined as long-term mean precipitation over 
#potential evapotranspiration (P/PET)

#load packages
library(tidyverse)
library(SPEI)

#download climate data and site info (see weatherSiteInitialFigure.R)
#merge climate and site tables
climate_site <- left_join(site_info, maw) 
climate_site_sort <- climate_site[order(climate_site$latitude, decreasing = TRUE), ]
aridity_site <- climate_site_sort %>%
  mutate(PET = SPEI::thornthwaite(mean_tmean_degC, latitude, na.rm = FALSE))#calculate PET

SPEI::thornthwaite(Tave = 10.43997267, lat = 48.2065)

climate_site$spei<-thornthwaite(climate_site$mean_tmean_degC, climate_site$latitude)
