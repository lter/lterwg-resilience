#script to process Konza Patch burn graze
#022525

#https://doi.org/10.6073/pasta/3290d82116366eeed870b15a4fd4b15b

#
#need to change input and output file paths

library(tidyverse)
library(plyr)


##Konza experimental data set pbg

#bring in main dataset 

konza_pbg_pre <- read.csv("G:/Shared drives/LTER-WG_Resilience-Management/data/raw_data/Konza_PBG_raw.csv")%>%
  filter(Woody == 0)%>%#filter out woody values 
  mutate(notes = 'standing_biomass',
         site_ID = 'kza',
         network = 'LTER')%>%
  dplyr::select(-c(Woody))


#bring in calibration dataset
konza_calibration <- read.csv('G:/Shared drives/LTER-WG_Resilience-Management/data/raw_data/konza_PGB_calibration.csv')%>%
  mutate(lvbiomass = (Lvgrass + Forbs ),
         totalbiomass =  (Lvgrass + Forbs  + Pdead) )%>%#manipulate calibration data into total live biomass
  filter(Comments != 'not sure if Aug. or Sept.')%>%# seem to be unit errors 
  filter(Woody == 0)#filter out woody values


#test global calibration relationship



live.m <- lm(lvbiomass ~ Diskht , data = konza_calibration)
summary(live.m)

totalbiomass.m <- lm(totalbiomass ~ Diskht, data = konza_calibration)
summary(totalbiomass.m)

#live relationship is better than total biomass
#create new column with estimated biomass

konza_pbg_pre$biomass.g.m2 <- predict(live.m, newdata = konza_pbg_pre)

konza_pbg_pre <- konza_pbg_pre%>%
  select(Recyear, Recmonth, Recday, Watershed, Transect, Plotnum, Rep, site_ID, network, notes, biomass.g.m2)
write.csv(konza_pbg_pre, "G:/Shared drives/LTER-WG_Resilience-Management/data/pre_processed_data/kza_pbg_anpp_nceas.csv")


