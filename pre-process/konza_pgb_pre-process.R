#script to process Konza Patch burn graze
#022525

#https://doi.org/10.6073/pasta/3290d82116366eeed870b15a4fd4b15b

#
#need to change input and output file paths

library(tidyverse)
library(plyr)


##Konza experimental data set PGB

#bring in main dataset 
konza_pgb_pre <- read.csv('C:/Users/tkm29/Downloads/Konza_PBG_pre_process.csv')%>%
  filter(Woody == 0)%>%#filter out woody values 
  mutate(graze_trt = 1,
         burn_trt = 1,
         notes = 'standing_biomass',
         site_ID = 'kza',
         network = 'LTER')%>%
  dplyr::rename( Year = Recyear,
         Month = Recmonth, 
         Day = Recday)%>%
  dplyr::select(-c(Woody))


#bring in calibration dataset
konza_calibration <- read.csv('C:/Users/tkm29/Downloads/konza_PGB_calibration.csv')%>%
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

konza_pgb_pre$anpp <- predict(live.m, newdata = konza_pgb_pre)


write.csv(konza_pgb_pre, "G:/Shared drives/LTER-WG_Resilience-Management/data/raw_data/konza_pgb_anpp_nceas.csv")
