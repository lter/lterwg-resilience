#####This script takes LTER data from EDI sources and makes them harmonization-ready

#need to change input and output file paths

library(tidyverse)
library(plyr)


##Sev
sev_anpp <- read.csv("G:/Shared drives/LTER-WG_Resilience-Management/data/raw_data/sev182_NPP_core_biomass.csv")%>%
  subset(site == "core_black" & season == "fall")%>% #picked black grama core site. fall season is major growing season
  dplyr::select(site, year, web, plot, quad, kartez, biomass.BM)%>%
  ddply(.(site, year, web, plot, quad), function(x)data.frame(
    anpp = sum(x$biomass.BM)
  ))
sev_anpp$site <- "sev_lter"

write.csv(sev_anpp, "G:/Shared drives/LTER-WG_Resilience-Management/data/raw_data_to_use/sev_anpp_nceas.csv")

rm(list = ls()); gc()

##Jornada
jornada_anpp <- read.csv("G:/Shared drives/LTER-WG_Resilience-Management/data/raw_data/Ecotone_ANPP_by_Site.csv")

jornada_anpp$zone <- revalue(jornada_anpp$zone, c(S = "shrubland", E = "ecotone", G = "grassland"))

jornada_anpp <- jornada_anpp%>%
  subset( ANPP_noYUEL != ".")%>%
  dplyr::select(site, zone, year, ANPP_noYUEL)
jornada_anpp$location <- jornada_anpp$site
jornada_anpp$site <- "jrn_lter"


write.csv(jornada_anpp, "G:/Shared drives/LTER-WG_Resilience-Management/data/raw_data_to_use/jrn_anpp_nceas.csv")

rm(list = ls()); gc()

##Konza
konza_anpp <- read.csv("G:/Shared drives/LTER-WG_Resilience-Management/data/raw_data/PAB011.csv")
konza_anpp$anpp <- (konza_anpp$LVGRASS + konza_anpp$FORBS)*10 #make per meter squared
konza_anpp <- konza_anpp%>%
              unite("date", c("RECYEAR","RECMONTH","RECDAY"), sep = "-",remove = TRUE)

#tu = lowland, fl = uplands, sl = slope
konza_anpp$topography <- ifelse(konza_anpp$SOILTYPE == "tu", "lowland",
                                ifelse(konza_anpp$SOILTYPE == "fl", "uplands",
                                       ifelse(konza_anpp$SOILTYPE == "sl", "slope", "NA")))
konza_anpp$treatment <- konza_anpp$WATERSHED

konza_anpp <- konza_anpp%>%
  #subset(WATERSHED == "001d")%>%#subset target watershed 001d both upland and lowland
  dplyr::select(date, treatment, topography, TRANSECT, PLOTNUM, anpp)

konza_anpp$site <- "knz_lter"

write.csv(konza_anpp, "G:/Shared drives/LTER-WG_Resilience-Management/data/raw_data_to_use/knz_anpp_nceas.csv")

rm(list = ls()); gc()



##Niwot
niwot_anpp <- read.csv("G:/Shared drives/LTER-WG_Resilience-Management/data/raw_data/saddgrid_npp.hh.data.csv")%>%
  dplyr::select(LTER_site, local_site, year, collection_date,veg_class, grid_pt, subsample, NPP)

write.csv(niwot_anpp, "G:/Shared drives/LTER-WG_Resilience-Management/data/raw_data_to_use/nwt_anpp_nceas.csv")
#pick vegclass FF=fellfield, DM=dry meadow, MM=moist meadow, ST=shrub tundra, SB=snowbed, WM=wet meadow, SF=snowfence. 

rm(list = ls()); gc()

##Cedar Creek
cdr_anpp <- read.csv("G:/Shared drives/LTER-WG_Resilience-Management/data/raw_data/E001_Aboveground_Biomass.csv")%>%#field C whole gets burned semi-regularly #data from 
  subset(NTrt == "9")%>%
  ddply(.(Year, Field, Plot),function(x)data.frame(
    anpp = sum(x$Biomass.g.m2)
  ))

cdr_anpp$burn_trt <- ifelse(cdr_anpp$Field == "C", "burn","control")
cdr_anpp$site <- "cdr_lter"

write.csv(cdr_anpp, "G:/Shared drives/LTER-WG_Resilience-Management/data/raw_data_to_use/cdr_anpp_nceas.csv")

rm(list = ls()); gc()kg


##CAP lter
cap_anpp <- read.csv("G:/Shared drives/LTER-WG_Resilience-Management/data/raw_data/AllData2017AnnualsAI8_23_18.csv")%>%
            subset(Treatment == "c")%>%
            subset(Patch_type == "IP")
cap_anpp$cap_location <- cap_anpp$Site

cap_anpp$site <- "cap_lter"

cap_anpp <- cap_anpp%>%dplyr::select(site, cap_location, Year, AnnBiomass)

write.csv(cdr_anpp, "G:/Shared drives/LTER-WG_Resilience-Management/data/raw_data_to_use/cap_anpp_nceas.csv")




