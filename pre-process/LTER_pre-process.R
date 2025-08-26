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

sev_anpp$site <- "sev"
sev_anpp$network <- "LTER"

write.csv(sev_anpp, "G:/Shared drives/LTER-WG_Resilience-Management/data/pre_processed_data/sev_anpp_nceas.csv")

rm(list = ls()); gc()

##Jornada
jornada_anpp <- read.csv("G:/Shared drives/LTER-WG_Resilience-Management/data/raw_data/Ecotone_ANPP_by_Site.csv")

jornada_anpp$zone <- revalue(jornada_anpp$zone, c(S = "shrubland", E = "ecotone", G = "grassland"))

jornada_anpp <- jornada_anpp%>%
  subset( ANPP_noYUEL != ".")%>%
  dplyr::select(site, zone, year, ANPP_noYUEL)
jornada_anpp$location <- jornada_anpp$site

jornada_anpp$site <- "jrn"
jornada_anpp$network <- "LTAR and LTER"

write.csv(jornada_anpp, "G:/Shared drives/LTER-WG_Resilience-Management/data/pre_processed_data/jrn_anpp_nceas.csv")

rm(list = ls()); gc()

##Konza
konza_anpp <- read.csv("G:/Shared drives/LTER-WG_Resilience-Management/data/raw_data/PAB011.csv")%>%
  replace_na(list(LVGRASS = 0, FORBS = 0, CUYRDEAD = 0, WOODY = 0))
konza_anpp$anpp <- (konza_anpp$LVGRASS + konza_anpp$FORBS + konza_anpp$CUYRDEAD + konza_anpp$WOODY)*10 #make per meter squared
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

konza_anpp$site_ID <- "knz"
konza_anpp$network <- "LTER"

write.csv(konza_anpp, "G:/Shared drives/LTER-WG_Resilience-Management/data/pre_processed_data/knz_anpp_nceas.csv")

rm(list = ls()); gc()



##Niwot
niwot_anpp <- read.csv("G:/Shared drives/LTER-WG_Resilience-Management/data/raw_data/saddgrid_npp.hh.data.csv")%>%
  dplyr::select(LTER_site, local_site, year, collection_date,veg_class, grid_pt, subsample, NPP)%>%
  subset(veg_class != "SB" & veg_class != "SF"& veg_class != "ST"  & veg_class != "WM") #Removing these per recommendations of Tom M.

niwot_anpp$network <- "LTER"
niwot_anpp$site_ID <- "nwt"

write.csv(niwot_anpp, "G:/Shared drives/LTER-WG_Resilience-Management/data/pre_processed_data/nwt_anpp_nceas.csv")
#pick vegclass FF=fellfield, DM=dry meadow, MM=moist meadow, ST=shrub tundra, SB=snowbed, WM=wet meadow, SF=snowfence. 

rm(list = ls()); gc()

##Cedar Creek
cdr_anpp <- read.csv("G:/Shared drives/LTER-WG_Resilience-Management/data/raw_data/E001_Aboveground_Biomass.csv")%>%#field C whole gets burned semi-regularly #data from 
  subset(NTrt == "9" & Field == "C")%>%
  subset(Species != "Miscellaneous litter" & Species != "Mosses & lichens" & Species != "Fungi"  & Species != "Mosses & lichens 2" & Species != "Pine needles" & Species != "Lichens" )%>%
  ddply(.(Year, Field, Plot),function(x)data.frame(
    anpp = sum(x$Biomass.g.m2)
  ))

cdr_anpp$burn_trt <- ifelse(cdr_anpp$Field == "C", "burn","control")
cdr_anpp$site_ID <- "cdr"
cdr_anpp$network <- "LTER"

write.csv(cdr_anpp, "G:/Shared drives/LTER-WG_Resilience-Management/data/pre_processed_data/cdr_anpp_nceas.csv")

rm(list = ls()); gc()


##CAP lter
cap_anpp <- read.csv("G:/Shared drives/LTER-WG_Resilience-Management/data/raw_data/AllData2017AnnualsAI8_23_18.csv")%>%
            subset(Treatment == "c")%>%
            subset(Patch_type == "IP")
cap_anpp$cap_location <- cap_anpp$Site

cap_anpp$site_ID <- "cap"
cap_anpp$network <- "LTER"

cap_anpp <- cap_anpp%>%dplyr::select(site_ID, cap_location, Year, AnnBiomass, network)

write.csv(cap_anpp, "G:/Shared drives/LTER-WG_Resilience-Management/data/pre_processed_data/cap_anpp_nceas.csv")


rm(list = ls()); gc()

