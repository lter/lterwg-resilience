#Calculate Aridity Index
#Aridity index defined as long-term mean precipitation over 
#potential evapotranspiration (P/PET)

#load packages
library(tidyverse)
library(SPEI)

source("ancillary/google_drive_urls.R")
#download climate data and site info (see weatherSiteInitialFigure.R) - Lines 5-23 DLH
  #Lina ran these lines and they did not work DLH
  #merge climate and site tables
  #climate_site <- left_join(site_info, maw) 
  #climate_site$PET<-thornthwaite(climate_site$mean_tmean_degC, climate_site$latitude)
  
  #climate_site$PET<-thornthwaite(climate_site$mean_tmean_degC, 41.111)

#monthly 
climate_site_month<-read.csv("data/tidy_data/daymet_monthly_weather.csv")
site_info <- read.csv("data/site_summary_info.csv",fileEncoding = "UTF-8-BOM")
climate_site_month <- left_join(climate_site_month, site_info) 

cper<-filter(climate_site_month, site_id == "CPER")
cper$PET <-thornthwaite(cper$mean_tmean_degC, a)
a=cper[1,9]

sites<-unique(climate_site_month$site_id)
l=length(sites)
climate_site_month2<-data.frame()

for (i in 1:l){
  #i=1
  site=as.character(sites[i])
  dat=filter(climate_site_month, site_id == site)
  lat=dat[i,9]
  dat$PET<-thornthwaite(dat$mean_tmean_degC, lat)
  climate_site_month2<-rbind(climate_site_month2, dat )
}

#calculate monthly PET and Precip sums
climate_site_month_sums<-climate_site_month2%>%
  group_by(site_id, year)%>%
  summarise(precip_mm=sum(precip_mmmonth, na.rm = T),
            PET_mm = sum(PET, na.rm = T))

climate_site_avgs<-climate_site_month_sums%>%
  group_by(site_id)%>%
  summarise(precip_mm=mean(precip_mm, na.rm =T),
            PET_mm = mean(PET_mm, na.rm =T))%>%
  mutate(AI = precip_mm/PET_mm)

climate_site_avgs<-left_join(climate_site_avgs, site_info)

#graph Aridy Index (AI) distributions
ggplot(data = climate_site_avgs, aes(x = AI, colour= network,  fill = network))+
  geom_histogram(alpha = 0.4) +
  facet_wrap(~network, ncol = 1)

