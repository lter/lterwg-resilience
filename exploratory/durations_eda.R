#site durations 
#started 022625 TKM

library(readxl)
library(tidyverse)


ltar_sites <- read_excel("G:/Shared drives/LTER-WG_Resilience-Management/data/Treatment_overview_LTAR.xlsx")%>%
  dplyr::rename( site_ID = site)%>%
  dplyr::select(site_ID, treatment_ID, start_yr, end_yr)%>%
  mutate


nutnet_sites <- read_excel("G:/Shared drives/LTER-WG_Resilience-Management/data/Treatment_overview_NutNet.xlsx")%>%
  dplyr::select(site_ID, treatment_ID, start_yr, end_yr)

lter_sites <- read.csv("G:/Shared drives/LTER-WG_Resilience-Management/data/lter_site_duration.csv")


all_site_duration <- bind_rows(ltar_sites, nutnet_sites)%>%
  bind_rows(lter_sites)


ggplot(all_site_duration)+
  geom_segment( aes(y = treatment_ID, x = start_yr, xend = end_yr))
