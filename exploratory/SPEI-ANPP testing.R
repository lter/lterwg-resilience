#SPEI testing
#Dave and Makki 
#
#8/26/25 start

#SPEI analysis based on Perez et al 2025

# Load libraries
library(lubridate)
library(tidyverse)
library(googledrive)
library(ggplot2)
library(ggpubr)
drive_auth()

# Make needed folder(s)
dir.create(file.path("data"), showWarnings = F)
dir.create(file.path("data", "harmonized_data"), showWarnings = F)
dir.create(file.path("data", "pre_processed_data"), showWarnings = T)

# Clear environment + collect garbage
rm(list = ls()); gc()

######################
##Read in SPEI-12 DATA
######################

# Identify desired file
focal_file <- "SPEI12.csv"

# Download harmonized data file
googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/13Ymkrr-kRLDmpaj1jwwVOnOSmEYnF-dJ")) %>% 
  dplyr::filter(name == focal_file) %>% 
  googledrive::drive_download(file = .$id, overwrite = T,
                              path = file.path("data", "pre_processed_data", .$name))

#Dave couldn't get file to upload from gdrive, so importing manually 
spei.12<-read.csv("C:/Users/david.hoover/OneDrive - USDA/HomeDrive/Projects/NCEAS_resilience/SPEI12.csv")

#extract 12 month values for August; categorize extreme
spei.12<-spei.12%>%
  mutate(month = month(as.Date(date)))%>%
  mutate(year = year(as.Date(date)))%>%
  filter(month == '8')%>%
  filter(year < 2025 & year > 1981)%>%
  pivot_longer(cols = 2:56, names_to = "site", values_to = "SPEI")%>%
  mutate(spei.cat = ifelse(SPEI>0.99, "wet", ifelse (SPEI <(-0.99), "dry", "normal")))



######################
##Read in ANPP DATA
######################

# Identify desired file
focal_file <- "04_anpp_aggregated-site-crop.csv"

# Download harmonized data file
googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/13Ymkrr-kRLDmpaj1jwwVOnOSmEYnF-dJ")) %>% 
  dplyr::filter(name == focal_file) %>% 
  googledrive::drive_download(file = .$id, overwrite = T,
                              path = file.path("data", "harmonized_data", .$name))

# Check structure
dplyr::glimpse(anpp_v1)

# 

#change jrn from "LTAR and LTER" to LTER
anpp_v1$network <-ifelse(anpp_v1$network == "LTAR and LTER", "LTER", anpp_v1$network)

#CPER testing
cper.SPEI.1<-filter(spei.12, site == "CPER")

cper.ANPP.1<-anpp_v1%>%
  filter(site == "CPER")%>%
  group_by(year)%>%
  summarize(anpp = mean(anpp_g_m2, na.rm=T))

cper.1<-left_join(cper.ANPP.1, cper.SPEI.1, by = 'year')

cper.anpp.mean <- cper.1%>%
  filter(spei.cat == "normal")

cper.anpp.mean = mean(cper.anpp.mean$anpp)
cper.anpp.mean.all =mean(cper.1$anpp)

cper.1<-cper.1%>%
  mutate(anpp.resist = cper.anpp.mean/(anpp - cper.anpp.mean))%>%
  mutate(perc.change = 100*((anpp - cper.anpp.mean.all)/cper.anpp.mean.all) )

cper.norm<-cper.1%>%
  filter(spei.cat == "normal")

cper.ext<-cper.1%>%
  filter(spei.cat != "normal")%>%
  mutate(spei.abs = abs(SPEI))

#lm.cper.norm <-lm(anpp.resist~SPEI, data = cper.norm)
#summary(lm.cper.norm)

#lm.cper.ext <-lm(anpp.resist~SPEI, data = cper.ext)
#summary(lm.cper.ext)


ggplot(cper.ext, aes(x = spei.abs, y = perc.change)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = TRUE, color = "red") +   # regression line
  stat_cor(method = "pearson", label.x = 2, label.y = 0.8)+ # correlation coefficient
  facet_wrap(~spei.cat)+
  labs(title = "CPER")
  
#PRHPA testing
prhpa.SPEI.1<-filter(spei.12, site == "PRHPA")

prhpa.ANPP.1<-anpp_v1%>%
  filter(treatment == "PRHPA_NEMELTCRS_ROT42")%>%
  filter(crop == "Corn")

prhpa.1<-left_join(prhpa.ANPP.1, prhpa.SPEI.1, by = 'year')

prhpa.anpp.mean <- prhpa.1%>%
  filter(spei.cat == "normal")

prhpa.anpp.mean = mean(prhpa.anpp.mean$anpp_g_m2, na.rm=T)
prhpa.anpp.mean.all =mean(prhpa.1$anpp_g_m2, na.rm=T)

prhpa.1<-prhpa.1%>%
  filter(!is.na(anpp_g_m2))%>%
  mutate(anpp.resist = prhpa.anpp.mean/(anpp_g_m2 - prhpa.anpp.mean))%>%
  mutate(perc.change = 100*((anpp_g_m2 - prhpa.anpp.mean.all)/prhpa.anpp.mean.all) )


prhpa.norm<-prhpa.1%>%
  filter(spei.cat == "normal")

prhpa.ext<-prhpa.1%>%
  filter(spei.cat != "normal")%>%
  mutate(spei.abs = abs(SPEI))


ggplot(prhpa.ext, aes(x = spei.abs, y = perc.change)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = TRUE, color = "red") +   # regression line
  stat_cor(method = "pearson", label.x = 2, label.y = 0.8)+ # correlation coefficient
  facet_wrap(~spei.cat)+
  labs(title = "PRHPA_NEMELTCRS_ROT42 Corn")

#########################################
#SPEI 3
#########################################

#Dave couldn't get file to upload from gdrive, so importing manually 
spei.3<-read.csv("C:/Users/david.hoover/OneDrive - USDA/HomeDrive/Projects/NCEAS_resilience/SPEI03.csv")

#extract 12 month values for August; categorize extreme
spei.3<-spei.3%>%
  mutate(month = month(as.Date(date)))%>%
  mutate(year = year(as.Date(date)))%>%
  filter(month == '6')%>%
  filter(year < 2025 & year > 1981)%>%
  pivot_longer(cols = 2:56, names_to = "site", values_to = "SPEI")%>%
  mutate(spei.cat = ifelse(SPEI>0.99, "wet", ifelse (SPEI <(-0.99), "dry", "normal")))


#CPER testing
cper.SPEI.3<-filter(spei.3, site == "CPER")

cper.ANPP.3<-anpp_v1%>%
  filter(site == "CPER")%>%
  group_by(year)%>%
  summarize(anpp = mean(anpp_g_m2, na.rm=T))

cper.3<-left_join(cper.ANPP.3, cper.SPEI.3, by = 'year')

cper.anpp.mean.3 <- cper.3%>%
  filter(spei.cat == "normal")

cper.anpp.mean.3 = mean(cper.anpp.mean.3$anpp)
cper.anpp.mean.all.3 =mean(cper.3$anpp)

cper.3<-cper.3%>%
  #mutate(anpp.resist = cper.anpp.mean.3/(anpp - cper.anpp.mean.3))%>%
  mutate(perc.change = 100*((anpp - cper.anpp.mean.all.3)/cper.anpp.mean.all.3))

cper.norm.3<-cper.3%>%
  filter(spei.cat == "normal")

cper.ext.3<-cper.3%>%
  filter(spei.cat != "normal")%>%
  mutate(spei.abs = abs(SPEI))

#lm.cper.norm <-lm(anpp.resist~SPEI, data = cper.norm)
#summary(lm.cper.norm)

#lm.cper.ext <-lm(anpp.resist~SPEI, data = cper.ext)
#summary(lm.cper.ext)


ggplot(cper.ext.3, aes(x = spei.abs, y = perc.change)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = TRUE, color = "red") +   # regression line
  stat_cor(method = "pearson", label.x = 2, label.y = 0.8)+ # correlation coefficient
  facet_wrap(~spei.cat)+
  labs(title = "CPER")


#PRHPA testing
prhpa.SPEI.3<-filter(spei.3, site == "PRHPA")

prhpa.ANPP.3<-anpp_v1%>%
  filter(treatment == "PRHPA_NEMELTCRS_ROT42")%>%
  filter(crop == "Corn")

prhpa.3<-left_join(prhpa.ANPP.3, prhpa.SPEI.3, by = 'year')

prhpa.anpp.mean.3 <- prhpa.3%>%
  filter(spei.cat == "normal")

prhpa.anpp.mean.3 = mean(prhpa.anpp.mean.3$anpp_g_m2, na.rm=T)

prhpa.3<-prhpa.3%>%
  filter(!is.na(anpp_g_m2))%>%
  mutate(anpp.resist = prhpa.anpp.mean/(anpp_g_m2 - prhpa.anpp.mean))

prhpa.norm.3<-prhpa.3%>%
  filter(spei.cat == "normal")

prhpa.ext.3<-prhpa.3%>%
  filter(spei.cat != "normal")%>%
  mutate(spei.abs = abs(SPEI))


ggplot(prhpa.ext.3, aes(x = spei.abs, y = anpp.resist)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = TRUE, color = "red") +   # regression line
  stat_cor(method = "pearson", label.x = 2, label.y = 0.8)+ # correlation coefficient
  facet_wrap(~spei.cat)+
  labs(title = "PRHPA_NEMELTCRS_ROT42 Corn")
