# This script creates the cleaned ANPP data frame used in all subsequent analyses of the stability project. 
# Essentially it reads in teh data, updates crop names and requires at least 5 years of data
# Here we also tested detrending, but we have decided not to use that in the workflow

# DLH, OLH
# 4-16-2026

# Load libraries
library(tidyverse)
library(googledrive)
library(broom)
library(corrplot)
library(multcompView)
library(purrr)

theme_set(theme_bw(12))

# Create folders and directories on the local computer
dir.create(file.path("exploratory_graphs"), showWarnings = F)
dir.create(file.path("exploratory_graphs", 'anpp_year'), showWarnings = F)
dir.create(file.path("data"), showWarnings = F)
dir.create(file.path("data", "harmonized_data"), showWarnings = F)


#read in annp, precip, and trt info
file2<-'anpp_wyr_trt_merged.csv'
googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/13Ymkrr-kRLDmpaj1jwwVOnOSmEYnF-dJ")) %>% 
  dplyr::filter(name == file2) %>% 
  googledrive::drive_download(file = .$id, overwrite = T,
                              path = file.path("data", "harmonized_data", .$name))

##read in the data and do some pre-processing. and classifying land management include crop type.
dat<- read.csv(file = file.path("data", "harmonized_data", file2)) %>%
  filter(site!='look.us'&site!='bnch.us') %>% #drop two odd NutNet sites
  filter(treatment!="PRHPA_NEMERREM_CCN4N")%>%#removing second treatment for PRHPA
  #filter(crop!='Garbanzo'&crop!='Canola'&crop!='Oats') %>% 
  filter(!is.na(anpp_g_m2))%>% #this removes sites with grain yield but not anpp
  mutate(crop=tolower(crop)) %>% 
  mutate(crop2=case_when(
    crop %in% c('orchardgrass/white clover', 'orchard/fescue/clover/alfalfa/chicory', 'sorghum-sudangrass') ~ 'mixed_grass',
    TRUE~crop)) %>% 
  mutate(fertilized=ifelse(is.na(fertilized), 0, fertilized)) %>% #this is wrong b/c it is making CSCAP and ISI... 0 when should prob be 1.
  mutate(keep=ifelse(network=='NutNet'&treatment=='NPK'|network=='NutNet'&treatment=='Control', 1, 0)) %>% #dropping all nutnet treatments except control and NPK
  filter(keep==1|network !='NutNet') %>% 
  mutate(type=case_when(
    site == 'KNZ' & treatment == 'KNZ_Cropland' & crop2 == 'corn' ~ 'Corn',
    site == 'KNZ' & treatment == 'KNZ_Cropland' & crop2 == 'soybean' ~ 'Soybean',
    site == 'KNZ' & treatment == 'KNZ_Cropland' & crop2 == 'wheat' ~ 'Wheat',
    network=='LTER'~ 'Grassland',
    network=='NutNet'&fertilized==0 ~ 'Grassland', 
    network=='NutNet'&fertilized==1 ~ 'Fert. Grassland', 
    !network %in% c('LTER', 'NutNet') & crop2=="" ~ 'Grassland',
    !network %in% c('LTER', 'NutNet') & crop2 %in% c('mixed_grass', 'switchgrass', 'alfalfa') ~ 'Pasture', 
    !network %in% c('LTER', 'NutNet') & crop2=='corn' ~ 'Corn', 
    !network %in% c('LTER', 'NutNet') & crop2=='soybean' ~ 'Soybean',
    !network %in% c('NutNet') & crop2 %in% c('winter_wheat', 'spring_wheat', 'wheat') ~ 'Wheat',
    TRUE~'999'
  ))%>%
  mutate(duration_years = ifelse(site == 'LCB', 9, duration_years))%>%
  filter(!treatment %in% c('004b', '020b')) %>%
  filter(!(network == "DAP" & treatment %in% c(1,2)))


####Okay, we are going to combine to just four land management
dat_4cat<-dat %>% 
  mutate(type2=ifelse(type %in% c('Grassland', 'Fert. Grassland', 'Pasture'), type, 'Cropland'))

## Add in column for length based on crop type
length <- dat_4cat %>%
  group_by(network, site, type, type2, fertilized)%>%
  summarize(nobs_crop=n())

dat4.1<-dat_4cat %>%
  left_join(length, by = c('network', 'site', 'type', 'type2', 'fertilized')) %>%
  filter(type!="Pasture") %>% # removing pasture! 
  mutate(stab.analysis = ifelse(nobs_crop >4, 1, 0))

####Decided not to detrend so commented out this section ###################################################################
#detrending ANPP data - from Makki's 'data_prep_Timing_Critical.R, and based on  this paper https://doi.org/10.1016/j.agrformet.2018.09.019
#detrend_resid_plus_mean <- function(df, y_col, t_col) {
#  y <- df[[y_col]]
#  t <- df[[t_col]]
#  ok <- is.finite(y) & is.finite(t)

#  if (sum(ok) < 3) {
#    df[[paste0(y_col, "_dt")]] <- NA_real_
#    return(df)
#  }

#  fit <- lm(y[ok] ~ t[ok])
#  yhat <- rep(NA_real_, length(y))
#  yhat[ok] <- predict(fit)
#  
#  df[[paste0(y_col, "_dt")]] <- (y - yhat) + mean(y[ok], na.rm = TRUE)
#  df
#}

#anpp_dt <- dat_4cat %>%
#  group_by(network, site, type, type2, fertilized)%>% #detrend by type x site
#  group_modify(~{
#    df <- .x
#    df <- detrend_resid_plus_mean(df, "anpp_g_m2", "w_yr")
#    df
#  }) %>%
#  ungroup()
#checking trends in ANPP through time by site and type

#anpp_models <- dat_4cat %>%
#  group_by(site, type) %>%
#  filter(!is.na(anpp_g_m2))%>%
#  summarize(n= n(), Intercept = lm(anpp_g_m2 ~ year)$coefficients[1], 
#            Coeff_x1 = lm(anpp_g_m2 ~ year)$coefficients[2],
#            R2 = summary(lm(anpp_g_m2 ~ year))$r.squared,
#           pval = summary(lm(anpp_g_m2 ~ year))$coefficients["year", 4])

#anpp_models <- dat_4cat %>%
#  group_by(site, type) %>%
#  filter(!is.na(anpp_g_m2)) %>%
#  filter(n() > 4)%>%
#  summarize(
#    n = n(),
#    Intercept = lm(anpp_g_m2 ~ year)$coefficients[1],
#    Coeff_x1 = lm(anpp_g_m2 ~ year)$coefficients[2],
#    R2 = summary(lm(anpp_g_m2 ~ year))$r.squared,
#    pval = summary(lm(anpp_g_m2 ~ year))$coefficients["year", "Pr(>|t|)"]
#  )

#graph site x crop with p <0.06
#anpp_models.2<-anpp_models%>%
#  filter(pval < 0.05)%>%
#  select(site, type) %>%
#  mutate(site_type = paste(site, type, by = "_"))

#sig.sites <- dat_4cat %>%
#  mutate(site_type = paste(site, type, by = "_"))%>%
#  filter(site_type %in% anpp_models.2$site_type) %>%
#  filter(type != "Pasture")

#ggplot(data =sig.sites, aes(year, anpp_g_m2, color = type))+
#  geom_point()+
#  geom_smooth(method="lm")+
#  facet_wrap(~site, scales = "free")
#ggplot(data = sig.sites, aes(wyr_ppt, anpp_g_m2, color = type))+
#  geom_point()+
#  geom_smooth(method="lm")+
# facet_wrap(~site,  scales = "free")
#ggplot(data =sig.sites, aes(year, wyr_ppt,  color = type))+
#  geom_point()+
#  geom_smooth(method="lm")+
#  facet_wrap(~site, scales = "free")

#ggplot(data= subset(dat_4cat, dat_4cat$type == "Corn"), aes(year, anpp_g_m2))+
#  geom_point()+
#  geom_smooth(method="lm")

####Decided not to detrend so commented out this above section ###################################################################



## dat_4cat is the final data set, going to save this in in the harmonized data folder and upload to the drive
output <- 'stability_anpp2.csv'

# Export locally
write.csv(x = dat4.1 , row.names = F, na = '',
          file = file.path("data", "harmonized_data", output))

# Upload to Drive
googledrive::drive_upload(media = file.path("data", "harmonized_data", output), overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/13Ymkrr-kRLDmpaj1jwwVOnOSmEYnF-dJ"))


