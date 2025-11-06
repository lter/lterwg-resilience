#Extremes SPEI EDA 
#Tom + Lina 
#11.05.2025
#Relate extremes to ANPP based on SPEI, using code from SPEI-ANPP(Hoover).


library(googledrive)
library(tidyverse)
library(lubridate)
library(lme4)
library(emmeans)


# Clear environment + collect garbage
rm(list = ls()); gc()

# Identify relevant tidy file
focal_file <- "anpp_wyr_trt_merged.csv"

# Download harmonized data file
googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/13Ymkrr-kRLDmpaj1jwwVOnOSmEYnF-dJ")) %>% 
  dplyr::filter(name == focal_file) %>% 
  googledrive::drive_download(file = .$id, overwrite = T,
                              path = file.path("data", "tidy", .$name))

# Read in harmonized data
anpp_data  <- read.csv(file = file.path("data", "tidy", focal_file))

# Identify desired file
focal_file <- "SPEI12.csv"

# Download harmonized data file
googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1JtFMD4IAizjNGd0wbLLZIBdgqnk4YR97")) %>% 
  dplyr::filter(name == focal_file) %>% 
  googledrive::drive_download(file = .$id, overwrite = T,
                              path = file.path("data", "tidy", .$name))

#Dave couldn't get file to upload from gdrive, so importing manually 
spei.12 <- read.csv(file = file.path("data", "tidy", focal_file))

#extract 12 month values for August; categorize extreme
spei.12.clean <- spei.12%>%
  mutate(month = month(as.Date(date)))%>%
  mutate(year = year(as.Date(date)))%>%
  filter(month == '8')%>%
  filter(year < 2025 & year > 1981)%>%
  pivot_longer(cols = 2:56, names_to = "site", values_to = "SPEI")%>%
  mutate(spei.cat = ifelse(SPEI>0.99, "wet", ifelse (SPEI <(-0.99), "dry", "normal")))%>%
  rename(w_yr = year )%>%
  dplyr::select(w_yr, site, SPEI, spei.cat)

# Identify relevant tidy file
focal_file <- "01_wyr_ppt_all_yrs.csv"

# Download harmonized data file
googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/13Ymkrr-kRLDmpaj1jwwVOnOSmEYnF-dJ")) %>% 
  dplyr::filter(name == focal_file) %>% 
  googledrive::drive_download(file = .$id, overwrite = T,
                              path = file.path("data", "tidy", .$name))


# Read in harmonized data
ppt_data  <- read.csv(file = file.path("data", "tidy", focal_file))


ppt_data_rel <- ppt_data%>%
  group_by(site_id, network)%>%
  mutate(mean_ppt = mean(wyr_ppt),
         per_dev_ppt = (wyr_ppt - mean_ppt)/mean_ppt,
         scaled_ppt  = scale(wyr_ppt)[,1])%>%
  rename(site = site_id)


anpp_data_rel <- anpp_data%>%
  group_by(site, network, crop, fertilized, N, P, K, grazed, burned, burn_freq, seeded, till) %>%
  mutate(mean_anpp = mean(anpp_g_m2, na.rm = T),
         per_dev_anpp = (anpp_g_m2 - mean_anpp)/mean_anpp,
         scaled_anpp  = (anpp_g_m2 - mean_anpp)/sd(anpp_g_m2, na.rm  = TRUE),
         n.obs = n())



merged_data <- ppt_data_rel%>%
  merge(anpp_data_rel, by = c('w_yr', 'site', 'network', 'wyr_ppt'))%>%
  left_join(spei.12.clean, by = c('w_yr', 'site'))


clean_data <- merged_data%>%
  filter(site!='look.us'&site!='bnch.us') %>% #drop two odd NutNet sites
  #filter(crop!='Garbanzo'&crop!='Canola'&crop!='Oats') %>% 
  mutate(crop=tolower(crop)) %>% 
  mutate(crop2=case_when(
    crop %in% c('orchardgrass/white clover', 'orchard/fescue/clover/alfalfa/chicory', 'sorghum-sudangrass') ~ 'mixed_grass',
    TRUE~crop)) %>% 
  mutate(fertilized=ifelse(is.na(fertilized), 0, fertilized)) %>% #this is wrong b/c it is making CSCAP and ISI... 0 when should prob be 1.
  mutate(keep=ifelse(network=='NutNet'&treatment=='NPK'|network=='NutNet'&treatment=='Control', 1, 0)) %>% #dropping all nutnet treatments except control and NPK
  filter(keep==1|network !='NutNet') %>% 
  mutate(type=case_when(
    network=='LTER'~ 'Grassland',
    network=='NutNet'&fertilized==0 ~ 'Grassland', 
    network=='NutNet'&fertilized==1 ~ 'Fert. Grassland', 
    !network %in% c('LTER', 'NutNet') & crop2=="" ~ 'Grassland',
    !network %in% c('LTER', 'NutNet') & crop2 %in% c('mixed_grass', 'switchgrass', 'alfalfa') ~ 'Pasture', 
    !network %in% c('LTER', 'NutNet') & crop2=='corn' ~ 'Corn', 
    !network %in% c('LTER', 'NutNet') & crop2=='soybean' ~ 'Soybean',
    !network %in% c('LTER', 'NutNet') & crop2 %in% c('winter_wheat', 'spring_wheat') ~ 'Wheat',
    TRUE~'999'
  ))


ggplot(merged_data, aes(x = per_dev_anpp))+
  geom_density()

ggplot(clean_data, aes(x = SPEI, y = scaled_anpp, color = type))+
  geom_point(alpha = 0.5)+
  geom_smooth(aes(shape = spei.cat), method = 'lm', se = F, linewidth = 1.4)+
  labs(x = 'SPEI', y = 'ANPP z-score')+
  theme_bw()

ggplot(clean_data, aes(x = scaled_ppt, y = scaled_anpp, color = type))+
  geom_point()+
  labs(x = 'water year ppt z-score', y = 'ANPP z-score')+
  theme_bw()


clean_data%>%
  filter(!(type %in% c('Grassland', 'Fert. Grassland')))%>%
  ggplot(aes(x = scaled_ppt, y = scaled_anpp, color = type))+
  geom_point()+
  labs(title = 'demeanded (zscore) anpp x ppt no grassland', x = 'water year ppt z-score', y = 'ANPP z-score')+
  theme_bw()


# ggplot(merged_data, aes(x = scaled_ppt, y = scaled_anpp, color = crop))+
#   geom_point()
# merged_data%>%
#   filter(crop != '')%>%
#   mutate(crop = ifelse(crop == 'Soybean', 'soybean', ifelse(crop == 'Orchard/fescue/clover/alfalfa/chicory', 'Mixed_grass', ifelse(crop == 'Orchardgrass/white clover', 'Mixed_grass', ifelse (co))))%>%
#   ggplot( aes(x = scaled_ppt, y = scaled_anpp, color = crop))+
#   geom_point()

# Identify relevant tidy file
focal_file <- "_all-sites_whiplash-data.csv"

# Download harmonized data file
googledrive::drive_ls(googledrive::as_id("http://drive.google.com/drive/u/0/folders/179rWnh1171GTEhb-IbTQh7gUvaJ8IOAG")) %>% 
  dplyr::filter(name == focal_file) %>% 
  googledrive::drive_download(file = .$id, overwrite = T,
                              path = file.path("data", "tidy", .$name))


whiplash_data  <- read.csv(file = file.path("data", "tidy", focal_file))%>%
  mutate(date = date(date),
         year = year(date))

whiplash_data%>%
  group_by(site, year)%>%
  summarise(n.yr = n())%>%
  filter(n.yr > 1)%>%
  filter(year > 1990)

#relativize ANPP data
anpp_data_rel <- clean_data%>%
  group_by(site, network, crop, fertilized, N, P, K, grazed, burned, burn_freq, seeded, till, type) %>%
  mutate(mean_anpp = mean(anpp_g_m2, na.rm = T),
         per_dev_anpp = (anpp_g_m2 - mean_anpp)/mean_anpp,
         scaled_anpp  = (anpp_g_m2 - mean_anpp)/sd(anpp_g_m2, na.rm  = TRUE),
         n.obs = n())

