#Extremes SPEI EDA 
#Tom + Lina 
#11.05.2025
#Relate extremes to ANPP based on SPEI, using code from SPEI-ANPP(Hoover).


library(googledrive)
library(tidyverse)
library(lubridate)
library(lme4)
library(emmeans)
library(ggpubr)


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
focal_file <- "spei12.csv"

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
  filter(month == '10')%>%
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
  ))%>%
  mutate(category = ifelse(type %in% c('Soybean', 'Corn', 'Wheat'), 'Crop', type))%>%
  filter(!(type %in% '999'))

clean_data_lag <- clean_data%>%
  group_by(site, network, crop, fertilized, N, P, K, grazed, burned, burn_freq, seeded, till)%>%
  arrange(w_yr)%>%
  mutate(lag.spei.cat = lag(spei.cat))%>%
  unite(compound, lag.spei.cat, spei.cat, remove = F)
  
clean_data_lag%>%
  group_by(type, compound)%>%
  summarise(n.obs = n())%>%
  filter(!(compound %in% c('NA_NA', 'normal_normal', 'NA_dry', 'NA_normal', 'NA_wet')))%>%
  ggplot(aes(x = type, y = n.obs, fill = compound))+
  geom_bar(stat = 'identity', position = position_dodge())+
  coord_cartesian(ylim = c(0, 30))


ggplot(merged_data, aes(x = per_dev_anpp))+
  geom_density()

#plot SPEI split by type and dry vs wet
ggplot(clean_data, aes(x = SPEI, y = scaled_anpp, color = type))+
  geom_point(alpha = 0.5)+
  geom_smooth(aes(shape = spei.cat), method = 'lm', se = F, linewidth = 1.4)+
  labs(x = 'SPEI', y = 'ANPP z-score')+
  theme_bw()

#generally plots less sensitive to wet years than dry years




ggplot(clean_data, aes(x = scaled_ppt, y = scaled_anpp, color = type))+
  geom_point()+
  labs(x = 'water year ppt z-score', y = 'ANPP z-score')+
  theme_bw()

#bin extreme dry, extreme wet, and normal
binned_data <- clean_data %>%
  mutate(extreme_10 = case_when(
    scaled_ppt <= -1.282 ~ "extreme dry",
    scaled_ppt >= 1.282 ~ "extreme wet",
    TRUE ~ "normal"
    ),
    extreme_15 = case_when(
      scaled_ppt <= -1.036 ~ "extreme dry",
      scaled_ppt >= 1.036 ~ "extreme wet",
      TRUE ~ "normal"
    ),
    extreme_20 = case_when(
      scaled_ppt <= -0.84 ~ "extreme dry",
      scaled_ppt >= 0.84 ~ "extreme wet",
      TRUE ~ "normal"
    ),
    extreme_30 = case_when(
      scaled_ppt <= -0.524 ~ "dry",
      scaled_ppt >= 0.524 ~ "wet",
      TRUE ~ "normal")) %>%
  filter(type != "999") 

binned_data$extreme_30 <- factor(binned_data$extreme_30, 
                                 levels = c("dry", "normal", "wet"))

binned_data$extreme_20 <- factor(binned_data$extreme_20, 
                                 levels = c("extreme dry", "normal", "extreme wet"))
binned_data$extreme_15 <- factor(binned_data$extreme_15, 
                                 levels = c("extreme dry", "normal", "extreme wet"))
binned_data$extreme_10 <- factor(binned_data$extreme_10, 
                                 levels = c("extreme dry", "normal", "extreme wet"))

extreme10 <- ggplot(binned_data, aes(x = extreme_10, y = scaled_anpp, color = type, fil = type))+
  geom_boxplot()+
  geom_jitter()+
  facet_grid(~type)+
  theme_bw()

extreme15 <- ggplot(binned_data, aes(x = extreme_15, y = scaled_anpp, color = type, fil = type))+
  geom_boxplot()+
  geom_jitter()+
  facet_grid(~type)+
  theme_bw()

extreme20 <- ggplot(binned_data, aes(x = extreme_20, y = scaled_anpp, color = type, fil = type))+
  geom_boxplot()+
  geom_jitter()+
  facet_grid(~type)+
  theme_bw()

extreme30 <- ggplot(binned_data, aes(x = extreme_30, y = scaled_anpp, color = type, fil = type))+
  geom_boxplot()+
  geom_jitter()+
  facet_grid(~type)+
  theme_bw()

ggarrange(extreme10,  extreme20, extreme30, ncol = 1)

###response ratio
response_ratios_extreme10 <- binned_data %>%
  group_by(type, extreme_10)


###filter out grasslands
clean_data%>%
  filter(!(type %in% c('Grassland', 'Fert. Grassland')))%>%
  ggplot(aes(x = scaled_ppt, y = scaled_anpp, color = type))+
  geom_smooth(method = 'lm')+
  geom_point()+
  labs(title = 'demeanded (zscore) anpp x ppt no grassland', x = 'water year ppt z-score', y = 'ANPP z-score')+
  theme_bw()


clean_data%>%
  filter(!is.na(anpp_g_m2))%>%
  filter(!(type == '999'))%>%
  ggplot( aes(x = w_yr, y = scaled_ppt, group = interaction(site, network, crop,  N, P, K, grazed, burned, burn_freq, type)))+
  geom_line()+
  geom_point( size = 3, shape = 21, aes(fill = scaled_anpp))+
  labs(x = 'water year ', y = 'PPT z-score')+
  theme_bw()+
  scale_fill_viridis_c(option = 'H')+
  facet_wrap(~type, scale = 'free_x')


#grassland
grass_data <- clean_data%>%
  filter((type %in% c('Grassland', 'Fert. Grassland')))

ggplot(grass_data, aes(x = scaled_ppt, y = scaled_anpp, color = type))+
  geom_smooth(method = 'lm')+
  geom_point()+
  labs(title = 'demeanded (zscore) anpp x ppt no grassland', x = 'water year ppt z-score', y = 'ANPP z-score')+
  theme_bw()



###

library(segmented)
subset(cleancategory == 'Grassland')

grass.anpp.spei.lm <- lm(scaled_anpp ~ SPEI , data = subset(clean_data, category == 'Grassland'))

summary(anpp.spei.lm)

grass.anpp.seg <- segmented(anpp.spei.lm, 
          seg.Z = ~SPEI,
          psi = list (SPEI = c(-1, 1)),
          by = category)

summary(grass.anpp.seg)

crop.anpp.spei.lm <- lmer(scaled_anpp ~ SPEI + (1|site) + (1|year), data = subset(clean_data, category == 'Crop'))

summary(crop.anpp.spei.lm)

crop.anpp.seg <- segmented(crop.anpp.spei.lm, 
                            seg.Z = ~SPEI,
                            psi = list (SPEI = c(-1, 1))
                            )

summary(crop.anpp.seg)

