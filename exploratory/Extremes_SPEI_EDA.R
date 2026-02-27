#Extremes SPEI EDA 
#Tom + Lina 
#Updated 2/25/2026
#Relate extremes to ANPP based on SPEI, PPT, and temp using code from SPEI-ANPP(Hoover).


library(googledrive)
library(tidyverse)
library(lubridate)
library(lme4)
library(emmeans)
library(ggpubr)

# LOAD DATA
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

unique(anpp_data$site)
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

unique(ppt_data$site_id)

# Identify relevant tidy file
focal_file <- "heat_indices_site.csv"

# Download harmonized data file
googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/13Ymkrr-kRLDmpaj1jwwVOnOSmEYnF-dJ")) %>% 
  dplyr::filter(name == focal_file) %>% 
  googledrive::drive_download(file = .$id, overwrite = T,
                              path = file.path("data", "tidy", .$name))


# Read in harmonized data
temp_data  <- read.csv(file = file.path("data", "tidy", focal_file)) 

#CALCULATE SCALED VARIABLES
ppt_data_rel <- ppt_data%>%
  group_by(site_id, network)%>%
  mutate(mean_ppt = mean(wyr_ppt),
         per_dev_ppt = (wyr_ppt - mean_ppt)/mean_ppt,
         scaled_ppt  = scale(wyr_ppt)[,1],
         ppt_cat = ifelse(scaled_ppt < -1, 'D', ifelse( scaled_ppt < 1, 'A', 'W')))%>%
  rename(site = site_id)


anpp_data_rel <- anpp_data%>%
  group_by(site, network, crop, fertilized, N, P, K, grazed, burned, burn_freq, seeded, till) %>%
  mutate(mean_anpp = mean(anpp_g_m2, na.rm = T),
         per_dev_anpp = (anpp_g_m2 - mean_anpp)/mean_anpp,
         scaled_anpp  = (anpp_g_m2 - mean_anpp)/sd(anpp_g_m2, na.rm  = TRUE),
         n.obs = n())

temp_clean <- temp_data %>%
  rename(site = site_id, w_yr = year) %>%
  group_by(site, network)%>%
  mutate(mean_tmax = mean(Tmaxc),
         per_dev_tmax = (Tmaxc - mean_tmax)/mean_tmax,
         scaled_tmax  = scale(Tmaxc)[,1])%>%
  dplyr::select(w_yr, site, network, Tmaxc, mean_tmax, scaled_tmax, 
                num_days_95th, warm_day_90th, meanTmax_95th, Tmax_95th, consecutive_days_heat_wave)


##MERGE TABLES
merged_data <- ppt_data_rel%>%
  merge(anpp_data_rel, by = c('w_yr', 'site', 'network', 'wyr_ppt'))%>%
  left_join(spei.12.clean, by = c('w_yr', 'site')) %>%
  left_join(temp_clean, by = c('w_yr', 'network', 'site'))


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
ggplot(clean_data, aes(x = scaled_ppt, y = scaled_anpp, color = category))+
  geom_point(alpha = 0.5)+
  geom_smooth(aes(shape = spei.cat), method = 'lm', se = F, linewidth = 1.4)+
  labs(x = 'SPEI', y = 'ANPP z-score')+
  theme_bw()

#plot scaled ppt split by dry v wet
ggplot(clean_data, aes(x = scaled_ppt, y = scaled_anpp, color = category))+
  geom_point(alpha = 0.4)+
  geom_smooth(aes(group = interaction(ppt_cat, category)), method = 'lm', se = F, linewidth = 1.4)+
  labs(x = 'PPT (scaled)', y = 'ANPP (scaled)')+
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



####BREAKPOINT ANALYSIS####

library(segmented)

#SPEI

grass.anpp.spei.lm <- lm(scaled_anpp ~ SPEI , data = subset(clean_data, category == 'Grassland'))

summary(grass.anpp.spei.lm)

grass.anpp.seg.2 <- segmented(grass.anpp.spei.lm, 
          seg.Z = ~SPEI,
          #psi = list (SPEI = c(-1, 1)),
          type = 'aic',
          check.dslope = T)

grass.anpp.seg.1 <- segmented(grass.anpp.spei.lm, 
                            seg.Z = ~SPEI,
                            #psi = list (SPEI = c(-1, 1)),
                            type = 'aic',
                            Kmax = 1,
                            check.dslope = T)

summary(grass.anpp.seg.2)

#AIC comparison 
MuMIn::AICc(grass.anpp.seg.2, grass.anpp.seg.1, grass.anpp.spei.lm )


#plot pred
newdat <- data.frame(SPEI = seq(min(clean_data$SPEI, na.rm = T),
                                max(clean_data$SPEI, na.rm = T),
                                length.out = 300))

# Predict from segmented model
newdat$fit <- predict(grass.anpp.seg.1, newdata = newdat)

# Extract breakpoints
bp <- grass.anpp.seg.1$psi[, "Est."]

# Plot
ggplot(subset(clean_data, category == "Grassland"),
       aes(x = SPEI, y = scaled_anpp)) +
  geom_point(alpha = 0.4) +
  geom_line(data = newdat, aes(y = fit), color = "blue", size = 1.2) +
  geom_vline(xintercept = bp, color = "red", linetype = "dashed", size = 1) +
  theme_minimal(base_size = 14) +
  labs(title = "Segmented Regression: ANPP ~ SPEI",
       y = "Scaled ANPP",
       x = "SPEI")

#### clip grasslands to crop

clean_data%>%
  filter(category == 'Crop')%>%
  summarise(min.map = min(mean_ppt, na.rm = T),
            max.map = max(mean_ppt, na.rm = T))

#minimum crop map = 453.2325

wetgrassland <- clean_data%>%
  filter(category == 'Grassland')%>%
  filter(mean_ppt > 453 &  mean_ppt < 1106)


##run segmented regression on grasslands within climate

wetgrass.anpp.spei.lm <- lm(scaled_anpp ~ SPEI , data = wetgrassland)

summary(wetgrass.anpp.spei.lm)

wetgrass.anpp.seg <- segmented(wetgrass.anpp.spei.lm, 
                              seg.Z = ~SPEI,
                              #psi = list (SPEI = c(-1, 1)),
                              type = 'aic',
                              check.dslope = T)

summary(wetgrass.anpp.seg)
#plot prediction fro crop climate grasslands
newdat <- data.frame(SPEI = seq(min(clean_data$SPEI, na.rm = T),
                                max(clean_data$SPEI, na.rm = T),
                                length.out = 300))

# Predict from segmented model
newdat$fit <- predict(wetgrass.anpp.seg, newdata = newdat)

# Extract breakpoints
bp <- wetgrass.anpp.seg$psi[, "Est."]

# Plot
ggplot(data =wetgrassland,
       aes(x = SPEI, y = scaled_anpp)) +
  geom_point(alpha = 0.4) +
  geom_line(data = newdat, aes(y = fit), color = "blue", size = 1.2) +
  geom_vline(xintercept = bp, color = "red", linetype = "dashed", size = 1) +
  theme_minimal(base_size = 14) +
  labs(title = "Segmented Regression: ANPP ~ SPEI",
       y = "Scaled ANPP",
       x = "SPEI")

#### crops segemented

crop.anpp.spei.lm <- lm(scaled_anpp ~ SPEI  , data = subset(clean_data, category == 'Crop'))

summary(crop.anpp.spei.lm)

crop.anpp.seg <- segmented(crop.anpp.spei.lm, 
                           seg.Z = ~SPEI,
                           #psi = list (SPEI = c(-1, 1)),
                           type = 'aic',
                           check.dslope = T)

summary(crop.anpp.seg)

AIC(crop.anpp.seg, crop.anpp.spei.lm)
#plot pAICc()#plot prediction fro crop
newdat <- data.frame(SPEI = seq(min(clean_data$SPEI, na.rm = T),
                                max(clean_data$SPEI, na.rm = T),
                                length.out = 300))

# Predict from segmented model
newdat$fit <- predict(crop.anpp.seg, newdata = newdat)

# Extract breakpoints
bp <- crop.anpp.seg$psi[, "Est."]

# Plot
ggplot(data =wetgrassland,
       aes(x = SPEI, y = scaled_anpp)) +
  geom_point(alpha = 0.4) +
  geom_line(data = newdat, aes(y = fit), color = "blue", size = 1.2) +
  geom_vline(xintercept = bp, color = "red", linetype = "dashed", size = 1) +
  theme_minimal(base_size = 14) +
  labs(title = "Segmented Regression: ANPP ~ SPEI",
       y = "Scaled ANPP",
       x = "SPEI")



#### corn only  segemented

corn.anpp.spei.lm <- lm(scaled_anpp ~ SPEI  , data = subset(clean_data, type == 'Corn'))

summary(crop.anpp.spei.lm)

corn.anpp.seg <- segmented(corn.anpp.spei.lm, 
                           seg.Z = ~SPEI,
                           #psi = list (SPEI = c(-1, 1)),
                           type = 'aic',
                           check.dslope = T)

summary(corn.anpp.seg)

AIC(corn.anpp.seg, corn.anpp.spei.lm)
#plot pAICc()#plot prediction fro crop
newdat <- data.frame(SPEI = seq(min(clean_data$SPEI, na.rm = T),
                                max(clean_data$SPEI, na.rm = T),
                                length.out = 300))

# Predict from segmented model
newdat$fit <- predict(corn.anpp.seg, newdata = newdat)
newdat$lmfit <- predict(corn.anpp.spei.lm, newdata = newdat)

# Extract breakpoints
bp <- corn.anpp.seg$psi[, "Est."]

# Plot
ggplot(data = subset(clean_data, type == 'Corn'),
       aes(x = SPEI, y = scaled_anpp)) +
  geom_point(alpha = 0.4) +
  geom_line(data = newdat, aes(y = fit), color = "blue", size = 1.2) +
  geom_line(data = newdat, aes(y = lmfit), color = "green", size = 1.2) +
  geom_vline(xintercept = bp, color = "red", linetype = "dashed", size = 1) +
  theme_minimal(base_size = 14) +
  labs(title = "Segmented Regression: ANPP ~ SPEI",
       y = "Scaled ANPP",
       x = "SPEI")
 
#Precip

grass.anpp.ppt.lm <- lm(scaled_anpp ~ scaled_ppt , data = subset(clean_data, category == 'Grassland'))

summary(grass.anpp.ppt.lm)

grass.anpp.seg.2 <- segmented(grass.anpp.spei.lm, 
                              seg.Z = ~SPEI,
                              #psi = list (SPEI = c(-1, 1)),
                              type = 'aic',
                              check.dslope = T)

grass.anpp.seg.1 <- segmented(grass.anpp.ppt.lm, 
                              seg.Z = ~scaled_ppt,
                              #psi = list (SPEI = c(-1, 1)),
                              type = 'aic',
                              Kmax = 1,
                              check.dslope = T)

summary(grass.anpp.seg.1)

#AIC comparison 
MuMIn::AICc( grass.anpp.seg.1, grass.anpp.ppt.lm )


#plot pred
newdat <- data.frame(scaled_ppt = seq(min(clean_data$scaled_ppt, na.rm = T),
                                max(clean_data$scaled_ppt, na.rm = T),
                                length.out = 300))

# Predict from segmented model
newdat$fit <- predict(grass.anpp.seg.1, newdata = newdat)
newdat$lmfit <- predict(grass.anpp.ppt.lm, newdata = newdat)


# Extract breakpoints
bp <- grass.anpp.seg.1$psi[, "Est."]

# Plot
ggplot(subset(clean_data, category == "Grassland"),
       aes(x = scaled_ppt, y = scaled_anpp)) +
  geom_point(alpha = 0.4) +
  geom_line(data = newdat, aes(y = fit), color = "blue", size = 1.2) +
  geom_line(data = newdat, aes(y = lmfit), color = "green", size = 1.2) +
  geom_vline(xintercept = bp, color = "red", linetype = "dashed", size = 1) +
  theme_minimal(base_size = 14) +
  labs(title = "Segmented Regression: ANPP ~ SPEI",
       y = "Scaled ANPP",
       x = "Scaled PPT")

grass.anpp.ppt.lm <- lm(scaled_anpp ~ scaled_ppt , data = subset(clean_data, category == 'Grassland'))

summary(grass.anpp.ppt.lm)

##PPT climate 


grass.anpp.ppt.lm <- lm(scaled_anpp ~ scaled_ppt , data = subset(clean_data, category == 'Grassland'))

summary(grass.anpp.ppt.lm)


grass.anpp.seg.1 <- segmented(grass.anpp.ppt.lm, 
                              seg.Z = ~scaled_ppt,
                              #psi = list (SPEI = c(-1, 1)),
                              type = 'aic',
                              Kmax = 1,
                              check.dslope = T)

summary(grass.anpp.seg.1)

#AIC comparison 
MuMIn::AICc( grass.anpp.seg.1, grass.anpp.ppt.lm )


#plot pred
newdat <- data.frame(scaled_ppt = seq(min(clean_data$scaled_ppt, na.rm = T),
                                max(clean_data$scaled_ppt, na.rm = T),
                                length.out = 300))

# Predict from segmented model
newdat$fit <- predict(grass.anpp.seg.1, newdata = newdat)
newdat$lmfit <- predict(grass.anpp.ppt.lm, newdata = newdat)


# Extract breakpoints
bp <- grass.anpp.seg.1$psi[, "Est."]

# Plot
ggplot(subset(clean_data, category == "Grassland"),
       aes(x = scaled_ppt, y = scaled_anpp)) +
  geom_point(alpha = 0.4) +
  geom_line(data = newdat, aes(y = fit), color = "blue", size = 1.2) +
  geom_line(data = newdat, aes(y = lmfit), color = "green", linetype = 'dashed', size = 1.2) +
  geom_vline(xintercept = bp, color = "red",  size = 1) +
  theme_minimal(base_size = 14) +
  labs(title = "Grasslands",
       y = "Scaled ANPP",
       x = "Scaled PPT")

###croplands


crop.ppt.lm <- lm(scaled_anpp ~ scaled_ppt , data = subset(clean_data, category == 'Crop'))

summary(crop.ppt.lm)


crop.ppt.seg.1 <- segmented(crop.ppt.lm, 
                              seg.Z = ~scaled_ppt,
                              #psi = list (SPEI = c(-1, 1)),
                              type = 'aic',
                              Kmax = 1,
                              check.dslope = T)

summary(crop.ppt.seg.1)

#AIC comparison 
MuMIn::AICc( crop.ppt.seg.1, crop.ppt.lm )

#davies test
davies.test(crop.ppt.lm)

#plot pred
newdat <- data.frame(scaled_ppt = seq(min(clean_data$scaled_ppt, na.rm = T),
                                      max(clean_data$scaled_ppt, na.rm = T),
                                      length.out = 300))

# Predict from segmented model
newdat$fit <- predict(crop.ppt.seg.1, newdata = newdat)
newdat$lmfit <- predict(crop.ppt.lm, newdata = newdat)


# Extract breakpoints
bp <- crop.ppt.seg.1$psi[, "Est."]

# Plot
ggplot(subset(clean_data, category == "Crop"),
       aes(x = scaled_ppt, y = scaled_anpp)) +
  geom_point(alpha = 0.4) +
  geom_line(data = newdat, aes(y = fit), color = "blue", size = 1.2, linetype = 'dashed') +
  geom_line(data = newdat, aes(y = lmfit), color = "green", size = 1.2) +
  geom_vline(xintercept = bp, color = "red", linetype = "dashed", size = 1) +
  theme_minimal(base_size = 14) +
  labs(title = "Crop",
       y = "Scaled ANPP",
       x = "Scaled PPT")
#dry grasslands 

dry.grass.ppt.lm <- lm(scaled_anpp ~ scaled_ppt , data = subset(clean_data, category == 'Grassland' & mean_ppt < mean(clean_data$mean_ppt, na.rm = T)))

#davies test: tests whether breakpoint is warranted based on difference in slopes
davies.test(dry.grass.ppt.lm, k = 10)#significant


dry.grass.ppt.seg.1 <- segmented(dry.grass.ppt.lm, 
                              seg.Z = ~scaled_ppt,
                              #psi = list (SPEI = c(-1, 1)),
                              type = 'aic',
                              Kmax = 1,
                              check.dslope = T)

summary(dry.grass.ppt.seg.1)

#AIC comparison 
aiccs <- MuMIn::AICc( dry.grass.ppt.seg.1, dry.grass.ppt.lm )


deltaAIC <- aiccs[1,2] - aiccs[2,2]
#plot pred
newdat <- data.frame(scaled_ppt = seq(min(clean_data$scaled_ppt, na.rm = T),
                                      max(clean_data$scaled_ppt, na.rm = T),
                                      length.out = 300))

# Predict from segmented model
newdat$fit <- predict(dry.grass.ppt.seg.1, newdata = newdat)
newdat$lmfit <- predict(dry.grass.ppt.lm, newdata = newdat)


# Extract breakpoints
bp <-dry.grass.ppt.seg.1$psi[, "Est."]
bp <-dry.grass.ppt.seg.1$psi[, "Est."]
# Plot
ggplot(subset(clean_data, category == "Grassland"& mean_ppt < mean(clean_data$mean_ppt, na.rm = T)),
       aes(x = scaled_ppt, y = scaled_anpp)) +
  geom_point(alpha = 0.4) +
  geom_line(data = newdat, aes(y = fit), color = "blue", size = 1.2) +
  geom_line(data = newdat, aes(y = lmfit), color = "green", linetype = 'dashed', size = 1.2) +
  geom_text(aes(x = 1.5, y = 2.5, label = paste0('dAIC = ', round(deltaAIC,1))))+
  geom_vline(xintercept = bp, color = "red",  size = 1) +
  theme_minimal(base_size = 14) +
  labs(title = "Dry grasslands < mean(MAP)",
       y = "Scaled ANPP",
       x = "Scaled PPT")



#wet grasslands 

wet.grass.ppt.lm <- lm(scaled_anpp ~ scaled_ppt , data = subset(clean_data, category == 'Grassland' & mean_ppt > mean(clean_data$mean_ppt, na.rm = T)))

wet.grass.ppt.seg.1 <- segmented(wet.grass.ppt.lm, 
                                 seg.Z = ~scaled_ppt,
                                 #psi = list (SPEI = c(-1, 1)),
                                 type = 'aic',
                                 Kmax = 1,
                                 check.dslope = T)

summary(wet.grass.ppt.seg.1)

#AIC comparison 
aiccs <- MuMIn::AICc(wet.grass.ppt.seg.1, wet.grass.ppt.lm )

deltaAIC <- aiccs[1,2] - aiccs[2,2]
#davies test
davies.test(wet.grass.ppt.lm)

#plot pred
newdat <- data.frame(scaled_ppt = seq(min(clean_data$scaled_ppt, na.rm = T),
                                      max(clean_data$scaled_ppt, na.rm = T),
                                      length.out = 300))

# Predict from segmented model
newdat$fit <- predict(wet.grass.ppt.seg.1, newdata = newdat)
newdat$lmfit <- predict(wet.grass.ppt.lm, newdata = newdat)


# Extract breakpoints
bp <-wet.grass.ppt.seg.1$psi[, "Est."]

# Plot
ggplot(subset(clean_data, category == "Grassland"& mean_ppt < mean(clean_data$mean_ppt, na.rm = T)),
       aes(x = scaled_ppt, y = scaled_anpp)) +
  geom_point(alpha = 0.4) +
  geom_line(data = newdat, aes(y = fit), color = "blue", size = 1.2) +
  geom_line(data = newdat, aes(y = lmfit), color = "green", size = 1.2) +
  geom_text(aes(x = 1.5, y = 2.5, label = paste0('dAIC = ', round(deltaAIC,1))))+
  geom_vline(xintercept = bp, color = "red", linetype = "dashed", size = 1) +
  theme_minimal(base_size = 14) +
  labs(title = "Wet grasslands > mean(MAP)",
       y = "Scaled ANPP",
       x = "Scaled PPT")



#TEMP 
#Maximum temperature (absolute) during the growing season - Vogel 2019 
#length of the growing season: March-August
#grassland
grass.anpp.tmax.lm <- lm(scaled_anpp ~ scaled_tmax  , data = subset(clean_data, type == 'Grassland'))

summary(grass.anpp.tmax.lm)

grass.anpp.tmax.seg <- segmented(grass.anpp.tmax.lm, 
                                seg.Z = ~scaled_tmax,
                                type = 'aic',
                                check.dslope = T)

summary(grass.anpp.tmax.seg)

AIC(grass.anpp.tmax.seg,grass.anpp.tmax.lm) #lm is better
#plot pAICc()#plot prediction for crop
newdat <- data.frame(scaled_tmax = seq(min(clean_data$scaled_tmax, na.rm = T),
                                       max(clean_data$scaled_tmax, na.rm = T),
                                       length.out = 300))

# Predict from segmented model
newdat$fit <- predict(grass.anpp.tmax.seg, newdata = newdat)
newdat$lmfit <- predict(grass.anpp.tmax.lm, newdata = newdat)

# Extract breakpoints
bp <- grass.anpp.tmax.seg$psi[, "Est."]

# Plot
ggplot(data = subset(clean_data, type == 'Grassland'),
       aes(x = scaled_tmax, y = scaled_anpp)) +
  geom_point(alpha = 0.4) +
  geom_line(data = newdat, aes(y = fit), color = "blue", size = 1.2) +
  geom_line(data = newdat, aes(y = lmfit), color = "green", size = 1.2) +
  geom_vline(xintercept = bp, color = "red", linetype = "dashed", size = 1) +
  theme_minimal(base_size = 14) +
  labs(title = "Segmented Regression: ANPP ~ Tmax (Grassland)",
       y = "Scaled ANPP",
       x = "Scaled Tmax Growing Season")

#cropland
crop.anpp.tmax.lm <- lm(scaled_anpp ~ scaled_tmax  , data = subset(clean_data, category == 'Crop'))

summary(crop.anpp.tmax.lm)

crop.anpp.tmax.seg <- segmented(crop.anpp.tmax.lm, 
                                 seg.Z = ~scaled_tmax,
                                 type = 'aic',
                                 check.dslope = T)

summary(crop.anpp.tmax.seg) #one breakpoint

AIC(crop.anpp.tmax.seg,crop.anpp.tmax.lm) #seg is better
#plot pAICc()#plot prediction for crop
newdat <- data.frame(scaled_tmax = seq(min(clean_data$scaled_tmax, na.rm = T),
                                       max(clean_data$scaled_tmax, na.rm = T),
                                       length.out = 300))

# Predict from segmented model
newdat$fit <- predict(crop.anpp.tmax.seg, newdata = newdat)
newdat$lmfit <- predict(crop.anpp.tmax.lm, newdata = newdat)

# Extract breakpoints
bp <- crop.anpp.tmax.seg$psi[, "Est."]

# Plot
ggplot(data = subset(clean_data, category == 'Crop'),
       aes(x = scaled_tmax, y = scaled_anpp)) +
  geom_point(alpha = 0.4) +
  geom_line(data = newdat, aes(y = fit), color = "blue", size = 1.2) +
  geom_line(data = newdat, aes(y = lmfit), color = "green", size = 1.2) +
  geom_vline(xintercept = bp, color = "red", linetype = "dashed", size = 1) +
  theme_minimal(base_size = 14) +
  labs(title = "Segmented Regression: ANPP ~ Tmax (Cropland)",
       y = "Scaled ANPP",
       x = "Scaled Tmax Growing Season")

#corn only
corn.anpp.tmax.lm <- lm(scaled_anpp ~ scaled_tmax  , data = subset(clean_data, type == 'Corn'))

summary(corn.anpp.tmax.lm)

corn.anpp.tmax.seg <- segmented(corn.anpp.tmax.lm, 
                           seg.Z = ~scaled_tmax,
                           type = 'aic',
                           check.dslope = T)

summary(corn.anpp.tmax.seg)

AIC(corn.anpp.tmax.seg,corn.anpp.tmax.lm) #lm is better
#plot pAICc()#plot prediction for crop
newdat <- data.frame(scaled_tmax = seq(min(clean_data$scaled_tmax, na.rm = T),
                                max(clean_data$scaled_tmax, na.rm = T),
                                length.out = 300))

# Predict from segmented model
newdat$fit <- predict(corn.anpp.tmax.seg, newdata = newdat)
newdat$lmfit <- predict(corn.anpp.tmax.lm, newdata = newdat)

# Extract breakpoints
bp <- corn.anpp.tmax.seg$psi[, "Est."]

# Plot
ggplot(data = subset(clean_data, type == 'Corn'),
       aes(x = scaled_tmax, y = scaled_anpp)) +
  geom_point(alpha = 0.4) +
  geom_line(data = newdat, aes(y = fit), color = "blue", size = 1.2) +
  geom_line(data = newdat, aes(y = lmfit), color = "green", size = 1.2) +
  geom_vline(xintercept = bp, color = "red", linetype = "dashed", size = 1) +
  theme_minimal(base_size = 14) +
  labs(title = "Segmented Regression: ANPP ~ Tmax (Corn)",
       y = "Scaled ANPP",
       x = "Scaled Tmax Growing Season")

#Max daily Tmax of heat waves (2+ consecutive days over 95th percentile)
#grassland
grass.anpp.heatwave.lm <- lm(scaled_anpp ~ Tmax_95th  , data = subset(clean_data, type == 'Grassland'))

summary(grass.anpp.heatwave.lm)

grass.anpp.heatwave.seg <- segmented(grass.anpp.heatwave.lm, 
                                 seg.Z = ~Tmax_95th,
                                 type = 'aic',
                                 check.dslope = T)

summary(grass.anpp.heatwave.seg)

AIC(grass.anpp.heatwave.seg,grass.anpp.heatwave.lm) #lm is better
#plot pAICc()#plot prediction for crop
newdat <- data.frame(Tmax_95th = seq(min(clean_data$Tmax_95th, na.rm = T),
                                       max(clean_data$Tmax_95th, na.rm = T),
                                       length.out = 300))

# Predict from segmented model
newdat$fit <- predict(grass.anpp.heatwave.seg, newdata = newdat)
newdat$lmfit <- predict(grass.anpp.heatwave.lm, newdata = newdat)

# Extract breakpoints
bp <- grass.anpp.heatwave.seg$psi[, "Est."]

# Plot
ggplot(data = subset(clean_data, type == 'Grassland'),
       aes(x = Tmax_95th, y = scaled_anpp)) +
  geom_point(alpha = 0.4) +
  geom_line(data = newdat, aes(y = fit), color = "blue", size = 1.2) +
  geom_line(data = newdat, aes(y = lmfit), color = "green", size = 1.2) +
  geom_vline(xintercept = bp, color = "red", linetype = "dashed", size = 1) +
  theme_minimal(base_size = 14) +
  labs(title = "Segmented Regression: ANPP ~ Tmax_95th (Grassland)",
       y = "Scaled ANPP",
       x = "Tmax (degree C) of 2+ consecutive days over 95th percentile")

#cropland
crop.anpp.heatwave.lm <- lm(scaled_anpp ~ Tmax_95th  , data = subset(clean_data, category == 'Crop'))

summary(crop.anpp.heatwave.lm)

crop.anpp.heatwave.seg <- segmented(crop.anpp.heatwave.lm, 
                                     seg.Z = ~Tmax_95th,
                                     type = 'aic',
                                     check.dslope = T)

summary(crop.anpp.heatwave.seg)

AIC(crop.anpp.heatwave.seg,crop.anpp.heatwave.lm) #seg is better
#plot pAICc()#plot prediction for crop
newdat <- data.frame(Tmax_95th = seq(min(clean_data$Tmax_95th, na.rm = T),
                                     max(clean_data$Tmax_95th, na.rm = T),
                                     length.out = 300))

# Predict from segmented model
newdat$fit <- predict(crop.anpp.heatwave.seg, newdata = newdat)
newdat$lmfit <- predict(crop.anpp.heatwave.lm, newdata = newdat)

# Extract breakpoints
bp <- crop.anpp.heatwave.seg$psi[, "Est."]

# Plot
ggplot(data = subset(clean_data, category == 'Crop'),
       aes(x = Tmax_95th, y = scaled_anpp)) +
  geom_point(alpha = 0.4) +
  geom_line(data = newdat, aes(y = fit), color = "blue", size = 1.2) +
  geom_line(data = newdat, aes(y = lmfit), color = "green", size = 1.2) +
  geom_vline(xintercept = bp, color = "red", linetype = "dashed", size = 1) +
  theme_minimal(base_size = 14) +
  labs(title = "Segmented Regression: ANPP ~ Tmax_95th (Cropland)",
       y = "Scaled ANPP",
       x = "Tmax (degree C) of 2+ consecutive days over 95th percentile")

#corn only
corn.anpp.heatwave.lm <- lm(scaled_anpp ~ Tmax_95th  , data = subset(clean_data, type == 'Corn'))

summary(corn.anpp.heatwave.lm)

corn.anpp.heatwave.seg <- segmented(corn.anpp.heatwave.lm, 
                                    seg.Z = ~Tmax_95th,
                                    type = 'aic',
                                    check.dslope = T)

summary(corn.anpp.heatwave.seg)

AIC(corn.anpp.heatwave.seg,corn.anpp.heatwave.lm) #seg is better
#plot pAICc()#plot prediction for crop
newdat <- data.frame(Tmax_95th = seq(min(clean_data$Tmax_95th, na.rm = T),
                                     max(clean_data$Tmax_95th, na.rm = T),
                                     length.out = 300))

# Predict from segmented model
newdat$fit <- predict(corn.anpp.heatwave.seg, newdata = newdat)
newdat$lmfit <- predict(corn.anpp.heatwave.lm, newdata = newdat)

# Extract breakpoints
bp <- corn.anpp.heatwave.seg$psi[, "Est."]

# Plot
ggplot(data = subset(clean_data, type == 'Corn'),
       aes(x = Tmax_95th, y = scaled_anpp)) +
  geom_point(alpha = 0.4) +
  geom_line(data = newdat, aes(y = fit), color = "blue", size = 1.2) +
  geom_line(data = newdat, aes(y = lmfit), color = "green", size = 1.2) +
  geom_vline(xintercept = bp, color = "red", linetype = "dashed", size = 1) +
  theme_minimal(base_size = 14) +
  labs(title = "Segmented Regression: ANPP ~ Tmax_95th (Corn)",
       y = "Scaled ANPP",
       x = "Tmax (degree C) of 2+ consecutive days over 95th percentile")

#Length of heat waves (number of consecutive days over 95th percentile)
#grassland
grass.anpp.heat.lm <- lm(scaled_anpp ~ consecutive_days_heat_wave  , data = subset(clean_data, type == 'Grassland'))

summary(grass.anpp.heat.lm)

grass.anpp.heat.seg <- segmented(grass.anpp.heat.lm, 
                                     seg.Z = ~consecutive_days_heat_wave,
                                     type = 'aic',
                                     check.dslope = T)

summary(grass.anpp.heat.seg)

AIC(grass.anpp.heat.seg,grass.anpp.heat.lm) #lm is better
#plot pAICc()#plot prediction for crop
newdat <- data.frame(consecutive_days_heat_wave = seq(min(clean_data$consecutive_days_heat_wave, na.rm = T),
                                     max(clean_data$consecutive_days_heat_wave, na.rm = T),
                                     length.out = 300))

# Predict from segmented model
newdat$fit <- predict(grass.anpp.heat.seg, newdata = newdat)
newdat$lmfit <- predict(grass.anpp.heat.lm, newdata = newdat)

# Extract breakpoints
bp <- grass.anpp.heat.seg$psi[, "Est."]

# Plot
ggplot(data = subset(clean_data, type == 'Grassland'),
       aes(x = consecutive_days_heat_wave, y = scaled_anpp)) +
  geom_point(alpha = 0.4) +
  geom_line(data = newdat, aes(y = fit), color = "blue", size = 1.2) +
  geom_line(data = newdat, aes(y = lmfit), color = "green", size = 1.2) +
  geom_vline(xintercept = bp, color = "red", linetype = "dashed", size = 1) +
  theme_minimal(base_size = 14) +
  labs(title = "Segmented Regression: ANPP ~ heat wave length (Grassland)",
       y = "Scaled ANPP",
       x = "Number of consecutive days \n Tmax over 95th percentile")

#cropland
crop.anpp.heat.lm <- lm(scaled_anpp ~ consecutive_days_heat_wave  , data = subset(clean_data, category == 'Crop'))

summary(crop.anpp.heat.lm)

crop.anpp.heat.seg <- segmented(crop.anpp.heat.lm, 
                                 seg.Z = ~consecutive_days_heat_wave,
                                 type = 'aic',
                                 check.dslope = T)

summary(crop.anpp.heat.seg)

AIC(crop.anpp.heat.seg,crop.anpp.heat.lm) #lm is better
#plot pAICc()#plot prediction for crop
newdat <- data.frame(consecutive_days_heat_wave = seq(min(clean_data$consecutive_days_heat_wave, na.rm = T),
                                                      max(clean_data$consecutive_days_heat_wave, na.rm = T),
                                                      length.out = 300))

# Predict from segmented model
newdat$fit <- predict(crop.anpp.heat.seg, newdata = newdat)
newdat$lmfit <- predict(crop.anpp.heat.lm, newdata = newdat)

# Extract breakpoints
bp <- crop.anpp.heat.seg$psi[, "Est."]

# Plot
ggplot(data = subset(clean_data, category == 'Crop'),
       aes(x = consecutive_days_heat_wave, y = scaled_anpp)) +
  geom_point(alpha = 0.4) +
  geom_line(data = newdat, aes(y = fit), color = "blue", size = 1.2) +
  geom_line(data = newdat, aes(y = lmfit), color = "green", size = 1.2) +
  geom_vline(xintercept = bp, color = "red", linetype = "dashed", size = 1) +
  theme_minimal(base_size = 14) +
  labs(title = "Segmented Regression: ANPP ~ heat wave length (Cropland)",
       y = "Scaled ANPP",
       x = "Number of consecutive days \n Tmax over 95th percentile")
