#1. How do the ANPP-ppt and ANPP-temp relationships vary by system?
#2. How variable are these relationships among sites within each system?
#3. How do non-linearities in these relationships compare across systems? #Does the ANPP response saturate? 

library(lubridate)
library(tidyverse)
library(googledrive)
library(cowplot)

dir.create(file.path("data", "harmonized_data"), showWarnings = F)
dir.create(file.path("data", "pre_processed_data"), showWarnings = F)

theme_set(theme_bw(12))

#read in ppt water year data
wyr_ppt<-'01_wyr_ppt_all_yrs.csv'
googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/13Ymkrr-kRLDmpaj1jwwVOnOSmEYnF-dJ")) %>% 
  dplyr::filter(name == wyr_ppt) %>% 
  googledrive::drive_download(file = .$id, overwrite = T,
                              path = file.path("data", "harmonized_data", .$name))
wyr_ppt_data <- read.csv(file = file.path("data", "harmonized_data", wyr_ppt))

#read in annp, precip, and trt info
file2<-'anpp_wyr_trt_merged.csv'
googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/13Ymkrr-kRLDmpaj1jwwVOnOSmEYnF-dJ")) %>% 
  dplyr::filter(name == file2) %>% 
  googledrive::drive_download(file = .$id, overwrite = T,
                              path = file.path("data", "harmonized_data", .$name))
anpp_data <- read.csv(file = file.path("data", "harmonized_data", file2))

#calculate +-1SD of long-term avg MSWEP ppt
wyr_ppt_summary <- wyr_ppt_data %>%
  group_by(site_id, network) %>%
  summarize(mean_ppt = mean(wyr_ppt),
            sd_ppt = sd(wyr_ppt),
            sd_upper_ppt = mean_ppt+sd_ppt,
            sd_lower_ppt = mean_ppt-sd_ppt) %>%
  rename(site = site_id)

#merge ANPP and ppt summary tables
merge_anpp_wyr_ppt <- anpp_data %>%
  left_join(., wyr_ppt_summary, by = c("site", "network"))

#identify sites less than years of anpp data and all years outside of +-1SD long-term avg MSWEP ppt
omit_select_sites <- merge_anpp_wyr_ppt %>%
  group_by(site) %>%
  filter(duration_years < 5 | all(wyr_ppt < sd_lower_ppt | wyr_ppt > sd_upper_ppt, na.rm = TRUE)) %>%
  distinct(site) %>%
  pull(site)
print(omit_select_sites)

#borrow Dave's data processing script in stability_analysis_DLH.R:
dat<- merge_anpp_wyr_ppt %>%
  filter(!site %in% c('BRADFORD.C', 'DPAC', 'HOYTVILLE.LTR', 'MAR', 'MO_Knox1',
                  'MO_Knox2', 'MO_Knox4', 'WOOSTER.LTR', 'lake.us', 'msla.us',
                  'msla_2.us', 'msla_3.us', 'unc.us')) %>% #drop all omit sites identified above
  filter(site!='look.us'& site!='bnch.us') %>% #drop two odd NutNet sites
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
  filter(!treatment %in% c('004b', '020b'))

#classify systems to just four land management types
dat_4cat<-dat %>% 
  mutate(type2=ifelse(type %in% c('Grassland', 'Fert. Grassland', 'Pasture'), type, 'Cropland'))
  
#remaking Olivia's Figure but no Pasture
f_ppt <- ggplot(data=dat_4cat %>% filter(type2 != "Pasture"), aes(x=wyr_ppt, y=anpp_g_m2))+
  geom_point(alpha = 0.1, aes(color=type2)) +
  geom_smooth(aes(shape = as.factor(site), color = type2), 
              method = 'lm', formula = 'y ~ x', se = F,
              alpha = 0.1, linewidth = 0.2) +
  geom_smooth(aes(color = type2), method = 'lm', formula = 'y ~ x', se = T)+
  scale_color_manual(name='Systems', values=c('orange','green4', 'green'))+
  xlab('Annual Precipitation (mm)')+
  ylab(expression(paste('ANPP (g ', m^-2,')')))+
  theme(panel.grid = element_blank())+
  theme_classic()

model <- lm(anpp_g_m2 ~ wyr_ppt * type2, data = dat_4cat) 
summary(model)
slopes <- emtrends(model,  ~type2, var = "wyr_ppt")
pairs(slopes) #none of the slopes are significantly different

##calculating sensitivity by site
sensitivity <-dat_4cat %>% 
  filter(!is.na(anpp_g_m2)) %>%
  group_by(network,site, type2, fertilized, mean_ppt) %>% 
  summarise(slope=lm(anpp_g_m2~wyr_ppt)$coefficient[2], 
            nobs=n(), manpp=mean(anpp_g_m2), sd=sd(anpp_g_m2), 
            var_slope = sd(slope, na.rm =TRUE)) 

ggplot(data=sensitivity%>% filter(type2 != "Pasture"), aes(x=mean_ppt, y=slope, color = type2))+
  theme_classic() +
  scale_color_manual(name='Systems', values=c('orange', 'green', 'green4'))+
  geom_point()+
  geom_smooth(method = 'loess', se=F)

f_inset_ppt <- ggplot(data=sensitivity%>% filter(type2 != "Pasture"), aes(x=type2, y=slope, fill = type2))+
  theme_classic() +
  scale_fill_manual(name='Systems', values=c('orange', 'green', 'green4'))+
  geom_boxplot()+
  geom_point()+
  theme(axis.title.x = element_blank(), legend.position = "none")

#variability among sites within each system
var_slopes <-sensitivity %>% 
  group_by(type2) %>% 
  summarise(var_slopes=var(slope, na.rm = TRUE),
            sd_slopes = sd(slope, na.rm = TRUE),
            mean_slopes = mean(slope, na.rm = TRUE),
            cv_slopes = sd_slopes/mean_slopes) 

##################################################### 
#Read in temp data
# Identify desired file
focal_file <- "daymet_daily_weather.csv"

# Download data file
googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1Sw-CdVIsCNvnS3laPn1a90WHoZsEoMif")) %>% 
  dplyr::filter(name == focal_file) %>% 
  googledrive::drive_download(file = .$id, overwrite = T,
                              path = file.path("data", "pre_processed_data", .$name))

# Read in data
daymet_daily_raw <- read.csv(file = file.path("data", "pre_processed_data", focal_file))

# Calculate annual mean temp 
annual_mean_temp <- daymet_daily_raw %>%
  group_by(network, site_id, year) %>% 
  summarise(Tmean = mean(tmean_degC))

colnames(annual_mean_temp)[2] <- "site"


# calculate growing season mean and max temp
gr_temp <- daymet_daily_raw %>%
  group_by(network, site_id, year) %>% 
  filter(between(month, 4, 8)) %>%
  summarise(gr_Tmax = max(tmax_degC),
            gr_Tmean = mean(tmean_degC))

colnames(gr_temp)[2] <- "site"

dat_all <- dat_4cat %>% 
  left_join(annual_mean_temp) %>%
  left_join(gr_temp)

#growing season T mean
ggplot(data=dat_all %>% filter(type2 != "Pasture"), aes(x=gr_Tmean, y=anpp_g_m2))+
  geom_point(alpha = 0.1, aes(color=type2)) +
  geom_smooth(aes(shape = as.factor(site), color = type2), 
              method = 'lm', formula = 'y ~ x', se = F,
              alpha = 0.1, linewidth = 0.2) +
  geom_smooth(aes(color = type2), method = 'lm', formula = 'y ~ x', se = T)+
  scale_color_manual(name='Systems', values=c('orange', 'green', 'green4'))+
  xlab('Growing Season Mean Temperature (degrees C)')+
  ylab(expression(paste('ANPP (g ', m^-2,')')))+
  theme(panel.grid = element_blank())+
  theme_classic()

model2 <- lm(anpp_g_m2 ~ gr_Tmean * type2, data =dat_all) 
summary(model2)
slopes2 <- emtrends(model2,  ~type2, var = "gr_Tmean")
pairs(slopes2) #none of the slopes are significantly different

#growing season T max
f_tmax <- ggplot(data=dat_all %>% filter(type2 != "Pasture"), aes(x=gr_Tmax, y=anpp_g_m2))+
  geom_point(alpha = 0.1, aes(color=type2)) +
  geom_smooth(aes(shape = as.factor(site), color = type2), 
              method = 'lm', formula = 'y ~ x', se = F,
              alpha = 0.1, linewidth = 0.2) +
  geom_smooth(aes(color = type2), method = 'lm', formula = 'y ~ x', se = T)+
  scale_color_manual(name='Systems', values=c('orange', 'green', 'green4'))+
  xlab('Growing Season Max Temperature (degrees C)')+
  ylab(expression(paste('ANPP (g ', m^-2,')')))+
  theme(panel.grid = element_blank())+
  theme_classic()

model3 <- lm(anpp_g_m2 ~ gr_Tmax * type2, data =dat_all) 
summary(model3)
slopes3 <- emtrends(model3,  ~type2, var = "gr_Tmax")
pairs(slopes3) #Cropland - grassland and Fert. grassland - grassland are significantly different

#calculate mean growing season temp of each site
mean_gr_temp <- daymet_daily_raw %>%
  group_by(network, site_id) %>% 
  filter(between(month, 4, 8)) %>%
  summarise(mean_gr_Tmean = mean(tmean_degC),
            mean_gr_Tmax = mean(tmax_degC))
colnames(mean_gr_temp)[2] <- "site"

sens_temp<-dat_all %>% 
  left_join(mean_gr_temp) %>%
  filter(!is.na(anpp_g_m2)) %>%
  filter(!is.na(gr_Tmean)) %>%
  filter(duration_years > 4) %>%
  group_by(network,site, type2, mean_gr_Tmean, mean_gr_Tmax) %>% 
  summarise(slope=lm(anpp_g_m2~gr_Tmean)$coefficient[2],
            slope_Tmax=lm(anpp_g_m2~gr_Tmax)$coefficient[2])

ggplot(data=sens_temp%>% 
         filter(type2 != "Pasture")%>%
         filter(slope <800), 
       aes(x=mean_gr_Tmean, y=slope, color = type2))+
  theme_classic() +
  scale_color_manual(name='Systems', values=c('orange', 'green', 'green4'))+
  geom_point()+
  geom_smooth(method = 'loess', se=F)

ggplot(data=sens_temp%>% 
         filter(type2 != "Pasture")%>%
         filter(slope <800), aes(x=type2, y=slope, fill = type2))+
  theme_classic() +
  scale_fill_manual(name='Systems', values=c('orange', 'green', 'green4'))+
  geom_boxplot()+
  geom_point()+
  theme(axis.title.x = element_blank())

ggplot(data=sens_temp%>% 
         filter(type2 != "Pasture")%>%
         filter(slope <800), 
       aes(x=mean_gr_Tmax, y=slope_Tmax, color = type2))+
  theme_classic() +
  scale_color_manual(name='Systems', values=c('orange', 'green', 'green4'))+
  geom_point()+
  geom_smooth(method = 'loess', se=F)

f_inset_tmax <- ggplot(data=sens_temp%>% filter(type2 != "Pasture")%>%filter(slope <800), aes(x=type2, y=slope_Tmax, fill = type2))+
  theme_classic() +
  scale_fill_manual(name='Systems', values=c('orange', 'green', 'green4'))+
  geom_boxplot()+
  geom_point()+
  theme(axis.title.x = element_blank(), legend.position = "none")

#combine main and inset plots
f_ppt_combined <- ggdraw(f_ppt) +
  draw_plot(f_inset_ppt,
            x = 0.08,
            y = 0.7,
            width = 0.3,
            height = 0.3)

f_tmax_combined <- ggdraw(f_tmax) +
  draw_plot(f_inset_tmax,
                x = 0.08,
                y = 0.7,
                width = 0.3,
                height = 0.3)
