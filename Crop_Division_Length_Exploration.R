library(tidyverse)
library(googledrive)

theme_set(theme_bw(12))

dir.create(file.path("exploratory_graphs"), showWarnings = F)
dir.create(file.path("exploratory_graphs", 'anpp_year'), showWarnings = F)
dir.create(file.path("data"), showWarnings = F)
dir.create(file.path("data", "harmonized_data"), showWarnings = F)


#STEP 2: Starting the Analyses

#read in annp, precip, and trt info
file2<-'anpp_wyr_trt_merged.csv'
googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/13Ymkrr-kRLDmpaj1jwwVOnOSmEYnF-dJ")) %>% 
  dplyr::filter(name == file2) %>% 
  googledrive::drive_download(file = .$id, overwrite = T,
                              path = file.path("data", "harmonized_data", .$name))

##read in the data and do some pre-processing. and classifying land management include crop type.
dat<- read.csv(file = file.path("data", "harmonized_data", file2)) %>%
  filter(site!='look.us'&site!='bnch.us') %>% #drop two odd NutNet sites
  #filter(crop!='Garbanzo'&crop!='Canola'&crop!='Oats') %>% 
  mutate(crop=tolower(crop)) %>% 
  mutate(crop2=case_when(
    crop %in% c('orchardgrass/white clover', 'orchard/fescue/clover/alfalfa/chicory', 'sorghum-sudangrass') ~ 'mixed_grass',
    TRUE~crop)) %>% 
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
    TRUE~'Other_Crops'
  ))


## Summarize by crop details
str(dat)
summary <- dat %>%
  mutate(test = 1) %>%
  group_by(type, site, year,network) %>%
  summarize(count = n(), mean_duration_test = mean(test)) %>%
  ungroup() %>%
  filter(site != "msla_2.us" & site !="msla_3.us")

sum2 <- summary %>%
  group_by(type, site, network) %>%
  summarize(sum = sum(mean_duration_test), min = min(year), 
            max =max(year))

ggplot(data = subset(sum2, sum2$type != "Other_Crops"), aes(sum))+
  geom_histogram( binwidth = 1)+
  geom_vline(xintercept = 4, color = "red")+
  facet_wrap(~type, scales="free")

count <- sum2.filter %>%
  ungroup()%>%
  filter(sum > 4) %>%
  group_by(type) %>%
  summarize(number = n())

# Summed by site  
dat_4cat<-dat %>% 
  mutate(type2=ifelse(type %in% c('Grassland', 'Fert. Grassland', 'Pasture'), type, 'Cropland'))

summary.2 <- dat_4cat %>%
  mutate(test = 1)%>%
  filter(site != "msla_2.us" & site !="msla_3.us") %>%
  group_by(type2, site, year, network) %>%
  summarize(count = n(), mean_duration_test = mean(test)) %>%
  ungroup() 

sum2.2 <- summary.2 %>%
  group_by(type2, site, network) %>%
  summarize(sum = sum(mean_duration_test), min = min(year), 
            max =max(year))

ggplot(data =sum2.2, aes(sum))+
  geom_histogram( binwidth = 1)+
  geom_vline(xintercept = 4, color = "red")+
  facet_wrap(~type2, scales="free")


count.2 <- sum2.2filter %>%
  ungroup()%>%
  filter(sum > 4) %>%
  group_by(type2) %>%
  summarize(number = n())

## JOin the precip data
file2<-'site_climate_mswep.csv'
googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/13Ymkrr-kRLDmpaj1jwwVOnOSmEYnF-dJ")) %>% 
  dplyr::filter(name == file2) %>% 
  googledrive::drive_download(file = .$id, overwrite = T,
                              path = file.path("data", "harmonized_data", .$name))

##read in precipitaiton data
ppt<- read.csv(file = file.path("data", "harmonized_data", file2))
ppt <- ppt %>%
  rename(site = site_id)

## Join with teh two different summaries
sum2 <- left_join(sum2, ppt, by = "site")

# 
sum2.filter <- sum2 %>%
  filter(MAP > 450)

ggplot(data = subset(sum2, sum2$type != "Other_Crops"), aes(MAP))+
  geom_histogram( binwidth = 50)+
  scale_x_continuous(breaks = seq(0, max(sum2$MAP, na.rm = TRUE), by = 200))+
  facet_wrap(~type)

sum2.2 <- left_join(sum2.2, ppt, by = "site")
# 
sum2.2filter <- sum2.2 %>%
  filter(MAP > 450)

ggplot(data = subset(sum2.2, sum2.2$type2 != "Pasture"), aes(MAP))+
  geom_histogram( binwidth = 50)+
  scale_x_continuous(breaks = seq(0, max(sum2$MAP, na.rm = TRUE), by = 200))+
  facet_wrap(~type2, nrow=3)
