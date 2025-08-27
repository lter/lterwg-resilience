library(tidyverse)
library(googledrive)

theme_set(theme_bw(12))

dir.create(file.path("exploratory_graphs"), showWarnings = F)
dir.create(file.path("exploratory_graphs", 'anpp_year'), showWarnings = F)
dir.create(file.path("data"), showWarnings = F)
dir.create(file.path("data", "harmonized_data"), showWarnings = F)

# Identify desired file
focal_file <- "anpp_wyr_merged.csv"

# Download harmonized data file
googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/13Ymkrr-kRLDmpaj1jwwVOnOSmEYnF-dJ")) %>% 
  dplyr::filter(name == focal_file) %>% 
  googledrive::drive_download(file = .$id, overwrite = T,
                              path = file.path("data", "harmonized_data", .$name))


# Read in harmonized data
anpp_v1 <- read.csv(file = file.path("data", "harmonized_data", focal_file))

sites<-unique(anpp_v1$site)

for (i in 1:length(sites)){
  
sub<-anpp_v1 %>% 
  filter(site==sites[i])

plot<-ggplot(data=sub, aes(x=year, y = anpp_g_m2, color=crop, shape=treatment))+
  geom_point()+
  geom_smooth(method = 'lm', se=F)+
  ggtitle(paste(sites[i]))

ggsave(filename = file.path("exploratory_graphs", 'anpp_year', paste0(sites[i],'.jpg')),device='jpeg', plot = plot, height=10, width=10, units='in')
  
}
#read in trt info
file2<-'anpp_wyr_trt_merged.csv'
googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/13Ymkrr-kRLDmpaj1jwwVOnOSmEYnF-dJ")) %>% 
  dplyr::filter(name == file2) %>% 
  googledrive::drive_download(file = .$id, overwrite = T,
                              path = file.path("data", "harmonized_data", .$name))


dat<- read.csv(file = file.path("data", "harmonized_data", file2)) %>% 
  filter(crop!='Garbanzo'&crop!='Canola'&crop!='Oats') %>% 
  mutate(crop2=ifelse(crop %in% c('Orchardgrass/white clover', 'Orchard/fescue/clover/alfalfa/chicory'), 'Mixed_grass', crop)) %>% 
 mutate(fertilized=ifelse(is.na(fertilized), 0, fertilized)) %>% 
  mutate(keep=ifelse(network=='NutNet'&treatment=='NPK'|network=='NutNet'&treatment=='Control', 1, ifelse(network %in% c('LTER', 'LTAR'),1,0))) %>% 
  filter(keep==1) %>% 
  filter(site!='look.us'&site!='bnch.us') %>% 
  mutate(type=ifelse(network=='LTER', 'Grassland', 
              ifelse(network=='NutNet'&fertilized==0, 'Grassland', 
              ifelse(network=='NutNet'&fertilized==1, 'Fert. grassland', 
              ifelse(network=='LTAR'& crop2=="", 'Grassland',  
              ifelse(network=='LTAR'& crop2 %in% c('Mixed_grass', 'Switchgrass'), 'Pasture', 
              ifelse(network=='LTAR'& crop2=='Corn', 'Corn', 
              ifelse(network=='LTAR'& crop2=='Soybean', 'Soybean',
              ifelse(network=='LTAR'& crop2 %in% c('Winter_Wheat', 'Spring_Wheat'), 'Wheat', 999)))))))))


ggplot(data=dat, aes(x=wyr_ppt, y=anpp_g_m2, color=type, group=type))+
  geom_point(alpha=0.1)+
  geom_smooth(method = 'lm', se=F, aes(group=site, color=type), alpha=0.1, linewidth=0.1)+
  geom_smooth(method= 'lm', se=T)


file3<-'site_climate_mswep.csv'
googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/13Ymkrr-kRLDmpaj1jwwVOnOSmEYnF-dJ")) %>% 
  dplyr::filter(name == file3) %>% 
  googledrive::drive_download(file = .$id, overwrite = T,
                              path = file.path("data", "harmonized_data", .$name))

climatedat<- read.csv(file = file.path("data", "harmonized_data", file3)) %>% 
  rename(site=site_id)

dat3<-dat %>% 
  left_join(climatedat)


# ggplot(data=subset(dat3, MAP<1000&MAP>400), aes(x=wyr_ppt, y=anpp_g_m2))+
#   geom_point()+
#   geom_smooth(method = 'lm', se=F, aes(group=site))+
#   geom_smooth(method= 'lm', se=T, color='red')+
#   facet_grid(crop2~fertilized)

dat2<-dat %>% 
  left_join(climatedat) %>% 
  filter(!is.na(anpp_g_m2)) %>% 
  filter(site!='look.us'&site!='bnch.us'&site!='CAP'&site!='NWT')

maprange<-dat2 %>% 
  group_by(network) %>% 
  summarise(min=min(MAP), max=max(MAP))

ggplot(data=dat2, aes(x=MAP))+
  geom_histogram()+
  facet_wrap(~network)

sens<-dat3 %>% 
  filter(!is.na(anpp_g_m2)) %>%
  group_by(network,site, type, fertilized, MAP, cv_ppt_inter) %>% 
  summarise(slope=lm(anpp_g_m2~wyr_ppt)$coefficient[2], nobs=n(), manpp=mean(anpp_g_m2), sd=sd(anpp_g_m2)) %>% 
  #filter(nobs>5) %>% 
  mutate(cv=sd/manpp) %>% 
  pivot_longer(manpp:cv, names_to = 'production', values_to = 'production_val') %>% 
  pivot_longer(MAP:cv_ppt_inter, names_to = 'precip', values_to = 'precip_val')

ggplot(data=sens, aes(x=precip_val, y=production_val, color=type))+
  geom_point()+
  geom_smooth(method = 'lm', se=F)+
  facet_grid(production~precip, scales='free')

meansens<-sens %>% 
  group_by(type) %>% 
  summarise(mcv=mean(cv), msd=mean(sd))

ggplot(data=meansens, aes(x=type, y=mcv))+
  geom_bar(stat = 'identity')

sensnostie<-dat3 %>% 
  filter(!is.na(anpp_g_m2)) %>% 
  group_by(type, fertilized) %>% 
  summarise(slope=lm(anpp_g_m2~wyr_ppt)$coefficient[2], nobs=n(), manpp=mean(anpp_g_m2), sd=sd(anpp_g_m2)) %>% 
  #filter(nobs>5) %>% 
  mutate(cv=sd/manpp)

ggplot(data=sens, aes(x=MAP, y=cv, color=type, shape=as.factor(fertilized)))+
  geom_point(size=3)+
  geom_hline(yintercept = 0)

ggplot(data=sensnostie, aes(x=type, y=cv))+
  geom_bar(stat = 'identity')

ggplot(data=sens, aes(x=MAP, y=manpp, color=crop2, shape=as.factor(fertilized)))+
  geom_point(size=3)+
  geom_smooth(method = 'lm', se=F)

ggplot(data=sens, aes(x=crop2, y=slope, color=crop2))+
  geom_violin(draw_quantiles = T)+
  facet_wrap(~fertilized)

ggplot(data=sens, aes(x=MAP, y=cv, color=crop2, shape=as.factor(fertilized)))+
  geom_point(size=3)#+
 # geom_smooth(method = 'lm', se=F)
