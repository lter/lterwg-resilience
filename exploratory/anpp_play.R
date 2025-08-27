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
  filter(keep==1)

file3<-'site_climate_mswep.csv'
googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/13Ymkrr-kRLDmpaj1jwwVOnOSmEYnF-dJ")) %>% 
  dplyr::filter(name == file3) %>% 
  googledrive::drive_download(file = .$id, overwrite = T,
                              path = file.path("data", "harmonized_data", .$name))

climatedat<- read.csv(file = file.path("data", "harmonized_data", file3)) %>% 
  rename(site=site_id)



ggplot(data=dat, aes(x=wyr_ppt, y=anpp_g_m2, color=crop2))+
  geom_point()+
  geom_smooth(method = 'lm', se=F, aes(group=site))+
  facet_grid(network~fertilized)

dat2<-dat %>% 
  left_join(climatedat) %>% 
  filter(!is.na(anpp_g_m2))

sens<-dat2 %>% 
  group_by(network,site, fertilized, crop2, MAP) %>% 
  summarise(slope=lm(anpp_g_m2~wyr_ppt)$coefficient[2], nobs=n(), manpp=mean(anpp_g_m2), sd=sd(anpp_g_m2)) %>% 
  filter(nobs>5) %>% 
  mutate(cv=sd/manpp)


ggplot(data=sens, aes(x=MAP, y=slope, color=crop2, shape=as.factor(fertilized)))+
  geom_point(size=3)+
  geom_hline(yintercept = 0)

ggplot(data=sens, aes(x=MAP, y=manpp, color=crop2, shape=as.factor(fertilized)))+
  geom_point(size=3)+
  geom_smooth(method = 'lm', se=F)

ggplot(data=sens, aes(x=MAP, y=cv, color=crop2, shape=as.factor(fertilized)))+
  geom_point(size=3)#+
 # geom_smooth(method = 'lm', se=F)
