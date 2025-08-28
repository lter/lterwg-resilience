library(tidyverse)
library(googledrive)

theme_set(theme_bw(12))

dir.create(file.path("exploratory_graphs"), showWarnings = F)
dir.create(file.path("exploratory_graphs", 'anpp_year'), showWarnings = F)
dir.create(file.path("data"), showWarnings = F)
dir.create(file.path("data", "harmonized_data"), showWarnings = F)

# # Identify desired file
# focal_file <- "anpp_wyr_merged.csv"
# 
# # Download harmonized data file
# googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/13Ymkrr-kRLDmpaj1jwwVOnOSmEYnF-dJ")) %>% 
#   dplyr::filter(name == focal_file) %>% 
#   googledrive::drive_download(file = .$id, overwrite = T,
#                               path = file.path("data", "harmonized_data", .$name))
# 
# 
# # Read in harmonized data
# anpp_v1 <- read.csv(file = file.path("data", "harmonized_data", focal_file))
# 
# sites<-unique(anpp_v1$site)
# 
# for (i in 1:length(sites)){
#   
# sub<-anpp_v1 %>% 
#   filter(site==sites[i])
# 
# plot<-ggplot(data=sub, aes(x=year, y = anpp_g_m2, color=crop, shape=treatment))+
#   geom_point()+
#   geom_smooth(method = 'lm', se=F)+
#   ggtitle(paste(sites[i]))
# 
# ggsave(filename = file.path("exploratory_graphs", 'anpp_year', paste0(sites[i],'.jpg')),device='jpeg', plot = plot, height=10, width=10, units='in')
#   
# }

#read in annp, precip, and trt info
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
              ifelse(network=='LTAR'& crop2 %in% c('Mixed_grass', 'Switchgrass', 'Alfalfa'), 'Pasture', 
              ifelse(network=='LTAR'& crop2=='Corn', 'Corn', 
              ifelse(network=='LTAR'& crop2=='Soybean', 'Soybean',
              ifelse(network=='LTAR'& crop2 %in% c('Winter_Wheat', 'Spring_Wheat'), 'Wheat', 999)))))))))

#remaking Olivia's Figure
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

file4<-'site_summary_info.csv'
googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/13Ymkrr-kRLDmpaj1jwwVOnOSmEYnF-dJ")) %>% 
  dplyr::filter(name == file4) %>% 
  googledrive::drive_download(file = .$id, overwrite = T,
                              path = file.path("data", "harmonized_data", .$name))

tempdat<- read.csv(file = file.path("data", "harmonized_data", file4)) %>% 
  rename(site=site_id) %>% 
  select(site, network, mat_degc)

dat3<-dat %>% 
  left_join(climatedat) %>% 
  left_join(tempdat)


# ggplot(data=subset(dat3, MAP<1000&MAP>400), aes(x=wyr_ppt, y=anpp_g_m2))+
#   geom_point()+
#   geom_smooth(method = 'lm', se=F, aes(group=site))+
#   geom_smooth(method= 'lm', se=T, color='red')+
#   facet_grid(crop2~fertilized)
# 
# dat2<-dat %>% 
#   left_join(climatedat) %>% 
#   filter(!is.na(anpp_g_m2)) %>% 
#   filter(site!='look.us'&site!='bnch.us'&site!='CAP'&site!='NWT')
# 
# maprange<-dat2 %>% 
#   group_by(network) %>% 
#   summarise(min=min(MAP), max=max(MAP))
# 
# ggplot(data=dat2, aes(x=MAP))+
#   geom_histogram()+
#   facet_wrap(~network)

sens<-dat3 %>% 
  filter(!is.na(anpp_g_m2)) %>%
  group_by(network,site, type, fertilized, MAP, cv_ppt_inter) %>% 
  summarise(slope=lm(anpp_g_m2~wyr_ppt)$coefficient[2], nobs=n(), manpp=mean(anpp_g_m2), sd=sd(anpp_g_m2)) %>% 
  #filter(nobs>5) %>% 
  mutate(cv=sd/manpp, stability=1/cv)# %>% 
 # pivot_longer(manpp:cv, names_to = 'production', values_to = 'production_val') %>% 
 # pivot_longer(MAP:cv_ppt_inter, names_to = 'precip', values_to = 'precip_val')

ggplot(data=sens, aes(x=precip_val, y=production_val, color=type))+
  geom_point()+
  geom_smooth(method = 'lm', se=F)+
  facet_grid(production~precip, scales='free')

meansens<-sens %>% 
  group_by(type) %>% 
  summarise(manpp2=mean(manpp), sdanpp=sd(manpp), mstab=mean(stability), sdstab=sd(stability), n=length(manpp)) %>% 
  mutate(seanpp=sdanpp/sqrt(n), sestab=sdstab/sqrt(n))

ggplot(data=meansens, aes(x=type, y=manpp2))+
  geom_bar(stat = 'identity')+
  geom_errorbar(aes(ymin=manpp2-seanpp, ymax=manpp2+seanpp), width=0.1)

ggplot(data=meansens, aes(x=type, y=mstab))+
  geom_bar(stat = 'identity')+
  geom_errorbar(aes(ymin=mstab-sestab, ymax=mstab+sestab), width=0.1)

sensnostie<-dat3 %>% 
  filter(!is.na(anpp_g_m2)) %>% 
  group_by(type, fertilized) %>% 
  summarise(slope=lm(anpp_g_m2~wyr_ppt)$coefficient[2], nobs=n(), manpp=mean(anpp_g_m2), sd=sd(anpp_g_m2)) %>% 
  #filter(nobs>5) %>% 
  mutate(cv=sd/manpp)

ggplot(data=sens, aes(x=MAP, y=cv, color=type, shape=as.factor(fertilized)))+
  geom_point(size=3)+
  geom_hline(yintercept = 0)

ggplot(data=sensnostie, aes(x=type, y=sd))+
  geom_bar(stat = 'identity')

###truncating to same MAP range as corn
sensnostie_cornlimits<-dat3 %>% 
  filter(MAP>750&MAP<1200) %>% 
  filter(!is.na(anpp_g_m2)) %>% 
  group_by(type, fertilized) %>% 
  summarise(slope=lm(anpp_g_m2~wyr_ppt)$coefficient[2], nobs=n(), manpp=mean(anpp_g_m2), sd=sd(anpp_g_m2)) %>% 
  #filter(nobs>5) %>% 
  mutate(cv=sd/manpp)

ggplot(data=sens, aes(x=MAP, y=cv, color=type, shape=as.factor(fertilized)))+
  geom_point(size=3)+
  geom_hline(yintercept = 0)

ggplot(data=sensnostie_cornlimits, aes(x=type, y=sd))+
  geom_bar(stat = 'identity')

#####looking at variation within a site
sitesens<-dat3 %>% 
  filter(!is.na(anpp_g_m2)) %>%
  group_by(network,site, type, fertilized, MAP, cv_ppt_inter) %>% 
  summarise(slope=lm(anpp_g_m2~wyr_ppt)$coefficient[2], nobs=n(), manpp=mean(anpp_g_m2), sd=sd(anpp_g_m2)) %>% 
  filter(nobs>5) %>% 
  mutate(cv=sd/manpp)

ggplot(data=sitesens, aes(x=MAP, y=manpp, color=type, shape=as.factor(fertilized)))+
  geom_point(size=3)+
  geom_hline(yintercept = 0)

###how many sites have multiple treatments or crop types?
repdata<-sitesens %>% 
  group_by(site, MAP) %>% 
  summarise(n=length(cv))

#try to make a MAP/MAT figure - no can do.
climdat<-dat3 %>% 
  select(network, site, MAP, mat_degc) %>% 
  unique()

ggplot(data=climdat, aes(x=MAP, y=mat_degc, colour = network, label=site))+
  geom_point()+
  geom_text()

#how many crops per year?
yrs<-dat3 %>% 
  group_by(network,site, type, fertilized, year) %>% 
  summarize(n=length(anpp_g_m2))

ggplot(data=sens, aes(x=crop2, y=manpp, color=crop2))+
  geom_violin(draw_quantiles = T)+
  facet_wrap(~fertilized)

ggplot(data=sens, aes(x=MAP, y=cv, color=crop2, shape=as.factor(fertilized)))+
  geom_point(size=3)#+
 # geom_smooth(method = 'lm', se=F)

###okay, we realized that we need to get a site column that is harmonized acorss dataset. We will write a datset with site and then add a site2 and region info
sitelist<-dat3 %>% 
  select(network, site) %>% 
  unique()

write.csv(sitelist, file.path("data", 'harmonized_data', paste0('sitelist.csv')), row.names=F)

harmsites<-read.csv(file = file.path("data", "harmonized_data",paste0('sitelist_site2.csv')))

dat4<-dat3 %>% 
  left_join(harmsites) %>% 
  relocate(site2, .after=site)

MAPMAT<-dat4 %>% 
  group_by(site2) %>% 
  summarize(MAP2=mean(MAP), cv_ppt_inter2=mean(cv_ppt_inter))

dat5<-dat4 %>% 
  left_join(MAPMAT)

#redoing sensitivity analyses with harmonized data
sens2<-dat5 %>% 
  filter(!is.na(anpp_g_m2)) %>%
  group_by(network,site2, type, treatment, fertilized, MAP2, cv_ppt_inter2) %>% 
  summarise(slope=lm(anpp_g_m2~wyr_ppt)$coefficient[2], nobs=n(), manpp=mean(anpp_g_m2), sd=sd(anpp_g_m2)) %>% 
  #filter(nobs>5) %>% 
  mutate(cv=sd/manpp, stability=1/cv)

#how many obs per site? All but 5 sites have two types of data obs.
repdata<-sens2 %>% 
  group_by(site2) %>% 
  summarise(n=length(cv))


ggplot(data=sens2, aes(x=MAP2, y=manpp, color=type))+
  geom_point()+
  geom_smooth(method = 'lm', se=F)
#mean production
ggplot(data=sens2, aes(x=MAP2, y=manpp, color=type, group=site2))+
  geom_line(color='black')+
  geom_point(size=3)+
  geom_smooth(method = 'lm', se=F)

#stability of production
ggplot(data=sens2, aes(x=MAP2, y=stability, color=type, group=site2))+
  geom_line(color='black')+
  geom_point(size=3)+
  geom_smooth(method = 'lm', se=F)
  
##for each site what is the variability in ANPP

sites2<-unique(sens2$site2)

deltaprod<-data.frame()

for (i in 1:length(sites2)){

sub<-sens2 %>%
  filter(site2==sites2[i]) %>% 
  ungroup() %>% 
  select(site2, type, treatment, manpp)

comparison_df <- sub %>%
  mutate(row_id = row_number()) %>%
  full_join(sub %>% mutate(row_id2 = row_number()), by = character()) %>%
  filter(row_id != row_id2) %>%
  mutate(
    manpp_diff = abs(manpp.x - manpp.y),
    comparison_type = paste(type.x, "vs", type.y))%>%
  select(site2.x, comparison_type, manpp_diff) %>% 
  distinct(.keep_all=T, manpp_diff) %>% 
  rename(site2=site2.x)

deltaprod<-deltaprod %>% 
  bind_rows(comparison_df)
}

mean_deltaprod<-deltaprod %>% 
  mutate(comparison_type2=ifelse(comparison_type=='Fert. grassland vs Grassland', 'Grassland vs Fert. grassland', comparison_type)) %>% 
  group_by(comparison_type2) %>% 
  summarise(means=mean(manpp_diff), sd=sd(manpp_diff), n=n()) %>% 
  mutate(se=sd/sqrt(n)) %>% 
  filter(n>1) %>% 
  mutate(compare=ifelse(comparison_type2 %in% c('Wheat vs Wheat', 'Corn vs Wheat', 'Corn vs Soybean'), 'Cropping', ifelse(comparison_type2 %in% c('Grassland vs Grassland', 'Grassland vs Fert. grassland'), 'Grassland', 'Pasture to Other')))

ggplot(data=mean_deltaprod, aes(x=comparison_type2, y=means, fill=compare))+
  geom_bar(stat = 'identity')+
  geom_errorbar(aes(ymin=means-se, ymax=means+se), width=0.1)+
  coord_flip()+
  scale_fill_manual(name='Management Type', values=c('orange', 'green2', 'skyblue'))+
  ylab('Difference in Mean ANPP')+
  xlab('Management Comparison')+
  theme(legend.position = 'top')

deltaprodMAP<-deltaprod %>% 
  left_join(MAPMAT)

ggplot(data=deltaprodMAP, aes(x=MAP2, y=manpp_diff, color=comparison_type))+
  geom_point()

# ##for each site what is the variability in CV
# 
# sites2<-unique(sens2$site2)
# 
# deltacv<-data.frame()
# 
# for (i in 1:length(sites2)){
#   
#   sub<-sens2 %>%
#     filter(site2==sites2[i]) %>% 
#     ungroup() %>% 
#     select(site2, type, treatment, cv)
#   
#   comparison_df_cv <- sub %>%
#     mutate(row_id = row_number()) %>%
#     full_join(sub %>% mutate(row_id2 = row_number()), by = character()) %>%
#     filter(row_id != row_id2) %>%
#     mutate(
#       cv_diff = abs(cv.x - cv.y),
#       comparison_type = paste(type.x, "vs", type.y))%>%
#     select(site2.x, comparison_type, cv_diff) %>% 
#     distinct(.keep_all=T, cv_diff) %>% 
#     rename(site2=site2.x)
#   
#   deltacv<-deltacv %>% 
#     bind_rows(comparison_df_cv)
# }
# 
# mean_deltacv<-deltacv %>% 
#   mutate(comparison_type2=ifelse(comparison_type=='Fert. grassland vs Grassland', 'Grassland vs Fert. grassland', comparison_type)) %>% 
#   group_by(comparison_type2) %>% 
#   summarise(means=mean(cv_diff, na.rm=T), sd=sd(cv_diff, na.rm = T), n=n()) %>% 
#   mutate(se=sd/sqrt(n)) %>% 
#   filter(n>1)
# 
# ggplot(data=mean_deltacv, aes(x=comparison_type2, y=means))+
#   geom_bar(stat = 'identity')+
#   geom_errorbar(aes(ymin=means-se, ymax=means+se), width=0.1)+
#   coord_flip()
# 
# deltaprodMAP<-deltaprod %>% 
#   left_join(MAPMAT)
# 
# ggplot(data=deltaprodMAP, aes(x=MAP2, y=manpp_diff, color=comparison_type))+
#   geom_point()

##for each site what is the variability in 1/CV
sites2<-unique(sens2$site2)

deltastab<-data.frame()

for (i in 1:length(sites2)){
  
  sub<-sens2 %>%
    filter(site2==sites2[i]) %>% 
    ungroup() %>% 
    select(site2, type, treatment, stability)
  
  comparison_df_stab <- sub %>%
    mutate(row_id = row_number()) %>%
    full_join(sub %>% mutate(row_id2 = row_number()), by = character()) %>%
    filter(row_id != row_id2) %>%
    mutate(
      stab_diff = abs(stability.x - stability.y),
      comparison_type = paste(type.x, "vs", type.y))%>%
    select(site2.x, comparison_type, stab_diff) %>% 
    distinct(.keep_all=T, stab_diff) %>% 
    rename(site2=site2.x)
  
  deltastab<-deltastab %>% 
    bind_rows(comparison_df_stab)
}

mean_deltastab<-deltastab %>% 
  mutate(comparison_type2=ifelse(comparison_type=='Fert. grassland vs Grassland', 'Grassland vs Fert. grassland', comparison_type)) %>% 
  group_by(comparison_type2) %>% 
  summarise(means=mean(stab_diff, na.rm=T), sd=sd(stab_diff, na.rm = T), n=n()) %>% 
  mutate(se=sd/sqrt(n)) %>% 
  filter(n>1)%>% 
  mutate(compare=ifelse(comparison_type2 %in% c('Wheat vs Wheat', 'Corn vs Wheat', 'Corn vs Soybean'), 'Cropping', ifelse(comparison_type2 %in% c('Grassland vs Grassland', 'Grassland vs Fert. grassland'), 'Grassland', 'Pasture to Other')))

ggplot(data=mean_deltastab, aes(x=comparison_type2, y=means, fill = compare))+
  geom_bar(stat = 'identity')+
  geom_errorbar(aes(ymin=means-se, ymax=means+se), width=0.1)+
  coord_flip()+
  scale_fill_manual(name='Management Type', values=c('orange', 'green2', 'skyblue'))+
  xlab('Managment Comparison')+
  ylab('Difference in Stability (1/CV)')+
  theme(legend.position = 'top')

deltaprodMAP<-deltaprod %>% 
  left_join(MAPMAT)

ggplot(data=deltaprodMAP, aes(x=MAP2, y=manpp_diff, color=comparison_type))+
  geom_point()
