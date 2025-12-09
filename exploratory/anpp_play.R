library(tidyverse)
library(googledrive)

theme_set(theme_bw(12))

dir.create(file.path("exploratory_graphs"), showWarnings = F)
dir.create(file.path("exploratory_graphs", 'anpp_year'), showWarnings = F)
dir.create(file.path("data"), showWarnings = F)
dir.create(file.path("data", "harmonized_data"), showWarnings = F)

###STEP 1: Make figures for each site and assess data, does it pass our smell test. Yes.

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

#did this work?
  dat %>%
    select(network, crop, crop2, type) %>%
    distinct() %>% 
    view()
    
#remaking Olivia's Figure
ggplot(data=subset(dat, type!=999&!is.na(anpp_g_m2)&!is.na(wyr_ppt)), aes(x=wyr_ppt, y=anpp_g_m2))+
  geom_point(alpha = 0.1, aes(color=type)) +
  geom_smooth(aes(shape = as.factor(site), color = type), 
              method = 'lm', formula = 'y ~ x', se = F,
              alpha = 0.1, linewidth = 0.2) +
  geom_smooth(aes(color = type), method = 'lm', formula = 'y ~ x', se = T)+
  scale_color_manual(name='Land Management', values=c('orange', 'green', 'green4', 'skyblue1', 'darkgoldenrod', 'chocolate2' ))+
  xlab('Precipitation (mm)')+
  ylab(expression(paste('ANPP (g ', m^-2,')')))+
  theme(panel.grid = element_blank())
  facet_wrap(~fertilized)
  

####Okay, we are going to combine to just four land management
dat_4cat<-dat %>% 
  mutate(type2=ifelse(type %in% c('Grassland', 'Fert. Grassland', 'Pasture'), type, 'Cropland'))

#remaking Olivia's Figure but with just four categories
ggplot(data=dat_4cat, aes(x=wyr_ppt, y=anpp_g_m2))+
  geom_point(alpha = 0.1, aes(color=type2)) +
  geom_smooth(aes(shape = as.factor(site), color = type2), 
              method = 'lm', formula = 'y ~ x', se = F,
              alpha = 0.1, linewidth = 0.2) +
  geom_smooth(aes(color = type2), method = 'lm', formula = 'y ~ x', se = T)+
  scale_color_manual(name='Land Management', values=c('orange', 'green', 'green4', 'skyblue'))+
  xlab('Precipitation (mm)')+
  ylab(expression(paste('ANPP (g ', m^-2,')')))+
  theme(panel.grid = element_blank())
  facet_wrap(~fertilized)
  
##STEP 3: Read in MAP data
file3<-'site_climate_mswep.csv'
googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/13Ymkrr-kRLDmpaj1jwwVOnOSmEYnF-dJ")) %>% 
  dplyr::filter(name == file3) %>% 
  googledrive::drive_download(file = .$id, overwrite = T,
                              path = file.path("data", "harmonized_data", .$name))

climatedat<- read.csv(file = file.path("data", "harmonized_data", file3)) %>% rename(site=site_id)

# THis is the temperature data and we are missing a lot of sites

# file4<-'site_summary_info.csv'
# googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/13Ymkrr-kRLDmpaj1jwwVOnOSmEYnF-dJ")) %>% 
#   dplyr::filter(name == file4) %>% 
#   googledrive::drive_download(file = .$id, overwrite = T,
#                               path = file.path("data", "harmonized_data", .$name))
# 
# tempdat<- read.csv(file = file.path("data", "harmonized_data", file4)) %>% 
#   rename(site=site_id) %>% 
#   select(site, network, mat_degc)

dat3<-dat_4cat %>% 
  left_join(climatedat) 



####SKIP THIS WHOLE SECTION 
####
#Go to Line in the 262
####





#looking into what range of data we have for different crops
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


#calculating sensitivity to explore different ways of looking at the data, calculating for each site and then averaging or Ingrid approach and calculating overall sites and not averaging first. For our analyses, since we are interested in MAP relationships later on we are going with the site level and then averaging approach.

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

ggplot(data=sens, aes(x=cv_ppt_inter, y=cv, color=type))+
  geom_point()+
  geom_smooth(method = 'lm', se=F)

ggplot(data=dat3, aes(x=avg_dryspell_length, y=anpp_g_m2, color=type))+
  geom_point()+
  geom_smooth(method = 'lm', se=F)

#########################################################
# Look at correlations between variables 
# Annual level
str(dat3)
cor.data <- dat3[ ,c(8,11, 30:41)]
# Remove NAs
cor.data <- cor.data %>%
  filter(!is.na(anpp_g_m2))
cors <- cor(cor.data)
corrplot(cors)

# Site-Level - summarized across years
site_level_vars<-dat3 %>% 
  filter(!is.na(anpp_g_m2)) %>%
  group_by(network,site, type, fertilized, MAP, cv_ppt_inter) %>% 
  summarise(slope=lm(anpp_g_m2~wyr_ppt)$coefficient[2], nobs=n(), manpp=mean(anpp_g_m2), sd=sd(anpp_g_m2),
            dry_spell = mean(avg_dryspell_length), daily_ppt = mean(daily_ppt_d)) %>% 
  #filter(nobs>5) %>% 
  mutate(cv=sd/manpp, stability=1/cv)

cor.data <- site_level_vars[ ,c(5:14)]
# Remove NAs
cor.data <- cor.data %>%
  filter(!is.na(slope))
cors <- cor(cor.data)
corrplot(cors)

# Plot
ggplot(site_level_vars, aes(daily_ppt, manpp, color=MAP, shape=type))+
  geom_point()

ggplot(site_level_vars, aes(dry_spell, manpp, color=type))+
  geom_point()


#making base bar graphs to explore data
meansens<-sens %>% 
  group_by(type) %>% 
  summarise(manpp2=mean(manpp, na.rm=TRUE), sdanpp=sd(manpp), mstab=mean(stability, na.rm=T), sdstab=sd(stability), n=length(manpp)) %>% 
  mutate(seanpp=sdanpp/sqrt(n), sestab=sdstab/sqrt(n))

ggplot(data=meansens, aes(x=type, y=manpp2))+
  geom_bar(stat = 'identity')+
  geom_errorbar(aes(ymin=manpp2-seanpp, ymax=manpp2+seanpp), width=0.1)

ggplot(data=meansens, aes(x=type, y=mstab))+
  geom_bar(stat = 'identity')+
  geom_errorbar(aes(ymin=mstab-sestab, ymax=mstab+sestab), width=0.1)

# Merge site-level averages back into the dat3 and then calculate the differences
# Can look at RUE max and diff with dry spell increases, etc
dat4 <- left_join(dat3, site_level_vars)
dat5.max <- dat4 %>%
  # Calculate diff in anpp from mean
  #filter(nobs>4) %>%
  dplyr::mutate(anpp_diff = ((anpp_g_m2 - manpp)/manpp) *100)%>%
  #new column for whether it is above or below mean ppt
  dplyr::mutate(ppt_diff = ((wyr_ppt - MAP)/MAP)*100) %>%
  # select the min precip row for each site/network/type combo
  group_by(site, network, type, type2) %>%
  slice(which.max(anpp_g_m2)) %>%
  mutate(class.type = "Max")

dat5.min <- dat4 %>%
  # Calculate diff in anpp from mean
  #filter(nobs>4) %>%
  dplyr::mutate(anpp_diff = ((anpp_g_m2 - manpp)/manpp) *100)%>%
  #new column for whether it is above or below mean ppt
  dplyr::mutate(ppt_diff = ((wyr_ppt - MAP)/MAP)*100) %>%
  # select the min precip row for each site/network/type combo
  group_by(site, network, type, type2) %>%
  slice(which.min(anpp_g_m2))%>%
  mutate(class.type = "Min") %>%
  rbind(dat5.max) %>%
  ungroup()

dat5.min2 <- dat5.min %>%
  select(c(site, network, type, type2, class.type, anpp_g_m2)) %>%
  pivot_wider(names_from = class.type, values_from = anpp_g_m2) %>%
  left_join(site_level_vars) %>%
  mutate(ai = (((Max - manpp)/manpp) - ((manpp - Min)/manpp)))  %>%
  ungroup()%>%
  group_by(type, type2) %>%
  summarize(mean = mean(ai, na.rm=T), se = sd(ai, na.rm=T)/sqrt(n()))

ggplot(dat5.min2, aes(ai, group = type)) +
  geom_histogram()+
  facet_wrap(~type)

ggplot(dat5.min2, aes(type, mean))+
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean - se, ymax = mean+se), position = position_dodge(0.2))

min <- ggplot(dat5, aes(wyr_ppt, (anpp_g_m2), color=type, shape=as.factor(fertilized)))+
  geom_point() 
min

ggplot(dat5, aes(ppt_diff, anpp_diff))+
  geom_point(aes(color=MAP), alpha = 0.5) +
  geom_smooth(method='lm', se= F)+
  geom_abline(slope=1)

ggplot(dat4, aes(log(manpp), log(sd^2)))+
  geom_point(aes(color=type))+
  geom_smooth(method='lm')+
  geom_abline(slope=2)
# sensnostie<-dat3 %>% 
#   filter(!is.na(anpp_g_m2)) %>% 
#   group_by(type, fertilized) %>% 
#   summarise(slope=lm(anpp_g_m2~wyr_ppt)$coefficient[2], nobs=n(), manpp=mean(anpp_g_m2), sd=sd(anpp_g_m2)) %>% 
#   #filter(nobs>5) %>% 
#   mutate(cv=sd/manpp)
# 
# ggplot(data=sens, aes(x=MAP, y=cv, color=type, shape=as.factor(fertilized)))+
#   geom_point(size=3)+
#   geom_hline(yintercept = 0)
# 
# ggplot(data=sensnostie, aes(x=type, y=sd))+
#   geom_bar(stat = 'identity')

# ###truncating to same MAP range as corn - kinda similar got more corn data
# sensnostie_cornlimits<-dat3 %>% 
#   filter(MAP>750&MAP<1200) %>% 
#   filter(!is.na(anpp_g_m2)) %>% 
#   group_by(type, fertilized) %>% 
#   summarise(slope=lm(anpp_g_m2~wyr_ppt)$coefficient[2], nobs=n(), manpp=mean(anpp_g_m2), sd=sd(anpp_g_m2)) %>% 
#   #filter(nobs>5) %>% 
#   mutate(cv=sd/manpp)
# 
# ggplot(data=sens, aes(x=MAP, y=cv, color=type, shape=as.factor(fertilized)))+
#   geom_point(size=3)+
#   geom_hline(yintercept = 0)
# 
# ggplot(data=sensnostie_cornlimits, aes(x=type, y=sd))+
#   geom_bar(stat = 'identity')

#####looking at variation within a site - this is the main analyses we are focusing on. 

sitesens<-dat3 %>% 
  filter(!is.na(anpp_g_m2)) %>%
  group_by(network,site, type, fertilized, MAP, cv_ppt_inter) %>% 
  summarise(slope=lm(anpp_g_m2~wyr_ppt)$coefficient[2], nobs=n(), manpp=mean(anpp_g_m2), sd=sd(anpp_g_m2)) %>% 
  filter(nobs>3) %>% 
  mutate(cv=sd/manpp)

ggplot(data=sitesens, aes(x=MAP, y=manpp, color=type, shape=as.factor(fertilized)))+
  geom_point(size=3)+
  geom_hline(yintercept = 0)

###how many sites have multiple treatments or crop types?
repdata<-sitesens %>% 
  group_by(site, MAP) %>% 
  summarise(n=length(cv))

# #try to make a MAP/MAT figure - no can do.
# climdat<-dat3 %>% 
#   select(network, site, MAP, mat_degc) %>% 
#   unique()
# 
# ggplot(data=climdat, aes(x=MAP, y=mat_degc, colour = network, label=site))+
#   geom_point()+
#   geom_text()

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

#write.csv(sitelist, file.path("data", 'harmonized_data', paste0('sitelist.csv')), row.names=F)

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
  group_by(network,site2, type, type2, treatment, fertilized, MAP2, cv_ppt_inter2) %>% 
  summarise(slope=lm(anpp_g_m2~wyr_ppt)$coefficient[2], nobs=n(), manpp=mean(anpp_g_m2), sd=sd(anpp_g_m2)) %>% 
  #filter(nobs>5) %>% 
  mutate(cv=sd/manpp, stability=1/cv)

#how many obs per site? All but 5 sites have two types of data obs.
repdata<-sens2 %>% 
  group_by(site2) %>% 
  summarise(n=length(cv)) %>% 
  filter(n>1)
# 
# ggplot(data=sens2, aes(x=MAP2, y=manpp, color=type2))+
#   geom_point()+
#   geom_smooth(method = 'lm', se=F)

#mean production
ggplot(data=sens2, aes(x=MAP2, y=manpp, color=type2, group=site2))+
  geom_line(color='black')+
  geom_point(size=3)+
  scale_color_manual(name='Land Management', values=c('orange', 'green', 'green4', 'skyblue'))+
  xlab('MAP (mm)')+
  ylab(expression(paste('ANPP (g ', m^-2,')')))+
  theme(panel.grid = element_blank())

#stability of production
ggplot(data=sens2, aes(x=MAP2, y=stability, color=type2, group=site2))+
  geom_line(color='black')+
  geom_point(size=3)+
  scale_color_manual(name='Land Management', values=c('orange', 'green', 'green4', 'skyblue'))+
  xlab('MAP (mm)')+
  ylab(expression(paste('Stability of Production (1/CV)')))+
  theme(panel.grid = element_blank())
  
##for each site what is the variability in ANPP

sites2<-unique(sens2$site2)

deltaprod<-data.frame()

for (i in 1:length(sites2)){

sub<-sens2 %>%
  filter(site2==sites2[i]) %>% 
  ungroup() %>% 
  select(site2, type, type2, treatment, manpp)

comparison_df <- sub %>%
  mutate(row_id = row_number()) %>%
  full_join(sub %>% mutate(row_id2 = row_number()), by = character()) %>%
  filter(row_id != row_id2) %>%
  mutate(
    manpp_diff = abs(manpp.x - manpp.y),
    comparison_type = paste(type.x, "vs", type.y),
    comparison_type2= paste(type2.x, 'vs', type2.y))%>%
  select(site2.x, comparison_type, comparison_type2, manpp_diff) %>% 
  distinct(.keep_all=T, manpp_diff) %>% 
  rename(site2=site2.x)

deltaprod<-deltaprod %>% 
  bind_rows(comparison_df)
}

mean_deltaprod<-deltaprod %>% 
  filter(!site2 %in% c('CAF', 'LCB')) %>% #we are dropping these b/c so little data and comparisions
  mutate(compare3=case_when(
    comparison_type2 %in% c('Grassland vs Fert. Grassland','Fert. Grassland vs Grassland') ~ 'Grassland vs Fert. Grassland',
    comparison_type2 %in% c('Cropland vs Grassland','Grassland vs Cropland') ~ 'Grassland vs Cropland',
    comparison_type2 %in% c('Cropland vs Pasture','Pasture vs Cropland') ~ 'Cropland vs Pasture',
    TRUE ~ comparison_type2
  )) %>% 
  group_by(compare3) %>% 
  summarise(means=mean(manpp_diff), sd=sd(manpp_diff), n=n()) %>% 
  mutate(se=sd/sqrt(n)) %>% 
  filter(n>4) %>% 
  mutate(compare=ifelse(compare3 %in% c('Grassland vs Grassland', 'Grassland vs Fert. Grassland'), 'Grassland', compare3))
           
ggplot(data=mean_deltaprod, aes(x=compare3, y=means, fill=compare))+
  geom_bar(stat = 'identity')+
  geom_errorbar(aes(ymin=means-se, ymax=means+se), width=0.1)+
  coord_flip()+
  scale_fill_manual(name='Management Type', values=c('#D55E00', '#AA4499','#009E73', '#0072B2'))+
  ylab(expression(paste('Difference in Mean ANPP (g ', m^-2,')')))+
  xlab('Management Comparison')+
  theme(panel.grid = element_blank())

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
    select(site2, type, type2, treatment, stability)
  
  comparison_df_stab <- sub %>%
    mutate(row_id = row_number()) %>%
    full_join(sub %>% mutate(row_id2 = row_number()), by = character()) %>%
    filter(row_id != row_id2) %>%
    mutate(
      stab_diff = abs(stability.x - stability.y),
      comparison_type = paste(type.x, "vs", type.y),
      comparison_type2= paste(type2.x, 'vs', type2.y))%>%
    select(site2.x, comparison_type,comparison_type2, stab_diff) %>% 
    distinct(.keep_all=T, stab_diff) %>% 
    rename(site2=site2.x)
  
  deltastab<-deltastab %>% 
    bind_rows(comparison_df_stab)
}

mean_deltastab<-deltastab %>% 
  filter(!site2 %in% c('CAF', 'LCB')) %>% #we are dropping these b/c so little data and comparisions
  mutate(compare3=case_when(
    comparison_type2 %in% c('Grassland vs Fert. Grassland','Fert. Grassland vs Grassland') ~ 'Grassland vs Fert. Grassland',
    comparison_type2 %in% c('Cropland vs Grassland','Grassland vs Cropland') ~ 'Grassland vs Cropland',
    comparison_type2 %in% c('Cropland vs Pasture','Pasture vs Cropland') ~ 'Cropland vs Pasture',
    TRUE ~ comparison_type2
  )) %>% 
  group_by(compare3) %>% 
  summarise(means=mean(stab_diff, na.rm=T), sd=sd(stab_diff, na.rm = T), n=n()) %>% 
  mutate(se=sd/sqrt(n)) %>% 
  filter(n>4)%>% 
  mutate(compare=ifelse(compare3 %in% c('Grassland vs Grassland', 'Grassland vs Fert. Grassland'), 'Grassland', compare3))

ggplot(data=mean_deltastab, aes(x=compare3, y=means, fill=compare))+
  geom_bar(stat = 'identity')+
  geom_errorbar(aes(ymin=means-se, ymax=means+se), width=0.1)+
  coord_flip()+
  scale_fill_manual(name='Management Type', values=c('#D55E00', '#AA4499','#009E73', '#0072B2'))+
  ylab('Difference in Stability (1/CV)')+
  xlab('Management Comparison')+
  theme(panel.grid = element_blank())


deltaprodMAP<-deltaprod %>% 
  left_join(MAPMAT)

ggplot(data=deltaprodMAP, aes(x=MAP2, y=manpp_diff, color=comparison_type))+
  geom_point()
