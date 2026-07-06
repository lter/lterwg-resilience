library(tidyverse)
library(googledrive)
library(broom)
library(corrplot)
library(emmeans)

theme_set(theme_bw(12))

dir.create(file.path("exploratory_graphs"), showWarnings = F)
dir.create(file.path("exploratory_graphs", 'anpp_year'), showWarnings = F)
dir.create(file.path("data"), showWarnings = F)
dir.create(file.path("data", "harmonized_data"), showWarnings = F)

#read in annp data
file2<-'stability_anpp.csv'
googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/13Ymkrr-kRLDmpaj1jwwVOnOSmEYnF-dJ")) %>% 
  dplyr::filter(name == file2) %>% 
  googledrive::drive_download(file = .$id, overwrite = T,
                              path = file.path("data", "harmonized_data", .$name))

##STEP 1: Read in the ANPP data
dat_4cat <- read.csv(file = file.path("data", "harmonized_data", file2)) 

dat_4cat<-dat_4cat%>% #using raw data, not detrended
  filter(stab.analysis == 1) #sites must have 5 or more years of data, might need to up to 15 based on Doring 2018 paper?
  



#############################
#DLH 12/3/25
  #run linear models for Taylor's power law calc 
  dat4.1<-dat_4cat%>%
  #filter(duration_years > 4)%>% #sites must have 5 or more years of data, might need to up to 15 based on Doring 2018 paper?
  filter(!is.na(anpp_g_m2)) %>%
  group_by(network, site, type, type2, fertilized)%>%
  summarize(nobs=n(), manpp=mean(anpp_g_m2), sd=sd(anpp_g_m2))%>%
  #filter(nobs > 4)%>% #crops within a site must have 5 or more years of data
  filter(type != "Pasture") #removing pasture due to sample size


  dat4.2<-dat4.1%>%
    mutate(mi = log10(manpp))%>%
    mutate(var = sd^2)%>% 
    mutate(vi = log10(var))
 
  ###########################################################
  ##Combining all types to calc aCV#####
  ggplot(dat4.2, aes(mi, vi))+
    geom_point(aes(color=type2))+
    geom_smooth(method='lm')+
    geom_abline(slope=2)+
    xlab('Log(Mean ANPP)')+
    ylab('Log(sd^2 ANPP)')
  
  
  mod.1<-lm(vi ~ mi, data = dat4.2)
  summary(mod.1)
  coef(mod.1)
  b<-coef(mod.1)["mi"]
  a<-coef(mod.1)["(Intercept)"]

  #calculate adjusted coefficent of variaion based on Doring and Recking 2018
  dat4.2<-dat4.2%>%
    mutate(ui = vi - (a +b*mi)) #calculate residuals from regression line (Power Law Residuals, POLAR) 
  
  a2<-a + (b-2)*mean(dat4.2$mi, na.rm=T)
  
  dat4.2<-dat4.2%>%
    mutate(vi2 = a2 + 2*mi + ui)%>%
    mutate(aCV = (sqrt(10^vi2)/manpp)*100)%>%
    mutate(CV = (sd/manpp)*100)
  
  ggplot(dat4.2, aes(CV, aCV))+
    geom_point(aes(color=type2))+
    geom_smooth(method='lm')
  
  data4.3<-dat4.2%>%
    pivot_longer(cols = c(aCV, CV), names_to = "vartype", values_to = "var2")%>%
    group_by(type2, vartype)%>%
    summarise(CV = mean(var2),
              seCV = sd(var2)/sqrt(n()))
  
  data4.3$vartype<-factor(data4.3$vartype, levels = c("CV", "aCV"))
  
  ggplot(data4.3, aes(x = type2, y = CV, fill = vartype)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
    geom_errorbar(aes(ymin = CV - seCV, ymax = CV + seCV),
                  width = 0.2, position = position_dodge(width = 0.8)) +
    theme_minimal()
  
  ###########################################################
  ##Combining all types to calc aCV#####
  
  #run by types s
  ggplot(data = dat4.2, aes(mi, vi, color = type2))+
    geom_point()+
    geom_smooth(method = 'lm')+
    geom_abline(slope=2) +
    xlab('Log(Mean ANPP)')+
    ylab('Log(sd^2 ANPP)')
  
  
  #run by types soy and wheat removed
  ggplot(data = subset(dat4.2, !dat4.2$type %in% c("Soybean", "Wheat")), aes(mi, vi, color = type2))+
    geom_point()+
    geom_smooth(method = 'lm')+
    geom_abline(slope=2) +
    xlab('Log(Mean ANPP)')+
    ylab('Log(sd^2 ANPP)')
  
  # run ancova to test if slopes are different  
  # Fit model with interaction
  model_interaction <- lm(vi ~ mi * type2, data = dat4.2)
  
  #model_interaction <- lm(vi ~ mi * type2, data = subset(dat4.2, !dat4.2$type %in% c("Soybean", "Wheat")))
  
  # Summary shows if slopes differ 
  summary(model_interaction)
  
  #extract slopes for each group
  slopes <- emtrends(model_interaction, ~ type2, var = "mi")
  slopes
  
  #pairwise comparison of slopes
  pairs(slopes, adjust = "tukey")
  
  
  
  
  # ANOVA to test significance of interaction
  #anova(model_interaction) # main effects of mi and type2 significant (p < 0.001), marginally significant interaction of mi:type2 (p=0.08)
                           # might suggest that each type2 is calc separately 
  
  #run POLAR aCV calculations by Type2
  types<-as.data.frame(unique(dat4.2$type2))
  colnames(types)[1]<-"type2"
  l=length(types$type2)
  acv_bytype.1<-data.frame()
  
  for (i in 1:l){
    #i=1
    t1<-types[i,]
    d1<-filter(dat4.2, type2 == t1)
    mod.2<-lm(vi ~ mi, data = d1)
    summary(mod.2)
    coef(mod.2)
    bx<-coef(mod.2)["mi"]
    ax<-coef(mod.2)["(Intercept)"]
    
    #calculate adjusted coefficent of variaion based on Doring and Recking 2018
    d1<-d1%>%
      mutate(ui = vi - (ax +bx*mi)) #calculate residuals from regression line (Power Law Residuals, POLAR) 
    
    a2x<-ax + (bx-2)*mean(d1$mi, na.rm=T)
    
    d1<-d1%>%
      mutate(vi2 = a2x + 2*mi + ui)%>%
      mutate(aCV = (sqrt(10^vi2)/manpp)*100)%>%
      mutate(CV = (sd/manpp)*100)
    
    acv_bytype.1<-rbind(acv_bytype.1, d1)
    
  }
  
  ggplot(acv_bytype.1, aes(CV, aCV, color = type2))+
    geom_point()+
    geom_smooth(method='lm')
  
  
  acv_bytype.2<-acv_bytype.1%>%
    pivot_longer(cols = c(aCV, CV), names_to = "vartype", values_to = "var2")%>%
    group_by(type2, vartype)%>%
    summarise(CV = mean(var2),
              seCV = sd(var2)/sqrt(n()))
  
  acv_bytype.2$vartype<-factor(acv_bytype.2$vartype, levels = c("CV", "aCV"))
  
  ggplot(acv_bytype.2, aes(x = type2, y = CV, fill = vartype)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
    geom_errorbar(aes(ymin = CV - seCV, ymax = CV + seCV),
                  width = 0.2, position = position_dodge(width = 0.8)) +
    theme_minimal()
  
  
  ############################################################
  #combine var metrics with precip metrics - all types combined
  data4.4<-left_join(dat4.2, climatedat, by = "site")
  
  #compare anpp cv with precip cv by type
  data4.5<-data4.4%>%
    filter(type != "Pasture")
  
  ggplot(data4.5, aes(x=cv_ppt_inter, y = aCV))+
    geom_point(alpha = 0.1, aes(color=type2)) +
    geom_smooth(aes(color = type2), method = 'lm', formula = 'y ~ x', se = T)+
    scale_color_manual(name='Land Management', values=c('orange', 'green', 'green4', 'skyblue1', 'darkgoldenrod', 'chocolate2' ))+
    xlab('Interannual Precipitation CV')+
    ylab('ANPP aCV')
  
  ggplot(data4.5, aes(x=cv_ppt_inter, y = CV))+
    geom_point(alpha = 0.1, aes(color=type2)) +
    geom_smooth(aes(color = type2), method = 'lm', formula = 'y ~ x', se = T)+
    scale_color_manual(name='Land Management', values=c('orange', 'green', 'green4', 'skyblue1', 'darkgoldenrod', 'chocolate2' ))+
    xlab('Interannual Precipitation CV')+
    ylab('ANPP CV')
  
  #correlation across all vars
  data4.6<-data4.5%>%
    filter(type == "Wheat") #can select individual types for correlations
  data4.6 <- data4.6[ ,c(6:27)]
  cors2 <- cor(data4.6)
  corrplot.mixed(cors2)
  
  #examining some interesting relationships
  var1="MAP"
  var2=manpp
  
  ggplot(data4.5, aes(x=log10(MAP), y = mi))+
    geom_point(alpha = 0.1, aes(color=type)) +
    geom_smooth(aes(color = type), method = 'lm', formula = 'y ~ x', se = F)+
    scale_color_manual(name='Land Management', values=c('orange', 'green', 'green4', 'skyblue1', 'darkgoldenrod', 'chocolate2' ))+
    xlab("MAP")+
    ylab("log(manpp)")

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
    
#############################
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
