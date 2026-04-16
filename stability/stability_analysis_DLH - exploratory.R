library(tidyverse)
library(googledrive)
library(broom)
library(corrplot)
library(multcompView)
library(purrr)

theme_set(theme_bw(12))

dir.create(file.path("exploratory_graphs"), showWarnings = F)
dir.create(file.path("exploratory_graphs", 'anpp_year'), showWarnings = F)
dir.create(file.path("data"), showWarnings = F)
dir.create(file.path("data", "harmonized_data"), showWarnings = F)



#read in annp, precip, and trt info
file2<-'anpp_wyr_trt_merged.csv'
googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/13Ymkrr-kRLDmpaj1jwwVOnOSmEYnF-dJ")) %>% 
  dplyr::filter(name == file2) %>% 
  googledrive::drive_download(file = .$id, overwrite = T,
                              path = file.path("data", "harmonized_data", .$name))

##read in the data and do some pre-processing. and classifying land management include crop type.
dat<- read.csv(file = file.path("data", "harmonized_data", file2)) %>%
  filter(site!='look.us'&site!='bnch.us') %>% #drop two odd NutNet sites
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


####Okay, we are going to combine to just four land management
dat_4cat<-dat %>% 
  mutate(type2=ifelse(type %in% c('Grassland', 'Fert. Grassland', 'Pasture'), type, 'Cropland'))

####Decided not to detrend so commented out this section ###################################################################
#detrending ANPP data - from Makki's 'data_prep_Timing_Critical.R, and based on  this paper https://doi.org/10.1016/j.agrformet.2018.09.019
#detrend_resid_plus_mean <- function(df, y_col, t_col) {
#  y <- df[[y_col]]
#  t <- df[[t_col]]
#  ok <- is.finite(y) & is.finite(t)
  
#  if (sum(ok) < 3) {
#    df[[paste0(y_col, "_dt")]] <- NA_real_
#    return(df)
#  }
  
#  fit <- lm(y[ok] ~ t[ok])
#  yhat <- rep(NA_real_, length(y))
#  yhat[ok] <- predict(fit)
#  
#  df[[paste0(y_col, "_dt")]] <- (y - yhat) + mean(y[ok], na.rm = TRUE)
#  df
#}

#anpp_dt <- dat_4cat %>%
#  group_by(network, site, type, type2, fertilized)%>% #detrend by type x site
#  group_modify(~{
#    df <- .x
#    df <- detrend_resid_plus_mean(df, "anpp_g_m2", "w_yr")
#    df
#  }) %>%
#  ungroup()
#checking trends in ANPP through time by site and type

#anpp_models <- dat_4cat %>%
#  group_by(site, type) %>%
#  filter(!is.na(anpp_g_m2))%>%
#  summarize(n= n(), Intercept = lm(anpp_g_m2 ~ year)$coefficients[1], 
#            Coeff_x1 = lm(anpp_g_m2 ~ year)$coefficients[2],
#            R2 = summary(lm(anpp_g_m2 ~ year))$r.squared,
#           pval = summary(lm(anpp_g_m2 ~ year))$coefficients["year", 4])

#anpp_models <- dat_4cat %>%
#  group_by(site, type) %>%
#  filter(!is.na(anpp_g_m2)) %>%
#  filter(n() > 4)%>%
#  summarize(
#    n = n(),
#    Intercept = lm(anpp_g_m2 ~ year)$coefficients[1],
#    Coeff_x1 = lm(anpp_g_m2 ~ year)$coefficients[2],
#    R2 = summary(lm(anpp_g_m2 ~ year))$r.squared,
#    pval = summary(lm(anpp_g_m2 ~ year))$coefficients["year", "Pr(>|t|)"]
#  )

#graph site x crop with p <0.06
#anpp_models.2<-anpp_models%>%
#  filter(pval < 0.05)%>%
#  select(site, type) %>%
#  mutate(site_type = paste(site, type, by = "_"))

#sig.sites <- dat_4cat %>%
#  mutate(site_type = paste(site, type, by = "_"))%>%
#  filter(site_type %in% anpp_models.2$site_type) %>%
#  filter(type != "Pasture")

#ggplot(data =sig.sites, aes(year, anpp_g_m2, color = type))+
#  geom_point()+
#  geom_smooth(method="lm")+
#  facet_wrap(~site, scales = "free")
#ggplot(data = sig.sites, aes(wyr_ppt, anpp_g_m2, color = type))+
#  geom_point()+
#  geom_smooth(method="lm")+
# facet_wrap(~site,  scales = "free")
#ggplot(data =sig.sites, aes(year, wyr_ppt,  color = type))+
#  geom_point()+
#  geom_smooth(method="lm")+
#  facet_wrap(~site, scales = "free")

#ggplot(data= subset(dat_4cat, dat_4cat$type == "Corn"), aes(year, anpp_g_m2))+
#  geom_point()+
#  geom_smooth(method="lm")

####Decided not to detrend so commented out this above section ###################################################################

##STEP 3: Read in MAP data
file3<-'site_climate_mswep.csv'
googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/13Ymkrr-kRLDmpaj1jwwVOnOSmEYnF-dJ")) %>% 
  dplyr::filter(name == file3) %>% 
  googledrive::drive_download(file = .$id, overwrite = T,
                              path = file.path("data", "harmonized_data", .$name))

climatedat<- read.csv(file = file.path("data", "harmonized_data", file3)) %>% rename(site=site_id)



#############################

  dat4.1<-dat_4cat%>% #using raw data, not detrended
    filter(duration_years > 4)%>% #sites must have 5 or more years of data, might need to up to 15 based on Doring 2018 paper?
    group_by(network, site, type, type2, fertilized)%>%
    summarize(nobs=n(), manpp=mean(anpp_g_m2), sd=sd(anpp_g_m2), anpp_pulse = (max(anpp_g_m2)-mean(anpp_g_m2))/mean(anpp_g_m2))%>%
    filter(nobs > 4)%>% #crops within a site must have 5 or more years of data
    filter(type != "Pasture") #removing pasture due to sample size
  
  duration<-dat_4cat%>%
    filter(duration_years > 4)%>% 
    filter(type != "Pasture")%>%
    select(network, site, type, type2, fertilized, duration_years)%>%
    unique()
  
  dat4.1<-left_join(dat4.1, duration, by = c('network', 'site', 'type', 'type2', 'fertilized') )

  #calc some stability metrics - taking mean across site (not Ingrids approach, which calcs across all sites within a type)
  dat4.2<-dat4.1%>%
    mutate(cv = sd/manpp)%>%
    mutate(stab = 1/cv)
  
  means.1<-dat4.2%>%
    select(-nobs)%>%
    pivot_longer(cols = c(manpp:stab), names_to = "var", values_to = "val")%>%
    group_by(type2, var)%>%
    summarise(nobs=n(), 
              mean=mean(val), 
              sd=sd(val),
              se = sd/sqrt(nobs))%>%
    filter(var %in% c("manpp", "sd", "stab", "anpp_pulse" ))
  
  
  #################
  # Anova's on raw data (not detrended)
  
  test_normality <- function(x) {
    list(
      raw = shapiro.test(x),
      log = shapiro.test(log(x)),
      sqrt = shapiro.test(sqrt(x)),
      cube = shapiro.test(x)^(1/3)
    )
  }
  
  #anpp###########
  transforms <- list(
    raw = dat4.2$manpp,
    log = log(dat4.2$manpp),
    sqrt = sqrt(dat4.2$manpp),
    cube = (dat4.2$manpp)^(1/3)
  )
  
  lapply(transforms, function(x) {
    m <- aov(x ~ type2, data = dat4.2)
    shapiro.test(residuals(m))
  })
  
  #Normality tests - $sqrt and $cube had w >0.95 and pvalue >0.05
    #using visual assessment to determine which model is best
  
  #sqrt <-- the plots were pretty close but liked the q-q residuals better on this so using this model
  mod.manpp.sqrt<-aov(sqrt(manpp) ~ type2, data = dat4.2)
    summary(mod.manpp.sqrt)
    
  par(mfrow=c(2,2))
  plot(mod.manpp.sqrt)
  
  tuk.manpp.1<-TukeyHSD(mod.manpp.sqrt)$type2
  tuk.manpp.1
  
  tuk.manpp.2<-multcompLetters(tuk.manpp.1[ , "p adj"])$Letters
  tuk.manpp.2
  
  tuk.manpp.3<-data.frame(type2 = names(tuk.manpp.2),
                          letters =  tuk.manpp.2,
                          var = "manpp")
  
  #cube
  #Aovfit.manpp.cube<-aov((manpp)^(1/3) ~ type2, data = dat4.2)
  #  summary(Aovfit.manpp.cube)
  
  #par(mfrow=c(2,2))
  #plot(Aovfit.manpp.cube)
  
  
  #sd###########
  transforms <- list(
    raw = dat4.2$sd,
    log = log(dat4.2$sd),
    sqrt = sqrt(dat4.2$sd),
    cube = (dat4.2$sd)^(1/3)
  )
  
  lapply(transforms, function(x) {
    m <- aov(x ~ type2, data = dat4.2)
    shapiro.test(residuals(m))
  })
  
  #Normality tests - $log and $cube had w >0.95 and pvalue >0.05
  #using visual assessment to determine which model is best
  
  #log <-- the plots were pretty close but liked the q-q residuals better on this so using this model
  mod.sd.log<-aov(log(sd) ~ type2, data = dat4.2)
  summary(mod.sd.log)
  
  par(mfrow=c(2,2))
  plot(mod.sd.log)
  
  tuk.sd.1<-TukeyHSD(mod.sd.log)$type2
  tuk.sd.1
  
  tuk.sd.2<-multcompLetters(tuk.sd.1[ , "p adj"])$Letters
  tuk.sd.2
  
  tuk.sd.3<-data.frame(type2 = names(tuk.sd.2),
                          letters =  tuk.sd.2,
                          var = "sd")
  
  #cube
  #Aovfit.sd.cube<-aov((sd)^(1/3) ~ type2, data = dat4.2)
  #summary(Aovfit.sd.cube)
  
  #par(mfrow=c(2,2))
  #plot(Aovfit.sd.cube)
  
  
  #stab###########
  transforms <- list(
    raw = dat4.2$stab,
    log = log(dat4.2$stab),
    sqrt = sqrt(dat4.2$stab),
    cube = (dat4.2$stab)^(1/3)
  )
  
  lapply(transforms, function(x) {
    m <- aov(x ~ type2, data = dat4.2)
    shapiro.test(residuals(m))
  })
  
  #Normality tests - $log and $cube had w >0.95 and pvalue >0.05
  #using visual assessment to determine which model is best
  
  #log <-- the plots were pretty close but liked the q-q residuals better on this so using this model
  mod.stab.log<-aov(log(stab) ~ type2, data = dat4.2)
  summary(mod.stab.log)
  
  par(mfrow=c(2,2))
  plot(mod.stab.log)
  
  tuk.stab.1<-TukeyHSD(mod.stab.log)$type2
  tuk.stab.1
  
  tuk.stab.2<-multcompLetters(tuk.stab.1[ , "p adj"])$Letters
  tuk.stab.2
  
  tuk.stab.3<-data.frame(type2 = names(tuk.stab.2),
                          letters =  tuk.stab.2,
                          var = "stab")
  #cube
  #mod.stab.cube<-aov((stab)^(1/3) ~ type2, data = dat4.2)
  #summary(mod.stab.cube)
  
  #par(mfrow=c(2,2))
  #plot(mod.stab.cube)
  
  
  #anpp_pulse###########
  transforms <- list(
    raw = dat4.2$anpp_pulse,
    log = log(dat4.2$anpp_pulse),
    sqrt = sqrt(dat4.2$anpp_pulse),
    cube = (dat4.2$anpp_pulse)^(1/3)
  )
  
  lapply(transforms, function(x) {
    m <- aov(x ~ type2, data = dat4.2)
    shapiro.test(residuals(m))
  })
  
  #Normality tests - $log, $sqrt and $cube had w >0.95 and pvalue >0.05
  #using visual assessment to determine which model is best
  
  #log <-- the plots were pretty close but liked the q-q residuals better on this so using this model
  mod.anpp_pulse.log<-aov(log(anpp_pulse) ~ type2, data = dat4.2)
  summary(mod.anpp_pulse.log)
  
  par(mfrow=c(2,2))
  plot(mod.anpp_pulse.log)
  
  tuk.anpp_pulse.1<-TukeyHSD(mod.anpp_pulse.log)$type2
  tuk.anpp_pulse.1
  
  tuk.anpp_pulse.2<-multcompLetters(tuk.anpp_pulse.1[ , "p adj"])$Letters
  tuk.anpp_pulse.2
  
  tuk.anpp_pulse.3<-data.frame(type2 = names(tuk.anpp_pulse.2),
                          letters =  tuk.anpp_pulse.2,
                          var = "anpp_pulse")
  
  
  #sqrt 
  #Aovfit.anpp_pulse.sqrt<-aov(sqrt(anpp_pulse) ~ type2, data = dat4.2)
  #summary(Aovfit.anpp_pulse.sqrt)
  
  #par(mfrow=c(2,2))
  #plot(Aovfit.anpp_pulse.sqrt)
  
  #cube
  #Aovfit.anpp_pulse.cube<-aov((anpp_pulse)^(1/3) ~ type2, data = dat4.2)
  #summary(Aovfit.anpp_pulse.cube)
  
  #par(mfrow=c(2,2))
  #plot(Aovfit.anpp_pulse.cube)
  
  #combine dataframes
  tuk.all<-rbind(tuk.manpp.3, tuk.sd.3, tuk.stab.3, tuk.anpp_pulse.3)
  
  means.1<-means.1%>%
    left_join(tuk.all, by = c("type2", "var"))
  
  means.1$var<-factor(means.1$var,
                      levels = c ("manpp", "sd", "stab", "anpp_pulse"))
  
  ggplot(means.1, aes(x = type2, y = mean, fill = type2)) +
    geom_bar(stat = "identity") +
    geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.1) +
    geom_text(aes(label = letters, y = mean + se),
              vjust = -0.3, size = 5) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    facet_wrap(~var, scales = "free") +
    theme_bw()
  
  #stopped here 3/26/26 DLH
  ############################
  
  #ordination of ppt data
  # Run the PCA, making sure to center and scale all of the variables
  climatedat.2<-climatedat%>%
    filter(site %in% dat4.2$site) %>%
    select (-site) 
  
  climatedat.site<-climatedat%>%
    filter(site %in% dat4.2$site) 
  
  pca_fs <- prcomp(climatedat.2, scale. = TRUE)
  summary(pca_fs)
  #biplot(pca_fs, scale = 0)
  pca_fs$rotation
  scores <- as.data.frame(pca_fs$x)
  
  # Get loadings (scaled for plotting)
  loadings <- as.data.frame(pca_fs$rotation[, 1:4])
  loadings$varname <- rownames(loadings)
  
  # Scale them up so arrows are visible
  mult <- min(
    (max(scores$PC1) - min(scores$PC1)) / (max(loadings$PC1) - min(loadings$PC1)),
    (max(scores$PC2) - min(scores$PC2)) / (max(loadings$PC2) - min(loadings$PC2))
  )
  loadings[,1:4] <- loadings[,1:4] * mult * 0.8
  
  ggplot(scores, aes(PC1, PC2)) +
    geom_point(size = 3) +
    geom_segment(data = loadings,
                 aes(x = 0, y = 0, xend = PC1, yend = PC2),
                 arrow = arrow(length = unit(0.2, "cm")), color = "gray40") +
    geom_text(data = loadings,
              aes(x = PC1, y = PC2, label = varname),
              size = 3, color = "black") +
    coord_equal() +
    labs(title = "PCA of Environmental Variables (scaled)",
         x = paste0("PC1 (", round(summary(pca_fs)$importance[2, 1]*100, 1), "%)"),
         y = paste0("PC2 (", round(summary(pca_fs)$importance[2, 2]*100, 1), "%)"))+
    theme_bw()
  
  # Extract just PC1 and PC1, add these columns to the env dataframe
  climate.dat.pca <- climatedat.site%>%
    # add variables
    dplyr::mutate(PC1 = scores$PC1, PC2 = scores$PC2) %>%
    left_join(dat4.2, by = "site")
  
  # trying MAP
  ggplot(climate.dat.pca, aes(PC1, PC2, color = type2)) +
    geom_point(size = 3) +
    geom_segment(data = loadings,
                 aes(x = 0, y = 0, xend = PC1, yend = PC2),
                 arrow = arrow(length = unit(0.2, "cm")), color = "gray40") +
    geom_text(data = loadings,
              aes(x = PC1, y = PC2, label = varname),
              size = 3, color = "black") +
    coord_equal() +
    labs(title = "PCA of Environmental Variables (scaled)",
         x = paste0("PC1 (", round(summary(pca_fs)$importance[2, 1]*100, 1), "%)"),
         y = paste0("PC2 (", round(summary(pca_fs)$importance[2, 2]*100, 1), "%)"))+
    theme_bw() 
  
  
  
  ## Quick check on the PCA with a the ANPP metrics
  vars <- c("manpp", "sd", "stab", "anpp_pulse")
  anpp_pca <- dat4.2 %>%
    ungroup()%>%
    select(all_of(vars))
  
  
  pca_anpp <- prcomp(anpp_pca, scale. = TRUE)
  summary(pca_anpp)
  #biplot(pca_fs, scale = 0)
  pca_anpp$rotation
  scores.anpp <- as.data.frame(pca_anpp$x)
  
  # Get loadings (scaled for plotting)
  loadings.anpp <- as.data.frame(pca_anpp$rotation[, 1:4])
  loadings.anpp$varname <- rownames(loadings.anpp)
  
  # Scale them up so arrows are visible
  mult.anpp <- min(
    (max(scores.anpp$PC1) - min(scores.anpp$PC1)) / (max(loadings.anpp$PC1) - min(loadings.anpp$PC1)),
    (max(scores.anpp$PC2) - min(scores.anpp$PC2)) / (max(loadings.anpp$PC2) - min(loadings.anpp$PC2))
  )
  loadings.anpp[,1:4] <- loadings.anpp[,1:4] * mult.anpp * 0.8
  
  ggplot(scores.anpp, aes(PC1, PC2)) +
    geom_point(size = 3) +
    geom_segment(data = loadings.anpp,
                 aes(x = 0, y = 0, xend = PC1, yend = PC2),
                 arrow = arrow(length = unit(0.2, "cm")), color = "gray40") +
    geom_text(data = loadings.anpp,
              aes(x = PC1, y = PC2, label = varname),
              size = 3, color = "black") +
    coord_equal() +
    labs(title = "PCA of ANPP Variables (scaled)",
         x = paste0("PC1 (", round(summary(pca_anpp)$importance[2, 1]*100, 1), "%)"),
         y = paste0("PC2 (", round(summary(pca_anpp)$importance[2, 2]*100, 1), "%)"))+
    theme_bw()
  
  # Extract just PC1 and PC1, add these columns to the env dataframe
anpp.pca.2 <- dat4.2%>%
  ungroup()%>%
    # add variables
    dplyr::mutate(PC1 = scores.anpp$PC1, PC2 = scores.anpp$PC2) %>%
    left_join(climatedat, by = "site")
  
  # trying MAP
str(anpp.pca.2)
  ggplot(anpp.pca.2, aes(PC1, PC2, color = type)) +
    geom_point(size = 3) +
    geom_segment(data = loadings.anpp,
                 aes(x = 0, y = 0, xend = PC1, yend = PC2),
                 arrow = arrow(length = unit(0.2, "cm")), color = "gray40") +
    geom_text(data = loadings.anpp,
              aes(x = PC1, y = PC2, label = varname),
              size = 3, color = "black") +
    coord_equal() +
    labs(title = "PCA of ANPP Variables (scaled)",
         x = paste0("PC1 (", round(summary(pca_anpp)$importance[2, 1]*100, 1), "%)"),
         y = paste0("PC2 (", round(summary(pca_anpp)$importance[2, 2]*100, 1), "%)"))+
    theme_bw() 
  
  
  
  # how does data length impact stability? #croplands have a negative relationship, but nothing across all
  ggplot(data=dat4.2, aes(x=duration_years, y=stab, color = type2))+
    geom_point()+
    geom_smooth(method = 'lm')
  
  ###############################################################################################
  #
  # Stop here for those adopting this code
  #
  #limiting range of MAP to fit croplands - didn't make a huge difference but worth considering
  dat4.2b<-left_join(dat4.2, climatedat, by = "site")
  
  means.2<-dat4.2b%>%
    filter(MAP > 450 & MAP <1130)%>%
    select(site, type2, manpp, sd, anpp_pulse, cv, stab)%>%
    pivot_longer(cols = c(manpp:stab), names_to = "var", values_to = "val")%>%
    group_by(type2, var)%>%
    summarise(nobs=n(), 
              mean=mean(val), 
              sd=sd(val),
              se = sd/sqrt(nobs))
  
  ggplot(data=means.2, aes(x=type2, y=mean))+
    geom_bar(stat = 'identity')+
    geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.1)+
    facet_wrap(~var, scales = "free")
  
  #combine with precip metrics and explore relationships
  dat4.2b.crop<-dat4.2b%>%
    filter(type2 == "Fert. Grassland")%>%
    ungroup()%>%
    select(manpp, sd, anpp_pulse, cv, MAP, cv_ppt_inter)
    
  #crop correlations
  cors.crop <- cor(dat4.2b.crop)
  corrplot.mixed( cors.crop)  
  
  cor.mtest <- function(mat, ...) {
    mat <- as.matrix(mat)
    n <- ncol(mat)
    p.mat<- matrix(NA, n, n)
    diag(p.mat) <- 0
    for (i in 1:(n - 1)) {
      for (j in (i + 1):n) {
        tmp <- cor.test(mat[, i], mat[, j], ...)
        p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
      }
    }
    colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
    p.mat
  }

    # matrix of the p-value of the correlation
  p.mat <- cor.mtest(dat4.2b.crop)
  head(p.mat[, 1:5])
  
  corrplot(cors.crop, type="upper", order="hclust", 
           p.mat = p.mat, sig.level = 0.05, insig = "blank")  
  
  #graph correlations
  ggplot(dat4.2b, aes(x = cv_ppt_inter, y = cv, color = type2)) +
    geom_point() +                                           # Add scatter plot points, colored by Species
    geom_smooth(method = "lm", se = FALSE)
 
    
  ###########################################################
  #run linear models for Taylor's power law calc 
  dat4.2<-dat4.1%>%
    mutate(var = sd^2)%>%
    mutate(mi = log10(manpp))%>%
    mutate(vi = log10(var))
  
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
  ggplot(dat4.2, aes(mi, vi, color = type2))+
    geom_point()+
    geom_smooth(method = 'lm')+
    geom_abline(slope=2) +
    xlab('Log(Mean ANPP)')+
    ylab('Log(sd^2 ANPP)')
  
    
  # Fit model with interaction
  model_interaction <- lm(vi ~ mi * type2, data = dat4.2)
  
  # Summary shows if slopes differ 
  summary(model_interaction)
  
  # ANOVA to test significance of interaction
  anova(model_interaction) # main effects of mi and type2 significant (p < 0.001), marginally significant interaction of mi:type2 (p=0.08)
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
