# This is a working file for the stability analysis
# Data is prepped in teh Stability_ANPP_DataPrep.R script
# This includes comparing mean, stability, PCA analysis, and looking at relationship between precipitation 
# and characteristics of the precipitation regime and ANPP

# Load necessary libraries
library(tidyverse)
library(googledrive)
library(broom)
library(corrplot)
library(multcompView)
library(purrr)
library(emmeans)


theme_set(theme_bw(12))


# Create the folders necessary for data download locally
dir.create(file.path("exploratory_graphs"), showWarnings = F)
dir.create(file.path("exploratory_graphs", 'anpp_year'), showWarnings = F)
dir.create(file.path("data"), showWarnings = F)
dir.create(file.path("data", "harmonized_data"), showWarnings = F)


# READ IN THE ANPP AND PRECIPITATION DATA
file2<-'stability_anpp.csv'
googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/13Ymkrr-kRLDmpaj1jwwVOnOSmEYnF-dJ")) %>% 
  dplyr::filter(name == file2) %>% 
  googledrive::drive_download(file = .$id, overwrite = T,
                              path = file.path("data", "harmonized_data", .$name))

##STEP 1: Read in the ANPP data
dat_4cat <- read.csv(file = file.path("data", "harmonized_data", file2)) 
  
##STEP 2: Read in MAP data
file3<-'site_climate_mswep.csv'
googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/13Ymkrr-kRLDmpaj1jwwVOnOSmEYnF-dJ")) %>% 
  dplyr::filter(name == file3) %>% 
  googledrive::drive_download(file = .$id, overwrite = T,
                              path = file.path("data", "harmonized_data", .$name))

climatedat<- read.csv(file = file.path("data", "harmonized_data", file3)) %>% rename(site=site_id)


#############################

dat4.1<-dat_4cat%>% #using raw data, not detrended
  filter(stab.analysis == 1)%>% #sites must have 5 or more years of data, might need to up to 15 based on Doring 2018 paper?
  group_by(network, site, type, type2, fertilized)%>%
  summarize(nobs=n(), manpp=mean(anpp_g_m2), sd=sd(anpp_g_m2), anpp_pulse = (max(anpp_g_m2)-mean(anpp_g_m2))/mean(anpp_g_m2))

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

#create stability figure
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

###############################################################################################
#
# Precip analysis
#
#limiting range of MAP to fit croplands - didn't make a huge difference but worth considering
dat4.2b<-left_join(dat4.2, climatedat, by = "site")%>%
  mutate(type2 = factor(type2), 
         log_variance = log10(sd^2), 
         log_anpp = log10(manpp))

# look at relationship between cv and map
ggplot(dat4.2b, aes(MAP, cv_ppt_inter, color = type2))+
  geom_point(alpha = 0.4, size = 2)+
  theme_bw()
  

### run models between precip and production ###

#using same transformations as anova


hist(dat4.2b$manpp)
model.1 <- lm(sqrt(manpp) ~ MAP*type2, data = dat4.2b)
  summary(model.1)
  car::Anova(model.1, type =3)#tests if slopes are different
  emtrends(model.1, ~ type2, var = "MAP")%>%# Estimated slopes within each category
    summary(infer = TRUE) #test each slope against zero
  
  #parallel-slopes model 
  model.2 <- lm(sqrt(manpp) ~ MAP + type2, data = dat4.2b)
  summary(model.2)
  car::Anova(model.2, type = 3)
  
  #centering MAP
  dat4.2b$MAP_c <- scale(dat4.2b$MAP, center = TRUE, scale = FALSE)
  model.2c <- lm(sqrt(manpp) ~ MAP_c + type2, data = dat4.2b)
  summary(model.2c)
  
  #back transform
  pred_sqrt <- predict(model.2, newdata = newdat)
  sigma2 <- summary(model.2)$sigma^2   # ≈ 44.7
  
  
  ggplot(dat4.2b, aes(MAP, sqrt(manpp), color = type2)) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", se = TRUE) +
    theme_bw()
  

ggplot(dat4.2b, aes(MAP, manpp, color = type2)) +
    geom_point()+
    geom_smooth(method="lm")

  
#sd
hist(dat4.2b$sd)

model.4 <- lm(log(sd) ~ MAP*type2, data = dat4.2b)
  summary(model.4)
  car::Anova(model.4, type =3)#tests if slopes are different
  emtrends(model.4, ~ type2, var = "MAP")%>%# Estimated slopes within each category
    summary(infer = TRUE) #test each slope against zero
  
ggplot(dat4.2b, aes(MAP, sd, color = type2)) +
    geom_point()+
    geom_smooth(method="lm")

#stability
hist(dat4.2b$stab)  

model.5 <- lm(log(stab) ~ MAP*type2, data = dat4.2b)
  summary(model.5)
  car::Anova(model.5, type =3)#tests if slopes are different
  emtrends(model.5, ~ type2, var = "MAP")%>%# Estimated slopes within each category
    summary(infer = TRUE) #test each slope against zero
  

ggplot(dat4.2b, aes(MAP, stab, color = type2)) +
  geom_point()+
  geom_smooth(method="lm")



