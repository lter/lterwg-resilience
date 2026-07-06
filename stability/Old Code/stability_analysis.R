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



# looking at the relationship among teh different climate variables
# vif(climatedat.2)
# library(usdm)
# 
# 
# clim.select <- vifstep(climatedat.2, th= 10)
final_vars <- clim.select@results
final_vars# 
# selected_vars <- vifcor(climatedat.2, th = 0.7)  # correlation threshold alternative
# selected_vars@results


# look at relationship between cv and map

ggplot(dat4.2b, aes(MAP, cv_ppt_inter, color = type2))+
  geom_point(alpha = 0.4, size = 2)+
  theme_bw()
  


## varpart testing
library(vegan)

str(dat4.2b)

ggplot(dat4.2b, aes(cv_ppt_inter))+
  geom_histogram()

varpart.df <- dat4.2b %>%
  mutate(type2 = factor(type2), 
         variance = log10(sd^2), 
         log_anpp = log10(manpp), 
         scaled_MAP = scale(MAP), 
         scaled_cv = scale(cv_ppt_inter)
         )

clim <- c("cv_ppt_inter")

clim <- varpart.df[,clim]
type <- varpart.df["type2"]
anpp <- varpart.df["log_anpp"]
y <- varpart.df$variance

results <- varpart(y, clim, type, anpp)
plot(results)


# try a linear model
model <- lm(data = subset(varpart.df, varpart.df$type2 == "Cropland"), variance ~daily_ppt_d)
summary(model)
car::Anova(model, type =3)


model <- lm(data = varpart.df, log_anpp ~MAP + type2)
summary(model)
car::Anova(model, type =3)

ggplot(varpart.df, aes(MAP,variance, color = type2)) +
  geom_point()+
  geom_smooth(method="lm")


model <- lm(data = subset(varpart.df, varpart.df$type2 == "Fert. Grassland"), log_anpp ~MAP)
summary(model)
car::Anova(model, type =3)


varpart.df3 <- varpart.df %>%
  mutate(fertilized = as.factor(ifelse(type2 == "Cropland", 1, fertilized))) %>%
  filter(MAP > 750)

model2 <- lm(data = varpart.df3, log_anpp ~ MAP + type2)
summary(model2)
car::Anova(model2, type = 3)
