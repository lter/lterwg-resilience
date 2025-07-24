

library(tidyverse)
library(ggthemes)
library(nlme)



# Load libraries
librarian::shelf(tidyverse, googledrive)

# Identify desired file
#focal_file <- "04_anpp_aggregated-site-crop.csv"
focal_file <- "anpp_wyr_merged.csv"

# Download harmonized data file
googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/13Ymkrr-kRLDmpaj1jwwVOnOSmEYnF-dJ")) %>% 
  dplyr::filter(name == focal_file) %>% 
  googledrive::drive_download(file = .$id, overwrite = T,
                              path = file.path("data", "harmonized_data", .$name))

# Read in harmonized data
#anpp.site.crop <- read.csv(file = file.path("data", "harmonized_data", focal_file))%>%
 #                 subset(is.na(anpp_g_m2) == FALSE )
  
anpp.precip <- read.csv(file = file.path("data", "harmonized_data", focal_file))%>%
  subset(is.na(anpp_g_m2) == FALSE )%>%
  unite(stp, c("site", "treatment", "crop"), sep = "::", remove = FALSE)

lter_ltar <- subset(anpp.precip, network != "NutNet")
nutnet <- subset(anpp.precip, network == "NutNet" & treatment == "Control")
anpp.precip <- rbind(lter_ltar, nutnet)
##upload treatment table




anpp.cv.site.crop <- anpp.precip%>%
                      group_by(stp,site, treatment, crop)%>%
                      dplyr::summarize(mean.anpp = mean(anpp_g_m2, na.rm = TRUE),
                                       se.mean.anpp = sd(anpp_g_m2)/sqrt(length(year)),
                                       cv.anpp = sd(anpp_g_m2, na.rm = TRUE)/mean.anpp,
                                       mean.precip = mean(wyr_ppt),
                                       cv.ppt = sd(wyr_ppt)/sqrt(length(year)),
                                       n_years = length(year))


anpp.cv.site.crop%>%
    subset(n_years >= 10)%>%
    ggplot( aes(x = stp, y = mean.anpp))+
      geom_pointrange(aes(ymax = mean.anpp+se.mean.anpp, ymin = mean.anpp-se.mean.anpp))


anpp.cv.site.crop%>%
  subset(n_years >= 3)%>%
  ggplot( aes(x = cv.ppt, y = cv.anpp, color = crop))+
  geom_point(size = 2)+
  geom_smooth(method = "lm",se = FALSE)+
  theme_base()


mod <- lme(cv.anpp~cv.ppt, random = ~1|site, data = subset(anpp.cv.site.crop, n_years >=10))
summary(mod)



















