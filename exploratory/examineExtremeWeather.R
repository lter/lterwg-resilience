library(ggplot2)
library(purrr)

##Katherine Muller
daymet <- read.csv("data/tidy_data/daymet_daily_weather.csv")
daymet_m <- read.csv("data/tidy_data/daymet_monthly_weather.csv")
daymet_a <- read.csv("data/tidy_data/daymet_annual_weather.csv")
daymet_ma <- read.csv("data/tidy_data/daymet_meanannual_weather.csv")
# mswep <- read.csv("data/tidy_data/mswep_daily.csv")
# mswep_m <- read.csv("data/tidy_data/mswep_monthly.csv")
# mswep_a <- read.csv("data/tidy_data/mswep_annual.csv")

# Precipitation-------
## use ecdf. 
site_info <- read.csv("data/site_summary_info.csv")
testvec <- daymet_a$precip_mmyear[which(daymet_a$site_id=="CAF")]
testecdf <- ecdf(testvec)
testecdf(testvec)
get_ecq <- function(x, lower = 0.1, upper = 0.9){
  myecdf <- ecdf(x)
  xcomb <- seq(min(x), max(x), length.out = 100)
  qvec <- myecdf(xcomb)
  qupper <- min(xcomb[which(qvec >= upper)])
  qlower <- max(xcomb[which(qvec <= lower)])
  return(c(qlower, qupper))
}


testvec <- daymet_a$precip_mmyear[which(daymet_a$site_id==site_info$site_id[2])]
testecq <- get_ecq(testvec)


precip_quantiles <- daymet_a %>% group_by(site_id) %>%
                      summarize(meanprecip = mean(precip_mmyear),
                                sdprecip = sd(precip_mmyear),
                                ql = qnorm(0.1, meanprecip, sdprecip),
                                qh = qnorm(0.9, meanprecip, sdprecip))

precipdf <- left_join(daymet_a, precip_quantiles[,c("site_id","ql","qh")], by = c('site_id'))
precipdf <- precipdf %>%
                mutate(below_ql = precip_mmyear < ql,
                       above_qh = precip_mmyear > qh,
                       precip_extreme = ifelse(below_ql,"dry",
                                               ifelse(above_qh,"wet","neither")))
table(precipdf$site_id,precipdf$precip_extreme)

## initial plot--turn into a function later
colz <- c("wet"= "blue", "dry" = "red", "neither" = "white")


# order sites by project. 
site_info <- site_info[order(site_info$network, site_info$site_id),]
site_info$sort <- 1:nrow(site_info)
precipdf$site_id <- factor(precipdf$site_id, levels = site_info$site_id[site_info$sort])
ggplot() + geom_point(precipdf,mapping = aes(x = year, y = site_id, color = precip_extreme ))+
  scale_color_manual(values = colz)  +
  geom_hline(yintercept = c(21.5,7.5)) +
  theme(panel.background = element_rect(fill = "white"))

ggplot(precipdf) + geom_bar(aes(site_id, fill = precip_extreme))
table(precipdf$site_id, precipdf$precip_extreme)


source("exploratory/weatherFunctions.R")

##other weather metrics
daymet$sat_vp_kpa <- huang_satv(daymet$tmean_degC)
daymet$VPD <- VPD(es = daymet$sat_vp_kpa, ea = daymet$vp_Pa/1000)
summary(daymet$VPD)

library(SPEI)
?SPEI::thornthwaite
mwa <- left_join(daymet_ma, site_info, by = c("site_id","network"))
thornthwaite(Tave = mwa$mean_tmean_degC,lat = mwa$latitude)
thornthwaite(Tave = mwa$mean_tmean_degC[1],lat = mwa$latitude[1])
thornthwaite(Tave = 60, lat = 27.4)
