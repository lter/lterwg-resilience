
## Source data and functions 
source("extremes/extremes_data_prep.R") #loads ext_data_clean

source('thresholds/threshold_model_functions.R')

####
res_grassland <- fit_threshold_models(ext_data_clean, "Grassland", lb.quantile = 0.05, ub.quantile = 0.95)
res_fertgrassland <- fit_threshold_models(ext_data_clean, "Fert. Grassland", lb.quantile = 0.05, ub.quantile = 0.95)
res_corn      <- fit_threshold_models(ext_data_clean, "Corn", lb.quantile = 0.05, ub.quantile = 0.95)
res_wheat     <- fit_threshold_models(ext_data_clean, "Wheat", lb.quantile = 0.05, ub.quantile = 0.95)
res_soy     <- fit_threshold_models(ext_data_clean, "Soybean", lb.quantile = 0.05, ub.quantile = 0.95)
#
res_grassland$comparison
summary(res_grassland$best_model)
summary(res_fertgrassland$best_model)


#plot 

library(patchwork)

p.grass <- plot_threshold_fit(res_grassland)
p.fertgrass <- plot_threshold_fit(res_fertgrassland)
p.corn <- plot_threshold_fit(res_corn)
p.wheat <- plot_threshold_fit(res_wheat)
p.soy <- plot_threshold_fit(res_soy)
p.grass + p.fertgrass + p.corn + p.wheat + p.soy


#
res_grassland_t <- fit_threshold_models(ext_data_clean, "Grassland",
                                      x_var        = "scaled_tmax",  lb.quantile = 0.05, ub.quantile = 0.95)
res_fertgrassland_t <- fit_threshold_models(ext_data_clean, "Fert. Grassland",
                                          x_var        = "scaled_tmax", lb.quantile = 0.05, ub.quantile = 0.95)
res_corn_t      <- fit_threshold_models(ext_data_clean, "Corn" ,
                                      x_var        = "scaled_tmax",lb.quantile = 0.05, ub.quantile = 0.95)
res_wheat_t     <- fit_threshold_models(ext_data_clean, "Wheat" ,
                                      x_var        = "scaled_tmax", lb.quantile = 0.05, ub.quantile = 0.95)
res_soy_t     <- fit_threshold_models(ext_data_clean, "Soybean" ,
                                        x_var        = "scaled_tmax", lb.quantile = 0.05, ub.quantile = 0.95)
#
res_grassland$comparison
summary(res_grassland$best_model)
summary(res_fertgrassland$best_model)


#plot 

library(patchwork)

p.grass_t <- plot_threshold_fit(res_grassland_t)
p.fertgrass_t <- plot_threshold_fit(res_fertgrassland_t)
p.corn_t <- plot_threshold_fit(res_corn_t)
p.wheat_t <- plot_threshold_fit(res_wheat_t)
p.soy_t <- plot_threshold_fit(res_soy_t)
p.grass_t + p.fertgrass_t + p.corn_t + p.wheat_t + p.soy_t 



#
res_grassland_t <- fit_threshold_models(ext_data_clean, "Grassland",
                                        x_var        = "scaled_tmax",  lb.quantile = 0.05, ub.quantile = 0.95)
res_fertgrassland_t <- fit_threshold_models(ext_data_clean, "Fert. Grassland",
                                            x_var        = "scaled_tmax", lb.quantile = 0.05, ub.quantile = 0.95)
res_corn_t      <- fit_threshold_models(ext_data_clean, "Corn" ,
                                        x_var        = "scaled_tmax",lb.quantile = 0.05, ub.quantile = 0.95)
res_wheat_t     <- fit_threshold_models(ext_data_clean, "Wheat" ,
                                        x_var        = "scaled_tmax", lb.quantile = 0.05, ub.quantile = 0.95)
res_soy_t     <- fit_threshold_models(ext_data_clean, "Soybean" ,
                                      x_var        = "scaled_tmax", lb.quantile = 0.05, ub.quantile = 0.95)
#
res_grassland$comparison
summary(res_grassland$best_model)
summary(res_fertgrassland$best_model)


#plot 

library(patchwork)

p.grass_t <- plot_threshold_fit(res_grassland_t)
p.fertgrass_t <- plot_threshold_fit(res_fertgrassland_t)
p.corn_t <- plot_threshold_fit(res_corn_t)
p.wheat_t <- plot_threshold_fit(res_wheat_t)
p.soy_t <- plot_threshold_fit(res_soy_t)
p.grass_t + p.fertgrass_t + p.corn_t + p.wheat_t + p.soy_t 



# Unscaled Tmax 
res_grassland_t <- fit_threshold_models(ext_data_clean, "Grassland",
                                        x_var        = "Tmaxc",  lb.quantile = 0.05, ub.quantile = 0.95)
res_fertgrassland_t <- fit_threshold_models(ext_data_clean, "Fert. Grassland",
                                            x_var        = "Tmaxc", lb.quantile = 0.05, ub.quantile = 0.95)
res_corn_t      <- fit_threshold_models(ext_data_clean, "Corn" ,
                                        x_var        = "Tmaxc",lb.quantile = 0.05, ub.quantile = 0.95)
res_wheat_t     <- fit_threshold_models(ext_data_clean, "Wheat" ,
                                        x_var        = "Tmaxc", lb.quantile = 0.05, ub.quantile = 0.95)
res_soy_t     <- fit_threshold_models(ext_data_clean, "Soybean" ,
                                      x_var        = "Tmaxc", lb.quantile = 0.05, ub.quantile = 0.95)
#
res_grassland$comparison
summary(res_grassland$best_model)
summary(res_fertgrassland$best_model)


#plot 

library(patchwork)

p.grass_t <- plot_threshold_fit(res_grassland_t)
p.fertgrass_t <- plot_threshold_fit(res_fertgrassland_t)
p.corn_t <- plot_threshold_fit(res_corn_t)
p.wheat_t <- plot_threshold_fit(res_wheat_t)
p.soy_t <- plot_threshold_fit(res_soy_t)
p.grass_t + p.fertgrass_t + p.corn_t + p.wheat_t + p.soy_t 



### Tmaxroll3 
 
res_grassland_t3 <- fit_threshold_models(ext_data_clean, "Grassland",
                                        x_var        = "Tmaxroll3",  lb.quantile = 0.05, ub.quantile = 0.95)

res_fertgrassland_t3 <- fit_threshold_models(ext_data_clean, "Fert. Grassland",
                                            x_var        = "Tmaxroll3", lb.quantile = 0.05, ub.quantile = 0.95)

res_corn_t3      <- fit_threshold_models(ext_data_clean, "Corn" ,
                                        x_var        = "Tmaxroll3",lb.quantile = 0.05, ub.quantile = 0.95)

res_wheat_t3     <- fit_threshold_models(ext_data_clean, "Wheat" ,
                                        x_var        = "Tmaxroll3", lb.quantile = 0.05, ub.quantile = 0.95)

res_soy_t3     <- fit_threshold_models(ext_data_clean, "Soybean" ,
                                      x_var        = "Tmaxroll3", lb.quantile = 0.05, ub.quantile = 0.95)



#plot 

library(patchwork)

p.grass_t3 <- plot_threshold_fit(res_grassland_t3)
p.fertgrass_t3 <- plot_threshold_fit(res_fertgrassland_t3)
p.corn_t3 <- plot_threshold_fit(res_corn_t3)
p.wheat_t3 <- plot_threshold_fit(res_wheat_t3)
p.soy_t3 <- plot_threshold_fit(res_soy_t3)
p.grass_t3 + p.fertgrass_t3 + p.corn_t3 + p.wheat_t3 + p.soy_t3


### heat wave days 

res_grassland_hw <- fit_threshold_models(ext_data_clean, "Grassland",
                                         x_var        = "consecutive_days_heat_wave",  lb.quantile = 0.05, ub.quantile = 0.95)

res_fertgrassland_hw <- fit_threshold_models(ext_data_clean, "Fert. Grassland",
                                             x_var        = "consecutive_days_heat_wave", lb.quantile = 0.05, ub.quantile = 0.95)

res_corn_hw     <- fit_threshold_models(ext_data_clean, "Corn" ,
                                         x_var        = "consecutive_days_heat_wave",lb.quantile = 0.05, ub.quantile = 0.95)

res_wheat_hw     <- fit_threshold_models(ext_data_clean, "Wheat" ,
                                         x_var        = "consecutive_days_heat_wave", lb.quantile = 0.05, ub.quantile = 0.95)

res_soy_hw    <- fit_threshold_models(ext_data_clean, "Soybean" ,
                                       x_var        = "consecutive_days_heat_wave", lb.quantile = 0.05, ub.quantile = 0.95)



#plot 

library(patchwork)

p.grass_hw <- plot_threshold_fit(res_grassland_hw)
p.fertgrass_hw <- plot_threshold_fit(res_fertgrassland_hw)
p.corn_hw <- plot_threshold_fit(res_corn_hw)
p.wheat_hw <- plot_threshold_fit(res_wheat_hw)
p.soy_hw <- plot_threshold_fit(res_soy_hw)
p.grass_hw + p.fertgrass_hw + p.corn_hw + p.wheat_hw + p.soy_hw

##########grain yield
####

res_corn_gr      <- fit_threshold_models(ext_data_clean, "Corn",  y_var = 'scaled_grain',lb.quantile = 0.05, ub.quantile = 0.95)
res_wheat_gr    <- fit_threshold_models(ext_data_clean, "Wheat",  y_var = 'scaled_grain',lb.quantile = 0.05, ub.quantile = 0.95)
res_soy_gr     <- fit_threshold_models(ext_data_clean, "Soybean", y_var = 'scaled_grain', lb.quantile = 0.05, ub.quantile = 0.95)



#plot 

library(patchwork)


p.corn_gr <- plot_threshold_fit(res_corn_gr)
p.wheat_gr <- plot_threshold_fit(res_wheat_gr)
p.soy_gr <- plot_threshold_fit(res_soy)
p.corn_gr + p.wheat_gr + p.soy_gr


 
#scaled grain tmax 
res_corn_gr_t3      <- fit_threshold_models(ext_data_clean, "Corn",  y_var = 'scaled_grain', x_var = 'Tmaxroll3', lb.quantile = 0.05, ub.quantile = 0.95)
res_wheat_gr_t3    <- fit_threshold_models(ext_data_clean, "Wheat",  y_var = 'scaled_grain',x_var = 'Tmaxroll3', lb.quantile = 0.05, ub.quantile = 0.95)
res_soy_gr_t3     <- fit_threshold_models(ext_data_clean, "Soybean", y_var = 'scaled_grain', x_var = 'Tmaxroll3', lb.quantile = 0.05, ub.quantile = 0.95)
 
 
 
 #plot 
 
library(patchwork)
 
 
p.corn_gr_t3 <- plot_threshold_fit(res_corn_gr_t3)
p.wheat_gr_t3 <- plot_threshold_fit(res_wheat_gr_t3)
p.soy_gr_t3 <- plot_threshold_fit(res_soy_gr_t3)
p.corn_gr_t3 + p.wheat_gr_t3 + p.soy_gr_t3
 