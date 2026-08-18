
## --- example usage ----------------------------------------------------------
source("extremes/extremes_data_prep.R")
source('thresholds/threshold_model_functions.R')
#
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
