library(grf)
library(tidyverse)
library(plyr)
library(lmerTest)
library(nlme)
library(visreg)
library(MuMIn)
library(ggthemes)
library(ggeffects)
library(MASS)
library(cowplot)
library(emmeans)
library(kernelshap)
library(shapviz)
library(ggbeeswarm)
library(hstats)
library(patchwork)
library(mgcv)
library(ranger)
library(googledrive)

set.seed(100)

# ----------------------------------
# Read and process data
# ----------------------------------
dir.create(file.path("exploratory_graphs"), showWarnings = F)
dir.create(file.path("data"), showWarnings = F)
dir.create(file.path("data", "harmonized_data"), showWarnings = F)

file2 <- 'anpp_wyr_trt_merged.csv'
googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/13Ymkrr-kRLDmpaj1jwwVOnOSmEYnF-dJ")) %>%
  dplyr::filter(name == file2) %>%
  googledrive::drive_download(file = .$id, overwrite = T,
                              path = file.path("data", "harmonized_data", .$name))

dat <- read.csv(file = file.path("data", "harmonized_data", file2)) %>%
  filter(crop != 'Garbanzo' & crop != 'Canola' & crop != 'Oats') %>%
  mutate(crop2 = ifelse(crop %in% c('Orchardgrass/white clover',
                                    'Orchard/fescue/clover/alfalfa/chicory'),
                        'Mixed_grass', crop)) %>%
  mutate(fertilized = ifelse(is.na(fertilized), 0, fertilized)) %>%
  mutate(keep = ifelse(network == 'NutNet' & treatment == 'NPK' |
                         network == 'NutNet' & treatment == 'Control', 1,
                       ifelse(network %in% c('LTER', 'LTAR', 'CSCAP', 'DRIVES','ISU Drainage'), 1, 0))) %>%
  filter(keep == 1) %>%
  filter(site != 'look.us' & site != 'bnch.us') %>%
  mutate(type = ifelse(network == 'LTER', 'Grassland',
                       ifelse(network == 'NutNet' & fertilized == 0, 'Grassland',
                              ifelse(network == 'NutNet' & fertilized == 1, 'Fert. grassland',
                                     ifelse(network == 'LTAR' & crop2 == "", 'Grassland',
                                            ifelse(network == 'LTAR' & crop2 %in% c('Mixed_grass', 'Switchgrass'), 'Pasture',
                                                   ifelse(network %in% c('LTER', 'LTAR', 'CSCAP', 'DRIVES','ISU Drainage') & crop2 == 'Corn', 'Corn',
                                                          ifelse(network %in% c('LTER', 'LTAR', 'CSCAP', 'DRIVES','ISU Drainage') & crop2 == 'Soybean', 'Soybean',
                                                                 ifelse(network %in% c('LTER', 'LTAR', 'CSCAP', 'DRIVES','ISU Drainage') & crop2 %in% c('Winter_Wheat', 'Spring_Wheat'), 'Wheat',
                                                                        999)))))))))

file3 <- 'site_climate_mswep.csv'
googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/13Ymkrr-kRLDmpaj1jwwVOnOSmEYnF-dJ")) %>%
  dplyr::filter(name == file3) %>%
  googledrive::drive_download(file = .$id, overwrite = T,
                              path = file.path("data", "harmonized_data", .$name))

climatedat <- read.csv(file = file.path("data", "harmonized_data", file3)) %>%
  dplyr::rename(site = site_id)

dat2 <- dat %>%
  left_join(climatedat) %>%
  filter(!is.na(anpp_g_m2))# %>%
#filter(site != 'look.us' & site != 'bnch.us' & site != 'CAP' & site != 'NWT')

grass.fert <- dat2 %>%
  mutate(type2 = ifelse(type == "Grassland" | type == "Fert. grassland", 0, 1)) %>%
  dplyr::select(site, treatment, type2, anpp_g_m2, fertilized, ppt_max_event,  ppt_mean_event,days_half_ppt,daily_ppt_d,n_wet_days,avg_dryspell_length,ppt_95th_percentile_size,MAP,cv_ppt_intra,cv_ppt_inter,yearly_ppt_d,seasonality_index ) %>%
  group_by(site, treatment, type2, fertilized, ppt_max_event,  ppt_mean_event,days_half_ppt,daily_ppt_d,n_wet_days,avg_dryspell_length,ppt_95th_percentile_size,MAP,cv_ppt_intra,cv_ppt_inter,yearly_ppt_d,seasonality_index ) %>%
  dplyr::summarise(anpp = mean(anpp_g_m2),
                   length = n()) %>%
  #subset(length > 2) %>%
  dplyr::select(site, treatment, type2, anpp, fertilized, ppt_max_event,  ppt_mean_event,days_half_ppt,daily_ppt_d,n_wet_days,avg_dryspell_length,ppt_95th_percentile_size,MAP,cv_ppt_intra,cv_ppt_inter,yearly_ppt_d,seasonality_index )
colnames(dat2)

# ----------------------------------
# Master variable naming key
# ----------------------------------
var_key <- c(
  MAP            = "Mean annual precipitation (mm)",
  seasonality_index = "Seasonality index",
  fertilized     = "Fertilization",
  cv_ppt_inter = "Interannual precipitation CV"
  
)

# ----------------------------------
# Define Y, W, X
# ----------------------------------
Y <- grass.fert$anpp

W <- grass.fert %>%
  ungroup() %>%
  pull(type2)

X <- grass.fert %>%
  ungroup() %>%
  dplyr::select(fertilized, ppt_max_event,  ppt_mean_event,days_half_ppt,daily_ppt_d,n_wet_days,avg_dryspell_length,ppt_95th_percentile_size,MAP,cv_ppt_intra,cv_ppt_inter,yearly_ppt_d,seasonality_index )

# ----------------------------------
# Site-level equal weighting
# ----------------------------------
site_counts <- grass.fert %>%
  add_count(site, name = "site_n") %>%
  mutate(w = 1 / site_n) %>%
  pull(w)


# ----------------------------------
# Fit causal forest
# ----------------------------------
eval.forest <- causal_forest(X, Y, W,
                             clusters       = as.factor(grass.fert$site),
                             sample.weights = site_counts,
                             #tune.parameters = "all",
                             num.trees      = 2000)

# ----------------------------------
# Average treatment effect
# ----------------------------------
average_treatment_effect(eval.forest)

# ----------------------------------
# Calibration test
# ----------------------------------
test_calibration(eval.forest)

# ----------------------------------
# Variable importance
# ----------------------------------
varimp <- variable_importance(eval.forest)
ranked.vars <- order(varimp, decreasing = TRUE)
colnames(X)[ranked.vars]
# [1] "seasonality_index" "cv_ppt_inter"      "MAP"               "fertilized" 

imp <- sort(setNames(variable_importance(eval.forest), colnames(X)))

varimp_df <- tibble(
  variable = names(imp),
  value    = as.numeric(imp)
) %>%
  mutate(moderator = recode(variable, !!!var_key)) %>%
  filter(!is.na(moderator))

p_varimp <- ggplot(varimp_df, aes(x = reorder(moderator, value), y = value)) +
  geom_col(fill = "grey40") +
  coord_flip() +
  labs(x = NULL, y = "Split-based variable importance") +
  theme_base()

p_varimp

# ----------------------------------
# RATE and TOC curve
# ----------------------------------
rate <- rank_average_treatment_effect(eval.forest,
                                      predict(eval.forest, X)$predictions)
plot(rate)
paste("AUTOC:", round(rate$estimate, 2), "+/-", round(1.96 * rate$std.err, 2))

# ----------------------------------
# Partial dependence plots with SE bands
# ----------------------------------
pdp_with_ci <- function(forest, focal_var, X, n_grid = 50) {
  
  grid_vals <- seq(
    min(X[[focal_var]], na.rm = TRUE),
    max(X[[focal_var]], na.rm = TRUE),
    length.out = n_grid
  )
  
  X_means <- as.data.frame(as.list(colMeans(X, na.rm = TRUE)))
  
  newdata <- X_means[rep(1L, n_grid), ]
  newdata[[focal_var]] <- grid_vals
  
  preds <- predict(forest, newdata = newdata, estimate.variance = TRUE)
  
  tibble(
    x        = grid_vals,
    estimate = preds$predictions,
    se       = sqrt(preds$variance.estimates),
    lower    = estimate - se,
    upper    = estimate + se,
    variable = focal_var
  )
}

pdp_data <- purrr::map_dfr(colnames(X), pdp_with_ci,
                           forest = eval.forest, X = X)

pdp_data <- pdp_data %>%
  mutate(label = recode(variable, !!!var_key))

pdp_plots <- pdp_data %>%
  group_by(variable) %>%
  group_split() %>%
  purrr::map(function(df) {
    ggplot(df, aes(x = x, y = estimate)) +
      #geom_ribbon(aes(ymin = lower, ymax = upper),
      #            fill = "grey70", alpha = 0.4) +
      geom_line(linewidth = 0.7, colour = "grey20") +
      #geom_hline(yintercept = 0, linetype = "dashed",
      #           colour = "grey50", linewidth = 0.4) +
      labs(
        x     = unique(df$label),
        y     = "Treatment effect on ANPP",
        title = NULL
      ) +
      theme(
        panel.background = element_rect(fill = "white", colour = "grey50"),
        axis.title       = element_text(size = 8),
        axis.text        = element_text(size = 7)
      )
  })

wrap_plots(pdp_plots, ncol = 4)

# ----------------------------------
# SHAP values
# ----------------------------------
pred_fun <- function(object, newdata, ...) {
  predict(object, newdata, ...)$predictions
}

ks <- kernelshap(eval.forest, X = X, pred_fun = pred_fun)
shap_values <- shapviz(ks)

sv_importance(shap_values) &
  theme(panel.background = element_rect(fill = "white", colour = "grey50"))

sv_importance(shap_values, kind = "bee") &
  theme(panel.background = element_rect(fill = "white", colour = "grey50"))

sv_dependence(shap_values, v = colnames(X), color_var = NULL, jitter_width = 0.01) +
  plot_layout(ncol = 4)

# ----------------------------------
# Interaction statistics
# ----------------------------------
H <- hstats(eval.forest, X = X, pred_fun = pred_fun, verbose = FALSE)
plot(H)



#___Rangeland vs wheat_______________________________________________

# ----------------------------------
# Filter to rangeland vs wheat only
# ----------------------------------
grass.wheat <- dat2 %>%
  filter(type %in% c("Grassland", "Fert. grassland", "Wheat")) %>%
  mutate(type2 = ifelse(type == "Grassland" | type == "Fert. grassland", 0, 1)) %>%
  dplyr::select(site, treatment, type2, anpp_g_m2, fertilized, ppt_max_event,  ppt_mean_event,days_half_ppt,daily_ppt_d,n_wet_days,avg_dryspell_length,ppt_95th_percentile_size,MAP,cv_ppt_intra,cv_ppt_inter,yearly_ppt_d,seasonality_index ) %>%
  group_by(site, treatment, type2, fertilized, ppt_max_event,  ppt_mean_event,days_half_ppt,daily_ppt_d,n_wet_days,avg_dryspell_length,ppt_95th_percentile_size,MAP,cv_ppt_intra,cv_ppt_inter,yearly_ppt_d,seasonality_index ) %>%
  dplyr::summarise(anpp = mean(anpp_g_m2),
                   length = n()) %>%
#  subset(length > 2) %>%
  dplyr::select(site, treatment, type2, anpp, fertilized, ppt_max_event,  ppt_mean_event,days_half_ppt,daily_ppt_d,n_wet_days,avg_dryspell_length,ppt_95th_percentile_size,MAP,cv_ppt_intra,cv_ppt_inter,yearly_ppt_d,seasonality_index )

length(unique(grass.wheat$site))
table(grass.wheat$type2)

# ----------------------------------
# Master variable naming key
# ----------------------------------
var_key <- c(
  MAP               = "Mean annual precipitation (mm)",
  seasonality_index = "Seasonality index",
  fertilized        = "Fertilization", 
  cv_ppt_inter = "Interannual precipitation CV"
)

# ----------------------------------
# Define Y, W, X
# ----------------------------------
Y <- grass.wheat$anpp

W <- grass.wheat %>%
  ungroup() %>%
  pull(type2)

X <- grass.wheat %>%
  ungroup() %>%
  dplyr::select(fertilized, #ppt_max_event,  ppt_mean_event,days_half_ppt,daily_ppt_d,n_wet_days,avg_dryspell_length,ppt_95th_percentile_size,
                MAP,#cv_ppt_intra,
                cv_ppt_inter,#yearly_ppt_d,
                seasonality_index )

# ----------------------------------
# Site-level equal weighting
# ----------------------------------
site_counts <- grass.wheat %>%
  add_count(site, name = "site_n") %>%
  mutate(w = 1 / site_n) %>%
  pull(w)

# ----------------------------------
# Fit nuisance models
# ----------------------------------
#Y.hat <- predict(regression_forest(X, Y, num.trees = 2000))$predictions
#W.hat <- predict(regression_forest(X, W, num.trees = 2000))$predictions

# ----------------------------------
# Fit causal forest
# ----------------------------------
eval.forest <- causal_forest(X, Y, W,
                             #                             Y.hat           = Y.hat,
                             #                             W.hat           = W.hat,
                             clusters        = as.factor(grass.wheat$site),
                             sample.weights  = site_counts,
                             # tune.parameters = "all",
                             num.trees       = 2000)

# ----------------------------------
# Average treatment effect
# ----------------------------------
average_treatment_effect(eval.forest)

# ----------------------------------
# Calibration test
# ----------------------------------
test_calibration(eval.forest)

# ----------------------------------
# RATE and TOC curve
# ----------------------------------
rate <- rank_average_treatment_effect(eval.forest,
                                      predict(eval.forest, X)$predictions)
plot(rate)
paste("AUTOC:", round(rate$estimate, 2), "+/-", round(1.96 * rate$std.err, 2))

# ----------------------------------
# Variable importance
# ----------------------------------
varimp <- variable_importance(eval.forest)
ranked.vars <- order(varimp, decreasing = TRUE)
colnames(X)[ranked.vars]

imp <- sort(setNames(variable_importance(eval.forest), colnames(X)))

varimp_df <- tibble(
  variable = names(imp),
  value    = as.numeric(imp)
) %>%
  mutate(moderator = recode(variable, !!!var_key)) %>%
  filter(!is.na(moderator))

p_varimp <- ggplot(varimp_df, aes(x = reorder(moderator, value), y = value)) +
  geom_col(fill = "grey40") +
  coord_flip() +
  labs(x = NULL, y = "Split-based variable importance") +
  theme_base()

p_varimp

# ----------------------------------
# Partial dependence plots with SE bands
# ----------------------------------
pdp_with_ci <- function(forest, focal_var, X, n_grid = 50) {
  
  grid_vals <- seq(
    min(X[[focal_var]], na.rm = TRUE),
    max(X[[focal_var]], na.rm = TRUE),
    length.out = n_grid
  )
  
  X_means <- as.data.frame(as.list(colMeans(X, na.rm = TRUE)))
  
  newdata <- X_means[rep(1L, n_grid), ]
  newdata[[focal_var]] <- grid_vals
  
  preds <- predict(forest, newdata = newdata, estimate.variance = TRUE)
  
  tibble(
    x        = grid_vals,
    estimate = preds$predictions,
    se       = sqrt(preds$variance.estimates),
    lower    = estimate - se,
    upper    = estimate + se,
    variable = focal_var
  )
}

pdp_data <- purrr::map_dfr(colnames(X), pdp_with_ci,
                           forest = eval.forest, X = X)

pdp_data <- pdp_data %>%
  mutate(label = recode(variable, !!!var_key))

pdp_plots <- pdp_data %>%
  group_by(variable) %>%
  group_split() %>%
  purrr::map(function(df) {
    ggplot(df, aes(x = x, y = estimate)) +
      #geom_ribbon(aes(ymin = lower, ymax = upper),
       #           fill = "grey70", alpha = 0.4) +
      geom_line(linewidth = 0.7, colour = "grey20") +
      #geom_hline(yintercept = 0, linetype = "dashed",
       #          colour = "grey50", linewidth = 0.4) +
      labs(
        x     = unique(df$label),
        y     = "Treatment effect on ANPP(Wheat vs. Rangeland)",
        title = NULL
      ) +
      theme(
        panel.background = element_rect(fill = "white", colour = "grey50"),
        axis.title       = element_text(size = 8),
        axis.text        = element_text(size = 7)
      )
  })

wrap_plots(pdp_plots, ncol = 4)

# ----------------------------------
# SHAP values
# ----------------------------------
pred_fun <- function(object, newdata, ...) {
  predict(object, newdata, ...)$predictions
}

ks <- kernelshap(eval.forest, X = X, pred_fun = pred_fun)
shap_values <- shapviz(ks)

sv_importance(shap_values) &
  theme(panel.background = element_rect(fill = "white", colour = "grey50"))

sv_importance(shap_values, kind = "bee") &
  theme(panel.background = element_rect(fill = "white", colour = "grey50"))

sv_dependence(shap_values, v = colnames(X), color_var = NULL, jitter_width = 0.01) +
  plot_layout(ncol = 4)

# ----------------------------------
# Interaction statistics
# ----------------------------------
H <- hstats(eval.forest, X = X, pred_fun = pred_fun, verbose = FALSE)
plot(H)



#__Rangeland vs corn______________________________

# ----------------------------------
# Filter to rangeland vs corn only
# ----------------------------------
grass.corn <- dat2 %>%
  filter(type %in% c("Grassland", "Fert. grassland", "Corn")) %>%
  mutate(type2 = ifelse(type == "Grassland" | type == "Fert. grassland", 0, 1)) %>%
  dplyr::select(site, treatment, type2, anpp_g_m2, fertilized, ppt_max_event,  ppt_mean_event,days_half_ppt,daily_ppt_d,n_wet_days,avg_dryspell_length,ppt_95th_percentile_size,MAP,cv_ppt_intra,cv_ppt_inter,yearly_ppt_d,seasonality_index ) %>%
  group_by(site, treatment, type2, fertilized, ppt_max_event,  ppt_mean_event,days_half_ppt,daily_ppt_d,n_wet_days,avg_dryspell_length,ppt_95th_percentile_size,MAP,cv_ppt_intra,cv_ppt_inter,yearly_ppt_d,seasonality_index ) %>%
  dplyr::summarise(anpp = mean(anpp_g_m2, na.rm = TRUE),
                   length = n()) %>%
  #subset(length > 2) %>%
  dplyr::select(site, treatment, type2, fertilized, anpp, ppt_max_event,  ppt_mean_event,days_half_ppt,daily_ppt_d,n_wet_days,avg_dryspell_length,ppt_95th_percentile_size,MAP,cv_ppt_intra,cv_ppt_inter,yearly_ppt_d,seasonality_index )%>%
  subset(site != "WICST_ARS")#this had some problem

length(unique(grass.corn$site))
table(grass.corn$type2)

# ----------------------------------
# Master variable naming key
# ----------------------------------
var_key <- c(
  MAP               = "Mean annual precipitation (mm)",
  seasonality_index = "Seasonality index",
  fertilized        = "Fertilization", 
  cv_ppt_inter ="Interannual precipitation CV"
)

# ----------------------------------
# Define Y, W, X
# ----------------------------------
Y <- grass.corn$anpp

W <- grass.corn %>%
  ungroup() %>%
  pull(type2)

X <- grass.corn %>%
  ungroup() %>%
  dplyr::select(fertilized, #ppt_max_event,  ppt_mean_event,days_half_ppt,daily_ppt_d,n_wet_days,avg_dryspell_length,ppt_95th_percentile_size,
                MAP,cv_ppt_intra,cv_ppt_inter,#yearly_ppt_d,
                seasonality_index )

# ----------------------------------
# Site-level equal weighting
# ----------------------------------
site_counts <- grass.corn %>%
  add_count(site, name = "site_n") %>%
  mutate(w = 1 / site_n) %>%
  pull(w)

# ----------------------------------
# Fit nuisance models
# ----------------------------------
#Y.hat <- predict(regression_forest(X, Y, num.trees = 2000))$predictions
#W.hat <- predict(regression_forest(X, W, num.trees = 2000))$predictions

# ----------------------------------
# Fit causal forest
# ----------------------------------
eval.forest <- causal_forest(X, Y, W,
                             #  Y.hat           = Y.hat,
                             #   W.hat           = W.hat,
                             clusters        = as.factor(grass.corn$site),
                             sample.weights  = site_counts,
                             num.trees       = 2000)

# ----------------------------------
# Average treatment effect
# ----------------------------------
average_treatment_effect(eval.forest)

# ----------------------------------
# Calibration test
# ----------------------------------
test_calibration(eval.forest)

# ----------------------------------
# RATE and TOC curve
# ----------------------------------
rate <- rank_average_treatment_effect(eval.forest,
                                      predict(eval.forest, X)$predictions)
plot(rate)
paste("AUTOC:", round(rate$estimate, 2), "+/-", round(1.96 * rate$std.err, 2))

# ----------------------------------
# Variable importance
# ----------------------------------
varimp <- variable_importance(eval.forest)
ranked.vars <- order(varimp, decreasing = TRUE)
colnames(X)[ranked.vars]

imp <- sort(setNames(variable_importance(eval.forest), colnames(X)))

varimp_df <- tibble(
  variable = names(imp),
  value    = as.numeric(imp)
) %>%
  mutate(moderator = recode(variable, !!!var_key)) %>%
  filter(!is.na(moderator))

p_varimp <- ggplot(varimp_df, aes(x = reorder(moderator, value), y = value)) +
  geom_col(fill = "grey40") +
  coord_flip() +
  labs(x = NULL, y = "Split-based variable importance") +
  theme_base()

p_varimp

# ----------------------------------
# Partial dependence plots with SE bands
# ----------------------------------
pdp_with_ci <- function(forest, focal_var, X, n_grid = 50) {
  
  grid_vals <- seq(
    min(X[[focal_var]], na.rm = TRUE),
    max(X[[focal_var]], na.rm = TRUE),
    length.out = n_grid
  )
  
  X_means <- as.data.frame(as.list(colMeans(X, na.rm = TRUE)))
  
  newdata <- X_means[rep(1L, n_grid), ]
  newdata[[focal_var]] <- grid_vals
  
  preds <- predict(forest, newdata = newdata, estimate.variance = TRUE)
  
  tibble(
    x        = grid_vals,
    estimate = preds$predictions,
    se       = sqrt(preds$variance.estimates),
    lower    = estimate - se,
    upper    = estimate + se,
    variable = focal_var
  )
}

pdp_data <- purrr::map_dfr(colnames(X), pdp_with_ci,
                           forest = eval.forest, X = X)

pdp_data <- pdp_data %>%
  mutate(label = recode(variable, !!!var_key))

pdp_plots <- pdp_data %>%
  group_by(variable) %>%
  group_split() %>%
  purrr::map(function(df) {
    ggplot(df, aes(x = x, y = estimate)) +
     # geom_ribbon(aes(ymin = lower, ymax = upper),
      #            fill = "grey70", alpha = 0.4) +
      geom_line(linewidth = 0.7, colour = "grey20") +
     # geom_hline(yintercept = 0, linetype = "dashed",
      #           colour = "grey50", linewidth = 0.4) +
      labs(
        x     = unique(df$label),
        y     = "Treatment effect on ANPP(Corn vs. Rangeland)",
        title = NULL
      ) +
      theme(
        panel.background = element_rect(fill = "white", colour = "grey50"),
        axis.title       = element_text(size = 8),
        axis.text        = element_text(size = 7)
      )
  })

wrap_plots(pdp_plots, ncol = 4)

# ----------------------------------
# SHAP values
# ----------------------------------
pred_fun <- function(object, newdata, ...) {
  predict(object, newdata, ...)$predictions
}

ks <- kernelshap(eval.forest, X = X, pred_fun = pred_fun)
shap_values <- shapviz(ks)

sv_importance(shap_values) &
  theme(panel.background = element_rect(fill = "white", colour = "grey50"))

sv_importance(shap_values, kind = "bee") &
  theme(panel.background = element_rect(fill = "white", colour = "grey50"))

sv_dependence(shap_values, v = colnames(X), color_var = NULL, jitter_width = 0.01) +
  plot_layout(ncol = 3)

# ----------------------------------
# Interaction statistics
# ----------------------------------
H <- hstats(eval.forest, X = X, pred_fun = pred_fun, verbose = FALSE)
plot(H)
