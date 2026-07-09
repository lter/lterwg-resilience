library(tidyverse)
library(mgcv)
library(gratia)
library(patchwork)
library(segmented)

source("extremes/extremes_data_prep.R")

# ── Generalized GAM threshold bootstrap ──────────────────────────────────────
# pred: quoted column name of the focal predictor (x-axis)
# covariate: quoted column name used as the smoothed covariate (controls for MAP)
gam_threshold <- function(data, pred,
                          nboot = 500, fill_color = "steelblue", label = NULL) {
  set.seed(123)
  fml_gam <- as.formula(paste0("scaled_anpp ~ s(", pred, ")"))
  fml_lm  <- as.formula(paste0("scaled_anpp ~ ", pred))
  smooth_term <- paste0("s(", pred, ")")

  fit  <- gam(fml_gam, data = data, method = "REML")
  lm   <- gam(fml_lm,  data = data, method = "REML")
  aics <- AIC(fit, lm)
  aic_label <- paste0("AIC Linear: ", round(aics[2,2], 2),
                      "\nAIC GAM: ",  round(aics[1,2], 2))

  fits <- smooth_estimates(fit, select = smooth_term) %>% mutate(category = label)
  d2   <- derivatives(fit, select = smooth_term, order = 2,
                      type = "central", n = 500, eps = 1e-5) %>% mutate(category = label)
  threshold <- d2[[pred]][which.min(d2$.derivative)]

  boot_thresholds <- sapply(seq_len(nboot), function(i) {
    bd  <- data[sample(nrow(data), replace = TRUE), ]
    bg  <- tryCatch(gam(fml_gam, data = bd, method = "REML"), error = function(e) NULL)
    if (is.null(bg)) return(NA)
    bd2 <- tryCatch(derivatives(bg, select = smooth_term, order = 2,
                                type = "central", n = 500, eps = 1e-5), error = function(e) NULL)
    if (is.null(bd2)) return(NA)
    bd2[[pred]][which.min(bd2$.derivative)]
  }) %>% na.omit()

  ci <- quantile(boot_thresholds, c(0.1, 0.9))
  is_gam_better <- aics[1,2] < aics[2,2] - 2

  plt.smooth <- fits %>%
    ggplot(aes(x = .data[[pred]], y = .estimate)) +
    geom_point(data = data, aes(x = .data[[pred]], y = scaled_anpp), alpha = 0.4) +
    geom_line(color = fill_color, linewidth = 1.5,
              linetype = ifelse(is_gam_better, "solid", "dashed")) +
    coord_cartesian(xlim = c(-2.5, 2.5)) +
    annotate("text", x = -Inf, y = Inf, label = aic_label,
             hjust = 0, vjust = 1, size = 3.5, color = "firebrick") +
    labs(title = paste(label, "-", pred), x = pred, y = "Scaled ANPP") +
    theme_classic(base_size = 13)

  plt.d <- d2 %>%
    ggplot(aes(x = .data[[pred]], y = .derivative)) +
    geom_line(color = fill_color, linewidth = 1.5) +
    coord_cartesian(xlim = c(-2.5, 2.5), ylim = c(0.5, -0.5)) +
    labs(x = pred, y = "S''(x)") + theme_classic(base_size = 13)

  plt.th <- data.frame(threshold = as.numeric(boot_thresholds)) %>%
    ggplot(aes(x = threshold)) +
    geom_histogram(fill = fill_color, colour = fill_color, bins = 40) +
    geom_vline(xintercept = threshold, color = "firebrick", linewidth = 1.2, linetype = "dashed") +
    geom_vline(xintercept = ci, color = "firebrick", linewidth = 0.8, linetype = "dotted") +
    coord_cartesian(xlim = c(-2.5, 2.5)) +
    labs(x = paste(pred, "threshold"), y = "Count") + theme_classic(base_size = 13)

  list(estimates = fits, d2s = d2,
       boot_thresholds = boot_thresholds, threshold = threshold,
       ci_lower = ci[1], ci_upper = ci[2],
       n_successful = length(boot_thresholds), n_failed = nboot - length(boot_thresholds),
       plot = plt.smooth / plt.d / plt.th)
}

# ── Run for all predictor × category combinations ────────────────────────────
predictors <- c("scaled_ppt", "scaled_tmax", "SPEI")
categories <- c("Grassland", "Fert. Grassland", "Crop")
colors     <- c(Grassland = "green", `Fert. Grassland` = "darkgreen", Crop = "orange")

results <- lapply(predictors, function(pred) {
  lapply(setNames(categories, categories), function(cat) {
    gam_threshold(
      data       = ext_data_clean[ext_data_clean$category == cat & !is.na(ext_data_clean[[pred]]), ],
      pred       = pred,
      fill_color = colors[cat],
      label      = cat,
      nboot      = 500
    )
  })
}) %>% setNames(predictors)


wrap_elements(results$SPEI$Crop$plot) + wrap_elements(results$scaled_ppt$Crop$plot) + plot_layout(ncol=2)
# ── Composite plots: one panel per predictor (categories as columns) ──────────
for (pred in predictors) {
  plt <- Reduce(`+`, lapply(categories, function(cat) wrap_elements(results[[pred]][[cat]]$plot))) +
    plot_layout(ncol = length(categories))
  print(plt)
}


# ── Segmented breakpoint analysis ────────────────────────────────────────────
# Fits a linear model then estimates one breakpoint via segmented(), controlling
# for mean_ppt. Returns the breakpoint estimate, 95% CI, and a plot.
seg_threshold <- function(data, pred, covariate = "mean_ppt",
                          fill_color = "steelblue", label = NULL) {
  fml_lm <- as.formula(paste0("scaled_anpp ~ ", pred, " + ", covariate))
  lm_fit <- lm(fml_lm, data = data)

  seg_fit <- tryCatch(
    segmented(lm_fit, seg.Z = as.formula(paste0("~", pred)),
              control = seg.control(it.max = 200)),
    error = function(e) NULL
  )

  if (is.null(seg_fit)) {
    message("segmented() failed for ", label, " / ", pred); return(NULL)
  }

  bp    <- seg_fit$psi[, "Est."]
  bp_ci <- confint(seg_fit)                 # matrix (or list) of Est./low/up
  if (is.list(bp_ci)) bp_ci <- bp_ci[[1]]
  bp_ci <- bp_ci[1, ]                        # first (only) breakpoint: c(Est., low, up)

  # Compare segmented vs linear fit; better (lower) AIC drawn solid, other dashed
  aics       <- AIC(lm_fit, seg_fit)
  seg_better <- aics["seg_fit", "AIC"] < aics["lm_fit", "AIC"]
  aic_label  <- paste0("AIC Linear: ", round(aics["lm_fit", "AIC"], 2),
                       "\nAIC Segmented: ", round(aics["seg_fit", "AIC"], 2))

  # predicted lines from both models for plotting
  xseq  <- seq(min(data[[pred]], na.rm = TRUE), max(data[[pred]], na.rm = TRUE), length.out = 300)
  newdf <- setNames(data.frame(xseq, mean(data[[covariate]], na.rm = TRUE)),
                    c(pred, covariate))
  newdf$.seg <- predict(seg_fit, newdata = newdf)
  newdf$.lin <- predict(lm_fit,  newdata = newdf)

  plt <- ggplot(data, aes(x = .data[[pred]], y = scaled_anpp)) +
    geom_point(alpha = 0.4) +
    geom_line(data = newdf, aes(x = .data[[pred]], y = .seg),
              color = fill_color, linewidth = 1.5,
              linetype = ifelse(seg_better, "solid", "dashed")) +
    geom_line(data = newdf, aes(x = .data[[pred]], y = .lin),
              color = "grey40", linewidth = 1.5,
              linetype = ifelse(seg_better, "dashed", "solid")) +
    geom_vline(xintercept = bp, color = "firebrick", linewidth = 1.2, linetype = "dashed") +
    geom_vline(xintercept = bp_ci[2:3],
               color = "firebrick", linewidth = 0.8, linetype = "dotted") +
    annotate("text", x = -Inf, y = Inf,
             label = paste0("BP: ", round(bp, 3),
                            " [", round(bp_ci[2], 3),
                            ", ", round(bp_ci[3], 3), "]"),
             hjust = 0, vjust = 1, size = 3.5, color = "firebrick") +
    annotate("text", x = Inf, y = Inf, label = aic_label,
             hjust = 1, vjust = 1, size = 3.5, color = "grey30") +
    coord_cartesian(xlim = c(-2.5, 2.5)) +
    labs(title = paste(label, "-", pred, "(segmented)"),
         x = pred, y = "Scaled ANPP") +
    theme_classic(base_size = 13)

  list(model = seg_fit, breakpoint = bp, ci = bp_ci,
       aic_lin = aics["lm_fit", "AIC"], aic_seg = aics["seg_fit", "AIC"],
       seg_better = seg_better, plot = plt)
}

seg_results <- lapply(predictors, function(pred) {
  lapply(setNames(categories, categories), function(cat) {
    seg_threshold(
      data       = ext_data_clean[ext_data_clean$category == cat & !is.na(ext_data_clean[[pred]]), ],
      pred       = pred,
      fill_color = colors[cat],
      label      = cat
    )
  })
}) %>% setNames(predictors)

# Composite plot: one figure per predictor, categories as columns
for (pred in predictors) {
  plt <- Reduce(`+`, lapply(categories, function(cat) seg_results[[pred]][[cat]]$plot)) +
    plot_layout(ncol = length(categories))
  print(plt)
}

###test thresholds on crop clim window####

ext_data_cropclim <- ext_data_clean%>%
  filter(mean_ppt > min(ext_data_clean$mean_ppt[ext_data_clean$category == 'Crop']) & mean_ppt < max(ext_data_clean$mean_ppt[ext_data_clean$category == 'Crop']))


results_cropwin <- lapply(predictors, function(pred) {
  lapply(setNames(categories, categories), function(cat) {
    gam_threshold(
      data       = ext_data_cropclim [ext_data_cropclim $category == cat & !is.na(ext_data_cropclim [[pred]]), ],
      pred       = pred,
      fill_color = colors[cat],
      label      = cat,
      nboot      = 500
    )
  })
}) %>% setNames(predictors)


for (pred in predictors) {
  plt <- Reduce(`+`, lapply(categories, function(cat) wrap_elements(results_cropwin[[pred]][[cat]]$plot))) +
    plot_layout(ncol = length(categories))
  print(plt)
}

seg_results_cropwin <- lapply(predictors, function(pred) {
  lapply(setNames(categories, categories), function(cat) {
    seg_threshold(
      data       = ext_data_cropclim[ext_data_cropclim$category == cat & !is.na(ext_data_cropclim[[pred]]), ],
      pred       = pred,
      fill_color = colors[cat],
      label      = cat
    )
  })
}) %>% setNames(predictors)

# Composite plot: one figure per predictor, categories as columns
for (pred in predictors) {
  plt <- Reduce(`+`, lapply(categories, function(cat) seg_results_cropwin[[pred]][[cat]]$plot)) +
    plot_layout(ncol = length(categories))
  print(plt)
}



###test thresholds on crop clim window####

ext_data_cropclim <- ext_data_clean%>%
  filter(mean_ppt > min(ext_data_clean$mean_ppt[ext_data_clean$category == 'Crop']) & mean_ppt < max(ext_data_clean$mean_ppt[ext_data_clean$category == 'Crop']))


results_cropwin <- lapply(predictors, function(pred) {
  lapply(setNames(categories, categories), function(cat) {
    gam_threshold(
      data       = ext_data_cropclim [ext_data_cropclim $category == cat & !is.na(ext_data_cropclim [[pred]]), ],
      pred       = pred,
      fill_color = colors[cat],
      label      = cat,
      nboot      = 500
    )
  })
}) %>% setNames(predictors)


for (pred in predictors) {
  plt <- Reduce(`+`, lapply(categories, function(cat) wrap_elements(results_cropwin[[pred]][[cat]]$plot))) +
    plot_layout(ncol = length(categories))
  print(plt)
}

seg_results_cropwin <- lapply(predictors, function(pred) {
  lapply(setNames(categories, categories), function(cat) {
    seg_threshold(
      data       = ext_data_cropclim[ext_data_cropclim$category == cat & !is.na(ext_data_cropclim[[pred]]), ],
      pred       = pred,
      fill_color = colors[cat],
      label      = cat
    )
  })
}) %>% setNames(predictors)

# Composite plot: one figure per predictor, categories as columns
for (pred in predictors) {
  plt <- Reduce(`+`, lapply(categories, function(cat) seg_results_cropwin[[pred]][[cat]]$plot)) +
    plot_layout(ncol = length(categories))
  print(plt)
}



####

ext_data_clean%>%
  ggplot()+
  geom_label(aes(label = site, x =  mean_anpp, y = mean_ppt ))
ext_data_arid <- ext_data_clean%>%
  filter(mean_ppt > 400)%>%
  filter(category %in% c('Grassland', 'Fert. Grassland'))

categories_arid <- c('Grassland', 'Fert. Grassland')

results_arid <- lapply(predictors, function(pred) {
  lapply(setNames(categories_arid, categories_arid), function(cat) {
    gam_threshold(
      data       = ext_data_arid[ext_data_arid $category == cat & !is.na(ext_data_arid[[pred]]), ],
      pred       = pred,
      fill_color = colors[cat],
      label      = cat,
      nboot      = 100
    )
  })
}) %>% setNames(predictors)

