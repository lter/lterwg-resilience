## ---------------------------------------------------------------------------
## threshold_model_functions.R
##
#---------------------

library(chngpt)


## --- internal helpers -------------------------------------------------------

# Gaussian log-likelihood evaluated at the MLE of sigma^2 (= RSS/n).
# Computed the same way for every model so the AIC comparison is apples-to-apples.
.gauss_loglik <- function(resid) {
  n <- length(resid)
  rss <- sum(resid^2)
  if (!is.finite(rss) || rss <= 0) return(NA_real_)
  -n / 2 * (log(2 * pi) + log(rss / n) + 1)
}

# Fitted values from an lm or a chngptm object.
.fitted_values <- function(fit) {
  fv <- tryCatch(as.numeric(stats::fitted(fit)), error = function(e) NULL)
  if (is.null(fv) || !length(fv)) {
    fv <- tryCatch(as.numeric(stats::predict(fit)), error = function(e) NULL)
  }
  fv
}

# Number of estimated parameters.
#   regression coefficients + 1 threshold (chngpt models only) + 1 residual variance
# chngpt sometimes reports the threshold inside coef(); strip it before re-adding
# so the count is right either way.
.n_par <- function(fit, has_threshold) {
  cf <- tryCatch(stats::coef(fit), error = function(e) NULL)
  if (is.null(cf)) return(NA_integer_)
  nm <- names(cf)
  n_reg <- length(cf)
  if (has_threshold && !is.null(nm)) {
    n_reg <- n_reg - sum(nm %in% c("chngpt", "threshold", "e"))
  }
  n_reg + as.integer(has_threshold) + 1L   # +1 threshold, +1 sigma
}

# Threshold estimate and (if available) its confidence interval.
.threshold_ci <- function(fit) {
  out <- c(threshold = NA_real_, threshold_lower = NA_real_, threshold_upper = NA_real_)
  if (is.null(fit)) return(out)
  out["threshold"] <- tryCatch(as.numeric(fit$chngpt)[1], error = function(e) NA_real_)
  ci <- tryCatch({
    v <- as.numeric(summary(fit)$chngpt)
    if (length(v) >= 3) v[2:3] else c(NA_real_, NA_real_)
  }, error = function(e) c(NA_real_, NA_real_))
  out["threshold_lower"] <- ci[1]
  out["threshold_upper"] <- ci[2]
  out
}


## --- main function ----------------------------------------------------------

#' Fit linear / segmented / stegmented models for one system type
#'
#' @param data         data frame (e.g. ext_data_clean)
#' @param type_name    character, the single level of `type_col` to analyse
#'                     (e.g. "Grassland", "Corn", "Wheat")
#' @param y_var,x_var  response and threshold predictor, as strings
#' @param type_col     column holding the system classification
#' @param covariates   optional character vector of additional linear covariates
#'                     (entered in formula.1, no threshold applied)
#' @param family       "gaussian" (default) or any family chngptm accepts
#' @param delta_cutoff a threshold model must beat the linear model by at least
#'                     this much AIC to be selected (default 2). Guards against
#'                     calling a threshold on a trivial improvement.
#' @param run_test     also run chngpt.test() for a p-value (slow; MC-based)
#' @param min_n        minimum complete observations required to attempt a fit
#'
#' @return list with
#'   $type, $n, $best_type, $best_model, $threshold_detected,
#'   $comparison (AIC table), $threshold (estimate + CI for the best model),
#'   $fits (all fitted objects), $data (rows actually used), $tests
fit_threshold_models <- function(data,
                                 type_name,
                                 y_var        = "scaled_anpp",
                                 x_var        = "scaled_ppt",
                                 type_col     = "type",
                                 covariates   = NULL,
                                 family       = "gaussian",
                                 delta_cutoff = 2,
                                 lb.quantile  = 0.1,
                                 ub.quantile  = 0.9,
                                 chngpts.cnt  = 100,
                                 est.method   = "default",
                                 run_test     = FALSE,
                                 test.mc.n    = 1e4,
                                 min_n        = 15,
                                 verbose      = TRUE) {

  stopifnot(is.data.frame(data), length(type_name) == 1)
  need <- c(y_var, x_var, type_col, covariates)
  missing_cols <- setdiff(need, names(data))
  if (length(missing_cols)) {
    stop("column(s) not found in data: ", paste(missing_cols, collapse = ", "))
  }

  ## ---- subset to this type, keep only complete + finite rows ---------------
  ## All three models are fit to exactly the same rows, otherwise the AIC
  ## comparison is meaningless.
  d <- as.data.frame(data)
  d <- d[!is.na(d[[type_col]]) & d[[type_col]] == type_name, , drop = FALSE]

  model_vars <- unique(c(y_var, x_var, covariates))
  keep <- stats::complete.cases(d[, model_vars, drop = FALSE])
  for (v in model_vars) {
    if (is.numeric(d[[v]])) keep <- keep & is.finite(d[[v]])
  }
  d <- d[keep, , drop = FALSE]
  n <- nrow(d)

  if (n < min_n) {
    warning(sprintf("type '%s': only %d usable rows (min_n = %d); skipping.",
                    type_name, n, min_n))
    return(list(type = type_name, n = n, best_type = NA_character_,
                best_model = NULL, threshold_detected = NA,
                comparison = NULL, threshold = NULL, fits = list(),
                data = d, tests = NULL))
  }
  if (verbose) message(sprintf("[%s] n = %d", type_name, n))

  ## ---- formulas ------------------------------------------------------------
  rhs1 <- if (is.null(covariates)) "1" else paste(covariates, collapse = " + ")
  f_lin  <- stats::as.formula(paste(y_var, "~", x_var,
                                    if (is.null(covariates)) "" else
                                      paste("+", rhs1)))
  f_null <- stats::as.formula(paste(y_var, "~", rhs1))   # formula.1 for chngptm
  f_chng <- stats::as.formula(paste("~", x_var))         # formula.2 for chngptm

  ## ---- fit -----------------------------------------------------------------
  fit_chng <- function(type) {
    tryCatch(
      chngpt::chngptm(formula.1   = f_null,
                      formula.2   = f_chng,
                      data        = d,
                      type        = type,
                      family      = family,
                      est.method  = est.method,
                      chngpts.cnt = chngpts.cnt,
                      lb.quantile = lb.quantile,
                      ub.quantile = ub.quantile,
                      var.type    = "default"),
      error = function(e) {
        warning(sprintf("type '%s': %s fit failed - %s",
                        type_name, type, conditionMessage(e)))
        NULL
      })
  }

  m_lin <- tryCatch(
    if (identical(family, "gaussian")) stats::lm(f_lin, data = d)
    else stats::glm(f_lin, data = d, family = family),
    error = function(e) { warning("linear fit failed: ", conditionMessage(e)); NULL })

  m_seg  <- fit_chng("segmented")
  m_steg <- fit_chng("stegmented")

  fits <- list(linear = m_lin, segmented = m_seg, stegmented = m_steg)

  ## ---- AIC table -----------------------------------------------------------
  y_obs <- d[[y_var]]

  one_row <- function(nm, fit, has_threshold) {
    if (is.null(fit)) {
      return(data.frame(model = nm, k = NA_integer_, logLik = NA_real_,
                        AIC = NA_real_, AIC_pkg = NA_real_, converged = FALSE,
                        threshold = NA_real_, threshold_lower = NA_real_,
                        threshold_upper = NA_real_,
                        stringsAsFactors = FALSE))
    }
    fv <- .fitted_values(fit)
    ll <- if (!is.null(fv) && length(fv) == length(y_obs) &&
              identical(family, "gaussian")) {
      .gauss_loglik(y_obs - fv)
    } else {
      tryCatch(as.numeric(stats::logLik(fit)), error = function(e) NA_real_)
    }
    k  <- .n_par(fit, has_threshold)
    th <- if (has_threshold) .threshold_ci(fit) else
      c(threshold = NA_real_, threshold_lower = NA_real_, threshold_upper = NA_real_)

    data.frame(model = nm,
               k = k,
               logLik = ll,
               AIC = -2 * ll + 2 * k,
               AIC_pkg = tryCatch(as.numeric(stats::AIC(fit)),
                                  error = function(e) NA_real_),
               converged = TRUE,
               threshold = th[["threshold"]],
               threshold_lower = th[["threshold_lower"]],
               threshold_upper = th[["threshold_upper"]],
               stringsAsFactors = FALSE)
  }

  comparison <- rbind(one_row("linear",     m_lin,  FALSE),
                      one_row("segmented",  m_seg,  TRUE),
                      one_row("stegmented", m_steg, TRUE))
  rownames(comparison) <- NULL
  comparison$type <- type_name
  comparison$n    <- n

  if (all(is.na(comparison$AIC))) {
    warning(sprintf("type '%s': no model converged.", type_name))
    return(list(type = type_name, n = n, best_type = NA_character_,
                best_model = NULL, threshold_detected = NA,
                comparison = comparison, threshold = NULL,
                fits = fits, data = d, tests = NULL))
  }

  comparison$delta_AIC <- comparison$AIC - min(comparison$AIC, na.rm = TRUE)
  w <- exp(-0.5 * comparison$delta_AIC)
  comparison$AIC_weight <- w / sum(w, na.rm = TRUE)

  ## ---- optional hypothesis tests ------------------------------------------
  ## Note: chngpt.test tests the threshold term against the no-threshold null.
  ## It is MC-based and slow, so it is off by default.
  tests <- NULL
  if (run_test) {
    run_one <- function(type) {
      tryCatch(chngpt::chngpt.test(formula.null   = f_null,
                                   formula.chngpt = f_chng,
                                   data           = d,
                                   family          = family,
                                   type            = type,
                                   test.statistic  = "lr",
                                   mc.n            = test.mc.n,
                                   lb.quantile     = lb.quantile,
                                   ub.quantile     = ub.quantile,
                                   chngpts.cnt     = chngpts.cnt,
                                   verbose         = FALSE),
               error = function(e) {
                 warning(sprintf("type '%s': %s test failed - %s",
                                 type_name, type, conditionMessage(e)))
                 NULL
               })
    }
    tests <- list(segmented = run_one("segmented"),
                  stegmented = run_one("stegmented"))
    pv <- vapply(tests, function(z)
      if (is.null(z)) NA_real_ else as.numeric(z$p.value)[1], numeric(1))
    comparison$p_value <- c(NA_real_, pv[["segmented"]], pv[["stegmented"]])
  }

  ## ---- pick the best model -------------------------------------------------
  ## A threshold model must beat the linear model by >= delta_cutoff AIC units.
  aic_lin <- comparison$AIC[comparison$model == "linear"]
  thr_rows <- comparison[comparison$model %in% c("segmented", "stegmented") &
                           !is.na(comparison$AIC), , drop = FALSE]

  best_type <- "linear"
  threshold_detected <- FALSE
  if (nrow(thr_rows)) {
    best_thr <- thr_rows$model[which.min(thr_rows$AIC)]
    aic_thr  <- min(thr_rows$AIC)
    if (is.na(aic_lin) || (aic_lin - aic_thr) >= delta_cutoff) {
      best_type <- best_thr
      threshold_detected <- TRUE
    }
  }
  comparison$best <- comparison$model == best_type
  comparison <- comparison[order(comparison$AIC, na.last = TRUE), ]
  rownames(comparison) <- NULL

  best_model <- fits[[best_type]]
  thr_out <- if (threshold_detected) .threshold_ci(best_model) else NULL

  if (verbose) {
    message(sprintf("[%s] best = %s%s", type_name, best_type,
                    if (threshold_detected)
                      sprintf(" (threshold %s = %.3f)", x_var, thr_out[["threshold"]])
                    else " (no threshold)"))
  }

  list(type               = type_name,
       n                  = n,
       x_var              = x_var,
       y_var              = y_var,
       best_type          = best_type,
       best_model         = best_model,
       threshold_detected = threshold_detected,
       threshold          = thr_out,
       comparison         = comparison,
       fits               = fits,
       tests              = tests,
       data               = d)
}


## --- convenience: run over several types and stack the results --------------

#' Run fit_threshold_models() once per type and return a tidy summary
#'
#' @return list with $results (named list, one element per type) and
#'         $summary (one row per type: best model, threshold, AIC gap)
fit_threshold_models_by_type <- function(data,
                                         types = NULL,
                                         type_col = "type",
                                         ...) {
  if (is.null(types)) types <- sort(unique(stats::na.omit(data[[type_col]])))
  results <- stats::setNames(lapply(types, function(tp)
    fit_threshold_models(data, type_name = tp, type_col = type_col, ...)), types)

  summary_tab <- do.call(rbind, lapply(results, function(r) {
    if (is.null(r$comparison)) return(NULL)
    lin <- r$comparison$AIC[r$comparison$model == "linear"]
    bst <- r$comparison$AIC[r$comparison$model == r$best_type]
    data.frame(type = r$type,
               n = r$n,
               best_model = r$best_type,
               threshold_detected = r$threshold_detected,
               aic_gain_vs_linear = if (length(lin) && length(bst)) lin - bst else NA_real_,
               threshold = if (is.null(r$threshold)) NA_real_ else r$threshold[["threshold"]],
               threshold_lower = if (is.null(r$threshold)) NA_real_ else r$threshold[["threshold_lower"]],
               threshold_upper = if (is.null(r$threshold)) NA_real_ else r$threshold[["threshold_upper"]],
               stringsAsFactors = FALSE)
  }))
  rownames(summary_tab) <- NULL

  list(results = results, summary = summary_tab)
}


## --- convenience: plot the selected fit -------------------------------------

#' Scatter + fitted line from the selected model, with the threshold marked
plot_threshold_fit <- function(res, point_alpha = 0.3) {
  if (is.null(res$best_model)) return(NULL)
  d  <- res$data
  xs <- seq(min(d[[res$x_var]]), max(d[[res$x_var]]), length.out = 300)
  nd <- d[rep(1L, length(xs)), , drop = FALSE]
  nd[[res$x_var]] <- xs
  pred <- tryCatch(as.numeric(stats::predict(res$best_model, newdata = nd)),
                   error = function(e) NULL)
  if (is.null(pred)) return(NULL)
  line_df <- data.frame(x = xs, y = pred)

  p <- ggplot2::ggplot(d, ggplot2::aes(x = .data[[res$x_var]],
                                       y = .data[[res$y_var]])) +
    ggplot2::geom_point(alpha = point_alpha) +
    ggplot2::geom_line(data = line_df, ggplot2::aes(x = x, y = y),
                       linewidth = 1, colour = "steelblue") +
    ggplot2::labs(title = sprintf("%s  (n = %d)  -  best: %s",
                                  res$type, res$n, res$best_type),
                  x = res$x_var, y = res$y_var) +
    ggplot2::theme_classic()

  if (isTRUE(res$threshold_detected) && is.finite(res$threshold[["threshold"]])) {
    p <- p + ggplot2::geom_vline(xintercept = res$threshold[["threshold"]],
                                 linetype = "dashed", colour = "red")
  }
  p
}
<<<<<<< HEAD
=======


## --- example usage ----------------------------------------------------------
# source("extremes/extremes_data_prep.R")
#
res_grassland <- fit_threshold_models(ext_data_clean, "Grassland", lb.quantile = 0.05, ub.quantile = 0.95)
res_fertgrassland <- fit_threshold_models(ext_data_clean, "Fert. Grassland", lb.quantile = 0.05, ub.quantile = 0.95)
res_corn      <- fit_threshold_models(ext_data_clean, "Corn", lb.quantile = 0.05, ub.quantile = 0.95)
res_wheat     <- fit_threshold_models(ext_data_clean, "Wheat", lb.quantile = 0.05, ub.quantile = 0.95)
#
res_grassland$comparison
summary(res_grassland$best_model)
summary(res_fertgrassland$best_model)

plot_threshold_fit(res_corn)
#
# # or all at once:
# all_res <- fit_threshold_models_by_type(ext_data_clean)
# all_res$summary

#plot 

library(patchwork)

p.grass <- plot_threshold_fit(res_grassland)
p.fertgrass <- plot_threshold_fit(res_fertgrassland)
p.corn <- plot_threshold_fit(res_corn)
p.wheat <- plot_threshold_fit(res_wheat)
p.grass + p.fertgrass
>>>>>>> d9a1346a802a9fc68c343770518fae9f00069c5e
