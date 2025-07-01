#' @title Identify Whiplash Events
#' 
#' @description Calculates whiplash events using the metholodogy described by Swain et al. 2025 (doi.org/10.1038/s43017-024-00624-z). Briefly, that involves calculating pairwise differences among months in a moving window for a user-defined number of prior months relative to the last month of the window. These differences are then evaluated against user-specified percentiles. If specified, the percentiles are evaluated against the differences in a reference period (if unspecified, percentiles are computed from full provided record). Note that differences of `NA` are automatically removed because this is most likely caused by 'prior' months preceding the start of available data in the provided data object
#' 
#' @param df (data.frame-like) tabular data object for which to apply this method
#' @param date_col (character) name of column in 'df' containing date information. Must be granular enough to include--at least--month-level information. Must be an unambiguous date format (see `?as.Date`)
#' @param enviro_col (character) name of column in 'df' containing environmental information. Must be a numeric column
#' @param ref_period (numeric) two, 4-digit years to mark the start and end of the reference period (inclusive)
#' @param window_size (numeric) number of months to include in each window. Defaults to "3"
#' @param whiplash_perc (numeric) two numbers indicating the percentiles the user wants to use for determining whiplash events
#' @param quiet (logical) whether to print a progress message for each window. For longer time series, calculating all differences can be time-consuming so it is recommended to leave this argument as `FALSE` (the default) so that it is clear the function has not crashed
#' 
#' @return (data.frame) a dataframe containing the date and environment columns provided in the initial object as well as columns for (1) the preceding date with the biggest difference for the focal date's window, (2) the environment value for that prior date, (3) the difference between the focal environment value the respective prior environment value, (4-5) the upper and lower environment thresholds calculated with the provided percentiles, (6) whether each date is a maximum or whiplash event, and (7) a duplicate of the difference between the focal and prior environment value but only non-`NA` for whiplash events (of either direction). Relevant column names inherit conventions from the values passed to the 'date_col' and 'enviro_col' arguments.
#' 
#' @importFrom magrittr %>%
#' 
#' @export
#' 
id_whiplash <- function(df = NULL, date_col = NULL, enviro_col = NULL,
                        ref_period = NULL, window_size = 3, 
                        whiplash_perc = NULL, quiet = FALSE){
  
  # Error for non-dataframe 'df' or missing columns
  if(is.null(df) || "data.frame" %in% class(df) != T)
    stop("'df' must be provided and be dataframe-like")
  
  # Errors for 'date_col' argument
  if(is.null(date_col) || is.character(date_col) != T || date_col %in% names(df) != T || length(date_col) != 1 || class(df[[date_col]]) != "Date")
    stop("'date_col' must be a length-one character vector that exactly matches a column name in 'df' containing date information")
  
  # Errors for 'enviro_col' argument
  if(is.null(enviro_col) || is.character(enviro_col) != T || enviro_col %in% names(df) != T || length(enviro_col) != 1 || any(c("numeric", "integer") %in% class(df[[enviro_col]])) != T)
    stop("'enviro_col' must be a length-one character vector that exactly matches a column name in 'df' containing numeric information")
  
  # If reference period is provided, do error checks
  if(is.null(ref_period) != T){
    if(length(ref_period) != 2 || length(unique(ref_period)) != 2 ||
       any(c("numeric", "integer") %in% class(ref_period)) != T ||
       all(nchar(ref_period) == 4) != T)
      stop("If provided, 'ref_period' must be two different, 4-digit numbers")
  }
  
  # Errors for 'window_size'
  if(is.null(window_size) || length(window_size) != 1 || all(c("numeric", "integer") %in% class(window_size) != T) || window_size <= 0 || window_size - floor(window_size) != 0)
    stop("'window_size' must be a single integer greater than 0")
  
  # Error checks for whiplash percentile
  if(is.null(whiplash_perc) || length(whiplash_perc) != 2 || 
     length(unique(whiplash_perc)) != 2 || is.numeric(whiplash_perc) != T ||
     any(whiplash_perc > 1) || any(whiplash_perc < 0) )
    stop("'whiplash_perc' must be provided as two numbers between 0 and 1")
  
  # Warning for non-logical 'quiet'
  if(is.logical(quiet) != T){
    warning("'quiet' must be a logical. Defaulting to 'FALSE'")
    quiet <- FALSE }  
  
  # Identify reference start / end
  ref_start <- min(ref_period, na.rm = T)
  ref_end <- max(ref_period, na.rm = T)
  
  # Calculate a 'year' column
  df_v2 <- dplyr::mutate(df, year___temp = year( df[[ {{date_col}} ]] ) )
  
  # Print a warning if the reference period is outside of the bounds of the data
  if(ref_start < min(df_v2$year___temp, na.rm = T) | 
     ref_end > max(df_v2$year___temp, na.rm = T))
    warning("Reference period is at least partially outside of the total range of years in 'df'")
  
  # Subset provided data to within reference period (if provided)
  ## Date should be after the start
  if(is.null(ref_start) != TRUE){
    ref_v1 <- dplyr::filter(df_v2, year___temp >= ref_start )
  } else { ref_v1 <- df_v2 } 
  ## And after end
  if(is.null(ref_end) != TRUE){
    ref_v2 <- dplyr::filter(ref_v1, year___temp <= ref_end )
  } else { ref_v2 <- ref_v1 }
  ## Then throw away temp year column
  ref <- dplyr::select(ref_v2, -year___temp)
  
  # Calculate month-by-month differences within specified windows
  ## For the reference period (if one is provided)
  if(is.null(ref_period) != T){
    if(quiet != T){ message("Calculating differences within windows for reference period")}
    ref_diff <- diff_windows(df = ref, date_col = date_col, enviro_col = enviro_col,
                             window_size = window_size, quiet = quiet)
  }
  ## For full record 
  if(quiet != T){ message("Calculating differences within windows for full data")}
  full_diff <- diff_windows(df = df, date_col = date_col, enviro_col = enviro_col,
                            window_size = window_size, quiet = quiet)
  
  # Identify the upper/lower percentiles
  perc_lower <- min(whiplash_perc, na.rm = T)
  perc_upper <- max(whiplash_perc, na.rm = T)
  
  # Pick which df should be used for identifying thresholds
  ## Is the reference data if any reference period is specified
  if(is.null(ref_period)){ thresh_df <- full_diff } else { thresh_df <- ref_diff } 
  
  # Identify the environmental threshold differences that match the user-defined percentiles
  thresh_lower <- as.numeric(quantile(x = thresh_df[[paste0(enviro_col, "_diff")]],
                                      probs = perc_lower))
  thresh_upper <- as.numeric(quantile(x = thresh_df[[paste0(enviro_col, "_diff")]],
                                      probs = perc_upper))
  
  # Now we have thresholds, we can actually identify whiplash events for the full record
  whiplash_out <- full_diff %>% 
    ## NOTE: whiplash events are *not* inclusive of the threshold value
    ## A threshold of 1.0 means a difference of 1.0 is **not** a whiplash event
    dplyr::mutate(whiplash = dplyr::case_when(
      .data[[paste0(enviro_col, "_diff")]] > thresh_upper ~ "max whiplash",
      .data[[paste0(enviro_col, "_diff")]] < thresh_lower ~ "min whiplash",
      T ~ NA)) %>% 
    # Duplicate enviro differences for just whiplash events
    dplyr::mutate(whiplash_diff = ifelse(is.na(whiplash), yes = NA,
                                         no = .data[[paste0(enviro_col, "_diff")]])) %>% 
    # And add thresholds to the dataframe (so users can double check them)
    dplyr::mutate(whiplash_thresh_upper = thresh_upper,
                  whiplash_thresh_lower = thresh_lower,
                  .before = whiplash)
  
  # Return desired object
  return(whiplash_out) }

# End ----
