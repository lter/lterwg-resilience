#' @title Calculate Differences within Moving Windows
#' 
#' @description Calculates pairwise differences between the last month in a moving window for a user-defined number of prior months relative to the last month in the window. Returns results as a dataframe for further customization. See argument descriptions for more details. Automatically removes differences of `NA` because this is most likely caused by 'prior' months preceding the start of available data in the provided data object.
#' 
#' @param df (data.frame-like) tabular data object for which to apply this method
#' @param date_col (character) name of column in 'df' containing date information. Must be granular enough to include--at least--month-level information. Must be an unambiguous date format (see `?as.Date`)
#' @param enviro_col (character) name of column in 'df' containing environmental information. Must be a numeric column
#' @param window_size (numeric) number of months to include in each window. Defaults to "3"
#' @param quiet (logical) whether to print a progress message for each window. For longer time series, calculating all differences can be time-consuming so it is recommended to leave this argument as `FALSE` (the default) so that it is clear the function has not crashed
#' 
#' @return (data.frame) a dataframe containing a date column and environmental column as well as a column for each prior date and environmental variable at that previous date. Also returns the difference between the original environmental variable and the 'prior' one. All column names inherit conventions from the values passed to the 'date_col' and 'enviro_col' arguments.
#' 
#' @importFrom magrittr %>%
#' 
#' @export
#' 
diff_windows <- function(df = NULL, date_col = "date", enviro_col = "SPEI",
                         window_size = 3, quiet = FALSE){
  
  # Error for non-dataframe 'df' or missing columns
  if("data.frame" %in% class(df) != T || any(c(date_col, enviro_col) %in% names(df)) != T )
    stop("'df' must be dataframe-like and have column names exactly matching 'date_col' and 'enviro_col'")
  
  # Errors for 'date_col' argument
  if(is.character(date_col) != T || date_col %in% names(df) != T || length(date_col) != 1 || class(df[[date_col]]) != "Date")
    stop("'date_col' must be a length-one character vector that exactly matches a column name in 'df' containing date information")
  
  # Errors for 'enviro_col' argument
  if(is.character(enviro_col) != T || enviro_col %in% names(df) != T || length(enviro_col) != 1 || any(c("numeric", "integer") %in% class(df[[enviro_col]])) != T)
    stop("'enviro_col' must be a length-one character vector that exactly matches a column name in 'df' containing numeric information")
  
  # Errors for 'window_size'
  if(length(window_size) != 1 || all(c("numeric", "integer") %in% class(window_size) != T) || window_size <= 0 || window_size - floor(window_size) != 0)
    stop("'window_size' must be a single integer greater than 0")
  
  # Warning for non-logical 'quiet'
  if(is.logical(quiet) != T){
    warning("'quiet' must be a logical. Defaulting to 'FALSE'")
    quiet <- FALSE }
  
  # Make a list for storing outputs
  diff_list <- list()
  
  # Loop across dates in df
  for(focal_date in df[[{{date_col}}]]){
    
    # Coerce to real date & overwrite existing object (should be one already...)
    focal_date <- as.Date(focal_date)
    
    # If 'quiet' isn't false, return a processing message  
    if(quiet != TRUE){
      message("Identifying environmental differences in window relative to ", focal_date) }
    
    # Grab enviro variable at that time point
    focal_enviro <- dplyr::filter(df, !!as.symbol(date_col) == focal_date)[[{{enviro_col}}]]
    
    # Loop across window size amount of prior months
    for(months_prior in window_size:1){
      ## months_prior <- 3
      
      # Identify date at number of months prior to focal date
      prior_date <- focal_date - months(months_prior)
      
      # Identify enviro at that date
      prior_enviro <- dplyr::filter(df, !!as.symbol(date_col) == prior_date)[[{{enviro_col}}]]
      
      # If no date exists...
      if(length(prior_enviro) == 0){
        ## ...Fill it and the difference with NA
        prior_enviro <- NA_real_
        diff_enviro <- NA_real_
        
        ## Otherwise, calculate true difference
      } else { diff_enviro <- focal_enviro - prior_enviro }
      
      # Assemble dataframe variant of output
      diff_out <- data.frame("date" = focal_date,
                             "enviro" = focal_enviro,
                             "prior_date" = prior_date,
                             "prior_enviro" = prior_enviro,
                             "enviro_diff" = diff_enviro)
      
      # Add to output list
      diff_list[[paste0(focal_date, "_", months_prior)]] <- diff_out
      
    } # Close prior months relative to focal date loop
  } # Close focal date loop
  
  # Unlist output & process it
  diff_df <- purrr::list_rbind(x = diff_list) %>% 
    # Drop NA enviro differences (where window exceeds available date range)
    dplyr::filter(!is.na(enviro_diff)) %>% 
    # Within dates, keep only most extreme value
    dplyr::group_by(date, enviro) %>% 
    dplyr::filter(prior_enviro == max(abs(prior_enviro), na.rm = T)) %>% 
    dplyr::ungroup()
  
  # Rename this to better match inputs
  diff_out <- supportR::safe_rename(data = diff_df,
                                    bad_names = c("date", "enviro", 
                                                  "prior_date", "prior_enviro",
                                                  "enviro_diff"),
                                    good_names = c(date_col, enviro_col,
                                                   paste0("prior_", date_col),
                                                   paste0("prior_", enviro_col),
                                                   paste0(enviro_col, "_diff")))
  
  
  # Return that to user
  return(diff_out) }

# End ----
