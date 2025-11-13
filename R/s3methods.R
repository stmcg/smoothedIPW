#' Print method for "ipw" objects
#'
#' Print method for objects of class "ipw".
#'
#' @param x Object of class "ipw".
#' @param ... Other arguments.
#' @return No value is returned.
#' @seealso \code{\link{ipw}}
#'
#' @examples
#' data_null_processed <- prep_data(data = data_null, grace_period_length = 2,
#'                                  baseline_vars = 'L')
#' res <- ipw(data = data_null_processed,
#'            time_smoothed = TRUE,
#'            outcome_times = c(6, 12, 18, 24),
#'            A_model = A ~ L + Z,
#'            R_model_numerator = R ~ L_baseline + Z,
#'            R_model_denominator = R ~ L + A + Z,
#'            Y_model = Y ~ L_baseline * (time + Z))
#' print(res)
#'
#' @export

print.ipw <- function(x, ...) {
  if (!inherits(x, "ipw")){
    stop("Argument 'x' must be an object of class \"ipw\".")
  }

  # Header
  cat("\n")
  cat("=======================================================================\n")
  if (x$outcome_type == 'continuous'){
    cat("  Point Estimates: Counterfactual Outcome Mean\n")
  } else if (x$outcome_type == 'binary'){
    cat("  Point Estimates: Counterfactual Outcome Probability\n")
  }
  cat("=======================================================================\n")
  cat("\n")

  # Key settings
  if (!x$args$time_smoothed){
    method_full <- "Non-smoothed IPW"
  } else {
    if (x$any_deaths & (x$args$smoothing_method %in% c('nonstacked', 'stacked'))){
      method_full <- paste("Time-smoothed IPW (", x$args$smoothing_method, ")", sep = "")
    } else {
      method_full <- "Time-smoothed IPW"
    }
  }
  cat("Settings:\n")
  cat("-----------------------------------------------------------------------\n")
  cat("  Method: ", method_full, "\n", sep = "")
  cat("  Outcome times: ", paste(x$args$outcome_times, collapse = ", "), "\n", sep = "")
  if (!is.null(x$args$truncation_percentile)){
    cat("  Weight truncation: ", round(x$args$truncation_percentile * 100, 1), "th percentile\n", sep = "")
  }
  cat("\n")

  # Estimates table
  cat("Estimates:\n")
  cat("-----------------------------------------------------------------------\n")
  print(x$est, row.names = FALSE)
  cat("\n")
}

#' Print method for "ipw_ci" objects
#'
#' Print method for objects of class "ipw_ci".
#'
#' @param x Object of class "ipw_ci".
#' @param ... Other arguments.
#' @return No value is returned.
#' @seealso \code{\link{get_CI}}
#'
#' @examples
#' \donttest{
#' set.seed(1234)
#' data_null_processed <- prep_data(data = data_null, grace_period_length = 2,
#'                                  baseline_vars = 'L')
#' res_est <- ipw(data = data_null_processed,
#'                time_smoothed = TRUE,
#'                outcome_times = c(6, 12, 18, 24),
#'                A_model = A ~ L + Z,
#'                R_model_numerator = R ~ L_baseline + Z,
#'                R_model_denominator = R ~ L + A + Z,
#'                Y_model = Y ~ L_baseline * (time + Z))
#' res_ci <- get_CI(ipw_res = res_est, data = data_null_processed, n_boot = 10)
#' print(res_ci)
#' }
#'
#' @export

print.ipw_ci <- function(x, ...) {
  if (!inherits(x, "ipw_ci")){
    stop("Argument 'x' must be an object of class \"ipw_ci\".")
  }

  # Header
  cat("\n")
  cat("=======================================================================\n")
  if (x$outcome_type == 'continuous'){
    cat("  Confidence Intervals: Counterfactual Outcome Mean\n")
  } else if (x$outcome_type == 'binary'){
    cat("  Confidence Intervals: Counterfactual Outcome Probability\n")
  }
  cat("=======================================================================\n")
  cat("\n")

  # Key settings
  cat("Settings:\n")
  cat("-----------------------------------------------------------------------\n")
  cat("  Method: ", x$method, "\n", sep = "")
  cat("  Outcome times: ", paste(x$outcome_times, collapse = ", "), "\n", sep = "")
  if (!is.null(x$truncation_percentile)){
    cat("  Weight truncation: ", round(x$truncation_percentile * 100, 1), "th percentile\n", sep = "")
  }
  cat("  Bootstrap samples: ", x$n_boot, "\n", sep = "")
  cat("  Confidence level: ", round(x$conf_level * 100, 1), "%\n", sep = "")
  cat("\n")

  # Confidence intervals table
  cat("Confidence Intervals:\n")
  cat("-----------------------------------------------------------------------\n")
  if (x$outcome_type == 'continuous'){
    label_prefix <- "Outcome Mean under Z = "
  } else if (x$outcome_type == 'binary'){
    label_prefix <- "Outcome Probability under Z = "
  }
  for (z_val in names(x$res_boot)){
    cat("\n", label_prefix, z_val, ":\n", sep = "")
    df_print <- as.data.frame(x$res_boot[[z_val]])
    print(df_print, row.names = FALSE)
  }
  cat("\n")
}
