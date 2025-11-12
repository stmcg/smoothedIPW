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
