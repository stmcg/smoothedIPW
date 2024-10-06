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
#'            pooled = TRUE,
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

  cat('INVERSE PROBABILITY WEIGHTED ESTIMATES OF THE COUNTERFACTUAL OUTCOME MEAN \n\n')
  print(x$est, row.names = FALSE)
}
