#' Prepare data set for inverse probability weighting
#'
#' This function adds columns to the input data set to assist with inverse probability weighting. See details.
#'
#' @param data Data frame containing the observed data
#' @param grace_period_length Numeric scalar indicating the length of the grace period, if applicable. The default is \code{0}, indicating no grace period.
#' @param baseline_vars Vector of character strings specifying the names of the baseline covariates that should be added to the observed data.
#' @param lag_vars Vector of character strings specifying the names of the covariates whose lags should be added as columns to the observed data. The number of lags is controlled by the \code{n_lags} argument.
#' @param n_lags Numeric scalar specifying the number of lags to use when computing the lagged values of \code{lag_vars}. Additional columns will be created for 1, ..., \code{n_lags} lags of the variables specified in \code{lag_vars}.
#'
#' @return A data table containing the observed data with the additional columns.
#'
#' @details
#' This function performs the following tasks:
#' \itemize{
#' \item Adds a column \code{C_artificial} which indicates when an individual should be artificially censored from the data when applying inverse probability weighting.
#' \item Adds a column \code{A_model_eligible} which indicates what records should be used for fitting the treatment adherence model.
#' \item If \code{baseline_vars} is supplied, it adds columns corresponding to the baseline value of these variables These columns have the name \code{_baseline} appended to them.
#' \item If \code{lag_vars} is supplied, it adds columns corresponding to the lagged value of these variables. For each of these variables, additional columns will be created for 1, ..., \code{n_lags} lags of the variable.
#' }
#'
#' @examples
#' data_null_processed <- prep_data(data = data_null, grace_period_length = 2,
#'                                  baseline_vars = 'L')
#'
#'
#' @export
prep_data <- function(data, grace_period_length = 0,
                      baseline_vars = NULL,
                      lag_vars = NULL,
                      n_lags = 1){

  # Inefficient version of the code
  # data[, C_artificial := 0]
  # data[, end_of_grace_period := 0]
  # for(i in 1:nrow(data)){
  #   if (data[i, 'time'] == 0){
  #     # Baseline
  #     count_not_adhered_until_now <- 0
  #   } else {
  #     # Follow-up
  #     count_not_adhered_until_now <- (count_not_adhered_until_now + 1) * (1 - data[i-1, 'A'])
  #     # data[i, 'end_of_grace_period'] <- ifelse(count_not_adhered_until_now == grace_period_length, 1, 0)
  #     # if ((data[i, 'A'] == 0 & data[i, 'end_of_grace_period'] == 1) |
  #     #     data[i - 1, 'C_artificial'] == 1){
  #     #   data[i, 'C_artificial'] <- 1
  #     # }
  #   }
  # }

  if (!is.data.table(data)){
    data <- as.data.table(data)
  }

  # Adding indicator of A_model_eligible
  if (grace_period_length > 0){
    data[, count_not_adhered_until_now := 0]
    data[, count_not_adhered_until_now := {
      count_vector <- numeric(.N)
      for (i in 2:.N) {
        if (time[i] > 0) {
          count_vector[i] <- (count_vector[i - 1] + 1) * (1 - A[i - 1])
        }
      }
      count_vector
    }]
    data[, end_of_grace_period := as.integer(count_not_adhered_until_now == grace_period_length)]
    data[, count_not_adhered_until_now := NULL]
    data[, A_model_eligible := end_of_grace_period]
  } else {
    data[, A_model_eligible := as.integer(time > 0)]
  }

  # Adding indicator of artificial censoring
  data[, C_artificial_temp := as.integer(A == 0 & A_model_eligible == 1)]
  data[, C_artificial := as.integer(cumsum(C_artificial_temp) >= 1), by = id]
  data[, C_artificial_temp := NULL]

  # Adding baseline covariates
  if (!is.null(baseline_vars)){
    if (!all(baseline_vars %in% colnames(data))){
      stop('Some variables in baseline_vars do not exist in data')
    }
    for (var in baseline_vars){
      data[, paste0(var, '_baseline') := get(var)[time == 0], by = id]
    }
  }

  # Adding lagged covariates
  if (!is.null(lag_vars)){
    if (!all(lag_vars %in% colnames(data))){
      stop('Some variables in lag_vars do not exist in data')
    }
    for (var in lag_vars){
      for (lag in 1:n_lags) {
        lagged_col_name <- paste0(var, "_lag", lag)
        data[, (lagged_col_name) := shift(get(var), n = lag, type = "lag"), by = id]
        data[is.na(get(lagged_col_name)), (lagged_col_name) := 0]
      }
    }
  }
  return(data)
}
