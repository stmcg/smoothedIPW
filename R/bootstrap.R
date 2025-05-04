#' Bootstrap-based confidence intervals
#'
#' This function applies nonparametric bootstrap to construct confidence intervals around the counterfactual mean/probability estimates obtained by \code{\link{ipw}}.
#'
#' @param ipw_res Output from the \code{ipw} function.
#' @param data Data table containing the observed data
#' @param n_boot Numeric scalar specifying the number of bootstrap replicates to use
#' @param conf_level Numeric scalar specifying the confidence level for the confidence intervals. The default is \code{0.95}.
#'
#' @return A list that includes the following components:
#' \item{res_boot}{A list where each component corresponds to a different medication \eqn{z} level. Each component of the list is a data frame containing the estimates and confidence intervals for the counterfactual outcome mean/probability under the treatment regime indexed by \eqn{z}.}
#' \item{res_boot_all}{A three dimensional array containing all the bootstrap replicates. The first dimension corresponds to the bootstrap replicate; The second dimension corresponds to the time interval; The third dimension corresponds to the medication \eqn{z} level.}
#'
#' @details
#' Additional description of the method
#'
#' @examples
#' \donttest{
#' set.seed(1234)
#' data_null_processed <- prep_data(data = data_null, grace_period_length = 2,
#'                                  baseline_vars = 'L')
#' res_est <- ipw(data = data_null_processed,
#'                pooled = TRUE,
#'                outcome_times = c(6, 12, 18, 24),
#'                A_model = A ~ L + Z,
#'                R_model_numerator = R ~ L_baseline + Z,
#'                R_model_denominator = R ~ L + A + Z,
#'                Y_model = Y ~ L_baseline * (time + Z))
#' res_ci <- get_CI(ipw_res = res_est, data = data_null_processed, n_boot = 10)
#' res_ci$res_boot
#' }
#'
#'
#' @export

get_CI <- function(ipw_res, data, n_boot, conf_level = 0.95){
  # Check input
  if (missing(n_boot)){
    stop('The argument n_boot must be specified')
  }
  if (conf_level < 0 | conf_level > 1){
    stop('conf_level must be between 0 and 1')
  }
  if (!data.table::is.data.table(data)) {
    message("The argument 'data' must be a data.table. Converting 'data' to a data.table.")
    data <- data.table::as.data.table(data)
  }

  outcome_times <- eval(ipw_res$args$outcome_times)
  time_points <- length(outcome_times)
  z_levels <- sort(unique(data$Z))
  n_z <- length(z_levels)

  # Step 1: Perform bootstrapping
  res_boot_all <- array(NA, dim = c(n_boot, time_points, n_z))
  for (i in 1:n_boot){
    data_boot <- resample_data(data = data)
    ipw_res_boot <- ipw(data = data_boot,
                        pooled = ipw_res$args$pooled,
                        outcome_times = outcome_times,
                        A_model = eval(ipw_res$args$A_model),
                        R_model_numerator = eval(ipw_res$args$R_model_numerator),
                        R_model_denominator = eval(ipw_res$args$R_model_denominator),
                        Y_model = eval(ipw_res$args$Y_model),
                        truncation_percentile = eval(ipw_res$args$truncation_percentile),
                        return_model_fits = FALSE,
                        trim_returned_models = TRUE)
    for (j in 1:n_z){
      res_boot_all[i, , j] <- ipw_res_boot$est[, paste0('Z=', z_levels[j])]
    }
  }

  # Step 2: Compute CI
  alpha <- 1 - conf_level
  res_boot <- vector(mode = 'list', length = n_z)
  for (j in 1:n_z){
    res_boot_single <- matrix(NA, nrow = time_points, ncol = 4)
    colnames(res_boot_single) <- c('Time', 'Estimate', 'CI Lower', 'CI Upper')

    z_val <- z_levels[j]
    res_boot_z <- res_boot_all[, , j]

    res_boot_single[, 1] <- ipw_res$est[, 1]
    res_boot_single[, 2] <- ipw_res$est[, paste0('Z=', z_val)]

    if (time_points > 1){
      for (i in 1:time_points){
        res_boot_single[i, c(3, 4)] <- stats::quantile(res_boot_z[, i], probs = c(alpha / 2, 1 - alpha / 2))
      }
    } else {
      res_boot_single[1, c(3, 4)] <- stats::quantile(res_boot_z, probs = c(alpha / 2, 1 - alpha / 2))
    }
    res_boot[[j]] <- res_boot_single
  }
  names(res_boot) <- z_levels
  return(list(res_boot = res_boot, res_boot_all = res_boot_all))
}

#' @import data.table
resample_data <- function(data){
  n_id <- length(unique(data$id))

  # Sample IDs with replacement and call them new_id
  ids <- data.table::as.data.table(sample(1:n_id, n_id, replace = TRUE))
  ids[, 'new_id' := 1:n_id]
  colnames(ids) <- c("id", "new_id")

  # Merge data set with new_id values with the original dataset
  resample_data <- copy(data)
  setkey(resample_data, "id")
  resample_data <- resample_data[J(ids), allow.cartesian = TRUE]
  resample_data[, 'id' := resample_data$new_id]
  resample_data[, 'new_id' := NULL]

  return(resample_data)
}
