#' Bootstrap-based confidence intervals
#'
#' This function applies nonparametric bootstrap to construct confidence intervals around the counterfactual mean estimates obtained by \code{\link{ipw}}.
#'
#' @param ipw_res Output from the \code{ipw} function.
#' @param df Data frame containing the observed data
#' @param n_boot Numeric scalar specifying the number of bootstrap replicates to use
#' @param conf_level Numeric scalar specifying the confidence level for the confidence intervals. The default is \code{0.95}.
#'
#' @return A list that includes the following components:
#' \item{res_dif}{A data frame containing the estimates and confidence intervals for the average treatment effect. The \eqn{k}th row corresponds to the \eqn{k}th time point.}
#' \item{res_z0}{A data frame containing the estimates and confidence intervals for the counterfactual mean under \eqn{Z=0}. The \eqn{k}th row corresponds to the \eqn{k}th time point.}
#' \item{res_z1}{A data frame containing the estimates and confidence intervals for the counterfactual mean under \eqn{Z=1}. The \eqn{k}th row corresponds to the \eqn{k}th time point.}
#' \item{res_z0_boot_all}{A data frame containing the estimates of the counterfactual mean under \eqn{Z=0} in each bootstrap replicate. The columns correspond to the time points, and the rows correspond to the bootstrap replicates.}
#' \item{res_z1_boot_all}{A data frame containing the estimates of the counterfactual mean under \eqn{Z=1} in each bootstrap replicate. The columns correspond to the time points, and the rows correspond to the bootstrap replicates.}
#'
#' @details
#' Additional description of the method
#'
#' @export

get_CI <- function(ipw_res, df, n_boot, conf_level){
  time_points <- length(ipw_res$est_z0) - 1
  res_z0_boot_all <- res_z1_boot_all <- matrix(NA, nrow = n_boot, ncol = time_points + 1)
  for (i in 1:n_boot){
    df_boot <- resample_data(df = df)
    ipw_res_boot <- ipw(A_model = eval(ipw_res$args$A_model),
                        R_model_numerator = eval(ipw_res$args$R_model_numerator),
                        R_model_denominator = eval(ipw_res$args$R_model_denominator),
                        Y_model = eval(ipw_res$args$Y_model),
                        pooled = ipw_res$args$pooled,
                        df = df_boot,
                        truncation_percentile = eval(ipw_res$args$truncation_percentile))
    res_z0_boot_all[i, ] <- ipw_res_boot$est_z0
    res_z1_boot_all[i, ] <- ipw_res_boot$est_z1
  }

  res_dif <- res_z0 <- res_z1 <- matrix(NA, nrow = time_points + 1, ncol = 3)
  res_dif[, 1] <- ipw_res$est_z0 - ipw_res$est_z1
  res_z0[, 1] <- ipw_res$est_z0
  res_z1[, 1] <- ipw_res$est_z1

  alpha <- 1 - conf_level
  for (i in 1:(time_points + 1)){
    res_dif[i, c(2, 3)] <- stats::quantile(res_z0_boot_all[, i] - res_z1_boot_all[, i], probs = c(alpha / 2, 1 - alpha / 2))
    res_z0[i, c(2, 3)] <- stats::quantile(res_z0_boot_all[, i], probs = c(alpha / 2, 1 - alpha / 2))
    res_z1[i, c(2, 3)] <- stats::quantile(res_z1_boot_all[, i], probs = c(alpha / 2, 1 - alpha / 2))
  }
  colnames(res_dif) <- colnames(res_z0) <- colnames(res_z1) <- c('Estimate', 'CI Lower', 'CI Upper')
  return(list(res_dif = res_dif, res_z0 = res_z0, res_z1 = res_z1,
              res_z0_boot_all = res_z0_boot_all, res_z1_boot_all = res_z1_boot_all))
}

#' @import data.table
resample_data <- function(df){
  n_id <- length(unique(df$id))

  # Sample IDs with replacement and call them new_id
  ids <- data.table::as.data.table(sample(1:n_id, n_id, replace = TRUE))
  ids[, 'new_id' := 1:n_id]
  colnames(ids) <- c("id", "new_id")

  # Merge data set with new_id values with the original dataset
  resample_data <- copy(df)
  setkey(resample_data, "id")
  resample_data <- resample_data[J(ids), allow.cartesian = TRUE]
  resample_data[, 'id' := resample_data$new_id]
  resample_data[, 'new_id' := NULL]

  return(resample_data)
}
