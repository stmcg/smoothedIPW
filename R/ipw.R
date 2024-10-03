#' Pooled inverse probability weighting
#'
#' This function applies the pooled inverse probability weighted approach described by McGrath et al. (in preparation).
#'
#' @param data Data frame containing the observed data
#' @param pooled Logical scalar specifying whether the pooled or nonpooled IPW method is applied. The default is \code{TRUE}, i.e., the pooled IPW method.
#' @param A_model Model statement for the treatment variable
#' @param R_model_numerator Model statement for the indicator variable for the measurement of the outcome variable, used in the numerator of the IP weights
#' @param R_model_denominator Model statement for the indicator variable for the measurement of the outcome variable, used in the denominator of the IP weights
#' @param Y_model Model statement for the outcome variable
#' @param truncation_percentile Numerical scalar specifying the percentile by which to truncated the IP weights
#'
#' @return A list that includes the following components:
#' \item{est}{A data frame containing the counterfactual mean estimates for each medication at each time interval.}
#' \item{args}{A list containing the arguments supplied to \code{ipw}, except the observed data set.}
#'
#' @details
#' Additional description of the method
#'
#' @examples
#' res <- ipw(data = data_null,
#'            pooled = TRUE,
#'            A_model = A ~ L + Z,
#'            R_model_numerator = R ~ L0 + Z,
#'            R_model_denominator = R ~ L + A + Z,
#'            Y_model = Y ~ L0 * (t0 + Z))
#' res$est
#'
#' @export

ipw <- function(data,
                pooled = TRUE,
                A_model,
                R_model_numerator,
                R_model_denominator,
                Y_model,
                truncation_percentile = NULL){
  # Set parameters
  time_points <- max(data$t0)

  # Fit models for the nuisance functions
  fit_A <- stats::glm(A_model, family = 'binomial', data = data[data$G == 1,])
  fit_R_denominator <- stats::glm(R_model_denominator, family = 'binomial', data = data)
  fit_R_numerator <- stats::glm(R_model_numerator, family = 'binomial', data = data)

  # Artificially censor individuals when they deviate from the treatment strategy
  data_censored <- data[data$C_artificial == 0,]

  # Compute IP weights based on censored data set
  prob_A1 <- ifelse(data_censored$G == 1, stats::predict(fit_A, type = 'response', newdata = data_censored), 1)
  prob_R1_denominator <- stats::predict(fit_R_denominator, type = 'response', newdata = data_censored)
  prob_R1_numerator <- stats::predict(fit_R_numerator, type = 'response', newdata = data_censored)
  weights_A <- unname(unlist(tapply(1 / prob_A1, data_censored$id, FUN = cumprod)))
  weights_R <- ifelse(data_censored$R == 1, prob_R1_numerator / prob_R1_denominator, 0)
  data_censored$weights <- weights_A * weights_R

  # Truncate IP weights, if applicable
  if (!is.null(truncation_percentile)){
    trunc_val <- stats::quantile(data_censored$weights, probs = truncation_percentile)
    data_censored$weights <- pmin(data_censored$weights, truncation_percentile)
  }

  # Preparing data sets for estimating counterfactual outcome means
  z_levels <- unique(data$Z)
  n_z <- length(z_levels)
  est <- matrix(NA, nrow = time_points + 1, ncol = n_z + 1)
  est[, 1] <- 0:time_points
  colnames(est) <- c('time', paste0('Z=', z_levels))
  data_baseline <- data[data$t0 == 0,]

  # Estimating counterfactual outcome means
  if (pooled){
    fit_Y <- stats::lm(formula = Y_model, data = data_censored, weights = weights)
    for (k in 0:time_points){
      data_temp <- data_baseline; data_temp$t0 <- k
      col_index <- 1
      for (z_val in z_levels){
        col_index <- col_index + 1
        data_temp$Z <- z_val
        est[k+1, col_index] <- mean(stats::predict(fit_Y, newdata = data_temp))
      }
    }
  } else {
    for (k in 0:time_points){
      fit_Y <- stats::lm(Y_model, data = data_censored[data_censored$t0 == k,],
                         weights = weights)
      data_temp <- data_baseline; data_temp$t0 <- k
      col_index <- 1
      for (z_val in z_levels){
        col_index <- col_index + 1
        data_temp$Z <- z_val
        est[k+1, col_index] <- mean(stats::predict(fit_Y, newdata = data_temp))
      }
    }
  }

  # Get all arguments supplied to the function, except the input data set
  args <- as.list(match.call())[-1]
  args$data <- NULL

  return(list(est = est, args = args))
}

