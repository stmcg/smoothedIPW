#' Pooled inverse probability weighting
#'
#' This function applies the pooled inverse probability weighted approach described by McGrath et al. (in preparation).
#'
#' @param df Data frame containing the observed data
#' @param pooled Logical scalar specifying whether the pooled or nonpooled IPW method is applied. The default is \code{TRUE}, i.e., the pooled IPW method.
#' @param A_model Model statement for the treatment variable
#' @param R_model_numerator Model statement for the indicator variable for the measurement of the outcome variable, used in the numerator of the IP weights
#' @param R_model_denominator Model statement for the indicator variable for the measurement of the outcome variable, used in the denominator of the IP weights
#' @param Y_model Model statement for the outcome variable
#' @param truncation_percentile Numerical scalar specifying the percentile by which to truncated the IP weights
#'
#' @return A list that includes the following components:
#' \item{est_z0}{A vector containing the counterfactual mean estimates under medication \eqn{Z=0}. The \eqn{k}th element in the vector corresponds to the \eqn{k}th follow-up time interval.}
#' \item{est_z1}{A vector containing the counterfactual mean estimates under medication \eqn{Z=1}. The \eqn{k}th element in the vector corresponds to the \eqn{k}th follow-up time interval.}
#'
#' @details
#' Additional description of the method
#'
#' @export

ipw <- function(df,
                pooled = TRUE,
                A_model,
                R_model_numerator,
                R_model_denominator,
                Y_model,
                truncation_percentile = NULL){
  # Set parameters
  time_points <- max(df$t0)

  # Fit models for the nuisance functions
  fit_A <- glm(A_model, family = 'binomial', data = df[df$G == 1,])
  fit_R_denominator <- glm(R_model_denominator, family = 'binomial', data = df)
  fit_R_numerator <- glm(R_model_numerator, family = 'binomial', data = df)

  # Artificially censor individuals when they deviate from the treatment strategy
  df_censored <- df[df$C_artificial == 0,]

  # Compute IP weights based on censored data set
  prob_A1 <- ifelse(df_censored$G == 1, predict(fit_A, type = 'response', newdata = df_censored), 1)
  prob_R1_denominator <- predict(fit_R_denominator, type = 'response', newdata = df_censored)
  prob_R1_numerator <- predict(fit_R_numerator, type = 'response', newdata = df_censored)
  weights_A <- unname(unlist(tapply(1 / prob_A1, df_censored$id, FUN = cumprod)))
  weights_R <- ifelse(df_censored$R == 1, prob_R1_numerator / prob_R1_denominator, 0)
  df_censored$weights <- weights_A * weights_R

  # Truncate IP weights, if applicable
  if (!is.null(truncation_percentile)){
    trunc_val <- quantile(df_censored$weights, probs = truncation_percentile)
    df_censored$weights <- pmin(df_censored$weights, truncation_percentile)
  }

  # Preparing data sets for estimating counterfactual outcome means
  est_z0 <- est_z1 <- rep(NA, times = time_points + 1)
  df_z0 <- df_z1 <- df[df$t0 == 0,]
  df_z0$Z <- 0; df_z1$Z <- 1

  # Estimating counterfactual outcome means
  if (pooled){
    fit_Y <- lm(formula = Y_model, data = df_censored, weights = weights)
    for (k in 0:time_points){
      df_z0_temp <- df_z0; df_z0_temp$t0 <- k
      df_z1_temp <- df_z1; df_z1_temp$t0 <- k
      est_z0[k+1] <- mean(predict(fit_Y, newdata = df_z0_temp))
      est_z1[k+1] <- mean(predict(fit_Y, newdata = df_z1_temp))
    }
  } else {
    for (k in 0:time_points){
      fit_Y <- lm(Y_model, data = df_censored[df_censored$t0 == k,],
                  weights = weights)
      df_z0_temp <- df_z0; df_z0_temp$t0 <- k
      df_z1_temp <- df_z1; df_z1_temp$t0 <- k
      est_z0[k+1] <- mean(predict(fit_Y, newdata = df_z0_temp))
      est_z1[k+1] <- mean(predict(fit_Y, newdata = df_z1_temp))
    }
  }

  # Get all arguments supplied to the function, except the input data set
  args <- as.list(match.call())[-1]
  args$df <- NULL

  return(list(est_z0 = est_z0, est_z1 = est_z1, args = args))
}

