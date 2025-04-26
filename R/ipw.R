#' Pooled inverse probability weighting
#'
#' This function applies the pooled inverse probability weighted (IPW) approach described by McGrath et al. (in preparation).
#'
#' @param data Data table (or data frame) containing the observed data
#' @param pooled Logical scalar specifying whether the pooled or nonpooled IPW method is applied. The default is \code{TRUE}, i.e., the pooled IPW method.
#' @param pooling_method Character string specify the pooled IPW method when there are deaths present. The options include \code{"nonstacked"} and \code{"stacked"}. The default is \code{"nonstacked"}.
#' @param outcome_times Numeric vector specifying the follow-up time(s) of interest for the counterfactual outcome mean. The default is all time points in \code{data}.
#' @param A_model Model statement for the treatment variable
#' @param R_model_numerator Model statement for the indicator variable for the measurement of the outcome variable, used in the numerator of the IP weights
#' @param R_model_denominator Model statement for the indicator variable for the measurement of the outcome variable, used in the denominator of the IP weights
#' @param Y_model Model statement for the outcome variable
#' @param truncation_percentile Numerical scalar specifying the percentile by which to truncated the IP weights
#' @param return_model_fits Logical scalar specifying whether to include the fitted models in the output
#'
#' @return A object of class "ipw". This object is a list that includes the following components:
#' \item{est}{A data frame containing the counterfactual mean estimates for each medication at each time interval.}
#' \item{model_fits}{A list containing the fitted models for the treatment, outcome measurement, and outcome (if \code{return_model_fits} is set to \code{TRUE}).
#'                   If the nonstacked pooled appraoch is used, the \eqn{i}th element in \code{model_fits} is a list of fitted models for the \eqn{i}th outcome time in \code{outcome_times}.
#'                   If the stacked pooled approach is used, the \eqn{i}th element in \code{model_fits} is a list of fitted models for to the outcome time \eqn{i+1} in the data set \code{data}. The last element in \code{model_fits} contains the fitted outcome model.}
#' \item{args}{A list containing the arguments supplied to \code{\link{ipw}}, except the observed data set.}
#'
#' @details
#' Additional description of the method
#'
#' @examples
#'
#' ## Pooled IPW without deaths
#' data_null_processed <- prep_data(data = data_null, grace_period_length = 2,
#'                                  baseline_vars = 'L')
#' res <- ipw(data = data_null_processed,
#'            pooled = TRUE,
#'            outcome_times = c(6, 12, 18, 24),
#'            A_model = A ~ L + Z,
#'            R_model_numerator = R ~ L_baseline + Z,
#'            R_model_denominator = R ~ L + A + Z,
#'            Y_model = Y ~ L_baseline * (time + Z))
#' res$est
#'
#' ## Pooled IPW with deaths, nonstacked pooling method
#' data_null_deaths_processed <- prep_data(data = data_null_deaths, grace_period_length = 2,
#'                                         baseline_vars = 'L')
#' res <- ipw(data = data_null_deaths_processed,
#'            pooled = TRUE,
#'            pooling_method = 'nonstacked',
#'            outcome_times = c(6, 12, 18, 24),
#'            A_model = A ~ L + Z,
#'            R_model_numerator = R ~ L_baseline + Z,
#'            R_model_denominator = R ~ L + A + Z,
#'            Y_model = Y ~ L_baseline * (time + Z))
#' res$est
#'
#' @export
ipw <- function(data,
                pooled = TRUE,
                pooling_method = 'nonstacked',
                outcome_times,
                A_model,
                R_model_numerator,
                R_model_denominator,
                Y_model,
                truncation_percentile = NULL,
                return_model_fits = TRUE){
  # Check input
  if (missing(data)){
    stop('The argument data must be specified')
  }
  if (missing(A_model)){
    stop('The argument A_model must be specified')
  }
  if (missing(R_model_denominator)){
    stop('The argument R_model_denominator must be specified')
  }
  if (missing(Y_model)){
    stop('The argument Y_model must be specified')
  }
  if (!is.null(truncation_percentile)){
    if (truncation_percentile < 0 | truncation_percentile > 1){
      stop('The argument truncation_percentile must be beween 0 and 1')
    }
  }

  # Check A_model
  if (!inherits(A_model, "formula")) {
    stop("A_model must be a formula, e.g., A ~ L + Z")
  }
  lhs <- all.vars(A_model[[2]]); rhs <- all.vars(A_model[[3]])
  if (length(lhs) != 1 || lhs != "A") {
    stop("The left-hand side of A_model must be the variable 'A'.")
  }
  if (!"Z" %in% rhs) {
    warning("A_model does not include 'Z' as a predictor.")
  }
  if ("D" %in% rhs) {
    stop("A_model should not include 'D' as a predictor.")
  }

  # Check R_model_numerator (if applicable)
  if (!missing(R_model_numerator)){
    if (!inherits(R_model_numerator, "formula")) {
      stop("R_model_numerator must be a formula, e.g., R ~ L_baseline + Z")
    }
    lhs <- all.vars(R_model_numerator[[2]]); rhs <- all.vars(R_model_numerator[[3]])
    if (length(lhs) != 1 || lhs != "R") {
      stop("The left-hand side of R_model_numerator must be the variable 'R'.")
    }
    if (!"Z" %in% rhs) {
      warning("R_model_numerator does not include 'Z' as a predictor.")
    }
    if ("D" %in% rhs) {
      stop("R_model_numerator should not include 'D' as a predictor.")
    }
  }

  # Check R_model_denominator
  if (!inherits(R_model_denominator, "formula")) {
    stop("R_model_denominator must be a formula, e.g., R ~ L + A + Z")
  }
  lhs <- all.vars(R_model_denominator[[2]]); rhs <- all.vars(R_model_denominator[[3]])
  if (length(lhs) != 1 || lhs != "R") {
    stop("The left-hand side of R_model_denominator must be the variable 'R'.")
  }
  if (!"Z" %in% rhs) {
    warning("R_model_denominator does not include 'Z' as a predictor.")
  }
  if ("D" %in% rhs) {
    stop("R_model_denominator should not include 'D' as a predictor.")
  }

  # Check Y_model
  if (!inherits(Y_model, "formula")) {
    stop("Y_model must be a formula, e.g., R ~ L_baseline + Z")
  }
  lhs <- all.vars(Y_model[[2]]); rhs <- all.vars(Y_model[[3]])
  if (length(lhs) != 1 || lhs != "Y") {
    stop("The left-hand side of Y_model must be the variable 'Y'.")
  }
  if (!"Z" %in% rhs) {
    warning("Y_model does not include 'Z' as a predictor.")
  }
  if (!"time" %in% rhs & pooled) {
    warning("Y_model does not include 'time' as a predictor although the pooled IPW method is selected.")
  }
  if ("A" %in% rhs) {
    stop("Y_model should not include 'A' (or any other post-baseline covariate) as a predictor.")
  }
  if ("R" %in% rhs) {
    stop("Y_model should not include 'R' (or any other post-baseline covariate) as a predictor.")
  }
  if ("D" %in% rhs) {
    stop("Y_model should not include 'D' as a predictor.")
  }

  # Checking that suitable data processing was performed
  if (!'C_artificial' %in% colnames(data)){
    stop("The observed data must include a column called 'C_artificial' indicating when an individual should be artificially censored.")
  }
  if (!'A_model_eligible' %in% colnames(data)){
    stop("The observed data must include a column called 'A_model_eligible' indicating what records should be used for fitting the treatment adherence model.")
  }

  # Set parameters
  if (missing(outcome_times)){
    outcome_times <- 0:max(data$time)
  }
  any_deaths <- 'D' %in% colnames(data)

  if (!any_deaths){
    # Pooled/nonpooled IPW without deaths
    res <- ipw_helper(data = data,
                      pooled = pooled,
                      outcome_times = outcome_times,
                      A_model = A_model,
                      R_model_numerator = R_model_numerator,
                      R_model_denominator = R_model_denominator,
                      Y_model = Y_model,
                      truncation_percentile = truncation_percentile,
                      return_model_fits = return_model_fits)
    est <- res$est
    model_fits <- res$model_fits
  } else {
    if (!pooled){
      # Nonpooled IPW with deaths
      if (return_model_fits){
        model_fits <- vector(mode = "list", length = length(outcome_times))
      } else {
        model_fits <- NULL
      }

      i <- 1
      for (outcome_time in outcome_times){
        newdata <- data[data$time <= outcome_time,]
        ids_to_keep <- newdata[newdata$time == outcome_time & newdata$D == 0, ]$id
        newdata <- newdata[newdata$id %in% ids_to_keep, ]

        res_temp <- ipw_helper(data = newdata,
                               pooled = FALSE,
                               outcome_times = outcome_time,
                               A_model = A_model,
                               R_model_numerator = R_model_numerator,
                               R_model_denominator = R_model_denominator,
                               Y_model = Y_model,
                               truncation_percentile = truncation_percentile,
                               return_model_fits = return_model_fits)
        if (i == 1){
          est <- res_temp$est
        } else {
          est <- rbind(est, res_temp$est)
        }
        if (return_model_fits){
          model_fits[[i]] <- res_temp$model_fits
        }
        i <- i + 1
      }
    } else {
      # IPW with deaths, nonstacked pooling method
      if (pooling_method == 'nonstacked'){
        if (return_model_fits){
          model_fits <- vector(mode = "list", length = length(outcome_times))
        } else {
          model_fits <- NULL
        }

        i <- 1
        for (outcome_time in outcome_times){
          newdata <- data[data$time <= outcome_time,]
          ids_to_keep <- newdata[newdata$time == outcome_time & newdata$D == 0, ]$id
          newdata <- newdata[newdata$id %in% ids_to_keep, ]

          res_temp <- ipw_helper(data = newdata,
                                 pooled = TRUE,
                                 outcome_times = outcome_time,
                                 A_model = A_model,
                                 R_model_numerator = R_model_numerator,
                                 R_model_denominator = R_model_denominator,
                                 Y_model = Y_model,
                                 truncation_percentile = truncation_percentile,
                                 return_model_fits = return_model_fits)
          if (i == 1){
            est <- res_temp$est
          } else {
            est <- rbind(est, res_temp$est)
          }
          if (return_model_fits){
            model_fits[[i]] <- res_temp$model_fits
          }
          i <- i + 1
        }
      } else if (pooling_method == 'stacked'){
        # IPW with deaths, stacked pooling method
        if (return_model_fits){
          model_fits <- vector(mode = "list", length = max(data$time) + 2)
        } else {
          model_fits <- NULL
        }

        # Step 1: Created stacked data set with weights
        for (j in 0:max(data$time)){
          newdata <- data[data$time <= j,]
          ids_to_keep <- newdata[newdata$time == j & newdata$D == 0, ]$id
          newdata <- newdata[newdata$id %in% ids_to_keep, ]

          res_temp <- ipw_helper(data = newdata,
                                pooled = TRUE,
                                outcome_times = j,
                                A_model = A_model,
                                R_model_numerator = R_model_numerator,
                                R_model_denominator = R_model_denominator,
                                Y_model = Y_model,
                                truncation_percentile = truncation_percentile,
                                return_model_fits = return_model_fits,
                                only_compute_weights = TRUE)
          df_stack <- res_temp$df_stack
          if (return_model_fits){
            model_fits[[j + 1]] <- res_temp$model_fits
          }
          if (j == 0){
            dat_stacked <- df_stack
          } else {
            dat_stacked <- rbind(dat_stacked, df_stack)
          }
        }

        # Step 2: Fit weighted outcome model
        fit_Y <- stats::lm(formula = Y_model, data = dat_stacked, weights = weights)
        if (return_model_fits){
          model_fits[[max(data$time) + 2]] <- fit_Y
        }

        # Step 3: Estimating counterfactual outcome means
        z_levels <- unique(data$Z)
        n_z <- length(z_levels)
        est <- matrix(NA, nrow = length(outcome_times), ncol = n_z + 1)
        est[, 1] <- outcome_times
        colnames(est) <- c('time', paste0('Z=', z_levels))
        data_baseline <- data[data$time == 0,]

        row_index <- 0
        for (outcome_time in outcome_times){
          row_index <- row_index + 1

          data_temp <- data_baseline
          data_temp$time <- outcome_time
          ids_to_keep <- data[data$time == outcome_time & data$D == 0, ]$id
          data_temp <- data_temp[data_temp$id %in% ids_to_keep, ]

          col_index <- 1
          for (z_val in z_levels){
            col_index <- col_index + 1
            data_temp$Z <- z_val
            est[row_index, col_index] <- mean(stats::predict(fit_Y, newdata = data_temp))
          }
        }
      }
    }
  }

  # Get all arguments supplied to the function, except the input data set
  args <- as.list(match.call())[-1]
  args$outcome_times <- outcome_times
  args$data <- NULL

  out <- list(est = est, args = args, model_fits = model_fits)
  class(out) <- 'ipw'
  return(out)
}



ipw_helper <- function(data,
                pooled = TRUE,
                outcome_times,
                A_model,
                R_model_numerator,
                R_model_denominator,
                Y_model,
                truncation_percentile = NULL,
                return_model_fits = TRUE,
                only_compute_weights = FALSE){

  time_points <- length(outcome_times)

  # Fit models for the nuisance functions
  fit_model_A <- nrow(data[data$A_model_eligible == 1,]) >= 1
  if (fit_model_A){
    fit_A <- stats::glm(A_model, family = 'binomial', data = data[data$A_model_eligible == 1,])
  } else {
    warning('There are no records that can be used to fit the treatment adherence model.')
    fit_A <- NULL
  }
  fit_R_denominator <- stats::glm(R_model_denominator, family = 'binomial', data = data)
  if (!missing(R_model_numerator)){
    fit_R_numerator <- stats::glm(R_model_numerator, family = 'binomial', data = data)
  } else {
    fit_R_numerator <- NULL
  }

  # Artificially censor individuals when they deviate from the treatment strategy
  data_censored <- data[data$C_artificial == 0,]

  # Compute IP weights based on censored data set
  prob_A1 <- ifelse(data_censored$A_model_eligible == 1, stats::predict(fit_A, type = 'response', newdata = data_censored), 1)
  prob_R1_denominator <- stats::predict(fit_R_denominator, type = 'response', newdata = data_censored)
  if (!missing(R_model_numerator)){
    prob_R1_numerator <- stats::predict(fit_R_numerator, type = 'response', newdata = data_censored)
  } else {
    prob_R1_numerator <- rep(1, times = nrow(data_censored))
  }
  weights_A <- unname(unlist(tapply(1 / prob_A1, data_censored$id, FUN = cumprod)))
  weights_R <- ifelse(data_censored$R == 1, prob_R1_numerator / prob_R1_denominator, 0)
  data_censored$weights <- weights_A * weights_R

  # End function if only needing weights
  if (only_compute_weights){
    if (return_model_fits){
      model_fits <- list(fit_A = fit_A,
                         fit_R_denominator = fit_R_denominator,
                         fit_R_numerator = fit_R_numerator)
    } else {
      model_fits <- NULL
    }
    return(list(est = NULL, model_fits = model_fits, df_stack = data_censored))
  }

  # Truncate IP weights, if applicable
  if (!is.null(truncation_percentile)){
    trunc_val <- stats::quantile(data_censored$weights, probs = truncation_percentile)
    data_censored$weights <- pmin(data_censored$weights, truncation_percentile)
  }

  # Preparing data sets for estimating counterfactual outcome means
  z_levels <- unique(data$Z)
  n_z <- length(z_levels)
  est <- matrix(NA, nrow = time_points, ncol = n_z + 1)
  est[, 1] <- outcome_times
  colnames(est) <- c('time', paste0('Z=', z_levels))
  data_baseline <- data[data$time == 0,]

  # Estimating counterfactual outcome means
  row_index <- 0
  if (pooled){
    fit_Y <- stats::lm(formula = Y_model, data = data_censored, weights = weights)
    for (k in outcome_times){
      row_index <- row_index + 1
      data_temp <- data_baseline; data_temp$time <- k
      col_index <- 1
      for (z_val in z_levels){
        col_index <- col_index + 1
        data_temp$Z <- z_val
        est[row_index, col_index] <- mean(stats::predict(fit_Y, newdata = data_temp))
      }
    }
  } else {
    if (return_model_fits){
      fit_Y_all <- vector(mode = "list", length = time_points)
    }
    for (k in outcome_times){
      row_index <- row_index + 1
      fit_Y <- stats::lm(Y_model, data = data_censored[data_censored$time == k,],
                         weights = weights)
      data_temp <- data_baseline; data_temp$time <- k
      col_index <- 1
      for (z_val in z_levels){
        col_index <- col_index + 1
        data_temp$Z <- z_val
        est[row_index, col_index] <- mean(stats::predict(fit_Y, newdata = data_temp))
      }
      if (return_model_fits){
        fit_Y_all[[k+1]] <- fit_Y
      }
    }
  }

  if (return_model_fits){
    if (pooled){
      model_fits <- list(fit_A = fit_A,
                         fit_R_denominator = fit_R_denominator,
                         fit_R_numerator = fit_R_numerator,
                         fit_Y = fit_Y)
    } else {
      model_fits <- list(fit_A = fit_A,
                         fit_R_denominator = fit_R_denominator,
                         fit_R_numerator = fit_R_numerator,
                         fit_Y = fit_Y_all)
    }
  } else {
    model_fits <- NULL
  }
  out <- list(est = as.data.frame(est), model_fits = model_fits, df_stack = NULL)

  return(out)
}

