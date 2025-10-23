#' Time-smoothed inverse probability weighting
#'
#' This function applies the time-smoothed inverse probability weighted (IPW) approach described by McGrath et al. (2025) to estimate effects of generalized time-varying treatment strategies on the mean of an outcome at one or more selected follow-up times of interest. Binary and continuous outcomes are supported.
#'
#' @param data Data table (or data frame) containing the observed data. See "Details".
#' @param time_smoothed Logical scalar specifying whether the time-smoothed or non-smoothed IPW method is applied. The default is \code{TRUE}, i.e., the time-smoothed IPW method.
#' @param smoothing_method Character string specify the time-smoothed IPW method when there are deaths present. The options include \code{"nonstacked"} and \code{"stacked"}. The default is \code{"nonstacked"}.
#' @param outcome_times Numeric vector specifying the follow-up time(s) of interest for the counterfactual outcome mean/probability
#' @param A_model Model statement for the treatment variable
#' @param R_model_numerator (Optional) Model statement for the indicator variable for the measurement of the outcome variable, used in the numerator of the IP weights
#' @param R_model_denominator Model statement for the indicator variable for the measurement of the outcome variable, used in the denominator of the IP weights
#' @param Y_model Model statement for the outcome variable
#' @param truncation_percentile Numerical scalar specifying the percentile by which to truncated the IP weights
#' @param include_baseline_outcome Logical scalar indicating whether to include the time interval indexed by 0 in fitting the time-smoothed outcome model and outcome measurement models. The default is \code{TRUE}.
#' @param return_model_fits Logical scalar specifying whether to include the fitted models in the output
#' @param return_weights Logical scalar specifying whether to return the estimated inverse probability weights
#' @param trim_returned_models Logical scalar specifying whether to only return the estimated coefficients (and corresponding standard errors, z scores, and p-values) of the fitted models (e.g., treatment model) rather than the full fitted model objects. This reduces the size of the object returned by the \code{ipw} function when \code{return_model_fits} is set to \code{TRUE}, especially when the observed data set is large. By default, this argument is set to \code{FALSE}.
#'
#' @return A object of class "ipw". This object is a list that includes the following components:
#' \item{est}{A data frame containing the counterfactual mean/probability estimates for each medication at each time interval.}
#' \item{model_fits}{A list containing the fitted models for the treatment, outcome measurement, and outcome (if \code{return_model_fits} is set to \code{TRUE}).
#'                   If the nonstacked time-smoothed appraoch is used, the \eqn{i}th element in \code{model_fits} is a list of fitted models for the \eqn{i}th outcome time in \code{outcome_times}.
#'                   If the stacked time-smoothed approach is used, the \eqn{i}th element in \code{model_fits} is a list of fitted models for to the outcome time \eqn{i+1} in the data set \code{data}. The last element in \code{model_fits} contains the fitted outcome model.}
#' \item{data_weights}{(A list containing) the artificially censored data set with columns for the estimated weights. The column \code{"weights"} contains the (final) inverse probability weight, and the columns \code{"weights_A"} and \code{"weights_R"} contain the inverse probability weights for treatment and outcome measurement, respectively.
#'                   If no deaths are present in the data, this object will be a data frame.
#'                   If deaths are present in the data and either the non-smoothed IPW method is applied or the time-smoothed non-stacked IPW method is applied, this object will be a list of length \code{length(outcome_times)} where each element corresponds to the artificially censored data set for each outcome time in \code{outcome_times}.
#'                   If deaths are present in the data and the time-smoothed stacked IPW method is applied, this object will be a data frame with the stacked, artificially censored data.}
#' \item{args}{A list containing the arguments supplied to \code{\link{ipw}}, except the observed data set.}
#'
#' @details
#'
#' \strong{Treatment strategies}
#'
#' Users can estimate effects of treatment strategies with the following components:
#' \itemize{
#'   \item Initiate treatment \eqn{z} at baseline
#'   \item Follow a user-specified time-varying adherence protocol for treatment \eqn{z}
#'   \item Ensure an outcome measurement at the follow-up time of interest.
#' }
#' The time-varying adherence protocol is specified by indicating in \code{data} when an individual deviates from their adherence protocol. The function \code{\link{prep_data}} facilitates this step. See also "Formating \code{data}".
#'
#' \strong{Formating \code{data}}
#'
#' The input data set \code{data} must be a data table (or data frame) in a "long" format, where each row represents one time interval for one individual. The data frame should contain the following columns:
#' \itemize{
#'   \item \code{id}: A unique identifier for each participant.
#'   \item \code{time}: The follow-up time index (e.g., 0, 1, 2, ...).
#'   \item Covariate columns: One or more columns for baseline and time-varying covariates.
#'   \item \code{Z}: The treatment initiated at baseline.
#'   \item \code{A}: An indicator for adherence to the treatment protocol at each time point.
#'   \item \code{R}: An indicator of whether the outcome was measured at that time point (1 for measured, 0 for not measured/censored).
#'   \item \code{Y}: The outcome variable, which can be binary or continuous.
#' }
#' To specify the intervention, the data set should additionally have the following columns:
#' \itemize{
#'   \item \code{C_artificial}: An indicator specifying when an individual should be artificially censored from the data due to violating the adherence protocol.
#'   \item \code{A_model_eligible}: An indicator specifying which records should be used for fitting the treatment adherence model.
#' }
#' The \code{\link{prep_data}} function facilitates adding these columns to the data set. Users may optionally include the following column for fitting the outcome measurement model:
#' \itemize{
#'   \item \code{R_model_denominator_eligible}: An indicator specifying which records should be used for fitting the outcome measurement model \code{R_model_denominator_eligible}.
#' }
#' Otherwise, the \code{R_model_denominator_eligible} is fit on all records on the artificially censored data set.
#'
#' \strong{Specifying the models}
#'
#' Users must specify model statements for the treatment (\code{A_model}), outcome measurement (\code{R_model_numerator} and \code{R_model_denominator}), and outcome variable (\code{Y_model}). The package uses pooled-over-time generalized linear models that are fit over the relevant time points (see "Formating \code{data}"), where logistic regression is used for binary variables and linear regression is used for continuous variables.
#'
#' For stabilized weights, the outcome measurement model \code{R_model_numerator} should \strong{only} include baseline covariates, treatment initiated \code{Z}, and \code{time} as predictors. It must not include time-varying covariates as predictors. The outcome model \code{Y_model} should also only depend on baseline covariates, treatment initiated \code{Z}, and \code{time} (if using time smoothing).
#'
#' \strong{A note on the outcome definition at baseline}
#'
#' In some settings, the outcome may not be defined in the baseline time interval. The \code{ipw} function can accomodate such settings in two ways:
#'
#' 1. Users can set a value of \code{NA} in the column \code{Y} in the input data set \code{data} in rows corresponding to time 0. In this case, users should set \code{include_baseline_outcome} to \code{FALSE}.
#'
#' 2. Users can specify the value of \eqn{Y_{t+1}} (rather than \eqn{Y_t}) in the column \code{Y} in the input data set \code{data} in rows corresponding to time \eqn{t}. That is, the value supplied for \code{Y} in the input data set \code{data} at time 0 is \eqn{Y_1}. In this case, users should set \code{include_baseline_outcome} to \code{TRUE}. Users should also set \code{outcome_times} accordingly.
#'
#' Note that these two approaches involve different assumptions. For example, the first approach allows the outcome at time \eqn{t} to depend on time-varying covariates up to and including time \eqn{t}, whereas the second approach only allows the outcome at time \eqn{t} to depend on covariates up to and including time \eqn{t-1}.
#'
#' @references
#' McGrath S, Kawahara T, Petimar J, Rifas-Shiman SL, Díaz I, Block JP, Young JG. (2025). Time-smoothed inverse probability weighted estimation of effects of generalized time-varying treatment strategies on repeated outcomes truncated by death. arXiv e-prints arXiv:2509.13971.
#'
#' @examples
#'
#' ## Time-smoothed IPW without deaths (continuous outcome)
#' data_null_processed <- prep_data(data = data_null, grace_period_length = 2,
#'                                  baseline_vars = 'L')
#' res <- ipw(data = data_null_processed,
#'            time_smoothed = TRUE,
#'            outcome_times = c(6, 12, 18, 24),
#'            A_model = A ~ L + Z,
#'            R_model_numerator = R ~ L_baseline + Z,
#'            R_model_denominator = R ~ L + A + Z,
#'            Y_model = Y ~ L_baseline * (time + Z))
#' res$est
#'
#' ## Time-smoothed IPW with deaths, nonstacked smoothing method (continuous outcome)
#' data_null_deaths_processed <- prep_data(data = data_null_deaths, grace_period_length = 2,
#'                                         baseline_vars = 'L')
#' res <- ipw(data = data_null_deaths_processed,
#'            time_smoothed = TRUE,
#'            smoothing_method = 'nonstacked',
#'            outcome_times = c(6, 12, 18, 24),
#'            A_model = A ~ L + Z,
#'            R_model_numerator = R ~ L_baseline + Z,
#'            R_model_denominator = R ~ L + A + Z,
#'            Y_model = Y ~ L_baseline * (time + Z))
#' res$est
#'
#' ## Time-smoothed IPW with deaths, stacked smoothing method (binary outcome)
#' \donttest{
#' data_null_deaths_binary_processed <- prep_data(data = data_null_deaths_binary,
#'                                                grace_period_length = 2,
#'                                                baseline_vars = 'L')
#' res <- ipw(data = data_null_deaths_binary_processed,
#'            time_smoothed = TRUE,
#'            smoothing_method = 'stacked',
#'            outcome_times = c(6, 12, 18, 24),
#'            A_model = A ~ L + Z,
#'            R_model_numerator = R ~ L_baseline + Z,
#'            R_model_denominator = R ~ L + A + Z,
#'            Y_model = Y ~ L_baseline * (time + Z))
#' res$est
#' }

#'
#' @export
ipw <- function(data,
                time_smoothed = TRUE,
                smoothing_method = 'nonstacked',
                outcome_times,
                A_model,
                R_model_numerator = NULL,
                R_model_denominator,
                Y_model,
                truncation_percentile = NULL,
                include_baseline_outcome = TRUE,
                return_model_fits = TRUE,
                return_weights = TRUE,
                trim_returned_models = FALSE){
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
  if (!'time' %in% colnames(data)){
    stop("The observed data must include a column called 'time' indicating the time interval.")
  }
  if (!'id' %in% colnames(data)){
    stop("The observed data must include a column called 'id' indicating the participant ID number.")
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
  if (!is.null(R_model_numerator)){
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
  if (!"time" %in% rhs & time_smoothed) {
    warning("Y_model does not include 'time' as a predictor although the time-smoothed IPW method is selected.")
  }
  if ("time" %in% rhs & !time_smoothed) {
    warning("Y_model should not include 'time' as a predictor since the non-smoothed IPW method is selected.")
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

  # Check person-time format of data
  if (!isTRUE(all.equal(sort(as.numeric(unique(data$id))),
                        1:length(unique(data$id))))){
    stop("Individual ID values (specified by the 'id' column in 'data') should range from 1 to n, where n denotes the number of unique individuals.")
  }
  problematic_ids <- tapply(data$time, data$id,
                            FUN = function(x){
                              !isTRUE(all.equal(x, 0:(length(x) - 1)))
                            })
  if (any(problematic_ids)){
    stop(paste0("For each individual, time intervals in the observed data (specified by the 'time' column in 'data') should begin with 0 and increase in increments of 1 in consecutive rows, where no time records are skipped. The data set includes ",
                sum(problematic_ids), ' individuals that do not follow this format correctly, including IDs ',
                paste(utils::head(names(problematic_ids)[problematic_ids]), collapse = ", "), '.'))
  }

  # Checking that suitable data processing was performed
  if (!'C_artificial' %in% colnames(data)){
    stop("The observed data must include a column called 'C_artificial' indicating when an individual should be artificially censored.")
  }
  if (!'A_model_eligible' %in% colnames(data)){
    stop("The observed data must include a column called 'A_model_eligible' indicating what records should be used for fitting the treatment adherence model.")
  }
  if (!'R_model_denominator_eligible' %in% colnames(data)){
    data$R_model_denominator_eligible <- !data$C_artificial
  }

  # Set parameters
  if (missing(outcome_times)){
    if (include_baseline_outcome){
      outcome_times <- 0:max(data$time)
    } else {
      outcome_times <- 1:max(data$time)
    }
  } else {
    if (!all(outcome_times %in% 0:max(data$time))){
      stop("All values in 'outcome_times' must be no greater than the maximum time interval in 'data' and no less than 0.")
    }
    if (!include_baseline_outcome & (0 %in% outcome_times)){
      stop("outcome_times cannot include 0 if include_baseline_outcome is set to FALSE.")
    }
  }
  any_deaths <- 'D' %in% colnames(data)
  if (is.factor(data$Y)){
    outcome_type <- 'binary'
  } else {
    outcome_type <- 'continuous'
    if (length(unique(data$Y)) == 2){
      warning("The outcome is treated as a continuous variable, although only two levels where detected.
              To treat the outcome as a binary variable, the column 'Y' in the observed data set should be a factor variable.")
    }
  }

  if (!any_deaths){
    # Time-smoothed/non-smoothed IPW without deaths
    res <- ipw_helper(data = data,
                      time_smoothed = time_smoothed,
                      outcome_times = outcome_times,
                      A_model = A_model,
                      R_model_numerator = R_model_numerator,
                      R_model_denominator = R_model_denominator,
                      Y_model = Y_model,
                      truncation_percentile = truncation_percentile,
                      include_baseline_outcome = include_baseline_outcome,
                      return_model_fits = return_model_fits,
                      return_weights = return_weights,
                      trim_returned_models = trim_returned_models,
                      outcome_type = outcome_type)
    est <- res$est
    model_fits <- res$model_fits
    data_weights <- res$data_weights
  } else {
    if (!time_smoothed){
      # Non-smoothed IPW with deaths
      if (return_model_fits){
        model_fits <- vector(mode = "list", length = length(outcome_times))
      } else {
        model_fits <- NULL
      }
      if (return_weights){
        data_weights <- vector(mode = "list", length = length(outcome_times))
      } else {
        data_weights <- NULL
      }

      i <- 1
      for (outcome_time in outcome_times){
        newdata <- data[data$time <= outcome_time,]
        ids_to_keep <- newdata[newdata$time == outcome_time & newdata$D == 0, ]$id
        newdata <- newdata[newdata$id %in% ids_to_keep, ]

        res_temp <- ipw_helper(data = newdata,
                               time_smoothed = FALSE,
                               outcome_times = outcome_time,
                               A_model = A_model,
                               R_model_numerator = R_model_numerator,
                               R_model_denominator = R_model_denominator,
                               Y_model = Y_model,
                               truncation_percentile = truncation_percentile,
                               include_baseline_outcome = include_baseline_outcome,
                               return_model_fits = return_model_fits,
                               return_weights = return_weights,
                               trim_returned_models = trim_returned_models,
                               outcome_type = outcome_type)
        if (i == 1){
          est <- res_temp$est
        } else {
          est <- rbind(est, res_temp$est)
        }
        if (return_model_fits){
          model_fits[[i]] <- res_temp$model_fits
        }
        if (return_weights){
          data_weights[[i]] <- res_temp$data_weights
        }
        i <- i + 1
      }
    } else {
      # IPW with deaths, nonstacked time-smoothing method
      if (smoothing_method == 'nonstacked'){
        if (return_model_fits){
          model_fits <- vector(mode = "list", length = length(outcome_times))
        } else {
          model_fits <- NULL
        }
        if (return_weights){
          data_weights <- vector(mode = "list", length = length(outcome_times))
        } else {
          data_weights <- NULL
        }

        i <- 1
        for (outcome_time in outcome_times){
          newdata <- data[data$time <= outcome_time,]
          ids_to_keep <- newdata[newdata$time == outcome_time & newdata$D == 0, ]$id
          newdata <- newdata[newdata$id %in% ids_to_keep, ]

          res_temp <- ipw_helper(data = newdata,
                                 time_smoothed = TRUE,
                                 outcome_times = outcome_time,
                                 A_model = A_model,
                                 R_model_numerator = R_model_numerator,
                                 R_model_denominator = R_model_denominator,
                                 Y_model = Y_model,
                                 truncation_percentile = truncation_percentile,
                                 include_baseline_outcome = include_baseline_outcome,
                                 return_model_fits = return_model_fits,
                                 return_weights = return_weights,
                                 trim_returned_models = trim_returned_models,
                                 outcome_type = outcome_type)
          if (i == 1){
            est <- res_temp$est
          } else {
            est <- rbind(est, res_temp$est)
          }
          if (return_model_fits){
            model_fits[[i]] <- res_temp$model_fits
          }
          if (return_weights){
            data_weights[[i]] <- res_temp$data_weights
          }
          i <- i + 1
        }
      } else if (smoothing_method == 'stacked'){
        # IPW with deaths, stacked time-smoothing method
        if (return_model_fits){
          model_fits <- vector(mode = "list", length = max(data$time) + 2)
        } else {
          model_fits <- NULL
        }

        # Step 1: Created stacked data set with weights
        if (include_baseline_outcome){
          start_val <- 0
        } else {
          start_val <- 1
        }
        for (j in start_val:max(data$time)){
          newdata <- data[data$time <= j,]
          ids_to_keep <- newdata[newdata$time == j & newdata$D == 0, ]$id
          newdata <- newdata[newdata$id %in% ids_to_keep, ]

          res_temp <- ipw_helper(data = newdata,
                                time_smoothed = TRUE,
                                outcome_times = j,
                                A_model = A_model,
                                R_model_numerator = R_model_numerator,
                                R_model_denominator = R_model_denominator,
                                Y_model = Y_model,
                                truncation_percentile = truncation_percentile,
                                include_baseline_outcome = include_baseline_outcome,
                                return_model_fits = return_model_fits,
                                return_weights = return_weights,
                                only_compute_weights = TRUE,
                                trim_returned_models = trim_returned_models,
                                outcome_type = outcome_type)
          df_stack <- res_temp$df_stack
          if (return_model_fits){
            model_fits[[j + 1]] <- res_temp$model_fits
          }
          if (j == start_val){
            dat_stacked <- df_stack
          } else {
            dat_stacked <- rbind(dat_stacked, df_stack)
          }
        }
        # Truncate IP weights, if applicable
        if (!is.null(truncation_percentile)){
          trunc_val <- stats::quantile(dat_stacked$weights, probs = truncation_percentile)
          dat_stacked$weights <- pmin(dat_stacked$weights, trunc_val)
        }
        if (return_weights){
          data_weights <- dat_stacked[, c('id', 'time', 'Z', 'weights', 'weights_A', 'weights_R')]
        } else {
          data_weights <- NULL
        }

        # Step 2: Fit weighted outcome model
        error_message_fit <- NULL
        fit_Y <- tryCatch(
          if (outcome_type == 'binary'){
            stats::glm(formula = Y_model, data = dat_stacked[dat_stacked$weights > 0,], family = stats::binomial(), weights = weights)
          } else {
            stats::glm(formula = Y_model, data = dat_stacked[dat_stacked$weights > 0,], family = stats::gaussian(), weights = weights)
          }
          ,
          error = function(e) {
            error_message_fit <<- paste0("Error in fitting the model for Y: ", conditionMessage(e))
            NULL
          }
        )
        if (return_model_fits){
          model_fits[[max(data$time) + 2]] <- trim_glm(fit_Y, trim_returned_models = trim_returned_models)
        }

        # Step 3: Estimating counterfactual outcome means
        z_levels <- sort(unique(data$Z))
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
            est[row_index, col_index] <- mean(stats::predict(fit_Y, type = 'response', newdata = data_temp))
          }
        }
      }
    }
  }

  args <- list(time_smoothed = time_smoothed,
               smoothing_method = smoothing_method,
               outcome_times = outcome_times,
               truncation_percentile = truncation_percentile,
               include_baseline_outcome = include_baseline_outcome,
               A_model = A_model,
               R_model_numerator = R_model_numerator,
               R_model_denominator = R_model_denominator,
               Y_model = Y_model)
  out <- list(est = est, model_fits = model_fits, data_weights = data_weights,
              outcome_type = outcome_type, args = args)
  class(out) <- 'ipw'
  return(out)
}



ipw_helper <- function(data,
                time_smoothed = TRUE,
                outcome_times,
                A_model,
                R_model_numerator,
                R_model_denominator,
                Y_model,
                truncation_percentile = NULL,
                include_baseline_outcome,
                return_model_fits = TRUE,
                return_weights = TRUE,
                only_compute_weights = FALSE,
                trim_returned_models,
                outcome_type){

  if (include_baseline_outcome){
    min_time <- 0
  } else {
    min_time <- 1
  }

  time_points <- length(outcome_times)

  # Artificially censor individuals when they deviate from the treatment strategy
  data_censored <- data[data$C_artificial == 0,]

  # Treatment model and weights
  fit_model_A <- nrow(data[data$A_model_eligible == 1,]) >= 1
  error_message_fit <- NULL
  if (fit_model_A){
    fit_A <- tryCatch(
      stats::glm(A_model, family = 'binomial', data = data[data$A_model_eligible == 1,]),
      error = function(e) {
        error_message_fit <<- paste0("Error in fitting the model for A: ", conditionMessage(e))
        NULL
      }
    )
    if (!is.null(error_message_fit)) {
      stop(error_message_fit)
    }
  } else {
    fit_A <- NULL
  }
  prob_A1 <- ifelse(data_censored$A_model_eligible == 1, stats::predict(fit_A, type = 'response', newdata = data_censored), 1)
  if (!return_model_fits){
    fit_A <- NULL
  } else {
    fit_A <- trim_glm(fit_A, trim_returned_models = trim_returned_models)
  }

  # Measurement model (denominator) and weights
  fit_R_denominator <- tryCatch(
    stats::glm(R_model_denominator, family = 'binomial', data = data[(data$R_model_denominator_eligible == 1) & (data$time >= min_time), ]),
    error = function(e) {
      error_message_fit <<- paste0("Error in fitting the model for R (denominator): ", conditionMessage(e))
      NULL
    }
  )
  if (!is.null(error_message_fit)) {
    stop(error_message_fit)
  }
  if (include_baseline_outcome){
    prob_R1_denominator <- stats::predict(fit_R_denominator, type = 'response', newdata = data_censored)
  } else {
    prob_R1_denominator <- rep(1, times = nrow(data_censored)) # Dummy values that will be overridden
    my_ind <- data_censored$time != 0
    prob_R1_denominator[my_ind] <- stats::predict(fit_R_denominator, type = 'response',
                                                  newdata = data_censored[my_ind, ])
  }
  if (!return_model_fits){
    fit_R_denominator <- NULL
  } else {
    fit_R_denominator <- trim_glm(fit_R_denominator, trim_returned_models = trim_returned_models)
  }

  # Measurement model (numerator) and weights
  if (!is.null(R_model_numerator)){
    fit_R_numerator <- tryCatch(
      stats::glm(R_model_numerator, family = 'binomial', data = data[data$time >= min_time, ]),
      error = function(e) {
        error_message_fit <<- paste0("Error in fitting the model for R (numerator): ", conditionMessage(e))
        NULL
      }
    )
    if (!is.null(error_message_fit)) {
      stop(error_message_fit)
    }
  } else {
    fit_R_numerator <- NULL
  }
  if (!is.null(R_model_numerator)){
    if (include_baseline_outcome){
      prob_R1_numerator <- stats::predict(fit_R_numerator, type = 'response', newdata = data_censored)
    } else {
      prob_R1_numerator <- rep(1, times = nrow(data_censored)) # Dummy values that will be overridden
      my_ind <- data_censored$time != 0
      prob_R1_numerator[my_ind] <- stats::predict(fit_R_numerator, type = 'response',
                                                    newdata = data_censored[my_ind, ])
    }
    if (!return_model_fits){
      fit_R_numerator <- NULL
    } else {
      fit_R_numerator <- trim_glm(fit_R_numerator, trim_returned_models = trim_returned_models)
    }
  } else {
    prob_R1_numerator <- rep(1, times = nrow(data_censored))
  }

  # Compute IP weights based on censored data set
  weights_A <- unname(unlist(tapply(1 / prob_A1, data_censored$id, FUN = cumprod)))
  weights_R <- ifelse(data_censored$R == 1, prob_R1_numerator / prob_R1_denominator, 0)
  if (!include_baseline_outcome){
    weights_A[data_censored$time == 0] <-
      weights_R[data_censored$time == 0] <- 0
  }
  data_censored$weights <- weights_A * weights_R
  if (return_weights){
    data_censored$weights_A <- weights_A
    data_censored$weights_R <- weights_R
  }

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
    trunc_val <- stats::quantile(data_censored[data_censored$time >= min_time]$weights, probs = truncation_percentile)
    data_censored$weights <- pmin(data_censored$weights, trunc_val)
  }

  # Preparing data sets for estimating counterfactual outcome means
  z_levels <- sort(unique(data$Z))
  n_z <- length(z_levels)
  est <- matrix(NA, nrow = time_points, ncol = n_z + 1)
  est[, 1] <- outcome_times
  colnames(est) <- c('time', paste0('Z=', z_levels))
  data_baseline <- data[data$time == 0,]

  # Estimating counterfactual outcome means
  row_index <- 0
  if (time_smoothed){
    fit_Y <- tryCatch(
      if (outcome_type == 'binary'){
        stats::glm(formula = Y_model, data = data_censored[data_censored$weights > 0,], family = stats::binomial(), weights = weights)
      } else {
        stats::glm(formula = Y_model, data = data_censored[data_censored$weights > 0,], family = stats::gaussian(), weights = weights)
      }
      ,
      error = function(e) {
        error_message_fit <<- paste0("Error in fitting the model for Y: ", conditionMessage(e))
        NULL
      }
    )
    if (!is.null(error_message_fit)) {
      stop(error_message_fit)
    }
    for (k in outcome_times){
      row_index <- row_index + 1
      data_temp <- data_baseline; data_temp$time <- k
      col_index <- 1
      for (z_val in z_levels){
        col_index <- col_index + 1
        data_temp$Z <- z_val
        est[row_index, col_index] <- mean(stats::predict(fit_Y, type = 'response', newdata = data_temp))
      }
    }
    if (return_model_fits){
      fit_Y <- trim_glm(fit_Y, trim_returned_models = trim_returned_models)
    }
  } else {
    if (return_model_fits){
      fit_Y_all <- vector(mode = "list", length = time_points)
    }
    for (k in outcome_times){
      row_index <- row_index + 1
      fit_Y <- tryCatch(
        if (outcome_type == 'binary'){
          stats::glm(Y_model, data = data_censored[data_censored$time == k & data_censored$weights > 0,],
                     family = stats::binomial(), weights = weights)
        } else {
          stats::glm(Y_model, data = data_censored[data_censored$time == k & data_censored$weights > 0,],
                     family = stats::gaussian(), weights = weights)
        }
        ,
        error = function(e) {
          error_message_fit <<- paste0(paste0("Error in fitting the model for Y at time ", k, ': '), conditionMessage(e))
          NULL
        }
      )
      if (!is.null(error_message_fit)) {
        stop(error_message_fit)
      }
      data_temp <- data_baseline; data_temp$time <- k
      col_index <- 1
      for (z_val in z_levels){
        col_index <- col_index + 1
        data_temp$Z <- z_val
        est[row_index, col_index] <- mean(stats::predict(fit_Y, type = 'response', newdata = data_temp))
      }
      if (return_model_fits){
        fit_Y_all[[k+1]] <- trim_glm(fit_Y, trim_returned_models = trim_returned_models)
      }
    }
  }

  if (return_model_fits){
    if (time_smoothed){
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
  if (return_weights){
    data_weights <- data_censored[, c('id', 'time', 'Z', 'weights', 'weights_A', 'weights_R')]
  } else {
    data_weights <- NULL
  }
  out <- list(est = as.data.frame(est), model_fits = model_fits, df_stack = NULL,
              data_weights = data_weights)

  return(out)
}

trim_glm <- function(fit, trim_returned_models) {
  if (trim_returned_models & !is.null(fit)){
    fit <- summary(fit)$coefficients
  }
  return(fit)
}
