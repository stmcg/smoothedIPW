#' Example dataset with null treatment effects
#'
#' A dataset consisting of 25,000 observations on 1,000 individuals over 25 time points. Each row in the dataset corresponds to the record of one individual at one time point.
#'
#' @docType data
#'
#' @format A data table with 25,000 rows and 7 variables:
#' \describe{
#'   \item{time}{Time index.}
#'   \item{id}{Unique identifier for each individual.}
#'   \item{L}{Binary time-varying covariate.}
#'   \item{Z}{Medication initiated at baseline.}
#'   \item{A}{Binary indicator of adhering to medication initiated at baseline.}
#'   \item{R}{Indicator if the outcome of interest is measured.}
#'   \item{Y}{Continuous outcome of interest.}
#' }
"data_null"
