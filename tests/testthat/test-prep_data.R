library('data.table')

expit <- function(x) {
  return(exp(x) / (1 + exp(x)))
}

datagen <- function(i, time_points, beta_L, beta_Z, beta_A, beta_R, beta_Y, sigma_Y, U_lb, U_ub, m){
  # Preallocate space in vectors
  time <- 0:time_points
  id <- rep(as.numeric(i), time_points+1)
  L <- rep(NA, time_points+1)
  L0 <- rep(NA, time_points+1)
  Z <- rep(NA, time_points+1)
  A <- rep(NA, time_points+1)
  lag1_A <- rep(NA, time_points+1)
  R <- rep(NA, time_points+1)
  Y <- rep(NA, time_points+1)
  C_artificial <- rep(NA, time_points+1)
  G <- rep(NA, time_points+1)
  U <- rep(NA, time_points+1)

  # Baseline
  U[1:(time_points + 1)] <- runif(1, min = U_lb, max = U_ub)
  L[1] <- rbinom(1, 1, expit(beta_L[1] + beta_L[4] * U[1]))
  L0[1:(time_points + 1)] <- L[1]
  Z[1:(time_points + 1)] <- rbinom(1, 1, expit(beta_Z[1] + beta_Z[2] * L[1]))
  A[1] <- 1
  R[1] <- rbinom(1, 1, expit(beta_R[1] + beta_R[2] * A[1] + beta_R[3] * Z[1] + beta_R[4] * L[1]))
  if (R[1] == 1){
    Y[1] <- rnorm(1, beta_Y[1] + beta_Y[2] * A[1] + beta_Y[3] * Z[1] + beta_Y[4] * L[1] + beta_Y[5] * U[1] +
                    beta_Y[6] * Z[1] * A[1] + beta_Y[7] * Z[1] * L[1] + beta_Y[8] * L[1] * A[1], sigma_Y)
  }
  lag1_A[1] <- 0
  C_artificial[1] <- ifelse(A[1] == 0, 1, 0)
  count_not_adhered <- 0
  G[1] <- 0


  # Follow-up times
  for (j in 2:(time_points + 1)){
    L[j] <- rbinom(1, 1, expit(beta_L[1] + beta_L[2] * A[j-1] + beta_L[3] * Z[1] + beta_L[4] * U[1] + beta_L[5] * (j - 1)))
    A[j] <- rbinom(1, 1, expit(beta_A[1] + beta_A[2] * A[j-1] +  beta_A[3] * Z[1] + beta_A[4] * L[j] + beta_A[5] * (j - 1)))
    R[j] <- rbinom(1, 1, expit(beta_R[1] + beta_R[2] * A[j] + beta_R[3] * Z[1] + beta_R[4] * L[j]))
    if (R[j] == 1){
      Y[j] <- rnorm(1, beta_Y[1] + beta_Y[2] * A[j] + beta_Y[3] * Z[1] + beta_Y[4] * L[j] + beta_Y[5] * U[1] +
                      beta_Y[6] * Z[1] * A[j] - beta_Y[7] * Z[1] * L[j] + beta_Y[8] * L[j] * A[j] + beta_Y[9] * (j - 1), sigma_Y)
    }

    count_not_adhered <- (count_not_adhered + 1) * (1 - A[j-1])
    G[j] <- ifelse(count_not_adhered == m, 1, 0)
    if ((A[j] == 0 & G[j] == 1) | C_artificial[j-1] == 1){
      C_artificial[j] <- 1
    } else {
      C_artificial[j] <- 0
    }
    lag1_A[j] <- A[j-1]
  }

  # Consolidate data in a single data frame
  temp_data <- data.table(id = id, time = time, L = L, L0 = L0, Z = Z,
                          A = A, lag1_A = lag1_A, R = R, Y = Y, C_artificial = C_artificial, G = G)
  return(temp_data)
}

set.seed(1234)
data_null_temp <- lapply(as.list(1:1000), FUN=function(ind){
  datagen(i = ind,
          time_points = 24,
          beta_L = c(0, 0.5, 0.25, 0.5, 0),
          beta_Z = c(0.5, -1),
          beta_A = c(0.5, 0.5, -0.25, 0.5, 0),
          beta_R = c(-1, 0.5, 0.25, -0.5),
          beta_Y = c(0, 0, 0, 0, 2, 0, 0, 0, 0),
          sigma_Y = 5,
          U_lb = -1,
          U_ub = 1,
          m = 2)
})
data_null_full <- rbindlist(data_null_temp)
data_null_processed <- prep_data(data_null, grace_period_length = 2,
                                 baseline_vars = 'L',
                                 lag_vars = 'A')

test_that("prep_data works", {
  expect_equal(data_null_processed$C_artificial, data_null_full$C_artificial, tolerance = 1e-7)
  expect_equal(data_null_processed$A_model_eligible, data_null_full$G, tolerance = 1e-7)
  expect_equal(data_null_processed$A_lag1, data_null_full$lag1_A, tolerance = 1e-7)
})

