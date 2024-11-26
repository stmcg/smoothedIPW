library('data.table')

expit <- function(x) {
  return(exp(x) / (1 + exp(x)))
}

datagen <- function(i, time_points, beta_L, beta_Z, beta_A, beta_R, beta_Y, sigma_Y, U_lb, U_ub, m){
  # Preallocate space in vectors
  time <- 0:time_points
  id <- rep(as.numeric(i), time_points+1)
  L <- rep(NA, time_points+1)
  Z <- rep(NA, time_points+1)
  A <- rep(NA, time_points+1)
  R <- rep(NA, time_points+1)
  Y <- rep(NA, time_points+1)

  # Baseline
  U <- runif(1, min = U_lb, max = U_ub)
  L[1] <- rbinom(1, 1, expit(beta_L[1] + beta_L[4] * U))
  Z[1:(time_points + 1)] <- rbinom(1, 1, expit(beta_Z[1] + beta_Z[2] * L[1]))
  A[1] <- 1
  R[1] <- rbinom(1, 1, expit(beta_R[1] + beta_R[2] * A[1] + beta_R[3] * Z[1] + beta_R[4] * L[1]))
  if (R[1] == 1){
    Y[1] <- rnorm(1, beta_Y[1] + beta_Y[2] * A[1] + beta_Y[3] * Z[1] + beta_Y[4] * L[1] + beta_Y[5] * U +
                    beta_Y[6] * Z[1] * A[1] + beta_Y[7] * Z[1] * L[1] + beta_Y[8] * L[1] * A[1], sigma_Y)
  }

  # Follow-up times
  for (j in 2:(time_points + 1)){
    L[j] <- rbinom(1, 1, expit(beta_L[1] + beta_L[2] * A[j-1] + beta_L[3] * Z[1] + beta_L[4] * U + beta_L[5] * (j - 1)))
    A[j] <- rbinom(1, 1, expit(beta_A[1] + beta_A[2] * A[j-1] +  beta_A[3] * Z[1] + beta_A[4] * L[j] + beta_A[5] * (j - 1)))
    R[j] <- rbinom(1, 1, expit(beta_R[1] + beta_R[2] * A[j] + beta_R[3] * Z[1] + beta_R[4] * L[j]))
    if (R[j] == 1){
      Y[j] <- rnorm(1, beta_Y[1] + beta_Y[2] * A[j] + beta_Y[3] * Z[1] + beta_Y[4] * L[j] + beta_Y[5] * U +
                      beta_Y[6] * Z[1] * A[j] - beta_Y[7] * Z[1] * L[j] + beta_Y[8] * L[j] * A[j] + beta_Y[9] * (j - 1), sigma_Y)
    }
  }

  # Consolidate data in a single data frame
  temp_data <- data.table(id = id, time = time, L = L, Z = Z,
                          A = A, R = R, Y = Y)
  return(temp_data)
}

datagen_with_deaths <- function(i, time_points, beta_L, beta_Z, beta_A, beta_R, beta_Y, beta_D, sigma_Y, U_lb, U_ub, m){
  # Preallocate space in vectors
  time <- 0:time_points
  id <- rep(as.numeric(i), time_points+1)
  L <- rep(NA, time_points+1)
  Z <- rep(NA, time_points+1)
  A <- rep(NA, time_points+1)
  R <- rep(NA, time_points+1)
  Y <- rep(NA, time_points+1)
  D <- rep(NA, time_points+1)

  # Baseline
  U <- runif(1, min = U_lb, max = U_ub)
  D[1] <- 0
  L[1] <- rbinom(1, 1, expit(beta_L[1] + beta_L[4] * U))
  Z[1:(time_points + 1)] <- rbinom(1, 1, expit(beta_Z[1] + beta_Z[2] * L[1]))
  A[1] <- 1
  R[1] <- rbinom(1, 1, expit(beta_R[1] + beta_R[2] * A[1] + beta_R[3] * Z[1] + beta_R[4] * L[1]))
  if (R[1] == 1){
    Y[1] <- rnorm(1, beta_Y[1] + beta_Y[2] * A[1] + beta_Y[3] * Z[1] + beta_Y[4] * L[1] + beta_Y[5] * U +
                    beta_Y[6] * Z[1] * A[1] + beta_Y[7] * Z[1] * L[1] + beta_Y[8] * L[1] * A[1], sigma_Y)
  }

  # Follow-up times
  for (j in 2:(time_points + 1)){
    D[j] <- rbinom(1, 1, expit(beta_D[1] + beta_D[2] * U))
    if (D[j] == 1){
      break
    }
    L[j] <- rbinom(1, 1, expit(beta_L[1] + beta_L[2] * A[j-1] + beta_L[3] * Z[1] + beta_L[4] * U + beta_L[5] * (j - 1)))
    A[j] <- rbinom(1, 1, expit(beta_A[1] + beta_A[2] * A[j-1] +  beta_A[3] * Z[1] + beta_A[4] * L[j] + beta_A[5] * (j - 1)))
    R[j] <- rbinom(1, 1, expit(beta_R[1] + beta_R[2] * A[j] + beta_R[3] * Z[1] + beta_R[4] * L[j]))
    if (R[j] == 1){
      Y[j] <- rnorm(1, beta_Y[1] + beta_Y[2] * A[j] + beta_Y[3] * Z[1] + beta_Y[4] * L[j] + beta_Y[5] * U +
                      beta_Y[6] * Z[1] * A[j] - beta_Y[7] * Z[1] * L[j] + beta_Y[8] * L[j] * A[j] + beta_Y[9] * (j - 1), sigma_Y)
    }
  }

  # Consolidate data in a single data frame
  temp_data <- data.table(id = id, time = time, L = L, Z = Z,
                          A = A, R = R, Y = Y, D = D)
  temp_data <- temp_data[!is.na(L), ]
  return(temp_data)
}

set.seed(1234)
data_null <- lapply(as.list(1:1000), FUN=function(ind){
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
data_null <- rbindlist(data_null)

usethis::use_data(data_null, overwrite = TRUE)


set.seed(2345)
data_null_deaths <- lapply(as.list(1:1000), FUN=function(ind){
  datagen_with_deaths(i = ind,
                      time_points = 24,
                      beta_L = c(0, 0.5, 0.25, 0.5, 0),
                      beta_Z = c(0.5, -1),
                      beta_A = c(0.5, 0.5, -0.25, 0.5, 0),
                      beta_R = c(-1, 0.5, 0.25, -0.5),
                      beta_Y = c(0, 0, 0, 0, 2, 0, 0, 0, 0),
                      beta_D = c(-4.5, 1),
                      sigma_Y = 5,
                      U_lb = -1,
                      U_ub = 1,
                      m = 2)
})
data_null_deaths <- rbindlist(data_null_deaths)

usethis::use_data(data_null_deaths, overwrite = TRUE)
