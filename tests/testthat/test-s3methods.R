data_null_processed <- prep_data(data_null, grace_period_length = 2, baseline_vars = 'L')
data_null_deaths_processed <- prep_data(data_null_deaths, grace_period_length = 2, baseline_vars = 'L')
data_null_deaths_binary_processed <- prep_data(data_null_deaths_binary, grace_period_length = 2, baseline_vars = 'L')

test_that("print.ipw doesn't fail for continuous outcomes without deaths", {
  res_smoothed_continuous <- ipw(data = data_null_processed,
                                 time_smoothed = TRUE,
                                 A_model = A ~ L + Z,
                                 R_model_numerator = R ~ L_baseline + Z,
                                 R_model_denominator = R ~ L + A + Z,
                                 Y_model = Y ~ L_baseline * (time + Z))
  expect_no_error(print(res_smoothed_continuous))
})

test_that("print.ipw doesn't fail for binary outcomes with deaths", {
  res_stacked <- ipw(data = data_null_deaths_binary_processed,
                     time_smoothed = TRUE,
                     smoothing_method = 'stacked',
                     A_model = A ~ L + Z,
                     R_model_numerator = R ~ L_baseline + Z,
                     R_model_denominator = R ~ L + A + Z,
                     Y_model = Y ~ L_baseline * (time + Z),
                     return_model_fits = FALSE,
                     return_weights = FALSE)
  expect_no_error(print(res_stacked))
})

test_that("print.ipw_ci doesn't fail for continuous outcomes without deaths", {
  set.seed(1234)
  res_smoothed_binary <- ipw(data = data_null_processed,
                             time_smoothed = FALSE,
                             outcome_times = c(6, 12, 18, 24),
                             A_model = A ~ L + Z,
                             R_model_numerator = R ~ L_baseline + Z,
                             R_model_denominator = R ~ L + A + Z,
                             Y_model = Y ~ L_baseline * (time + Z),
                             return_model_fits = FALSE,
                             return_weights = FALSE)
  res_ci <- get_CI(ipw_res = res_smoothed_binary, data = data_null_processed, n_boot = 5)
  expect_no_error(print(res_ci))
})
