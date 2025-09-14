data_null_processed <- prep_data(data_null, grace_period_length = 2, baseline_vars = 'L')

test_that("time-smoothed ipw CI doesn't fail", {
  res_smoothed <- ipw(data = data_null_processed,
                    time_smoothed = TRUE,
                    A_model = A ~ L + Z,
                    R_model_numerator = R ~ L_baseline + Z,
                    R_model_denominator = R ~ L + A + Z,
                    Y_model = Y ~ L_baseline * (time + Z))
  expect_no_error(get_CI(ipw_res = res_smoothed, data = data_null_processed, n_boot = 5))
})

test_that("non-smoothed ipw CI doesn't fail", {
  res_nonsmoothed <- ipw(data = data_null_processed,
                       time_smoothed = FALSE,
                       A_model = A ~ L + Z,
                       R_model_numerator = R ~ L_baseline + Z,
                       R_model_denominator = R ~ L + A + Z,
                       Y_model = Y ~ L_baseline * Z)
  expect_no_error(get_CI(ipw_res = res_nonsmoothed, data = data_null_processed, n_boot = 5))
})
