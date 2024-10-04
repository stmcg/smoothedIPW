data_null_processed <- prep_data(data_null, grace_period_length = 2, baseline_vars = 'L')

test_that("pooled ipw CI doesn't fail", {
  res_pooled <- ipw(data = data_null_processed,
                    pooled = TRUE,
                    A_model = A ~ L + Z,
                    R_model_numerator = R ~ L_baseline + Z,
                    R_model_denominator = R ~ L + A + Z,
                    Y_model = Y ~ L_baseline * (t0 + Z))
  expect_no_error(get_CI(ipw_res = res_pooled, data = data_null_processed, n_boot = 5))
})

test_that("nonpooled ipw CI doesn't fail", {
  res_nonpooled <- ipw(data = data_null_processed,
                       pooled = FALSE,
                       A_model = A ~ L + Z,
                       R_model_numerator = R ~ L_baseline + Z,
                       R_model_denominator = R ~ L + A + Z,
                       Y_model = Y ~ L_baseline * Z)
  expect_no_error(get_CI(ipw_res = res_nonpooled, data = data_null_processed, n_boot = 5))
})
