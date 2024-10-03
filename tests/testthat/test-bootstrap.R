test_that("pooled ipw CI doesn't fail", {
  res_pooled <- ipw(data = data_null,
                    pooled = TRUE,
                    A_model = A ~ L + Z,
                    R_model_numerator = R ~ L0 + Z,
                    R_model_denominator = R ~ L + A + Z,
                    Y_model = Y ~ L0 * (t0 + Z))
  expect_no_error(get_CI(ipw_res = res_pooled, data = data_null, n_boot = 5))
})

test_that("nonpooled ipw CI doesn't fail", {
  res_nonpooled <- ipw(data = data_null,
                       pooled = FALSE,
                       A_model = A ~ L + Z,
                       R_model_numerator = R ~ L0 + Z,
                       R_model_denominator = R ~ L + A + Z,
                       Y_model = Y ~ L0 * Z)
  expect_no_error(get_CI(ipw_res = res_nonpooled, data = data_null, n_boot = 5))
})
