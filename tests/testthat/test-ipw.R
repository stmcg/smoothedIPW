data_null_processed <- prep_data(data_null, grace_period_length = 2, baseline_vars = 'L')

z0_est_pooled_expected <- c(0.031534949,  0.027343466,  0.023151983,  0.018960500,  0.014769017,
                            0.010577534,  0.006386051,  0.002194568, -0.001996915, -0.006188398,
                            -0.010379881, -0.014571364, -0.018762847, -0.022954330, -0.027145813,
                            -0.031337296, -0.035528779, -0.039720262, -0.043911745, -0.048103228,
                            -0.052294711, -0.056486194, -0.060677677, -0.064869160, -0.069060643)

z1_est_pooled_expected <- c(-0.01192126, -0.01611274, -0.02030422, -0.02449570, -0.02868719, -0.03287867, -0.03707015, -0.04126164,
                            -0.04545312, -0.04964460, -0.05383609, -0.05802757, -0.06221905, -0.06641053, -0.07060202, -0.07479350,
                            -0.07898498, -0.08317647, -0.08736795, -0.09155943, -0.09575092, -0.09994240, -0.10413388, -0.10832536,
                            -0.11251685)

z0_est_nonpooled_expected <- c(0.109291117,  0.242196202,  0.791238126, -0.320970800,  0.692382888,  0.293595687,  0.169931201,
                              -0.334306817, -0.469619513,  0.811723956,  0.225452250, -0.251976811, -0.863918583,  0.365035448,
                              -0.079874027,  0.115322326,  0.148011923, -1.244113506,  0.009446923, -0.030368244, -0.399889527,
                              0.707131357,  0.077880922,  0.053675629, -1.139421998)

z1_est_nonpooled_expected <- c(0.185840821, -0.443490959, -0.464866890, -0.013478042, -0.004946504, -0.386224477,  0.279217008,
                              -0.057075144, -0.554268808, -0.545806639, -0.954148690,  0.241492006,  0.129839644,  0.177066443,
                              0.925731463, -0.303373625,  0.068389502, -0.009483802,  0.391594133,  0.245451883, -0.241449398,
                              0.258538137,  0.018108352, -0.783299935,  0.309880043)

test_that("pooled ipw gives correct point estimates", {
  res_pooled <- ipw(data = data_null_processed,
                    pooled = TRUE,
                    A_model = A ~ L + Z,
                    R_model_numerator = R ~ L_baseline + Z,
                    R_model_denominator = R ~ L + A + Z,
                    Y_model = Y ~ L_baseline * (time + Z))

  expect_equal(res_pooled$est[, 'Z=0'], z0_est_pooled_expected, tolerance = 1e-7)
  expect_equal(res_pooled$est[, 'Z=1'], z1_est_pooled_expected, tolerance = 1e-7)
})

test_that("pooled ipw gives the same result for limited outcome times", {
  outcome_times <- c(6, 12, 18, 24)
  res_pooled_limited <- ipw(data = data_null_processed,
                            pooled = TRUE,
                            outcome_times = outcome_times,
                            A_model = A ~ L + Z,
                            R_model_numerator = R ~ L_baseline + Z,
                            R_model_denominator = R ~ L + A + Z,
                            Y_model = Y ~ L_baseline * (time + Z))

  expect_equal(res_pooled_limited$est[, 'Z=0'], z0_est_pooled_expected[outcome_times + 1], tolerance = 1e-7)
  expect_equal(res_pooled_limited$est[, 'Z=1'], z1_est_pooled_expected[outcome_times + 1], tolerance = 1e-7)
})

test_that("pooled ipw doesn't fail with weight truncation and without stabilization", {
  expect_no_error(ipw(data = data_null_processed,
                      pooled = TRUE,
                      A_model = A ~ L + Z,
                      R_model_denominator = R ~ L + A + Z,
                      Y_model = Y ~ L_baseline * (time + Z),
                      truncation_percentile = 0.99))
})

test_that("nonpooled ipw gives correct point estimates", {
  res_nonpooled <- ipw(data = data_null_processed,
                       pooled = FALSE,
                       A_model = A ~ L + Z,
                       R_model_numerator = R ~ L_baseline + Z,
                       R_model_denominator = R ~ L + A + Z,
                       Y_model = Y ~ L_baseline * Z)

  expect_equal(res_nonpooled$est[, 'Z=0'], z0_est_nonpooled_expected, tolerance = 1e-7)
  expect_equal(res_nonpooled$est[, 'Z=1'], z1_est_nonpooled_expected, tolerance = 1e-7)
})
