data_null_processed <- prep_data(data_null, grace_period_length = 2, baseline_vars = 'L')
data_null_deaths_processed <- prep_data(data_null_deaths, grace_period_length = 2, baseline_vars = 'L')

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

z0_est_nonpooled_deaths_expected <- c(-0.20947195,  -0.60893578,  0.38963582, -0.71313073,  0.62601909,  0.17701877, -0.88596022,  0.68330028, -0.94222787,
                                      0.16280928,  0.10019350,  0.50532133,  0.87649099, -0.83882660, -1.31230167, -0.12469010,  0.05335536, -0.88485581,
                                      -0.41192932,  0.29922991, -0.25089479, -1.05685652,  0.06638825, -1.30516562,  0.30651255)

z1_est_nonpooled_deaths_expected <- c(-0.01989214, -0.02824488, -0.03880921,  0.42925198,  0.09342189,  0.15342348, -0.42012736,  0.30532485,  0.25839283,
                                      -0.59484269, -0.06031143, -0.14138420,  0.77925841, -1.62476874, -0.25750049, -0.50756147, -1.58298688, -0.18945883,
                                      -1.13905975, -0.95124549,  0.73964706,  0.75971246, -0.05470227, -0.32883955, -0.55431465)

z0_est_pooled_nonstacked_expected <- c(-0.20947195, -0.41236900, -0.14155255, -0.32246336, -0.11625118, -0.07567268, -0.16099631, -0.08369982, -0.21930321,
                                       -0.20623380, -0.18743577, -0.14778820, -0.07411653, -0.16506898, -0.25644493, -0.25059328, -0.19927372, -0.25571240,
                                       -0.27568179, -0.24953520, -0.25832054, -0.31073375, -0.32233092, -0.38590181, -0.37889485)

z1_est_pooled_nonstacked_expected <- c(-0.019892144, -0.042804355, -0.062309358,  0.085611876,  0.033818400,  0.016782376, -0.041466064, -0.027750889, -0.041672202,
                                       -0.109488003, -0.094093503, -0.067001577, -0.005532849, -0.142148489, -0.150705673, -0.194584814, -0.257196806, -0.254409271,
                                       -0.311127499, -0.325018083, -0.298957506, -0.268690714, -0.244281242, -0.241814674, -0.255096318)

z0_est_pooled_stacked_expected <- c(-0.2398914, -0.2409788, -0.2428356, -0.2425603, -0.2430395, -0.2434906, -0.2440017, -0.2410027, -0.2410642, -0.2411061,
                                    -0.2403580, -0.2420030, -0.2437031, -0.2446095, -0.2463441, -0.2459868, -0.2464878, -0.2491936, -0.2484589, -0.2493840,
                                    -0.2477947, -0.2470794, -0.2485220, -0.2495893, -0.2516465)

z1_est_pooled_stacked_expected <- c(-0.1811741, -0.1818121, -0.1829016, -0.1827401, -0.1830213, -0.1832859, -0.1835858, -0.1818261, -0.1818622, -0.1818868,
                                    -0.1814478, -0.1824131, -0.1834106, -0.1839425, -0.1849603, -0.1847507, -0.1850446, -0.1866323, -0.1862012, -0.1867440,
                                    -0.1858115, -0.1853918, -0.1862383, -0.1868645, -0.1880716)

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

test_that("nonpooled ipw (with deaths) gives correct point estimates", {
  res_nonpooled_deaths <- ipw(data = data_null_deaths_processed,
                       pooled = FALSE,
                       A_model = A ~ L + Z,
                       R_model_numerator = R ~ L_baseline + Z,
                       R_model_denominator = R ~ L + A + Z,
                       Y_model = Y ~ L_baseline * Z)

  expect_equal(res_nonpooled_deaths$est[, 'Z=0'], z0_est_nonpooled_deaths_expected, tolerance = 1e-7)
  expect_equal(res_nonpooled_deaths$est[, 'Z=1'], z1_est_nonpooled_deaths_expected, tolerance = 1e-7)
})

test_that("pooled, nonstacked ipw gives correct point estimates", {
  res_pooled_nonstacked <- ipw(data = data_null_deaths_processed,
                       pooled = TRUE,
                       pooling_method = 'nonstacked',
                       A_model = A ~ L + Z,
                       R_model_numerator = R ~ L_baseline + Z,
                       R_model_denominator = R ~ L + A + Z,
                       Y_model = Y ~ L_baseline * Z)

  expect_equal(res_pooled_nonstacked$est[, 'Z=0'], z0_est_pooled_nonstacked_expected, tolerance = 1e-7)
  expect_equal(res_pooled_nonstacked$est[, 'Z=1'], z1_est_pooled_nonstacked_expected, tolerance = 1e-7)
})

test_that("pooled, stacked ipw gives correct point estimates", {
  res_pooled_stacked <- ipw(data = data_null_deaths_processed,
                               pooled = TRUE,
                               pooling_method = 'stacked',
                               A_model = A ~ L + Z,
                               R_model_numerator = R ~ L_baseline + Z,
                               R_model_denominator = R ~ L + A + Z,
                               Y_model = Y ~ L_baseline * Z)

  expect_equal(res_pooled_stacked$est[, 'Z=0'], z0_est_pooled_stacked_expected, tolerance = 1e-6)
  expect_equal(res_pooled_stacked$est[, 'Z=1'], z1_est_pooled_stacked_expected, tolerance = 1e-6)
})
