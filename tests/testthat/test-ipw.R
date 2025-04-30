data_null_processed <- prep_data(data_null, grace_period_length = 2, baseline_vars = 'L')
data_null_deaths_processed <- prep_data(data_null_deaths, grace_period_length = 2, baseline_vars = 'L')
data_null_deaths_binary_processed <- prep_data(data_null_deaths_binary, grace_period_length = 2, baseline_vars = 'L')


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

z0_est_pooled_nonstacked_expected <- c(-0.20947195, -0.49024347, -0.00748269, -0.24164239,  0.16931754,  0.22974005, -0.15321854,  0.06409311,
                                       -0.13005127, -0.15667018, -0.10152610, -0.03341317,  0.22103102, -0.16762584, -0.30597952, -0.32178331,
                                       -0.36936366, -0.44678523, -0.51994721, -0.49101131, -0.40311585, -0.42475215, -0.38166340, -0.51486520,
                                       -0.51240921)

z1_est_pooled_nonstacked_expected <- c(-0.019892144, -0.101871581,  0.084357263,  0.177066591,  0.321716882,  0.312231351, -0.032906233,
                                       0.118439512,  0.049822400, -0.058243698, -0.008859516,  0.044112930,  0.277070169, -0.144464899,
                                       -0.199247312, -0.264312348, -0.423659528, -0.441419263, -0.549740917, -0.563930729, -0.442648241,
                                       -0.382168504, -0.302273768, -0.368950706, -0.387293111)

z0_est_pooled_stacked_expected <- c(-0.1276283, -0.1433089, -0.1598122, -0.1738726, -0.1887649, -0.2036066, -0.2184916, -0.2297657, -0.2442321,
                                    -0.2586767, -0.2723626, -0.2883220, -0.3042612, -0.3194180, -0.3352583, -0.3492446, -0.3639579, -0.3804306,
                                    -0.3940819, -0.4090342, -0.4220777, -0.4358204, -0.4511052, -0.4660732, -0.4816462)

z1_est_pooled_stacked_expected <- c(-0.06730221, -0.08253691, -0.09827873, -0.11245201, -0.12714780, -0.14180451, -0.15647993, -0.16898388,
                                    -0.18342506, -0.19785246, -0.21184516, -0.22712997, -0.24237198, -0.25715716, -0.27228614, -0.28641890,
                                    -0.30092673, -0.31628987, -0.33024245, -0.34481540, -0.35851065, -0.37254667, -0.38723987, -0.40177023,
                                    -0.41649961)

z0_est_pooled_stacked_binary_expected <- c(0.2867749, 0.2856037, 0.2843047, 0.2829766, 0.2817799, 0.2803123, 0.2789753, 0.2778298, 0.2767138,
                                           0.2753450, 0.2742037, 0.2729753, 0.2717795, 0.2704913, 0.2691981, 0.2678112, 0.2666467, 0.2653856,
                                           0.2639812, 0.2628156, 0.2615856, 0.2604200, 0.2593965, 0.2581328, 0.2567171)

z1_est_pooled_stacked_binary_expected <- c(0.2767358, 0.2755453, 0.2742768, 0.2729904, 0.2717842, 0.2704116, 0.2691188, 0.2679439, 0.2667875,
                                           0.2654745, 0.2643026, 0.2630766, 0.2618710, 0.2606076, 0.2593410, 0.2580153, 0.2568292, 0.2555823,
                                           0.2542447, 0.2530578, 0.2518303, 0.2506437, 0.2495477, 0.2482989, 0.2469529)

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
                       Y_model = Y ~ L_baseline * (time + Z))

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
                               Y_model = Y ~ L_baseline * (time + Z))

  expect_equal(res_pooled_stacked$est[, 'Z=0'], z0_est_pooled_stacked_expected, tolerance = 1e-6)
  expect_equal(res_pooled_stacked$est[, 'Z=1'], z1_est_pooled_stacked_expected, tolerance = 1e-6)
})

test_that("pooled, stacked ipw gives correct point estimates (binary outcome)", {
  res_pooled_stacked_binary <- ipw(data = data_null_deaths_binary_processed,
                            pooled = TRUE,
                            pooling_method = 'stacked',
                            A_model = A ~ L + Z,
                            R_model_numerator = R ~ L_baseline + Z,
                            R_model_denominator = R ~ L + A + Z,
                            Y_model = Y ~ L_baseline * (time + Z))

  expect_equal(res_pooled_stacked_binary$est[, 'Z=0'], z0_est_pooled_stacked_binary_expected, tolerance = 1e-6)
  expect_equal(res_pooled_stacked_binary$est[, 'Z=1'], z1_est_pooled_stacked_binary_expected, tolerance = 1e-6)
})
