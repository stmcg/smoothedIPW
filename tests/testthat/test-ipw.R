data_null_processed <- prep_data(data_null, grace_period_length = 2, baseline_vars = 'L')
data_null_deaths_processed <- prep_data(data_null_deaths, grace_period_length = 2, baseline_vars = 'L')
data_null_deaths_binary_processed <- prep_data(data_null_deaths_binary, grace_period_length = 2, baseline_vars = 'L')


z0_est_smoothed_expected <- c(0.033695099,  0.029266727,  0.024838355,  0.020409982,  0.015981610,  0.011553237,  0.007124865,  0.002696493,
                            -0.001731880, -0.006160252, -0.010588624, -0.015016997, -0.019445369, -0.023873742, -0.028302114, -0.032730486,
                            -0.037158859, -0.041587231, -0.046015603, -0.050443976, -0.054872348, -0.059300721, -0.063729093, -0.068157465,
                            -0.072585838)

z1_est_smoothed_expected <- c(-0.008793305, -0.013221677, -0.017650050, -0.022078422, -0.026506794, -0.030935167, -0.035363539, -0.039791912,
                            -0.044220284, -0.048648656, -0.053077029, -0.057505401, -0.061933773, -0.066362146, -0.070790518, -0.075218891,
                            -0.079647263, -0.084075635, -0.088504008, -0.092932380, -0.097360752, -0.101789125, -0.106217497, -0.110645870,
                            -0.115074242)

z0_est_smoothed_exclude_expected <- c(0.003639603,  0.001225031, -0.001189541, -0.003604113, -0.006018685, -0.008433257,
                                       -0.010847829, -0.013262401, -0.015676974, -0.018091546, -0.020506118, -0.022920690,
                                       -0.025335262, -0.027749834, -0.030164406, -0.032578978, -0.034993550, -0.037408122,
                                       -0.039822694, -0.042237266, -0.044651838, -0.047066411, -0.049480983, -0.051895555)

z1_est_smoothed_exclude_expected <- c(-0.04583356, -0.04824813, -0.05066270, -0.05307727, -0.05549184, -0.05790642, -0.06032099,
                                      -0.06273556, -0.06515013, -0.06756470, -0.06997928, -0.07239385, -0.07480842, -0.07722299,
                                      -0.07963756, -0.08205214, -0.08446671, -0.08688128, -0.08929585, -0.09171043, -0.09412500,
                                      -0.09653957, -0.09895414, -0.10136871)


z1_est_smoothed_exclude0_expected <- c(-0.008793305, -0.013221677, -0.017650050, -0.022078422, -0.026506794, -0.030935167, -0.035363539, -0.039791912,
                              -0.044220284, -0.048648656, -0.053077029, -0.057505401, -0.061933773, -0.066362146, -0.070790518, -0.075218891,
                              -0.079647263, -0.084075635, -0.088504008, -0.092932380, -0.097360752, -0.101789125, -0.106217497, -0.110645870,
                              -0.115074242)

z0_est_nonsmoothed_expected <- c(0.10929112,  0.23990904,  0.78571503, -0.32424609,  0.69298737,  0.29323273,  0.17205791, -0.32695050, -0.46813946,
                               0.81456876,  0.22910089, -0.25499925, -0.86210545,  0.36676885, -0.08063944,  0.11322842,  0.14744050, -1.24314532,
                               0.01132552, -0.03306652, -0.40103043,  0.70248373,  0.07574034,  0.05158158, -1.14718357)

z1_est_nonsmoothed_expected <- c(0.185840821, -0.443993594, -0.467302219, -0.011671359, -0.005634188, -0.384221570,  0.276457679, -0.052784627,
                               -0.555980896, -0.538800399, -0.958010977,  0.243207064,  0.132182623,  0.179988606,  0.921852941, -0.301177430,
                               0.070512321, -0.007608560,  0.395291306,  0.245289319, -0.248197264,  0.257987460,  0.017138105, -0.781741115,
                               0.310778695)

z0_est_nonsmoothed_deaths_expected <- c(0.1529172, 0.6075785, 0.8302382, 0.3906618, -0.3547536, 0.1245714, 0.1265027, -0.4130842, -0.3764367, -1.0304501,
                                      0.210773, -0.1886008, -0.4370287, -0.6085252, -0.5027678, 0.0679676, -0.1897028, 0.0546655, -0.6101373, -0.3539107,
                                      -0.8455759, -0.0832803, 0.5298722, -0.1103118, -0.5373807)

z1_est_nonsmoothed_deaths_expected <- c(0.1377707, -0.2205935, 0.3995058, 0.1386209, -0.1542105, 0.158589, -0.6044604, -0.0351985, -0.3417839, -0.6148372,
                                      0.2877484, 0.1010049, -0.3325362, -0.7112611, -0.113067, 0.3642725, -1.0165705, -0.0591755, -0.1825445, 0.7291569,
                                      -0.4350094, -0.3580729, -1.2195861, -1.5458772, 1.1251525)

z0_est_smoothed_nonstacked_expected <- c(0.1529172, 0.4224504, 0.800727, 0.5888571, 0.1087672, 0.1281464, 0.0321949, -0.0933615, -0.2018775, -0.3926769,
                                       -0.2895989, -0.3043059, -0.3322181, -0.3862993, -0.4243115, -0.3284608, -0.3937843, -0.3563559, -0.3843766, -0.3592989,
                                       -0.4199996, -0.4180559, -0.3890296, -0.4580703, -0.4463164)

z1_est_smoothed_nonstacked_expected <- c(0.1377707, 0.1744433, 0.4726687, 0.2320443, -0.0597527, -0.0598492, -0.2047087, -0.2722221, -0.3374018, -0.4813847,
                                       -0.3437971, -0.3425994, -0.3572665, -0.473782, -0.4761673, -0.3215609, -0.3827928, -0.3467382, -0.3460489, -0.235213,
                                       -0.2720144, -0.2690793, -0.2876055, -0.4171882, -0.3221055)

z0_est_smoothed_stacked_expected <- c(0.0658144, 0.0436669, 0.0199681, -0.0028293, -0.0268196, -0.0513495, -0.0760241, -0.1008861, -0.1258838, -0.1507927,
                                    -0.1758365, -0.2011272, -0.2258062, -0.2507915, -0.2762735, -0.3017498, -0.327577, -0.353887, -0.3789723, -0.4043315,
                                    -0.4278967, -0.453502, -0.4757005, -0.5006924, -0.5261938)

z1_est_smoothed_stacked_expected <- c(0.0657007, 0.0435466, 0.0198456, -0.0029593, -0.0269534, -0.0514853, -0.0761619, -0.1010252, -0.1260215, -0.1509297,
                                    -0.1759756, -0.2012692, -0.2259453, -0.2509307, -0.2764149, -0.3018927, -0.3277222, -0.3540354, -0.37912, -0.4044793,
                                    -0.4280407, -0.4536468, -0.4758398, -0.5008316, -0.5263338)

z0_est_smoothed_stacked_exclude_expected <- c(-0.0760241, -0.2258062, -0.3789723, -0.5261938)
z1_est_smoothed_stacked_exclude_expected <- c(-0.0761619, -0.2259453, -0.37912, -0.5263338)

z0_est_smoothed_stacked_binary_expected <- c(0.5044258, 0.5033667, 0.5022897, 0.501244, 0.5001383, 0.4990187, 0.4979748, 0.4968145, 0.4956901, 0.4947164, 0.4937702,
                                           0.4926234, 0.4915006, 0.4902454, 0.4892213, 0.4884389, 0.4871944, 0.4859701, 0.4846766, 0.4831787, 0.4819593, 0.4805748,
                                           0.479458, 0.4779636, 0.4767415)

z1_est_smoothed_stacked_binary_expected <- c(0.4749674, 0.4738961, 0.4728001, 0.4717703, 0.4706408, 0.4695011, 0.4684704, 0.4672846, 0.4661504, 0.4652044, 0.4642861,
                                           0.4631238, 0.4619923, 0.4607083, 0.4596942, 0.4589543, 0.4576864, 0.4564439, 0.4551261, 0.4535839, 0.4523538, 0.4509432,
                                           0.4498283, 0.448301, 0.4470727)

test_that("time-smoothed ipw gives correct point estimates", {
  res_smoothed <- ipw(data = data_null_processed,
                    time_smoothed = TRUE,
                    A_model = A ~ L + Z,
                    R_model_numerator = R ~ L_baseline + Z,
                    R_model_denominator = R ~ L + A + Z,
                    Y_model = Y ~ L_baseline * (time + Z))

  expect_equal(res_smoothed$est[, 'Z=0'], z0_est_smoothed_expected, tolerance = 1e-7)
  expect_equal(res_smoothed$est[, 'Z=1'], z1_est_smoothed_expected, tolerance = 1e-7)
})

test_that("time-smoothed ipw gives correct point estimates when excluding baseline outcome", {
  res_smoothed_exclude <- ipw(data = data_null_processed,
                      time_smoothed = TRUE,
                      A_model = A ~ L + Z,
                      R_model_numerator = R ~ L_baseline + Z,
                      R_model_denominator = R ~ L + A + Z,
                      Y_model = Y ~ L_baseline * (time + Z),
                      include_baseline_outcome = FALSE)

  expect_equal(res_smoothed_exclude$est[, 'Z=0'], z0_est_smoothed_exclude_expected, tolerance = 1e-7)
  expect_equal(res_smoothed_exclude$est[, 'Z=1'], z1_est_smoothed_exclude_expected, tolerance = 1e-7)
})

test_that("time-smoothed ipw gives the same result for limited outcome times", {
  outcome_times <- c(6, 12, 18, 24)
  res_smoothed_limited <- ipw(data = data_null_processed,
                            time_smoothed = TRUE,
                            outcome_times = outcome_times,
                            A_model = A ~ L + Z,
                            R_model_numerator = R ~ L_baseline + Z,
                            R_model_denominator = R ~ L + A + Z,
                            Y_model = Y ~ L_baseline * (time + Z))

  expect_equal(res_smoothed_limited$est[, 'Z=0'], z0_est_smoothed_expected[outcome_times + 1], tolerance = 1e-7)
  expect_equal(res_smoothed_limited$est[, 'Z=1'], z1_est_smoothed_expected[outcome_times + 1], tolerance = 1e-7)
})

test_that("time-smoothed ipw doesn't fail with weight truncation and without stabilization", {
  expect_no_error(ipw(data = data_null_processed,
                      time_smoothed = TRUE,
                      A_model = A ~ L + Z,
                      R_model_denominator = R ~ L + A + Z,
                      Y_model = Y ~ L_baseline * (time + Z),
                      truncation_percentile = 0.99))
})

test_that("non-smoothed ipw gives correct point estimates", {
  res_nonsmoothed <- ipw(data = data_null_processed,
                       time_smoothed = FALSE,
                       A_model = A ~ L + Z,
                       R_model_numerator = R ~ L_baseline + Z,
                       R_model_denominator = R ~ L + A + Z,
                       Y_model = Y ~ L_baseline * Z)

  expect_equal(res_nonsmoothed$est[, 'Z=0'], z0_est_nonsmoothed_expected, tolerance = 1e-7)
  expect_equal(res_nonsmoothed$est[, 'Z=1'], z1_est_nonsmoothed_expected, tolerance = 1e-7)
})

test_that("non-smoothed ipw (with deaths) gives correct point estimates", {
  res_nonsmoothed_deaths <- ipw(data = data_null_deaths_processed,
                              time_smoothed = FALSE,
                       A_model = A ~ L + Z,
                       R_model_numerator = R ~ L_baseline + Z,
                       R_model_denominator = R ~ L + A + Z,
                       Y_model = Y ~ L_baseline * Z)

  expect_equal(res_nonsmoothed_deaths$est[, 'Z=0'], z0_est_nonsmoothed_deaths_expected, tolerance = 1e-7)
  expect_equal(res_nonsmoothed_deaths$est[, 'Z=1'], z1_est_nonsmoothed_deaths_expected, tolerance = 1e-7)
})

test_that("time-smoothed, nonstacked ipw gives correct point estimates", {
  res_smoothed_nonstacked <- ipw(data = data_null_deaths_processed,
                               time_smoothed = TRUE,
                       smoothing_method = 'nonstacked',
                       A_model = A ~ L + Z,
                       R_model_numerator = R ~ L_baseline + Z,
                       R_model_denominator = R ~ L + A + Z,
                       Y_model = Y ~ L_baseline * (time + Z))

  expect_equal(res_smoothed_nonstacked$est[, 'Z=0'], z0_est_smoothed_nonstacked_expected, tolerance = 1e-7)
  expect_equal(res_smoothed_nonstacked$est[, 'Z=1'], z1_est_smoothed_nonstacked_expected, tolerance = 1e-7)
})

test_that("time-smoothed, stacked ipw gives correct point estimates", {
  res_smoothed_stacked <- ipw(data = data_null_deaths_processed,
                            time_smoothed = TRUE,
                            smoothing_method = 'stacked',
                               A_model = A ~ L + Z,
                               R_model_numerator = R ~ L_baseline + Z,
                               R_model_denominator = R ~ L + A + Z,
                               Y_model = Y ~ L_baseline * (time + Z))

  expect_equal(res_smoothed_stacked$est[, 'Z=0'], z0_est_smoothed_stacked_expected, tolerance = 1e-6)
  expect_equal(res_smoothed_stacked$est[, 'Z=1'], z1_est_smoothed_stacked_expected, tolerance = 1e-6)
})

test_that("time-smoothed, stacked ipw gives correct point estimates when excluding baseline outcome", {
  res_smoothed_stacked_exclude <- ipw(data = data_null_deaths_processed,
                              time_smoothed = TRUE,
                              outcome_times = c(6, 12, 18, 24),
                              smoothing_method = 'stacked',
                              A_model = A ~ L + Z,
                              R_model_numerator = R ~ L_baseline + Z,
                              R_model_denominator = R ~ L + A + Z,
                              Y_model = Y ~ L_baseline * (time + Z),
                              include_baseline_outcome = TRUE)

  expect_equal(res_smoothed_stacked_exclude$est[, 'Z=0'], z0_est_smoothed_stacked_exclude_expected, tolerance = 1e-6)
  expect_equal(res_smoothed_stacked_exclude$est[, 'Z=1'], z1_est_smoothed_stacked_exclude_expected, tolerance = 1e-6)
})

test_that("time-smoothed, stacked ipw gives correct point estimates (binary outcome)", {
  res_smoothed_stacked_binary <- ipw(data = data_null_deaths_binary_processed,
                            time_smoothed = TRUE,
                            smoothing_method = 'stacked',
                            A_model = A ~ L + Z,
                            R_model_numerator = R ~ L_baseline + Z,
                            R_model_denominator = R ~ L + A + Z,
                            Y_model = Y ~ L_baseline * (time + Z))

  expect_equal(res_smoothed_stacked_binary$est[, 'Z=0'], z0_est_smoothed_stacked_binary_expected, tolerance = 1e-6)
  expect_equal(res_smoothed_stacked_binary$est[, 'Z=1'], z1_est_smoothed_stacked_binary_expected, tolerance = 1e-6)
})
