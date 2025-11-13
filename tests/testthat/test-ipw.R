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

z0_est_smoothed_stacked_expected <- c(0.11924041038398, 0.0999053413943711, 0.0789259660043708, 0.0590362870316119, 0.0378637659353326, 0.0160994273014894, -0.00579565263748251, -0.027895117555884, -0.0502542521020279, -0.0724982620308653, -0.0947293891984816, -0.1171516965924, -0.13928822014726, -0.161568172047008, -0.184199742233888, -0.206859360408505, -0.229806591488653, -0.253157571088614, -0.27556389419079, -0.298185696622601, -0.319338223933292, -0.342150088232287, -0.362123612055558, -0.384409519120199, -0.407126800862725)

z1_est_smoothed_stacked_expected <- c(0.0985465761054786, 0.0785800885160858, 0.0573829220793237, 0.036767838893964, 0.0152245911329024, -0.00672706465132674, -0.0288159469608082, -0.0510477265096508, -0.0732726178706493, -0.0954476657112365, -0.117884305058661, -0.140586542759478, -0.162444432862936, -0.184724883745102, -0.207572504743411, -0.230381815292493, -0.253552927955065, -0.277214089911463, -0.299553651589524, -0.322184653187139, -0.342964316051731, -0.365860598107695, -0.385288970573369, -0.407575515542905, -0.430373933285862)

z0_est_smoothed_stacked_exclude_expected <- c(-0.00579565263748251, -0.13928822014726, -0.27556389419079, -0.407126800862725)
z1_est_smoothed_stacked_exclude_expected <- c(-0.0288159469608082, -0.162444432862936, -0.299553651589524, -0.430373933285862)

z0_est_smoothed_stacked_binary_expected <- c(0.51296975683981, 0.511259038479771, 0.50953301418121, 0.507821236604855, 0.506067736436188, 0.504297618983874, 0.502589137470423, 0.50077924569825, 0.498998533964249, 0.49735604602773, 0.495742276006735, 0.493941147929956, 0.4921616450611, 0.490253837064286, 0.488567872091272, 0.487118219607086, 0.485218762088925, 0.483337717518314, 0.481387220198517, 0.479232882022681, 0.477352845530906, 0.475307843255503, 0.473528951413156, 0.471372066910595, 0.469486940787829)

z1_est_smoothed_stacked_binary_expected <- c(0.500525184578862, 0.498792310290241, 0.497031976916193, 0.4953489242828, 0.493552477670515, 0.491746029798851, 0.490061439586866, 0.488205286868665, 0.486406984192114, 0.484814545147582, 0.483251238184782, 0.481422026663042, 0.479626757178218, 0.477666883138792, 0.475999045121323, 0.474626329497295, 0.472684425186545, 0.470770454967601, 0.46877607989136, 0.466541622273949, 0.464642378058321, 0.462550147115183, 0.460774548282716, 0.458558294382755, 0.456661860409339)

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
