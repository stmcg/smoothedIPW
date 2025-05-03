data_null_processed <- prep_data(data_null, grace_period_length = 2, baseline_vars = 'L')
data_null_deaths_processed <- prep_data(data_null_deaths, grace_period_length = 2, baseline_vars = 'L')
data_null_deaths_binary_processed <- prep_data(data_null_deaths_binary, grace_period_length = 2, baseline_vars = 'L')


z0_est_pooled_expected <- c(0.033695099,  0.029266727,  0.024838355,  0.020409982,  0.015981610,  0.011553237,  0.007124865,  0.002696493,
                            -0.001731880, -0.006160252, -0.010588624, -0.015016997, -0.019445369, -0.023873742, -0.028302114, -0.032730486,
                            -0.037158859, -0.041587231, -0.046015603, -0.050443976, -0.054872348, -0.059300721, -0.063729093, -0.068157465,
                            -0.072585838)

z1_est_pooled_expected <- c(-0.008793305, -0.013221677, -0.017650050, -0.022078422, -0.026506794, -0.030935167, -0.035363539, -0.039791912,
                            -0.044220284, -0.048648656, -0.053077029, -0.057505401, -0.061933773, -0.066362146, -0.070790518, -0.075218891,
                            -0.079647263, -0.084075635, -0.088504008, -0.092932380, -0.097360752, -0.101789125, -0.106217497, -0.110645870,
                            -0.115074242)

z0_est_nonpooled_expected <- c(0.10929112,  0.23990904,  0.78571503, -0.32424609,  0.69298737,  0.29323273,  0.17205791, -0.32695050, -0.46813946,
                               0.81456876,  0.22910089, -0.25499925, -0.86210545,  0.36676885, -0.08063944,  0.11322842,  0.14744050, -1.24314532,
                               0.01132552, -0.03306652, -0.40103043,  0.70248373,  0.07574034,  0.05158158, -1.14718357)

z1_est_nonpooled_expected <- c(0.185840821, -0.443993594, -0.467302219, -0.011671359, -0.005634188, -0.384221570,  0.276457679, -0.052784627,
                               -0.555980896, -0.538800399, -0.958010977,  0.243207064,  0.132182623,  0.179988606,  0.921852941, -0.301177430,
                               0.070512321, -0.007608560,  0.395291306,  0.245289319, -0.248197264,  0.257987460,  0.017138105, -0.781741115,
                               0.310778695)

z0_est_nonpooled_deaths_expected <- c(-0.20947195, -0.60893578,  0.38963582, -0.71206080,  0.62681535,  0.19091808, -0.87698878,  0.68578951, -0.93837714,
                                      0.15305808,  0.09398406,  0.50615184,  0.86904678, -0.84391865, -1.31521428, -0.11980941,  0.04747411, -0.88324593,
                                      -0.41366467,  0.29172002, -0.26816082, -1.06751216,  0.06539332, -1.32821068,  0.30842475)

z1_est_nonpooled_deaths_expected <- c(-0.01989214, -0.02824488, -0.03880921,  0.43047569,  0.09319434,  0.14552004, -0.42306956,  0.30305822,  0.26505640,
                                      -0.59472543, -0.05825298, -0.14408363,  0.77054805, -1.62764818, -0.24789924, -0.50745583, -1.59453630, -0.20333480,
                                      -1.12970074, -0.94285728,  0.74563265,  0.75655648, -0.06185027, -0.31699992, -0.55291111)

z0_est_pooled_nonstacked_expected <- c(-0.20947195, -0.49024347, -0.00748269, -0.24018517,  0.17027342,  0.23379703, -0.14762154,  0.07065138, -0.12322159,
                                       -0.15284321, -0.10105475, -0.03239595,  0.22010220, -0.17197681, -0.30905273, -0.32410626, -0.37381431, -0.45159340,
                                       -0.52423256, -0.49588908, -0.40802371, -0.43054952, -0.38579516, -0.51971676, -0.51599274)

z1_est_pooled_nonstacked_expected <- c(-0.01989214, -0.10187158,  0.08435726,  0.17678000,  0.32089975,  0.31053952, -0.03381910,  0.11958196,  0.04988746,
                                       -0.05938516, -0.01230337,  0.04049774,  0.27104083, -0.15196432, -0.20437889, -0.26838714, -0.43161221, -0.44961832,
                                       -0.55734341, -0.57021106, -0.44771261, -0.38629360, -0.30671550, -0.37295752, -0.39079743)

z0_est_pooled_stacked_expected <- c(-0.1256075, -0.1415623, -0.1583381, -0.1726759, -0.1878440, -0.2029616, -0.2181225, -0.2296790, -0.2444219, -0.2591431,
                                    -0.2731068, -0.2893403, -0.3055538, -0.3209863, -0.3371013, -0.3513650, -0.3663548, -0.3831016, -0.3970309, -0.4122596,
                                    -0.4255817, -0.4396022, -0.4551630, -0.4704076, -0.4862567)

z1_est_pooled_stacked_expected <- c(-0.06778918, -0.08330174, -0.09932255, -0.11377226, -0.12874549, -0.14367968, -0.15863273, -0.17140870, -0.18612664,
                                    -0.20083078, -0.21509877, -0.23066335, -0.24618546, -0.26124938, -0.27665898, -0.29106801, -0.30585401, -0.32150058,
                                    -0.33572864, -0.35058111, -0.36454954, -0.37886082, -0.39383506, -0.40864561, -0.42365818)

z0_est_pooled_stacked_binary_expected <- c(0.4894086, 0.4880697, 0.4866323, 0.4851527, 0.4837887, 0.4821243, 0.4805734, 0.4792522, 0.4779863, 0.4763626, 0.4750723,
                                           0.4736508, 0.4722858, 0.4707543, 0.4692018, 0.4674474, 0.4661334, 0.4646176, 0.4627687, 0.4614534, 0.4599904, 0.4586890,
                                           0.4577687, 0.4562335, 0.4542698)

z1_est_pooled_stacked_binary_expected <- c(0.5055721, 0.5042321, 0.5027964, 0.5013192, 0.4999546, 0.4982956, 0.4967473, 0.4954244, 0.4941562, 0.4925357, 0.4912436,
                                           0.4898223, 0.4884567, 0.4869266, 0.4853756, 0.4836248, 0.4823096, 0.4807945, 0.4789495, 0.4776329, 0.4761700, 0.4748674,
                                           0.4739431, 0.4724087, 0.4704489)

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
