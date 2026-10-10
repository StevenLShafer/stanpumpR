test_that("returns the reduced enantiomer parameters with the switch off", {
  # R(-): CL 0.79, V 103; S(+): CL 0.35, V 57 (Burm 1997), half the racemate
  # each, reduced by hand to the equivalent two-compartment model.
  actual <- mepivacaine(70, 171, 50, "male", adjustToFFM = FALSE)
  expected <- list(
    PK = list(
      default = list(
        v1 = 73.3875,
        v2 = 0.775627116,
        v3 = 1,
        cl1 = 0.4850877193,
        cl2 = 0.005526343202,
        cl3 = 0,
        ka_RA = 0.0272,
        bioavailability_RA = 1,
        tlag_RA = 0
      )
    ),
    tPeak = 0,
    MEAC = 0,
    typical = 0,
    upperTypical = 0,
    lowerTypical = 0,
    reference = mepivacaine(70, 171, 50, "male")$reference
  )
  expect_equal_rounded(actual, expected)
})

test_that("scales to fat-free mass for a 120 kg man", {
  actual <- mepivacaine(120, 170, 50, "male")
  expected <- list(v1 = 95.76384045, v2 = 1.012121020, v3 = 1,
                   cl1 = 0.5922497086, cl2 = 0.006747182047, cl3 = 0)
  expect_equal_rounded(actual$PK$default[names(expected)], expected)
})

test_that("the two-compartment model is exactly the sum of the enantiomer pools", {
  PK <- getDrugPK("mepivacaine", 70, 170, 35, "male",
                  getDrugDefaultsGlobal()[getDrugDefaultsGlobal()$Drug == "mepivacaine", ])
  s <- PK$PK$default
  t <- c(1, 10, 60, 240)
  model <- s$p_coef_bolus_l1 * exp(-s$lambda_1 * t) + s$p_coef_bolus_l2 * exp(-s$lambda_2 * t)
  pools <- 0.5 / 103 * exp(-0.79 / 103 * t) + 0.5 / 57 * exp(-0.35 / 57 * t)
  expect_equal(model, pools, tolerance = 1e-10)
})
