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
        bioavailability_RA = 1,
        tlag_RA = 0,
        ka_RA = 0.1669714901,
        ka_RA_slow = 0.0049724280,
        fraction_RA_slow = 0.5278755306
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

test_that("RA doses go through a fast and a slow depot that share the dose", {
  dd <- getDrugDefaultsGlobal()
  s <- getDrugPK("mepivacaine", 70, 170, 35, "male", dd[dd$Drug == "mepivacaine", ])$PK$default
  expect_equal(s$ka_RA, 0.1669714901)
  expect_equal(s$ka_RAslow, 0.0049724280)
  expect_equal(s$bioavailability_RA, 0.4721244694, tolerance = 1e-10)
  expect_equal(s$bioavailability_RAslow, 0.5278755306, tolerance = 1e-10)
  expect_equal(s$tlag_RAslow, s$tlag_RA)

  # The handoff's reference simulator (RK4, 0.1 min steps) gives a peak of
  # 3.81 mcg/mL at 27.4 min after 600 mg with this input.
  w <- simCpCe(data.frame(Drug = "mepivacaine", Time = 0, Dose = 600, Units = "mg RA"),
               data.frame(Time = numeric(0), Event = character(0)),
               getDrugPK("mepivacaine", 70, 170, 35, "male", dd[dd$Drug == "mepivacaine", ]),
               180, FALSE)$wide
  expect_equal(max(w$Plasma), 3.81, tolerance = 0.005)
})
