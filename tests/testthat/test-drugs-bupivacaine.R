test_that("returns the published parameters with the switch off", {
  actual <- bupivacaine(70, 171, 50, "male", adjustToFFM = FALSE)
  expected <- list(
    PK = list(
      default = list(
        v1 = 33,
        v2 = 35,
        v3 = 1,
        cl1 = 0.52,
        cl2 = 0.3208219829,   # reconstructed Q, from the research handoff
        cl3 = 0,
        ka_RA = 0.0187,
        bioavailability_RA = 1,
        tlag_RA = 0
      )
    ),
    tPeak = 0,
    MEAC = 0,
    typical = 0,
    upperTypical = 0,
    lowerTypical = 0,
    reference = bupivacaine(70, 171, 50, "male")$reference
  )
  expect_equal_rounded(actual, expected)
})

test_that("scales to fat-free mass for a 120 kg man", {
  # volumes x 1.3049067, clearances x 1.2209126 (docs/adding-a-drug.md)
  actual <- bupivacaine(120, 170, 50, "male")
  expected <- list(v1 = 43.0619211, v2 = 45.6717345, v3 = 1,
                   cl1 = 0.634874552, cl2 = 0.3916956013, cl3 = 0,
                   ka_RA = 0.0187)
  expect_equal_rounded(actual$PK$default[names(expected)], expected)
})

test_that("keeps the intravenous terminal half-life and Vss", {
  PK <- getDrugPK("bupivacaine", 70, 170, 35, "male",
                  getDrugDefaultsGlobal()[getDrugDefaultsGlobal()$Drug == "bupivacaine", ])
  s <- PK$PK$default
  expect_equal(log(2) / min(s$lambda_1, s$lambda_2), 143, tolerance = 1e-8)
  expect_equal(s$v1 + s$v2, 68, tolerance = 1e-8)
  expect_equal(s$ke0, 0)
})
