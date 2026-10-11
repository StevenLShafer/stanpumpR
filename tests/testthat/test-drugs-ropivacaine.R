test_that("returns the published parameters with the switch off", {
  actual <- ropivacaine(70, 171, 50, "male", adjustToFFM = FALSE)
  expected <- list(
    PK = list(
      default = list(
        v1 = 7,
        v2 = 12.6,
        v3 = 28.7,
        cl1 = 0.3,
        cl2 = 0.9666666667,
        cl3 = 0.4,
        ka_RA = 0.00612,
        bioavailability_RA = 1,
        tlag_RA = 0
      )
    ),
    tPeak = 0,
    MEAC = 0,
    typical = 0,
    upperTypical = 0,
    lowerTypical = 0,
    reference = ropivacaine(70, 171, 50, "male")$reference
  )
  expect_equal_rounded(actual, expected)
})

test_that("scales to fat-free mass for a 120 kg man", {
  actual <- ropivacaine(120, 170, 50, "male")
  expected <- list(v1 = 9.1343469, v2 = 16.44182442, v3 = 37.45082229,
                   cl1 = 0.36627378, cl2 = 1.180215513, cl3 = 0.48836504)
  expect_equal_rounded(actual$PK$default[names(expected)], expected)
})
