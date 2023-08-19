test_that("returns the correct calculations", {
  weight <- 70
  height <- 171
  age <- 50
  sex <- "male"
  actual <- hydromorphone(weight, height, age, sex)

  expected <- list(
    PK = list(
      default = list(
        v1 = 11.2,
        v2 = 112,
        v3 = 943.1579,
        cl1 = 1.2992,
        cl2 = 3.36,
        cl3 = 0.896,
        ka_PO = 0.01,
        bioavailability_PO = 0.6,
        tlag_PO = 0,
        ka_IM = 0.01,
        bioavailability_IM = 0.6,
        tlag_IM = 90,
        ka_IN = 0.01,
        bioavailability_IN = 0.6,
        tlag_IN = 180
      )
    ),
    tPeak = 19.6,
    MEAC = 0.0015,
    typical = 0.0018,
    upperTypical = 0.0012,
    lowerTypical = 0.003,
    reference = "Drover PK"
  )
  expect_equal_rounded(actual, expected)
})
