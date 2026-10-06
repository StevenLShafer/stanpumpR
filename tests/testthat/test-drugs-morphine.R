test_that("returns the published parameters with total-body-weight scaling", {
  weight <- 70
  height <- 171
  age <- 50
  sex <- "male"
  # The switch off reproduces the pre-fat-free-mass output exactly.
  actual <- morphine(weight, height, age, sex, adjustToFFM = FALSE)

  expected <- list(
    PK = list(
      default = list(
        v1 = 17.5,
        v2 = 85.82865,
        v3 = 195.646,
        cl1 = 1.233848,
        cl2 = 2.228464,
        cl3 = 0.3195225
      )
    ),
    tPeak = 93.8,
    MEAC = 0.008,
    typical = 0.0096,
    upperTypical = 0.0064,
    lowerTypical = 0.016,
    reference = "Lotsch J et al., Clin Pharmacol Ther 2002;72(2):151-162. https://pubmed.ncbi.nlm.nih.gov/12189362/"
  )
  expect_equal_rounded(actual, expected)
})

test_that("scales to fat-free mass for a 120 kg man", {
  # 120 kg, 170 cm, 50 y male: FFM 71.09 kg against the 54.48 kg reference, so
  # volumes x 1.3049067 and clearances x 1.3049067^0.75 = 1.2209126 (worked out
  # from the Al-Sallami formula by hand, not from the code under test).
  actual <- morphine(120, 170, 50, "male")
  expected <- list(
        v1 = 22.835866,
        v2 = 111.99838,
        v3 = 255.29977,
        cl1 = 1.5064206,
        cl2 = 2.7207598,
        cl3 = 0.39010905
  )
  expect_equal_rounded(actual$PK$default[names(expected)], expected)
})
