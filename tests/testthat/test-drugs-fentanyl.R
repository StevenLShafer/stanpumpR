test_that("returns the published parameters with total-body-weight scaling", {
  weight <- 70
  height <- 171
  age <- 50
  sex <- "male"
  # The switch off reproduces the pre-fat-free-mass output exactly.
  actual <- fentanyl(weight, height, age, sex, adjustToFFM = FALSE)

  expected <- list(
    PK = list(
      default = list(
        v1 = 12.1,
        v2 = 35.7,
        v3 = 224,
        cl1 = 0.632,
        cl2 = 2.8,
        cl3 = 1.55
      )
    ),
    tPeak = 3.694,
    MEAC = 0.6,
    typical = 0.72,
    upperTypical = 0.48,
    lowerTypical = 1.2,
    reference = "Scott JC, Stanski DR. J Pharmacol Exp Ther 1987;240(1):159-166. https://pubmed.ncbi.nlm.nih.gov/3100765/"
  )
  expect_equal_rounded(actual, expected)
})

test_that("scales to fat-free mass for a 120 kg man", {
  # 120 kg, 170 cm, 50 y male: FFM 71.09 kg against the 54.48 kg reference, so
  # volumes x 1.3049067 and clearances x 1.3049067^0.75 = 1.2209126 (worked out
  # from the Al-Sallami formula by hand, not from the code under test).
  actual <- fentanyl(120, 170, 50, "male")
  expected <- list(
        v1 = 15.78937,
        v2 = 46.585167,
        v3 = 292.29909,
        cl1 = 0.77161678,
        cl2 = 3.4185553,
        cl3 = 1.8924146
  )
  expect_equal_rounded(actual$PK$default[names(expected)], expected)
})
