test_that("returns the published parameters with total-body-weight scaling", {
  weight <- 70
  height <- 171
  age <- 50
  sex <- "male"
  # The switch off reproduces the pre-fat-free-mass output exactly.
  actual <- rocuronium(weight, height, age, sex, adjustToFFM = FALSE)

  expected <- list(
    PK = list(
      default = list(
        v1 = 3.92,
        v2 = 16.06096,
        v3 = 1,
        cl1 = 0.684432,
        cl2 = 0.3934935,
        cl3 = 0
      )
    ),
    tPeak = 2.2,
    MEAC = 0,
    typical = 1.5,
    upperTypical = 2.2,
    lowerTypical = 1,
    reference = "Plaud B et al., Clin Pharmacol Ther 1995;58(2):185-191. https://pubmed.ncbi.nlm.nih.gov/7648768/"
  )

  expect_equal_rounded(actual, expected)
})

test_that("scales to fat-free mass for a 120 kg man", {
  # 120 kg, 170 cm, 50 y male: FFM 71.09 kg against the 54.48 kg reference, so
  # volumes x 1.3049067 and clearances x 1.3049067^0.75 = 1.2209126 (worked out
  # from the Al-Sallami formula by hand, not from the code under test).
  # v3 = 1 and cl3 = 0 are placeholders for a missing compartment and are
  # left alone.
  actual <- rocuronium(120, 170, 50, "male")
  expected <- list(
        v1 = 5.1152341,
        v2 = 20.958054,
        v3 = 1,
        cl1 = 0.83563167,
        cl2 = 0.48042118,
        cl3 = 0
  )
  expect_equal_rounded(actual$PK$default[names(expected)], expected)
})
