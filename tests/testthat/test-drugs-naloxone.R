test_that("returns the published parameters with total-body-weight scaling", {
  weight <- 70
  height <- 171
  age <- 50
  sex <- "male"
  # The switch off reproduces the pre-fat-free-mass output exactly.
  actual <- naloxone(weight, height, age, sex, adjustToFFM = FALSE)

  expected <- list(
    PK = list(
      default = list(
        v1 = 28.56,
        v2 = 44.52,
        v3 = 114.59,
        cl1 = 3.43,
        cl2 = 3.22,
        cl3 = 1.82
      )
    ),
    tPeak = 1,
    MEAC = 0,
    typical = 0,
    upperTypical = 0,
    lowerTypical = 0,
    reference = "Papathanasiou T et al., Br J Anaesth 2019;123(2):e204-e214. https://pubmed.ncbi.nlm.nih.gov/30915992/"
  )
  expect_equal_rounded(actual, expected)
})

test_that("scales to fat-free mass for a 120 kg man", {
  # 120 kg, 170 cm, 50 y male: FFM 71.09 kg against the 54.48 kg reference, so
  # volumes x 1.3049067 and clearances x 1.3049067^0.75 = 1.2209126 (worked out
  # from the Al-Sallami formula by hand, not from the code under test).
  actual <- naloxone(120, 170, 50, "male")
  expected <- list(
        v1 = 37.268134,
        v2 = 58.094444,
        v3 = 149.52925,
        cl1 = 4.1877303,
        cl2 = 3.9313386,
        cl3 = 2.222061
  )
  expect_equal_rounded(actual$PK$default[names(expected)], expected)
})
