test_that("returns the published parameters with total-body-weight scaling", {
  weight <- 70
  height <- 171
  age <- 50
  sex <- "male"
  # The switch off reproduces the pre-fat-free-mass output exactly.
  actual <- midazolam(weight, height, age, sex, adjustToFFM = FALSE)

  expected <- list(
    PK = list(
      default = list(
        v1 = 3.3,
        v2 = 17.56348,
        v3 = 96.75715,
        cl1 = 0.5351973,
        cl2 = 2.014531,
        cl3 = 0.8321115
      )
    ),
    tPeak = 4,
    MEAC = 0,
    typical = 0.1,
    upperTypical = 0.04,
    lowerTypical = 0.12,
    reference = "Mould DR et al., Clin Pharmacol Ther 1995;58(1):35-43. https://pubmed.ncbi.nlm.nih.gov/7628181/"
  )
  expect_equal_rounded(actual, expected)
})

test_that("scales to fat-free mass for a 120 kg man", {
  # 120 kg, 170 cm, 50 y male: FFM 71.09 kg against the 54.48 kg reference, so
  # volumes x 1.3049067 and clearances x 1.3049067^0.75 = 1.2209126 (worked out
  # from the Al-Sallami formula by hand, not from the code under test).
  actual <- midazolam(120, 170, 50, "male")
  expected <- list(
        v1 = 4.3061919,
        v2 = 22.918702,
        v3 = 126.25905,
        cl1 = 0.65342914,
        cl2 = 2.4595663,
        cl3 = 1.0159354
  )
  expect_equal_rounded(actual$PK$default[names(expected)], expected)
})
