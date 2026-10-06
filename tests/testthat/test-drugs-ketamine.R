test_that("returns the published parameters with total-body-weight scaling", {
  weight <- 70
  height <- 171
  age <- 50
  sex <- "male"
  # The switch off reproduces the pre-fat-free-mass output exactly.
  actual <- ketamine(weight, height, age, sex, adjustToFFM = FALSE)

  expected <- list(
    PK = list(
      default = list(
        v1 = 4.41,
        v2 = 10.56972,
        v3 = 178.2123,
        cl1 = 1.93158,
        cl2 = 2.61072,
        cl3 = 2.6019

      )
    ),
    tPeak = 3,
    MEAC = 0,
    typical = 0.12,
    upperTypical = 0.1,
    lowerTypical = 0.16,
    reference = "Domino EF et al., Clin Pharmacol Ther 1984;36(5):645-653. https://pubmed.ncbi.nlm.nih.gov/6488686/"
  )
  expect_equal_rounded(actual, expected)
})

test_that("scales to fat-free mass for a 120 kg man", {
  # 120 kg, 170 cm, 50 y male: FFM 71.09 kg against the 54.48 kg reference, so
  # volumes x 1.3049067 and clearances x 1.3049067^0.75 = 1.2209126 (worked out
  # from the Al-Sallami formula by hand, not from the code under test).
  actual <- ketamine(120, 170, 50, "male")
  expected <- list(
        v1 = 5.7546383,
        v2 = 13.792498,
        v3 = 232.55042,
        cl1 = 2.3582904,
        cl2 = 3.187461,
        cl3 = 3.1766925
  )
  expect_equal_rounded(actual$PK$default[names(expected)], expected)
})
