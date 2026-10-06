test_that("returns the published parameters with total-body-weight scaling", {
  weight <- 70
  height <- 171
  age <- 50
  sex <- "male"
  # The switch off reproduces the pre-fat-free-mass output exactly.
  actual <- pethidine(weight, height, age, sex, adjustToFFM = FALSE)

  expected <- list(
    PK = list(
      default = list(
        v1 = 18.1,
        v2 = 60.87231,
        v3 = 165.4081,
        cl1 = 0.7625801,
        cl2 = 5.430411,
        cl3 = 1.782144
      )
    ),
    tPeak = 10,
    MEAC = 0.25,
    typical = 0.3,
    upperTypical = 0.2,
    lowerTypical = 0.5,
    reference = "Bjorkman S, J Pharmacokinet Pharmacodyn 2003;30(4):285-307. https://pubmed.ncbi.nlm.nih.gov/14650375/"
  )
  expect_equal_rounded(actual, expected)
})

test_that("scales to fat-free mass for a 120 kg man", {
  # 120 kg, 170 cm, 50 y male: FFM 71.09 kg against the 54.48 kg reference, so
  # volumes x 1.3049067 and clearances x 1.3049067^0.75 = 1.2209126 (worked out
  # from the Al-Sallami formula by hand, not from the code under test).
  actual <- pethidine(120, 170, 50, "male")
  expected <- list(
        v1 = 23.61881,
        v2 = 79.432682,
        v3 = 215.84213,
        cl1 = 0.93104367,
        cl2 = 6.6300573,
        cl3 = 2.1758421
  )
  expect_equal_rounded(actual$PK$default[names(expected)], expected)
})
