test_that("returns the published parameters with total-body-weight scaling", {
  weight <- 70
  height <- 171
  age <- 50
  sex <- "male"
  # The switch off reproduces the pre-fat-free-mass output exactly.
  actual <- alfentanil(weight, height, age, sex, adjustToFFM = FALSE)

  expected <- list(
    PK = list(
      default = list(
        v1 = 2.1853,
        v2 = 6.698864,
        v3 = 14.52582,
        cl1 = 0.1988623,
        cl2 = 1.433557,
        cl3 = 0.2469389
      )
    ),
    tPeak = 1.4,
    MEAC = 39,
    typical = 46.8,
    upperTypical = 31.2,
    lowerTypical = 78,
    reference = "Scott JC, Stanski DR. J Pharmacol Exp Ther 1987;240(1):159-166. https://pubmed.ncbi.nlm.nih.gov/3100765/"
  )

  expect_equal_rounded(actual, expected)
})

test_that("scales to fat-free mass for a 120 kg man", {
  # 120 kg, 170 cm, 50 y male: FFM 71.09 kg against the 54.48 kg reference, so
  # volumes x 1.3049067 and clearances x 1.3049067^0.75 = 1.2209126 (worked out
  # from the Al-Sallami formula by hand, not from the code under test).
  actual <- alfentanil(120, 170, 50, "male")
  expected <- list(
        v1 = 2.8516125,
        v2 = 8.7413922,
        v3 = 18.954839,
        cl1 = 0.24279349,
        cl2 = 1.7502478,
        cl3 = 0.30149082
  )
  expect_equal_rounded(actual$PK$default[names(expected)], expected)
})
