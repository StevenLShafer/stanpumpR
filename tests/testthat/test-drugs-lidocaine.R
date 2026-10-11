test_that("returns the published parameters with total-body-weight scaling", {
  weight <- 70
  height <- 171
  age <- 50
  sex <- "male"
  # The switch off reproduces the pre-fat-free-mass output exactly.
  actual <- lidocaine(weight, height, age, sex, adjustToFFM = FALSE)

  expected <- list(
    PK = list(
      default = list(
        v1 = 6.16,
        v2 = 28.00002,
        v3 = 1,
        cl1 = 1.400002,
        cl2 = 3.920002,
        cl3 = 0,
        ka_RA = 0.0111,
        bioavailability_RA = 1,
        tlag_RA = 0
      )
    ),
    tPeak = 5,
    MEAC = 0,
    typical = 1,
    upperTypical = 1.5,
    lowerTypical = 0.5,
    reference = "Schnider TW et al., Anesthesiology 1996;84(5):1043-1050. https://pubmed.ncbi.nlm.nih.gov/8623997/"
  )
  expect_equal_rounded(actual, expected)
})

test_that("scales to fat-free mass for a 120 kg man", {
  # 120 kg, 170 cm, 50 y male: FFM 71.09 kg against the 54.48 kg reference, so
  # volumes x 1.3049067 and clearances x 1.3049067^0.75 = 1.2209126 (worked out
  # from the Al-Sallami formula by hand, not from the code under test).
  # v3 = 1 and cl3 = 0 are placeholders for a missing compartment and are
  # left alone.
  actual <- lidocaine(120, 170, 50, "male")
  expected <- list(
        v1 = 8.038225,
        v2 = 36.537412,
        v3 = 1,
        cl1 = 1.7092801,
        cl2 = 4.7859799,
        cl3 = 0
  )
  expect_equal_rounded(actual$PK$default[names(expected)], expected)
})
