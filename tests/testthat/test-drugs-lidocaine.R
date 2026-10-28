test_that("returns the published parameters with total-body-weight scaling", {
  weight <- 70
  height <- 171
  age <- 50
  sex <- "male"
  # The switch off gives Schnider's set C per kilogram at 70 kg (V1 0.101, V2
  # 0.452 L/kg; Cl1 0.0215, Cl2 0.0589 L/kg/min); set B until 2026-10-28.
  actual <- lidocaine(weight, height, age, sex, adjustToFFM = FALSE)

  expected <- list(
    PK = list(
      default = list(
        v1 = 7.07,
        v2 = 31.64,
        v3 = 1,
        cl1 = 1.505,
        cl2 = 4.123,
        cl3 = 0,
        ka_RA = 0.0125,
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
        v1 = 9.22569,
        v2 = 41.287248,
        v3 = 1,
        cl1 = 1.837473,
        cl2 = 5.033823,
        cl3 = 0
  )
  expect_equal_rounded(actual$PK$default[names(expected)], expected)
})
