test_that("returns the published parameters with total-body-weight scaling", {
  weight <- 70
  height <- 171
  age <- 50
  sex <- "male"
  # The switch off reproduces the pre-fat-free-mass output exactly.
  actual <- oxycodone(weight, height, age, sex, adjustToFFM = FALSE)

  expected <- list(
    PK = list(
      default = list(
        v1 = 90.2,
        v2 = 68.9,
        v3 = 1,
        cl1 = 0.6233333,
        cl2 = 3.433333,
        cl3 = 0,
        ka_PO = 0.06,
        bioavailability_PO = 0.5,
        tlag_PO = 0
      )
    ),
    tPeak = 60,
    MEAC = 12,
    typical = 14.4,
    upperTypical = 9.6,
    lowerTypical = 24,
    reference = "Lamminsalo M et al., Expert Opin Drug Deliv 2019;16(6):649-656. https://pubmed.ncbi.nlm.nih.gov/31092024/",
    # Oxymorphone, added 2026-10-05.  The disposition above is unchanged: a
    # metabolite is an independent transfer and is not subtracted from the
    # parent, so oxycodone's own plasma and effect-site curves are identical
    # with and without it.  Asserted in test-drugs-oxymorphone.R.
    metabolite = list(
      name              = "oxymorphone",
      kFormation        = 6.629873e-06 * 70,
      firstPassFraction = 0,
      mwRatio           = 301.34 / 315.36
    )
  )
  expect_equal_rounded(actual, expected)
})

test_that("scales to fat-free mass for a 120 kg man", {
  # 120 kg, 170 cm, 50 y male: FFM 71.09 kg against the 54.48 kg reference, so
  # volumes x 1.3049067 and clearances x 1.3049067^0.75 = 1.2209126 (worked out
  # from the Al-Sallami formula by hand, not from the code under test).
  # v3 = 1 and cl3 = 0 are placeholders for a missing compartment and are
  # left alone.
  actual <- oxycodone(120, 170, 50, "male")
  expected <- list(
        v1 = 117.70258,
        v2 = 89.908068,
        v3 = 1,
        cl1 = 0.76103549,
        cl2 = 4.1917996,
        cl3 = 0
  )
  expect_equal_rounded(actual$PK$default[names(expected)], expected)
})
