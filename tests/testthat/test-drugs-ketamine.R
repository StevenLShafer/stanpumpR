test_that("returns the published parameters with total-body-weight scaling", {
  weight <- 70
  height <- 171
  age <- 50
  sex <- "male"
  # The switch off gives Domino's values at 70 kg: V1 0.063 L/kg, CL 0.848
  # L/kg/h (0.989 L/min), V3 1.51 L/kg; V2 and the intercompartmental
  # clearances follow from the three half-lives (see the drug file).
  actual <- ketamine(weight, height, age, sex, adjustToFFM = FALSE)

  expected <- list(
    PK = list(
      default = list(
        v1 = 4.41,
        v2 = 8.68798875,
        v3 = 105.7000153,
        cl1 = 0.989333226,
        cl2 = 3.837635802,
        cl3 = 1.451376423
      )
    ),
    tPeak = 3,
    MEAC = 0,
    typical = 0.12,
    upperTypical = 0.16,
    lowerTypical = 0.1,
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
        v2 = 11.337014,
        v3 = 137.92865,
        cl1 = 1.2078894,
        cl2 = 4.685418,
        cl3 = 1.7720038
  )
  expect_equal_rounded(actual$PK$default[names(expected)], expected)
})

test_that("the rate constants reproduce Domino's clearance, volumes and half-lives at once", {
  # Domino 1984, placebo arm: V1 0.063 L/kg, CL 0.848 L/kg/h, V3 about 1.51
  # L/kg, half-lives 24.1 s, 4.68 min, 2.17 h.  Until 2026-10-14 the means
  # of the individual rate constants gave a clearance of 1.66 L/kg/h.
  s <- ketamine(70, 170, 40, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(s$v1 / 70, 0.063)
  expect_equal(s$cl1 * 60 / 70, 0.848, tolerance = 1e-6)
  expect_equal(s$v3 / 70, 1.51, tolerance = 1e-6)
  K <- matrix(c(-(s$cl1 + s$cl2 + s$cl3) / s$v1, s$cl2 / s$v2, s$cl3 / s$v3,
                s$cl2 / s$v1, -s$cl2 / s$v2, 0,
                s$cl3 / s$v1, 0, -s$cl3 / s$v3), 3, 3, byrow = TRUE)
  lambda <- sort(-Re(eigen(K)$values), decreasing = TRUE)
  expect_equal(log(2) / lambda * c(60, 1, 1 / 60), c(24.1, 4.68, 2.17), tolerance = 1e-4)
})
