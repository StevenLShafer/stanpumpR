test_that("returns the published parameters with total-body-weight scaling", {
  weight <- 70
  height <- 171
  age <- 50
  sex <- "male"
  # The switch off reproduces the pre-fat-free-mass output exactly: Dahan's
  # parameters, unscaled.
  actual <- oliceridine(weight, height, age, sex, adjustToFFM = FALSE)

  expected <- list(
    v1 = 28,
    v2 = 29.1,
    v3 = 1,
    cl1 = 0.5283333,
    cl2 = 0.625,
    cl3 = 0
  )
  expect_equal_rounded(actual$PK$default[names(expected)], expected)
  # Dahan 2020: analgesic equilibration too fast to estimate, carried as a
  # t1/2 ke0 of 1 min, which peaks a bolus at 4.38 min on this disposition
  # (worked out by hand); 0.25 h was the ventilatory t1/2 ke0, not a tPeak.
  expect_equal(actual$tPeak, 4.4)
  expect_equal(actual$reference, "Dahan A et al., Anesthesiology 2020;133(3):559-568. https://pubmed.ncbi.nlm.nih.gov/32788558/")
})

test_that("scales to fat-free mass for a 120 kg man", {
  # 120 kg, 170 cm, 50 y male: volumes x 1.3049067, clearances x 1.2209126
  # (worked out from the Al-Sallami formula by hand).  v3 = 1 and cl3 = 0 are
  # placeholders for a missing compartment and are left alone.
  actual <- oliceridine(120, 170, 50, "male")
  expected <- list(
    v1 = 36.53739,
    v2 = 37.97278,
    v3 = 1,
    cl1 = 0.6450488,
    cl2 = 0.7630704,
    cl3 = 0
  )
  expect_equal_rounded(actual$PK$default[names(expected)], expected)
})

test_that("the analgesic tPeak of 4.4 min is an equilibration half-time of about 1 min", {
  # Dahan 2020 found analgesic equilibration too fast to estimate; the tPeak
  # carries that as a t1/2 ke0 of 1 min (opioid MEAC and tPeak audit,
  # 2026-10-12, kept over the ventilatory ke0 by decision of Steven L. Shafer
  # when the audit was merged, 2026-10-17).
  dd <- getDrugDefaultsGlobal()
  PK <- getDrugPK("oliceridine", 70, 170, 50, "male", dd[dd$Drug == "oliceridine", ])
  expect_equal(PK$tPeak, 4.4)
  expect_equal(log(2) / PK$PK$default$ke0, 1, tolerance = 0.01)
  # The library's MEAC is Dahan's analgesic potency and the typical
  # concentration his respiratory potency (drugDefaults_global.csv).
  row <- dd[dd$Drug == "oliceridine", ]
  expect_equal(row$MEAC, 27.9)
  expect_equal(row$Typical, 27.4)
})
