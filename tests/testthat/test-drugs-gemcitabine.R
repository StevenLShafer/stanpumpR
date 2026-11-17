# Gemcitabine and dFdU (R/drugs_gemcitabine.R): Tham 2008.  Drafted by
# Claude Code, 2026-11-06.

noEvents <- data.frame(Time = double(), Event = character())

test_that("Table 2 at the 70-kg, 1.76-m standard: CLG 305.2 L/h and its split", {
  set <- getDrugPK("gemcitabine", 70, 176, 35, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(round(set$cl1 * 60, 1), 305.2)
  expect_equal_rounded(set$v1, 2.96)
  expect_equal_rounded(set$cl2 * 60, 156)
  expect_equal_rounded(set$v2, 47.6)
  halfLives <- sort(log(2) / c(set$lambda_1, set$lambda_2))      # minutes
  expect_equal(round(halfLives, 1), c(0.3, 19.3))
  expect_equal(set$metabolite$name, "gemcitabineDFDU")
  m <- getDrugPK("gemcitabineDFDU", 70, 176, 35, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(round(log(2) * m$v1 / (m$cl1 * 60), 2), 1.70)
})

test_that("size is lean body mass over the standard's 56.13 kg (Janmahasatian in adults)", {
  expect_equal(round(ffmAlSallami(70, 176, 35, "male"), 2), 56.13)
  s <- ffmAlSallami(58.1, 161, 58, "female") / ffmAlSallami(70, 176, 35, "male")
  set <- gemcitabine(58.1, 161, 58, "female")$PK$default
  expect_equal_rounded(set$v2, 47.6 * s)
  expect_equal_rounded(set$cl1 * 60, 70.2 / 0.23 * s^0.75)
  # the switch does not change a model with its own lean-body-mass covariate
  expect_equal(gemcitabine(90, 170, 50, "male", adjustToFFM = TRUE)$PK,
               gemcitabine(90, 170, 50, "male", adjustToFFM = FALSE)$PK)
})

test_that("1000 mg/m2 over 30 min: peaks of 40.7 umol/L gemcitabine and 71.6 umol/L dFdU", {
  dose <- 1000 * 1.856
  d <- data.frame(Drug = "gemcitabine", Time = c(0, 30), Dose = c(dose * 2, 0),
                  Units = "mg/hr")
  w <- simulateDrugsWithCovariates(d, noEvents, 70, 176, 35, "male", 6 * 60, FALSE,
                                   adjustToFFM = FALSE)
  expect_equal(max(w$gemcitabine$wide$Plasma) / 263.20 * 1000, 40.7, tolerance = 0.02)
  expect_equal(max(w$gemcitabineDFDU$wide$Plasma) / 264.19 * 1000, 71.6, tolerance = 0.02)
})

test_that("gemcitabine is a chemotherapy drug with the metabolite-only gemcitabineDFDU", {
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "gemcitabine", ]
  expect_identical(row$Category, CHEMOTHERAPY_DRUG_CATEGORY)
  expect_equal(c(row$Lower, row$Upper, row$Typical, row$MEAC), c(0, 0, 0, 0))
  expect_match(getDrugPK("gemcitabine", 70, 170, 60, "male")$reference, "Tham")
  expect_identical(therapeuticRange("gemcitabine")$band_status, "not_applicable")
  expect_identical(therapeuticRange("gemcitabineDFDU")$band_status, "not_applicable")
})
