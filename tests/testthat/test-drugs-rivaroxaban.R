# Rivaroxaban: Willmann 2018 apparent oral one-compartment model, atrial
# fibrillation.  See the header of R/drugs_rivaroxaban.R.

noEvents <- data.frame(Time = numeric(0), Event = character(0))

test_that("returns the reference parameters with the switch off", {
  # Covariates centred on the population means: 82.48 kg, 97.17 mL/min, 60.53
  # y.  The creatinine giving Cockcroft-Gault 97.17 at 82.48 kg, 60.53 y male
  # (below the Tietz truncation for this BSA) anchors CL at its reference.
  scr <- (140 - 60.53) * 82.48 / (72 * 97.17)
  actual <- rivaroxaban(82.48, 176, 60.53, "male", adjustToFFM = FALSE,
                        creatinine = scr)
  expected <- list(
    PK = list(default = list(
      v1 = 62.5, v2 = 1, v3 = 1,
      cl1 = 6.58 * 0.849 / 60, cl2 = 0, cl3 = 0,
      ka_PO = 0.821 / 60, tlag_PO = 0, bioavailability_PO = 1
    )),
    tPeak = 0, MEAC = 0,
    typical = 0, upperTypical = 0, lowerTypical = 0,
    oralSaturation = actual$oralSaturation,
    reference = actual$reference
  )
  expect_equal_rounded(actual, expected)
})

test_that("relative bioavailability is 1 at 10 mg and falls with the dose", {
  sat <- rivaroxaban(82, 176, 60, "male")$oralSaturation
  f <- oralSaturationFraction(c(10, 20), sat)
  expect_equal(f, c(0.998, 0.842), tolerance = 1e-3)   # exponential form
  expect_true(oralSaturationFraction(2.5, sat) > 1)    # relative, can exceed 1
})

test_that("creatinine clearance is Tietz-truncated", {
  # A low creatinine is held at 140 x BSA / 1.73.
  cl <- rivaroxaban(82.48, 176, 60.53, "male", adjustToFFM = FALSE,
                    creatinine = 0.3)$PK$default$cl1
  crcl <- min(creatinineClearanceCG(82.48, 60.53, "male", 0.3),
              140 * bsaDuBois(82.48, 176) / 1.73)
  expected <- 6.58 * (crcl / 97.17)^0.406 * (82.48 / 82.48)^-0.278 * 0.849 / 60
  expect_equal(cl, expected, tolerance = 1e-8)
})

test_that("the female volume term applies", {
  f <- rivaroxaban(82.48, 165, 60.53, "female", adjustToFFM = FALSE)$PK$default$v1
  m <- 62.5 * (82.48 / 82.48)^0.216 * (60.53 / 60.53)^-0.189 * 0.889
  expect_equal(f, m, tolerance = 1e-8)
})
