# aprepitant and fosaprepitant: see the header of R/drugs_aprepitant.R.  Pins
# were worked out from Nijstad's equations and the fat-free-mass factors by
# hand (Python), not from the code under test.

noEvents <- data.frame(Time = numeric(0), Event = character(0))

test_that("aprepitant returns Nijstad's equations at 70 kg with the switch off", {
  actual <- aprepitant(70, 171, 50, "male", adjustToFFM = FALSE)
  expected <- list(
    PK = list(
      default = list(v1 = 86.8, v2 = 1, v3 = 1, cl1 = 0.097166667, cl2 = 0, cl3 = 0)
    ),
    tPeak = 0, MEAC = 0, typical = 500, upperTypical = 1500, lowerTypical = 116,
    reference = actual$reference
  )
  expect_equal_rounded(actual, expected)
})

test_that("aprepitant uses the pharmacokinetic weight for a 120 kg man", {
  # FFM ratio 1.3049067, so the pharmacokinetic weight is 91.343 kg
  actual <- aprepitant(120, 170, 50, "male")
  expected <- list(v1 = 113.26590, v2 = 1, v3 = 1, cl1 = 0.11863201, cl2 = 0, cl3 = 0)
  expect_equal_rounded(actual$PK$default[names(expected)], expected)
})

test_that("fosaprepitant divides V and CL by the molar fraction 534.44 / 614.40", {
  expect_equal(FOSAPREPITANT_APREPITANT_FRACTION, 0.86985677, tolerance = 1e-7)
  off <- fosaprepitant(70, 171, 50, "male", adjustToFFM = FALSE)$PK$default
  expect_equal_rounded(off[c("v1", "cl1")], list(v1 = 99.786543, cl1 = 0.11170421))
  on <- fosaprepitant(120, 170, 50, "male")$PK$default
  expect_equal_rounded(on[c("v1", "cl1")], list(v1 = 130.21213, cl1 = 0.13638109))
})

simAt <- function(drug, DT, t, weight, height, age) {
  o <- simulateDrugsWithCovariates(DT[DT$Time < t, ], noEvents, weight, height,
                                   age, "male", t, FALSE, adjustToFFM = FALSE)
  w <- o[[drug]]$wide
  w$Plasma[nrow(w)]
}

test_that("30 mg fosaprepitant over 30 minutes at 30 kg reproduces the reference implementation", {
  # Contributed Python reference (antiemetic_pkpd.py, total body weight):
  # 687.138, 264.514 and 1.81693 ng/mL at 0.5, 12 and 72 h.
  DT <- data.frame(Drug = "fosaprepitant", Time = c(0, 30), Dose = c(60, 0),
                   Units = "mg/hr")
  expect_equal(simAt("fosaprepitant", DT, 30, 30, 138, 10), 687.13849, tolerance = 1e-5)
  expect_equal(simAt("fosaprepitant", DT, 720, 30, 138, 10), 264.51373, tolerance = 1e-5)
  expect_equal(simAt("fosaprepitant", DT, 4320, 30, 138, 10), 1.8169304, tolerance = 1e-5)
})

test_that("a fosaprepitant dose plots the aprepitant its conversion yields", {
  fos <- data.frame(Drug = "fosaprepitant", Time = 0, Dose = 150, Units = "mg")
  apr <- data.frame(Drug = "aprepitant", Time = 0,
                    Dose = 150 * FOSAPREPITANT_APREPITANT_FRACTION, Units = "mg")
  for (t in c(60, 1440)) {
    expect_equal(simAt("fosaprepitant", fos, t, 70, 170, 50),
                 simAt("aprepitant", apr, t, 70, 170, 50), tolerance = 1e-8)
  }
})

test_that("the bands are the CSV's", {
  for (drug in c("aprepitant", "fosaprepitant")) {
    d <- getDrugDefaults(drug)
    x <- get(drug)(70, 170, 50, "male")
    expect_equal(c(d$Lower, d$Upper, d$Typical, d$MEAC),
                 c(x$lowerTypical, x$upperTypical, x$typical, x$MEAC), info = drug)
  }
})
