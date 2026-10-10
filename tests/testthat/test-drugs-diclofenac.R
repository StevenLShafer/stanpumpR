# Diclofenac: Standing 2011 three-compartment disposition, dispersible tablet
# through two lagged oral depots.  See the header of R/drugs_diclofenac.R.
# Pins are the published numbers; the simulation is checked against an
# independent Runge-Kutta integration of the published model.

noEvents <- data.frame(Time = numeric(0), Event = character(0))

test_that("returns the published parameters with the fat-free-mass switch off", {
  actual <- diclofenac(70, 171, 50, "male", adjustToFFM = FALSE)
  expected <- list(
    PK = list(
      default = list(
        v1 = 3.68, v2 = 7.48, v3 = 3.79,
        cl1 = 16.5 / 60, cl2 = 1.75 / 60, cl3 = 7.21 / 60,
        ka_PO = 2.95 / 60, tlag_PO = 3.6, bioavailability_PO = 0.35,
        ka_PO2 = 2.23 / 60, tlag_PO2 = 45, fraction_PO2 = 0.74
      )
    ),
    tPeak = 0, MEAC = 0,
    typical = 0, upperTypical = 0, lowerTypical = 0,
    reference = actual$reference
  )
  expect_equal_rounded(actual, expected)
  # With the switch off, the published allometry on total weight
  off <- diclofenac(140, 171, 50, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(off$v2, 7.48 * 2)
  expect_equal(off$cl1, 16.5 / 60 * 2^0.75)
})

test_that("scales to fat-free mass for a 120 kg man", {
  # 120 kg, 170 cm, 50 y male: volumes x 1.3049067, clearances x 1.2209126
  actual <- diclofenac(120, 170, 50, "male")$PK$default
  expected <- list(
    v1 = 3.68 * 1.3049067, v2 = 7.48 * 1.3049067, v3 = 3.79 * 1.3049067,
    cl1 = 16.5 / 60 * 1.2209126, cl2 = 1.75 / 60 * 1.2209126,
    cl3 = 7.21 / 60 * 1.2209126
  )
  expect_equal_rounded(actual[names(expected)], expected)
})

test_that("getDrugPK splits the bioavailability between the two oral depots", {
  PK <- getDrugPK("diclofenac", 70, 170, 40, "male")$PK$default
  expect_equal(PK$bioavailability_PO, 0.35 * 0.26)
  expect_equal(PK$bioavailability_PO2, 0.35 * 0.74)
  expect_equal(PK$tlag_PO2, 45)
  expect_equal(PK$ke0, 0)
})

# Independent check: the published three-compartment model with two lagged
# first-order depots, by fourth-order Runge-Kutta, at the library's reference
# man (70 kg, 170 cm: volumes x 0.9989, clearances x 0.9992 of the 70 kg
# values, taken from pkSizeFactors() so that only the structure is tested).
rk4Diclofenac <- function(times, doseIV, dosePO, dt = 0.01)
{
  size <- pkSizeFactors(70, 170, 40, "male", TRUE)
  v1 <- 3.68 * size$volume; v2 <- 7.48 * size$volume; v3 <- 3.79 * size$volume
  cl <- 16.5 / 60 * size$clearance; q2 <- 1.75 / 60 * size$clearance
  q3 <- 7.21 / 60 * size$clearance
  ka1 <- 2.95 / 60; ka2 <- 2.23 / 60; lag1 <- 3.6; lag2 <- 45
  d <- function(s) {
    c1 <- s[3] / v1
    c(-ka1 * s[1], -ka2 * s[2],
      ka1 * s[1] + ka2 * s[2] - cl * c1 - q2 * (c1 - s[4] / v2) - q3 * (c1 - s[5] / v3),
      q2 * (c1 - s[4] / v2), q3 * (c1 - s[5] / v3))
  }
  s <- c(0, 0, doseIV, 0, 0); t <- 0
  given1 <- given2 <- FALSE
  out <- numeric(length(times))
  for (i in seq_along(times)) {
    while (t < times[i] - 1e-12) {
      if (!given1 && t >= lag1 - 1e-12) { s[1] <- s[1] + dosePO * 0.35 * 0.26; given1 <- TRUE }
      if (!given2 && t >= lag2 - 1e-12) { s[2] <- s[2] + dosePO * 0.35 * 0.74; given2 <- TRUE }
      nextLag <- c(if (!given1) lag1, if (!given2) lag2)
      h <- min(dt, times[i] - t, nextLag - t)
      k1 <- d(s); k2 <- d(s + h / 2 * k1); k3 <- d(s + h / 2 * k2); k4 <- d(s + h * k3)
      s <- s + h * (k1 + 2 * k2 + 2 * k3 + k4) / 6
      t <- t + h
    }
    out[i] <- s[3] / v1
  }
  out
}

test_that("50 mg by mouth and 75 mg intravenously match an independent integration", {
  po <- simulateDrugsWithCovariates(
    data.frame(Drug = "diclofenac", Time = 0, Dose = 50, Units = "mg PO"),
    noEvents, 70, 170, 40, "male", 480, TRUE)$diclofenac$wide
  expect_equal(po$Plasma, rk4Diclofenac(po$Time, 0, 50), tolerance = 1e-5)
  # Nothing reaches plasma before the first lag (0.06 h, which in floating
  # point is a hair under 3.6 min)
  expect_true(all(po$Plasma[po$Time < 3.5] == 0))

  iv <- simulateDrugsWithCovariates(
    data.frame(Drug = "diclofenac", Time = 0, Dose = 75, Units = "mg"),
    noEvents, 70, 170, 40, "male", 480, TRUE)$diclofenac$wide
  keep <- iv$Time > 0
  expect_equal(iv$Plasma[keep], rk4Diclofenac(iv$Time[keep], 75, 0), tolerance = 1e-5)
})

test_that("the second oral depot is carried through a change in PK sets", {
  # advanceClosedForm1(), across a switch to an identical PK set at 50 min,
  # must give the single-set engine's curve: both depots, both lags.
  PK <- getDrugPK("diclofenac", 70, 170, 40, "male")
  dose <- data.frame(Drug = "diclofenac", Time = c(0, 20), Dose = c(50, 50),
                     Units = "mg PO")
  ref <- simCpCe(dose, noEvents, PK, 480, FALSE)$results
  PK$PK$Switch <- PK$PK$default
  PK$pkEvents <- c(PK$pkEvents, "Switch")
  alt <- simCpCe(dose, data.frame(Time = 50, Event = "Switch"), PK, 480, FALSE)$results
  ref <- ref[ref$Site == "Plasma", ]
  alt <- alt[alt$Site == "Plasma", ]
  # compared where both engines put a point (the switch adds its own)
  at <- intersect(round(alt$Time, 6), round(ref$Time, 6))
  expect_gt(length(at), 50)
  expect_equal(alt$Y[match(at, round(alt$Time, 6))], ref$Y[match(at, round(ref$Time, 6))],
               tolerance = 1e-6)
})
