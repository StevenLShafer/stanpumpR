# Ketorolac: Cloesmeijer 2021 S (three-compartment) and R (two-compartment)
# enantiomers, simulated as parallel systems and summed.  See the header of
# R/drugs_ketorolac.R.  Pins are the published numbers; the sum is checked
# against an independent Runge-Kutta integration of both systems.

noEvents <- data.frame(Time = numeric(0), Event = character(0))

test_that("returns the published parameters with the fat-free-mass switch off", {
  actual <- ketorolac(70, 171, 50, "male", adjustToFFM = FALSE)
  oral <- list(ka_PO = log(2) / 3.8, bioavailability_PO = 1, tlag_PO = 0)
  expect_equal_rounded(actual$PK$default, c(list(
    v1 = 4.03, v2 = 43.3, v3 = 5.90,
    cl1 = 3.97 / 60, cl2 = 1.86 / 60, cl3 = 19.7 / 60), oral))
  r <- actual$parallelSystems[[1]]
  expect_identical(r$name, "R-ketorolac")
  expect_equal_rounded(r$PK$default, c(list(
    v1 = 4.43, v2 = 5.18, v3 = 1,
    cl1 = 1.45 / 60, cl2 = 1.90 / 60, cl3 = 0), oral))
  # 30 mg tromethamine is 10.17 mg of each enantiomer as free acid
  expect_equal(30 * actual$doseFraction, 10.1727, tolerance = 1e-5)
  expect_equal(r$doseFraction, actual$doseFraction)
  expect_equal(actual$tPeak, 0)
})

test_that("scales both systems to fat-free mass for a 120 kg man", {
  x <- ketorolac(120, 170, 50, "male")
  expect_equal(x$PK$default$v2, 43.3 * 1.3049067, tolerance = 1e-7)
  expect_equal(x$PK$default$cl1, 3.97 / 60 * 1.2209126, tolerance = 1e-7)
  r <- x$parallelSystems[[1]]$PK$default
  expect_equal(r$v1, 4.43 * 1.3049067, tolerance = 1e-7)
  expect_equal(r$cl1, 1.45 / 60 * 1.2209126, tolerance = 1e-7)
})

test_that("getDrugPK builds the R system's coefficients", {
  PK <- getDrugPK("ketorolac", 70, 170, 40, "male")
  r <- PK$parallelSystems[[1]]$PK$default
  # two compartments: one zero eigenvalue
  expect_equal(r$lambda_3, 0)
  # bolus coefficients sum to 1 / V1
  expect_equal(r$p_coef_bolus_l1 + r$p_coef_bolus_l2, 1 / r$v1)
  expect_equal(PK$doseFraction, KETOROLAC_ENANTIOMER_FRACTION)
})

# Independent check by fourth-order Runge-Kutta: the S and R systems at the
# library's reference man, each given doseFraction of the labelled dose, as
# a bolus or into a first-order oral depot, summed.
rk4Ketorolac <- function(times, doseIV, dosePO, dt = 0.01)
{
  size <- pkSizeFactors(70, 170, 40, "male", TRUE)
  f <- 0.5 * 255.273 / 376.409
  ka <- log(2) / 3.8
  sys <- list(
    S = list(v = c(4.03, 43.3, 5.90) * size$volume,
             cl = c(3.97, 1.86, 19.7) / 60 * size$clearance),
    R = list(v = c(4.43, 5.18, 1) * size$volume,
             cl = c(1.45, 1.90, 0) / 60 * size$clearance)
  )
  total <- 0
  for (p in sys) {
    v <- p$v; cl <- p$cl
    d <- function(s) {
      c1 <- s[2] / v[1]
      c(-ka * s[1],
        ka * s[1] - cl[1] * c1 - cl[2] * (c1 - s[3] / v[2]) - cl[3] * (c1 - s[4] / v[3]),
        cl[2] * (c1 - s[3] / v[2]), cl[3] * (c1 - s[4] / v[3]))
    }
    s <- c(dosePO * f, doseIV * f, 0, 0); t <- 0
    out <- numeric(length(times))
    for (i in seq_along(times)) {
      while (t < times[i] - 1e-12) {
        h <- min(dt, times[i] - t)
        k1 <- d(s); k2 <- d(s + h / 2 * k1); k3 <- d(s + h / 2 * k2); k4 <- d(s + h * k3)
        s <- s + h * (k1 + 2 * k2 + 2 * k3 + k4) / 6
        t <- t + h
      }
      out[i] <- s[2] / v[1]
    }
    total <- total + out
  }
  total
}

test_that("30 mg intravenously and 10 mg by mouth match an independent integration", {
  iv <- simulateDrugsWithCovariates(
    data.frame(Drug = "ketorolac", Time = 0, Dose = 30, Units = "mg"),
    noEvents, 70, 170, 40, "male", 720, TRUE)$ketorolac$wide
  keep <- iv$Time > 0
  expect_equal(iv$Plasma[keep], rk4Ketorolac(iv$Time[keep], 30, 0), tolerance = 1e-5)

  po <- simulateDrugsWithCovariates(
    data.frame(Drug = "ketorolac", Time = 0, Dose = 10, Units = "mg PO"),
    noEvents, 70, 170, 40, "male", 720, TRUE)$ketorolac$wide
  expect_equal(po$Plasma, rk4Ketorolac(po$Time, 0, 10), tolerance = 1e-5)
})

test_that("the time until threshold is solved from the summed systems", {
  PK <- getDrugPK("ketorolac", 70, 170, 40, "male")
  out <- simCpCe(data.frame(Drug = "ketorolac", Time = 0, Dose = 30, Units = "mg"),
                 noEvents, PK, 720, plotRecovery = TRUE)
  x <- out$results
  cp <- x[x$Site == "Plasma", ]
  # The plasma falls to 0.37 mg/L (endCe) at the time the recovery at t = 0+
  # predicts: read the recovery just after the dose and check the curve there
  rec <- out$equiSpace$Recovery[2]
  t0  <- out$equiSpace$Time[2]
  expect_equal(approx(cp$Time, cp$Y, t0 + rec)$y, 0.37, tolerance = 5e-3)
})

test_that("target-controlled infusion is refused for a sum of systems", {
  PK <- getDrugPK("ketorolac", 70, 170, 40, "male")
  expect_error(
    simCpCe(data.frame(Drug = "ketorolac", Time = 0, Dose = 1, Units = "Plasma target"),
            noEvents, PK, 120, FALSE),
    "not available"
  )
})

test_that("pregnancy: every clearance and volume of S and R x 1.5", {
  # A woman of 30 years, 70 kg, 165 cm: CL, every volume and every
  # distribution clearance x 1.5 in both enantiomers, so the rate constants
  # and half-lives are unchanged (R/drugs_ketorolac.R, PREGNANCY).
  on  <- getDrugPK("ketorolac", 70, 165, 30, SEX_FEMALE, pregnant = TRUE)
  off <- getDrugPK("ketorolac", 70, 165, 30, SEX_FEMALE, pregnant = FALSE)
  expect_identical(off, getDrugPK("ketorolac", 70, 165, 30, SEX_FEMALE))
  a <- on$PK[[1]]; b <- off$PK[[1]]
  expect_equal(a$v1 / b$v1, KETOROLAC_PREGNANCY_FACTOR, tolerance = 1e-6)
  expect_equal(a$v2 / b$v2, KETOROLAC_PREGNANCY_FACTOR, tolerance = 1e-6)
  expect_equal(a$v3 / b$v3, KETOROLAC_PREGNANCY_FACTOR, tolerance = 1e-6)
  expect_equal(a$cl1 / b$cl1, KETOROLAC_PREGNANCY_FACTOR, tolerance = 1e-6)
  expect_equal(a$cl2 / b$cl2, KETOROLAC_PREGNANCY_FACTOR, tolerance = 1e-6)
  expect_equal(a$cl3 / b$cl3, KETOROLAC_PREGNANCY_FACTOR, tolerance = 1e-6)
  # the rate constants, and so the half-lives, are unchanged
  same <- c("k10", "k12", "k13", "k21", "k31", "lambda_1", "lambda_2",
            "lambda_3", "ka_PO", "bioavailability_PO", "tlag_PO")
  expect_true(all(same %in% names(a)))
  expect_equal(a[same], b[same], tolerance = 1e-12)
  r <- on$parallelSystems[[1]]$PK[[1]]; s <- off$parallelSystems[[1]]$PK[[1]]
  for (p in c("v1", "v2", "cl1", "cl2"))
    expect_equal(r[[p]] / s[[p]], KETOROLAC_PREGNANCY_FACTOR, tolerance = 1e-6)
  # the R system's placeholder third volume, with no clearance to it, stays 1
  rSame <- c("v3", "cl3", "k10", "k12", "k21", "lambda_1", "lambda_2")
  expect_true(all(rSame %in% names(r)))
  expect_equal(r[rSame], s[rSame], tolerance = 1e-12)
  expect_match(on$reference, "Kulo", fixed = TRUE)
  expect_no_match(off$reference, "Kulo", fixed = TRUE)
  # The values the model gives with the flag on
  actual <- ketorolac(70, 165, 30, "female", pregnant = TRUE)$PK$default
  expect_equal_rounded(4.872844596, actual$v1)
  expect_equal_rounded(52.3558737, actual$v2)
  expect_equal_rounded(7.13394122, actual$v3)
  expect_equal_rounded(0.08443455278, actual$cl1)
  expect_equal_rounded(0.03955875773, actual$cl2)
  expect_equal_rounded(0.4189825415, actual$cl3)
})
