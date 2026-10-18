# Fluindione: Verstuyft 2012 apparent two-compartment PK and the indirect-
# response INR model.  See the header of R/drugs_fluindione.R and R/inr.R.

noEvents <- data.frame(Time = numeric(0), Event = character(0))
fluDefaults <- getDrugDefaults("fluindione")

test_that("returns the published parameters at the 69 kg reference subject", {
  actual <- fluindione(69, 170, 40, "male", cyp2c9 = "*1/*1", vkorc1 = "AA")
  expect_equal_rounded(actual$PK$default, list(
    v1 = 5.19, v2 = 2.77, v3 = 1,
    cl1 = 0.17 / 60, cl2 = 0.173 / 60, cl3 = 0,
    ka_PO = 1.83 / 60, tlag_PO = 0, bioavailability_PO = 1
  ))
  expect_equal_rounded(actual$inr, list(
    type = "indirect", c50 = 3.63, gamma = 2.32,
    kin = 0.0385 / 60, kout = 0.0391 / 60))
  expect_null(actual$parallelSystems)
  expect_null(actual$doseFraction)
  expect_equal(actual$MEAC, 0)
})

test_that("CYP2C9 *3 carriage lowers clearance; the rest act as wild type", {
  base <- fluindione(69, 170, 40, "male", cyp2c9 = "*1/*1", vkorc1 = "AA")
  for (g in c("*1/*1", "*1/*2", "*2/*2"))
    expect_equal(fluindione(69, 170, 40, "male", cyp2c9 = g)$PK$default$cl1,
                 base$PK$default$cl1, info = g)
  for (g in c("*1/*3", "*2/*3", "*3/*3"))
    expect_equal(fluindione(69, 170, 40, "male", cyp2c9 = g)$PK$default$cl1,
                 0.17 * (1 - 0.363) / 60, tolerance = 1e-9, info = g)
})

test_that("VKORC1 raises the C50 for GG or GA; AA is the sensitive reference", {
  expect_equal(fluindione(69, 170, 40, "male", vkorc1 = "AA")$inr$c50, 3.63)
  expect_equal(fluindione(69, 170, 40, "male", vkorc1 = "GG")$inr$c50, 3.63 * 1.3)
  expect_equal(fluindione(69, 170, 40, "male", vkorc1 = "GA")$inr$c50, 3.63 * 1.3)
})

test_that("body weight scales V1 and CL, in both adjustToFFM positions", {
  for (ffm in c(TRUE, FALSE)) {
    m <- fluindione(90, 180, 40, "male", adjustToFFM = ffm)
    expect_equal(m$PK$default$v1, 5.19 * (90 / 69), tolerance = 1e-9)
    expect_equal(m$PK$default$cl1, 0.17 * (90 / 69)^0.75 / 60, tolerance = 1e-9)
  }
})

test_that("invalid genotypes are rejected", {
  expect_error(fluindione(69, 170, 40, "male", cyp2c9 = "*9/*9"), "cyp2c9")
  expect_error(fluindione(69, 170, 40, "male", vkorc1 = "ZZ"), "vkorc1")
})

test_that("the simulated INR starts near 1, rises, and has no effect site", {
  dose <- data.frame(Drug = "fluindione", Time = 0, Dose = 20, Units = "mg PO qd",
                     stringsAsFactors = FALSE)
  X <- simulateDrugsWithCovariates(dose, noEvents, 69, 170, 40, "male",
                                   maximum = 20 * 1440, plotRecovery = FALSE,
                                   cyp2c9 = "*1/*1", vkorc1 = "AA")
  r <- X$fluindione$results
  p <- r[r$Site == "Plasma", ]
  expect_equal(p$Y[1], 0.0391 / 0.0385, tolerance = 1e-6)   # kout/kin baseline
  expect_gt(max(p$Y), 2)                                    # it climbs
  ce <- r[r$Site == "Effect Site", ]
  expect_true(all(is.na(ce$Y)))                             # no effect site
})

test_that("the steady-state INR follows Table IV's covariate directions", {
  endOf <- function(cyp, vk) {
    dose <- data.frame(Drug = "fluindione", Time = 0, Dose = 20, Units = "mg PO qd",
                       stringsAsFactors = FALSE)
    X <- simulateDrugsWithCovariates(dose, noEvents, 69, 170, 40, "male",
                                     maximum = 20 * 1440, plotRecovery = FALSE,
                                     cyp2c9 = cyp, vkorc1 = vk)
    p <- X$fluindione$results
    max(p$Y[p$Site == "Plasma"])
  }
  # *3 carrier > wild type; the sensitive AA > the less-sensitive GG.
  expect_gt(endOf("*1/*3", "AA"), endOf("*1/*1", "AA"))
  expect_gt(endOf("*1/*3", "AA"), endOf("*1/*3", "GG"))
  # Fluindione accumulates, so its wild-type steady-state INR is close to the
  # paper's Table IV value of 2.9.
  expect_equal(endOf("*1/*1", "AA"), 2.9, tolerance = 0.3)
})
