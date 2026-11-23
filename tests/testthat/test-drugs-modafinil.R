# Modafinil: R- and S-modafinil, one compartment each, simulated as parallel
# systems and summed (FDA review of NDA 20-717, study CEP-2101, Day 7 means;
# see the header of R/drugs_modafinil.R).  Pins are the published numbers;
# the simulated sum is checked against an independent closed form.

noEvents <- data.frame(Time = numeric(0), Event = character(0))

test_that("returns the published parameters with the fat-free-mass switch off", {
  actual <- modafinil(70, 171, 50, "male", adjustToFFM = FALSE)
  oral <- list(ka_PO = 1.4 / 60, bioavailability_PO = 1, tlag_PO = 0)
  # R-modafinil (Table IV, Day 7): V/F 38 L, CL/F 29 mL/min
  expect_equal_rounded(actual$PK$default, c(list(
    v1 = 38, v2 = 1, v3 = 1, cl1 = 0.029, cl2 = 0, cl3 = 0), oral))
  s <- actual$parallelSystems[[1]]
  expect_identical(s$name, "S-modafinil")
  # S-modafinil (Table III, Day 7): V/F 39 L, CL/F 100 mL/min
  expect_equal_rounded(s$PK$default, c(list(
    v1 = 39, v2 = 1, v3 = 1, cl1 = 0.100, cl2 = 0, cl3 = 0), oral))
  expect_equal(actual$doseFraction, 0.5)
  expect_equal(s$doseFraction, 0.5)
  expect_equal(actual$tPeak, 0)
  expect_equal(actual$MEAC, 0)
  # Hart 2026 Table 5: 1,000-1,700 ng/mL, drawn in mcg/mL
  expect_equal(c(actual$lowerTypical, actual$typical, actual$upperTypical),
               c(1.0, 1.35, 1.7))
  # the half-lives the table prints: R 16 +/- 2 h, S 4.7 +/- 0.8 h
  expect_equal(log(2) * 38 / (0.029 * 60), 15.14, tolerance = 1e-3)
  expect_equal(log(2) * 39 / (0.100 * 60), 4.5055, tolerance = 1e-4)
})

test_that("scales both systems to fat-free mass for a 120 kg man", {
  # 120 kg, 170 cm, 50 y male: volumes x 1.3049067, clearances x 1.2209126
  x <- modafinil(120, 170, 50, "male")
  expect_equal(x$PK$default$v1, 38 * 1.3049067, tolerance = 1e-7)
  expect_equal(x$PK$default$cl1, 0.029 * 1.2209126, tolerance = 1e-7)
  s <- x$parallelSystems[[1]]$PK$default
  expect_equal(s$v1, 39 * 1.3049067, tolerance = 1e-7)
  expect_equal(s$cl1, 0.100 * 1.2209126, tolerance = 1e-7)
  # placeholders are not scaled
  expect_equal(c(s$v2, s$v3, s$cl2, s$cl3), c(1, 1, 0, 0))
})

# The sum of two one-compartment systems with first-order absorption, each
# given half of every dose: the Bateman function, superposed over doses.
modafinilClosedForm <- function(tMin, doseMg, doseTimesMin = 0)
{
  ka <- 1.4 / 60
  total <- 0
  for (p in list(c(v = 38, cl = 0.029), c(v = 39, cl = 0.100))) {
    k <- p[["cl"]] / p[["v"]]
    for (t0 in doseTimesMin) {
      s <- pmax(tMin - t0, 0)
      total <- total + doseMg / 2 * ka / (p[["v"]] * (ka - k)) *
        (exp(-k * s) - exp(-ka * s))
    }
  }
  total
}

test_that("200 mg by mouth matches the closed-form sum of R and S", {
  po <- simulateDrugsWithCovariates(
    data.frame(Drug = "modafinil", Time = 0, Dose = 200, Units = "mg PO"),
    noEvents, 70, 170, 35, "male", 2880, FALSE)$modafinil$wide
  expect_equal(po$Plasma, modafinilClosedForm(po$Time, 200), tolerance = 1e-6)
  # the curve itself, from the closed form on a fine grid: peak 4.26 mcg/mL
  # at 2.06 h (CEP-2101 Day 1 observed mean Cmax 4.8; MOD-022's fasted mean
  # curve 3.84 at 2 h), 0.978 at 24 h
  t <- seq(60, 240, by = 0.01)
  c200 <- modafinilClosedForm(t, 200)
  expect_equal(t[which.max(c200)] / 60, 2.064, tolerance = 1e-3)
  expect_equal(max(c200), 4.2607, tolerance = 1e-4)
  expect_equal(modafinilClosedForm(1440, 200), 0.97832, tolerance = 1e-4)
})

test_that("200 mg once daily reaches a trough inside Hart 2026's range", {
  # CEP-2101's observed racemate troughs at 200 mg: 0.91 (Day 2) to 1.73
  # (Day 7) ug/mL; the model's steady state 1.433, 95% of it R-modafinil
  # (the label: 90%)
  doses <- data.frame(Drug = "modafinil", Time = 0, Dose = 200, Units = "mg PO qd")
  x <- simulateDrugsWithCovariates(doses, noEvents, 70, 170, 35, "male",
                                   7 * 1440, FALSE)$modafinil$wide
  cp <- approxfun(x$Time, x$Plasma)
  trough <- cp(7 * 1440)
  expect_equal(trough, modafinilClosedForm(7 * 1440, 200, 1440 * 0:6), tolerance = 1e-6)
  expect_equal(trough, 1.43258, tolerance = 1e-4)
  expect_true(trough > 1.0 && trough < 1.7)
  expect_equal(cp(1440), 0.97832, tolerance = 1e-4)
})
