# Bivalirudin: Zhang 2012 two-compartment per-kilogram model in healthy
# volunteers.  See the header of R/drugs_bivalirudin.R.

noEvents <- data.frame(Time = numeric(0), Event = character(0))

test_that("returns the published per-kilogram parameters with the switch off", {
  # Switch off: proportional to total body weight, 70 kg here.
  actual <- bivalirudin(70, 170, 50, "male", adjustToFFM = FALSE)
  expected <- list(
    PK = list(default = list(
      v1 = 0.086 * 70, v2 = 0.0554 * 70, v3 = 1,
      cl1 = 0.323 * 70 / 60, cl2 = 0.0957 * 70 / 60, cl3 = 0
    )),
    tPeak = 0, MEAC = 0,
    typical = 0, upperTypical = 0, lowerTypical = 0,
    reference = actual$reference
  )
  expect_equal_rounded(actual, expected)
})

test_that("off the switch the parameters are proportional to body weight", {
  a <- bivalirudin(100, 180, 40, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(a$v1, 0.086 * 100)
  expect_equal(a$cl1, 0.323 * 100 / 60)
})

test_that("scales to fat-free mass for a 120 kg man", {
  # 120 kg, 170 cm, 50 y male: volumes x 1.3049067, clearances x 1.2209126,
  # relative to the reference-man values (70 x the per-kg numbers)
  actual <- bivalirudin(120, 170, 50, "male")$PK$default
  expected <- list(
    v1 = 0.086 * 70 * 1.3049067,
    cl1 = 0.323 * 70 / 60 * 1.2209126
  )
  expect_equal_rounded(actual[names(expected)], expected)
})

test_that("the PCI regimen gives concentrations near the study's range", {
  # 0.75 mg/kg bolus then 1.75 mg/kg/h for 4 h; EC50 was 2.44 mg/L, so the
  # bolus peak is several mg/L.
  dose <- data.frame(
    Drug = "bivalirudin",
    Time = c(0, 0, 240),
    Dose = c(0.75, 1.75, 0),
    Units = c("mg/kg", "mg/kg/hr", "mg/kg/hr"))
  r <- simulateDrugsWithCovariates(dose, noEvents, 70, 170, 50, "male",
                                   300, TRUE, adjustToFFM = FALSE)$bivalirudin
  cp <- r$results$Y[r$results$Site == "Plasma"]
  expect_equal(max(cp), 0.75 * 70 / (0.086 * 70), tolerance = 1e-6)  # bolus peak
  expect_true(all(cp >= 0))
})

test_that("the half-lives at 70 kg are the ones the header and help page state", {
  # Two compartments: 7.7 and 35 min from CL 22.6 L/h, V1 6.02 L, Q 6.70 L/h,
  # V2 3.88 L (the label's terminal half-life is 25 min).
  p <- bivalirudin(70, 170, 40, "male", adjustToFFM = FALSE)$PK$default
  k10 <- p$cl1 / p$v1; k12 <- p$cl2 / p$v1; k21 <- p$cl2 / p$v2
  s <- k10 + k12 + k21; d <- sqrt(s^2 - 4 * k10 * k21)
  expect_equal(log(2) / c((s + d) / 2, (s - d) / 2), c(7.71, 34.57), tolerance = 1e-3)
})
