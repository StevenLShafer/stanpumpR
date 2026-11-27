# Posaconazole (R/drugs_posaconazole.R): two compartments derived from the
# NOXAFIL label.  Drafted by Claude Code, 2026-11-26.  The expectations are
# the label's own numbers, checked against the model's concentrations, not
# against the derivation's arithmetic.

events <- data.frame(Time = numeric(0), Event = character(0), Fill = character(0))

# Concentration (mg/L) at time t (h) after an infusion of `dose` mg over T h
infusionConc <- function(set, dose, T, t) {
  k10 <- set$cl1 / set$v1; k12 <- set$cl2 / set$v1; k21 <- set$cl2 / set$v2
  s <- k10 + k12 + k21
  l <- c(s + sqrt(s^2 - 4 * k10 * k21), s - sqrt(s^2 - 4 * k10 * k21)) / 2  # /min
  A <- c(l[1] - k21, k21 - l[2]) / (l[1] - l[2]) / set$v1
  R <- dose / (T * 60)
  tm <- t * 60; Tm <- T * 60
  sapply(tm, function(x) sum(A * R / l *
    (if (x <= Tm) 1 - exp(-l * x) else exp(-l * (x - Tm)) - exp(-l * x))))
}

test_that("the reference man reproduces the label's four numbers", {
  set <- posaconazole(70, 170, 35, "male")$PK$default
  k10 <- set$cl1 / set$v1; k12 <- set$cl2 / set$v1; k21 <- set$cl2 / set$v2
  s <- k10 + k12 + k21
  lz <- (s - sqrt(s^2 - 4 * k10 * k21)) / 2
  expect_equal(set$cl1 * 60, 7.3)
  expect_equal(log(2) / lz / 60, 27)
  expect_equal(set$v1 + set$v2, 261)
  expect_equal(infusionConc(set, 300, 0.5, 0.5) * 1000, 2840, tolerance = 1e-8)
  expect_equal_rounded(95.12418, set$v1)
  expect_equal_rounded(165.8758, set$v2)
  expect_equal_rounded(34.50275, set$cl2 * 60)
})

test_that("the label's checks the derivation did not use", {
  set <- posaconazole(70, 170, 35, "male")$PK$default
  # Table 19, healthy volunteers: AUC(0-12) 13 000 ng h/mL after 300 mg/30 min
  auc12 <- integrate(function(t) infusionConc(set, 300, 0.5, t), 0, 12,
                     rel.tol = 1e-10)$value * 1000
  expect_equal(auc12, 13000, tolerance = 0.03)
  # Table 20, patients at steady state on 300 mg over 90 min once daily
  ss <- function(t) sum(sapply(0:60, function(n) infusionConc(set, 300, 1.5, t + 24 * n)))
  expect_equal(ss(1.5) * 1000, 3280, tolerance = 0.07)
  expect_equal(ss(24 - 1e-9) * 1000, 1090, tolerance = 0.07)
  # Table 19, patients on day 1: the model overstates the first dose by about half
  expect_equal(infusionConc(set, 300, 1.5, 1.5) * 1000 / 1590, 1.49, tolerance = 0.01)
})

test_that("the simulation engine draws the same curve", {
  dt <- data.frame(Drug = "posaconazole", Time = c(0, 90), Dose = c(200, 0), Units = "mg/hr")
  w <- simulateDrugsWithCovariates(dt, events, 70, 170, 35, "male", 24 * 60, FALSE)$posaconazole$wide
  set <- posaconazole(70, 170, 35, "male")$PK$default
  at <- w$Time %in% c(90, 600, 1440)
  expect_equal(w$Plasma[at], infusionConc(set, 300, 1.5, w$Time[at] / 60), tolerance = 1e-6)
})

test_that("body size scales the derived values", {
  size <- pkSizeFactors(50, 160, 60, "female", TRUE)
  on <- posaconazole(50, 160, 60, "female")$PK$default
  ref <- posaconazole(70, 170, 35, "male")$PK$default
  expect_equal(on$v1, ref$v1 * size$volume)
  expect_equal(on$cl1, ref$cl1 * size$clearance)
  off <- posaconazole(50, 160, 60, "female", adjustToFFM = FALSE)$PK$default
  expect_equal(off$v1, ref$v1 * 50 / 70)
  expect_equal(off$cl1, ref$cl1 * 50 / 70)
})

test_that("posaconazole is offered as the injection only, with no band", {
  dd <- getDrugDefaultsGlobal()
  units <- unlist(dd$Units[dd$Drug == "posaconazole"])
  expect_setequal(units, c("mg", "mg/kg", "mg/hr", "mg/kg/hr"))
  expect_equal(dd$Upper[dd$Drug == "posaconazole"], 0)
})
