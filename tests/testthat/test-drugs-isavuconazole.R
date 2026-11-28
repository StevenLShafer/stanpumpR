# Isavuconazole (R/drugs_isavuconazole.R; Desai 2016).  Drafted by Claude
# Code, 2026-11-28.  The expectations are Desai's Table 5, the dose basis, and
# an independent integration of the exact Weibull system, computed here, not
# read from the drug file.

events <- data.frame(Time = numeric(0), Event = character(0), Fill = character(0))
SALT <- 372 / 200

test_that("Desai 2016's Table 5 with the switch off, in sulfate amounts", {
  set <- isavuconazole(90, 160, 70, "female", adjustToFFM = FALSE)$PK$default
  expect_equal(set$cl1 * 60, 2.36 * SALT)
  expect_equal(set$v1, 49.1 * SALT)
  expect_equal(set$cl2 * 60, 26.6 * SALT)
  expect_equal(set$v2, 390 * SALT)
  expect_equal(set$ka_PO * 60, 1.08)
  expect_equal(set$bioavailability_PO, 1)
  # Vp 390 L gives the terminal half-life the Discussion's 130 h supports (138 h)
  k10 <- set$cl1 / set$v1; k12 <- set$cl2 / set$v1; k21 <- set$cl2 / set$v2
  s <- k10 + k12 + k21
  beta <- (s - sqrt(s^2 - 4 * k10 * k21)) / 2
  expect_equal(round(log(2) / beta / 60), 138)
})

test_that("the switch on anchors at Desai's cohort", {
  size <- pkSizeFactors(90, 160, 70, "female", TRUE)
  cohort <- cohortFactorsIndependent(list(groups = list(
    list(weight = 77.8, height = 175, age = 43, men = 140, women = 49),
    list(weight = 67.0, height = 168.4, age = 54, men = 132, women = 100))))
  set <- isavuconazole(90, 160, 70, "female")$PK$default
  expect_equal(set$cl1 * 60, 2.36 * SALT * size$clearance / cohort$clearance)
  expect_equal(set$v2, 390 * SALT * size$volume / cohort$volume)
})

# The exact system: dA/dt = -WB(t) A, WB = 1.08 [1 - exp(-(0.72 t)^4.88)] per
# hour, into two compartments; classical Runge-Kutta at 1/4000 h
weibullExact <- function(set, dose, tmax) {
  ka <- 1.08 / 60; ra <- 0.72 / 60; gam <- 4.88
  k10 <- set$cl1 / set$v1; k12 <- set$cl2 / set$v1; k21 <- set$cl2 / set$v2
  wb <- function(t) ka * (1 - exp(-(ra * t)^gam))
  f <- function(t, s) {
    r <- wb(t) * s[1]
    c(-r, r - (k10 + k12) * s[2] + k21 * s[3], k12 * s[2] - k21 * s[3])
  }
  h <- 60 / 4000; t <- seq(0, tmax, by = h); s <- c(dose, 0, 0)
  out <- numeric(length(t))
  for (i in seq_along(t)) {
    out[i] <- s[2] / set$v1
    k1 <- f(t[i], s); k2 <- f(t[i] + h / 2, s + h / 2 * k1)
    k3 <- f(t[i] + h / 2, s + h / 2 * k2); k4 <- f(t[i] + h, s + h * k3)
    s <- s + h / 6 * (k1 + 2 * k2 + 2 * k3 + k4)
  }
  list(t = t, C = out)
}

test_that("the oral pulses reproduce the Weibull input to the documented standard", {
  expect_equal(sum(ISAVUCONAZOLE_PULSE_FRACTION), 1, tolerance = 1e-6)
  for (p in list(list(70, 170, 35, "male"), list(67, 168.4, 54, "female"),
                 list(110, 180, 60, "male"))) {
    set <- do.call(isavuconazole, p)$PK$default
    ex <- weibullExact(set, 372, 24 * 60)
    dt <- data.frame(Drug = "isavuconazole", Time = 0, Dose = 372, Units = "mg PO")
    w <- simulateDrugsWithCovariates(dt, events, p[[1]], p[[2]], p[[3]], p[[4]],
                                     24 * 60, FALSE)$isavuconazole$wide
    # The exact system read at the engine's own points, as hydrochlorothiazide's
    # test does, so both peaks are sampled alike; the engine's time line has a
    # point at every pulse, where the approximation kinks and the error peaks.
    expect_true(all(ISAVUCONAZOLE_PULSE_DELAY %in% w$Time))
    ref <- approx(ex$t, ex$C, w$Time)$y
    peak <- max(ref)
    expect_lt(max(abs(w$Plasma - ref)) / peak, 0.0053)
    expect_lt(abs(max(w$Plasma) - peak) / peak, 0.0001)
  }
})

test_that("intravenous doses take no pulses; the AUC after 372 mg is 200 mg / CL", {
  dt <- data.frame(Drug = "isavuconazole", Time = c(0, 60), Dose = c(372, 0), Units = "mg/hr")
  w <- simulateDrugsWithCovariates(dt, events, 70, 170, 35, "male", 60 * 24 * 30, FALSE)$isavuconazole$wide
  set <- isavuconazole(70, 170, 35, "male")$PK$default
  auc <- sum(diff(w$Time) * (head(w$Plasma, -1) + tail(w$Plasma, -1)) / 2) / 60   # mg h/L
  expect_equal(auc, 200 / (set$cl1 * 60 / SALT), tolerance = 0.03)
})

test_that("isavuconazole offers the labelled units and no band", {
  dd <- getDrugDefaultsGlobal()
  units <- unlist(dd$Units[dd$Drug == "isavuconazole"])
  expect_true(all(c("mg PO", "mg PO qd", "mg PO tid", "mg", "mg/hr") %in% units))
  expect_equal(dd$Upper[dd$Drug == "isavuconazole"], 0)
  expect_equal(dd$Category[dd$Drug == "isavuconazole"], "Other")
})
