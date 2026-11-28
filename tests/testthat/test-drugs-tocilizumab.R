# tocilizumab: see R/drugs_tocilizumab.R (Moes 2022, Table 3), run by the
# numerical engine, R/advanceMichaelisMenten.R: one compartment, linear plus
# Michaelis-Menten elimination, intravenous only.
noEvents <- data.frame(Time = numeric(0), Event = character(0))

test_that("Table 3 with the switch off, whatever the patient", {
  for (p in list(list(96.4, 177, 64, "male"), list(60, 160, 30, "female"),
                 list(130, 190, 80, "male"))) {
    X <- do.call(tocilizumab, c(p, adjustToFFM = FALSE))
    pk <- X$PK$default
    mm <- X$michaelisMenten
    expect_equal(pk$v1, 4.34)
    expect_equal(mm$clLinear * MINS_PER_DAY, 0.725)
    expect_equal(mm$vmax * MINS_PER_DAY, 4.19)
    expect_equal(mm$km, 0.22)
    expect_equal(pk$cl1, mm$clLinear + mm$vmax / mm$km)
    expect_equal(c(pk$v2, pk$v3, pk$cl2, pk$cl3), c(1, 1, 0, 0))
    expect_equal(c(X$tPeak, X$MEAC, X$typical, X$lowerTypical, X$upperTypical), rep(0, 5))
  }
  # The paper's threshold above which linear clearance exceeds the saturable
  # pathway: Vmax/CL - Km, "about 5 mcg/mL" (Figure 6)
  expect_equal(round(4.19 / 0.725 - 0.22, 2), 5.56)
})

test_that("switch on: V, CL and Vmax by fat-free mass, anchored at Moes's cohort", {
  cohort <- cohortFactorsIndependent(list(weight = 96.4, height = 177, age = 64,
                                          men = 21, women = 8))
  for (p in list(list(96.4, 177, 64, "male"), list(60, 160, 30, "female"),
                 list(70, 170, 35, "male"))) {
    size <- do.call(pkSizeFactors, c(p, adjustToFFM = TRUE))
    X <- do.call(tocilizumab, c(p, adjustToFFM = TRUE))
    expect_equal(X$PK$default$v1, 4.34 * size$volume / cohort[["volume"]])
    expect_equal(X$michaelisMenten$clLinear * MINS_PER_DAY,
                 0.725 * size$clearance / cohort[["clearance"]])
    expect_equal(X$michaelisMenten$vmax * MINS_PER_DAY,
                 4.19 * size$clearance / cohort[["clearance"]])
    expect_equal(X$michaelisMenten$km, 0.22)   # a concentration: not scaled
  }
  # Pins at the reference man
  X <- tocilizumab(70, 170, 35, "male", TRUE)
  expect_equal_rounded(X$PK$default$v1, 3.717179)
  expect_equal_rounded(X$michaelisMenten$clLinear * MINS_PER_DAY, 0.645477)
  expect_equal_rounded(X$michaelisMenten$vmax * MINS_PER_DAY, 3.730412)
})

test_that("finite and positive across the legal patients", {
  for (sex in c("male", "female")) for (wt in c(0.5, 3, 40, 96.4, 250))
    for (age in c(0, 40, 90)) for (adj in c(TRUE, FALSE)) {
      X <- tocilizumab(wt, 170, age, sex, adj)
      v <- c(X$PK$default$v1, X$michaelisMenten$vmax, X$michaelisMenten$clLinear,
             X$michaelisMenten$km)
      expect_true(all(is.finite(v) & v > 0))
    }
})

# An independent solution of the source's equations (Moes 2022, Figure 1:
# dA/dt = input - CL C - Vmax C/(Km + C), C = A/V), by classical Runge-Kutta
# in days, the dose infused at a constant rate over `inf` days (a whole
# number of steps, so the end of the infusion falls on one).
rk4Tocilizumab <- function(dose, inf, until, v = 4.34, cl = 0.725, vmax = 4.19,
                           km = 0.22, h = 1 / 2880) {
  f <- function(a, rate) {
    c <- a / v
    rate - cl * c - vmax * c / (km + c)
  }
  t <- seq(0, until, by = h)
  out <- numeric(length(t)); a <- 0
  for (i in seq_along(t)) {
    out[i] <- a / v
    # the rate is held over each whole step, so the infusion ends on one
    rate <- if (t[i] < inf - h / 2) dose / inf else 0
    k1 <- f(a, rate); k2 <- f(a + h / 2 * k1, rate)
    k3 <- f(a + h / 2 * k2, rate); k4 <- f(a + h * k3, rate)
    a <- max(a + h / 6 * (k1 + 2 * k2 + 2 * k3 + k4), 0)
  }
  list(t = t, C = out)
}

test_that("the engine reproduces the source's equations: 781 mg over an hour", {
  PK <- getDrugPK("tocilizumab", 96.4, 177, 64, "male", adjustToFFM = FALSE)
  d <- data.frame(Drug = "tocilizumab", Time = c(0, 60), Dose = c(781, 0),
                  Units = "mg/hr")
  w <- simCpCe(d, noEvents, PK, 30 * MINS_PER_DAY, FALSE)$wide
  ref <- rk4Tocilizumab(781, 1 / 24, 30)
  tt <- w$Time / MINS_PER_DAY
  keep <- tt <= 30
  err <- w$Plasma[keep] - approx(ref$t, ref$C, tt[keep])$y
  expect_lt(max(abs(err)) / max(ref$C), 1e-5)
  expect_true(all(is.na(w$`Effect Site`)))
  # Pins against the source and the label (header, CHECKS): peak 179 mcg/mL
  # (label's COVID-19 median after 8 mg/kg 151, range 77.5-319); 12.2 at day
  # 14; below 1 mcg/mL at day 20 (Figure 2: about days 13-22)
  expect_equal(round(max(w$Plasma)), 179)
  expect_equal(round(approx(tt, w$Plasma, 14)$y, 1), 12.2)
  expect_equal(floor(tt[max(which(w$Plasma >= 1))]), 20)
  # AUC(0-inf): Moes's mean 938 +/- 190 mcg/mL.day; the model 958
  auc <- sum(diff(ref$t) * (head(ref$C, -1) + tail(ref$C, -1)) / 2)
  expect_equal(round(auc), 958)
})

test_that("8 mg/kg and the hourly rate units reach the engine as mg", {
  PK <- getDrugPK("tocilizumab", 80, 175, 60, "male", adjustToFFM = FALSE)
  a <- data.frame(Drug = "tocilizumab", Time = c(0, 60), Dose = c(8, 0),
                  Units = "mg/kg/hr")
  b <- data.frame(Drug = "tocilizumab", Time = c(0, 60), Dose = c(640, 0),
                  Units = "mg/hr")
  wa <- simCpCe(a, noEvents, PK, 10 * MINS_PER_DAY, FALSE)$wide
  wb <- simCpCe(b, noEvents, PK, 10 * MINS_PER_DAY, FALSE)$wide
  expect_equal(wa$Plasma, wb$Plasma)
  expect_true(drugOnSaturableEngine("tocilizumab"))
})
