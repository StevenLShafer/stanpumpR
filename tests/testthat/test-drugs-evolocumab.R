# evolocumab: see R/drugs_evolocumab.R (Kuchimanchi 2018, Table 3), run by
# the numerical engine, R/advanceMichaelisMenten.R, with its subcutaneous
# depot written as the first-order case of the saturable one.
noEvents <- data.frame(Time = numeric(0), Event = character(0))
MW <- 144000                      # g/mol, Repatha label
nM <- MW * 1e-6                   # mg/L per nM

test_that("Table 3 at the paper's reference patient (84 kg man, switch off)", {
  X <- evolocumab(84, 175, 50, "male", adjustToFFM = FALSE)
  pk <- X$PK$default
  mm <- X$michaelisMenten
  expect_equal(pk$v1, 5.18)
  expect_equal(mm$clLinear * MINS_PER_DAY, 0.105)
  expect_equal(mm$km, 27.3 * nM)
  expect_equal(round(mm$km, 3), 3.931)               # "approximately 4 ug/mL"
  # Vmax is a concentration rate: 9.85 nM/day x V, in mg/min
  expect_equal(mm$vmax * MINS_PER_DAY, 9.85 * nM * 5.18)
  expect_equal(round(mm$vmax * MINS_PER_DAY, 2), 7.35)
  expect_equal(pk$cl1, mm$clLinear + mm$vmax / mm$km)
  expect_equal(c(pk$v2, pk$v3, pk$cl2, pk$cl3), c(1, 1, 0, 0))
  expect_equal(c(X$tPeak, X$MEAC, X$typical, X$lowerTypical, X$upperTypical), rep(0, 5))
  # The source's two derived half-lives (Discussion): 1.9 days below km
  # (Vmax / km) and 34 days when saturated (V / CL)
  expect_equal(round(log(2) / (9.85 / 27.3), 2), 1.92)
  expect_equal(round(log(2) * pk$v1 / (mm$clLinear * MINS_PER_DAY), 1), 34.2)
  # The first-order SC depot as the degenerate saturable one
  sc <- mm$saturableSC
  expect_equal(c(sc$fraction, sc$bioavailability), c(1, 1))
  expect_equal(sc$kaMax / sc$kmKa * MINS_PER_DAY, 0.319)
  expect_gte(sc$kmKa, 1e12)
  expect_equal(mm$saltFactor$SC, 0.72)
})

test_that("women: V x 1.11; weight on CL, V and Vmax about 84 kg", {
  m <- evolocumab(84, 175, 50, "male", FALSE)
  f <- evolocumab(84, 165, 50, "female", FALSE)
  expect_equal(f$PK$default$v1, 5.18 * 1.11)
  expect_equal(f$michaelisMenten$clLinear, m$michaelisMenten$clLinear)
  # Vmax x V: the concentration rate is unchanged, the amount rate follows V
  expect_equal(f$michaelisMenten$vmax / f$PK$default$v1,
               m$michaelisMenten$vmax / m$PK$default$v1)
  h <- evolocumab(120, 180, 50, "male", FALSE)
  expect_equal(h$PK$default$v1, 5.18 * (120 / 84)^1.04)
  expect_equal(h$michaelisMenten$clLinear * MINS_PER_DAY, 0.105 * (120 / 84)^0.276)
  expect_equal(h$michaelisMenten$vmax * MINS_PER_DAY,
               9.85 * (120 / 84)^0.145 * nM * 5.18 * (120 / 84)^1.04)
  # a child is extrapolated, not held at the studied 41 kg
  k <- evolocumab(20, 115, 7, "male", FALSE)
  expect_equal(k$PK$default$v1, 5.18 * (20 / 84)^1.04)
})

test_that("switch on: the weight terms read the pharmacokinetic weight", {
  size <- pkSizeFactors(100, 170, 50, "female", TRUE)
  X <- evolocumab(100, 170, 50, "female", TRUE)
  w <- size$pkWeight / 84
  expect_equal(X$PK$default$v1, 5.18 * w^1.04 * 1.11)
  expect_equal(X$michaelisMenten$clLinear * MINS_PER_DAY, 0.105 * w^0.276)
  expect_equal(X$michaelisMenten$vmax * MINS_PER_DAY,
               9.85 * w^0.145 * nM * X$PK$default$v1)
  expect_equal_rounded(evolocumab(70, 170, 35, "male", TRUE)$PK$default$v1,
                       5.18 * (70 / 84)^1.04)
})

test_that("finite and positive across the legal patients", {
  for (sex in c("male", "female")) for (wt in c(0.5, 3, 40, 84, 250))
    for (adj in c(TRUE, FALSE)) {
      X <- evolocumab(wt, 170, 40, sex, adj)
      v <- c(X$PK$default$v1, X$michaelisMenten$vmax, X$michaelisMenten$clLinear,
             X$michaelisMenten$km)
      expect_true(all(is.finite(v) & v > 0))
    }
})

# An independent solution of the source's equations, in concentration
# (Kuchimanchi 2018: one compartment, first-order absorption, dC/dt = ka A/V
# - CL/V C - Vmax C/(km + C)), by classical Runge-Kutta in days.
rk4Evolocumab <- function(doseTimes, doses, until, v = 5.18, cl = 0.105,
                          vmaxNM = 9.85, h = 0.002) {
  ka <- 0.319; km <- 27.3 * nM; vm <- vmaxNM * nM
  f <- function(s) c(-ka * s[1], ka * s[1] / v - cl / v * s[2] - vm * s[2] / (km + s[2]))
  t <- seq(0, until, by = h)
  out <- numeric(length(t)); s <- c(0, 0)
  for (i in seq_along(t)) {
    hit <- abs(doseTimes - t[i]) < h / 2
    if (any(hit)) s[1] <- s[1] + 0.72 * sum(doses[hit])
    out[i] <- s[2]
    k1 <- f(s); k2 <- f(s + h / 2 * k1); k3 <- f(s + h / 2 * k2); k4 <- f(s + h * k3)
    s <- s + h / 6 * (k1 + 2 * k2 + 2 * k3 + k4)
  }
  list(t = t, C = out)
}

test_that("the engine reproduces the source's equations: 140 mg and 420 mg once", {
  PK <- getDrugPK("evolocumab", 84, 175, 50, "male", adjustToFFM = FALSE)
  expect_equal(PK$tissueRoute, ROUTE_SC)
  for (dose in c(140, 420)) {
    d <- data.frame(Drug = "evolocumab", Time = 0, Dose = dose, Units = "mg SC")
    w <- simCpCe(d, noEvents, PK, 60 * MINS_PER_DAY, FALSE)$wide
    ref <- rk4Evolocumab(0, dose, 60)
    # Compared at the engine's own points (between them the plot is a line,
    # which is no part of this check): within 1e-5 of the peak
    tt <- w$Time / MINS_PER_DAY
    keep <- tt <= 60
    err <- w$Plasma[keep] - approx(ref$t, ref$C, tt[keep])$y
    expect_lt(max(abs(err)) / max(ref$C), 1e-5)
    expect_true(all(is.na(w$`Effect Site`)))
  }
  # Pins (reference patient, no statin): peak 10.36 ug/mL at 5.05 days after
  # 140 mg, 39.53 at 6.85 days after 420 mg; the label's healthy volunteers
  # peaked higher (18.6 and 59.0 ug/mL), which the header records.
  d <- data.frame(Drug = "evolocumab", Time = 0, Dose = 140, Units = "mg SC")
  w <- simCpCe(d, noEvents, PK, 60 * MINS_PER_DAY, FALSE)$wide
  expect_equal(round(max(w$Plasma), 1), 10.4)
})

test_that("140 mg every 2 weeks: the doses add on the saturable engine", {
  PK <- getDrugPK("evolocumab", 84, 175, 50, "male", adjustToFFM = FALSE)
  times <- seq(0, by = 14, length.out = 7)
  d <- data.frame(Drug = "evolocumab", Time = times * MINS_PER_DAY, Dose = 140,
                  Units = "mg SC")
  w <- simCpCe(d, noEvents, PK, 100 * MINS_PER_DAY, FALSE)$wide
  ref <- rk4Evolocumab(times, rep(140, 7), 100)
  tt <- w$Time / MINS_PER_DAY
  # the dose times themselves are steps: compare everywhere else
  keep <- tt <= 100 & !vapply(tt, function(x) any(abs(x - times) < 0.01), logical(1))
  err <- w$Plasma[keep] - approx(ref$t, ref$C, tt[keep])$y
  expect_lt(max(abs(err)) / max(ref$C), 1e-5)
  # Week-12 trough 8.5 ug/mL without a statin (label, phase 3 on statins: 7.21)
  expect_equal(round(approx(ref$t, ref$C, 84 - 0.002)$y, 1), 8.5)
  expect_equal(round(min(w$Plasma[tt < 84 & tt > 83.5]), 1), 8.5)
})
