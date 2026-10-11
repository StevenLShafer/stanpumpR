# Naproxen: Valitalo 2012's two-compartment apparent oral model, re-centred
# allometrically on its median child (20 kg), with Bjornsson 2011's analgesic
# EC50 as the plasma threshold.  See the header of R/drugs_naproxen.R.  Pins
# were worked out from the published numbers by hand (Python), not from the
# code under test.

noEvents <- data.frame(Time = numeric(0), Event = character(0))

test_that("returns the re-centred parameters with the fat-free-mass switch off", {
  # 70 kg: Valitalo's values at 20 kg, scaled by (70/20)^0.75 for
  # clearances and 70/20 for volumes
  actual <- naproxen(70, 171, 50, "male", adjustToFFM = FALSE)
  expected <- list(
    PK = list(
      default = list(
        v1 = 8.2,
        v2 = 4.3,
        v3 = 1,
        cl1 = 0.0075548079,
        cl2 = 0.0059707353,
        cl3 = 0,
        ka_PO = 0.0183333333,
        bioavailability_PO = 1,
        tlag_PO = 0
      )
    ),
    tPeak = 0,
    MEAC = 0,
    typical = 0,
    upperTypical = 0,
    lowerTypical = 0,
    reference = actual$reference
  )
  expect_equal_rounded(actual, expected)
})

test_that("a 20 kg child with the switch off receives Valitalo's Table II exactly", {
  x <- naproxen(20, 110, 5, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(x$cl1 * 60, 0.62 * 20 / 70)      # CL/F, linear in weight
  expect_equal(x$v1, 8.2 * 20 / 70)
  expect_equal(x$v2, 4.3 * 20 / 70)
  expect_equal(x$cl2 * 60, 0.14)                # Q/F, not weight-scaled
  expect_equal(x$ka_PO * 60, 1.1)
})

test_that("scales to fat-free mass for a 120 kg man", {
  # 120 kg, 170 cm, 50 y male: FFM 71.09 kg; volumes x 1.3049067,
  # clearances x 1.2209126 on the 70 kg values
  actual <- naproxen(120, 170, 50, "male")
  expected <- list(
    v1 = 10.7002344983,
    v2 = 5.6110985784,
    v3 = 1,
    cl1 = 0.0092237603,
    cl2 = 0.0072897461,
    cl3 = 0
  )
  expect_equal_rounded(actual$PK$default[names(expected)], expected)
  # With the switch off, allometry on total weight
  off <- naproxen(120, 170, 50, "male", adjustToFFM = FALSE)$PK$default
  expect_equal_rounded(off[c("v1", "v2", "cl1", "cl2")],
                       list(v1 = 14.0571428571, v2 = 7.3714285714,
                            cl1 = 0.0113184207, cl2 = 0.0089452035))
  # The reference man is the same in both positions
  expect_equal(naproxen(70, 170, 35, "male")$PK$default,
               naproxen(70, 170, 35, "male", adjustToFFM = FALSE)$PK$default)
})

test_that("the reference man's half-lives are 4.6 h and 23 h", {
  x <- naproxen(70, 170, 35, "male")$PK$default
  k10 <- x$cl1 / x$v1; k12 <- x$cl2 / x$v1; k21 <- x$cl2 / x$v2
  s <- k10 + k12 + k21; r <- sqrt(s^2 - 4 * k10 * k21)
  expect_equal(log(2) / c((s + r) / 2, (s - r) / 2) / 60, c(4.561, 22.873),
               tolerance = 1e-3)
})

test_that("the threshold is Bjornsson's unbound EC50 as total naproxen", {
  # EC50 0.135 umol/L unbound; Ctot = Cu + Bmax Cu / (Km + Cu) with Bmax
  # 643 and Km 0.549 umol/L; naproxen 230.26 g/mol
  cu <- 0.135
  total <- (cu + 643 * cu / (0.549 + cu)) * 230.26 / 1000
  expect_equal(total, 29.25, tolerance = 1e-3)
  dd <- getDrugDefaults("naproxen")
  expect_equal(dd$endCe, 29)
  expect_equal(c(dd$Lower, dd$Upper, dd$Typical, dd$MEAC), c(0, 0, 0, 0))
  expect_equal(dd$Category, "Oral analgesics")
})

# Independent check: integrate the two-compartment model with first-order
# oral input by fourth-order Runge-Kutta, from the published parameters, and
# compare with the closed-form engine at its own time points.
rk4Naproxen <- function(times, dosePO, dt = 0.5)
{
  v1 <- 8.2; v2 <- 4.3
  cl <- 0.62 * (20 / 70)^0.25 / 60; q <- 0.14 * (70 / 20)^0.75 / 60
  ka <- 1.1 / 60
  d <- function(s) c(-ka * s[1],
                     ka * s[1] - (cl + q) * s[2] / v1 + q * s[3] / v2,
                     q * s[2] / v1 - q * s[3] / v2)
  s <- c(dosePO, 0, 0); t <- 0
  out <- numeric(length(times))
  for (i in seq_along(times)) {
    while (t < times[i] - 1e-12) {
      h <- min(dt, times[i] - t)
      k1 <- d(s); k2 <- d(s + h / 2 * k1); k3 <- d(s + h / 2 * k2); k4 <- d(s + h * k3)
      s <- s + h * (k1 + 2 * k2 + 2 * k3 + k4) / 6
      t <- t + h
    }
    out[i] <- s[2] / v1
  }
  out
}

test_that("500 mg by mouth matches an independent integration", {
  po <- simulateDrugsWithCovariates(
    data.frame(Drug = "naproxen", Time = 0, Dose = 500, Units = "mg PO"),
    noEvents, 70, 170, 35, "male", 2880, TRUE)$naproxen$wide
  expect_equal(po$Plasma, rk4Naproxen(po$Time, 500), tolerance = 1e-6)
  # No effect site: the effect-site column is not plotted
  expect_true(all(is.na(po$"Effect Site")) || is.null(po$"Effect Site"))
  # 48 mg/L at about 2.5 h; AUC 1103 mg.h/L
  expect_gt(max(po$Plasma), 47.5); expect_lt(max(po$Plasma), 48.5)
  x <- naproxen(70, 170, 35, "male")$PK$default
  expect_equal(500 / (x$cl1 * 60), 1103.05, tolerance = 1e-4)
})


# The Oral analgesics scenario (inst/help/scenarios/naproxen-twice-daily.md)
# quotes these numbers; if the model changes, the narrative has to change
# with it.  The curve is the two-compartment oral solution written out from
# the model's own parameters, checked against the simulation at its time
# points; peaks and crossings are found on it.

naproxenCurves <- function(p) {
  pk <- getDrugPK("naproxen", p$weight, p$height, p$age, p$sex,
                  adjustToFFM = p$adjustToFFM)$PK$default
  k10 <- pk$cl1 / pk$v1; k12 <- pk$cl2 / pk$v1; k21 <- pk$cl2 / pk$v2
  s <- k10 + k12 + k21; r <- sqrt(s^2 - 4 * k10 * k21)
  al <- (s + r) / 2; be <- (s - r) / 2
  A <- (al - k21) / (pk$v1 * (al - be)); B <- (k21 - be) / (pk$v1 * (al - be))
  ka <- pk$ka_PO; lam <- c(al, be, ka)
  coef <- ka * c(A / (ka - al), B / (ka - be)); coef <- c(coef, -sum(coef))
  one <- function(t) { s <- pmax(t, 0); colSums(coef * exp(-outer(lam, s))) }
  list(pk = pk, beta = be,
       doses = function(D, times) function(t) Reduce(`+`, lapply(times, function(t0) D * one(t - t0))))
}

test_that("the twice-daily scenario says what the model does", {
  s <- helpScenarioById("naproxen-twice-daily")
  p <- s$patient
  x <- naproxenCurves(p)
  expect_true(s$options$showThreshold)
  bid <- seq(0, 84 * 60, by = 720)                 # 0 to 84 h
  cb <- x$doses(500, bid)
  out <- simulateDrugsWithCovariates(s$doses, noEvents, p$weight, p$height,
                                     p$age, p$sex, s$options$maximum, TRUE,
                                     adjustToFFM = p$adjustToFFM)$naproxen$wide
  expect_equal(out$Plasma, cb(out$Time), tolerance = 1e-8)

  thr <- getDrugDefaults("naproxen")$endCe
  first <- x$doses(500, 0)
  pk1 <- stats::optimize(first, c(0, 600), maximum = TRUE)
  expect_equal(c(pk1$objective, pk1$maximum), c(48.34, 147.6), tolerance = 1e-3)
  up <- stats::uniroot(function(t) first(t) - thr, c(1, pk1$maximum))$root
  dn <- stats::uniroot(function(t) first(t) - thr, c(pk1$maximum, 720))$root
  expect_equal(c(up, dn), c(36.9, 597.2), tolerance = 1e-3)
  back <- stats::uniroot(function(t) cb(t) - thr, c(720, 900))$root
  expect_equal(back - 720, 3.4, tolerance = 0.03)
  expect_equal((back - dn) / 60, 2.1, tolerance = 0.02)        # two hours below

  # Troughs before the 2nd to 5th doses, and the last peak
  troughs <- cb(bid[2:5] - 1e-6)
  expect_equal(troughs, c(25.44, 39.68, 49.02, 55.43), tolerance = 1e-3)
  expect_equal(cb(60 * 60 - 1e-6), 59.86, tolerance = 1e-3)
  expect_true(all(cb(seq(800, 5760, 1)) > thr))                 # never below again
  last <- stats::optimize(cb, c(84 * 60, 84 * 60 + 600), maximum = TRUE)
  expect_equal(c(last$objective, last$maximum / 60), c(107.43, 86.1), tolerance = 1e-3)

  # Daily averages against the steady state, F x dose rate / CL
  ss <- 500 / (x$pk$cl1 * 720)
  expect_equal(ss, 91.92, tolerance = 1e-3)
  expect_equal(x$pk$cl1 * 60, 0.453, tolerance = 1e-3)
  daily <- sapply(1:3, function(d) mean(cb(seq((d - 1) * 1440, d * 1440 - 1, 1)))) / ss
  expect_equal(daily, c(0.50, 0.77, 0.89), tolerance = 0.01)
  expect_equal(log(2) / x$beta / 60, 22.87, tolerance = 1e-3)

  # Time until threshold at the last peak: 33.6 h; the app agrees
  below <- stats::uniroot(function(t) cb(t) - thr, c(last$maximum, 20000))$root
  expect_equal((below - last$maximum) / 60, 33.6, tolerance = 2e-3)
  late <- out$Time > 84 * 60 & !is.na(out$Recovery)
  expect_true(any(late))
  expect_equal(out$Recovery[late], below - out$Time[late], tolerance = 1e-3)

  # Try next: 250 mg qid
  cq <- x$doses(250, seq(0, 90 * 60, by = 360))
  lastq <- cq(seq(84 * 60, 96 * 60, 1)); lastb <- cb(seq(84 * 60, 96 * 60, 1))
  expect_equal(range(lastq), c(76.49, 94.38), tolerance = 1e-3)
  expect_equal(range(lastb), c(65.09, 107.43), tolerance = 1e-3)
  expect_equal(stats::optimize(x$doses(250, 0), c(0, 600), maximum = TRUE)$objective,
               24.17, tolerance = 1e-3)
  expect_false(any(x$doses(250, 0)(seq(0, 2880, 1)) >= thr))
  crossq <- stats::uniroot(function(t) cq(t) - thr, c(361, 600))$root
  expect_equal(crossq, 382.8, tolerance = 1e-3)

  # Try next: a row of 0 mg PO bid at 2 days stops the repeats; the last
  # dose is at 36 h, and the app's readout at 2 days is 17 h
  stopDoses <- rbind(s$doses, data.frame(Drug = "naproxen", Time = 2880, Dose = 0,
                                         Units = "mg PO bid"))
  outStop <- simulateDrugsWithCovariates(stopDoses, noEvents, p$weight, p$height,
                                         p$age, p$sex, s$options$maximum, TRUE,
                                         adjustToFFM = p$adjustToFFM)$naproxen$wide
  cs <- x$doses(500, seq(0, 36 * 60, by = 720))
  expect_equal(outStop$Plasma, cs(outStop$Time), tolerance = 1e-8)
  expect_equal(cs(2880), 55.43, tolerance = 1e-3)
  belowStop <- stats::uniroot(function(t) cs(t) - thr, c(2880, 6000))$root
  expect_equal(belowStop / 60, 65.11, tolerance = 1e-3)
  at2 <- outStop$Time == 2880
  expect_true(any(at2))
  expect_equal(outStop$Recovery[at2], belowStop - 2880, tolerance = 1e-3)
  expect_equal((belowStop - 2880) / 60, 17.11, tolerance = 1e-3)
})
