# Acetaminophen: Morse 2022 intravenous and fasted-tablet disposition with
# clearance on normal fat mass, and ke0 from Anderson 2001.  See the header of
# R/drugs_acetaminophen.R.  Pins were worked out from the published numbers by
# hand (Python), not from the code under test.

noEvents <- data.frame(Time = numeric(0), Event = character(0))

test_that("returns the published parameters with the fat-free-mass switch off", {
  weight <- 70
  height <- 171
  age <- 50
  sex <- "male"
  # Clearance uses normal fat mass in either switch position (70 kg, 171 cm
  # male: NFM 67.17 kg against Morse's standard 67.45 kg), so it is not
  # exactly 24 L/h here.
  actual <- acetaminophen(weight, height, age, sex, adjustToFFM = FALSE)

  expected <- list(
    PK = list(
      default = list(
        v1 = 43.7,
        v2 = 29.7,
        v3 = 1,
        cl1 = 0.398876879,
        cl2 = 0.725,
        cl3 = 0,
        ka_PO = 0.0602736679,
        bioavailability_PO = 0.859,
        tlag_PO = 5.3
      )
    ),
    tPeak = 0,
    ke0 = 0.0130782487,
    MEAC = 0,
    typical = 7,
    upperTypical = 15,
    lowerTypical = 3,
    reference = actual$reference
  )
  expect_equal_rounded(actual, expected)
})

test_that("scales to fat-free mass for a 120 kg man", {
  # 120 kg, 170 cm, 50 y male: FFM 71.09 kg.  Clearance follows its own
  # normal-fat-mass covariate, 24 x ((71.09 + 0.816 x 48.91) / 67.45)^0.75
  # L/h; the volumes see the pharmacokinetic weight 91.34 kg; Q takes the
  # library clearance factor 1.2209126.
  actual <- acetaminophen(120, 170, 50, "male")
  expected <- list(
    v1 = 57.024420436,
    v2 = 38.75572739,
    v3 = 1,
    cl1 = 0.581202027,
    cl2 = 0.885161648,
    cl3 = 0
  )
  expect_equal_rounded(actual$PK$default[names(expected)], expected)
})

test_that("Morse's vector is recovered for her standard 70 kg, 176 cm man", {
  x <- acetaminophen(70, 176, 35, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(x$cl1 * 60, 24.0)
  expect_equal(x$v1, 43.7); expect_equal(x$v2, 29.7); expect_equal(x$cl2 * 60, 43.5)
  # The library's reference man is a little fatter: 23.92 L/h
  y <- acetaminophen(70, 170, 35, "male")$PK$default
  expect_equal(y$cl1, 0.398647084, tolerance = 1e-8)
  expect_equal(y$v1, 43.7); expect_equal(y$cl2 * 60, 43.5)
  # With the switch off, volumes and Q follow total weight as published
  z <- acetaminophen(140, 176, 35, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(z$v1, 87.4); expect_equal(z$v2, 59.4)
  expect_equal(z$cl2 * 60, 43.5 * 2^0.75)
})

test_that("the oral absorption is Morse's fasted tablet, lag kept", {
  x <- acetaminophen(70, 170, 35, "male")$PK$default
  expect_equal(x$tlag_PO, 5.3)
  expect_equal(log(2) / x$ka_PO, 11.5)              # absorption half-life, min
  expect_equal(x$bioavailability_PO, 0.859)
})

test_that("ke0 is Anderson's 53 min equilibration half-time", {
  PK <- getDrugPK("acetaminophen", 70, 170, 35, "male",
                  getDrugDefaults("acetaminophen"))$PK$default
  expect_equal(log(2) / PK$ke0, 53)
})

# Independent check: integrate the two-compartment model with an effect site
# and first-order oral input by fourth-order Runge-Kutta, from the published
# parameters, and compare with the closed-form engine at its own time points.
# The oral lag is applied by time shift (the system is time-invariant): an
# oral dose at 0 gives, at time t, what an unlagged dose gives at t - 5.3,
# and nothing before 5.3 min.
rk4Acetaminophen <- function(times, doseIV, dosePO, dt = 0.02)
{
  v1 <- 43.7; v2 <- 29.7; cl <- 0.398647084; q <- 43.5 / 60  # reference man
  ka <- log(2) / 11.5; f <- 0.859; ke0 <- log(2) / 53
  d <- function(s) c(-ka * s[1],
                     f * ka * s[1] - (cl + q) * s[2] / v1 + q * s[3] / v2,
                     q * s[2] / v1 - q * s[3] / v2,
                     ke0 * (s[2] / v1 - s[4]))
  s <- c(dosePO, doseIV, 0, 0); t <- 0
  out <- matrix(NA_real_, length(times), 2)
  for (i in seq_along(times)) {
    while (t < times[i] - 1e-12) {
      h <- min(dt, times[i] - t)
      k1 <- d(s); k2 <- d(s + h / 2 * k1); k3 <- d(s + h / 2 * k2); k4 <- d(s + h * k3)
      s <- s + h * (k1 + 2 * k2 + 2 * k3 + k4) / 6
      t <- t + h
    }
    out[i, ] <- c(s[2] / v1, s[4])
  }
  out
}

test_that("1 g intravenous and oral match an independent integration", {
  iv <- simulateDrugsWithCovariates(
    data.frame(Drug = "acetaminophen", Time = 0, Dose = 1000, Units = "mg"),
    noEvents, 70, 170, 35, "male", 360, TRUE)$acetaminophen$wide
  ref <- rk4Acetaminophen(iv$Time, 1000, 0)
  # skip t = 0, where the engine reports the pre-bolus value
  keep <- iv$Time > 0
  expect_equal(iv$Plasma[keep], ref[keep, 1], tolerance = 1e-5)
  expect_equal(iv$"Effect Site"[keep], ref[keep, 2], tolerance = 1e-5)

  po <- simulateDrugsWithCovariates(
    data.frame(Drug = "acetaminophen", Time = 0, Dose = 1000, Units = "mg PO"),
    noEvents, 70, 170, 35, "male", 360, TRUE)$acetaminophen$wide
  lag <- 5.3
  after <- po$Time > lag
  ref <- rk4Acetaminophen(po$Time[after] - lag, 0, 1000)
  expect_equal(po$Plasma[after], ref[, 1], tolerance = 1e-5)
  expect_equal(po$"Effect Site"[after], ref[, 2], tolerance = 1e-5)
  # Nothing reaches plasma during the lag
  expect_true(all(po$Plasma[po$Time < lag] == 0))
  # Fasted 1 g tablet: the Python integration peaks at 11.05 mg/L at 34 min
  expect_gt(max(po$Plasma), 10.8)
  expect_lt(max(po$Plasma), 11.2)
  expect_gt(po$Time[which.max(po$Plasma)], 29)
  expect_lt(po$Time[which.max(po$Plasma)], 39)
})


test_that("the CSV band and threshold match the model, and 1 g IV crosses it", {
  dd <- getDrugDefaults("acetaminophen")
  x <- acetaminophen(70, 170, 35, "male")
  expect_equal(dd$Lower, x$lowerTypical)
  expect_equal(dd$Upper, x$upperTypical)
  expect_equal(dd$Typical, x$typical)
  expect_equal(dd$endCe, 5)
  expect_equal(dd$MEAC, 0)
  # The threshold is set so an ordinary adult dose produces a real time
  # until threshold: 1 g IV in the reference man peaks near 7.3 mcg/mL in
  # the effect site, above 5.
  w <- simulateDrugsWithCovariates(
    data.frame(Drug = "acetaminophen", Time = 0, Dose = 1000, Units = "mg"),
    noEvents, 70, 170, 35, "male", 480, TRUE)$acetaminophen$wide
  expect_gt(max(w$"Effect Site"), dd$endCe)
  expect_gt(max(w$Recovery, na.rm = TRUE), 60)
})


# The two Oral analgesics scenarios (inst/help/scenarios/acetaminophen-*.md)
# quote these numbers; if the model changes, the narratives have to change
# with it.  The curves are the two-compartment oral solution with its effect
# site, written out from the model's own parameters and checked against the
# simulation at its time points; the peaks and crossings are found on that
# solution, since the plotted grid is too coarse.

scenarioCurves <- function(id) {
  s <- helpScenarioById(id)
  p <- s$patient
  pk <- getDrugPK("acetaminophen", p$weight, p$height, p$age, p$sex,
                  adjustToFFM = p$adjustToFFM)$PK$default
  k10 <- pk$cl1 / pk$v1; k12 <- pk$cl2 / pk$v1; k21 <- pk$cl2 / pk$v2
  sum_ <- k10 + k12 + k21; root <- sqrt(sum_^2 - 4 * k10 * k21)
  al <- (sum_ + root) / 2; be <- (sum_ - root) / 2
  A <- (al - k21) / (pk$v1 * (al - be)); B <- (k21 - be) / (pk$v1 * (al - be))
  ka <- pk$ka_PO; ke0 <- pk$ke0; lag <- pk$tlag_PO
  lam <- c(al, be, ka)
  cpCoef <- pk$bioavailability_PO * ka * c(A / (ka - al), B / (ka - be))
  cpCoef <- c(cpCoef, -sum(cpCoef))
  ceCoef <- cpCoef * ke0 / (ke0 - lam)
  one <- function(coef, withKe0) function(t) {
    s <- pmax(t - lag, 0)
    out <- colSums(coef * exp(-outer(lam, s)))
    if (withKe0) out <- out - sum(coef) * exp(-ke0 * s)
    out
  }
  cp1 <- one(cpCoef, FALSE); ce1 <- one(ceCoef, TRUE)
  doses <- function(D, times) list(
    cp = function(t) Reduce(`+`, lapply(times, function(t0) D * cp1(t - t0))),
    ce = function(t) Reduce(`+`, lapply(times, function(t0) D * ce1(t - t0)))
  )
  list(s = s, p = p, pk = pk, beta = be, doses = doses,
       ivCe = function(D) {
         civ <- c(A, B); ceiv <- civ * ke0 / (ke0 - c(al, be))
         function(t) D * (colSums(ceiv * exp(-outer(c(al, be), t))) - sum(ceiv) * exp(-ke0 * t))
       })
}

test_that("the headache scenario says what the model does", {
  x <- scenarioCurves("acetaminophen-headache")
  expect_equal(log(2) / x$beta / 60, 2.345, tolerance = 1e-3)    # half-life, h
  c500 <- x$doses(500, 0)
  out <- simulateDrugsWithCovariates(x$s$doses, noEvents, x$p$weight, x$p$height,
                                     x$p$age, x$p$sex, x$s$options$maximum, FALSE,
                                     adjustToFFM = x$p$adjustToFFM)$acetaminophen$wide
  expect_equal(out$Plasma, c500$cp(out$Time), tolerance = 1e-8)
  expect_equal(out$"Effect Site", c500$ce(out$Time), tolerance = 1e-8)

  cpPeak <- stats::optimize(c500$cp, c(0, 200), maximum = TRUE)
  cePeak <- stats::optimize(c500$ce, c(0, 400), maximum = TRUE)
  expect_equal(c(cpPeak$objective, cpPeak$maximum), c(5.525, 33.9), tolerance = 1e-3)
  expect_equal(c(cePeak$objective, cePeak$maximum), c(3.099, 116.4), tolerance = 1e-3)
  # At the effect-site peak the plasma has fallen to meet it
  expect_equal(c500$cp(cePeak$maximum), cePeak$objective, tolerance = 1e-4)
  # Above the band's lower edge for about an hour, never above the threshold
  t <- seq(0, 480, 0.5)
  expect_equal(sum(c500$ce(t) >= 3) * 0.5, 57, tolerance = 0.02)
  expect_false(any(c500$ce(t) >= 5))
  # Anderson's Emax model: 5.17 x Ce / (9.98 + Ce)
  andersonRelief <- function(ce) 5.17 * ce / (9.98 + ce)
  expect_equal(andersonRelief(cePeak$objective), 1.22, tolerance = 1e-2)

  # Try next: 1000 mg doubles everything; relief about 2 points
  c1000 <- x$doses(1000, 0)
  ce1000 <- stats::optimize(c1000$ce, c(0, 400), maximum = TRUE)$objective
  expect_equal(ce1000, 6.197, tolerance = 1e-3)
  expect_equal(sum(c1000$ce(t) >= 5) * 0.5, 152, tolerance = 0.02)   # ~2.5 h
  expect_equal(andersonRelief(ce1000), 1.98, tolerance = 1e-2)
  # ... and 1000 mg IV: plasma 23, effect site 7.3 at 90 min
  ceIV <- x$ivCe(1000)
  ivPeak <- stats::optimize(ceIV, c(0, 300), maximum = TRUE)
  expect_equal(1000 / x$pk$v1, 22.88, tolerance = 1e-3)
  expect_equal(c(ivPeak$objective, ivPeak$maximum), c(7.33, 89.6), tolerance = 2e-3)
})

test_that("the four-times-a-day scenario says what the model does", {
  x <- scenarioCurves("acetaminophen-arthritis-qid")
  qt <- c(0, 360, 720, 1080)
  cq <- x$doses(1000, qt)
  out <- simulateDrugsWithCovariates(x$s$doses, noEvents, x$p$weight, x$p$height,
                                     x$p$age, x$p$sex, x$s$options$maximum, TRUE,
                                     adjustToFFM = x$p$adjustToFFM)$acetaminophen$wide
  # The qid row repeats at 6, 12 and 18 h
  expect_equal(out$Plasma, cq$cp(out$Time), tolerance = 1e-8)
  expect_equal(out$"Effect Site", cq$ce(out$Time), tolerance = 1e-8)

  cpPeaks <- sapply(qt, function(a) stats::optimize(cq$cp, c(a, a + 200), maximum = TRUE)$objective)
  expect_equal(cpPeaks, c(11.05, 12.58, 12.84, 12.88), tolerance = 1e-3)
  expect_equal(cq$cp(qt[-1]), c(1.80, 2.11, 2.16), tolerance = 2e-3)       # troughs
  ceSteady <- c(stats::optimize(cq$ce, c(1080, 1440), maximum = TRUE)$objective, cq$ce(1080))
  expect_equal(ceSteady, c(8.20, 3.33), tolerance = 2e-3)
  # Average at steady state: F x dose rate / CL
  expect_equal(x$pk$bioavailability_PO * 1000 / (x$pk$cl1 * 360), 5.99, tolerance = 1e-3)
  expect_equal(x$pk$cl1 * 60, 23.92, tolerance = 1e-3)
  # Above 5 mg/L from 30 min to 4.5 h after a steady dose; 60% of the day
  ti <- seq(720, 1080, 0.25)
  above <- ti[cq$ce(ti) >= 5] - 720
  expect_equal(range(above), c(30, 268), tolerance = 0.02)
  t24 <- seq(0, 1440, 0.5)
  expect_equal(mean(cq$ce(t24) >= 5), 0.60, tolerance = 0.02)
  expect_equal(sum(cq$ce(t24) >= 3 & cq$ce(t24) <= 15) * 0.5, 1370, tolerance = 0.01)

  # Try next: time until threshold after the last dose, from its effect-site
  # peak, is 2 h 49 min; the app's own readout agrees at its grid points
  peak4 <- stats::optimize(cq$ce, c(1080, 1440), maximum = TRUE)$maximum
  below <- stats::uniroot(function(t) cq$ce(t) - 5, c(peak4, 2100))$root
  expect_equal(below - peak4, 169, tolerance = 0.01)
  late <- out$Time > peak4 & out$Time < below
  expect_true(any(late))
  expect_equal(out$Recovery[late], below - out$Time[late], tolerance = 1e-3)

  # Try next: 1000 mg tid
  ct <- x$doses(1000, c(0, 480, 960))
  expect_equal(ct$ce(1440), 1.74, tolerance = 2e-3)
  expect_equal(mean(ct$ce(t24) >= 5), 0.38, tolerance = 0.02)
})
