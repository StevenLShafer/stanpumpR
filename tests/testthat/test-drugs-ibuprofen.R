# Ibuprofen: Morse 2022 intravenous and fasted-tablet disposition with
# clearance and volumes on normal fat mass, and ke0 from Hannam 2018.  See the
# header of R/drugs_ibuprofen.R.  Pins were worked out from the published
# numbers by hand (Python), not from the code under test.

noEvents <- data.frame(Time = numeric(0), Event = character(0))

test_that("returns the published parameters with the fat-free-mass switch off", {
  weight <- 70
  height <- 171
  age <- 50
  sex <- "male"
  # Clearance and volumes use normal fat mass in either switch position
  # (70 kg, 171 cm male: NFMcl 67.91 kg against Morse's standard 68.10,
  # NFMv 65.70 against 66.09), so they are not exactly Table 3's here.
  actual <- ibuprofen(weight, height, age, sex, adjustToFFM = FALSE)

  expected <- list(
    PK = list(
      default = list(
        v1 = 6.014589513,
        v2 = 4.344422508,
        v3 = 1,
        cl1 = 0.063035891,
        cl2 = 0.175,
        cl3 = 0,
        ka_PO = 0.0259605686,
        bioavailability_PO = 0.941,
        tlag_PO = 6.66
      )
    ),
    tPeak = 0,
    ke0 = 0.0111081279,
    MEAC = 0,
    typical = 17,
    upperTypical = 35,
    lowerTypical = 5,
    reference = actual$reference
  )
  expect_equal_rounded(actual, expected)
})

test_that("scales to fat-free mass for a 120 kg man", {
  # 120 kg, 170 cm, 50 y male: FFM 71.09 kg.  Clearance follows its own
  # normal fat mass, 3.79 x ((71.09 + 0.863 x 48.91) / 68.10)^0.75 L/h; the
  # volumes theirs, x (71.09 + 0.718 x 48.91) / 66.09; Q takes the library
  # clearance factor 1.2209126.
  actual <- ibuprofen(120, 170, 50, "male")
  expected <- list(
    v1 = 9.722597509,
    v2 = 7.022768779,
    v3 = 1,
    cl1 = 0.092533464,
    cl2 = 0.213659708,
    cl3 = 0
  )
  expect_equal_rounded(actual$PK$default[names(expected)], expected)
  # With the switch off only Q changes: (120 / 70)^0.75 on total weight
  off <- ibuprofen(120, 170, 50, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(off[c("v1", "v2", "cl1")], actual$PK$default[c("v1", "v2", "cl1")])
  expect_equal(off$cl2, 0.26218054, tolerance = 1e-8)
})

test_that("Morse's Table 3 is recovered for the standard 70 kg, 176 cm man", {
  x <- ibuprofen(70, 176, 35, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(x$cl1 * 60, 3.79)
  expect_equal(x$v1, 6.05); expect_equal(x$v2, 4.37); expect_equal(x$cl2 * 60, 10.5)
  # The library's reference man is a little fatter: 3.78 L/h, 6.01 and 4.34 L
  y <- ibuprofen(70, 170, 35, "male")$PK$default
  expect_equal(y$cl1, 0.063009138, tolerance = 1e-8)
  expect_equal(y$v1, 6.00734849, tolerance = 1e-8)
  expect_equal(y$v2, 4.339192215, tolerance = 1e-8)
  expect_equal(y$cl2 * 60, 10.5)
  # With the switch off, Q follows total weight as published
  z <- ibuprofen(140, 176, 35, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(z$cl2 * 60, 10.5 * 2^0.75)
})

test_that("the oral absorption is Morse's fasted tablet, lag kept", {
  x <- ibuprofen(70, 170, 35, "male")$PK$default
  expect_equal(x$tlag_PO, 6.66)
  expect_equal(log(2) / x$ka_PO, 26.7)              # absorption half-life, min
  expect_equal(x$bioavailability_PO, 0.941)
})

test_that("ke0 is Hannam's 1.04 h equilibration half-time", {
  PK <- getDrugPK("ibuprofen", 70, 170, 35, "male",
                  getDrugDefaults("ibuprofen"))$PK$default
  expect_equal(log(2) / PK$ke0, 62.4)
})

# Independent check: integrate the two-compartment model with an effect site
# and first-order oral input by fourth-order Runge-Kutta, from the published
# parameters, and compare with the closed-form engine at its own time points.
# The oral lag is applied by time shift, as in test-drugs-acetaminophen.R.
rk4Ibuprofen <- function(times, doseIV, dosePO, dt = 0.02)
{
  # reference man (70 kg, 170 cm), worked out from Table 3 in Python
  v1 <- 6.00734849; v2 <- 4.339192215; cl <- 0.063009138; q <- 10.5 / 60
  ka <- log(2) / 26.7; f <- 0.941; ke0 <- log(2) / 62.4
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

test_that("400 mg intravenous and oral match an independent integration", {
  iv <- simulateDrugsWithCovariates(
    data.frame(Drug = "ibuprofen", Time = 0, Dose = 400, Units = "mg"),
    noEvents, 70, 170, 35, "male", 480, TRUE)$ibuprofen$wide
  ref <- rk4Ibuprofen(iv$Time, 400, 0)
  # skip t = 0, where the engine reports the pre-bolus value
  keep <- iv$Time > 0
  expect_equal(iv$Plasma[keep], ref[keep, 1], tolerance = 1e-5)
  expect_equal(iv$"Effect Site"[keep], ref[keep, 2], tolerance = 1e-5)

  po <- simulateDrugsWithCovariates(
    data.frame(Drug = "ibuprofen", Time = 0, Dose = 400, Units = "mg PO"),
    noEvents, 70, 170, 35, "male", 480, TRUE)$ibuprofen$wide
  lag <- 6.66
  after <- po$Time > lag
  ref <- rk4Ibuprofen(po$Time[after] - lag, 0, 400)
  expect_equal(po$Plasma[after], ref[, 1], tolerance = 1e-5)
  expect_equal(po$"Effect Site"[after], ref[, 2], tolerance = 1e-5)
  # Nothing reaches plasma during the lag
  expect_true(all(po$Plasma[po$Time < lag] == 0))
  # Fasted 400 mg tablet: the integration peaks at 23.6 mg/L at 62 min
  expect_gt(max(po$Plasma), 23.3)
  expect_lt(max(po$Plasma), 23.8)
  expect_gt(po$Time[which.max(po$Plasma)], 55)
  expect_lt(po$Time[which.max(po$Plasma)], 70)
})

test_that("the CSV band and threshold match the model, and 400 mg crosses it", {
  dd <- getDrugDefaults("ibuprofen")
  x <- ibuprofen(70, 170, 35, "male")
  expect_equal(dd$Lower, x$lowerTypical)
  expect_equal(dd$Upper, x$upperTypical)
  expect_equal(dd$Typical, x$typical)
  expect_equal(dd$endCe, 6.3)
  expect_equal(dd$MEAC, 0)
  expect_equal(dd$Category, "Oral analgesics")
  # The band's reasons, in the reference man: the typical line is the
  # steady-state average of 400 mg by mouth every 6 h, F x dose / (CL x tau)
  expect_equal(0.941 * 400 / (x$PK$default$cl1 * 360), 16.6, tolerance = 1e-3)
  # A 400 mg tablet's effect site clears the threshold, so the time until
  # threshold is a real one once absorption has started
  w <- simulateDrugsWithCovariates(
    data.frame(Drug = "ibuprofen", Time = 0, Dose = 400, Units = "mg PO"),
    noEvents, 70, 170, 35, "male", 720, TRUE)$ibuprofen$wide
  expect_gt(max(w$"Effect Site"), dd$endCe)
  expect_true(all(is.na(w$Recovery[w$Time < 6.66])))
  expect_gt(max(w$Recovery, na.rm = TRUE), 400)
})


# The Oral analgesics scenario (inst/help/scenarios/ibuprofen-with-
# acetaminophen.md) quotes these numbers; if either model changes, the
# narrative has to change with it.  The curves are each drug's
# two-compartment solution with its effect site, written out from the model's
# own parameters and checked against the simulation at its time points; the
# peaks and crossings are found on those solutions, since the plotted grid is
# too coarse.

scenarioDrug <- function(drug, p) {
  pk <- getDrugPK(drug, p$weight, p$height, p$age, p$sex,
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
  civ <- c(A, B); ceiv <- civ * ke0 / (ke0 - c(al, be))
  list(
    pk = pk, beta = be,
    po = function(D) list(
      cp = function(t) { s <- pmax(t - lag, 0); D * colSums(cpCoef * exp(-outer(lam, s))) },
      ce = function(t) {
        s <- pmax(t - lag, 0)
        D * (colSums(ceCoef * exp(-outer(lam, s))) - sum(ceCoef) * exp(-ke0 * s))
      }
    ),
    ivCe = function(D) function(t)
      D * (colSums(ceiv * exp(-outer(c(al, be), t))) - sum(ceiv) * exp(-ke0 * t))
  )
}

peakOf <- function(f, upper = 600) {
  o <- stats::optimize(f, c(0, upper), maximum = TRUE)
  c(o$objective, o$maximum)
}
crossings <- function(f, threshold, peakAt, from = 0) c(
  stats::uniroot(function(t) f(t) - threshold, c(from, peakAt))$root,
  stats::uniroot(function(t) f(t) - threshold, c(peakAt, 2000))$root
)

test_that("the ibuprofen-with-acetaminophen scenario says what the models do", {
  s <- helpScenarioById("ibuprofen-with-acetaminophen")
  p <- s$patient
  ibu <- scenarioDrug("ibuprofen", p)
  apap <- scenarioDrug("acetaminophen", p)
  out <- simulateDrugsWithCovariates(s$doses, noEvents, p$weight, p$height,
                                     p$age, p$sex, s$options$maximum, TRUE,
                                     adjustToFFM = p$adjustToFFM)
  i400 <- ibu$po(400); a1000 <- apap$po(1000)
  wi <- out$ibuprofen$wide; wa <- out$acetaminophen$wide
  expect_equal(wi$Plasma, i400$cp(wi$Time), tolerance = 1e-8)
  expect_equal(wi$"Effect Site", i400$ce(wi$Time), tolerance = 1e-8)
  expect_equal(wa$Plasma, a1000$cp(wa$Time), tolerance = 1e-8)
  expect_equal(wa$"Effect Site", a1000$ce(wa$Time), tolerance = 1e-8)

  # Absorption and equilibration half-times, min; plasma half-lives, h
  expect_equal(log(2) / c(ibu$pk$ka_PO, apap$pk$ka_PO), c(26.7, 11.5))
  expect_equal(log(2) / c(ibu$pk$ke0, apap$pk$ke0), c(62.4, 53))
  expect_equal(log(2) / c(ibu$beta, apap$beta) / 60, c(2.028, 2.345), tolerance = 1e-3)

  # Peaks: plasma 23.6 at 62 min and 11.0 at 34; effect site 16.1 at 2 h 44
  # and 6.2 at 1 h 56
  expect_equal(peakOf(i400$cp, 300), c(23.63, 61.8), tolerance = 1e-3)
  expect_equal(peakOf(a1000$cp, 300), c(11.05, 33.9), tolerance = 1e-3)
  ceI <- peakOf(i400$ce); ceA <- peakOf(a1000$ce)
  expect_equal(ceI, c(16.15, 164.0), tolerance = 1e-3)
  expect_equal(ceA, c(6.197, 116.4), tolerance = 1e-3)

  # Above the thresholds: ibuprofen 6.3 from 48 min to 7 h 17 min,
  # acetaminophen 5 from 1 h 2 min to 3 h 35 min
  dd <- getDrugDefaultsGlobal()
  thrI <- dd$endCe[dd$Drug == "ibuprofen"]; thrA <- dd$endCe[dd$Drug == "acetaminophen"]
  expect_equal(c(thrI, thrA), c(6.3, 5))
  aboveI <- crossings(i400$ce, thrI, ceI[2], from = 7)
  aboveA <- crossings(a1000$ce, thrA, ceA[2], from = 6)
  expect_equal(aboveI, c(48.05, 436.97), tolerance = 1e-3)
  expect_equal(aboveA, c(62.24, 214.83), tolerance = 1e-3)
  expect_equal(diff(aboveI), 388.9, tolerance = 1e-3)            # 6 h 29 min
  expect_equal(diff(aboveA), 152.6, tolerance = 1e-3)            # about 2.5 h
  # Two and a half times its threshold, against a quarter above
  expect_equal(c(ceI[1] / thrI, ceA[1] / thrA), c(2.56, 1.24), tolerance = 2e-3)

  # The time-until-threshold line: blank during each lag, then the time to
  # the last crossing, a little over 7 h and about 3.5 h at the start
  expect_true(s$options$showThreshold)
  expect_true(all(is.na(wi$Recovery[wi$Time < 6.66])))
  expect_true(all(is.na(wa$Recovery[wa$Time < 5.3])))
  liveI <- wi$Time >= 6.66 & wi$Time < aboveI[2]
  liveA <- wa$Time >= 5.3 & wa$Time < aboveA[2]
  expect_true(any(liveI)); expect_true(any(liveA))
  expect_equal(wi$Recovery[liveI], aboveI[2] - wi$Time[liveI], tolerance = 1e-3)
  expect_equal(wa$Recovery[liveA], aboveA[2] - wa$Time[liveA], tolerance = 1e-3)
  expect_equal((aboveI[2] - 6.66) / 60, 7.17, tolerance = 2e-3)
  expect_equal((aboveA[2] - 5.3) / 60, 3.49, tolerance = 2e-3)

  # Try next: 200 mg of ibuprofen, effect site 8.1, above 6.3 from 1 h 30 min
  # to 4 h 39 min, 3 h 9 min
  i200 <- ibu$po(200)
  ce200 <- peakOf(i200$ce)
  expect_equal(ce200[1], 8.074, tolerance = 1e-3)
  above200 <- crossings(i200$ce, thrI, ce200[2], from = 7)
  expect_equal(above200, c(89.87, 279.14), tolerance = 1e-3)
  expect_equal(diff(above200), 189.3, tolerance = 1e-3)
  expect_lt(diff(above200), diff(aboveI) / 2)

  # Try next: acetaminophen 1 g intravenously, 7.3 at 90 min, above 5 from
  # 27 min to 3 h 54 min
  aIV <- apap$ivCe(1000)
  ceIV <- peakOf(aIV, 300)
  expect_equal(ceIV, c(7.327, 89.6), tolerance = 2e-3)
  aboveIV <- crossings(aIV, thrA, ceIV[2], from = 1e-6)
  expect_equal(aboveIV, c(26.71, 234.05), tolerance = 1e-3)
  expect_lt(diff(aboveIV), 210)
})
