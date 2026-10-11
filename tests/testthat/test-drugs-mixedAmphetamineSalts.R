# Mixed amphetamine salts (Adderall IR and XR): d-amphetamine in children,
# fitted here to McGough 2003 and the Adderall labels.
#
# What is guarded: the dose basis (worked from the four salts and checked
# against the labels' base equivalence), the parameters and their derivation,
# the XR pulses, and the fit against McGough's observed group means.

noEvents <- data.frame(Time = numeric(0), Event = character(0))

trapz <- function(x, y) sum(diff(x) * (utils::head(y, -1) + utils::tail(y, -1)) / 2)

# McGough's mean child: 37.8 kg, 138.7 cm, 9.5 y; switch off, so the weight
# equations see 37.8 kg and the parameters are the fitted ones.
masCurve <- function(dose, maximum) {
  simulateDrugsWithCovariates(dose, noEvents, 37.8, 138.7, 9.5, "male",
                              maximum, FALSE, adjustToFFM = FALSE
  )$mixedAmphetamineSalts$wide
}
masDose <- function(time, mg, units) {
  data.frame(Drug = "mixedAmphetamineSalts", Time = time, Dose = mg, Units = units)
}
# The last day of a week of once-daily dosing
steadyDay <- function(w) {
  day <- w[w$Time >= 7 * 1440, ]
  list(cmax = max(day$Plasma), tmax = (day$Time[which.max(day$Plasma)] - 7 * 1440) / 60)
}


test_that("the dose basis reproduces the labels' amphetamine base equivalence", {
  d <- amphetamineBaseFraction("d")
  l <- amphetamineBaseFraction("l")
  # Adderall XR label: 20 mg capsule = 12.5 mg total amphetamine base;
  # Adderall label: 5 mg tablet = 3.13 mg, 30 mg tablet = 18.8 mg
  expect_equal(round(20 * (d + l), 1), 12.5)
  expect_equal(round(5 * (d + l), 2), 3.13)
  expect_equal(round(30 * (d + l), 1), 18.8)
  # d:l about 3:1 (the labels), 3.15 from the salts
  expect_equal(d / l, 3.15, tolerance = 0.005)
  expect_equal(20 * d, 9.498, tolerance = 1e-4)
})


test_that("returns the correct calculations", {
  actual <- mixedAmphetamineSalts(37.8, 138.7, 9.5, "male", adjustToFFM = FALSE)
  # CL/F = 9.498 mg / 936.7 ng.h/mL (McGough AUC0-inf); V/F from t1/2 9 h
  cl <- 20 * 0.4748968 / 936.7 * 1000
  expected <- list(
    PK = list(default = list(
      v1 = cl * 9 / log(2), v2 = 1, v3 = 1,
      cl1 = cl / 60, cl2 = 0, cl3 = 0,
      ka_PO = 0.689 / 60,
      bioavailability_PO = 0.4748968,
      tlag_PO = 0
    )),
    tPeak = 0,
    MEAC = 0,
    typical = 0,
    upperTypical = 0,
    lowerTypical = 0,
    reference = actual$reference,
    oralPulses = list(XR = list(fraction = c(0.5, 0.5), delay = c(0, 240)))
  )
  expect_equal_rounded(actual, expected)
  expect_equal(cl, 10.14, tolerance = 1e-3)
  expect_equal(actual$PK$default$v1, 131.7, tolerance = 1e-3)
})


test_that("weight follows the borrowed Tsuda exponents", {
  x <- mixedAmphetamineSalts(25, 125, 7, "female", adjustToFFM = FALSE)$PK$default
  ref <- mixedAmphetamineSalts(37.8, 138.7, 9.5, "male", adjustToFFM = FALSE)$PK$default
  expect_equal_rounded(x$cl1 / ref$cl1, (25 / 37.8)^0.600)
  expect_equal_rounded(x$v1 / ref$v1, (25 / 37.8)^0.776)

  # 120 kg, 170 cm, 50 y male, switch on: pharmacokinetic weight 70 x 1.3049067
  pkW <- 70 * 1.3049067
  y <- mixedAmphetamineSalts(120, 170, 50, "male")$PK$default
  expect_equal_rounded(y$cl1 / ref$cl1, (pkW / 37.8)^0.600)
  expect_equal_rounded(y$v1 / ref$v1, (pkW / 37.8)^0.776)
})


test_that("XR 20 mg is exactly Adderall 10 mg twice, 4 h apart", {
  # The model's definition of XR, and the label's comparison
  xr <- masCurve(masDose(0, 20, "mg PO XR"), 1440)
  ir <- masCurve(masDose(c(0, 240), 10, "mg PO"), 1440)
  expect_equal(xr$Plasma, approx(ir$Time, ir$Plasma, xr$Time)$y, tolerance = 1e-8)
})


test_that("the fit reproduces McGough's XR 20 mg single dose", {
  # Observed: Cmax 48.8 ng/mL, Tmax 6.8 h, AUC0-24 703.9 ng.h/mL (SEM 2.0,
  # 0.5, 27.5; n = 48).  Held within 10%: a typical curve, not a mean of
  # individual curves.
  w <- masCurve(masDose(0, 20, "mg PO XR"), 1440)
  expect_equal(max(w$Plasma) / 48.8, 1, tolerance = 0.10)
  expect_equal(w$Time[which.max(w$Plasma)] / 60, 6.8, tolerance = 0.05)
  expect_equal(trapz(w$Time, w$Plasma) / 60 / 703.9, 1, tolerance = 0.10)

  # AUC to infinity is the dose basis over CL/F by construction
  long <- masCurve(masDose(0, 20, "mg PO XR"), 20160)
  expect_equal(trapz(long$Time, long$Plasma) / 60 / 936.7, 1, tolerance = 0.01)
})


test_that("the fit reproduces McGough's steady-state groups", {
  # Table 3, uncorrected data, after a week once daily
  ir10 <- steadyDay(masCurve(masDose(0, 10, "mg PO qd"), 8 * 1440))
  expect_equal(ir10$tmax, 3.3, tolerance = 0.05)       # observed 3.3 (SEM 0.4)
  expect_equal(ir10$cmax / 33.8, 1, tolerance = 0.10)  # observed 33.8 (3.7)

  xr30 <- steadyDay(masCurve(masDose(0, 30, "mg PO XR qd"), 8 * 1440))
  expect_equal(xr30$cmax / 89.0, 1, tolerance = 0.10)  # observed 89.0 (6.4)
  # XR peaks well after IR, as both McGough and the label report
  expect_gt(xr30$tmax - ir10$tmax, 2.5)
})


test_that("the half-life is the label's 9 h in children", {
  x <- mixedAmphetamineSalts(37.8, 138.7, 9.5, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(log(2) * x$v1 / x$cl1 / 60, 9, tolerance = 1e-8)
})


test_that("offered orally only, with no claimed effect or band", {
  dd <- getDrugDefaultsGlobal(FALSE)
  row <- dd[dd$Drug == "mixedAmphetamineSalts", ]
  units <- strsplit(row$Units, ",")[[1]]
  expect_equal(units, c("mg PO", "mg PO bid", "mg PO XR", "mg PO XR qd"))
  expect_true(all(doseRoute(units) == "PO"))
  expect_equal(row$Default.Units, "mg PO XR")
  expect_equal(row$Category, "Stimulants")
  expect_equal(c(row$Lower, row$Upper, row$Typical, row$MEAC, row$endCe), rep(0, 5))
})
