# gabapentin: see the header of R/drugs_gabapentin.R for the model, the three
# decisions it records, and what it leaves out.  Pins were worked out from the
# published numbers and the fat-free-mass factors by hand (Python), not from
# the code under test.

noEvents <- data.frame(Time = numeric(0), Event = character(0))

oralGabapentin <- function(dt, maximum = 1440, adjustToFFM = FALSE, creatinine = NULL) {
  simulateDrugsWithCovariates(
    dt, noEvents, 70, 171, 50, "male", maximum, FALSE,
    adjustToFFM = adjustToFFM, creatinine = creatinine
  )$gabapentin$wide
}

gabapentinDose <- function(time, dose, units = "mg PO") {
  data.frame(Drug = "gabapentin", Time = time, Dose = dose, Units = units)
}


test_that("returns the re-anchored Tran parameters with the fat-free-mass switch off", {
  # 70 kg, 50 y man at the assumed creatinine of 1.0 mg/dL: Cockcroft-Gault
  # 87.5 mL/min, so CL = 11.1 x 58/81 x 87.5/106.3 L/h.
  actual <- gabapentin(70, 171, 50, "male", adjustToFFM = FALSE)

  expected <- list(
    PK = list(default = list(
      v1 = 58, v2 = 1, v3 = 1,
      cl1 = 0.1090409161, cl2 = 0, cl3 = 0,
      ka_PO = 0.860 / 60,
      bioavailability_PO = 1,
      tlag_PO = 18.66
    )),
    tPeak = 0,
    tPeakRoute = ROUTE_PO,
    MEAC = 0,
    typical = 6,
    upperTypical = 9.4,
    lowerTypical = 4.1,
    reference = actual$reference,
    oralSaturation = list(Imax = 0.906, ID50 = 571)
  )

  expect_equal_rounded(actual, expected)
})


test_that("scales to fat-free mass for a 120 kg man", {
  # 120 kg, 170 cm, 50 y male: FFM 71.09 kg against the 54.48 kg reference, so
  # V x 1.3049066; Cockcroft-Gault at the pharmacokinetic weight 91.34 kg is
  # 114.18 mL/min, and clearance takes no further size factor.
  actual <- gabapentin(120, 170, 50, "male")
  expected <- list(
    v1 = 75.68458549,
    v2 = 1,
    v3 = 1,
    cl1 = 0.1422882161,
    cl2 = 0,
    cl3 = 0
  )
  expect_equal_rounded(actual$PK$default[names(expected)], expected)

  # With the switch off V is 58 L whatever the weight: Tran retained no size
  # term on V.
  expect_equal(gabapentin(120, 170, 50, "male", adjustToFFM = FALSE)$PK$default$v1, 58)
})


test_that("Tran's published model is recoverable from the re-anchored one", {
  x <- gabapentin(70, 171, 50, "male", adjustToFFM = FALSE)
  anchor <- 58 / 81
  # V 81 L, and CL 11.1 L/h at Tran's median CrCL of 106.3 mL/min: give the
  # patient that creatinine clearance and undo the anchor.
  scr <- (140 - 50) * 70 / (72 * 106.3)
  y <- gabapentin(70, 171, 50, "male", adjustToFFM = FALSE, creatinine = scr)
  expect_equal(x$PK$default$v1 / anchor, 81)
  expect_equal(y$PK$default$cl1 * 60 / anchor, 11.1)
  # The anchor scales V and CL together, so the half-life is Tran's: 5.06 h
  # at the median CrCL.
  expect_equal(log(2) * y$PK$default$v1 / (y$PK$default$cl1 * 60), log(2) * 81 / 11.1)

  # The dose-bioavailability curve reproduces the three values Tran reports.
  f <- oralSaturationFraction(c(300, 400, 800), x$oralSaturation)
  expect_equal(round(f, 3), c(0.688, 0.627, 0.471))
})


test_that("clearance is proportional to creatinine clearance", {
  # Not Tran's ^0.331: see the header.  Doubling the creatinine halves
  # Cockcroft-Gault and so halves clearance.
  normal <- gabapentin(70, 171, 50, "male", adjustToFFM = FALSE, creatinine = 1)
  raised <- gabapentin(70, 171, 50, "male", adjustToFFM = FALSE, creatinine = 2)
  expect_equal(raised$PK$default$cl1, normal$PK$default$cl1 / 2)
  expect_equal(raised$PK$default$cl1, 0.0545204581, tolerance = 1e-8)

  # So the half-life at a creatinine clearance of 20 mL/min is 27 h, rather
  # than the 9 h Tran's exponent would give; the label reports 52 h below 30.
  scr <- (140 - 50) * 70 / (72 * 20)
  low <- gabapentin(70, 171, 50, "male", adjustToFFM = FALSE, creatinine = scr)$PK$default
  expect_equal(log(2) * low$v1 / (low$cl1 * 60), 26.885, tolerance = 1e-4)
})


test_that("gabapentin is offered orally only", {
  dd <- getDrugDefaultsGlobal(FALSE)
  units <- strsplit(dd$Units[dd$Drug == "gabapentin"], ",")[[1]]
  expect_equal(units, c("mg PO", paste("mg PO", names(SCHEDULE_INTERVALS))))
  expect_true(all(doseRoute(units) == ROUTE_PO))
  expect_true(is.na(dd$Bolus.Units[dd$Drug == "gabapentin"]))
  expect_true(is.na(dd$Infusion.Units[dd$Drug == "gabapentin"]))
})


test_that("the CSV row agrees with the drug function", {
  # The plot reads the band and MEAC from the CSV, not from the function.
  dd  <- getDrugDefaultsGlobal(FALSE)
  row <- dd[dd$Drug == "gabapentin", ]
  x   <- gabapentin(70, 171, 50, "male")
  expect_equal(row$Lower, x$lowerTypical)
  expect_equal(row$Upper, x$upperTypical)
  expect_equal(row$Typical, x$typical)
  expect_equal(row$MEAC, 0)          # not an opioid: off the MEAC panel
  expect_equal(row$endCe, 0)
  expect_equal(row$Concentration.Units, "mcg")
  expect_equal(x$tPeak, GABAPENTIN_TPEAK)
})


test_that("each oral dose is scaled by its own bioavailability", {
  # The curves of 300 and 1200 mg have the same shape, so at every time their
  # ratio is the ratio of the amounts absorbed: 1200 x 0.38611 over
  # 300 x 0.68794 = 2.2450, not 4.
  a <- oralGabapentin(gabapentinDose(0, 300))
  b <- oralGabapentin(gabapentinDose(0, 1200))
  expect_equal(a$Time, b$Time)
  absorbing <- a$Plasma > 0
  expect_equal(b$Plasma[absorbing] / a$Plasma[absorbing],
               rep(2.2450027479, sum(absorbing)), tolerance = 1e-8)

  # Doses are scaled one at a time and then superpose.  600 mg followed by a
  # 300 mg twice-daily schedule (repeats at 1440 and 2160 min) is checked at
  # the simulation's own time points against the one-compartment oral
  # solution written out independently, each dose carrying Tran's F at its
  # own size.
  w <- oralGabapentin(
    rbind(gabapentinDose(0, 600), gabapentinDose(720, 300, "mg PO bid")),
    maximum = 2880
  )
  V   <- 58
  CL  <- 11.1 * 58 / 81 * 87.5 / 106.3 / 60        # L/min
  k   <- CL / V
  ka  <- 0.860 / 60
  lag <- 0.311 * 60
  oral <- function(t, t0, D) {
    s <- pmax(t - t0 - lag, 0)
    (1 - 0.906 * D / (571 + D)) * D / V * ka / (ka - k) * (exp(-k * s) - exp(-ka * s))
  }
  expected <- oral(w$Time, 0, 600) + oral(w$Time, 720, 300) +
    oral(w$Time, 1440, 300) + oral(w$Time, 2160, 300)
  expect_equal(w$Plasma, expected, tolerance = 1e-8)
})


test_that("the reference patient's oral peak falls at 3.0 h", {
  # Analytically, off the coefficients rather than the plotted grid: lag
  # 0.311 h plus ln(ka/k)/(ka - k) with k = CL/V at CrCL 87.5 mL/min.
  pk <- getDrugPK("gabapentin", 70, 171, 50, "male", adjustToFFM = FALSE)
  d  <- pk$PK$default
  k  <- d$cl1 / d$v1
  peak <- d$tlag_PO + log(d$ka_PO / k) / (d$ka_PO - k)
  expect_equal(peak / 60, 3.02956, tolerance = 1e-5)
  expect_equal(log(2) / k / 60, 6.14487, tolerance = 1e-5)
  expect_equal(pk$PK$default$ke0, 0)     # plasma only, see GABAPENTIN_TPEAK
})


test_that("the re-anchored model reproduces the Western single-dose data", {
  # Tran's median subject (CrCL 106.3 mL/min).  Observed: AUC after 300 mg
  # 24.8 (label, 300 mg q8h) to 28.3; after 400 mg 34.1 (Gidal 2000); after
  # 600 mg 43.9 (Eckhardt 2000); peak after 600 mg 3.87-4.22 mcg/mL.
  scr <- (140 - 50) * 70 / (72 * 106.3)
  w <- oralGabapentin(gabapentinDose(0, 600), maximum = 4320, creatinine = scr)
  expect_equal(max(w$Plasma), 3.913, tolerance = 0.01)

  # AUC to infinity is F(D) x D / CL, in mcg.h/mL for mg and L/h.  Worked out
  # from the model rather than off the plotted grid, which is too coarse to
  # integrate.
  pk  <- getDrugPK("gabapentin", 70, 171, 50, "male", adjustToFFM = FALSE,
                   creatinine = scr)
  auc <- function(D) {
    D * oralSaturationFraction(D, pk$oralSaturation) *
      pk$PK$default$bioavailability_PO / (pk$PK$default$cl1 * 60)
  }
  expect_equal(auc(300), 25.97, tolerance = 1e-3)
  expect_equal(auc(400), 31.54, tolerance = 1e-3)
  expect_equal(auc(600), 40.45, tolerance = 1e-3)
})


test_that("the saturable-absorption scenario says what the model does", {
  # inst/help/scenarios/gabapentin-saturable-absorption.md quotes these
  # numbers; if the model changes, the narrative has to change with it.
  # Worked out by hand: 40 y, 70 kg man, CrCL 97.2 mL/min, half-life 5.53 h;
  # peaks 3.99 and 5.80 mcg/mL (the second carrying 0.07 left from the first).
  s <- helpScenarioById("gabapentin-saturable-absorption")
  p <- s$patient
  out <- simulateDrugsWithCovariates(
    s$doses, noEvents, p$weight, p$height, p$age, p$sex,
    s$options$maximum, FALSE, adjustToFFM = p$adjustToFFM
  )
  w <- out$gabapentin$wide
  first  <- max(w$Plasma[w$Time < 2160])
  second <- max(w$Plasma[w$Time >= 2160])
  expect_equal(first, 3.99, tolerance = 0.01)
  expect_equal(second, 5.80, tolerance = 0.01)
  expect_equal(second / first, 1.45, tolerance = 0.01)

  pk <- getDrugPK("gabapentin", p$weight, p$height, p$age, p$sex)
  expect_equal(log(2) * pk$PK$default$v1 / (pk$PK$default$cl1 * 60), 5.53, tolerance = 0.01)
  absorbed <- c(600, 1200) * oralSaturationFraction(c(600, 1200), pk$oralSaturation)
  expect_equal(round(absorbed), c(321, 463))
  expect_equal(round(2 * 600 * oralSaturationFraction(600, pk$oralSaturation)), 643)
})
