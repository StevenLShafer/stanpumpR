# Lumefantrine (R/drugs_lumefantrine.R): Ding 2026, Table 4.  Drafted by
# Claude Code, 2026-11-24.  Expectations are computed here from the paper,
# not from the drug file.

ding4 <- function(w) list(
  cl = 4.35 * (w / 45)^0.75, q = 1.65 * (w / 45)^0.75,
  vc = 101 * w / 45, vp = 311 * w / 45)

test_that("Ding 2026, Table 4, switch off", {
  for (w in c(15, 45, 90)) {
    x <- lumefantrine(w, 170, 30, "female", adjustToFFM = FALSE)
    set <- x$PK$default
    ref <- ding4(w)
    expect_equal(set$cl1, ref$cl / 60)
    expect_equal(set$cl2, ref$q / 60)
    expect_equal(set$v1, ref$vc)
    expect_equal(set$v2, ref$vp)
    # The dose depot and five transit compartments, each at 6 / MTT
    expect_identical(x$transitChain$n, 6L)
    expect_equal(x$transitChain$ktr * 60, 6 / 5.43)
    expect_equal(set$ka_PO * 60, 6 / 5.43)
    expect_null(x$transitChain$metabolite)
  }
})

test_that("switch on: weight at the pharmacokinetic weight", {
  pkW <- pkSizeFactors(100, 180, 40, "male")$pkWeight
  set <- lumefantrine(100, 180, 40, "male")$PK$default
  expect_equal(set$cl1, ding4(pkW)$cl / 60)
  expect_equal(set$v2, ding4(pkW)$vp)
})

test_that("bioavailability by the dose per kilogram, held within the studied doses", {
  f <- function(w, mg) oralSaturationFraction(mg, lumefantrine(w, 170, 30, "male")$oralSaturation)
  # Table 4: F x [1 - 0.0647 (dose/WT - 9.8)], on the actual weight
  expect_equal(f(45, 9.8 * 45), 1)
  expect_equal(f(45, 480), 1 - 0.0647 * (480 / 45 - 9.8))
  expect_equal(f(70, 480), 1 - 0.0647 * (480 / 70 - 9.8))
  # Held at 4.75 and 16.0 mg/kg a dose
  expect_equal(f(70, 120), 1 - 0.0647 * (4.75 - 9.8))
  expect_equal(f(10, 480), 1 - 0.0647 * (16.0 - 9.8))
  expect_equal(f(70, 0), 1 - 0.0647 * (4.75 - 9.8))
  # And it stays positive at the ends for any weight
  expect_gt(f(1, 1e6), 0)
})

test_that("the typical patient's course against Table 4's medians", {
  dt <- data.frame(Drug = "Coartem", Time = c(0, 8, 24, 36, 48, 60) * 60,
                   Dose = 4, Units = "tablet PO")
  events <- data.frame(Time = numeric(0), Event = character(0), Fill = character(0))
  l <- withOutputTimes(7 * 1440, simulateDrugsWithCovariates(
    dt, events, 45, 160, 30, "male", 21 * 1440, FALSE,
    adjustToFFM = FALSE))$lumefantrine$wide
  # Day-7 concentration, Table 4: 452 ng/mL (median)
  expect_equal(round(l$Plasma[l$Time == 7 * 1440] * 1000), 462)
  # Area, Table 4: 600 h.mcg/mL (median, to infinity); to 21 days here
  auc <- sum(diff(l$Time) * (head(l$Plasma, -1) + tail(l$Plasma, -1)) / 2) / 60
  expect_equal(round(auc), 591)
  # The label's patients (Table 4, six doses): mean peaks 5.6 to 9.0 mcg/mL
  expect_equal(round(max(l$Plasma), 1), 7.2)
  # The first dose's peak at 6 to 8 hours (label 12.3)
  first <- l$Time < 8 * 60
  expect_gt(l$Time[first][which.max(l$Plasma[first])] / 60, 5)
})

test_that("against an independent solution of the transit chain", {
  # One dose of 480 mg, F 0.944 (10.67 mg/kg), Runge-Kutta on a fine step
  p <- ding4(45); ktr <- 6 / 5.43
  F <- 1 - 0.0647 * (480 / 45 - 9.8)
  x <- c(480 * F, rep(0, 7)); h <- 0.005
  f <- function(x) c(-ktr * x[1], ktr * x[1:5] - ktr * x[2:6],
                     ktr * x[6] - (p$cl + p$q) / p$vc * x[7] + p$q / p$vp * x[8],
                     p$q / p$vc * x[7] - p$q / p$vp * x[8])
  ref <- numeric(0); check <- c(3, 6, 12, 48)
  for (i in 1:(48 / h)) {
    k1 <- f(x); k2 <- f(x + h / 2 * k1); k3 <- f(x + h / 2 * k2); k4 <- f(x + h * k3)
    x <- x + h / 6 * (k1 + 2 * k2 + 2 * k3 + k4)
    if (any(abs(i * h - check) < 1e-9)) ref <- c(ref, x[7] / p$vc)
  }
  dt <- data.frame(Drug = "lumefantrine", Time = 0, Dose = 480, Units = "mg PO")
  events <- data.frame(Time = numeric(0), Event = character(0), Fill = character(0))
  l <- withOutputTimes(check * 60, simulateDrugsWithCovariates(
    dt, events, 45, 160, 30, "male", 3 * 1440, FALSE,
    adjustToFFM = FALSE))$lumefantrine$wide
  expect_equal(l$Plasma[match(check * 60, l$Time)], ref, tolerance = 1e-6)
})

test_that("no band, no effect site, not offered on its own", {
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "lumefantrine", ]
  expect_equal(c(row$Lower, row$Typical, row$Upper, row$MEAC, row$endCe), c(0, 0, 0, 0, 0))
  expect_identical(row$Concentration.Units, "mcg")
  expect_false(isDosableDrug("lumefantrine", dd))
  expect_equal(lumefantrine(70, 170, 40, "male")$tPeak, 0)
})

test_that("pregnancy: Juma 2026, CL/F x 1.232 and VC/F x 1.281", {
  off <- lumefantrine(60, 160, 28, "female")$PK$default
  on  <- lumefantrine(60, 160, 28, "female", pregnant = TRUE)$PK$default
  expect_equal(on$cl1 / off$cl1, 1.232)
  expect_equal(on$v1 / off$v1, 1.281)
  expect_equal(on$v2, off$v2)
  expect_equal(on$cl2, off$cl2)
  expect_identical(lumefantrine(60, 160, 28, "female", pregnant = FALSE),
                   lumefantrine(60, 160, 28, "female"))
})
