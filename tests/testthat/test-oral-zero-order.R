# Zero-order oral release (R/oral-zero-order.R): a dose delivered at a
# constant rate over a fitted duration, after a lag.  Drafted by Claude Code,
# 2026-10-17.
#
# The curve is checked against an independent closed-form solution of the
# two-compartment constant-rate input, written here from the micro constants
# rather than taken from the engine.

events <- data.frame(Time = double(), Event = character())

# An independent analytic solution: the central concentration of a
# two-compartment model receiving rate R on [lag, lag + D1), then nothing.
analyticZeroOrder <- function(t, dose, V1, V2, CL, Q, lag, D1, scale = 1000)
{
  k10 <- CL / V1; k12 <- Q / V1; k21 <- Q / V2
  b <- k10 + k12 + k21; c2 <- k10 * k21
  l <- c((b + sqrt(b^2 - 4 * c2)) / 2, (b - sqrt(b^2 - 4 * c2)) / 2)
  coef <- c((l[1] - k21), (k21 - l[2])) / (V1 * (l[1] - l[2]))
  R <- dose / D1
  vapply(t - lag, function(u) {
    if (u <= 0) return(0)
    if (u < D1) sum(coef / l * (1 - exp(-l * u))) * R * scale
    else sum(coef / l * (1 - exp(-l * D1)) * exp(-l * (u - D1))) * R * scale
  }, numeric(1))
}

test_that("the block is validated, and a bad one is refused", {
  expect_null(validateOralZeroOrder(NULL, "x"))
  expect_equal(validateOralZeroOrder(list(duration = 30), "x"),
               list(duration = 30, lag = 0))
  expect_equal(validateOralZeroOrder(list(duration = 30, lag = 5), "x"),
               list(duration = 30, lag = 5))
  expect_error(validateOralZeroOrder(list(duration = 0), "x"), "duration")
  expect_error(validateOralZeroOrder(list(duration = -1), "x"), "duration")
  expect_error(validateOralZeroOrder(list(lag = 5), "x"), "duration")
  expect_error(validateOralZeroOrder(list(duration = 30, lag = -1), "x"), "lag")
  expect_error(validateOralZeroOrder("30", "x"), "a list")
})

test_that("one dose becomes the two rate rows of its release", {
  dose <- data.frame(Drug = "x", Time = 0, Dose = 12, Units = "mg PO",
                     stringsAsFactors = FALSE)
  out <- expandOralZeroOrder(dose, list(duration = 24, lag = 10), 1440)
  expect_equal(nrow(out), 2)
  expect_equal(out$Time, c(10, 34))
  # Rate is the dose over the duration, in the amount unit per minute
  expect_equal_rounded(c(0.5, 0), out$Dose)
  expect_equal(unique(out$Units), "mg/min PO")
  # The rows are an infusion, so the route word survives but the row is not
  # classified as an oral bolus
  expect_true(all(doseRoute(out$Units) == ROUTE_PO))
  expect_true(all(isRateUnit(out$Units)))
})

test_that("overlapping releases add instead of replacing one another", {
  # Two 10 mg doses an hour apart, each released over five hours: the rate
  # doubles while both are running.  The infusion rate on the time line is a
  # step function, so the expansion has to emit the total.
  dose <- data.frame(Drug = "x", Time = c(0, 60), Dose = c(10, 10),
                     Units = "mg PO", stringsAsFactors = FALSE)
  out <- expandOralZeroOrder(dose, list(duration = 300, lag = 0), 1440)
  expect_equal(out$Time, c(0, 60, 300, 360))
  expect_equal_rounded(c(10 / 300, 20 / 300, 10 / 300, 0), out$Dose)
  # Every milligram is still delivered: the rate integrated over time is the
  # total dose given
  width <- diff(c(out$Time, 360))
  expect_equal_rounded(20, sum(out$Dose * width))
})

test_that("a per-kilogram oral dose stays per-kilogram", {
  dose <- data.frame(Drug = "x", Time = 0, Dose = 2, Units = "mg/kg PO",
                     stringsAsFactors = FALSE)
  out <- expandOralZeroOrder(dose, list(duration = 20, lag = 0), 1440)
  expect_equal(unique(out$Units), "mg/kg/min PO")
  expect_equal_rounded(c(0.1, 0), out$Dose)
})

test_that("other routes and rate rows are left alone", {
  dose <- data.frame(
    Drug = "x", Time = c(0, 0, 0), Dose = c(1, 2, 3),
    Units = c("mg", "mg IM", "mg/day PO"), stringsAsFactors = FALSE)
  out <- expandOralZeroOrder(dose, list(duration = 20, lag = 0), 1440)
  expect_equal(nrow(out), 3)
  expect_setequal(out$Units, dose$Units)
})

test_that("a release beginning at or after the end of the plot is dropped", {
  dose <- data.frame(Drug = "x", Time = 100, Dose = 10, Units = "mg PO",
                     stringsAsFactors = FALSE)
  # Lag puts the start past the end
  out <- expandOralZeroOrder(dose, list(duration = 20, lag = 50), 120)
  expect_equal(nrow(out), 0)
  # Starting inside the window but ending outside keeps the start only: the
  # step function then holds that rate to the end, which is the release
  out <- expandOralZeroOrder(dose, list(duration = 60, lag = 0), 120)
  expect_equal(out$Time, 100)
  expect_equal_rounded(10 / 60, out$Dose)
})

test_that("the simulated curve matches an independent closed-form solution", {
  # carvedilol is the library's zero-order drug: 12.5 mg, the source's single
  # dose, in its reference patient with the scaling switch off
  wide <- simulateDrugsWithCovariates(
    data.frame(Drug = "carvedilol", Time = 0, Dose = 12.5, Units = "mg PO"),
    events, 70, 174, 27, "male", 1440, FALSE,
    adjustToFFM = FALSE)$carvedilol$wide
  expected <- analyticZeroOrder(
    wide$Time, dose = 12.5, V1 = 440, V2 = 754, CL = 153 / 60, Q = 41.3 / 60,
    lag = 0.215 * 60, D1 = 0.383 * 60)
  # Machine precision: the engine is solving the same integral
  expect_equal(wide$Plasma, expected, tolerance = 1e-10)
})

test_that("nothing is absorbed before the lag, and the peak is where release ends", {
  wide <- simulateDrugsWithCovariates(
    data.frame(Drug = "carvedilol", Time = 0, Dose = 25, Units = "mg PO"),
    events, 70, 174, 27, "male", 1440, FALSE,
    adjustToFFM = FALSE)$carvedilol$wide
  lag <- 0.215 * 60
  expect_true(all(wide$Plasma[wide$Time <= lag] == 0))
  expect_true(all(wide$Plasma[wide$Time > lag] > 0))
  # A constant input into a linear system rises throughout, so the maximum is
  # exactly at the end of release
  expect_equal_rounded(lag + 0.383 * 60, wide$Time[which.max(wide$Plasma)])
})

test_that("the whole dose arrives once, with no second multiplication by F", {
  # The specification's fixture for an apparent model: an apparent CL/F and
  # V/F set receives the FULL dose in apparent coordinates, so the area under
  # the curve is dose over apparent clearance.  Multiplying by a
  # bioavailability again would shrink it.
  wide <- simulateDrugsWithCovariates(
    data.frame(Drug = "carvedilol", Time = 0, Dose = 25, Units = "mg PO"),
    events, 70, 174, 27, "male", 20160, FALSE,
    adjustToFFM = FALSE)$carvedilol$wide
  auc <- sum(diff(wide$Time) *
               (head(wide$Plasma, -1) + tail(wide$Plasma, -1)) / 2)
  expect_equal(25 / (153 / 60) * 1000, auc, tolerance = 0.01)
  # And the engine applies no bioavailability, because the apparent scale
  # already contains it
  set <- getDrugPK("carvedilol", 70, 174, 27, "male")$PK$default
  expect_equal(set$ka_PO, 0)
  expect_equal(set$bioavailability_PO, 0)
})

test_that("the release is linear in the dose", {
  one <- simulateDrugsWithCovariates(
    data.frame(Drug = "carvedilol", Time = 0, Dose = 12.5, Units = "mg PO"),
    events, 70, 174, 27, "male", 720, FALSE,
    adjustToFFM = FALSE)$carvedilol$wide
  two <- simulateDrugsWithCovariates(
    data.frame(Drug = "carvedilol", Time = 0, Dose = 25, Units = "mg PO"),
    events, 70, 174, 27, "male", 720, FALSE,
    adjustToFFM = FALSE)$carvedilol$wide
  expect_equal(2 * one$Plasma, two$Plasma, tolerance = 1e-10)
})

test_that("a scheduled zero-order dose repeats and each repeat is released", {
  # "mg PO qd" expands to its repeats first, and every repeat then becomes its
  # own release: the expansions compose in that order.
  wide <- simulateDrugsWithCovariates(
    data.frame(Drug = "carvedilol", Time = 0, Dose = 25, Units = "mg PO qd"),
    events, 70, 174, 27, "male", 4320, FALSE,
    adjustToFFM = FALSE)$carvedilol$wide
  lag <- 0.215 * 60
  # A peak after each of the three daily doses, each at the end of its release
  for (day in 0:2) {
    peakAt <- day * MINS_PER_DAY + lag + 0.383 * 60
    near <- abs(wide$Time - peakAt) < 1
    expect_true(any(near), info = day)
  }
  # Accumulation: the third day's trough exceeds the first day's
  trough <- function(day) wide$Plasma[which.min(abs(wide$Time - (day + 1) * MINS_PER_DAY))]
  expect_gt(trough(2), trough(0))
})

test_that("the mode is refused alongside anything that rewrites the oral rows", {
  base <- function(extra) {
    model <- list(
      PK = list(default = list(v1 = 10, v2 = 10, v3 = 1,
                               cl1 = 1, cl2 = 1, cl3 = 0)),
      tPeak = 0,
      oralZeroOrder = list(duration = 20)
    )
    c(model, extra)
  }
  for (extra in list(
        list(metabolite = list(name = "morphine", kFormation = 0.001)),
        list(oralFormulations = list(ER = list(ka_PO = 0.01))),
        list(oralSaturation = list(form = "saturable", d50 = 100)),
        list(sublingualSaturation = list(form = "saturable", d50 = 100)),
        list(parallelSystems = list(list(name = "a", PK = list()))),
        list(michaelisMenten = list(vmax = 1, km = 1)),
        list(osmotic = list(molecularWeight = 182)),
        list(molar = list(molecularWeight = 147))))
    expect_error(
      getDrugPK("carvedilol", 70, 170, 35, "male", model = base(extra)),
      "oralZeroOrder", info = names(extra)[1])
})

test_that("a dose table's own column types are tolerated", {
  # The dose table carries Time as typed, and a hand-built table may carry
  # Dose as text too; the expansion coerces rather than failing.
  dose <- data.frame(Drug = "x", Time = c("0", "720"), Dose = c("12.5", "25"),
                     Units = "mg PO", stringsAsFactors = FALSE)
  out <- expandOralZeroOrder(dose, list(duration = 22.98, lag = 12.9), 1440)
  expect_equal(nrow(out), 4)
  expect_equal_rounded(c(12.90, 35.88, 732.90, 755.88), as.numeric(out$Time))
  expect_equal_rounded(c(12.5 / 22.98, 0, 25 / 22.98, 0), as.numeric(out$Dose))
})

test_that("two doses at one instant release together", {
  dose <- data.frame(Drug = "x", Time = c(0, 0), Dose = c(10, 10),
                     Units = "mg PO", stringsAsFactors = FALSE)
  out <- expandOralZeroOrder(dose, list(duration = 20, lag = 0), 600)
  expect_equal(out$Time, c(0, 20))
  expect_equal_rounded(c(1, 0), out$Dose)
})

test_that("a dose of zero releases nothing and breaks nothing", {
  dose <- data.frame(Drug = "x", Time = 0, Dose = 0, Units = "mg PO",
                     stringsAsFactors = FALSE)
  out <- expandOralZeroOrder(dose, list(duration = 20, lag = 0), 600)
  expect_equal_rounded(0, out$Dose)
})
