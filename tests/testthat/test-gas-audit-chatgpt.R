# ChatGPT's audit of the gas engine, 2026-11-23 (at stanpumpR c90c33e and
# GasMan d3a2dd3; docs/gas-engine-audit/chatgpt-2026-11-23-assessment.md).
# Every test here but the uncoupled advance's uniform steps, a guard on what
# did not change, failed on 23b87a2, before the fixes.  A-04's, the frozen
# GasMan C++ output, is in test-gas-scenarios.R with the scenarios it checks.


# A-03.  The coupled advance holds the summed uptake over each sub-step, and
# the sub-step was the plot's length over 600, so a 24-hour plot stepped 2.4
# minutes from the start of a nitrous oxide induction and read 13% low at its
# first point.  The sub-steps now start at 0.01 minute after each change and
# grow by a tenth.

test_that("gasSubSteps() grades from each change and sums to the interval", {
  h <- gasSubSteps(60, 0.1)
  expect_equal(sum(h), 60)
  expect_equal(h[[1]], GAS_STEP_FIRST)
  expect_equal(h[[2]] / h[[1]], GAS_STEP_GROWTH)
  # The rest of the interval at the plot's step, to the rounding of its count
  expect_lte(max(h), 0.1 * (1 + 1e-9))
  # Uniform where asked, and where the plot's own step is already short
  expect_equal(gasSubSteps(60, 0.1, graded = FALSE), rep(0.1, 600))
  expect_equal(gasSubSteps(1, 0.005), rep(0.005, 200))
  # An interval shorter than the grading
  short <- gasSubSteps(0.03, 2.4)
  expect_equal(sum(short), 0.03)
  expect_true(all(short > 0))
  # The cost: a day's plot takes a few dozen more steps than its 600
  expect_lt(length(gasSubSteps(1440, 1440 / 601)), 700)
})

test_that("a nitrous oxide induction reads the same on a day's plot as on a short one", {
  d <- data.frame(Time = 0, Drug = c("oxygen", "nitrousOxide", "sevoflurane", "ventilation"),
                  Dose = c(2, 4, 2, 6))
  # Reference: 12 minutes at a 0.01-minute step throughout
  ref <- advanceClosedFormGas(d, weight = 70, age = 40, maximum = 12, resolution = 1200)
  day <- advanceClosedFormGas(d, weight = 70, age = 40, maximum = 1440)
  early <- day$timeLine > 0 & day$timeLine <= 12
  for (g in c("nitrousOxide", "sevoflurane")) {
    r <- stats::approx(ref$timeLine, ref$state[[g]][, 2], day$timeLine[early])$y
    err <- 100 * abs(day$state[[g]][early, 2] / r - 1)
    # Was 13.4% (nitrous oxide) and 12.5% (sevoflurane) at the first point
    expect_lt(max(err), 0.6, label = paste(g, "largest error, %"))
  }
  # The first plotted point is now a hundredth of a minute in, not 2.4 minutes
  expect_equal(day$timeLine[[2]], GAS_STEP_FIRST)
})

test_that("without the uptake coupling the steps stay uniform, and the answer exact", {
  d <- data.frame(Time = 0, Drug = c("oxygen", "sevoflurane", "ventilation"), Dose = c(6, 2, 6))
  a <- advanceClosedFormGas(d, maximum = 60, uptakeEffect = FALSE)
  expect_equal(diff(a$timeLine), rep(60 / 601, 601), tolerance = 1e-9)
})

test_that("a row that repeats the settings does not restart the grading", {
  d <- data.frame(Time = 0, Drug = c("oxygen", "sevoflurane", "ventilation"), Dose = c(6, 2, 6))
  again <- rbind(d, data.frame(Time = 30, Drug = "sevoflurane", Dose = 2))
  a <- advanceClosedFormGas(d, maximum = 60)
  b <- advanceClosedFormGas(again, maximum = 60)
  expect_equal(length(b$timeLine), length(a$timeLine), tolerance = 0.02)
  # A real change at 30 does restart it
  changed <- rbind(d, data.frame(Time = 30, Drug = "sevoflurane", Dose = 3))
  c3 <- advanceClosedFormGas(changed, maximum = 60)
  expect_true(any(abs(c3$timeLine - (30 + GAS_STEP_FIRST)) < 1e-9))
  expect_false(any(abs(b$timeLine - (30 + GAS_STEP_FIRST)) < 1e-9))
})


# C-02.  A physiology event at the plot's last minute was never taken (the
# loop over intervals stopped one change point short), and the volatile
# agent's band was drawn at a 40-year-old's MAC whatever the patient's age.

coldAt <- function(t) {
  physiologyEventsFrom(data.frame(
    Time = t, Event = "Physiology",
    Settings = physiologyEventEncode(c(liver = 1, renal = 1, cardiac = 1, gi = 1,
                                       temperature = 32, altitude = 0,
                                       endobronchial = 0, haemoglobin = 15)),
    stringsAsFactors = FALSE))
}

test_that("a physiology event at the end of the plot is taken there", {
  d <- data.frame(Time = 0, Drug = c("oxygen", "sevoflurane", "ventilation"), Dose = c(6, 2, 6))
  atEnd <- simulateGases(d, age = 40, maximum = 30, physiologyEvents = coldAt(30))
  longer <- simulateGases(d, age = 40, maximum = 40, physiologyEvents = coldAt(30))
  mac <- function(s) s$results[s$results$Drug == "MAC", ]
  endRows <- mac(atEnd)[mac(atEnd)$Time == 30, "Y"]
  # Before and after the step, the same time twice, as for any event
  expect_length(endRows, 2)
  expect_equal(utils::tail(atEnd$bodyRow, 1), 2L)
  midRows <- mac(longer)[mac(longer)$Time == 30, "Y"]
  # (the two plots step differently, so they agree to the step's error)
  expect_equal(endRows, midRows, tolerance = 1e-3)
  # Colder: MAC lower, so more MAC-equivalents at the same tension (0.83 to 1.08)
  expect_gt(endRows[[2]], endRows[[1]] * 1.25)
})

test_that("a volatile agent's band is the patient's MAC, at the patient's age", {
  props <- getGasProperties()
  expect_equal(gasTypicalBand("sevoflurane", props),
               c(lower = 0.7 * 1.8, typical = 1.8, upper = 1.3 * 1.8))
  b80 <- gasTypicalBand("sevoflurane", props, age = 80)
  expect_equal(b80[["typical"]], macForAge(1.8, 80))
  expect_equal(b80[["typical"]], 1.404988, tolerance = 1e-6)
  # Oxygen and nitrous oxide keep their delivery ranges at any age
  expect_equal(gasTypicalBand("nitrousOxide", props, age = 80),
               c(lower = 50, typical = 60, upper = 70))
  d <- data.frame(Time = 0, Drug = c("oxygen", "sevoflurane", "ventilation"), Dose = c(6, 2, 6))
  e <- gasDrugEntries(simulateGases(d, age = 80, maximum = 30), d, maximum = 30, age = 80)
  expect_equal(e$sevoflurane$typical, macForAge(1.8, 80))
  # The band and the default threshold now follow age alike: the threshold
  # stays a tenth of the band's MAC
  expect_equal(gasThresholdForAge("sevoflurane", 0.18, 80), 0.1 * b80[["typical"]],
               tolerance = 1e-9)
})


# D-01.  The comment on the opioid-adjusted time until threshold said it
# always erred long; the help page, rightly, says the error has no fixed
# direction.

test_that("the opioid-MAC code and its help agree that the error has no fixed direction", {
  f <- "../../R/opioidMacInteraction.R"
  if (!file.exists(f)) skip("package source not available")
  code <- paste(readLines(f), collapse = " ")
  expect_false(grepl("errs on the side of a longer", code))
  expect_true(grepl("no\\s+#?\\s*fixed direction", code))
})


# C-01, decided by Shafer on 2026-11-24: every potent gas's threshold, the
# default or one typed, is its value at 37 C and moves with temperature as its
# MAC does, as a volatile agent's moves with age.  Until then a threshold was
# held at its typed tension whatever the temperature.

test_that("a gas's threshold is its 37 C value, moved with MAC", {
  f32 <- gasMacTemperatureFactor(32)
  expect_equal(gasThresholdForTemperature("sevoflurane", 0.18, 32), 0.18 * f32)
  expect_equal(gasThresholdForTemperature("nitrousOxide", 10, 32),
               10 * gasMacTemperatureFactor(32, "nitrousOxide"))
  # Unchanged at 37 C, with no temperature, and for anything not potent
  expect_equal(gasThresholdForTemperature("sevoflurane", 0.18, 37), 0.18)
  expect_equal(gasThresholdForTemperature("sevoflurane", 0.18, NULL), 0.18)
  expect_equal(gasThresholdForTemperature(c("propofol", "oxygen"), c(1, 5), 32), c(1, 5))
})

test_that("at 32 C the time until threshold is to a tenth of the cooled MAC", {
  d <- data.frame(Time = 0, Drug = c("oxygen", "sevoflurane", "ventilation"), Dose = c(6, 2, 6))
  s <- simulateGases(d, age = 40, maximum = 30, temperature = 32)
  w <- gasWashout(s, d)
  f32 <- gasMacTemperatureFactor(32)
  moved <- gasRecoveryTime(w, "sevoflurane", 0.18)
  expect_equal(moved, gasRecoveryTime(w, "sevoflurane", 0.18 * f32, atBodyTemperature = FALSE))
  # 13.24 minutes; held at 0.18% it was 11.57 (12.76 and 11.19 until
  # 2026-11-28, without the 10% shunt under anesthesia, which slows the
  # brain's washout as it slows its wash-in)
  # (12.46 and 10.81 since Esper's blood:gas coefficients and the tissues'
  # own temperature slopes, the same day)
  # (10.61 and 9.31 since the fresh gas adds to the delivered ventilation
  # and the washout runs at 1.5 times the setting, the same day)
  expect_equal(utils::tail(moved, 1), 10.61, tolerance = 2e-3)
  held <- gasRecoveryTime(w, "sevoflurane", 0.18, atBodyTemperature = FALSE)
  expect_equal(utils::tail(held, 1), 9.31, tolerance = 2e-3)
  # The entry draws the threshold at the temperature in force at time 0
  e <- gasDrugEntries(s, d, maximum = 30, washout = w, age = 40)
  expect_equal(e$sevoflurane$endCe, 0.18 * f32)
  expect_equal(utils::tail(e$sevoflurane$equiSpace$Recovery, 1), utils::tail(moved, 1),
               tolerance = 1e-6)
  # At 37 C nothing moves
  s37 <- simulateGases(d, age = 40, maximum = 30)
  w37 <- gasWashout(s37, d)
  expect_equal(gasRecoveryTime(w37, "sevoflurane", 0.18),
               gasRecoveryTime(w37, "sevoflurane", 0.18, atBodyTemperature = FALSE))
})

test_that("with a temperature event each moment's threshold is the one in force then", {
  d <- data.frame(Time = 0, Drug = c("oxygen", "sevoflurane", "ventilation"), Dose = c(6, 2, 6))
  s <- simulateGases(d, age = 40, maximum = 30, physiologyEvents = coldAt(15))
  w <- gasWashout(s, d)
  expect_length(w$segments, 2)
  f32 <- gasMacTemperatureFactor(32)
  r <- gasRecoveryTime(w, "sevoflurane", 0.18)
  before <- w$rows[[1]]
  after <- w$rows[[2]]
  expect_equal(r[before], gasRecoveryTime(w, "sevoflurane", 0.18, atBodyTemperature = FALSE)[before])
  expect_equal(r[after], gasRecoveryTime(w, "sevoflurane", 0.18 * f32, atBodyTemperature = FALSE)[after])
  # Colder from minute 15: the threshold is lower, so the time is longer
  expect_gt(r[after][1], r[before][length(before)])
})
