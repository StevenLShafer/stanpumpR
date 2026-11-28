# CodeRabbit's review of PR #858 (2026-11-24, at f054308), the gas engine
# after ChatGPT's audit.  Each test here failed on f054308.


# A row that repeats the settings in force inside the graded ramp (the short
# sub-steps after a change of setting) dropped to the plot's step for the rest
# of the interval, so on a day's plot the coupling error of A-03 came back:
# a nitrous oxide row repeated at 2 minutes took the first half hour's error
# from 0.43 to 4.5 per cent.

test_that("gasSubSteps() says where a ramp it did not finish would go on", {
  h <- gasSubSteps(0.03, 2.4)
  expect_equal(sum(h), 0.03)
  expect_true(attr(h, "nextStep") > GAS_STEP_FIRST)
  expect_true(attr(h, "nextStep") < 2.4)
  # Carried on, the ramp starts at that step
  more <- gasSubSteps(1, 2.4, first = attr(h, "nextStep"))
  expect_equal(more[[1]], attr(h, "nextStep"))
  # A finished ramp, or uniform steps, carry nothing on
  expect_null(attr(gasSubSteps(60, 0.1), "nextStep"))
  expect_null(attr(gasSubSteps(60, 0.1, graded = FALSE), "nextStep"))
})

test_that("a row repeating the settings inside the ramp does not cut it short", {
  d <- data.frame(Time = 0, Drug = c("oxygen", "nitrousOxide", "sevoflurane", "ventilation"),
                  Dose = c(2, 4, 2, 6))
  again <- rbind(d, data.frame(Time = 2, Drug = "nitrousOxide", Dose = 4))
  ref <- advanceClosedFormGas(d, weight = 70, age = 40, maximum = 30, resolution = 3000)
  a <- advanceClosedFormGas(d, weight = 70, age = 40, maximum = 1440)
  b <- advanceClosedFormGas(again, weight = 70, age = 40, maximum = 1440)
  early <- b$timeLine > 0 & b$timeLine <= 30
  r <- stats::approx(ref$timeLine, ref$state$nitrousOxide[, 2], b$timeLine[early])$y
  err <- 100 * abs(b$state$nitrousOxide[early, 2] / r - 1)
  # Was 4.5%; the same table without the row reads 0.43%
  expect_lt(max(err), 0.6)
  # And at the times both plots have, the two agree
  at <- intersect(round(a$timeLine, 9), round(b$timeLine, 9))
  at <- at[at > 0 & at <= 30]
  expect_gt(length(at), 10)
  ia <- match(at, round(a$timeLine, 9))
  ib <- match(at, round(b$timeLine, 9))
  expect_equal(b$state$nitrousOxide[ib, 2], a$state$nitrousOxide[ia, 2], tolerance = 1e-6)
})


# GasMan's Calc() skips its checks (negative tension, more than 90% decay in a
# sub-step) at the last vernier level, so a tick there always completes.  The
# R restatement kept the checks, and when one failed at the last level it broke
# off after the first of the 16 sub-steps and recorded that as the whole tick:
# at 1000 L/min of ventilation the alveolar sevoflurane read 0 after the first
# tick and 0.05% after the second, where a tenth of the step gives 0.38 and 0.67.

test_that("a GasMan tick at the vernier cap is completed, not cut short", {
  d <- data.frame(Time = 0, Drug = c("oxygen", "sevoflurane", "ventilation"),
                  Dose = c(50, 2, 1000))
  a <- advanceGasManBaseline(d, maximum = 0.3, dt = 0.1, resolution = 4)
  f <- advanceGasManBaseline(d, maximum = 0.3, dt = 0.01, resolution = 4)
  expect_equal(a$maxVernier, GASMAN_MAX_VERNIER - 1)
  alv <- a$state$sevoflurane[, "ALV"]
  fine <- f$state$sevoflurane[, "ALV"]
  # (GasMan's own splitting error at this step is about 20%; the cut-short
  # tick was 100%)
  expect_gt(alv[[2]], 0.5 * fine[[2]])
  expect_gt(alv[[3]], 0.5 * fine[[3]])
})

test_that("the standalone GasMan restatement completes its capped ticks too", {
  f <- system.file("validation", "gasman_baseline_standalone.R", package = "stanpumpR")
  if (!nzchar(f)) f <- "../../inst/validation/gasman_baseline_standalone.R"
  if (!file.exists(f)) skip("standalone baseline not available")
  env <- new.env()
  sys.source(f, envir = env)
  run <- function(dtMs) env$gasman_simulate(list(list(name = "Sevoflurane", del = 2)),
                                            fgf = 50, va = 1000, minutes = 0.3,
                                            dt_ms = dtMs, every_seconds = 6)
  a <- run(6000)
  fine <- run(600)
  expect_equal(attr(a, "max_vernier"), env$GASMAN_MAX_VERNIER - 1)
  at <- c(0.1, 0.2)
  alv <- a$ALV[match(at, round(a$Time, 9))]
  ref <- fine$ALV[match(at, round(fine$Time, 9))]
  expect_true(all(alv > 0.5 * ref))
})


# CodeRabbit's second round on PR #858: after the ramp, the rest of an
# interval was split into round(rest / dtNominal) steps, so one step could be
# up to 1.5 times the plot's step (0.1465 at an interval of 1.13 minutes on a
# 60-minute plot, 0.1495 at 1.133).  It is now ceiling().

test_that("no sub-step after the ramp is longer than the plot's step", {
  h <- gasSubSteps(1.13, 0.1)
  expect_equal(sum(h), 1.13)
  expect_lte(max(h), 0.1 * (1 + 1e-9))
  worst <- max(vapply(seq(0.95, 3, by = 0.001),
                      function(len) max(gasSubSteps(len, 0.1)), numeric(1)))
  expect_lte(worst, 0.1 * (1 + 1e-9))
  # The cost stays a few dozen steps over the plot's own 600
  expect_lt(length(gasSubSteps(60, 0.1)), 600 + 40)
  # The uniform steps too, which the coupled engine takes after its ramp
  # (CodeRabbit on PR #864: one step of 0.149)
  expect_equal(as.vector(gasSubSteps(0.149, 0.1, graded = FALSE)), c(0.0745, 0.0745))
  expect_length(gasSubSteps(60, 0.1, graded = FALSE), 600)
  d <- data.frame(Time = 0, Drug = c("oxygen", "nitrousOxide", "sevoflurane", "ventilation"),
                  Dose = c(2, 4, 2, 6))
  again <- rbind(d, data.frame(Time = c(30, 30.149), Drug = "nitrousOxide", Dose = 4))
  b <- advanceClosedFormGas(again, weight = 70, age = 40, maximum = 60)
  expect_lte(max(diff(b$timeLine)), 0.1 * (1 + 1e-9))
})


# CodeRabbit's review of PR #864 (2026-11-24, at bfb0a6b5), the fixes for
# Gemini's audit.

geminiRemiRun <- function(maximum, bolusAt, events = NULL) {
  defaults <- getDrugDefaultsGlobal(FALSE)
  DT <- data.frame(Drug = "remifentanil", Time = bolusAt, Dose = 140, Units = "mcg")
  drugs <- suppressMessages(processdoseTable(
    DT, data.frame(Time = numeric(0), Event = character(0)),
    recalculatePK(NULL, defaults, DT, 40, 70, 170, "male"), maximum, FALSE))
  gasDose <- data.frame(Time = 0, Drug = c("oxygen", "ventilation", "sevoflurane"),
                        Dose = c(2, 6, 2))
  sim <- simulateGases(gasDose, weight = 70, age = 40, maximum = maximum,
                       physiologyEvents = events)
  washout <- gasWashout(sim, gasDose, weight = 70)
  entries <- gasDrugEntries(sim, gasDose, defaults, maximum, washout = washout, age = 40)
  list(drugs = drugs, entries = entries, out = applyOpioidMacInteraction(entries, drugs))
}

geminiRecoveryAt <- function(r, t) {
  mac <- r$entries$MAC
  aug <- gasWashoutWithTimes(mac$washout, t)
  R <- opioidMacReduction(totalOpioidMEAC(r$drugs, aug$washout$Time)$U)
  rec <- macRecoveryTime(aug$washout, 40, mac$endCe * (1 - R))
  rec[which(abs(aug$washout$Time - t) < 1e-9)]
}

# The opioid's own times join the MAC series, and the time until threshold
# was interpolated there between the gas engine's points, 2.4 minutes apart
# late in a 24-hour plot; it jumps at a bolus (13 to 200 minutes), so 1.6
# minutes after a remifentanil bolus the plotted time read 157 minutes where
# it is 208.
test_that("the opioid-adjusted time until threshold is computed at the opioid's own times", {
  local_mocked_bindings(outputComments = function(...) invisible(NULL))
  tq <- equispaceTimes(1440)[15]
  day <- geminiRemiRun(1440, tq - 1.6)
  # The reference: the same moment from a short plot, whose gas points are
  # 0.35 minute apart
  fine <- geminiRemiRun(tq + 10, tq - 1.6)
  reference <- geminiRecoveryAt(fine, tq)
  expect_gt(reference, 150)
  expect_equal(day$out$MAC$equiSpace$Recovery[15], reference, tolerance = 0.005)
})

test_that("rows are added to a washout split by physiology events too", {
  local_mocked_bindings(outputComments = function(...) invisible(NULL))
  cold <- physiologyEventsFrom(data.frame(
    Time = 20, Event = "Physiology",
    Settings = physiologyEventEncode(c(liver = 1, renal = 1, cardiac = 1, gi = 1,
                                       temperature = 33, altitude = 0,
                                       endobronchial = 0, haemoglobin = 15)),
    stringsAsFactors = FALSE))
  r <- geminiRemiRun(60, 35, events = cold)
  w <- r$entries$MAC$washout
  expect_false(is.null(w$segments))
  aug <- gasWashoutWithTimes(w, c(10.05, 35.7))
  expect_equal(length(aug$washout$Time), length(w$Time) + 2)
  expect_equal(sort(unlist(aug$washout$rows)), seq_along(aug$washout$Time))
  # The entry is built, and its time until threshold is finite throughout
  rec <- r$out$MAC$equiSpace$Recovery
  expect_true(all(is.finite(rec)))
})

# gasStateAcrossPressure() scaled the circuit's entry for every circuit, but
# only the semi-closed circuit holds gas: in the ideal circuit at a descent
# from 5000 m a 2% sevoflurane vaporiser read 3.77% for one row.  Unscaled,
# the step's row still read the inspired gas from before the step (an ascent
# kept the sea-level oxygen, and a step at the end of the plot had no later
# sub-step to correct it): it is now set from the settings in force after it.
test_that("an altitude step's row reads the inspired gas in force after it", {
  enc <- function(a) physiologyEventEncode(c(liver = 1, renal = 1, cardiac = 1, gi = 1,
                                              temperature = 37, altitude = a,
                                              endobronchial = 0, haemoglobin = 15))
  events <- function(a) physiologyEventsFrom(data.frame(
    Time = c(0, 10), Event = "Physiology", Settings = c(enc(a[1]), enc(a[2])),
    stringsAsFactors = FALSE))
  # (10 L/min of oxygen since 2026-11-28, where it was 6: the fresh gas now
  # adds a third of itself to the 6 L/min of ventilation set, so nothing is
  # rebreathed only from 8.0 L/min on the flowmeters, and this test is of
  # the inspired gas where it is the fresh gas)
  d <- data.frame(Time = 0, Drug = c("oxygen", "sevoflurane", "ventilation"), Dose = c(10, 2, 6))
  # The descent: the vaporiser's 2% throughout, and no spike with the uptake on
  for (uptake in c(FALSE, TRUE)) {
    s <- simulateGases(d, weight = 70, age = 40, maximum = 20,
                       physiologyEvents = events(c(5000, 0)), uptakeEffect = uptake)
    step <- which(s$timeLine == 10)
    after <- s$state$sevoflurane[step[2], 1]
    if (!uptake) expect_equal(after, 2)
    else expect_lt(abs(after / s$state$sevoflurane[step[2] + 1, 1] - 1), 0.01)
  }
  # The ascent: the oxygen's entry falls with the pressure at the step's row,
  # and so it does at the last row when the step ends the plot
  s <- simulateGases(d, weight = 70, age = 40, maximum = 20,
                     physiologyEvents = events(c(0, 5000)), uptakeEffect = FALSE)
  step <- which(s$timeLine == 10)
  inspired <- s$state$oxygen[step[2] + 1, 1]
  expect_lt(inspired, 0.6 * s$state$oxygen[step[1], 1])
  expect_equal(s$state$oxygen[step[2], 1], inspired)
  end <- simulateGases(d, weight = 70, age = 40, maximum = 10,
                       physiologyEvents = events(c(0, 5000)), uptakeEffect = FALSE)
  last <- length(end$timeLine)
  expect_equal(end$timeLine[last], 10)
  expect_equal(end$state$oxygen[last, 1], inspired)
})

# At a rise in pressure the ideal circuit's lungs drew fresh gas whatever the
# flow.  With no flow gasSettingsAt() gives every flowmeter gas 0 and a
# vaporiser its setting, so at a descent from 5000 m a 2% vaporiser put 0.95%
# sevoflurane in the alveoli while no oxygen was drawn.  Only the fresh gas's
# share of the inspired gas is drawn now: none at no flow.
test_that("with no fresh gas a descent draws nothing into the lungs", {
  enc <- function(a) physiologyEventEncode(c(liver = 1, renal = 1, cardiac = 1, gi = 1,
                                              temperature = 37, altitude = a,
                                              endobronchial = 0, haemoglobin = 15))
  ev <- physiologyEventsFrom(data.frame(Time = c(0, 10), Event = "Physiology",
                                        Settings = c(enc(5000), enc(0)),
                                        stringsAsFactors = FALSE))
  d <- data.frame(Time = c(0, 0, 0, 9), Drug = c("oxygen", "sevoflurane", "ventilation", "oxygen"),
                  Dose = c(2, 2, 6, 0))
  # And in apnoea, where gasCircuitBlend() calls the inspired gas all fresh
  # whatever the flow (CodeRabbit on PR #864: sevoflurane 0.99% to 1.95%)
  apnoea <- rbind(d, data.frame(Time = 9, Drug = "ventilation", Dose = 0))
  for (table in list(d, apnoea)) for (uptake in c(TRUE, FALSE)) {
    s <- simulateGases(table, weight = 70, age = 40, maximum = 20, physiologyEvents = ev,
                       uptakeEffect = uptake)
    step <- which(s$timeLine == 10)
    for (g in c("sevoflurane", "oxygen", "nitrogen"))
      expect_equal(s$state[[g]][step[2], 2], s$state[[g]][step[1], 2])
  }
})
