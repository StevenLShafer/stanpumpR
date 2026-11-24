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
})
