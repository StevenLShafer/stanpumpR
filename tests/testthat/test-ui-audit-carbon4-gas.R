# Carbon's fourth UI audit (2026-11-15), finding H9: the inhaled agents' time
# until threshold (gasCoupledRecovery() in R/gasRecovery.R).
#
#   1. A row of the coupled washout was finished as soon as it was below the
#      threshold and falling.  Isoflurane 2.5% for three hours, the lungs then
#      flushed at 25 L/min for five minutes, then a minute ventilation of
#      0.4 L/min: the brain is under a threshold of 0.6% and falling when the
#      flush ends, so the time read 0, but it rises to 0.90% and is not back
#      under 0.6% for 215.5 minutes.  Fixed: a row is finished only when the
#      uncoupled washout, an upper bound on the coupled one, cannot rise above
#      the threshold within the day.
#   2. The time points were sampled and interpolated across a change of
#      setting, where the time jumps: 112 minutes at the moment a desflurane
#      washout's ventilation fell, where the answer is 282, and 56 at the
#      sample before, where it is 0.  Fixed: every interval's first and last
#      points are sampled, and the interpolation stays within an interval.
#   3. The exponential step followed the coupling's rate down with no floor: a
#      cardiac output of 5,000 L/min (gasWashout(cardiacOutput =), which the
#      app does not set) ran for more than a minute, and 5,000,000 L/min for
#      days.  Fixed: GAS_RECOVERY_MIN_STEP.
#
# Each test fails on 59094cf, or, for part 3, runs out of its time limit.
#
# Drafted by Claude Code, 2026-11-15, at the request of Steven L. Shafer.

carbon4Rows <- function(...) do.call(rbind, lapply(list(...), function(r)
  data.frame(Time = r[[1]], Drug = r[[2]], Dose = r[[3]], stringsAsFactors = FALSE)))

# The weights that time one gas's vessel-rich group (gasRecoveryTime()'s), and
# MAC's (macRecoveryTimeExact()'s).
carbon4BrainTarget <- function(w, gas) {
  gases <- names(w$state)
  target <- numeric(4 * length(gases))
  target[(match(gas, gases) - 1) * 4 + 2] <- 1
  target
}
carbon4MacTarget <- function(w, age) {
  props <- getGasProperties()
  gases <- names(w$state)
  target <- numeric(4 * length(gases))
  for (g in intersect(props$gas[props$potent], gases))
    target[(match(g, gases) - 1) * 4 + 1] <- 1 / macForAge(props$MAC40[props$gas == g], age)
  target
}

# The manoeuvre the time until threshold stands for, in the full engine: the
# agent (or every potent agent, for MAC) turned off at t, the remaining fresh
# gas raised to 100,000 L/min so that nothing is rebreathed, and the engine run
# on (as carbon3Brute() in test-ui-audit-carbon3-gas.R, with the resolution
# and the cardiac output as arguments).  Returns the time of the last downward
# crossing, the trajectory and the state the engine had at t.
carbon4Engine <- function(gasDose, weight, t, gas, threshold, horizon,
                          perMin = 50, cardiacOutput = NULL) {
  off <- if (gas == "MAC") potentAgents() else gas
  d <- gasDose[gasDose$Time <= t, ]
  flows <- c("oxygen", "air", "nitrousOxide")
  bySetting <- split(d, d$Drug)
  now <- vapply(flows, function(g) settingAt(bySetting[[g]], t), numeric(1))
  total <- sum(now)
  now[intersect(off, flows)] <- 0
  if (sum(now) == 0) now[["oxygen"]] <- total
  now <- now / sum(now) * 1e5
  vap <- intersect(off, c("sevoflurane", "isoflurane", "desflurane"))
  d <- rbind(d, data.frame(Time = t, Drug = c(flows, vap), Dose = c(now, rep(0, length(vap))),
                           stringsAsFactors = FALSE))
  b <- advanceClosedFormGas(d, weight = weight, age = 40, maximum = t + horizon,
                            resolution = round((t + horizon) * perMin) + 1,
                            cardiacOutput = cardiacOutput)
  y <- if (gas == "MAC") b$results$Y[b$results$Drug == "MAC"] else b$state[[gas]][, 3]
  keep <- which(b$timeLine >= t - 1e-9)
  time <- b$timeLine[keep]; y <- y[keep]
  above <- which(y > threshold)
  cross <- if (length(above) == 0) 0 else {
    i <- max(above)
    if (i == length(y)) Inf else
      time[i] + (threshold - y[i]) * (time[i + 1] - time[i]) / (y[i + 1] - y[i]) - t
  }
  props <- getGasProperties()
  gases <- props$gas[props$soluble]                 # gasWashout()'s order
  list(cross = cross, s = time - t, y = y,
       Y = matrix(unlist(lapply(gases, function(g) b$state[[g]][keep[1], 2:5])), 1))
}

# Evaluated within a time limit, so that a search that does not end fails the
# test instead of holding it.
carbon4Within <- function(seconds, expr) {
  setTimeLimit(elapsed = seconds, transient = TRUE)
  on.exit(setTimeLimit(elapsed = Inf), add = TRUE)
  force(expr)
}

# Isoflurane 2.5% for three hours; then the vaporiser off and the lungs
# flushed (25 L/min, 30 L/min of oxygen) until `flushEnd`; then 0.4 L/min.
carbon4Flush <- function(agent = "isoflurane", pct = 2.5, flushEnd = 185) {
  carbon4Rows(list(0, agent, pct), list(0, "oxygen", 6), list(0, "ventilation", 6),
              list(180, agent, 0), list(180, "ventilation", 25), list(180, "oxygen", 30),
              list(flushEnd, "ventilation", 0.4))
}


test_that("H9.1: a brain under the threshold and falling is followed back above it", {
  DT <- carbon4Flush()
  sim <- simulateGases(DT, weight = 70, age = 40, maximum = 240)
  w <- gasWashout(sim, DT, weight = 70)
  at <- which(abs(w$Time - 185) < 1e-9)
  # Under the threshold and falling at the end of the flush ...
  expect_lt(sim$state$isoflurane[at, 3], 0.6)
  expect_lt(sim$state$isoflurane[at, 2], sim$state$isoflurane[at, 3])
  # ... and the full engine, run on from there, has it rise to 0.90% and come
  # back down through 0.6% 215.5 minutes later.  On 59094cf the time read 0.
  ref <- carbon4Engine(DT, 70, 185, "isoflurane", 0.6, horizon = 240, perMin = 10)
  expect_gt(max(ref$y), 0.89)
  expect_equal(ref$cross, 215.51, tolerance = 1e-4)
  raw <- gasCoupledRecovery(w, off = "isoflurane", target = carbon4BrainTarget(w, "isoflurane"),
                            threshold = 0.6, maxPoints = length(w$Time))
  expect_lt(abs(raw[at] - ref$cross), 0.01)
  # The app's own path (the sample, and the line between its points)
  app <- gasRecoveryTime(w, "isoflurane", 0.6)
  expect_lt(abs(app[at] - ref$cross), 0.01)
  # Through the flush, at the far higher ventilation, it falls for good
  before <- which(w$Time > 182 & w$Time < 185)
  expect_true(all(raw[before] < 3))
})


test_that("H9.1: the bound that finishes a row lies above the full engine's coupled washout", {
  # The argument in R/gasRecovery.R ("When a row of the washout is
  # finished"): for a gas turned off, its washout with the coupling left out
  # is an upper bound on its coupled washout, whatever the other gases do.
  # Checked against the full engine, from the engine's own state, on the
  # strongest coupling there is (MAC with 4 L/min of nitrous oxide washing
  # out) and on the flush above; and the grid bound, which is what decides,
  # lies above the sum of exponentials it bounds.
  props <- getGasProperties()
  boundAt <- function(w, t, target, Y) {
    gases <- names(w$state)
    s <- gasSettingsAt(w$bySetting, t, w$deadSpace)
    blocks <- lapply(gases, function(g)
      gasSystemSoluble(props[props$gas == g, ], w$body, s$Q, s$VA, w$Qco, Ffgf = 0,
                       totUptake = 0, circuit = "open")$A[2:5, 2:5])
    b <- gasWashoutBound(blocks, target, stats::setNames(numeric(length(gases)), gases))
    alpha <- do.call(cbind, lapply(b$modes, function(m)
      sweep(pmax(Y[, m$cols, drop = FALSE], 0) %*% m$toModes, 2, m$weight, `*`)))
    rate <- unlist(lapply(b$modes, `[[`, "rate"))
    list(b = b, B = function(s) as.vector(exp(-outer(s, rate)) %*% t(alpha)),
         U = as.vector(pmax(alpha, 0) %*% b$left + pmin(alpha, 0) %*% b$right))
  }
  dense <- c(0, exp(seq(log(1e-4), log(MINS_PER_DAY), length.out = 20000)))

  DT <- carbon4Rows(list(0, "nitrousOxide", 4), list(0, "oxygen", 2), list(0, "sevoflurane", 2),
                    list(0, "ventilation", 6))
  sim <- simulateGases(DT, weight = 70, age = 40, maximum = 60)
  w <- gasWashout(sim, DT, weight = 70)
  for (t in c(2, 30)) {
    ref <- carbon4Engine(DT, 70, t, "MAC", GAS_MAC_THRESHOLD, horizon = 40)
    bd <- boundAt(w, t, carbon4MacTarget(w, 40), ref$Y)
    expect_gte(min(bd$B(ref$s) - ref$y), -1e-12)
    expect_gte(max(bd$U), max(bd$B(dense)) - 1e-12)
  }

  DT <- carbon4Flush()
  sim <- simulateGases(DT, weight = 70, age = 40, maximum = 200)
  w <- gasWashout(sim, DT, weight = 70)
  ref <- carbon4Engine(DT, 70, 185, "isoflurane", 0.6, horizon = 120, perMin = 10)
  bd <- boundAt(w, 185, carbon4BrainTarget(w, "isoflurane"), ref$Y)
  expect_gte(min(bd$B(ref$s) - ref$y), -1e-12)
  # Close: the bound's peak is within 2% of the engine's
  expect_lt(max(bd$U), max(ref$y) * 1.02)
  # So the row is not finished while the rise is to come
  expect_false(gasWashoutBelow(bd$b, ref$Y, 0.6))
  expect_true(gasWashoutBelow(bd$b, ref$Y, 0.95))

  # No bound for a weighed gas still being breathed, or a negative weight
  blocks <- list(diag(-1, 4))
  expect_null(gasWashoutBound(blocks, c(0, 1, 0, 0), c(isoflurane = 1)))
  expect_null(gasWashoutBound(blocks, c(0, -1, 0, 0), c(isoflurane = 0)))
})


test_that("H9.2: the time until threshold is not interpolated across a change of setting", {
  # Desflurane 6% for three hours, a flush at 25 L/min from 180 to 195, then
  # 0.4 L/min: the time is 0 to the last point of the flush and 282 minutes
  # from the change, and on 59094cf the line ran between them, 56 minutes at
  # 194.5 and 112 at 195.
  DT <- carbon4Flush("desflurane", 6, flushEnd = 195)
  sim <- simulateGases(DT, weight = 70, age = 40, maximum = 300)
  w <- gasWashout(sim, DT, weight = 70)
  raw <- gasCoupledRecovery(w, off = "desflurane", target = carbon4BrainTarget(w, "desflurane"),
                            threshold = 0.6, maxPoints = length(w$Time))
  app <- gasRecoveryTime(w, "desflurane", 0.6)
  change <- which(abs(w$Time - 195) < 1e-9)
  expect_equal(raw[change - 1], 0)
  expect_equal(raw[change], 281.9, tolerance = 1e-4)
  expect_equal(app[change - 1], 0)
  expect_equal(app[change], raw[change])
  # Every interval's first and last points are integrated, not interpolated
  first <- which(c(TRUE, diff(w$interval) != 0))
  last <- c(first[-1] - 1, length(w$Time))
  expect_equal(app[c(first, last)], raw[c(first, last)])
  # and no point of the flush is drawn from the far side of the change, nor
  # any point after it from the near side
  flush <- w$interval == w$interval[change - 1]
  after <- w$interval == w$interval[change]
  expect_lte(max(app[flush]), max(raw[flush]) + 1e-9)
  # (to 1e-4 minute: between its sampled points the line is interpolated, and
  # on the grid graded from each change of setting, since 2026-11-23, that
  # reads up to 8e-8 minute under the curve; the error guarded against, a
  # reading from across the change, was 56 minutes)
  expect_gte(min(app[after & w$Time < 250]), min(raw[after & w$Time < 250]) - 1e-4)

  # The emergence scenario: the time at the moment the vaporiser is turned
  # off is the peak of the line, 11.93 minutes; on 59094cf the line was drawn
  # across it, 11.31.  The answers themselves, from every point, are
  # unchanged to the last digit.
  s <- helpScenarioById("emergence-sevoflurane")
  d <- s$doses[, c("Time", "Drug", "Dose")]
  sim <- simulateGases(d, weight = 70, age = 40, maximum = 240)
  w <- gasWashout(sim, d, weight = 70)
  thr <- gasThresholdForAge("sevoflurane", 0.21, 40)
  # The grid's points nearest 30 and 60 minutes, and 120 itself.  Since
  # 2026-11-23 the sub-steps are graded from each change of setting
  # (gasSubSteps()), so 30 and 60 are no longer points of the grid (30.011
  # and 60.007 are); the pins were 9.2484063244, 10.1613239319 and
  # 11.9260230849, and 4.9883913082, 5.9114313778 and 7.7032604568, at 30, 60
  # and 120 on the uniform grid.
  at <- vapply(c(30, 60, 120), function(t) which.min(abs(w$Time - t)), 1L)
  expect_equal(w$Time[at[3]], 120)
  raw <- gasCoupledRecovery(w, off = "sevoflurane", target = carbon4BrainTarget(w, "sevoflurane"),
                            threshold = thr, maxPoints = length(w$Time))
  rawMac <- gasCoupledRecovery(w, off = intersect(potentAgents(), names(w$state)),
                               target = carbon4MacTarget(w, 40), threshold = GAS_MAC_THRESHOLD,
                               maxPoints = length(w$Time))
  expect_equal(raw[at], c(9.2487904421, 10.1615432004, 11.9260224320), tolerance = 1e-10)
  expect_equal(rawMac[at], c(4.9887751535, 5.9116560370, 7.7032597883), tolerance = 1e-10)
  expect_equal(gasRecoveryTime(w, "sevoflurane", thr)[at[3]], raw[at[3]])
  expect_equal(macRecoveryTime(w, 40, GAS_MAC_THRESHOLD)[at[3]], rawMac[at[3]])
})


test_that("H9.3: the coupled washout's step has a floor", {
  expect_equal(gasRecoveryStep(0, 1e9), GAS_RECOVERY_MIN_STEP)
  expect_equal(gasRecoveryStep(60, 1e9), GAS_RECOVERY_MIN_STEP)
  # It binds nowhere the app reaches: the coupling's rate is largest at the
  # app's lightest patient, and even a vaporiser at 100% there gives a step
  # above it.
  body <- getGasBody(MIN_WEIGHT)
  props <- getGasProperties()
  lb <- props$lambda_blood[match(c("isoflurane", "nitrousOxide", "nitrogen"), props$gas)]
  coupling <- 2 * body$Q_cardiac / body$V_alveolar * sum(lb * c(100, 100, 100)) / 100 +
    body$VO2 * (1 - GAS_RESPIRATORY_QUOTIENT) / body$V_alveolar
  expect_gt(1 / coupling, GAS_RECOVERY_MIN_STEP)
  expect_equal(gasRecoveryStep(0, 4.2), GAS_RECOVERY_STEP_FIRST_HOUR)
  expect_equal(gasRecoveryStep(60, 7.5), GAS_RECOVERY_STEP_LATER)
})


test_that("H9.3: a cardiac output of 5,000,000 L/min is timed in bounded time, and right", {
  # Through the R API: the app's cardiac output follows the weight.  On
  # 59094cf the exponential step was 1e-6 minute here and the search did not
  # end (5,000 L/min ran for more than a minute).
  DT <- carbon4Rows(list(0, "sevoflurane", 2), list(0, "oxygen", 2), list(0, "nitrousOxide", 4),
                    list(0, "ventilation", 6))
  co <- 5e6
  sim <- simulateGases(DT, weight = 70, age = 40, maximum = 20, cardiacOutput = co)
  w <- gasWashout(sim, DT, weight = 70, cardiacOutput = co)
  mac <- carbon4Within(60, macRecoveryTime(w, 40, GAS_MAC_THRESHOLD))
  expect_true(all(is.finite(mac)) && all(mac >= 0))
  # Against the full engine at 0.01 minute: within two seconds
  ref <- carbon4Engine(DT, 70, 5, "MAC", GAS_MAC_THRESHOLD, horizon = 20, perMin = 100,
                       cardiacOutput = co)
  expect_gt(ref$cross, 10)
  expect_lt(abs(stats::approx(w$Time, mac, 5)$y - ref$cross), 0.05)
})
