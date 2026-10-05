# "Time until threshold" for the inhaled agents and MAC, checked against the
# definition (Shafer, 2026-10-05): turn the agent off, turn the fresh gas flow
# up so that there is no rebreathing, leave the ventilation alone, and see when
# the concentration comes down through the threshold.  Each agent is its own
# decision; turning off the vaporiser and turning off the nitrous oxide are
# separate.
#
# The reference here is the full, coupled engine with the same thing done to
# the dose table, the fresh gas flow being raised to 1000 L/min to stand for
# "no rebreathing".  The fast method in R/gasRecovery.R leaves the uptake
# coupling out of the washout; these tests measure what that costs.
#
# (Claude Code, Claude Fable 5.1, 2026-10-05; run on R 4.6.1.)

gasRows <- function(...) do.call(rbind, lapply(list(...), function(r)
  data.frame(Time = r[[1]], Drug = r[[2]], Dose = r[[3]], stringsAsFactors = FALSE)))

# The dose table with `off` turned off at t.  With flush = TRUE the remaining
# fresh gas flows are scaled up to 1000 L/min in total, which keeps the mixture
# and removes the rebreathing; if nothing else is flowing, oxygen is.
turnedOff <- function(gasDose, t, off, flush = TRUE) {
  d <- gasDose[gasDose$Time <= t, ]
  d <- d[order(d$Time), ]
  bySetting <- split(d, d$Drug)
  flows <- c("oxygen", "air", "nitrousOxide")
  now <- vapply(flows, function(g) settingAt(bySetting[[g]], t), numeric(1))
  total <- sum(now)
  now[intersect(off, flows)] <- 0
  if (sum(now) == 0) now[["oxygen"]] <- total
  if (!flush && "nitrousOxide" %in% off) now[["oxygen"]] <- total - now[["air"]]
  if (flush) now <- now / sum(now) * 1000
  vap <- intersect(off, c("sevoflurane", "isoflurane", "desflurane"))
  rbind(d, data.frame(Time = t, Drug = c(flows, vap), Dose = c(now, rep(0, length(vap))),
                      stringsAsFactors = FALSE))
}

lastCrossing <- function(time, y, threshold, t) {
  keep <- time >= t; time <- time[keep]; y <- y[keep]
  above <- which(y > threshold)
  if (length(above) == 0) return(0)
  i <- max(above)
  time[i] + (threshold - y[i]) * (time[i + 1] - time[i]) / (y[i + 1] - y[i]) - t
}

bruteGas <- function(gasDose, weight, age, t, gas, threshold, off = gas,
                     flush = TRUE, horizon = 240) {
  if (identical(off, "MAC")) off <- potentAgents()
  b <- advanceClosedFormGas(turnedOff(gasDose, t, off, flush), weight = weight, age = age,
                            maximum = t + horizon,
                            resolution = round((t + horizon) * 10) + 1)
  if (gas == "MAC") {
    m <- b$results[b$results$Drug == "MAC", ]
    lastCrossing(m$Time, m$Y, threshold, t)
  } else {
    lastCrossing(b$timeLine, b$state[[gas]][, 3], threshold, t)   # vessel-rich group
  }
}

at <- function(w, series, t) stats::approx(w$Time, series, t)$y


test_that("a volatile agent alone: matches turning it off with no rebreathing", {
  gasDose <- gasRows(list(0, "oxygen", 2), list(0, "ventilation", 4), list(0, "sevoflurane", 2))
  sim <- simulateGases(gasDose, weight = 70, age = 40, maximum = 120)
  w <- gasWashout(sim, gasDose, weight = 70)
  agent <- gasRecoveryTime(w, "sevoflurane", 0.21)
  mac   <- macRecoveryTime(w, 40, 0.1)

  for (t in c(15, 60, 119)) {
    expect_lt(abs(at(w, agent, t) - bruteGas(gasDose, 70, 40, t, "sevoflurane", 0.21)), 0.2)
    expect_lt(abs(at(w, mac, t)   - bruteGas(gasDose, 70, 40, t, "MAC", 0.1, off = "MAC")), 0.2)
  }

  # Longer anaesthetic, longer wait: the tissues have had time to fill.
  expect_true(all(diff(at(w, agent, c(15, 30, 60, 119))) > 0))
  # Nothing to wait for before the brain has reached the threshold.
  expect_equal(agent[1], 0)
})


test_that("no rebreathing: the answer does not depend on the fresh gas flow in use", {
  # Same vaporiser, same ventilation, same brain tension reached by different
  # routes is not available, so compare the WASHOUT instead: once the flow is
  # turned up, what it was before does not matter.  Low flow slows the
  # rebreathing washout a great deal and the no-rebreathing one not at all.
  gasDose <- gasRows(list(0, "oxygen", 4), list(0, "ventilation", 4),
                     list(0, "isoflurane", 1.5), list(15, "oxygen", 0.5))
  sim <- simulateGases(gasDose, weight = 70, age = 60, maximum = 180)
  clean <- gasWashout(sim, gasDose, weight = 70)
  stale <- gasWashout(sim, gasDose, weight = 70, rebreathing = TRUE)
  thr <- gasThresholdForAge("isoflurane", 0.11, 60)

  tClean <- gasRecoveryTime(clean, "isoflurane", thr)
  tStale <- gasRecoveryTime(stale, "isoflurane", thr)
  for (t in c(30, 90, 179)) {
    expect_lt(abs(at(clean, tClean, t) -
                    bruteGas(gasDose, 70, 60, t, "isoflurane", thr, horizon = 600)), 0.3)
    expect_lt(abs(at(stale, tStale, t) -
                    bruteGas(gasDose, 70, 60, t, "isoflurane", thr, flush = FALSE,
                             horizon = 900)) / at(stale, tStale, t), 0.01)
    # Leaving half a litre a minute running is far slower than flushing.
    expect_gt(at(stale, tStale, t), 2 * at(clean, tClean, t))
  }
  # No jump in the no-rebreathing time when the flow is turned down at 15 min...
  around <- at(clean, tClean, c(14.5, 15.5))
  expect_lt(abs(diff(around)), 1)
  # ...where the rebreathing time jumps at once.
  expect_gt(diff(at(stale, tStale, c(14.5, 15.5))), 5)
})


test_that("each agent is its own decision: the vaporiser off, nitrous oxide left running", {
  gasDose <- gasRows(list(0, "oxygen", 2.3), list(0, "nitrousOxide", 5.7),
                     list(0, "ventilation", 4), list(0, "sevoflurane", 2))
  sim <- simulateGases(gasDose, weight = 70, age = 40, maximum = 120)
  w <- gasWashout(sim, gasDose, weight = 70)
  sevo <- gasRecoveryTime(w, "sevoflurane", 0.21)
  n2o  <- gasRecoveryTime(w, "nitrousOxide", 10)

  for (t in c(30, 119)) {
    # Sevoflurane off, nitrous oxide still on: within 2%.
    want <- bruteGas(gasDose, 70, 40, t, "sevoflurane", 0.21, off = "sevoflurane")
    expect_lt(abs(at(w, sevo, t) - want) / want, 0.02)
    # Nitrous oxide off, sevoflurane still on: it is the big one, so the
    # coupling it carries matters more.  Within 10% (8.5% when written).
    want <- bruteGas(gasDose, 70, 40, t, "nitrousOxide", 10, off = "nitrousOxide")
    expect_lt(abs(at(w, n2o, t) - want) / want, 0.10)
  }
  # Nitrous oxide comes off faster than sevoflurane, even with further to go
  # in proportion (68% down to 10%).
  expect_lt(at(w, n2o, 119), at(w, sevo, 119))
})


test_that("both turned off together: the neglected coupling costs 5% for the agent and 16% for MAC, and errs long", {
  gasDose <- gasRows(list(0, "oxygen", 2.3), list(0, "nitrousOxide", 5.7),
                     list(0, "ventilation", 4), list(0, "sevoflurane", 2))
  sim <- simulateGases(gasDose, weight = 70, age = 40, maximum = 120)
  w <- gasWashout(sim, gasDose, weight = 70)
  sevo <- gasRecoveryTime(w, "sevoflurane", 0.21)
  mac  <- macRecoveryTime(w, 40, 0.1)
  both <- c("sevoflurane", "nitrousOxide")
  for (t in c(30, 119)) {
    want <- bruteGas(gasDose, 70, 40, t, "sevoflurane", 0.21, off = both)
    expect_gte(at(w, sevo, t), want - 0.05)
    expect_lt((at(w, sevo, t) - want) / want, 0.07)     # 5.4% when written

    want <- bruteGas(gasDose, 70, 40, t, "MAC", 0.1, off = "MAC")
    expect_gte(at(w, mac, t), want - 0.05)
    expect_lt((at(w, mac, t) - want) / want, 0.18)      # 15.8% when written
  }
})


test_that("thresholds follow MAC with age for the volatile agents only", {
  # At the reference age nothing changes.
  expect_equal(gasThresholdForAge(c("sevoflurane", "nitrousOxide", "propofol"), c(0.21, 10, 1), 40),
               c(0.21, 10, 1))
  # Older: the volatile threshold falls with MAC; the others do not move.
  old <- gasThresholdForAge(c("sevoflurane", "isoflurane", "desflurane", "nitrousOxide", "fentanyl"),
                            c(0.21, 0.11, 0.6, 10, 0.6), 80)
  expect_equal(old[1:3], c(0.21, 0.11, 0.6) * macForAge(1, 80))
  expect_lt(old[1], 0.21)
  expect_equal(old[4:5], c(10, 0.6))
  # A tenth of the age-adjusted MAC, at any age.
  props <- getGasProperties()
  dd <- getDrugDefaultsGlobal()
  for (g in c("sevoflurane", "isoflurane", "desflurane")) for (age in c(20, 40, 75)) {
    expect_equal(gasThresholdForAge(g, dd$endCe[dd$Drug == g], age),
                 GAS_AGENT_THRESHOLD_FRACTION * macForAge(props$MAC40[props$gas == g], age))
  }
  expect_equal(dd$endCe[dd$Drug == "nitrousOxide"], 10)
  expect_equal(GAS_MAC_THRESHOLD, 0.1)
  # And back again.
  expect_equal(gasThresholdForAge("sevoflurane", gasThresholdForAge("sevoflurane", 0.21, 70), 70,
                                  inverse = TRUE), 0.21)
})


test_that("the Drug Thresholds table shows every editable threshold and round-trips", {
  dd <- getDrugDefaultsGlobal()
  shown <- thresholdTableForDisplay(dd, age = 70, macThreshold = 0.1)
  expect_true(all(c("sevoflurane", "isoflurane", "desflurane", "nitrousOxide", "MAC", "propofol")
                  %in% shown$Drug))
  expect_false(any(c("air", "oxygen", "ventilation") %in% shown$Drug))
  expect_equal(shown$Threshold[shown$Drug == "MAC"], 0.1)
  expect_equal(shown$Threshold[shown$Drug == "nitrousOxide"], 10)
  # Shown at the patient's age: what is on screen is what the plot uses.
  expect_equal(shown$Threshold[shown$Drug == "sevoflurane"],
               signif(0.21 * macForAge(1, 70), 3))
  expect_equal(shown$Threshold[shown$Drug == "propofol"], dd$endCe[dd$Drug == "propofol"])

  # Unedited, it goes back to (very nearly) where it came from...
  back <- thresholdTableToDefaults(shown, dd, age = 70, macThreshold = 0.1)
  expect_equal(back$drugDefaults$endCe, dd$endCe, tolerance = 2e-3)
  expect_equal(back$macThreshold, 0.1)

  # ...and edits land where they should.
  shown$Threshold[shown$Drug == "MAC"] <- 0.33
  shown$Threshold[shown$Drug == "sevoflurane"] <- 0.5
  shown$Threshold[shown$Drug == "nitrousOxide"] <- 20
  shown$Threshold[shown$Drug == "propofol"] <- NA
  back <- thresholdTableToDefaults(shown, dd, age = 70, macThreshold = 0.1)
  expect_equal(back$macThreshold, 0.33)
  e <- stats::setNames(back$drugDefaults$endCe, back$drugDefaults$Drug)
  expect_equal(gasThresholdForAge("sevoflurane", e[["sevoflurane"]], 70), 0.5)
  expect_equal(e[["nitrousOxide"]], 20)
  expect_equal(e[["propofol"]], 0)
  # Rows the dialog does not show keep what they had.
  expect_equal(e[["oxygen"]], dd$endCe[dd$Drug == "oxygen"])
})


test_that("no threshold, no time; and a lower threshold takes longer", {
  gasDose <- gasRows(list(0, "oxygen", 4), list(0, "ventilation", 4), list(0, "sevoflurane", 2))
  sim <- simulateGases(gasDose, weight = 70, age = 40, maximum = 60)
  w <- gasWashout(sim, gasDose, weight = 70)
  n <- length(w$Time)
  expect_equal(gasRecoveryTime(w, "sevoflurane", 0), rep(0, n))
  expect_equal(gasRecoveryTime(w, "sevoflurane", NA), rep(0, n))
  expect_gt(gasRecoveryTime(w, "sevoflurane", 0.21)[n], gasRecoveryTime(w, "sevoflurane", 0.7)[n])
  expect_gt(macRecoveryTime(w, 40, 0.1)[n], macRecoveryTime(w, 40, 0.33)[n])
  expect_null(gasWashout(NULL, gasDose))
})


test_that("the entries carry the time until threshold only when asked", {
  gasDose <- gasRows(list(0, "oxygen", 2.3), list(0, "nitrousOxide", 5.7),
                     list(0, "ventilation", 4), list(0, "sevoflurane", 2))
  sim <- simulateGases(gasDose, weight = 70, age = 70, maximum = 60)

  plain <- gasDrugEntries(sim, gasDose, maximum = 60)
  expect_true(all(plain$sevoflurane$equiSpace$Recovery == 0))
  expect_equal(plain$sevoflurane$endCe, 0)
  expect_equal(plain$MAC$max$Recovery, 0)

  w <- gasWashout(sim, gasDose, weight = 70)
  out <- gasDrugEntries(sim, gasDose, maximum = 60, washout = w, age = 70)
  expect_gt(out$sevoflurane$max$Recovery, 5)
  # The threshold drawn is the age-adjusted one, a tenth of this patient's MAC.
  expect_equal(out$sevoflurane$endCe, 0.21 * macForAge(1, 70))
  expect_equal(length(out$sevoflurane$equiSpace$Recovery), RESOLUTION)
  expect_gt(out$nitrousOxide$max$Recovery, 0)
  expect_equal(out$nitrousOxide$endCe, 10)
  expect_gt(out$MAC$max$Recovery, 2)
  expect_equal(out$MAC$endCe, GAS_MAC_THRESHOLD)
  # A different MAC threshold is honoured.
  loose <- gasDrugEntries(sim, gasDose, maximum = 60, washout = w, age = 70, macThreshold = 0.33)
  expect_equal(loose$MAC$endCe, 0.33)
  expect_lt(loose$MAC$max$Recovery, out$MAC$max$Recovery)
  # Oxygen has no threshold and so no curve.
  expect_true(all(out$oxygen$equiSpace$Recovery == 0))
  # The concentrations themselves are untouched by asking.
  expect_equal(out$sevoflurane$results, plain$sevoflurane$results)
})


test_that("an opioid lengthens the time until the opioid-adjusted MAC reaches threshold", {
  gasDose <- gasRows(list(0, "oxygen", 4), list(0, "ventilation", 4), list(0, "sevoflurane", 2))
  sim <- simulateGases(gasDose, weight = 70, age = 40, maximum = 60)
  w <- gasWashout(sim, gasDose, weight = 70)
  entries <- gasDrugEntries(sim, gasDose, maximum = 60, washout = w, age = 40)
  Time <- entries$MAC$equiSpace$Time
  opioid <- list(fentanyl = list(equiSpace = data.frame(Time = Time, Ce = 1.2, MEAC = 200)))
  out <- applyOpioidMacInteraction(entries, opioid)

  # More MAC for the same gas means further to fall.
  late <- Time > 30
  expect_true(all(out$MAC$equiSpace$Recovery[late] > entries$MAC$equiSpace$Recovery[late]))
  # It is the time for the UNADJUSTED MAC to reach threshold * (1 - R).
  R <- opioidMacReduction(2)
  expect_equal(out$MAC$max$Recovery,
               max(macRecoveryTime(w, 40, GAS_MAC_THRESHOLD * (1 - R))))
  expect_equal(out$MAC$endCe, GAS_MAC_THRESHOLD)
})
