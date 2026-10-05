# "Time until threshold" for the inhaled agents and MAC, checked against the
# definition (Shafer, 2026-10-05): turn the agent off, turn the fresh gas flow
# up so that there is no rebreathing, leave the ventilation alone, and see when
# the concentration comes down through the threshold.  Each agent is its own
# decision; turning off the vaporiser and turning off the nitrous oxide are
# separate.
#
# The reference here is the full, coupled engine with the same thing done to
# the dose table, the fresh gas flow being raised to 100,000 L/min to stand for
# "no rebreathing".  With the ideal circuit, the engine's default, any flow at
# or above the ventilation would do.  The absurd figure dates from when the
# engine had only the semi-closed circuit, a well-mixed 8 L volume from which
# some agent is always rebreathed: there the time for MAC in the nitrous oxide
# case below was 22.6 min with a 100 L/min flush, 20.3 at 1000, 20.11 at 10,000
# and 20.08 at 100,000, which is the limit and is what macRecoveryTimeExact()
# gives.  It is kept because it is right for either circuit.
#
# The individual agents use a shortcut that leaves the uptake coupling out of
# the washout; these tests measure what that costs.  MAC is done exactly.
#
# (Claude Code, Claude Fable 5.1, 2026-10-05; run on R 4.6.1.)

gasRows <- function(...) do.call(rbind, lapply(list(...), function(r)
  data.frame(Time = r[[1]], Drug = r[[2]], Dose = r[[3]], stringsAsFactors = FALSE)))

# The dose table with `off` turned off at t.  With flush = TRUE the remaining
# fresh gas flows are scaled up to 100,000 L/min in total, which keeps the
# mixture and removes the rebreathing; if nothing else is flowing, oxygen is.
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
  if (flush) now <- now / sum(now) * 1e5
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
                             horizon = 900)) / at(stale, tStale, t), 0.02)
    # Leaving half a litre a minute running is far slower than flushing.
    expect_gt(at(stale, tStale, t), 2 * at(clean, tClean, t))
  }
  # No jump in the no-rebreathing time when the flow is turned down at 15 min...
  around <- at(clean, tClean, c(14.5, 15.5))
  expect_lt(abs(diff(around)), 1)
  # ...where the rebreathing time jumps at once.
  expect_gt(diff(at(stale, tStale, c(14.5, 15.5))), 5)
})


test_that("each agent is its own decision, and each line is exact for it", {
  gasDose <- gasRows(list(0, "oxygen", 2.3), list(0, "nitrousOxide", 5.7),
                     list(0, "ventilation", 4), list(0, "sevoflurane", 2))
  sim <- simulateGases(gasDose, weight = 70, age = 40, maximum = 120)
  w <- gasWashout(sim, gasDose, weight = 70)
  sevo <- gasRecoveryTime(w, "sevoflurane", 0.21)
  n2o  <- gasRecoveryTime(w, "nitrousOxide", 10)
  mac  <- macRecoveryTime(w, 40, 0.1)

  for (t in c(30, 119)) {
    # Sevoflurane off, nitrous oxide still on.
    expect_lt(abs(at(w, sevo, t) -
                    bruteGas(gasDose, 70, 40, t, "sevoflurane", 0.21, off = "sevoflurane")), 0.1)
    # Nitrous oxide off, sevoflurane still on.
    expect_lt(abs(at(w, n2o, t) -
                    bruteGas(gasDose, 70, 40, t, "nitrousOxide", 10, off = "nitrousOxide")), 0.1)
    # Everything off, for MAC.
    expect_lt(abs(at(w, mac, t) -
                    bruteGas(gasDose, 70, 40, t, "MAC", 0.1, off = "MAC")), 0.1)
  }
  # Nitrous oxide comes off faster than sevoflurane, even with further to go
  # in proportion (68% down to 10%).
  expect_lt(at(w, n2o, 119), at(w, sevo, 119))

  # The sevoflurane line answers "if I turn the vaporiser off".  Turning the
  # nitrous oxide off at the same moment is a different question with a shorter
  # answer, because nitrous oxide on its way out takes sevoflurane with it.
  together <- bruteGas(gasDose, 70, 40, 119, "sevoflurane", 0.21,
                       off = c("sevoflurane", "nitrousOxide"))
  expect_lt(together, at(w, sevo, 119) - 1)
})


test_that("the sum-of-exponentials shortcut reads long when nitrous oxide is washing out", {
  # What the coupled simulation is for.  With 70% nitrous oxide after two hours
  # the shortcut, which leaves the coupling out, was 18% long for the nitrous
  # oxide line and 10% for MAC when this was written.
  gasDose <- gasRows(list(0, "oxygen", 2.3), list(0, "nitrousOxide", 5.7),
                     list(0, "ventilation", 4), list(0, "sevoflurane", 2))
  sim <- simulateGases(gasDose, weight = 70, age = 40, maximum = 120)
  w <- gasWashout(sim, gasDose, weight = 70)

  n2oShort <- gasRecoveryTime(w, "nitrousOxide", 10, exact = FALSE)
  want <- bruteGas(gasDose, 70, 40, 119, "nitrousOxide", 10, off = "nitrousOxide")
  expect_gt((at(w, n2oShort, 119) - want) / want, 0.10)

  macShort <- macRecoveryTime(w, 40, 0.1, exact = FALSE)
  want <- bruteGas(gasDose, 70, 40, 119, "MAC", 0.1, off = "MAC")
  expect_gt((at(w, macShort, 119) - want) / want, 0.05)

  # For a volatile agent turned off alone the shortcut is as good as exact.
  sevoShort <- gasRecoveryTime(w, "sevoflurane", 0.21, exact = FALSE)
  want <- bruteGas(gasDose, 70, 40, 119, "sevoflurane", 0.21, off = "sevoflurane")
  expect_lt(abs(at(w, sevoShort, 119) - want) / want, 0.02)
})


test_that("a tissue still filling when the agent is turned off is followed over the top", {
  # Two minutes into a brisk wash-in the brain is below the threshold but the
  # alveoli are far above it: turn the vaporiser off now and the brain goes on
  # rising for a while before it falls.  The time until threshold is the time
  # until it comes back down, not zero.
  gasDose <- gasRows(list(0, "oxygen", 8), list(0, "ventilation", 6), list(0, "sevoflurane", 8))
  sim <- simulateGases(gasDose, weight = 70, age = 40, maximum = 10)
  w <- gasWashout(sim, gasDose, weight = 70)
  brain <- sim$state$sevoflurane[, 3]
  thr <- 1.5
  # Close enough below the threshold that the overshoot clears it.  (Further
  # below, the brain peaks short of the threshold and the answer drops to zero
  # at a stroke; the series is computed at about the plot's resolution and
  # joins the two sides of that step with a straight line, so points on the
  # step itself are not compared.)
  early <- which(brain < 0.97 * thr & brain > 0.9 * thr)
  expect_gt(length(early), 0)
  rec <- gasRecoveryTime(w, "sevoflurane", thr)
  for (t in w$Time[range(early)]) {
    want <- bruteGas(gasDose, 70, 40, t, "sevoflurane", thr)
    expect_gt(want, 0.5)                                 # the brute force agrees it is not zero
    expect_lt(abs(at(w, rec, t) - want), 0.15)
  }
  # And well below the threshold, where it never gets there: zero.
  expect_equal(rec[which(brain > 0 & brain < 0.5 * thr)[1]], 0)
})


test_that("exact MAC: air in the fresh gas, low flow, a change of settings, a heavy patient", {
  # Desflurane and nitrous oxide in oxygen and air at 100 kg, turned down to a
  # low flow at 20 minutes.  Nitrogen goes on being inspired after the agents
  # are off, and the ventilation and cardiac output are not the 70 kg ones.
  gasDose <- gasRows(list(0, "oxygen", 1), list(0, "air", 1), list(0, "nitrousOxide", 2),
                     list(0, "ventilation", 5.2), list(0, "desflurane", 6),
                     list(20, "oxygen", 0.3), list(20, "nitrousOxide", 0.5), list(20, "air", 0.2))
  sim <- simulateGases(gasDose, weight = 100, age = 65, maximum = 180)
  w <- gasWashout(sim, gasDose, weight = 100)
  mac <- macRecoveryTime(w, 65, 0.1)
  for (t in c(10, 21, 179)) {
    expect_lt(abs(at(w, mac, t) -
                    bruteGas(gasDose, 100, 65, t, "MAC", 0.1, off = "MAC", horizon = 400)), 0.15)
  }
})


test_that("exact MAC: limits, per-point thresholds, and agreement with the shortcut when uncoupled", {
  gasDose <- gasRows(list(0, "oxygen", 2), list(0, "ventilation", 4), list(0, "sevoflurane", 2))
  sim <- simulateGases(gasDose, weight = 70, age = 40, maximum = 60)
  w <- gasWashout(sim, gasDose, weight = 70)
  n <- length(w$Time)

  # Already below the threshold, or no threshold: no time.
  expect_equal(macRecoveryTimeExact(w, 40, 5), rep(0, n))
  expect_equal(macRecoveryTimeExact(w, 40, 0), rep(0, n))
  expect_equal(macRecoveryTimeExact(w, 40, NA), rep(0, n))

  # With a volatile agent alone the only coupling is nitrogen's, which is
  # small, so exact and shortcut nearly coincide once it has washed out.
  exact <- macRecoveryTimeExact(w, 40, 0.1)
  short <- macRecoveryTime(w, 40, 0.1, exact = FALSE)
  late <- w$Time > 20
  expect_lt(max(abs(exact - short)[late]), 0.15)

  # One threshold per time point, as the opioid interaction supplies.
  thr <- seq(0.05, 0.3, length.out = n)
  per <- macRecoveryTimeExact(w, 40, thr)
  expect_equal(per[n], macRecoveryTimeExact(w, 40, 0.3)[n])
  expect_equal(per[1], 0)
  expect_gt(macRecoveryTimeExact(w, 40, 0.05)[n], macRecoveryTimeExact(w, 40, 0.3)[n])

  # With rebreathing asked for there is no exact method; the shortcut is used.
  stale <- gasWashout(sim, gasDose, weight = 70, rebreathing = TRUE)
  expect_equal(macRecoveryTime(stale, 40, 0.1), macRecoveryTime(stale, 40, 0.1, exact = FALSE))
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
