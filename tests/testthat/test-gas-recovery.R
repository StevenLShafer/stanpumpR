# "Time until threshold" for the inhaled agents and MAC, checked against the
# definition: turn the vaporisers and the nitrous oxide off, keep the total
# fresh gas flow (as oxygen) and the ventilation, simulate on in the full
# coupled engine, and see when the concentration comes down through the
# threshold.
#
# The fast method in R/gasRecovery.R leaves the uptake coupling out of the
# washout.  These tests measure what that costs: nothing detectable for a
# volatile agent alone, and under about a tenth of the time when 70% nitrous
# oxide is washing out with it.
#
# (Claude Code, Claude Fable 5.1, 2026-10-05; run on R 4.6.1.)

gasRows <- function(...) do.call(rbind, lapply(list(...), function(r)
  data.frame(Time = r[[1]], Drug = r[[2]], Dose = r[[3]], stringsAsFactors = FALSE)))

# The dose table with delivery stopped at t.
deliveryStopped <- function(gasDose, t) {
  d <- gasDose[gasDose$Time <= t, ]
  d <- d[order(d$Time), ]
  s <- gasSettingsAt(split(d, d$Drug), t)
  off <- intersect(unique(d$Drug),
                   c("sevoflurane", "isoflurane", "desflurane", "nitrousOxide", "air"))
  rbind(d, data.frame(Time = t, Drug = c(off, "oxygen"),
                      Dose = c(rep(0, length(off)), s$Q), stringsAsFactors = FALSE))
}

lastCrossing <- function(time, y, threshold, t) {
  keep <- time >= t; time <- time[keep]; y <- y[keep]
  above <- which(y > threshold)
  if (length(above) == 0) return(0)
  i <- max(above)
  time[i] + (threshold - y[i]) * (time[i + 1] - time[i]) / (y[i + 1] - y[i]) - t
}

bruteGas <- function(gasDose, weight, age, t, gas, threshold, horizon = 240) {
  b <- advanceClosedFormGas(deliveryStopped(gasDose, t), weight = weight, age = age,
                            maximum = t + horizon,
                            resolution = round((t + horizon) * 10) + 1)
  if (gas == "MAC") {
    m <- b$results[b$results$Drug == "MAC", ]
    lastCrossing(m$Time, m$Y, threshold, t)
  } else {
    lastCrossing(b$timeLine, b$state[[gas]][, 3], threshold, t)   # vessel-rich group
  }
}


test_that("a volatile agent alone: the fast method matches stopping delivery in the engine", {
  gasDose <- gasRows(list(0, "oxygen", 4), list(0, "ventilation", 4), list(0, "sevoflurane", 2))
  sim <- simulateGases(gasDose, weight = 70, age = 40, maximum = 120)
  w <- gasWashout(sim, gasDose, weight = 70)
  agent <- gasRecoveryTime(w, "sevoflurane", 0.7)
  mac   <- macRecoveryTime(w, 40, GAS_MAC_THRESHOLD)

  for (t in c(15, 60, 119)) {
    expect_lt(abs(stats::approx(w$Time, agent, t)$y -
                    bruteGas(gasDose, 70, 40, t, "sevoflurane", 0.7)), 0.15)
    expect_lt(abs(stats::approx(w$Time, mac, t)$y -
                    bruteGas(gasDose, 70, 40, t, "MAC", GAS_MAC_THRESHOLD)), 0.15)
  }

  # Longer anaesthetic, longer wait: the tissues have had time to fill.
  expect_true(all(diff(stats::approx(w$Time, agent, c(15, 30, 60, 119))$y) > 0))
  # Nothing to wait for before the brain has reached the threshold.
  expect_equal(agent[w$Time < 1], rep(0, sum(w$Time < 1)))
})


test_that("low flow and a change of settings: still matches", {
  # Wash-in at 4 L/min, then 1 L/min from 15 minutes.  The washout must use the
  # settings in force at each moment, so the time jumps when the flow drops.
  gasDose <- gasRows(list(0, "oxygen", 4), list(0, "ventilation", 4),
                     list(0, "isoflurane", 1.5), list(15, "oxygen", 1))
  sim <- simulateGases(gasDose, weight = 70, age = 60, maximum = 180)
  w <- gasWashout(sim, gasDose, weight = 70)
  agent <- gasRecoveryTime(w, "isoflurane", 0.4)
  for (t in c(10, 90, 179)) {
    expect_lt(abs(stats::approx(w$Time, agent, t)$y -
                    bruteGas(gasDose, 70, 60, t, "isoflurane", 0.4, horizon = 480)), 0.2)
  }
  # Less fresh gas, slower washout.
  before <- agent[max(which(w$Time < 15))]; after <- agent[min(which(w$Time > 15))]
  expect_gt(after, before)
})


test_that("with nitrous oxide the neglected coupling costs under a tenth, and errs long", {
  gasDose <- gasRows(list(0, "oxygen", 2.3), list(0, "nitrousOxide", 5.7),
                     list(0, "ventilation", 4), list(0, "sevoflurane", 2))
  sim <- simulateGases(gasDose, weight = 70, age = 40, maximum = 120)
  w <- gasWashout(sim, gasDose, weight = 70)
  agent <- gasRecoveryTime(w, "sevoflurane", 0.7)
  mac   <- macRecoveryTime(w, 40, GAS_MAC_THRESHOLD)
  for (t in c(30, 119)) {
    fast <- stats::approx(w$Time, agent, t)$y
    want <- bruteGas(gasDose, 70, 40, t, "sevoflurane", 0.7)
    expect_gte(fast, want - 0.05)
    expect_lt((fast - want) / want, 0.06)

    fast <- stats::approx(w$Time, mac, t)$y
    want <- bruteGas(gasDose, 70, 40, t, "MAC", GAS_MAC_THRESHOLD)
    expect_gte(fast, want - 0.05)
    expect_lt((fast - want) / want, 0.11)
  }
})


test_that("no threshold, no time; and a lower threshold takes longer", {
  gasDose <- gasRows(list(0, "oxygen", 4), list(0, "ventilation", 4), list(0, "sevoflurane", 2))
  sim <- simulateGases(gasDose, weight = 70, age = 40, maximum = 60)
  w <- gasWashout(sim, gasDose, weight = 70)
  n <- length(w$Time)
  expect_equal(gasRecoveryTime(w, "sevoflurane", 0), rep(0, n))
  expect_equal(gasRecoveryTime(w, "sevoflurane", NA), rep(0, n))
  expect_gt(gasRecoveryTime(w, "sevoflurane", 0.35)[n], gasRecoveryTime(w, "sevoflurane", 0.7)[n])
  expect_gt(macRecoveryTime(w, 40, 0.2)[n], macRecoveryTime(w, 40, 0.33)[n])
  expect_null(gasWashout(NULL, gasDose))
})


test_that("the entries carry the time until threshold only when asked", {
  gasDose <- gasRows(list(0, "oxygen", 4), list(0, "ventilation", 4), list(0, "sevoflurane", 2))
  sim <- simulateGases(gasDose, weight = 70, age = 40, maximum = 60)

  plain <- gasDrugEntries(sim, gasDose, maximum = 60)
  expect_true(all(plain$sevoflurane$equiSpace$Recovery == 0))
  expect_equal(plain$sevoflurane$endCe, 0)
  expect_equal(plain$MAC$max$Recovery, 0)

  w <- gasWashout(sim, gasDose, weight = 70)
  out <- gasDrugEntries(sim, gasDose, maximum = 60, washout = w, age = 40)
  expect_gt(out$sevoflurane$max$Recovery, 5)
  expect_equal(out$sevoflurane$endCe,
               getDrugDefaultsGlobal()$endCe[getDrugDefaultsGlobal()$Drug == "sevoflurane"])
  expect_equal(length(out$sevoflurane$equiSpace$Recovery), RESOLUTION)
  expect_gt(out$MAC$max$Recovery, 2)
  expect_equal(out$MAC$endCe, GAS_MAC_THRESHOLD)
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
