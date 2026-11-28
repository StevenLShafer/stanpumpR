# CodeRabbit's review of PR #730 (2026-11-15): the inhaled gases.
#
#   1. gasCircuitBlend(): once the ideal circuit is closed (the fresh gas less
#      than the uptake plus the carbon dioxide absorbed), the gas the alveoli
#      push out on emergence (a negative summed uptake u) was all vented,
#      though nothing leaves a closed circuit.  A nitrous oxide washout into a
#      closed circuit lost every gas at -u times its alveolar tension.  Fixed:
#      the pushed-out gas is rebreathed as far as the shortfall needs, and
#      only the circuit's surplus is vented.
#   3. restoreThresholds(): a volatile agent's threshold is stored at age 40;
#      the Drug Thresholds dialog accepted MAX_DOSE_VALUE shown at age 70,
#      stored it above MAX_DOSE_VALUE, and the link that saved it then
#      dropped it.  Fixed: held to the most the dialog can store.
#   4. gasWashout(): every dose row was a change point, and each interval is
#      integrated on its own for the time until threshold, so rows repeating
#      the setting in force multiplied the work (500 such rows: 33 s, against
#      half a second for one).  Fixed: a row that changes no setting is not a
#      change point.
#
# (Item 2 was the help page's equations, inst/app-help/models/gas-engine.md.)
# Each test fails on f197d63.
#
# Drafted by Claude Code, 2026-11-15, at the request of Steven L. Shafer.

cr730Rows <- function(...) do.call(rbind, lapply(list(...), function(r)
  data.frame(Time = r[[1]], Drug = r[[2]], Dose = r[[3]], stringsAsFactors = FALSE)))

# Each compartment's capacity for a gas, litres of gas per unit tension (%/100
# of it): the weights that make the amount the patient holds.  The ideal
# circuit holds none.
cr730Capacity <- function(g, body = getGasBody(70)) {
  props <- getGasProperties()
  if (g == "oxygen") return(c(0, body$V_alveolar))
  ltg <- gasPartitionTissueGas(props[props$gas == g, ])
  c(0, body$V_alveolar, body$V_brain * ltg[["brain"]],
    body$V_muscle * ltg[["muscle"]], body$V_fat * ltg[["fat"]])
}

cr730Amount <- function(sim, g) {
  as.vector(sim$state[[g]] %*% cr730Capacity(g, sim$body)) / 100
}


test_that("1: a closed circuit rebreathes the gas pushed out on emergence, and loses none", {
  # CodeRabbit's case: no fresh gas, a summed uptake of -0.1 L/min, carbon
  # dioxide 0.196 L/min at 70 kg.  Nothing is vented, so each gas's amount
  # changes only by what the fresh gas brings, which is nothing: the capacity-
  # weighted sum of every column of A is zero.  Before, each gas lost
  # 0.1 L/min times its alveolar tension, and oxygen as much again over its
  # consumption.
  body <- getGasBody(70)
  props <- getGasProperties()
  MV <- 4.6
  VA <- MV * (1 - GAS_DEAD_SPACE_FRACTION)
  VCO2 <- GAS_RESPIRATORY_QUOTIENT * gasOxygenUptake(body)
  cE <- VCO2 / MV
  u <- -0.1
  for (g in props$gas[props$soluble]) {
    sys <- gasSystemSoluble(props[props$gas == g, ], body, Q = 0, VA = VA, Qco = body$Q_cardiac,
                            Ffgf = 50, totUptake = u, MV = MV, cE = cE)
    cap <- cr730Capacity(g, body)
    expect_lt(max(abs(cap %*% sys$A)), 1e-12)
    expect_equal(sum(cap * sys$b), 0)
  }
  ox <- gasSystemOxygen(body, Q = 0, VA = VA, Ffgf = 100, MV = MV, totUptake = u, cE = cE)
  expect_lt(abs(ox$A[2, 2]), 1e-12)
  expect_equal(body$V_alveolar * ox$b[2], -100 * gasOxygenUptake(body))   # the oxygen consumed, no more

  # A little fresh gas, the pushed-out gas more than the shortfall: the
  # alveoli receive all of VA again, and what is vented is the circuit's
  # surplus, Q - VCO2 - u, not all of -u.
  Q <- 0.15
  bl <- gasCircuitBlend(Q, VA, MV, u = u, cE = cE)
  expect_equal(bl$supply, 1)
  for (g in props$gas[props$soluble]) {
    A <- gasSystemSoluble(props[props$gas == g, ], body, Q, VA, body$Q_cardiac, Ffgf = 0,
                          totUptake = u, MV = MV, cE = cE)$A
    expect_equal(-as.vector(cr730Capacity(g, body) %*% A)[2], Q - VCO2 - u, tolerance = 1e-12)
  }
  # Continuous where the circuit closes: at Q = VCO2 the blend is the open one.
  above <- gasCircuitBlend(VCO2 * (1 + 1e-12), VA, MV, u = u, cE = cE)
  below <- gasCircuitBlend(VCO2 * (1 - 1e-12), VA, MV, u = u, cE = cE)
  expect_equal(below, above, tolerance = 1e-9)
})


test_that("1: below the closed circuit the blend is unchanged, to the bit", {
  MV <- 4.6
  VA <- MV * (1 - GAS_DEAD_SPACE_FRACTION)
  cE <- GAS_RESPIRATORY_QUOTIENT * getGasBody(70)$VO2 / MV
  for (u in c(-0.5, -0.1, 0, 0.05, 0.9)) {
    for (Q in c(0.3, 1, 3, 4.5)) {
      inspired <- MV + max(u, 0)
      if (Q >= inspired) next
      k <- (inspired - Q) / (MV * (1 - cE))
      if (k > 1) next
      D <- inspired - k * (MV - VA)
      expect_identical(gasCircuitBlend(Q, VA, MV, u = u, cE = cE),
                       list(fresh = Q / D, alveolar = k * VA / D, supply = 1))
    }
  }
  # And so is the closed circuit on induction (u > 0), where nothing is pushed out.
  for (Q in c(0, 0.1)) {
    u <- 0.05
    D <- Q + MV * (1 - cE) - (MV - VA)
    expect_identical(gasCircuitBlend(Q, VA, MV, u = u, cE = cE),
                     list(fresh = Q / D, alveolar = 1 * VA / D, supply = D / (VA + u)))
  }
})


test_that("1: a nitrous oxide washout into a closed circuit conserves every gas", {
  # Nitrous oxide 2 L/min in 6 for 20 minutes, the lungs flushed with oxygen
  # for a minute, then the fresh gas closed.  The tissues give back nitrous
  # oxide while the patient consumes oxygen; the summed uptake starts at
  # -0.17 L/min, less than the carbon dioxide absorbed, so the circuit is
  # short and nothing is vented.  Until the oxygen runs out, nitrous oxide
  # and nitrogen are conserved, and oxygen falls by exactly what is consumed.
  # Before, 8.9 mL of nitrous oxide and 0.24 mL of nitrogen went nowhere.
  DT <- cr730Rows(list(0, "nitrousOxide", 2), list(0, "oxygen", 4), list(0, "ventilation", 4.6),
                  list(20, "nitrousOxide", 0), list(20, "oxygen", 6), list(21, "oxygen", 0))
  sim <- simulateGases(DT, weight = 70, age = 40, maximum = 35)
  body <- sim$body
  VCO2 <- GAS_RESPIRATORY_QUOTIENT * gasOxygenUptake(body)
  closed <- which(sim$timeLine >= 21 - 1e-9 & sim$state$oxygen[, 2] > 0)
  expect_gt(length(closed), 50)
  first <- lapply(sim$state, function(m) m[closed[1], ])
  u <- gasTotalUptake(first, getGasProperties(), body, body$Q_cardiac) + gasOxygenUptake(body) - VCO2
  expect_true(u < 0 && u > -VCO2)                 # a washout, and the circuit short
  for (g in c("nitrousOxide", "nitrogen")) {
    held <- cr730Amount(sim, g)[closed]
    expect_equal(held, rep(held[1], length(held)), tolerance = 1e-10, info = g)
  }
  o2 <- cr730Amount(sim, "oxygen")[closed]
  t <- sim$timeLine[closed]
  expect_equal(o2, o2[1] - gasOxygenUptake(body) * (t - t[1]), tolerance = 1e-9)
})


test_that("1: what a closed circuit vents on emergence is only its surplus", {
  # Nitrous oxide for an hour, a minute's flush, then no fresh gas: at first
  # the tissues give back more than the oxygen consumed and the carbon
  # dioxide absorbed take, so the circuit has a surplus and vents it, and
  # then it is short and vents nothing.  The nitrous oxide lost is the
  # surplus at its alveolar tension, max(0, Q - VCO2 - u) F_alv: 31 mL
  # (18 mL since 2026-11-28, the oxygen consumed being 1.21 times the volume
  # in the lungs: gasStpdFactor(); 15 mL with the shunt under anesthesia).
  # Before, all of -u was vented, and 115 mL was lost.
  DT <- cr730Rows(list(0, "nitrousOxide", 4), list(0, "oxygen", 2), list(0, "ventilation", 4.6),
                  list(60, "nitrousOxide", 0), list(60, "oxygen", 6), list(61, "oxygen", 0))
  sim <- simulateGases(DT, weight = 70, age = 40, maximum = 66)
  props <- getGasProperties()
  body <- sim$body
  VCO2 <- GAS_RESPIRATORY_QUOTIENT * gasOxygenUptake(body)
  tl <- sim$timeLine
  closed <- which(tl >= 61 - 1e-9 & sim$state$oxygen[, 2] > 0)
  vented <- 0
  for (j in seq_len(length(closed) - 1)) {
    i <- closed[j]
    st <- lapply(sim$state, function(m) m[i, ])
    u <- gasTotalUptake(st, props, body, body$Q_cardiac) + gasOxygenUptake(body) - VCO2
    alv <- (sim$state$nitrousOxide[i, 2] + sim$state$nitrousOxide[i + 1, 2]) / 2
    vented <- vented + max(0, -VCO2 - u) * alv / 100 * (tl[i + 1] - tl[i])
  }
  n2o <- cr730Amount(sim, "nitrousOxide")
  lost <- n2o[closed[1]] - n2o[closed[length(closed)]]
  # (15 mL with the 10% shunt under anesthesia, 2026-11-28)
  expect_gt(vented, 0.012)
  expect_equal(lost, vented, tolerance = 5e-3)
})


test_that("3: a volatile agent's threshold the dialog accepts survives the link", {
  dd <- getDrugDefaultsGlobal()
  # At 70 the dialog shows sevoflurane's threshold at that age and accepts
  # the grid's ceiling; it stores it at age 40, above the ceiling.
  shown <- thresholdTableForDisplay(dd, age = 70)
  shown$Threshold[shown$Drug == "sevoflurane"] <- MAX_DOSE_VALUE
  back <- thresholdTableToDefaults(shown, dd, age = 70)
  expect_identical(back$invalid, character(0))
  stored <- back$drugDefaults$endCe[back$drugDefaults$Drug == "sevoflurane"]
  expect_gt(stored, MAX_DOSE_VALUE)
  # The link carries it, and reopening the link keeps it.
  saved <- changedThresholds(back$drugDefaults)
  restored <- restoreThresholds(dd, saved)$drugDefaults
  expect_equal(restored$endCe[restored$Drug == "sevoflurane"], stored)
  # The most the dialog can store, at the oldest age, is the ceiling; past it
  # a link is still refused, and every other drug keeps MAX_DOSE_VALUE.
  most <- MAX_DOSE_VALUE / macForAge(1, MAX_AGE)
  ok <- restoreThresholds(dd, c(isoflurane = most))$drugDefaults
  expect_equal(ok$endCe[ok$Drug == "isoflurane"], most)
  over <- restoreThresholds(dd, c(isoflurane = most * 1.01, nitrousOxide = MAX_DOSE_VALUE * 1.01,
                                  propofol = MAX_DOSE_VALUE * 1.01))$drugDefaults
  for (d in c("isoflurane", "nitrousOxide", "propofol"))
    expect_equal(over$endCe[over$Drug == d], dd$endCe[dd$Drug == d], info = d)
})


test_that("4: rows that repeat the setting in force are not change points of the washout", {
  base <- cr730Rows(list(0, "sevoflurane", 2), list(0, "oxygen", 6), list(0, "ventilation", 4.6))
  reps <- data.frame(Time = seq(1, 119, length.out = 200), Drug = "sevoflurane", Dose = 2,
                     stringsAsFactors = FALSE)
  DT <- rbind(base, reps)
  sim <- simulateGases(DT, weight = 70, age = 40, maximum = 120)
  w <- gasWashout(sim, DT, weight = 70)
  expect_identical(w$changes, 0)
  expect_length(unique(w$interval), 1)
  # The same answer as the washout of the table without them, from the same
  # simulation; the settings read are the table's own.
  w1 <- gasWashout(sim, base, weight = 70)
  thr <- gasThresholdForAge("sevoflurane", getDrugDefaultsGlobal()$endCe[getDrugDefaultsGlobal()$Drug == "sevoflurane"], 40)
  expect_identical(gasRecoveryTime(w, "sevoflurane", thr), gasRecoveryTime(w1, "sevoflurane", thr))
  expect_identical(macRecoveryTime(w, 40), macRecoveryTime(w1, 40))
  expect_identical(nrow(w$bySetting$sevoflurane), 201L)

  # A row that changes a setting is a change point; one overridden at its own
  # time by the setting already in force is not; nor is a repeated flow.
  DT2 <- rbind(base, cr730Rows(list(30, "sevoflurane", 3), list(45, "oxygen", 6),
                               list(60, "sevoflurane", 4), list(60, "sevoflurane", 3),
                               list(75, "ventilation", 6), list(90, "oxygen", 2)))
  sim2 <- simulateGases(DT2, weight = 70, age = 40, maximum = 120)
  expect_identical(gasWashout(sim2, DT2, weight = 70)$changes, c(0, 30, 75, 90))
  # A table in which every row changes something keeps every time.
  DT3 <- rbind(base, cr730Rows(list(30, "sevoflurane", 3), list(75, "ventilation", 6)))
  sim3 <- simulateGases(DT3, weight = 70, age = 40, maximum = 120)
  expect_identical(gasWashout(sim3, DT3, weight = 70)$changes, c(0, 30, 75))
})
