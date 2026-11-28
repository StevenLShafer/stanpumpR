# Carbon's third UI audit (2026-11-15), finding H2, parts 1 and 3: the inhaled
# gases.
#
#   1. advanceClosedFormGas(): on the ideal circuit with no fresh gas (or less
#      than the patient takes up), the circuit blend rebreathed more gas than
#      was exhaled.  Nitrogen rose past an atmosphere, the uptake coupling fed
#      on it, and on a plot of three days or more the states overflowed and the
#      gas panels vanished ("missing value where TRUE/FALSE needed").  Fixed in
#      gasCircuitBlend(): the circuit is closed, nothing is created.
#   3. gasCoupledRecovery(): the time until threshold's fixed Runge-Kutta step
#      was outside its region of stability for a 3 kg patient ventilated at
#      4.6 L/min, or 70 kg at 100 L/min, and failed ("NAs are not allowed in
#      subscripted assignments").  Fixed by exponential time differencing
#      where the Runge-Kutta step would not serve.
#
# Each is reproduced through the app's own path (simulateGases(),
# gasWashout(), gasDrugEntries(), as the gases() reactive in app_server()
# calls them).
#
# Drafted by Claude Code, 2026-11-15, at the request of Steven L. Shafer.

carbon3Rows <- function(...) do.call(rbind, lapply(list(...), function(r)
  data.frame(Time = r[[1]], Drug = r[[2]], Dose = r[[3]], stringsAsFactors = FALSE)))

# The amount of a gas in the lungs and the tissues, litres of gas.
carbon3Amount <- function(sim, g) {
  props <- getGasProperties()
  body <- sim$body
  st <- sim$state[[g]]
  ltg <- gasPartitionTissueGas(props[props$gas == g, ])
  (body$V_alveolar * st[, 2] + body$V_brain * ltg[["brain"]] * st[, 3] +
     body$V_muscle * ltg[["muscle"]] * st[, 4] + body$V_fat * ltg[["fat"]] * st[, 5]) / 100
}

# The app's gases() reactive, with "Show time until threshold" on.
carbon3AppGases <- function(DT, weight, age = 40, maximum = 60) {
  sim <- simulateGases(DT, weight = weight, age = age, maximum = maximum)
  washout <- gasWashout(sim, DT, weight = weight)
  gasDrugEntries(sim, DT, getDrugDefaultsGlobal(), maximum, washout = washout,
                 age = age, macThreshold = GAS_MAC_THRESHOLD)
}


test_that("H2.1: no fresh gas on the ideal circuit plots over a week, and nothing overflows", {
  # The audit's case: sevoflurane 2% and 4.6 L/min of ventilation with no
  # oxygen flowing, or no oxygen row at all, on the plots of 3 and 7 days the
  # time settings allow.
  withO2 <- carbon3Rows(list(0, "sevoflurane", 2), list(0, "ventilation", 4.6), list(0, "oxygen", 0))
  for (DT in list(withO2, withO2[1:2, ])) {
    for (maximum in c(4320, 10080)) {
      sim <- simulateGases(DT, weight = 70, age = 40, maximum = maximum)
      expect_true(all(vapply(sim$state, function(m) all(is.finite(m)), logical(1))))
      # Nitrogen stays what the patient held: nothing is breathed in or out.
      expect_lte(max(sim$state$nitrogen[, 2]), 100 * AIR_FRACTION_N2 + 1e-9)
      entries <- gasDrugEntries(sim, DT, getDrugDefaultsGlobal(), maximum, age = 40)
      expect_setequal(names(entries), c("sevoflurane", "MAC"))
    }
  }
  # The plots that did not fail were wrong too: alveolar nitrogen reached
  # 101% of an atmosphere on the 1-hour plot, 105% on 4 hours, 142% on a day.
  sim <- simulateGases(withO2, weight = 70, age = 40, maximum = 60)
  expect_lte(max(sim$state$nitrogen[, 2]), 100 * AIR_FRACTION_N2 + 1e-9)
})


test_that("H2.1: a closed circuit creates no gas, and conserves every gas exactly", {
  # No fresh gas at all: the circuit and the patient are one closed system.
  # Each gas's amount is what it was (oxygen apart, which is consumed); the
  # sevoflurane dial delivers nothing, there being no flow to carry it.
  DT <- carbon3Rows(list(0, "sevoflurane", 2), list(0, "ventilation", 4.6), list(0, "oxygen", 0))
  sim <- simulateGases(DT, weight = 70, age = 40, maximum = 240)
  n2 <- carbon3Amount(sim, "nitrogen")
  expect_equal(n2, rep(n2[1], length(n2)), tolerance = 1e-10)
  expect_equal(max(abs(carbon3Amount(sim, "sevoflurane"))), 0)
  expect_equal(min(sim$state$oxygen), 0)            # consumed, and no further

  # Oxygen below the patient's consumption (0.245 L/min at 70 kg) carrying 2%
  # sevoflurane: the circuit is closed until the oxygen runs out, so every
  # litre of vapour delivered is in the patient and none is vented; after,
  # some is vented.  Before 2026-11-15 the body held up to 1.8 mL more
  # vapour than had been delivered, and 0.6 L more nitrogen than it started
  # with.
  for (o2 in c(0.1, 0.2)) {
    DT <- carbon3Rows(list(0, "sevoflurane", 2), list(0, "ventilation", 4.6), list(0, "oxygen", o2))
    sim <- simulateGases(DT, weight = 70, age = 40, maximum = 240)
    # (the oxygen in the lungs' litres, as the amounts are counted:
    # gasFreshGasExpansion(), 2026-11-28)
    delivered <- o2 * gasFreshGasExpansion(sim$body) * 0.02 * sim$timeLine
    held <- carbon3Amount(sim, "sevoflurane")
    expect_true(all(held <= delivered * (1 + 1e-9) + 1e-12), info = paste("oxygen", o2))
    closed <- sim$state$oxygen[, 2] > 0
    expect_equal(held[closed], delivered[closed], tolerance = 1e-8, info = paste("oxygen", o2))
    n2 <- carbon3Amount(sim, "nitrogen")
    expect_lte(max(n2), n2[1] * (1 + 1e-10))
  }

  # A low-flow nitrous oxide induction, whose uptake outruns the fresh gas at
  # first: still no gas made.
  DT <- carbon3Rows(list(0, "nitrousOxide", 0.3), list(0, "oxygen", 0.3),
                    list(0, "ventilation", 4.6), list(0, "sevoflurane", 2))
  sim <- simulateGases(DT, weight = 70, age = 40, maximum = 60)
  carrier <- 1 - 2 / 100
  e <- gasFreshGasExpansion(sim$body)
  expect_true(all(carbon3Amount(sim, "nitrousOxide") <= carrier * 0.3 * e * sim$timeLine + 1e-9))
  expect_true(all(carbon3Amount(sim, "sevoflurane") <= 0.6 * e * 0.02 * sim$timeLine + 1e-12))
})


test_that("H2.1: the circuit blend never rebreathes more than is exhaled", {
  body <- getGasBody(70)
  MV <- 4.6
  VA <- MV * (1 - GAS_DEAD_SPACE_FRACTION)
  VCO2 <- GAS_RESPIRATORY_QUOTIENT * body$VO2
  cE <- VCO2 / MV
  u <- body$VO2 - VCO2                  # the oxygen's net share of the uptake

  # Above the uptake plus the carbon dioxide absorbed the blend is the one
  # documented in the file header, and the circuit supplies all that is drawn.
  for (Q in c(0.3, 1, 3)) {
    bl <- gasCircuitBlend(Q, VA, MV, u = u, cE = cE)
    k <- (MV + u - Q) / (MV * (1 - cE))
    D <- MV + u - k * (MV - VA)
    expect_equal(bl, list(fresh = Q / D, alveolar = k * VA / D, supply = 1))
  }
  # Below it the circuit is closed: everything exhaled is rebreathed, the
  # inspired gas is what there is, and the alveoli receive Q + VA - VCO2.
  for (Q in c(0, 0.1, 0.2)) {
    bl <- gasCircuitBlend(Q, VA, MV, u = u, cE = cE)
    D <- Q + VA - VCO2
    expect_equal(bl$fresh, Q / D)
    expect_equal(bl$alveolar, VA / D)
    expect_equal(bl$supply, D / (VA + u))
    expect_lt(bl$supply, 1)
  }
  # Apnoea: the uptake is drawn from the fresh gas, as far as it goes.
  expect_equal(gasCircuitBlend(0, 0, 0, u = 0.05)$supply, 0)
  expect_equal(gasCircuitBlend(0.02, 0, 0, u = 0.05)$supply, 0.4)
  expect_equal(gasCircuitBlend(0.1, 0, 0, u = 0.05)$supply, 1)

  # So no system the engine builds can grow: every eigenvalue is at most zero,
  # where at Q = 0 nitrogen's was +0.097 per minute and oxygen's diagonal +0.10.
  props <- getGasProperties()
  for (Q in c(0, 0.1, 0.2, 0.3, 4.6)) {
    for (g in props$gas[props$soluble]) {
      A <- gasSystemSoluble(props[props$gas == g, ], body, Q, VA, body$Q_cardiac, 0,
                            totUptake = u, MV = MV, cE = cE)$A[2:5, 2:5]
      expect_lte(max(Re(eigen(A, only.values = TRUE)$values)), 1e-12)
    }
    expect_lte(gasSystemOxygen(body, Q, VA, 0, MV = MV, totUptake = u, cE = cE)$A[2, 2], 1e-12)
  }
})


test_that("H2.1: no ventilation and no fresh gas draws no vapour from the dial", {
  # Ventilation set from 10 minutes, the vaporiser at 8% and no fresh gas from
  # the start: until the ventilation starts nothing reaches the lungs, where
  # before 2026-11-15 the oxygen's uptake drew about 1 mL/min of vapour into
  # them from the dial alone.
  DT <- carbon3Rows(list(0, "sevoflurane", 8), list(0, "oxygen", 0), list(10, "ventilation", 4.6))
  sim <- simulateGases(DT, weight = 70, age = 40, maximum = 20)
  expect_equal(max(abs(carbon3Amount(sim, "sevoflurane"))), 0)
})


# The time until threshold by brute force, as test-gas-recovery.R does it: the
# agent (or every potent agent, for MAC) turned off at t in the dose table,
# the remaining fresh gas raised to 100,000 L/min so that nothing is
# rebreathed, and the full engine run on until the line comes down through
# the threshold.
carbon3Brute <- function(gasDose, weight, t, gas, threshold, horizon = 12) {
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
  b <- gasFlushEngine(d, t, weight, age = 40, maximum = t + horizon,
                            resolution = round((t + horizon) * 50) + 1)
  y <- if (gas == "MAC") b$results$Y[b$results$Drug == "MAC"] else b$state[[gas]][, 3]
  time <- b$timeLine
  keep <- time >= t
  time <- time[keep]; y <- y[keep]
  above <- which(y > threshold)
  if (length(above) == 0) return(0)
  i <- max(above)
  time[i] + (threshold - y[i]) * (time[i + 1] - time[i]) / (y[i + 1] - y[i]) - t
}


test_that("H2.3: the time until threshold is computed for a small patient or a large ventilation", {
  dd <- getDrugDefaultsGlobal()
  cases <- list(
    list(w = 3,   vent = 4.6, agent = "sevoflurane", pct = 2,   o2 = 6),
    list(w = 2,   vent = 4.6, agent = "isoflurane",  pct = 1.5, o2 = 6),
    list(w = 70,  vent = 100, agent = "isoflurane",  pct = 1.2, o2 = 100),
    list(w = 0.5, vent = defaultGasVentilation(0.5), agent = "isoflurane", pct = 1.5, o2 = 2)
  )
  for (cs in cases) {
    DT <- carbon3Rows(list(0, cs$agent, cs$pct), list(0, "oxygen", cs$o2), list(0, "ventilation", cs$vent))
    entries <- carbon3AppGases(DT, cs$w, maximum = 240)
    for (g in c(cs$agent, "MAC")) {
      r <- entries[[g]]$equiSpace$Recovery
      expect_true(all(is.finite(r)) && all(r >= 0), info = paste(cs$w, "kg", cs$vent, "L/min", g))
      expect_gt(max(r), 0)
    }
  }
})


test_that("H2.3: where the Runge-Kutta step would not serve, the answer matches the full engine", {
  # A 3 kg patient ventilated at 4.6 L/min, with nitrous oxide so that the
  # coupling is in play, and 70 kg at 100 L/min: the washout's fastest rates
  # are over 30 per minute, where the fixed 0.1-minute step was unstable.
  # Brute force, at a step of 0.02 minute, agrees with the time until
  # threshold to within a few seconds (the crossing of MAC, inside the first
  # 0.1-minute step, is interpolated across it).
  dd <- getDrugDefaultsGlobal()
  cases <- list(
    list(w = 3, DT = carbon3Rows(list(0, "oxygen", 2), list(0, "nitrousOxide", 4),
                                 list(0, "ventilation", 4.6), list(0, "isoflurane", 1.5)),
         agent = "isoflurane", at = c(5, 30)),
    list(w = 70, DT = carbon3Rows(list(0, "oxygen", 100), list(0, "ventilation", 100),
                                  list(0, "sevoflurane", 3)),
         agent = "sevoflurane", at = c(5, 30))
  )
  for (cs in cases) {
    sim <- simulateGases(cs$DT, weight = cs$w, age = 40, maximum = 45)
    w <- gasWashout(sim, cs$DT, weight = cs$w)
    thr <- gasThresholdForAge(cs$agent, dd$endCe[dd$Drug == cs$agent], 40)
    agent <- gasRecoveryTime(w, cs$agent, thr)
    mac <- macRecoveryTime(w, 40, GAS_MAC_THRESHOLD)
    for (t in cs$at) {
      want <- carbon3Brute(cs$DT, cs$w, t, cs$agent, thr)
      expect_gt(want, 0.5)
      expect_lt(abs(stats::approx(w$Time, agent, t)$y - want), 0.05,
                label = paste(cs$w, "kg agent at", t))
      wantMac <- carbon3Brute(cs$DT, cs$w, t, "MAC", GAS_MAC_THRESHOLD)
      expect_lt(abs(stats::approx(w$Time, mac, t)$y - wantMac), 0.05,
                label = paste(cs$w, "kg MAC at", t))
    }
  }
})


test_that("H2.3: any ventilation the dose table takes is finished, and finite", {
  # 1e9 L/min, the dose table's ceiling: a Runge-Kutta step held within its
  # region of stability would be a few billionths of a minute.
  DT <- carbon3Rows(list(0, "oxygen", 6), list(0, "ventilation", MAX_DOSE_VALUE),
                    list(0, "sevoflurane", 2))
  entries <- carbon3AppGases(DT, 3, maximum = 60)
  for (g in c("sevoflurane", "MAC")) {
    r <- entries[[g]]$equiSpace$Recovery
    expect_true(all(is.finite(r)) && all(r >= 0))
  }
  expect_gt(max(entries$sevoflurane$equiSpace$Recovery), 0)
})


test_that("H2.3: the exponential step is exact for the linear part and a constant inflow", {
  # One block, y' = A y + c: the step must be the exact solution, whatever
  # the step and however stiff A.
  body <- getGasBody(3)
  p <- getGasProperties()
  p <- p[p$gas == "isoflurane", ]
  A <- gasSystemSoluble(p, body, 100, 70, body$Q_cardiac, 0, circuit = "open")$A[2:5, 2:5]
  y0 <- c(1.2, 1.0, 0.6, 0.2)
  c0 <- c(3, 0, 0, 0)
  for (h in c(0.1, 0.25)) {
    cf <- gasEtdCoefficients(list(A), h)
    g <- matrix(c0, 1)
    Y <- matrix(y0, 1)
    # The half step's stage, and the full step's weights, against the
    # engine's own exact segment (advanceGasSegment()).
    half <- Y %*% cf$E2 + g %*% cf$P2
    expect_equal(as.vector(half), advanceGasSegment(y0, A, c0, h / 2), tolerance = 1e-12)
    step <- Y %*% cf$E + g %*% cf$F1 + 2 * (g + g) %*% cf$F2 + g %*% cf$F3
    expect_equal(as.vector(step), advanceGasSegment(y0, A, c0, h), tolerance = 1e-12)
    expect_equal(t(cf$E), expmPade(h * A), tolerance = 1e-14)
  }
})


test_that("H2.3: at ordinary rates the Runge-Kutta step is the one it always was", {
  # The steps are held only where the fastest rate demands it.  At 70 kg and
  # 4.6 to 12 L/min (4.2 to 7.5 per minute) they stand, so the times there
  # are the ones computed before 2026-11-15 to the last digit.
  expect_equal(gasRecoveryStep(0, 7.5), GAS_RECOVERY_STEP_FIRST_HOUR)
  expect_equal(gasRecoveryStep(60, 7.5), GAS_RECOVERY_STEP_LATER)
  expect_lt(gasRecoveryStep(0, 36), GAS_RECOVERY_STEP_FIRST_HOUR)
  expect_lt(gasRecoveryStep(60, 9.6), GAS_RECOVERY_STEP_LATER)
  expect_equal(gasRecoveryStep(0, 0), GAS_RECOVERY_STEP_FIRST_HOUR)
})
