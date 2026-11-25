# Gemini's audit of the gas engine, 2026-11-23 (at stanpumpR c90c33e and
# GasMan d3a2dd3; docs/gas-engine-audit/gemini-2026-11-23-assessment.md).
# Each test here failed on ec042bd6, the master the fixes were made on.


# D-01.  The opioid-adjusted MAC read each opioid from its 100 equispaced
# points.  On a 24-hour plot those are 14.5 minutes apart, so a remifentanil
# bolus at 5 minutes read 0.64 x MEAC at 6.2 minutes where its effect site
# was at 7.8 ng/mL (adjusted MAC 0.86 where it is 2.45), and on a plot of any
# length its effect began before it was given.

geminiOpioidRun <- function(maximum, bolusAt = 5) {
  defaults <- getDrugDefaultsGlobal(FALSE)
  DT <- data.frame(Drug = "remifentanil", Time = bolusAt, Dose = 140, Units = "mcg")
  events <- data.frame(Time = numeric(0), Event = character(0))
  drugs <- suppressMessages(processdoseTable(
    DT, events, recalculatePK(NULL, defaults, DT, 40, 70, 170, "male"), maximum, FALSE))
  gasDose <- data.frame(Time = 0, Drug = c("oxygen", "ventilation", "sevoflurane"),
                        Dose = c(2, 6, 2))
  sim <- simulateGases(gasDose, weight = 70, age = 40, maximum = maximum)
  washout <- gasWashout(sim, gasDose, weight = 70)
  entries <- gasDrugEntries(sim, gasDose, defaults, maximum, washout = washout, age = 40)
  list(drugs = drugs, entries = entries, out = applyOpioidMacInteraction(entries, drugs))
}

geminiPlasma <- function(entry) {
  r <- entry$results
  r[r$Site == "Plasma", c("Time", "Y")]
}

test_that("D-01: the opioid's effect on MAC starts with the bolus, at its own concentration", {
  local_mocked_bindings(outputComments = function(...) invisible(NULL))
  day <- geminiOpioidRun(1440)
  unadjusted <- geminiPlasma(day$entries$MAC)
  adjusted <- geminiPlasma(day$out$MAC)

  # Before the bolus nothing changes (it read up to 1.25 times the
  # unadjusted MAC at 4.88 minutes)
  before <- adjusted[adjusted$Time > 0 & adjusted$Time < 5, ]
  expect_equal(before$Y, seriesStepAt(unadjusted$Time, unadjusted$Y, before$Time),
               tolerance = 1e-12)

  # After it, the adjusted MAC is the unadjusted over 1 - R of the effect site
  # read from the drug's own series
  remi <- day$drugs$remifentanil
  after <- adjusted[adjusted$Time > 5 & adjusted$Time <= 15, ]
  U <- derivedEffectSite(remi, after$Time) / as.numeric(remi$MEAC)
  expect_equal(after$Y, seriesStepAt(unadjusted$Time, unadjusted$Y, after$Time) /
                 (1 - opioidMacReduction(U)), tolerance = 1e-9)

  # A day's plot agrees with an hour's: 2.449 against 2.450 at 6.2 minutes
  # (it read 0.856)
  hour <- geminiOpioidRun(60)
  a <- geminiPlasma(hour$out$MAC)
  expect_equal(seriesStepAt(adjusted$Time, adjusted$Y, 6.2),
               seriesStepAt(a$Time, a$Y, 6.2), tolerance = 0.02)

  # A bolus late in the plot is drawn at its peak, the opioid's own times
  # having joined the MAC series (3.883; 3.742 at the gas engine's points)
  late <- geminiOpioidRun(1440, bolusAt = 200)
  l <- geminiPlasma(late$out$MAC)
  remiLate <- late$drugs$remifentanil
  times <- remiLate$results$Time[remiLate$results$Site == "Effect Site"]
  times <- times[times > 195 & times < 215]
  u <- geminiPlasma(late$entries$MAC)
  exact <- seriesStepAt(u$Time, u$Y, times) /
    (1 - opioidMacReduction(derivedEffectSite(remiLate, times) / as.numeric(remiLate$MEAC)))
  expect_equal(max(l$Y[l$Time > 195 & l$Time < 215]), max(exact), tolerance = 1e-3)
})

test_that("D-01: the opioid-adjusted time until threshold is computed where the opioid changes", {
  local_mocked_bindings(outputComments = function(...) invisible(NULL))
  day <- geminiOpioidRun(1440)
  mac <- day$entries$MAC
  w <- mac$washout
  series <- geminiPlasma(mac)
  R <- opioidMacReduction(totalOpioidMEAC(day$drugs, series$Time)$U)
  threshold <- mac$endCe * (1 - R)
  props <- gasPropertiesAt(w$body$temperature)
  gases <- names(w$state)
  potent <- intersect(props$gas[props$potent], gases)
  m <- gasStateCount(w$body) - 1L
  target <- numeric(m * length(gases))
  for (g in potent)
    target[(match(g, gases) - 1) * m + 1] <- 1 / macForAge(props$MAC40[props$gas == g], 40)
  every <- gasCoupledRecovery(w, off = potent, target = target, threshold = threshold,
                              maxPoints = Inf)
  sampled <- macRecoveryTime(w, 40, threshold)
  # Within 0.1 minute of every row computed (the 120-row sample read 4.3
  # minutes where every row gives 8.7, at 5.9 minutes)
  near <- w$Time >= 5 & w$Time <= 15
  expect_lt(max(abs(sampled[near] - every[near])), 0.1)
  # A threshold constant within each interval adds nothing to the sample
  expect_identical(gasRecoveryRefine(c(1L, 5L, 10L), rep(0.1, 10), 1:10, rep(1L, 10)),
                   c(1L, 5L, 10L))
  # and one that moves adds the rows it moves at
  thr <- c(rep(0.1, 5), 0.05, rep(0.1, 4))
  expect_true(6L %in% gasRecoveryRefine(c(1L, 5L, 10L), thr, 1:10, rep(1L, 10)))
})


# C-07.  A physiology event at time 0 left the run starting in the normal
# body for no time: at 5000 m the lungs started at sea-level room air (752
# Torr where the ambient pressure is 420), the first time was recorded
# twice, and the Visualizer refused a run whose temperature was 30 C
# throughout as one whose temperature changed.

geminiEventAt0 <- function(...) {
  x <- c(liver = 1, renal = 1, cardiac = 1, gi = 1, temperature = 37, altitude = 0,
         endobronchial = 0, haemoglobin = 15)
  set <- list(...)
  x[names(set)] <- unlist(set)
  physiologyEventsFrom(data.frame(Time = 0, Event = "Physiology",
                                  Settings = physiologyEventEncode(x),
                                  stringsAsFactors = FALSE))
}

test_that("C-07: an event at time 0 gives the run the slider gives", {
  d <- data.frame(Time = 0, Drug = c("air", "sevoflurane", "ventilation"), Dose = c(6, 2, 6))
  slider <- simulateGases(d, weight = 70, age = 40, maximum = 60, altitude = 5000)
  event <- simulateGases(d, weight = 70, age = 40, maximum = 60,
                         physiologyEvents = geminiEventAt0(altitude = 5000))
  expect_identical(event$timeLine, slider$timeLine)
  for (g in names(slider$state))
    expect_equal(event$state[[g]], slider$state[[g]], tolerance = 1e-12, label = g)
  # (the lungs started at 78.07% nitrogen and 20.93% oxygen, sea level's)
  expect_lt(event$state$nitrogen[1, 2], 45)
  expect_identical(event$bodySteps$time, 0)
})

test_that("C-07: the Visualizer draws a run whose temperature is set at time 0", {
  d <- data.frame(Time = 0, Drug = c("air", "sevoflurane", "ventilation"), Dose = c(6, 2, 6))
  s <- simulateGases(d, weight = 70, age = 40, maximum = 30,
                     physiologyEvents = geminiEventAt0(temperature = 30))
  e <- gasDrugEntries(s, d, maximum = 30, age = 40)
  expect_no_error(viewerGasRun(e$sevoflurane, 0, 30))
  # An event later in the run is still taken there, the body before it the
  # normal one
  later <- physiologyEventsFrom(data.frame(
    Time = c(0, 20), Event = "Physiology",
    Settings = c(physiologyEventEncode(c(liver = 1, renal = 1, cardiac = 1, gi = 1,
                                         temperature = 37, altitude = 1000,
                                         endobronchial = 0, haemoglobin = 15)),
                 physiologyEventEncode(c(liver = 1, renal = 1, cardiac = 1, gi = 1,
                                         temperature = 37, altitude = 3000,
                                         endobronchial = 0, haemoglobin = 15))),
    stringsAsFactors = FALSE))
  steps <- gasBodySteps(70, later)
  expect_equal(steps$time, c(0, 20))
  expect_equal(vapply(steps$body, function(b) b$altitude, 0), c(1000, 3000))
})


# C-07, the third part.  A step in altitude kept every gas-phase tension, so
# just after a step from sea level to 5000 m the alveolar gases summed to 94%
# of a sea-level atmosphere where at most 52% fits.

geminiAltitudeStep <- function(from, to, at = 10) {
  enc <- function(a) physiologyEventEncode(c(liver = 1, renal = 1, cardiac = 1, gi = 1,
                                              temperature = 37, altitude = a,
                                              endobronchial = 0, haemoglobin = 15))
  physiologyEventsFrom(data.frame(Time = c(0, at), Event = "Physiology",
                                  Settings = c(enc(from), enc(to)),
                                  stringsAsFactors = FALSE))
}

test_that("C-07: a fall in pressure lets gas out of the lungs at their composition", {
  d <- data.frame(Time = 0, Drug = c("air", "sevoflurane", "ventilation"), Dose = c(6, 2, 6))
  s <- simulateGases(d, weight = 70, age = 40, maximum = 30,
                     physiologyEvents = geminiAltitudeStep(0, 5000))
  step <- which(s$timeLine == 10)
  expect_length(step, 2)
  dry <- function(a) (barometricPressure(a) - WATER_VAPOUR_PRESSURE) /
    (barometricPressure(0) - WATER_VAPOUR_PRESSURE)
  alveolar <- function(i) sum(vapply(s$state, function(m) m[i, 2], 0))
  # Dalton: the gases fill the dry share of the new pressure, no more
  expect_lte(alveolar(step[2]), 100 * dry(5000) + 1e-9)
  # and each falls in proportion
  for (g in names(s$state))
    expect_equal(s$state[[g]][step[2], 2], s$state[[g]][step[1], 2] * dry(5000) / dry(0),
                 tolerance = 1e-12, label = g)
  # Dissolved agent does not move at the step
  expect_equal(s$state$sevoflurane[step[2], 3:5], s$state$sevoflurane[step[1], 3:5])
})

test_that("C-07: a rise in pressure draws fresh gas into the alveoli", {
  # A flow that covers what the lungs draw: at less, only the fresh gas's
  # share of the inspired gas is drawn (test-coderabbit-858.R)
  d <- data.frame(Time = 0, Drug = c("air", "sevoflurane", "ventilation"), Dose = c(10, 2, 6))
  s <- simulateGases(d, weight = 70, age = 40, maximum = 30,
                     physiologyEvents = geminiAltitudeStep(5000, 0))
  step <- which(s$timeLine == 10)
  r <- pressureRatio(5000)
  a1 <- r * gasHumidityRatio(list(altitude = 5000))
  # The ideal circuit holds no gas: the compressed lungs fill with fresh gas at
  # the new pressure (sea level, r = 1)
  fresh <- gasSettingsAt(split(d, d$Drug), 10, GAS_DEAD_SPACE_FRACTION, 1)$Ffgf
  for (g in names(s$state))
    expect_equal(s$state[[g]][step[2], 2],
                 s$state[[g]][step[1], 2] + unname(fresh[g]) * (1 - a1),
                 tolerance = 1e-12, label = g)
  alveolar <- sum(vapply(s$state, function(m) m[step[2], 2], 0))
  expect_lte(alveolar, 100 + 1e-9)
  # A semi-closed circuit's own stored gas is what fills them
  sc <- simulateGases(d, weight = 70, age = 40, maximum = 30, circuit = "semi-closed",
                      physiologyEvents = geminiAltitudeStep(5000, 0))
  k <- which(sc$timeLine == 10)
  expect_equal(sc$state$sevoflurane[k[2], 2],
               sc$state$sevoflurane[k[1], 2] + sc$state$sevoflurane[k[1], 1] / r * (1 - a1),
               tolerance = 1e-12)
  expect_equal(sc$state$sevoflurane[k[2], 1], sc$state$sevoflurane[k[1], 1] / r,
               tolerance = 1e-12)
})


# C-06.  The alveolar tension was converted to mmHg at 760 Torr per 100%,
# but the engine's alveolar tensions are fractions of the dry gas, whose
# share of a sea-level atmosphere is 760 - 47 = 713 Torr: on pure oxygen the
# alveolar oxygen read 722.7 mmHg, more than the dry gas can hold.

test_that("C-06: the alveolar oxygen never exceeds the dry share of the pressure", {
  DT <- data.frame(Drug = c("oxygen", "ventilation"), Time = 0, Dose = c(6, 6))
  a <- gasArterialOxygen(simulateGases(DT, 70, 40, maximum = 30))
  expect_lt(max(a$PAO2), 760 - 47)
  # Humidified room air in the lungs at 7000 m reads 0.2093 x (P_B - 47), as
  # Dalton's law says (at 760 Torr per 100% it read 6.6% high)
  air <- simulateGases(data.frame(Drug = c("air", "ventilation"), Time = 0, Dose = c(6, 6)),
                       70, 40, maximum = 10, altitude = 7000)
  # (to West's model atmosphere, whose sea level is 759.5 Torr, not 760:
  # 0.07%)
  expect_equal(gasArterialOxygen(air)$PAO2[1],
               AIR_FRACTION_O2 * (barometricPressure(7000) - WATER_VAPOUR_PRESSURE),
               tolerance = 1e-3)
})


# A-01.  GasMan starts a new history sample every ten ticks (SAMP_EVERY) and
# re-arms its fast-decay test at each one, settings changed or not; the R
# restatement re-armed it only at a settings row, so a 5 kg patient read
# 1.2% low against GasMan.  GasMan's own output for this case, frozen here:
# gasman_run built from rasman/gasmanonline at d3a2dd3 (GPL-3.0), input
#   [patient] weight_kg 5, dt_ms 6000; [agent] Sevoflurane;
#   [settings] va,fgf,circuit,time,co,del: 1,1,Semi,00:00:00,1,0 and
#   4,20,Semi,00:01:00,1,2; --end 600 s (no nitrogen agent),
# alveolar sevoflurane at 1:06, 1:12, 2:06, 2:12, 3:00 and 5:06.

test_that("A-01: the GasMan restatement re-arms its fast-decay test every ten ticks", {
  DT <- data.frame(Time = c(0, 0, 0, 1, 1, 1),
                   Drug = rep(c("oxygen", "sevoflurane", "ventilation"), 2),
                   Dose = c(1, 0, 1, 20, 2, 4))
  gm <- advanceGasManBaseline(DT, maximum = 6, weight = 5, cardiacOutput = 1, dt = 0.1,
                              circuit = "semi-closed", resolution = 61, nitrogenAmbient = 0)
  cpp <- c(`1.1` = 0.145385, `1.2` = 0.465449, `2.1` = 1.61298, `2.2` = 1.65586,
           `3` = 1.81463, `5.1` = 1.90338)
  at <- vapply(as.numeric(names(cpp)), function(t) which.min(abs(gm$timeLine - t)), 1L)
  r <- gm$state$sevoflurane[at, "ALV"]
  # Within 0.25% (it was 1.19% low at 2.1 minutes); the first two exact
  expect_equal(unname(r[1:2]), unname(cpp[1:2]), tolerance = 1e-5)
  expect_lt(max(abs(r / cpp - 1)), 0.0025)
  expect_equal(gm$maxVernier, 1)
})
