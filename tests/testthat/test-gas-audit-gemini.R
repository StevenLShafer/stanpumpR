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
