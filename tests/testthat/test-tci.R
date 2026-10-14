# Target-controlled infusion (tci.R): Shafer & Gregg, J Pharmacokinet Biopharm
# 1992;20:147-169.  Drafted by Claude Code (Claude Fable 5.1), 2026-10-06, at
# the request of Steven L. Shafer; run on R 4.6.1.

local_mocked_bindings(outputComments = function(...) {})

noEvents <- data.frame(Time = double(), Event = character())
propofolPK <- getDrugPK("propofol", 70, 170, 50, "male", getDrugDefaults("propofol"))

tciDose <- function(Time, Dose, Units, drug = "propofol") {
  data.frame(Drug = drug, Time = Time, Dose = Dose, Units = Units)
}
series <- function(X, site) {
  r <- X$results[X$results$Site == site, ]
  r[order(r$Time), ]
}
at <- function(s, times) stats::approx(s$Time, s$Y, times)$y

test_that("an effect-site target is reached at tPeak without overshoot and then held", {
  X <- simCpCe(tciDose(0, 3, TCI_UNIT_EFFECT), noEvents, propofolPK, 60, FALSE)
  ce <- series(X, "Effect Site")
  cp <- series(X, "Plasma")

  # The loading infusion drives the plasma far above the target ...
  expect_gt(at(cp, 0.2), 2 * 3)
  # ... the effect site peaks at the target at tPeak, never above it ...
  expect_lt(max(ce$Y), 3 * 1.002)
  expect_equal(at(ce, propofolPK$tPeak), 3, tolerance = 0.01)
  # ... and plasma and effect site are then both held at the target.
  late <- ce$Time > 2 * propofolPK$tPeak
  expect_equal(ce$Y[late], rep(3, sum(late)), tolerance = 0.002)
  expect_equal(cp$Y[cp$Time > 2 * propofolPK$tPeak], rep(3, sum(cp$Time > 2 * propofolPK$tPeak)), tolerance = 0.002)
})

test_that("maintenance is a smooth plasma hold, not an aliasing effect-site controller", {
  X <- simCpCe(tciDose(0, 3, TCI_UNIT_EFFECT), noEvents, propofolPK, 60, FALSE)
  rates <- X$tci$rates
  maint <- rates$Rate[rates$Time > 5]
  expect_true(all(maint > 0))
  expect_lt(max(abs(diff(maint)) / maint[-1]), 0.01)
  # The rate falls monotonically as the peripheral compartments fill.
  expect_true(all(diff(maint) < 0))
})

test_that("the loading dose is reported as a bolus and the pump is then off until the peak", {
  X <- simCpCe(tciDose(0, 3, TCI_UNIT_EFFECT), noEvents, propofolPK, 60, FALSE)
  rates <- X$tci$rates
  expect_equal(rates$Units[1], "mg/kg/min")
  expect_true(rates$Bolus[1])
  expect_equal(rates$Time[1], 0)
  # Off from the end of the first interval until the peak ...
  expect_equal(rates$Rate[2], 0)
  expect_equal(rates$Time[2], TCI_INTERVAL)
  off <- rates$Time >= TCI_INTERVAL & rates$Time < propofolPK$tPeak - TCI_INTERVAL
  expect_true(all(rates$Rate[off] == 0))
  firstOn <- min(rates$Time[rates$Time >= TCI_INTERVAL & rates$Rate > 0])
  expect_gt(firstOn, propofolPK$tPeak - 2 * TCI_INTERVAL)
  # ... and the loading dose is the drug given in that first interval, in mg.
  b <- X$tci$boluses
  expect_equal(nrow(b), 1)
  expect_equal(b$Units, "mg")
  expect_equal(b$Amount, rates$Rate[1] * 70 * TCI_INTERVAL)
  # Roughly the target times the volume of distribution at peak effect
  # (the reciprocal of the unit-bolus effect-site concentration at tPeak).
  k <- propofolPK$PK[[1]]
  vdPeakEffect <- 1 / CE(propofolPK$tPeak,
                         k$e_coef_bolus_l1, k$e_coef_bolus_l2, k$e_coef_bolus_l3, k$e_coef_bolus_ke0,
                         k$lambda_1, k$lambda_2, k$lambda_3, k$ke0)
  expect_equal(b$Amount, 3 * vdPeakEffect, tolerance = 0.1)
})

test_that("a plasma target is held exactly from the end of the first interval", {
  X <- simCpCe(tciDose(0, 3, TCI_UNIT_PLASMA), noEvents, propofolPK, 60, FALSE)
  cp <- series(X, "Plasma")
  expect_equal(cp$Y[cp$Time >= TCI_INTERVAL], rep(3, sum(cp$Time >= TCI_INTERVAL)), tolerance = 1e-6)
  ce <- series(X, "Effect Site")
  expect_lt(max(ce$Y), 3 * 1.001)
  expect_true(X$tci$rates$Bolus[1])
  expect_gt(X$tci$rates$Rate[2], 0)   # maintenance starts at once
})

test_that("a lower target turns the pump off until the effect site has fallen to it", {
  X <- simCpCe(tciDose(c(0, 20), c(3, 1.5), TCI_UNIT_EFFECT), noEvents, propofolPK, 60, FALSE)
  rates <- X$tci$rates
  ce <- series(X, "Effect Site")
  off <- rates[rates$Time >= 20 & rates$Time < 23, ]
  expect_true(all(off$Rate == 0))
  expect_gt(max(rates$Rate[rates$Time > 30]), 0)
  expect_equal(ce$Y[ce$Time > 35], rep(1.5, sum(ce$Time > 35)), tolerance = 0.002)
  # Never below the new target by more than the plasma hand-off allows.
  expect_gt(min(ce$Y[ce$Time > 20]), 1.5 * 0.99)
  expect_equal(nrow(X$tci$boluses), 1)
})

test_that("a target already exceeded by a manual bolus and infusion gives no loading dose, and still simulates", {
  # The table that crashed the app: a 150 mg bolus and 100 mcg/kg/min with a
  # 3 mcg/ml effect-site target, all at time 0.  The bolus puts the effect
  # site above the target before the pump could load it, so the TCI schedule
  # has rates but no boluses, and tciDisplay() must build an empty bolus
  # table rather than fail on a length-1 Drug column beside zero-length ones.
  DT <- data.frame(
    Drug  = "propofol",
    Time  = c(0, 0, 0),
    Dose  = c(150, 100, 3),
    Units = c("mg", "mcg/kg/min", TCI_UNIT_EFFECT)
  )
  X <- simCpCe(DT, noEvents, propofolPK, 60, FALSE)
  expect_gt(nrow(X$tci$rates), 0)
  expect_equal(nrow(X$tci$boluses), 0)
  expect_equal(names(X$tci$boluses), c("Drug", "Time", "Amount", "Units"))
  expect_false(any(X$tci$rates$Bolus))
  ce <- series(X, "Effect Site")
  expect_gt(max(ce$Y), 3)
  expect_equal(ce$Y[ce$Time > 40], rep(3, sum(ce$Time > 40)), tolerance = 0.01)
})

test_that("a higher target during maintenance gives a second loading dose", {
  X <- simCpCe(tciDose(c(0, 20), c(3, 4), TCI_UNIT_EFFECT), noEvents, propofolPK, 60, FALSE)
  b <- X$tci$boluses
  expect_equal(b$Time, c(0, 20))
  expect_lt(b$Amount[2], b$Amount[1])
  ce <- series(X, "Effect Site")
  expect_lt(max(ce$Y), 4 * 1.002)
  expect_equal(ce$Y[ce$Time > 25], rep(4, sum(ce$Time > 25)), tolerance = 0.002)
})

test_that("a manual bolus during TCI stops delivery until the target is regained", {
  X <- simCpCe(
    tciDose(c(0, 30), c(3, 50), c(TCI_UNIT_EFFECT, "mg")),
    noEvents, propofolPK, 60, FALSE
  )
  rates <- X$tci$rates
  ce <- series(X, "Effect Site")
  expect_gt(at(ce, 31.5), 3.5)
  expect_true(all(rates$Rate[rates$Time >= 30 & rates$Time < 33] == 0))
  expect_gt(max(rates$Rate[rates$Time > 45]), 0)
  expect_equal(at(ce, 60), 3, tolerance = 0.01)
})

test_that("a manual infusion stops the TCI infusion", {
  X <- simCpCe(
    tciDose(c(0, 30), c(3, 100), c(TCI_UNIT_EFFECT, "mcg/kg/min")),
    noEvents, propofolPK, 60, FALSE
  )
  rates <- X$tci$rates
  # The schedule ends with a zero at the takeover, as for a target of 0
  expect_equal(max(rates$Time), 30)
  expect_equal(rates$Rate[nrow(rates)], 0)
  expect_lt(max(rates$Time[rates$Rate > 0]), 30)
  # From 30 min the drug follows the manual rate, so the concentration drifts
  # away from the target.
  ce <- series(X, "Effect Site")
  expect_gt(abs(at(ce, 60) - 3), 0.1)
})

# The concentrations were always right, and are unchanged: the engine adds the
# zero row to the manual rate set at the same moment.  What the zero row fixes
# is the display.  Without it the rate panel held the last TCI rate (0.097
# mg/kg/min at minute 29.83 here) out to the end of the plot, and the hover
# read it at minute 40, while the pump was running the manual 20 mcg/kg/min
# alone.
test_that("after a manual takeover the rate panel and its hover read zero", {
  doseTable <- tciDose(c(0, 30), c(3, 20), c(TCI_UNIT_EFFECT, "mcg/kg/min"))
  drugs <- processdoseTable(
    doseTable, noEvents,
    recalculatePK(NULL, getDrugDefaultsGlobal(FALSE), doseTable, 50, 70, 170, "male"),
    60, FALSE
  )
  rates <- drugs$propofol$tci$rates
  # the hover's rule: the last rate row at or before the hovered time
  hovered <- function(x) rates$Rate[max(which(rates$Time <= x), 1)]
  expect_gt(hovered(29.9), 0)
  expect_equal(hovered(40), 0)

  p <- simulationPlot(
    drugs = drugs, events = noEvents,
    drugDefaults = getDrugDefaultsGlobal(FALSE), eventDefaults = getEventDefaults(),
    xMaximum = 60, plotRecovery = FALSE, plotEvents = FALSE
  )
  panel <- p$plotResults[p$plotResults$Drug == "propofol TCI", ]
  expect_true(all(panel$Y[panel$Time >= 30] == 0))
})

# Every TCI drug: the controller's last word before a manual infusion is zero.
test_that("every TCI drug's schedule records the manual takeover", {
  for (drug in c("propofol", "remifentanil", "fentanyl", "alfentanil", "sufentanil",
                 "lidocaine", "hydromorphone", "etomidate", "ketamine")) {
    PK <- getDrugPK(drug, 70, 170, 50, "male", getDrugDefaults(drug))
    infusion <- getDrugDefaults(drug)$Infusion.Units
    X <- simCpCe(tciDose(c(0, 30), c(PK$typical, 1), c(TCI_UNIT_EFFECT, infusion), drug),
                 noEvents, PK, 60, FALSE)
    r <- X$tci$rates
    expect_equal(r$Time[nrow(r)], 30, info = drug)
    expect_equal(r$Rate[nrow(r)], 0, info = drug)
  }
})

test_that("a target of 0 stops the TCI infusion and the drug washes out", {
  X <- simCpCe(tciDose(c(0, 10), c(3, 0), TCI_UNIT_EFFECT), noEvents, propofolPK, 60, FALSE)
  rates <- X$tci$rates
  expect_equal(max(rates$Time), 10)
  expect_equal(rates$Rate[nrow(rates)], 0)
  ce <- series(X, "Effect Site")
  expect_lt(at(ce, 60), 1)
})

test_that("a target zeroes a manual infusion entered at the same time", {
  withManual <- simCpCe(
    tciDose(c(0, 0), c(100, 3), c("mcg/kg/min", TCI_UNIT_EFFECT)),
    noEvents, propofolPK, 60, FALSE
  )
  alone <- simCpCe(tciDose(0, 3, TCI_UNIT_EFFECT), noEvents, propofolPK, 60, FALSE)
  expect_equal(withManual$results, alone$results)
})

test_that("a manual infusion already running is zeroed when the target starts", {
  X <- simCpCe(
    tciDose(c(0, 10), c(100, 3), c("mcg/kg/min", TCI_UNIT_EFFECT)),
    noEvents, propofolPK, 60, FALSE
  )
  cp <- series(X, "Plasma")
  expect_equal(cp$Y[cp$Time > 15], rep(3, sum(cp$Time > 15)), tolerance = 0.002)
})

test_that("drugs measured in ng/ml report rates in mcg/kg/min and loading doses in mcg", {
  PK <- getDrugPK("remifentanil", 70, 170, 50, "male", getDrugDefaults("remifentanil"))
  X <- simCpCe(tciDose(0, 4, TCI_UNIT_EFFECT, "remifentanil"), noEvents, PK, 30, FALSE)
  expect_equal(X$tci$rates$Units[1], "mcg/kg/min")
  expect_equal(X$tci$boluses$Units, "mcg")
  ce <- series(X, "Effect Site")
  expect_lt(max(ce$Y), 4 * 1.002)
  expect_equal(at(ce, 10), 4, tolerance = 0.002)
})

test_that("every TCI drug reaches an effect-site target without overshoot", {
  for (drug in c("propofol", "remifentanil", "fentanyl", "alfentanil", "sufentanil",
                 "lidocaine", "hydromorphone", "etomidate", "ketamine")) {
    PK <- getDrugPK(drug, 70, 170, 50, "male", getDrugDefaults(drug))
    target <- PK$typical
    X <- simCpCe(tciDose(0, target, TCI_UNIT_EFFECT, drug), noEvents, PK, 120, FALSE)
    ce <- series(X, "Effect Site")
    expect_lt(max(ce$Y), target * 1.005, label = paste(drug, "max Ce"))
    expect_equal(at(ce, 100), target, tolerance = 0.005, label = paste(drug, "Ce at 100 min"))
  }
})

test_that("a long simulation stays on target with a bounded number of rate rows", {
  X <- simCpCe(tciDose(0, 3, TCI_UNIT_EFFECT), noEvents, propofolPK, MINS_PER_DAY, FALSE)
  ce <- series(X, "Effect Site")
  expect_equal(ce$Y[ce$Time > 10], rep(3, sum(ce$Time > 10)), tolerance = 0.002)
  expect_lt(nrow(X$tci$rates), 3000)
})

test_that("no target means no TCI schedule", {
  X <- simCpCe(tciDose(0, 100, "mg"), noEvents, propofolPK, 60, FALSE)
  expect_null(X$tci)
})

test_that("the dose table accepts target units for the TCI drugs", {
  DT <- data.frame(Drug = "propofol", Time = "0", Dose = "3", Units = TCI_UNIT_EFFECT)
  expect_true(validateDoseTableInput(DT))
  expect_true(TCI_UNIT_EFFECT %in% getDrugDefaults("propofol")$Units[[1]])
  expect_true(TCI_UNIT_PLASMA %in% getDrugDefaults("remifentanil")$Units[[1]])
  expect_false(TCI_UNIT_EFFECT %in% getDrugDefaults("rocuronium")$Units[[1]])
})

test_that("the TCI rows merge into the dose table for export", {
  doseTable <- tciDose(0, 3, TCI_UNIT_EFFECT)
  drugs <- processdoseTable(
    doseTable, noEvents,
    recalculatePK(NULL, getDrugDefaultsGlobal(FALSE), doseTable, 50, 70, 170, "male"),
    60, FALSE
  )
  merged <- tciMergeDoseTable(doseTable, drugs)
  expect_equal(names(merged), c("Drug", "Time", "Dose", "Units"))
  expect_equal(nrow(merged), 1 + nrow(drugs$propofol$tci$rates))
  expect_true(all(merged$Units[-1] == "mg/kg/min"))
  expect_equal(merged$Units[1], TCI_UNIT_EFFECT)
})

test_that("the plot gets a rate panel per TCI drug, coloured like the drug", {
  doseTable <- rbind(
    tciDose(0, 3, TCI_UNIT_EFFECT),
    tciDose(0, 100, "mcg", "fentanyl")
  )
  drugs <- processdoseTable(
    doseTable, noEvents,
    recalculatePK(NULL, getDrugDefaultsGlobal(FALSE), doseTable, 50, 70, 170, "male"),
    60, TRUE
  )
  p <- simulationPlot(
    drugs = drugs, events = noEvents,
    drugDefaults = getDrugDefaultsGlobal(FALSE), eventDefaults = getEventDefaults(),
    plotRecovery = TRUE, plotEvents = TRUE
  )
  wraps <- levels(p$plotResults$Wrap)
  expect_true("propofol TCI\n(mg/kg/min)" %in% wraps)
  expect_false(any(grepl("fentanyl TCI", wraps)))
  # The rate panel sits after the concentration panels and before Events.
  expect_lt(which(wraps == "fentanyl\n(ng/ml)"), which(wraps == "propofol TCI\n(mg/kg/min)"))
  expect_lt(which(wraps == "propofol TCI\n(mg/kg/min)"), which(wraps == PLOT_NAME_EVENTS))
  rate <- p$plotResults[p$plotResults$Site == "Rate", ]
  expect_true(all(rate$Drug == "propofol TCI"))
  expect_equal(max(rate$Time), 60)
  expect_s3_class(p$plotObject, "ggplot")
  expect_no_error(ggplot2::ggplot_build(p$plotObject))
})

# A regional infusion entered before a plasma target (lidocaine "mg/hr RA" at
# 0, "Plasma target" at 5 min).  The TCI rows are copies of the first row of
# the table, and the RArate flag once survived the copy: the controller's
# intravenous schedule was then delivered into the tissue depot and the plasma
# plotted at about half the target an hour later, with no error.  Found in a
# production audit on 2026-10-13.  (Claude Code, 2026-10-14, at the request of
# Steven L. Shafer.)
test_that("a regional infusion before a plasma target does not leak its route into the TCI rows", {
  dd <- getDrugDefaultsGlobal()
  lidoPK <- getDrugPK("lidocaine", 70, 170, 50, "male", dd[dd$Drug == "lidocaine", ])
  expect_gt(lidoPK$PK$default$ka_RA, 0)

  # The dose table as the engine receives it, captured from the engine call
  seen <- list()
  origDepot <- advanceClosedFormPO_IM_IN
  origIV    <- advanceClosedForm0
  local_mocked_bindings(
    advanceClosedFormPO_IM_IN = function(dose, ...) { seen$depot <<- dose; origDepot(dose, ...) },
    advanceClosedForm0        = function(dose, ...) { seen$iv    <<- dose; origIV(dose, ...) }
  )
  lido <- function(unit) data.frame(
    Drug = "lidocaine", Time = c(0, 5), Dose = c(100, 1),
    Units = c(unit, TCI_UNIT_PLASMA))

  raFirst <- simCpCe(lido("mg/hr RA"), noEvents, lidoPK, 60, FALSE)
  engineDose <- seen$depot
  expect_false(is.null(engineDose))
  # Every logical per-dose flag the engine sees is one tciSchedule() clears;
  # a new flag in simCpCe() is added to tciRouteFlags() or this fails.
  flags <- names(engineDose)[vapply(engineDose, is.logical, logical(1))]
  expect_setequal(flags, tciRouteFlags())
  tciRows <- engineDose[engineDose$Units == "TCI", ]
  expect_gt(nrow(tciRows), 0)
  for (flag in tciRouteFlags()) expect_false(any(tciRows[[flag]]), info = flag)
  # The regional infusion itself keeps its route and runs on, as entered
  raRow <- engineDose[engineDose$Units != "TCI", ]
  expect_equal(nrow(raRow), 1)
  expect_true(raRow$RArate)

  # The trajectory: the controller knows nothing of the depot, so the plasma
  # is the target plus what the regional infusion alone contributes.
  raOnly  <- simCpCe(lido("mg/hr RA")[1, ], noEvents, lidoPK, 60, FALSE)
  ivFirst <- simCpCe(lido("mg/hr"), noEvents, lidoPK, 60, FALSE)
  late <- seq(5 + 2 * TCI_INTERVAL, 60, by = 1)
  expect_equal(at(series(raFirst, "Plasma"), late),
               1 + at(series(raOnly, "Plasma"), late), tolerance = 0.002)
  expect_equal(at(series(ivFirst, "Plasma"), late), rep(1, length(late)), tolerance = 0.002)
  # Before the fix the regional-first plasma was 0.49 at 60 min
  expect_gt(at(series(raFirst, "Plasma"), 60), 1)
  # Both schedules are the same intravenous pump, in the same units
  expect_equal(raFirst$tci$rates$Units, ivFirst$tci$rates$Units)
})
