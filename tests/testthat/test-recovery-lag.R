# "Time until threshold" across an absorption lag.
#
# An extravascular dose may carry a lag, which the engines apply by moving the
# dose later.  Between being given and starting, the drug is in the patient and
# nothing in the model represents it, so recoveryCalc() finds nothing above the
# threshold and returns zero -- which reads as "recovered" when it means "not
# yet absorbed".  Those are opposite situations.  The engines now report NA
# over such a window instead, and these tests pin that.
#
# This is the small half of the problem, deliberately: reporting the RIGHT time
# through a lag means recoveryCalc() accounting for input that has not arrived
# yet, which is a change to its contract.  See R/recoveryStates.R.
#
# Before gabapentin no drug carried a lag -- hydromorphone's intramuscular and
# intranasal lags were the last, and went when its absorption was refitted --
# so most tests here put one in by hand.  Three drugs now carry one, as below.
#
# Acetaminophen followed on 2026-10-08, keeping Morse 2022's published 5.3 min
# oral lag by decision of Steven L. Shafer (see R/drugs_acetaminophen.R); the
# test after the allow-list pins it on the real drug.
#
# Gabapentin made the behaviour live on 2026-10-07: Tran 2017's oral lag of
# 0.311 h is an estimated parameter (RSE 8%), kept rather than folded into ka.
# Gabapentin has no effect site and no default threshold, so the gap shows
# only when a threshold is set under Drug Thresholds, timed on the plasma; the
# last test here pins it on the real drug.
#
# Pregabalin followed on 2026-10-08, with Chan 2021's estimated lag of 0.32 h
# and, unlike gabapentin, an effect site, so with a threshold set the gap is
# timed on the effect site; the test after gabapentin's pins that.
#
# (Claude Code, Claude Opus 5, 2026-10-06; run on R 4.6.1.)

noEvents <- data.frame(Time = numeric(0), Event = character(0))

lagPK <- function(drug, ..., height = 171) {
  dd <- getDrugDefaultsGlobal()
  PK <- getDrugPK(drug, 70, height, 50, "male", dd[dd$Drug == drug, ])
  PK$endCe <- dd$endCe[dd$Drug == drug]
  lags <- list(...)
  for (nm in names(lags)) PK$PK$default[[nm]] <- lags[[nm]]
  PK
}


test_that("only gabapentin, pregabalin and acetaminophen carry an absorption lag", {
  # If this ever fails it is not a defect -- a drug has gained or lost a lag,
  # and the behaviour the rest of this file guards has changed where it is
  # live.  Worth knowing, and worth rereading R/recoveryStates.R.
  dd <- getDrugDefaultsGlobal()
  lagged <- character(0)
  for (drug in dd$Drug[!isGasDrug(dd$Drug)])
  {
    PK <- tryCatch(getDrugPK(drug, 70, 170, 50, "male", dd[dd$Drug == drug, ]),
                   error = function(e) NULL)
    if (is.null(PK)) next
    for (s in PK$PK)
    {
      lags <- c(s$tlag_PO, s$tlag_IM, s$tlag_IN)
      if (any(!is.na(lags) & lags > 0)) lagged <- c(lagged, drug)
    }
  }
  expect_setequal(unique(lagged), c("gabapentin", "pregabalin", "acetaminophen"))
})


test_that("acetaminophen's live oral lag blanks recovery for 5.3 min only", {
  # Acetaminophen's live lag, end to end through getDrugPK and simCpCe.  An
  # ordinary 1 g tablet: its effect site clears the 5 mcg/mL threshold, so the
  # time reported once absorption starts is a real one.
  dd <- getDrugDefaultsGlobal()
  PK <- getDrugPK("acetaminophen", 70, 170, 35, "male",
                  dd[dd$Drug == "acetaminophen", ])
  PK$endCe <- dd$endCe[dd$Drug == "acetaminophen"]
  expect_equal(PK$PK$default$tlag_PO, 5.3)
  DT <- data.frame(Drug = "acetaminophen", Time = 0, Dose = 1000,
                   Units = "mg PO")
  X <- simCpCe(DT, noEvents, PK, 720, TRUE)
  w <- X$wide

  expect_true(all(is.na(w$Recovery[w$Time < 5.3])))
  expect_false(anyNA(w$Recovery[w$Time >= 5.3]))
  expect_gt(max(w$Recovery, na.rm = TRUE), 60)
})


test_that("pendingDoseTimes marks the half-open window a dose has not started in", {
  times <- c(0, 10, 44.99, 45, 60)

  # Nothing lagged, nothing pending, and no vector to carry around
  expect_null(pendingDoseTimes(c(0, 30), c(0, 30), c(1, 1), times))

  # Given at 0, starts at 45: pending up to but NOT including 45, because at
  # that instant the state exists and recovery is computable again
  expect_equal(pendingDoseTimes(0, 45, 1, times),
               c(TRUE, TRUE, TRUE, FALSE, FALSE))

  # A zero dose contributes nothing, so it cannot make anything pending.  Some
  # callers add zero rows purely to put a point on the time line.
  expect_null(pendingDoseTimes(0, 45, 0, times))

  # Two lagged doses union
  expect_equal(pendingDoseTimes(c(0, 50), c(10, 60), c(1, 1),
                                c(0, 5, 10, 50, 55, 60, 70)),
               c(TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))

  # A lagged dose alongside an unlagged one: only the lagged one counts
  expect_equal(pendingDoseTimes(c(0, 0), c(0, 45), c(1, 1), times),
               c(TRUE, TRUE, TRUE, FALSE, FALSE))
})


test_that("a state set with nothing pending carries no mask", {
  S <- matrix(c(1, 0.5), ncol = 1)
  expect_null(recoveryStateSet(c(0, 10), S, 0.1, c(FALSE, FALSE))$pending)
  expect_null(recoveryStateSet(c(0, 10), S, 0.1)$pending)
  expect_equal(recoveryStateSet(c(0, 10), S, 0.1, c(TRUE, FALSE))$pending,
               c(TRUE, FALSE))
  expect_error(recoveryStateSet(c(0, 10), S, 0.1, c(TRUE, FALSE, TRUE)))
  expect_error(recoveryStateSet(c(0, 10), S, 0.1, c(1, 0)))
})


test_that("recoveryFromStates reports NA where a dose is pending, not zero", {
  lambda <- 0.01
  S <- matrix(c(3, 2, 1), ncol = 1)
  times <- c(0, 10, 20)

  plain <- recoveryStateSet(times, S, lambda)
  masked <- recoveryStateSet(times, S, lambda, c(TRUE, FALSE, FALSE))

  got <- recoveryFromStates(masked, 0.5)
  expect_true(is.na(got[1]))
  # The unmasked points are untouched
  expect_equal(got[-1], recoveryFromStates(plain, 0.5)[-1])

  # No threshold still wins: zero means "no threshold set", not "recovered"
  expect_equal(recoveryFromStates(masked, NULL), rep(0, 3))
})


test_that("the mask is carried onto another time line from the left anchor", {
  # The mask turns on when a dose is given and off when it starts absorbing,
  # and both instants are points on the engine's own line, so the value holds
  # from each point forward -- the opposite end from the one lambda is taken
  # at.
  set <- recoveryStateSet(c(0, 10, 20, 30),
                          matrix(c(1, 1, 1, 1), ncol = 1), 0.01,
                          c(TRUE, TRUE, FALSE, FALSE))

  got <- advanceStatesOnto(set, c(0, 5, 10, 15, 20, 25, 30))
  expect_equal(got$pending, c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE))

  # The same line comes straight back
  expect_equal(advanceStatesOnto(set, set$time)$pending, set$pending)
  # And a set with no mask stays without one
  expect_null(advanceStatesOnto(recoveryStateSet(c(0, 10),
                                                 matrix(c(1, 1), ncol = 1), 0.01),
                                c(0, 5, 10))$pending)
})


test_that("combinedRecovery blanks when any one contribution has a dose pending", {
  # A pending parent dose leaves the metabolite drug's row unable to report a
  # time just as a pending dose of its own would: metabolite certain to be
  # formed is missing from the amplitudes.
  k <- 0.01
  times <- c(0, 10, 20)
  decay <- function(A, pending = NULL)
    recoveryStateSet(times, matrix(A * exp(-k * times), ncol = 1), k, pending)

  clean <- combinedRecovery(times, list(decay(5), decay(3)), 1)
  expect_false(anyNA(clean))

  masked <- combinedRecovery(times,
                             list(decay(5), decay(3, c(TRUE, FALSE, FALSE))), 1)
  expect_true(is.na(masked[1]))
  expect_equal(masked[-1], clean[-1])
})


test_that("an engine reports NA across a lag and a real time once it starts", {
  PK <- lagPK("hydromorphone", tlag_IN = 180)
  DT <- data.frame(Drug = "hydromorphone", Time = 0, Dose = 2, Units = "mg IN")
  X <- simCpCe(DT, noEvents, PK, 1440, TRUE)
  w <- X$wide

  expect_false(is.null(X$recoveryStates$pending))
  expect_true(anyNA(w$Recovery))

  # Blank over [0, 180) and nowhere else
  expect_true(all(is.na(w$Recovery[w$Time < 180])))
  expect_false(anyNA(w$Recovery[w$Time >= 180]))

  # And the time reported once it starts is a real one, not zero
  expect_gt(max(w$Recovery, na.rm = TRUE), 60)
})


test_that("without a lag nothing is masked and nothing is missing", {
  # The guard must be invisible on every drug as the library stands.
  PK <- lagPK("hydromorphone")
  expect_equal(PK$PK$default$tlag_IN, 0)
  DT <- data.frame(Drug = "hydromorphone", Time = 0, Dose = 2, Units = "mg IN")
  X <- simCpCe(DT, noEvents, PK, 1440, TRUE)

  expect_null(X$recoveryStates$pending)
  expect_false(anyNA(X$wide$Recovery))
  expect_false(anyNA(X$equiSpace$Recovery))
  expect_gt(X$max$Recovery, 0)
})


test_that("a pending dose blanks even when another dose would give a number", {
  # This is the case that makes the mask more than a zero-guard: the effect
  # site is well above the threshold from the intravenous dose, so the old code
  # reported a confident time -- one that left out 2 mg certain to be absorbed.
  PK <- lagPK("hydromorphone", tlag_IN = 180)
  DT <- data.frame(Drug = "hydromorphone", Time = c(0, 0), Dose = c(2, 2),
                   Units = c("mg", "mg IN"))
  X <- simCpCe(DT, noEvents, PK, 1440, TRUE)
  w <- X$wide

  early <- w[w$Time > 1 & w$Time < 180, ]
  expect_gt(nrow(early), 3)
  expect_gt(max(early$"Effect Site"), PK$endCe)   # well above threshold
  expect_true(all(is.na(early$Recovery)))         # and still no time reported
})


test_that("a zero dose does not blank anything", {
  PK <- lagPK("hydromorphone", tlag_IN = 180)
  DT <- data.frame(Drug = "hydromorphone", Time = c(0, 300), Dose = c(2, 0),
                   Units = "mg IN")
  w <- simCpCe(DT, noEvents, PK, 1440, TRUE)$wide

  expect_true(all(is.na(w$Recovery[w$Time < 180])))
  # The window the zero dose would have opened, 300 to 480, is not blank
  expect_false(anyNA(w$Recovery[w$Time >= 300 & w$Time <= 480]))
})


test_that("the equispaced series keeps the gap instead of interpolating it", {
  # approx() would happily draw a straight line across the one stretch that has
  # no answer, which is the number this whole thing exists to not report.
  PK <- lagPK("hydromorphone", tlag_IN = 180)
  DT <- data.frame(Drug = "hydromorphone", Time = 0, Dose = 2, Units = "mg IN")
  X <- simCpCe(DT, noEvents, PK, 1440, TRUE)

  es <- X$equiSpace
  expect_true(anyNA(es$Recovery))
  expect_true(all(is.na(es$Recovery[es$Time < 174])))
  expect_false(anyNA(es$Recovery[es$Time > 190]))

  # max is taken over what is known, and is finite
  expect_true(is.finite(X$max$Recovery))
  expect_equal(X$max$Recovery, max(X$wide$Recovery, na.rm = TRUE))
})


test_that("a plot window shorter than the lag does not break the interpolator", {
  # The case that matters: a two-hour plot of a dose that starts absorbing at
  # three.  Every point on the equispaced grid is inside the pending window, so
  # there is at most one known value to interpolate between -- and approx()
  # refuses fewer than two.  It used to throw here.
  PK <- lagPK("hydromorphone", tlag_IN = 180)
  DT <- data.frame(Drug = "hydromorphone", Time = 0, Dose = 2, Units = "mg IN")
  X <- simCpCe(DT, noEvents, PK, 120, TRUE)

  expect_true(all(is.na(X$equiSpace$Recovery)))
  expect_true(is.finite(X$max$Recovery))
})


test_that("an all-missing recovery column gives a maximum of zero, not -Inf", {
  # The plot reads max$Recovery to scale the recovery axis and treats zero as
  # "no axis to draw".  -Inf would reach a comparison and break the panel.
  wide <- data.frame(Time = c(0, 10, 20), Plasma = c(0, 1, 2),
                     `Effect Site` = c(0, 1, 2), Recovery = rep(NA_real_, 3),
                     check.names = FALSE)
  out <- finishDrugSeries(wide, list(drug = "x", MEAC = 0), 20, TRUE)

  expect_equal(out$max$Recovery, 0)
  expect_true(all(is.na(out$equiSpace$Recovery)))
})


test_that("a lagged parent blanks the metabolite drug's row through the fold", {
  PK <- lagPK("oxycodone", tlag_PO = 45)
  DT <- data.frame(Drug = "oxycodone", Time = 0, Dose = 40, Units = "mg PO")
  X <- simCpCe(DT, noEvents, PK, 1440, TRUE)

  expect_false(is.null(X$metaboliteRecoveryStates$pending))

  drugs <- list(
    oxycodone = list(drug = "oxycodone", MEAC = 0, endCe = PK$endCe,
                     metaboliteName = "oxymorphone",
                     metaboliteSeries = X$metaboliteSeries,
                     metaboliteRecoveryStates = X$metaboliteRecoveryStates),
    oxymorphone = list(drug = "oxymorphone", MEAC = 0,
                       endCe = getDrugDefaults("oxymorphone")$endCe)
  )
  r <- foldMetabolites(drugs, 1440, TRUE)$oxymorphone$wide

  expect_true(all(is.na(r$Recovery[r$Time < 45])))
  expect_false(anyNA(r$Recovery[r$Time >= 45]))
  expect_gt(max(r$Recovery, na.rm = TRUE), 60)
})


test_that("the plot builds, and breaks the recovery line, across a lag", {
  local_mocked_bindings(outputComments = function(...) {})

  # An intravenous dose first and a lagged intranasal one later, so the blank
  # stretch falls in the MIDDLE of the line.  A gap at the very start leaves
  # one piece, correctly, and would not test the break.
  dd <- getDrugDefaultsGlobal(FALSE)
  doseTable <- data.frame(Drug = "hydromorphone", Time = c(0, 300),
                          Dose = c(2, 2), Units = c("mg", "mg IN"))
  eventTable <- data.frame(Time = 0, Event = "Event")

  newDrugs <- recalculatePK(NULL, dd, doseTable, 50, 70, 171, "male")
  newDrugs$hydromorphone$PK$default$tlag_IN <- 180
  drugs <- processdoseTable(doseTable, eventTable, newDrugs, 1440, TRUE)

  expect_true(anyNA(drugs$hydromorphone$equiSpace$Recovery))

  p <- simulationPlot(
    drugs = drugs, events = eventTable, drugDefaults = dd,
    eventDefaults = getEventDefaultsGlobal(), plotRecovery = TRUE
  )
  # Missing values must not reach ggplot; the line is split into groups
  # instead, so there is no "removed rows containing missing values" warning.
  expect_no_warning(built <- ggplot2::ggplot_build(p$plotObject))
  expect_s3_class(p$plotObject, "ggplot")

  # The recovery line is the last layer, and it is drawn in more than one piece
  recoveryLayer <- built$data[[length(built$data)]]
  expect_gt(length(unique(recoveryLayer$group)), 1)
  expect_false(anyNA(recoveryLayer$y))
})


test_that("a recovery column that is missing throughout still renders", {
  # Flagged by the active-metabolites session: a row that is entirely missing
  # in one series can vanish or break a panel rather than falling back on what
  # it does have.  A plot window shorter than the lag produces exactly that --
  # every equispaced point inside the pending window -- so the panel is drawn
  # with no recovery line and the drug's other series intact.
  local_mocked_bindings(outputComments = function(...) {})

  dd <- getDrugDefaultsGlobal(FALSE)
  doseTable <- data.frame(Drug = "hydromorphone", Time = 0, Dose = 2,
                          Units = "mg IN")
  eventTable <- data.frame(Time = 0, Event = "Event")

  newDrugs <- recalculatePK(NULL, dd, doseTable, 50, 70, 171, "male")
  newDrugs$hydromorphone$PK$default$tlag_IN <- 180
  drugs <- processdoseTable(doseTable, eventTable, newDrugs, 120, TRUE)

  expect_true(all(is.na(drugs$hydromorphone$equiSpace$Recovery)))

  p <- simulationPlot(
    drugs = drugs, events = eventTable, drugDefaults = dd,
    eventDefaults = getEventDefaultsGlobal(), plotRecovery = TRUE
  )
  expect_s3_class(p$plotObject, "ggplot")
  expect_no_warning(ggplot2::ggplot_build(p$plotObject))
})


test_that("a drug with no effect site at all renders with recovery switched on", {
  # The other half of the same blind spot: three drugs now have no effect site,
  # so plotRecovery has to cope with a panel whose effect-site series is
  # missing from end to end.
  local_mocked_bindings(outputComments = function(...) {})

  dd <- getDrugDefaultsGlobal(FALSE)
  doseTable <- data.frame(Drug = "tramadol", Time = 0, Dose = 100,
                          Units = "mg PO")
  eventTable <- data.frame(Time = 0, Event = "Event")

  newDrugs <- recalculatePK(NULL, dd, doseTable, 50, 70, 171, "male")
  drugs <- processdoseTable(doseTable, eventTable, newDrugs, 1440, TRUE)

  p <- simulationPlot(
    drugs = drugs, events = eventTable, drugDefaults = dd,
    eventDefaults = getEventDefaultsGlobal(), plotRecovery = TRUE
  )
  expect_s3_class(p$plotObject, "ggplot")
  expect_no_warning(ggplot2::ggplot_build(p$plotObject))
})


test_that("gabapentin's own lag blanks a plasma threshold, dose by dose", {
  # The live case.  No effect site, so time until threshold is timed on the
  # plasma; with a threshold of 2 mcg/mL set, each oral dose leaves the
  # readout missing for the 18.66 min before its absorption starts, and a real
  # time everywhere else -- including the stretch before the second dose, when
  # the plasma is already below the threshold and zero would be the old,
  # wrong answer.
  dd <- getDrugDefaultsGlobal()
  PK <- getDrugPK("gabapentin", 70, 171, 50, "male", dd[dd$Drug == "gabapentin", ])
  PK$endCe <- 2
  lag <- 0.311 * 60
  expect_equal(PK$PK$default$tlag_PO, lag)
  DT <- data.frame(Drug = "gabapentin", Time = c(0, 720), Dose = c(600, 300),
                   Units = "mg PO")
  X <- simCpCe(DT, noEvents, PK, 1440, TRUE)
  w <- X$wide

  pending <- (w$Time < lag) | (w$Time >= 720 & w$Time < 720 + lag)
  expect_true(all(is.na(w$Recovery[pending])))
  expect_false(anyNA(w$Recovery[!pending]))
  expect_gt(w$Recovery[w$Time == lag], 60)
})


test_that("pregabalin's lag blanks an effect-site threshold, dose by dose", {
  # Pregabalin has an effect site, so time until threshold is timed on it.
  # With a threshold of 1 mcg/mL the readout is missing for the 19.2 min after
  # each oral dose and a real time everywhere else, including the stretch
  # before the second dose, when the effect site is already below 1.
  dd <- getDrugDefaultsGlobal()
  PK <- getDrugPK("pregabalin", 70, 170, 50, "male", dd[dd$Drug == "pregabalin", ])
  PK$endCe <- 1
  lag <- 0.32 * 60
  expect_equal(PK$PK$default$tlag_PO, lag)
  expect_gt(PK$PK$default$ke0, 0)
  DT <- data.frame(Drug = "pregabalin", Time = c(0, 720), Dose = c(150, 75),
                   Units = "mg PO")
  X <- simCpCe(DT, noEvents, PK, 1440, TRUE)
  w <- X$wide

  pending <- (w$Time < lag) | (w$Time >= 720 & w$Time < 720 + lag)
  expect_true(all(is.na(w$Recovery[pending])))
  expect_false(anyNA(w$Recovery[!pending]))
  expect_gt(w$Recovery[w$Time == lag], 60)
})
