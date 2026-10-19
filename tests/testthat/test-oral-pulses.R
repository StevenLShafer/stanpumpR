# Pulsed oral release (R/oral-pulses.R): an XR dose given as fixed fractions
# at fixed delays, each absorbed as the drug's default oral form.

pulses <- list(XR = list(fraction = c(0.5, 0.5), delay = c(0, 240)))

doseRows <- function(time, dose, units) {
  data.frame(Drug = "x", Time = time, Dose = dose, Units = units)
}


test_that("an XR dose becomes its pulses, as plain oral doses", {
  out <- expandOralPulses(doseRows(60, 20, "mg PO XR"), pulses, 1440)
  expect_equal(out$Time, c(60, 300))
  expect_equal(out$Dose, c(10, 10))
  expect_equal(out$Units, c("mg PO", "mg PO"))
  # The amount given is unchanged; only its timing is
  expect_equal(sum(out$Dose), 20)
})


test_that("other doses pass through untouched", {
  dose <- doseRows(c(0, 30, 60), c(5, 20, 1), c("mg PO", "mg PO XR", "mg"))
  out <- expandOralPulses(dose, pulses, 1440)
  expect_equal(nrow(out), 4)
  expect_equal(out$Units[out$Time == 0], "mg PO")
  expect_equal(out$Units[out$Time == 60], "mg")
  # and a drug without pulses, or a formulation it does not pulse, is unchanged
  expect_identical(expandOralPulses(dose, NULL, 1440), dose)
  liquid <- doseRows(0, 5, "mg PO liquid")
  expect_identical(expandOralPulses(liquid, pulses, 1440), liquid)
})


test_that("a pulse at or after the end of the plot is not given", {
  out <- expandOralPulses(doseRows(1300, 20, "mg PO XR"), pulses, 1440)
  expect_equal(out$Time, 1300)
  expect_equal(out$Dose, 10)
})


test_that("scheduled XR doses are expanded first, then pulsed", {
  dose <- doseRows(0, 20, "mg PO XR qd")
  sched <- expandScheduledDoses(dose, 3 * 1440)$dose
  expect_equal(sched$Units, rep("mg PO XR", 3))
  out <- expandOralPulses(sched, pulses, 3 * 1440)
  expect_equal(out$Time, c(0, 240, 1440, 1680, 2880, 3120))
  expect_true(all(out$Units == "mg PO"))
  expect_true(all(out$Dose == 10))
})


test_that("the XR units are oral, and may carry a frequency", {
  expect_true("XR" %in% ORAL_FORMULATIONS)
  expect_equal(doseRoute(c("mg PO XR", "mg PO XR qd")), c(ROUTE_PO, ROUTE_PO))
  expect_equal(doseFormulation(c("mg PO XR", "mg PO XR qd", "mg PO")), c("XR", "XR", NA))
  expect_true(all(c("mg PO XR", "mg/kg PO XR") %in% allUnits))
  expect_true("mg PO XR qd" %in% scheduledUnits)
})


test_that("malformed blocks are refused", {
  expect_null(validateOralPulses(NULL, "x"))
  expect_identical(validateOralPulses(pulses, "x"), pulses)
  expect_error(validateOralPulses(list(SR = pulses$XR), "x"), "not one of")
  expect_error(validateOralPulses(list(XR = list(fraction = c(0.5, 0.4), delay = c(0, 240))), "x"),
               "summing to 1")
  expect_error(validateOralPulses(list(XR = list(fraction = c(0.5, 0.5), delay = 0)), "x"),
               "summing to 1")
  expect_error(validateOralPulses(list(XR = list(fraction = c(0.5, 0.5), delay = c(0, -1))), "x"),
               "summing to 1")
  expect_error(validateOralPulses(list(list(fraction = 1, delay = 0)), "x"), "named list")
})


# ---------------------------------------------------------------------------
# The reserved default oral form (ORAL_PULSE_DEFAULT_FORM): the pulses apply
# to every plain oral row and the unit is left exactly as entered.  This is
# the discretisation path, for a published input shape the engine lacks
# (hydrochlorothiazide's Weibull); R/oral-pulses.R says how it differs from a
# product's own release.
# ---------------------------------------------------------------------------

defaultPulses <- list(default = list(fraction = c(0.25, 0.25, 0.25, 0.25),
                                     delay = c(0, 15, 30, 45)))

test_that("the default form pulses every plain oral dose, keeping its unit", {
  out <- expandOralPulses(doseRows(60, 20, "mg PO"), defaultPulses, 1440)
  expect_equal(out$Time, c(60, 75, 90, 105))
  expect_equal(out$Dose, rep(5, 4))
  expect_equal(out$Units, rep("mg PO", 4))   # not rewritten: it is already plain
  expect_equal(sum(out$Dose), 20)
  # mg/kg PO is a plain oral unit too, and keeps its own spelling
  perKg <- expandOralPulses(doseRows(0, 0.4, "mg/kg PO"), defaultPulses, 1440)
  expect_equal(perKg$Units, rep("mg/kg PO", 4))
  expect_equal(sum(perKg$Dose), 0.4)
})

test_that("the default form leaves every non-oral row alone", {
  # Times chosen so that no pulse lands on another row's time, which would
  # make the row-by-unit check below ambiguous rather than wrong.
  dose <- doseRows(c(0, 200, 210, 220, 230),
                   c(5, 1, 2, 3, 4),
                   c("mg PO", "mg", "mg SL", "mg IM", "mcg/kg/min"))
  out <- expandOralPulses(dose, defaultPulses, 1440)
  # the one oral row became four; the other four are untouched
  expect_equal(nrow(out), 4 + 4)
  expect_equal(out$Units[out$Time >= 200],
               c("mg", "mg SL", "mg IM", "mcg/kg/min"))
  expect_equal(out$Dose[out$Time >= 200], c(1, 2, 3, 4))
  expect_equal(sum(out$Dose[out$Units == "mg PO"]), 5)
  expect_equal(out$Time[out$Units == "mg PO"], c(0, 15, 30, 45))
})

test_that("a scheduled plain oral dose is expanded first, then pulsed", {
  sched <- expandScheduledDoses(doseRows(0, 25, "mg PO qd"), 2 * 1440)$dose
  expect_equal(sched$Units, rep("mg PO", 2))
  out <- expandOralPulses(sched, defaultPulses, 2 * 1440)
  expect_equal(out$Time, c(0, 15, 30, 45, 1440, 1455, 1470, 1485))
  expect_true(all(out$Dose == 6.25))
  expect_true(all(out$Units == "mg PO"))
})

test_that("a default-form pulse past the end of the plot is not given", {
  out <- expandOralPulses(doseRows(1400, 20, "mg PO"), defaultPulses, 1440)
  expect_equal(out$Time, c(1400, 1415, 1430))
  expect_equal(sum(out$Dose), 15)
})

test_that("a formulation unit is not also pulsed by the default form", {
  # An "mg PO XR" row IS an oral row, so the two keys would read the same
  # rows.  The block is refused rather than resolved by declaration order.
  expect_error(validateOralPulses(c(defaultPulses, pulses), "x"),
               "cannot be declared alongside")
  expect_error(validateOralPulses(c(pulses, defaultPulses), "x"),
               "cannot be declared alongside")
  # With only the default key declared, a formulation row passes through: the
  # drug has not said what its XR product does.
  xr <- doseRows(0, 20, "mg PO XR")
  expect_identical(expandOralPulses(xr, defaultPulses, 1440), xr)
})

test_that("the reserved name is accepted and is not a formulation", {
  expect_identical(validateOralPulses(defaultPulses, "x"), defaultPulses)
  expect_false(ORAL_PULSE_DEFAULT_FORM %in% ORAL_FORMULATIONS)
  # It never appears in a dose unit, so it cannot collide with one
  expect_false(any(grepl(paste0(" ", ORAL_PULSE_DEFAULT_FORM, "( |$)"), allUnits)))
  expect_true(is.na(doseFormulation(paste("mg PO", ORAL_PULSE_DEFAULT_FORM))))
  # and it is still validated like any other entry
  expect_error(validateOralPulses(list(default = list(fraction = c(0.5, 0.4),
                                                      delay = c(0, 10))), "x"),
               "summing to 1")
})

test_that("the default form leaves a standing oral rate alone", {
  # "mg/day PO" (poRateUnits) is a continuous input on the apparent oral
  # parameters, classified as an infusion.  It has no release to discretise,
  # and splitting it into fractions at delays would turn one rate into several
  # starting at different times.  It passes through; the amount rows beside it
  # are still pulsed.
  rate <- doseRows(0, 200, "mg/day PO")
  expect_identical(expandOralPulses(rate, defaultPulses, 1440), rate)
  mixed <- doseRows(c(0, 600), c(200, 20), c("mg/day PO", "mg PO"))
  out <- expandOralPulses(mixed, defaultPulses, 1440)
  expect_equal(nrow(out), 1 + 4)
  expect_equal(out$Units[1], "mg/day PO")
  expect_equal(out$Dose[1], 200)
  expect_equal(sum(out$Dose[out$Units == "mg PO"]), 20)
  # and the rate row is untouched by a formulation block too, as before
  expect_identical(expandOralPulses(rate, pulses, 1440), rate)
})
