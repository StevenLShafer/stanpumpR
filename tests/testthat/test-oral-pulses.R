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
