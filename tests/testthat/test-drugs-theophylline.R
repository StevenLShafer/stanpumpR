# Theophylline (R/drugs_theophylline.R): one compartment from the FDA label
# (Theophylline in 5% Dextrose Injection, NDA 019826), oral absorption from
# Taylor 1983, aminophylline converted by its dihydrate stoichiometry.
# Drafted by Claude Code, 2026-11-12.
#
# The expectations are built from the printed numbers, not from the drug file.

events <- data.frame(Time = double(), Event = character())

simulateTheo <- function(dose, weight = 70, age = 35, maximum = 4320) {
  simulateDrugsWithCovariates(dose, events, weight, 170, age, "male", maximum,
                              FALSE, adjustToFFM = FALSE)$theophylline$wide
}

test_that("the adult has the label's volume and Table V's clearance", {
  set <- theophylline(70, 170, 35, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(set$v1, 0.45 * 70)
  # 0.4 mg/kg/h holds 10 mg/L, so CL = 0.04 L/kg/h
  expect_equal(set$cl1, 0.04 * 70 / 60)
  expect_equal(c(set$v2, set$v3, set$cl2, set$cl3), c(1, 1, 0, 0))
  # Taylor 1983, Table 1: the mean of the fifteen individual ka, 2.31 /h; F 1
  expect_equal(set$ka_PO, 2.31 / 60)
  expect_equal(set$bioavailability_PO, 1)
  # Within the label's Table I: 0.65 mL/kg/min measured, 8.7 h half-life
  expect_equal(set$cl1 / 70 * 1000, 0.667, tolerance = 1e-3)
  expect_equal(log(2) * set$v1 / set$cl1 / 60, 7.8, tolerance = 0.01)
})

test_that("clearance follows Table V's rates across age", {
  cl <- function(age) theophylline(20, 110, age, "male", adjustToFFM = FALSE)$PK$default$cl1 * 60 / 20
  expect_equal(cl(10 / 365.25), (1 / 12) / 10)           # up to 24 days
  expect_equal(cl(30 / 365.25), (1.5 / 12) / 10)         # 24 days to 6 weeks
  weeks <- 20
  expect_equal(cl(weeks * 7 / 365.25), (0.008 * weeks + 0.21) / 10)
  expect_equal(cl(5), 0.08)
  expect_equal(cl(10), 0.07)
  expect_equal(cl(14), 0.05)
  expect_equal(cl(60), 0.04)
  expect_equal(cl(61), 0.03)
  # finite everywhere, birth to 90
  for (age in c(0, 0.5, 1, 9, 12, 16, 90)) expect_true(is.finite(cl(age)) && cl(age) > 0)
})

test_that("0.4 mg/kg/h reaches the label's 10 mcg/mL in a nonsmoking adult", {
  wide <- simulateTheo(data.frame(Drug = "theophylline", Time = 0, Dose = 0.4,
                                  Units = "mg/kg/hr"))
  expect_equal(tail(wide$Plasma, 1), 10, tolerance = 0.005)
})

test_that("aminophylline carries 0.7894 of its mass as theophylline", {
  expect_equal(AMINOPHYLLINE_THEOPHYLLINE_FRACTION, 0.7894, tolerance = 1e-4)
  expect_true(all(aminophyllineUnits %in% allUnits))
  expect_true(all(doseRoute(aminophyllineUnits) == ROUTE_IV))
  theo <- simulateTheo(data.frame(Drug = "theophylline", Time = 0, Dose = 400,
                                  Units = "mg"), maximum = 720)
  amino <- simulateTheo(data.frame(Drug = "theophylline", Time = 0,
                                   Dose = 400 / AMINOPHYLLINE_THEOPHYLLINE_FRACTION,
                                   Units = "mg aminophylline"), maximum = 720)
  expect_equal(amino$Plasma, theo$Plasma)
  # a bolus of 5.6 mg/kg aminophylline is 4.42 mg/kg of theophylline, which
  # in 0.45 L/kg is 9.8 mg/L at time zero
  bolus <- simulateTheo(data.frame(Drug = "theophylline", Time = 0, Dose = 5.6,
                                   Units = "mg/kg aminophylline"), maximum = 60)
  expect_equal(max(bolus$Plasma), 5.6 * 0.7894 / 0.45, tolerance = 1e-3)
})

test_that("a 5 mg/kg oral dose peaks near Taylor 1983's measured 9.7 mcg/mL", {
  # Taylor 1983 Table 1 (morning): Cmax 9.7 +/- 0.6 mcg/mL, time to peak
  # 1.69 h, in young nonsmokers whose own clearance (0.053 L/kg/h) was higher
  # than the label's 0.04; the model, at the label's clearance, peaks within
  # the study's standard error
  wide <- simulateTheo(data.frame(Drug = "theophylline", Time = 0, Dose = 5,
                                  Units = "mg/kg PO"), maximum = 1440)
  expect_equal(max(wide$Plasma), 9.7, tolerance = 0.06)
  expect_true(wide$Time[which.max(wide$Plasma)] > 60 &&
                wide$Time[which.max(wide$Plasma)] < 120)
})

test_that("with the switch on, the weight is the pharmacokinetic weight", {
  size <- pkSizeFactors(120, 170, 35, "male", TRUE)
  set <- theophylline(120, 170, 35, "male")$PK$default
  expect_equal(set$v1, 0.45 * size$pkWeight)
  expect_equal(set$cl1, 0.04 * size$pkWeight / 60)
  expect_lt(set$v1, 0.45 * 120)
})

test_that("the defaults row draws the label's 10-15 mcg/mL band", {
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "theophylline", ]
  expect_equal(c(row$Lower, row$Upper, row$Typical, row$endCe), c(10, 15, 12.5, 20))
  expect_identical(as.character(row$Concentration.Units), "mcg")
  expect_equal(row$MEAC, 0)
})
