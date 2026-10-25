# Vasopressin (R/drugs_vasopressin.R): Fabian 1969's one-compartment medians,
# dosed in units and plotted in mcU/mL.  Drafted by Claude Code, 2026-10-22.

noEvents <- data.frame(Time = double(), Event = character())

test_that("the model is Fabian 1969's median volume and clearance", {
  set <- getDrugPK("vasopressin", 70, 175, 21, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(set$v1, 8.5)
  expect_equal(set$cl1, 1.0)
  # One compartment: the half-life is 5.9 min, inside the 95% limits of the
  # median 5.6 (3.9-9.5)
  thalf <- log(2) / set$lambda_1
  expect_equal(round(thalf, 1), 5.9)
  expect_gt(thalf, 3.9)
  expect_lt(thalf, 9.5)
  expect_identical(getDrugPK("vasopressin", 70, 175, 21, "male")$Concentration.Units, "mcU")
})

test_that("Fabian's infusion reaches the 120 mcU/mL measured at equilibrium", {
  # 120 mU/min for 20 min: the plateau is 120 mcU/mL, 90% reached by 20 min,
  # as the authors designed it
  wide <- simulateDrugsWithCovariates(
    data.frame(Drug = "vasopressin", Time = c(0, 20), Dose = c(0.12, 0),
               Units = "U/min"),
    noEvents, 70, 175, 21, "male", 60, FALSE, adjustToFFM = FALSE)$vasopressin$wide
  end <- wide$Plasma[wide$Time == 20][1]
  expect_equal(end, 120 * (1 - exp(-20 / 8.5)), tolerance = 1e-3)
  expect_gt(end / 120, 0.9)
})

test_that("0.01 U/min gives Landry 1997's concentration in septic shock", {
  # Landry: about 30 pg/mL on 0.01 U/min, the patients' own 3.1 pg/mL
  # included; 1 mcU/mL is 2.5 pg/mL at 400 U/mg
  wide <- simulateDrugsWithCovariates(
    data.frame(Drug = "vasopressin", Time = 0, Dose = 0.01, Units = "U/min"),
    noEvents, 70, 175, 60, "male", 120, FALSE, adjustToFFM = FALSE)$vasopressin$wide
  plateau <- tail(wide$Plasma, 1)
  expect_equal(plateau, 10, tolerance = 1e-3)
  expect_lt(abs((plateau * 2.5 + 3.1) / 30 - 1), 0.1)
})

test_that("units and hours and per kg are the same input", {
  sim <- function(dose, units)
    simulateDrugsWithCovariates(
      data.frame(Drug = "vasopressin", Time = 0, Dose = dose, Units = units),
      noEvents, 80, 175, 60, "male", 60, FALSE)$vasopressin$wide$Plasma
  expect_equal(sim(0.03, "U/min"), sim(1.8, "U/hr"))
  expect_equal(sim(0.03, "U/min"), sim(0.03 / 80, "U/kg/min"))
  expect_equal(sim(0.03, "U/min"), sim(1.8 / 80, "U/kg/hr"))
  # a 1 U bolus starts at 1000 mU in 8.5 L
  bolus <- simulateDrugsWithCovariates(
    data.frame(Drug = "vasopressin", Time = 0, Dose = 1, Units = "U"),
    noEvents, 70, 175, 21, "male", 30, FALSE, adjustToFFM = FALSE)$vasopressin$wide
  expect_equal(bolus$Plasma[1], 1000 / 8.5, tolerance = 1e-6)
})

test_that("a mass unit is refused", {
  expect_error(
    simulateDrugsWithCovariates(
      data.frame(Drug = "vasopressin", Time = 0, Dose = 1, Units = "mcg"),
      noEvents, 70, 175, 60, "male", 30, FALSE),
    "units")
})

test_that("pregnancy multiplies the clearance by Davison 1993's 3.4 / 0.67", {
  off <- getDrugPK("vasopressin", 65, 165, 30, "female")
  on  <- getDrugPK("vasopressin", 65, 165, 30, "female", pregnant = TRUE)
  expect_equal(VASOPRESSIN_PREGNANCY_CL, 3.4 / 0.67)
  expect_equal(on$PK$default$cl1 / off$PK$default$cl1, 3.4 / 0.67)
  expect_equal(on$PK$default$v1, off$PK$default$v1)
  expect_match(on$reference, "Davison")
  expect_no_match(off$reference, "pregnancy")
})

test_that("the effect site peaks 3 min after a bolus (provisional clinical)", {
  PK <- getDrugPK("vasopressin", 70, 170, 60, "male")
  expect_equal(PK$tPeak, VASOPRESSIN_TPEAK)
  expect_equal(VASOPRESSIN_TPEAK, 3)
  pk <- PK$PK$default
  coef <- c(pk$p_coef_bolus_l1, pk$p_coef_bolus_l2, pk$p_coef_bolus_l3)
  lambda <- c(pk$lambda_1, pk$lambda_2, pk$lambda_3)
  expect_equal(effectSitePeakTime(coef, lambda, pk$ke0), 3, tolerance = 1e-4)
  expect_match(PK$reference, "provisional clinical observation")
})

test_that("vasopressin is a vasoactive and resuscitation drug with no band", {
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "vasopressin", ]
  expect_identical(row$Category, VASOACTIVE_DRUG_CATEGORY)
  expect_equal(row$MEAC, 0)
  expect_equal(row$Lower, 0)
  expect_true(all(unlist(row$Units) %in% ivPotencyUnits))
  expect_true(all(ivPotencyUnits %in% allUnits))
  expect_false(any(isRateUnit("U")))
  expect_true(all(isRateUnit(setdiff(ivPotencyUnits, "U"))))
  expect_true(all(doseRoute(ivPotencyUnits) == ROUTE_IV))
})
