# Milrinone (R/drugs_milrinone.R): Bailey 1994's three compartments per
# kilogram after cardiac surgery, with Butterworth 1995's time to peak effect.
# Drafted by Claude Code, 2026-10-19.

events <- data.frame(Time = double(), Event = character())

test_that("the parameters are Table 2's, per kilogram", {
  set <- milrinone(80, 175, 61, "male", adjustToFFM = FALSE)$PK$default
  expect_equal_rounded(0.102 * 80, set$v1)
  expect_equal_rounded(0.198 * 80, set$v2)
  expect_equal_rounded(1.398 * 80, set$v3)
  expect_equal_rounded(0.00188 * 80, set$cl1)
  expect_equal_rounded(0.01440 * 80, set$cl2)
  expect_equal_rounded(0.00291 * 80, set$cl3)
})

test_that("they reproduce Table 2's triexponential for 50 mcg/kg", {
  PK <- getDrugPK("milrinone", 1, 175, 61, "male", adjustToFFM = FALSE)$PK$default
  # alpha .2478, beta .0150, delta .0008 /min as printed
  expect_equal(round(PK$lambda_1, 3), 0.247)
  expect_equal(round(PK$lambda_2, 4), 0.0150)
  expect_equal(round(PK$lambda_3, 4), 0.0008)
  # A 365.7, B 110.8, C 13.2 ng/mL; per mcg/kg the coefficients are mcg/L
  coef <- 50 * c(PK$p_coef_bolus_l1, PK$p_coef_bolus_l2, PK$p_coef_bolus_l3)
  expect_equal(round(coef / c(365.7, 110.8, 13.2), 1), c(1, 1, 1))
  # Vss 1.70 L/kg and a terminal half-life of 15 h
  set <- milrinone(1, 175, 61, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(round(set$v1 + set$v2 + set$v3, 2), 1.70)
  expect_equal(round(log(2) / PK$lambda_3 / 60, 1), 15.3)
})

test_that("50 mcg/kg then 0.5 mcg/kg/min holds above 100 ng/mL, as in Figure 5", {
  wide <- simulateDrugsWithCovariates(
    data.frame(Drug = "milrinone", Time = c(0, 0), Dose = c(50, 0.5),
               Units = c("mcg/kg", "mcg/kg/min")),
    events, 87, 175, 61, "male", 1000, FALSE, adjustToFFM = FALSE)$milrinone$wide
  at <- function(t) stats::approx(wide$Time, wide$Plasma, t, ties = mean)$y
  expect_equal(round(at(360)), 141)
  expect_equal(round(at(960)), 186)
  expect_true(all(wide$Plasma[wide$Time >= 30] > 100))
})

test_that("the effect site peaks 5 minutes after a bolus (Butterworth 1995)", {
  PK <- getDrugPK("milrinone", 80, 175, 61, "male")
  expect_equal(PK$tPeak, 5)
  expect_gt(PK$PK$default$ke0, 0)
  wide <- simulateDrugsWithCovariates(
    data.frame(Drug = "milrinone", Time = 0, Dose = 50, Units = "mcg/kg"),
    events, 80, 175, 61, "male", 30, FALSE)$milrinone$wide
  peak <- wide$Time[which.max(wide$`Effect Site`)]
  expect_lt(abs(peak - 5), 0.5)
})

test_that("weight scales every parameter, or the pharmacokinetic weight", {
  size <- pkSizeFactors(116, 181, 50, "male", TRUE)
  on <- milrinone(116, 181, 50, "male", adjustToFFM = TRUE)$PK$default
  off <- milrinone(116, 181, 50, "male", adjustToFFM = FALSE)$PK$default
  expect_equal_rounded(0.102 * size$pkWeight, on$v1)
  expect_equal_rounded(0.00188 * size$pkWeight, on$cl1)
  expect_equal_rounded(0.102 * 116, off$v1)
})

test_that("milrinone carries the 100-300 ng/mL target range", {
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "milrinone", ]
  expect_identical(row$Category, VASOACTIVE_DRUG_CATEGORY)
  expect_equal(c(row$Lower, row$Typical, row$Upper), c(100, 167, 300))
  expect_equal(row$MEAC, 0)
})
