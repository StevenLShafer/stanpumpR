# Verapamil (R/drugs_verapamil.R): the mean of Kates 1981's twelve
# three-compartment fits, checked against Eichelbaum 1984.  Drafted by Claude
# Code, 2026-10-19.
#
# The expectations are built from Tables I-III of Kates 1981 independently of
# the drug file's derivation.

k <- VERAPAMIL_KATES

test_that("each patient's printed Vc k10 and Vdss follow from the rate constants", {
  # Table II's derived columns, patient 2 with the corrected 9.272 L
  vck10 <- c(1.00, 0.63, 1.01, 1.93, 1.09, 1.40, 0.55, 0.68, 0.75, 0.60, 1.16, 1.07)
  vdss  <- c(2.06, 4.36, 2.22, 3.72, 1.54, 3.51, 4.01, 2.86, 4.81, 5.93, 4.11, 2.81)
  expect_lt(max(abs(k$Vc * k$k10 - vck10)), 0.011)
  ss <- k$Vc * (1 + k$k12 / k$k21 + k$k13 / k$k31) / k$weight
  expect_lt(max(abs(ss - vdss)), 0.04)
  # The misprint: 92.72 L would make patient 2's Vc k10 6.30 and Vdss 43.5
  expect_equal(round(92.72 * k$k10[2], 2), 6.30)
  expect_gt(92.72 * (1 + k$k12[2] / k$k21[2] + k$k13[2] / k$k31[2]) / 58, 40)
  # and it is in the printed mean Vc, 28.6 L, which the correction makes 21.6
  expect_equal(round(mean(replace(k$Vc, 2, 92.72)), 1), 28.6)
})

test_that("the typical model reproduces the paper's own mean clearance and Vss", {
  p <- verapamilTypicalPerKg()
  # "average values for clearance and steady-state volume of distribution are
  # 12.5 +/- 5.4 ml/min/kg and 3.50 +/- 1.3 l/kg"
  expect_equal(round(p[["cl1"]] * 1000, 1), 12.5)
  expect_equal(round(p[["v1"]] + p[["v2"]] + p[["v3"]], 2), 3.49)
  set <- getDrugPK("verapamil", 81, 175, 60, "male", adjustToFFM = FALSE)$PK$default
  expect_equal_rounded(p[["cl1"]] * 81, set$cl1)
  expect_equal_rounded(p[["v1"]] * 81, set$v1)
  expect_equal_rounded(p[["v3"]] * 81, set$v3)
})

test_that("Eichelbaum 1984's racemic subjects, which the model never saw, agree", {
  p <- verapamilTypicalPerKg()
  # Table 2 (+/-) means: CL 14.27 mL/min/kg, Vss 3.40 L/kg
  expect_lt(abs(p[["cl1"]] * 1000 / 14.27 - 1), 0.15)
  expect_lt(abs((p[["v1"]] + p[["v2"]] + p[["v3"]]) / 3.40 - 1), 0.05)
})

test_that("Eichelbaum's Table 1 says its doses were the hydrochloride", {
  # Subject 1, racemate, 10 mg: Table 1's coefficients (ng/mL) and exponents
  # (/min) give a clearance 8% above Table 2's 1030 mL/min, the
  # hydrochloride-to-base ratio
  C <- c(454.8, 55.4, 25.4); L <- c(0.45, 0.034, 0.0040)
  cl <- 10 * 1e6 / sum(C / L)
  expect_equal(round(cl / 1030, 2), round(491.07 / 454.60, 2))
})

test_that("weight is linear, or the pharmacokinetic weight", {
  p <- verapamilTypicalPerKg()
  size <- pkSizeFactors(110, 175, 60, "male", TRUE)
  on <- verapamil(110, 175, 60, "male", adjustToFFM = TRUE)$PK$default
  expect_equal_rounded(p[["cl1"]] * size$pkWeight, on$cl1)
})

test_that("verapamil is a vasoactive and resuscitation drug with no band", {
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "verapamil", ]
  expect_identical(row$Category, VASOACTIVE_DRUG_CATEGORY)
  expect_equal(row$MEAC, 0)
  PK <- getDrugPK("verapamil", 81, 175, 60, "male")
  expect_equal(PK$tPeak, 4)
})

test_that("the effect site peaks 4 min after a bolus (the label's 3-5 min)", {
  PK <- getDrugPK("verapamil", 70, 170, 60, "male")
  expect_equal(PK$tPeak, 4)
  pk <- PK$PK$default
  coef <- c(pk$p_coef_bolus_l1, pk$p_coef_bolus_l2, pk$p_coef_bolus_l3)
  lambda <- c(pk$lambda_1, pk$lambda_2, pk$lambda_3)
  expect_equal(effectSitePeakTime(coef, lambda, pk$ke0), 4, tolerance = 1e-4)
})

test_that("a bolus and an infusion simulate to the eigen-solution of the model", {
  # End to end through simulateDrugsWithCovariates() (adversarial review,
  # 2026-10-27: this file checked the parameters but never ran the curve).
  # 10 mg at 0 with 6 mg/hr for an hour; ng/mL = mg/L x 1000.
  set <- getDrugPK("verapamil", 81, 175, 60, "male", adjustToFFM = FALSE)$PK$default
  wide <- simulateDrugsWithCovariates(
    data.frame(Drug = "verapamil", Time = c(0, 0, 60), Dose = c(10, 6, 0),
               Units = c("mg", "mg/hr", "mg/hr")),
    data.frame(Time = double(), Event = character()),
    81, 175, 60, "male", 600, FALSE, adjustToFFM = FALSE)$verapamil$wide
  # Every point of the simulated time line after the dose
  times <- wide$Time[wide$Time > 0]
  got <- wide$Plasma[wide$Time > 0]
  expect_gt(length(times), 20)
  ref <- 1000 * mammillaryReference(set, times, bolus = 10, rate = 6 / 60, tStop = 60)
  expect_lt(max(abs(got / ref - 1)), 1e-6)
})
