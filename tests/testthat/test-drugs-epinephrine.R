# Epinephrine (R/drugs_epinephrine.R): Abboud 2009's one-compartment model in
# adult septic shock.  Drafted by Claude Code, 2026-10-18.
#
# The expectations are built from the published numbers (Table 3 of the full
# text, PMC2750169) independently of the drug file.

# Abboud I et al., Crit Care 2009;13:R120
CLH  <- 127     # L/h at 70 kg, SAPS II 50
V    <- 7.9     # L
R0   <- 43.5    # nmol/h, endogenous input
MW   <- 183.2   # g/mol, epinephrine base

events <- data.frame(Time = double(), Event = character())

simulateEpinephrine <- function(dose, maximum = 120, adjustToFFM = FALSE,
                                weight = 70, height = 170, age = 64,
                                sex = "male") {
  simulateDrugsWithCovariates(dose, events, weight, height, age, sex, maximum,
                              FALSE, adjustToFFM = adjustToFFM)$epinephrine$wide
}

test_that("the reference patient has the published clearance and volume", {
  PK <- getDrugPK("epinephrine", 70, 170, 64, "male", adjustToFFM = FALSE)
  set <- PK$PK$default
  expect_equal_rounded(CLH / 60, set$cl1)
  expect_equal_rounded(V, set$v1)
  expect_equal(set$cl2, 0)
  expect_equal(set$cl3, 0)
  # The half-life of the final model: ln 2 x 7.9 / 127 h = 2.587 min
  expect_equal_rounded(2.5870, round(log(2) * V / CLH * 60, 4))
  expect_equal_rounded(log(2) * V / CLH * 60, log(2) / set$lambda_1)
  expect_identical(PK$Concentration.Units, "ng")
  expect_equal(PK$tPeak, 1)
  expect_gt(set$ke0, 0)
  expect_equal(PK$MEAC, 0)
  expect_equal(PK$typical, 0)
})

test_that("the basic model's 3.5-minute half-life is the paper's", {
  # Results: basic model CL 108 L/h and V 9.1 L, "The corresponding half-life
  # was 3.5 minutes".  A check that the paper's clearances are per hour.
  expect_equal(round(log(2) * 9.1 / 108 * 60, 1), 3.5)
})

test_that("the endogenous input reproduces the measured baseline", {
  # R0 / CL is the baseline the model implies; the paper's median measured
  # pre-infusion concentration is 0.34 nmol/L (Table 2)
  expect_equal(round(R0 / CLH, 2), 0.34)
  # In ng/mL that is about 0.063, under 1% of the median infusion level
  # (95.8 nmol/L), which is why it is not plotted
  expect_equal_rounded(0.0627, round(R0 / CLH * MW / 1000, 4))
  expect_lt((R0 / CLH) / 95.8, 0.01)
})

test_that("weight enters clearance as (BW/70)^0.60 and SAPS II stays at 50", {
  light <- epinephrine(50, 165, 64, "female", adjustToFFM = FALSE)$PK$default
  heavy <- epinephrine(100, 180, 64, "male", adjustToFFM = FALSE)$PK$default
  expect_equal_rounded(CLH / 60 * (50 / 70)^0.60, light$cl1)
  expect_equal_rounded(CLH / 60 * (100 / 70)^0.60, heavy$cl1)
  # The volume has no size term in the source
  expect_equal_rounded(V, light$v1)
  expect_equal_rounded(V, heavy$v1)
})

test_that("with the switch on, clearance uses the pharmacokinetic weight and V the volume factor", {
  size <- pkSizeFactors(120, 170, 50, "male", TRUE)
  set <- epinephrine(120, 170, 50, "male", adjustToFFM = TRUE)$PK$default
  expect_equal_rounded(CLH / 60 * (size$pkWeight / 70)^0.60, set$cl1)
  expect_equal_rounded(V * size$volume, set$v1)
})

test_that("a steady infusion plateaus at rate / clearance", {
  # 0.15 mcg/kg/min in a 70 kg patient, the study's starting rate
  dose <- data.frame(Drug = "epinephrine", Time = c(0, 90), Dose = c(0.15, 0),
                     Units = "mcg/kg/min")
  wide <- simulateEpinephrine(dose)
  rate <- 0.15 * 70                                 # mcg/min
  css  <- rate / (CLH / 60)                          # mcg/L = ng/mL
  expect_equal_rounded(4.9606, round(css, 4))
  # Thirty half-lives in, the plateau is the steady state
  expect_equal_rounded(css, wide$Plasma[wide$Time == 90][1], tolerance = 1e-4)
  # In the paper's units, nmol/L
  expect_equal_rounded(27.08, round(css / MW * 1000, 2))
})

test_that("epinephrine is a vasoactive drug with no MEAC or band", {
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "epinephrine", ]
  expect_identical(row$Category, VASOACTIVE_DRUG_CATEGORY)
  expect_equal(row$MEAC, 0)
  expect_equal(row$Lower, 0)
  expect_equal(row$Upper, 0)
})

test_that("the effect site peaks 1 min after a bolus (clinical)", {
  PK <- getDrugPK("epinephrine", 70, 170, 60, "male")
  expect_equal(PK$tPeak, 1)
  pk <- PK$PK$default
  coef <- c(pk$p_coef_bolus_l1, pk$p_coef_bolus_l2, pk$p_coef_bolus_l3)
  lambda <- c(pk$lambda_1, pk$lambda_2, pk$lambda_3)
  expect_equal(effectSitePeakTime(coef, lambda, pk$ke0), 1, tolerance = 1e-4)
})
