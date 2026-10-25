# Propranolol (R/drugs_propranolol.R): Fagan 1982's mean triexponential in
# healthy men, checked against Cheymol 1987.  Drafted by Claude Code,
# 2026-10-19.

# Fagan TC et al., Br J Clin Pharmacol 1982;13:571-574, Table 1 means
DOSE <- 3.8                                  # mg
COEF <- c(317, 17.2, 9.3)                    # ng/mL
EXPO <- c(0.856, 0.0548, 0.203 / 60)         # /min

test_that("the model's unit-bolus response is the published triexponential", {
  PK <- getDrugPK("propranolol", 76, 178, 25, "male", adjustToFFM = FALSE)
  set <- PK$PK$default
  # The exponents are the paper's
  expect_equal_rounded(sort(EXPO, decreasing = TRUE),
                       c(set$lambda_1, set$lambda_2, set$lambda_3))
  # and the central volume is the dose over the zero-time intercept
  expect_equal_rounded(DOSE / sum(COEF) * 1000, set$v1)
  expect_identical(PK$Concentration.Units, "ng")
  expect_equal(PK$tPeak, 5)
  expect_gt(set$ke0, 0)
})

test_that("a bolus reproduces the curve at every time", {
  wide <- simulateDrugsWithCovariates(
    data.frame(Drug = "propranolol", Time = 0, Dose = DOSE, Units = "mg"),
    data.frame(Time = double(), Event = character()),
    76, 178, 25, "male", 480, FALSE, adjustToFFM = FALSE)$propranolol$wide
  t <- wide$Time
  expected <- COEF[1] * exp(-EXPO[1] * t) + COEF[2] * exp(-EXPO[2] * t) +
    COEF[3] * exp(-EXPO[3] * t)
  expect_equal(wide$Plasma, expected, tolerance = 1e-6)
})

test_that("the paper's AUC and clearance check it", {
  auc <- sum(COEF / EXPO) / 60                 # ng h/mL
  expect_equal_rounded(57.2, round(auc, 1))
  expect_lt(abs(auc / 57.6 - 1), 0.01)
  set <- propranolol(76, 178, 25, "male", adjustToFFM = FALSE)$PK$default
  expect_equal_rounded(DOSE / (auc * 60) * 1000, set$cl1)
  expect_lt(abs(set$cl1 / 1.19 - 1), 0.08)
})

test_that("Cheymol 1987's controls, which the model never saw, agree", {
  set <- propranolol(76, 178, 25, "male", adjustToFFM = FALSE)$PK$default
  PK  <- getDrugPK("propranolol", 76, 178, 25, "male", adjustToFFM = FALSE)$PK$default
  expect_lt(abs(set$cl1 * 60 / 75.9 - 1), 0.17)                 # CL 75.9 L/h
  expect_lt(abs((set$v1 + set$v2 + set$v3) / 318.6 - 1), 0.20)  # Vss 318.6 L
  expect_lt(abs(log(2) / PK$lambda_3 / 60 / 3.1 - 1), 0.12)     # 3.1 h
})

test_that("every subject's own triexponential is admissible too", {
  subjects <- list(
    c(3.9, 49, 30, 11, 0.377, 0.0539, 0.181),
    c(4.5, 523, 18, 10, 1.175, 0.0526, 0.234),
    c(4.0, 1007, 14, 6, 2.445, 0.0254, 0.101),
    c(3.5, 41, 9, 9, 0.304, 0.1064, 0.262),
    c(3.0, 53, 14, 4, 0.394, 0.0355, 0.197))
  for (s in subjects) {
    p <- threeCompartmentFromTriexponential(s[2:4] / s[1], c(s[5], s[6], s[7] / 60))
    expect_true(all(unlist(p) > 0))
  }
  # and a curve that rises cannot be a mammillary model
  expect_error(threeCompartmentFromTriexponential(c(1, -2, 2), c(1, 0.1, 0.01)),
               "admissible")
})

test_that("propranolol is a vasoactive and resuscitation drug with no band", {
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "propranolol", ]
  expect_identical(row$Category, VASOACTIVE_DRUG_CATEGORY)
  expect_equal(row$MEAC, 0)
})

test_that("the effect site peaks 5 min after a bolus (heart rate, Achong 1976)", {
  PK <- getDrugPK("propranolol", 70, 170, 60, "male")
  expect_equal(PK$tPeak, 5)
  pk <- PK$PK$default
  coef <- c(pk$p_coef_bolus_l1, pk$p_coef_bolus_l2, pk$p_coef_bolus_l3)
  lambda <- c(pk$lambda_1, pk$lambda_2, pk$lambda_3)
  expect_equal(effectSitePeakTime(coef, lambda, pk$ke0), 5, tolerance = 1e-4)
})
