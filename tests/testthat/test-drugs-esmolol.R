# Esmolol (R/drugs_esmolol.R): de Bruijn 1987's two-compartment means in
# anaesthetised cardiac surgical patients.  Drafted by Claude Code, 2026-10-19.

# de Bruijn NP et al., Anesthesiology 1987;66:323-326, Tables 3 and 4
CL <- 0.128; V1 <- 0.413          # L/min/kg, L/kg
TA <- 1.34; TB <- 9.9             # min
VBETA <- 1.90                     # L/kg

events <- data.frame(Time = double(), Event = character())

test_that("the decomposition gives the published rates, clearance and central volume", {
  PK <- getDrugPK("esmolol", 70, 175, 57, "male", adjustToFFM = FALSE)
  set <- PK$PK$default
  expect_equal_rounded(CL * 70, set$cl1)
  expect_equal_rounded(V1 * 70, set$v1)
  expect_equal_rounded(TA, log(2) / set$lambda_1)
  expect_equal_rounded(TB, log(2) / set$lambda_2)
  l1 <- log(2) / TA; l2 <- log(2) / TB
  k10 <- CL / V1; k21 <- l1 * l2 / k10; k12 <- l1 + l2 - k10 - k21
  expect_equal_rounded(k12 * V1 / k21 * 70, set$v2)
  expect_equal_rounded(0.567, round(k12 * V1 / k21, 3))
  # Well conditioned: k12 is a quarter of the sum of the rates
  expect_gt(k12 / (l1 + l2), 0.25)
  expect_identical(PK$Concentration.Units, "mcg")
  expect_equal(PK$tPeak, 0)
  expect_equal(set$ke0, 0)
})

test_that("V-beta, which the model does not use, checks it", {
  vbeta <- CL / (log(2) / TB)
  expect_equal_rounded(1.828, round(vbeta, 3))
  expect_lt(abs(vbeta / VBETA - 1), 0.05)
})

test_that("the stepped regimen's plateau is the paper's measured order of magnitude", {
  # Table 1 ends on 300 mcg/kg/min; Table 3's mean steady-state blood level
  # was 2.53 mcg/mL.  300 / 128 = 2.34 mcg/mL at steady state.
  dose <- data.frame(Drug = "esmolol",
                     Time = c(0, 1, 5, 6, 10, 11.5, 55),
                     Dose = c(500, 100, 500, 200, 500, 300, 0),
                     Units = "mcg/kg/min")
  wide <- simulateDrugsWithCovariates(dose, events, 78, 175, 57, "male", 90,
                                      FALSE, adjustToFFM = FALSE)$esmolol$wide
  late <- wide$Plasma[wide$Time == 55][1]
  expect_lt(abs(late / (0.300 / CL) - 1), 0.05)
  expect_lt(abs(late / 2.53 - 1), 0.15)
})

test_that("per-kilogram parameters follow the weight, or the pharmacokinetic weight", {
  size <- pkSizeFactors(110, 175, 57, "male", TRUE)
  on <- esmolol(110, 175, 57, "male", adjustToFFM = TRUE)$PK$default
  expect_equal_rounded(CL * size$pkWeight, on$cl1)
  off <- esmolol(110, 175, 57, "male", adjustToFFM = FALSE)$PK$default
  expect_equal_rounded(CL * 110, off$cl1)
})

test_that("esmolol is a vasoactive and resuscitation drug with no band", {
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "esmolol", ]
  expect_identical(row$Category, VASOACTIVE_DRUG_CATEGORY)
  expect_equal(row$MEAC, 0)
  expect_equal(row$Lower, 0)
})
