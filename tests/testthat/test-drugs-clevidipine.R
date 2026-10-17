# Clevidipine (R/drugs_clevidipine.R): Ericsson 1999's two-compartment
# whole-blood model, with V1, V2 and Q derived from the four quantities the
# paper reports.  Drafted by Claude Code, 2026-10-17.
#
# The expectations here are built from the published numbers INDEPENDENTLY of
# the drug file: the two-compartment relations are re-solved below from the
# half-lives, clearance and steady-state volume, and the simulated curve is
# checked against the paper's own statements.

# Ericsson H et al., Br J Clin Pharmacol 1999;47:531-538
CL   <- 0.121   # L/min/kg
VSS  <- 0.56    # L/kg
TH1  <- 1.8     # min, initial
TH2  <- 9.5     # min, terminal
l1 <- log(2) / TH1
l2 <- log(2) / TH2

events <- data.frame(Time = double(), Event = character())

simulateClevidipine <- function(dose, maximum = 120, adjustToFFM = FALSE,
                                weight = 70, height = 170, age = 35,
                                sex = "male") {
  simulateDrugsWithCovariates(dose, events, weight, height, age, sex, maximum,
                              FALSE, adjustToFFM = adjustToFFM)$clevidipine$wide
}

test_that("the derived two-compartment vector satisfies the published relations", {
  p <- twoCompartmentFromHalfLives(CL, VSS, TH1, TH2)
  k10 <- p$cl1 / p$v1
  k12 <- p$cl2 / p$v1
  k21 <- p$cl2 / p$v2
  # The two defining relations between micro constants and eigenvalues
  expect_equal_rounded(l1 + l2, k10 + k12 + k21)
  expect_equal_rounded(l1 * l2, k10 * k21)
  # Clearance and steady-state volume are reproduced exactly
  expect_equal_rounded(CL, p$cl1)
  expect_equal_rounded(VSS, p$v1 + p$v2)
  # A physically admissible mammillary model: everything positive, and k21
  # between the two eigenvalues
  expect_true(p$v1 > 0 && p$v2 > 0 && p$cl2 > 0)
  expect_true(k21 > l2 && k21 < l1)
})

test_that("the derivation reproduces the paper's own AUC statement, which it does not use", {
  p <- twoCompartmentFromHalfLives(CL, VSS, TH1, TH2)
  k21 <- p$cl2 / p$v2
  # Coefficients of the unit-bolus biexponential
  A <- (l1 - k21) / (p$v1 * (l1 - l2))
  B <- (k21 - l2) / (p$v1 * (l1 - l2))
  # Total AUC per unit dose is 1/CL, as it must be for any linear model
  expect_equal_rounded(1 / CL, A / l1 + B / l2)
  # The paper: the initial half-life contributes "more than 80%" of the AUC
  fast <- (A / l1) / (A / l1 + B / l2)
  expect_gt(fast, 0.80)
  expect_equal_rounded(0.8171, round(fast, 4))
})

test_that("the solver refuses a set of numbers with no admissible decomposition", {
  # A steady-state volume so large that the peripheral compartment would need
  # a negative central volume
  expect_error(twoCompartmentFromHalfLives(CL, 20, TH1, TH2),
               "not physically admissible")
})

test_that("getDrugPK gives the published parameters at the reference man, plasma only", {
  PK <- getDrugPK("clevidipine", 70, 170, 35, "male", adjustToFFM = FALSE)
  set <- PK$PK$default
  expect_equal_rounded(CL * 70, set$cl1)
  expect_equal_rounded(VSS * 70, set$v1 + set$v2)
  # Two compartments: the third is the library's placeholder
  expect_equal(set$cl3, 0)
  # Eigenvalue half-lives are the paper's
  expect_equal_rounded(TH1, log(2) / set$lambda_1)
  expect_equal_rounded(TH2, log(2) / set$lambda_2)
  # Whole blood in ng/mL, no effect site, no band, not an opioid
  expect_identical(PK$Concentration.Units, "ng")
  expect_equal(PK$tPeak, 0)
  expect_equal(set$ke0, 0)
  expect_equal(PK$MEAC, 0)
  expect_equal(PK$typical, 0)
})

test_that("the fat-free-mass switch scales the per-kilogram model and leaves the reference man alone", {
  on  <- getDrugPK("clevidipine", 70, 170, 35, "male", adjustToFFM = TRUE)$PK$default
  off <- getDrugPK("clevidipine", 70, 170, 35, "male", adjustToFFM = FALSE)$PK$default
  # The reference man is the anchor: both switch positions agree there
  expect_equal_rounded(off$v1, on$v1)
  expect_equal_rounded(off$cl1, on$cl1)
  # With the switch off a 120 kg man gets the published value times his weight
  obese <- getDrugPK("clevidipine", 120, 170, 35, "male",
                     adjustToFFM = FALSE)$PK$default
  expect_equal_rounded(CL * 120, obese$cl1)
  # With it on he is scaled to fat-free mass instead, so clearance is lower
  obeseFFM <- getDrugPK("clevidipine", 120, 170, 35, "male",
                        adjustToFFM = TRUE)$PK$default
  expect_lt(obeseFFM$cl1, obese$cl1)
  # The half-lives are weight-free: a per-kilogram model keeps its rates
  expect_equal_rounded(TH1, log(2) / obeseFFM$lambda_1)
  expect_equal_rounded(TH2, log(2) / obeseFFM$lambda_2)
})

test_that("a bolus starts at dose over the central volume", {
  # The analytic fixture the specification asks for: C(0+) = D / V1.  One
  # milligram into a reference man's 0.3689 x 70 = 25.82 L gives 38.7 ng/mL.
  p <- twoCompartmentFromHalfLives(CL, VSS, TH1, TH2)
  expected <- 1 / (p$v1 * 70) * 1000      # mg into L, reported as ng/mL
  wide <- simulateClevidipine(
    data.frame(Drug = "clevidipine", Time = 0, Dose = 1, Units = "mg"))
  expect_equal_rounded(expected, wide$Plasma[wide$Time == 0], tolerance = 1e-4)
  expect_equal_rounded(38.73, round(wide$Plasma[wide$Time == 0], 2))
})

test_that("an infusion approaches the steady state its clearance implies", {
  # Css = rate / CL.  10 mg/h into a reference man whose clearance is
  # 0.121 x 70 = 8.47 L/min: 10/60 mg/min / 8.47 L/min = 19.68 ng/mL.
  rate <- 10                                    # mg/hr
  css <- (rate / 60) / (CL * 70) * 1000         # ng/mL
  wide <- simulateClevidipine(
    data.frame(Drug = "clevidipine", Time = c(0, 300),
               Dose = c(rate, 0), Units = "mg/hr"),
    maximum = 360)
  # Clevidipine's terminal half-life is 9.5 min, so five hours is many
  # half-lives: the plateau is the steady state to a tenth of a percent
  expect_equal_rounded(css, wide$Plasma[wide$Time == 300], tolerance = 1e-3)
  expect_equal_rounded(19.68, round(wide$Plasma[wide$Time == 300], 2))
})

test_that("the dose units are interchangeable and the curve decays as published", {
  # 1 mcg/kg/min in a 70 kg man is 0.07 mg/min, i.e. 4.2 mg/hr
  byRate <- simulateClevidipine(
    data.frame(Drug = "clevidipine", Time = c(0, 60), Dose = c(1, 0),
               Units = "mcg/kg/min"), maximum = 120)
  byHour <- simulateClevidipine(
    data.frame(Drug = "clevidipine", Time = c(0, 60), Dose = c(4.2, 0),
               Units = "mg/hr"), maximum = 120)
  expect_equal_rounded(byRate$Plasma, byHour$Plasma, tolerance = 1e-6)
  # After the infusion stops the fall is fast: the paper's initial half-life
  # carries most of the decline, so by 10 minutes after stopping the
  # concentration is a small fraction of the plateau
  plateau <- byRate$Plasma[byRate$Time == 60][1]
  after <- byRate$Plasma[which.min(abs(byRate$Time - 70))]
  expect_lt(after / plateau, 0.25)
})

test_that("the registry marks clevidipine executable for concentration and never for blood pressure", {
  reg <- antihypertensiveRegistry()
  rows <- reg[reg$stanpumpr_drug %in% "clevidipine" & !is.na(reg$stanpumpr_drug), ]
  expect_equal(nrow(rows), 1)
  expect_identical(rows$endpoint, "concentration")
  expect_identical(rows$matrix, "whole blood")
  expect_identical(rows$execution_status, "ready_for_source_reproduction")
  expect_false(rows$bp_simulation_enabled)
  # The postoperative threshold record is a different matrix and stays gated
  post <- reg[grepl("bailey2002", reg$model_id), ]
  expect_identical(post$matrix, "plasma")
  expect_identical(post$execution_status, "requires_source_extraction")
  expect_true(is.na(post$stanpumpr_drug))
})
