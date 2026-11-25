# Landiolol (R/drugs_landiolol.R): Kunisawa 2015's two compartments per
# kilogram in anaesthetised elderly patients with peripheral arterial disease.
# Drafted by Claude Code, 2026-11-25.

# Kunisawa T et al., Ther Clin Risk Manag 2015;11:107-114, Table 2
CL <- 30.7; V1 <- 65.0; Q <- 48.3; V2 <- 54.4   # mL/min/kg and mL/kg

events <- data.frame(Time = double(), Event = character())

test_that("the PK set is Table 2 per kilogram, at total weight with the switch off", {
  PK <- getDrugPK("landiolol", 58.9, 159, 73, "male", adjustToFFM = FALSE)
  set <- PK$PK$default
  expect_equal_rounded(CL / 1000 * 58.9, set$cl1)
  expect_equal_rounded(V1 / 1000 * 58.9, set$v1)
  expect_equal_rounded(Q / 1000 * 58.9, set$cl2)
  expect_equal_rounded(V2 / 1000 * 58.9, set$v2)
  expect_equal(set$cl3, 0)
  expect_identical(PK$Concentration.Units, "mcg")
  # The two exponents, from the micro constants of Table 2
  k10 <- CL / V1; k12 <- Q / V1; k21 <- Q / V2
  s <- k10 + k12 + k21
  lam <- c(s + sqrt(s^2 - 4 * k10 * k21), s - sqrt(s^2 - 4 * k10 * k21)) / 2
  expect_equal_rounded(lam[1], set$lambda_1)
  expect_equal_rounded(lam[2], set$lambda_2)
  # Terminal half-life about 4 minutes, as the label says of landiolol
  expect_lt(abs(log(2) / lam[2] - 4), 1)
})

test_that("per-kilogram parameters follow the pharmacokinetic weight with the switch on", {
  size <- pkSizeFactors(110, 175, 57, "male", TRUE)
  on <- landiolol(110, 175, 57, "male", adjustToFFM = TRUE)$PK$default
  expect_equal_rounded(CL / 1000 * size$pkWeight, on$cl1)
  expect_equal_rounded(V1 / 1000 * size$pkWeight, on$v1)
  off <- landiolol(110, 175, 57, "male", adjustToFFM = FALSE)$PK$default
  expect_equal_rounded(CL / 1000 * 110, off$cl1)
})

test_that("an infusion reaches the steady state its clearance sets", {
  # 10 mcg/kg/min for 60 min: steady state = 10 / 30.7 mcg/mL per (mL/min/kg)
  dose <- data.frame(Drug = "landiolol", Time = c(0, 60),
                     Dose = c(10, 0), Units = "mcg/kg/min")
  wide <- simulateDrugsWithCovariates(dose, events, 60, 160, 73, "male", 90,
                                      FALSE, adjustToFFM = FALSE)$landiolol$wide
  late <- wide$Plasma[wide$Time == 60][1]
  expect_lt(abs(late / (10 / CL) - 1), 0.01)
  # and it is gone within a quarter of an hour of the stop
  after <- wide$Plasma[wide$Time >= 75][1]
  expect_lt(after / late, 0.1)
})

test_that("the effect site peaks 2 min after a bolus (heart rate, Krumpl 2017)", {
  PK <- getDrugPK("landiolol", 70, 170, 60, "male")
  expect_equal(PK$tPeak, 2)
  pk <- PK$PK$default
  coef <- c(pk$p_coef_bolus_l1, pk$p_coef_bolus_l2, pk$p_coef_bolus_l3)
  lambda <- c(pk$lambda_1, pk$lambda_2, pk$lambda_3)
  expect_equal(effectSitePeakTime(coef, lambda, pk$ke0), 2, tolerance = 1e-4)
})

test_that("landiolol is a vasoactive and resuscitation drug with no band", {
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "landiolol", ]
  expect_identical(row$Category, VASOACTIVE_DRUG_CATEGORY)
  expect_equal(row$MEAC, 0)
  expect_equal(row$Lower, 0)
  expect_equal(row$Upper, 0)
})
