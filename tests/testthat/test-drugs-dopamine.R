# Dopamine (R/drugs_dopamine.R): MacGregor 2000's two-compartment model in
# healthy men.  Drafted by Claude Code, 2026-10-19.

# MacGregor DA et al., Anesthesiology 2000;92:338-346, Table 4
K10 <- 1.10; K12 <- 0.244; K21 <- 0.070   # /min
V1 <- 0.241                               # L/kg

events <- data.frame(Time = double(), Event = character())

test_that("the micro constants are Table 4's, per kilogram", {
  PK <- getDrugPK("dopamine", 90, 175, 33, "male", adjustToFFM = FALSE)
  set <- PK$PK$default
  expect_equal_rounded(V1 * 90, set$v1)
  expect_equal_rounded(K10 * V1 * 90, set$cl1)
  expect_equal_rounded(K12 * V1 * 90, set$cl2)
  expect_equal_rounded(K12 * V1 / K21 * 90, set$v2)
  expect_equal(set$cl3, 0)
  expect_identical(PK$Concentration.Units, "ng")
  expect_equal(PK$tPeak, 0)
  expect_equal(set$ke0, 0)
})

test_that("they reproduce every derived value Table 4 prints", {
  set <- dopamine(1, 175, 33, "male", adjustToFFM = FALSE)$PK$default   # per kg
  expect_equal(round(set$cl1, 2), 0.27)        # Cl10, L/kg/min
  expect_equal(round(set$cl2, 3), 0.059)       # Cl12
  expect_equal(round(set$v2, 3), 0.840)        # V2, L/kg
  PK <- getDrugPK("dopamine", 1, 175, 33, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(round(PK$lambda_1, 2), 1.36)    # alpha
  expect_equal(round(PK$lambda_2, 4), 0.0567)  # beta
  expect_equal(round(log(2) / PK$lambda_1, 2), 0.51)
  expect_equal(round(log(2) / PK$lambda_2, 1), 12.2)  # printed 12.3
  # Equation 3: Css = 3.77 min/L x k0 (mcg/kg/min), in mcg/L
  expect_equal(round(1 / set$cl1, 2), 3.77)
})

test_that("the 3 mcg/kg/min plateau is Equation 3's 11 300 ng/L", {
  wide <- simulateDrugsWithCovariates(
    data.frame(Drug = "dopamine", Time = c(0, 90), Dose = c(3, 0),
               Units = "mcg/kg/min"),
    events, 90, 175, 33, "male", 120, FALSE, adjustToFFM = FALSE)$dopamine$wide
  at90 <- wide$Plasma[wide$Time == 90][1]       # ng/mL
  # The paper: within 95% of steady state by 24.8 min
  expect_gt(at90, 0.99 * 11.3 * 0.99)
  expect_lt(at90, 11.4)
})

test_that("weight scales every parameter, or the pharmacokinetic weight", {
  size <- pkSizeFactors(116, 181, 23, "male", TRUE)
  on <- dopamine(116, 181, 23, "male", adjustToFFM = TRUE)$PK$default
  expect_equal_rounded(V1 * size$pkWeight, on$v1)
})

test_that("dopamine is a vasoactive and resuscitation drug with no band", {
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "dopamine", ]
  expect_identical(row$Category, VASOACTIVE_DRUG_CATEGORY)
  expect_equal(row$MEAC, 0)
})
