# Atropine (R/drugs_atropine.R): Hinderling 1985's two-compartment means in
# healthy men.  Drafted by Claude Code, 2026-10-19.

# Hinderling PH et al., J Pharm Sci 1985;74:703-710, Table I, overall means
VP <- 19.1; CL <- 1.175; KPT <- 0.560; KTP <- 0.0557
BASE <- 2 * 289.37 / 694.83     # base per unit of atropine sulfate monohydrate

test_that("the model is Table I's, on a sulfate dose", {
  PK <- getDrugPK("atropine", 70, 175, 22, "male", adjustToFFM = FALSE)
  set <- PK$PK$default
  expect_equal_rounded(VP / BASE, set$v1)
  expect_equal_rounded(CL / BASE, set$cl1)
  expect_equal_rounded(KPT * VP / BASE, set$cl2)
  expect_equal_rounded(VP * KPT / KTP / BASE, set$v2)
  expect_equal_rounded(0.8329, round(BASE, 4))
  expect_identical(PK$Concentration.Units, "ng")
  expect_equal(PK$tPeak, 0)
  expect_equal(set$ke0, 0)
})

test_that("Table I's other means, which the model does not use, check it", {
  set <- getDrugPK("atropine", 70, 175, 22, "male", adjustToFFM = FALSE)$PK$default
  # Vdss 210 L (in base terms)
  expect_lt(abs((set$v1 + set$v2) * BASE / 210 - 1), 0.02)
  # alpha 0.684 /min, terminal half-life 132 min
  expect_lt(abs(set$lambda_1 / 0.684 - 1), 0.03)
  expect_lt(abs(log(2) / set$lambda_2 / 132 - 1), 0.04)
  # Vd pseudo-steady-state = CL / beta, 234 L
  expect_lt(abs(CL / set$lambda_2 / 234 - 1), 0.02)
})

test_that("a 1 mg sulfate bolus starts at the base over the central volume", {
  wide <- simulateDrugsWithCovariates(
    data.frame(Drug = "atropine", Time = 0, Dose = 1, Units = "mg"),
    data.frame(Time = double(), Event = character()),
    70, 175, 22, "male", 60, FALSE, adjustToFFM = FALSE)$atropine$wide
  expect_equal_rounded(1 * BASE / VP * 1000, wide$Plasma[wide$Time == 0][1],
                       tolerance = 1e-4)
})

test_that("fixed parameters stand with the switch off and scale with it on", {
  size <- pkSizeFactors(110, 175, 50, "male", TRUE)
  on <- atropine(110, 175, 50, "male", adjustToFFM = TRUE)$PK$default
  expect_equal_rounded(CL * size$clearance / BASE, on$cl1)
  expect_equal_rounded(VP * size$volume / BASE, on$v1)
})

test_that("atropine is a vasoactive and resuscitation drug with no band", {
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "atropine", ]
  expect_identical(row$Category, VASOACTIVE_DRUG_CATEGORY)
  expect_equal(row$MEAC, 0)
})
