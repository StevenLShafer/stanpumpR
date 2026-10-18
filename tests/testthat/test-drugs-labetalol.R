# Labetalol (R/drugs_labetalol.R): Kanto 1981's two-compartment means in four
# healthy volunteers.  Drafted by Claude Code, 2026-10-18.
#
# The expectations re-derive the model from the five numbers in the abstract
# independently of the drug file, and use the one the model is not built
# from as the check.

# Kanto J et al., Int J Clin Pharmacol Ther Toxicol 1981;19:41-44
CL  <- 0.0248          # L/min/kg
V1  <- 1.1             # L/kg
VSS <- 9.41            # L/kg
l1  <- log(2) / 4.9    # /min
l2  <- log(2) / (4.9 * 60)

events <- data.frame(Time = double(), Event = character())

test_that("the decomposition gives the published rates, clearance and central volume", {
  PK <- getDrugPK("labetalol", 70, 175, 35, "male", adjustToFFM = FALSE)
  set <- PK$PK$default
  expect_equal_rounded(CL * 70, set$cl1)
  expect_equal_rounded(V1 * 70, set$v1)
  expect_equal_rounded(4.9, log(2) / set$lambda_1)
  expect_equal_rounded(4.9 * 60, log(2) / set$lambda_2)
  # Micro constants solved by hand: k10 = CL/V1, k21 = l1 l2 / k10
  k10 <- CL / V1
  k21 <- l1 * l2 / k10
  k12 <- l1 + l2 - k10 - k21
  expect_equal_rounded(k12 * V1 * 70, set$cl2)
  expect_equal_rounded(k12 * V1 / k21 * 70, set$v2)
  expect_equal_rounded(7.918, round(k12 * V1 / k21, 3))
  expect_equal(set$cl3, 0)
  expect_identical(PK$Concentration.Units, "ng")
  expect_equal(PK$tPeak, 0)
  expect_equal(set$ke0, 0)
  expect_equal(PK$MEAC, 0)
})

test_that("the printed steady-state volume, which the model does not use, checks it", {
  p <- twoCompartmentFromCentralVolume(CL, V1, 4.9, 4.9 * 60)
  vss <- p$v1 + p$v2
  expect_equal_rounded(9.018, round(vss, 3))
  expect_lt(abs(vss / VSS - 1), 0.05)
  # Building from Vss instead is ill-conditioned and misses the measured
  # central volume by a third
  fromVss <- twoCompartmentFromHalfLives(CL, VSS, 4.9, 4.9 * 60)
  expect_gt(fromVss$v1 / V1, 1.25)
})

test_that("the solver refuses numbers with no admissible model", {
  expect_error(twoCompartmentFromCentralVolume(CL, 50, 4.9, 4.9 * 60),
               "admissible")
})

test_that("per-kilogram parameters follow the weight, or the pharmacokinetic weight", {
  off <- labetalol(100, 180, 50, "male", adjustToFFM = FALSE)$PK$default
  expect_equal_rounded(CL * 100, off$cl1)
  size <- pkSizeFactors(100, 180, 50, "male", TRUE)
  on <- labetalol(100, 180, 50, "male", adjustToFFM = TRUE)$PK$default
  expect_equal_rounded(CL * size$pkWeight, on$cl1)
  expect_equal_rounded(V1 * size$pkWeight, on$v1)
})

test_that("a bolus starts at dose over the central volume", {
  # 20 mg into 1.1 x 70 = 77 L: 259.7 ng/mL
  wide <- simulateDrugsWithCovariates(
    data.frame(Drug = "labetalol", Time = 0, Dose = 20, Units = "mg"),
    events, 70, 175, 35, "male", 60, FALSE, adjustToFFM = FALSE)$labetalol$wide
  expect_equal_rounded(20 / 77 * 1000, wide$Plasma[wide$Time == 0][1],
                       tolerance = 1e-4)
  expect_equal_rounded(259.74, round(wide$Plasma[wide$Time == 0][1], 2))
})

test_that("labetalol is a vasoactive and resuscitation drug with no band", {
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "labetalol", ]
  expect_identical(row$Category, VASOACTIVE_DRUG_CATEGORY)
  expect_equal(row$MEAC, 0)
  expect_equal(row$Lower, 0)
  # Intravenous only: the model is the intravenous one
  expect_false(any(grepl("PO", unlist(row$Units))))
})
