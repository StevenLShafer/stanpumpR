# Fat-free mass and the size factors derived from it.  Expected values were
# worked out from the published formulas by hand (Python), not from this code.
#
#   Janmahasatian 2005, adults:
#     male   FFM = 9270 * WT / (6680 + 216 * BMI)
#     female FFM = 9270 * WT / (8780 + 244 * BMI)
#   Al-Sallami 2015 maturation multiplier:
#     male   0.88 + 0.12 / (1 + (age / 13.4)^-12.7)
#     female 1.11 - 0.11 / (1 + (age / 7.1)^-1.1)

test_that("fat-free mass matches the Al-Sallami 2015 formula", {
  # Reference male: 70 kg, 170 cm, 35 y
  expect_equal_rounded(ffmAlSallami(70, 170, 35, "male"), 54.47521)
  # Adult male, maturation term is 1 to seven decimals
  expect_equal_rounded(ffmAlSallami(120, 170, 50, "male"), 71.08506)
  # Adult female: the maturation term is 1.0115 at 50 y, so this is slightly
  # above the Janmahasatian adult value of 44.17 kg
  expect_equal_rounded(ffmAlSallami(70, 170, 50, "female"), 44.68106)
  expect_equal_rounded(ffmAlSallami(55, 160, 30, "female"), 37.04040)
  # Child: the male maturation term is 0.88 at 5 y
  expect_equal_rounded(ffmAlSallami(20, 110, 5, "male"), 15.91689)
})

test_that("the reference male has size factors of exactly 1", {
  size <- pkSizeFactors(70, 170, 35, "male")
  expect_equal(size$volume, 1)
  expect_equal(size$clearance, 1)
  expect_equal(size$pkWeight, 70)
  expect_equal_rounded(size$ffmReference, 54.47521)
  expect_equal_rounded(FFM_REFERENCE, 54.47521)
})

test_that("volumes scale with the fat-free mass ratio and clearances with its 0.75 power", {
  size <- pkSizeFactors(120, 170, 50, "male")
  expect_equal_rounded(size$ffm, 71.08506)
  expect_equal_rounded(size$volume, 1.304907)
  expect_equal_rounded(size$clearance, 1.220913)
  expect_equal_rounded(size$pkWeight, 91.34347)

  size <- pkSizeFactors(70, 170, 50, "female")
  expect_equal_rounded(size$volume, 0.8202091)
  expect_equal_rounded(size$clearance, 0.8618733)
  expect_equal_rounded(size$pkWeight, 57.41463)
})

test_that("adjustToFFM = FALSE returns the legacy factors the caller supplies", {
  # default legacy: weight/70 for both (fixed rate constants)
  size <- pkSizeFactors(120, 170, 50, "male", adjustToFFM = FALSE)
  expect_equal(size$volume, 120 / 70)
  expect_equal(size$clearance, 120 / 70)
  # allometric legacy
  size <- pkSizeFactors(120, 170, 50, "male", adjustToFFM = FALSE,
                        legacyClearance = (120 / 70)^0.75)
  expect_equal(size$volume, 120 / 70)
  expect_equal(size$clearance, (120 / 70)^0.75)
  # unscaled legacy
  size <- pkSizeFactors(120, 170, 50, "male", adjustToFFM = FALSE, legacyVolume = 1)
  expect_equal(size$volume, 1)
  expect_equal(size$clearance, 1)
  # fat-free mass is still reported so the UI can show it
  expect_equal_rounded(size$ffm, 71.08506)
})

test_that("getDrugPK passes the switch through to the drug model", {
  on  <- getDrugPK("fentanyl", 120, 170, 50, "male")
  off <- getDrugPK("fentanyl", 120, 170, 50, "male", adjustToFFM = FALSE)
  expect_equal_rounded(on$PK$default$v1,  12.1 * 1.304907)
  expect_equal_rounded(on$PK$default$cl1, 0.632 * 1.220913)
  expect_equal_rounded(off$PK$default$v1,  12.1 * 120 / 70)
  expect_equal_rounded(off$PK$default$cl1, 0.632 * (120 / 70)^0.75)
})

test_that("models with their own fat-free-mass covariate ignore the switch", {
  for (drug in c("propofol", "remifentanil")) {
    on  <- getDrugPK(drug, 120, 170, 50, "male")
    off <- getDrugPK(drug, 120, 170, 50, "male", adjustToFFM = FALSE)
    expect_identical(on$PK, off$PK, info = drug)
  }
})

test_that("simulateDrugsWithCovariates honours the switch", {
  dose <- data.frame(Drug = c("fentanyl", "propofol"), Time = 0,
                     Dose = c(100, 200), Units = c("mcg", "mg"))
  events <- data.frame(Time = double(), Event = character())
  on  <- simulateDrugsWithCovariates(dose, events, 120, 170, 50, "male", 60, FALSE)
  off <- simulateDrugsWithCovariates(dose, events, 120, 170, 50, "male", 60, FALSE,
                                     adjustToFFM = FALSE)
  # A 120 kg man has less fat-free mass than 120/70 of the reference, so the
  # same fentanyl bolus gives higher concentrations with the switch on.
  expect_gt(max(on$fentanyl$results$Y), max(off$fentanyl$results$Y))
  # Propofol (Eleveld) is unchanged.
  expect_equal(on$propofol$results, off$propofol$results)
})

test_that("recalculatePK passes the switch through", {
  DT <- data.frame(Drug = "ketamine", Time = 0, Dose = 100, Units = "mg")
  on  <- recalculatePK(NULL, getDrugDefaultsGlobal(FALSE), DT,
                       age = 50, weight = 120, height = 170, sex = "male")
  off <- recalculatePK(NULL, getDrugDefaultsGlobal(FALSE), DT,
                       age = 50, weight = 120, height = 170, sex = "male",
                       adjustToFFM = FALSE)
  expect_equal_rounded(on$ketamine$PK$default$v1,  .063 * 70 * 1.304907)
  expect_equal_rounded(off$ketamine$PK$default$v1, .063 * 120)
})
