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
  # Fentanyl (Bae 2020) evaluates its own weight terms at the pharmacokinetic
  # weight, 70 kg x the fat-free-mass ratio 1.304907, with the switch on, and
  # at total weight with it off.
  expect_equal_rounded(on$PK$default$v1,  10.1 * 1.304907^1.23)
  expect_equal_rounded(on$PK$default$cl1, 0.704 * 1.304907^0.313)
  expect_equal_rounded(off$PK$default$v1,  10.1 * (120 / 70)^1.23)
  expect_equal_rounded(off$PK$default$cl1, 0.704 * (120 / 70)^0.313)
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
  expect_equal_rounded(on$ketamine$PK$default$v1,  .0583 * 70 * 1.304907)
  expect_equal_rounded(off$ketamine$PK$default$v1, .0583 * 120)
})

test_that("a cohort of reference men anchors exactly where the reference does", {
  k <- cohortSizeFactors(70, 35, men = 5, women = 0, height = 170)
  expect_equal(k$volume, 1, tolerance = 1e-12)
  expect_equal(k$clearance, 1, tolerance = 1e-12)
  expect_equal(pkSizeFactors(90, 180, 40, "female", TRUE, cohort = k)[1:2],
               pkSizeFactors(90, 180, 40, "female", TRUE)[1:2])
})

test_that("the cohort's typical patient receives factors of 1, and the switch off ignores the cohort", {
  # A cohort of 100 kg, 175 cm, 50-year-old men: its typical patient is one.
  k <- cohortSizeFactors(100, 50, men = 12, women = 0, height = 175)
  f <- pkSizeFactors(100, 175, 50, "male", TRUE, cohort = k)
  expect_equal(f$volume, 1, tolerance = 1e-12)
  expect_equal(f$clearance, 1, tolerance = 1e-12)
  # With the switch off the legacy factors stand, cohort or not.
  expect_identical(pkSizeFactors(100, 175, 50, "male", FALSE, legacyVolume = 1, cohort = k)[1:2],
                   list(volume = 1, clearance = 1))
})

test_that("a mixed cohort is the geometric mean of its man and its woman, weighted by their numbers", {
  # The BMI gives the height when none is printed: 81 kg at BMI 28 is 170.08 cm.
  k <- cohortSizeFactors(81, 45, men = 3, women = 1, bmi = 28)
  h <- 100 * sqrt(81 / 28)
  m <- pkSizeFactors(81, h, 45, "male", TRUE)
  w <- pkSizeFactors(81, h, 45, "female", TRUE)
  expect_equal(k$volume, (m$volume^3 * w$volume)^(1 / 4), tolerance = 1e-12)
  expect_equal(k$clearance, (m$clearance^3 * w$clearance)^(1 / 4), tolerance = 1e-12)
  expect_error(cohortSizeFactors(81, 45, men = 3, women = 1), "height or a BMI")
  expect_error(cohortSizeFactors(81, 45, men = 0, women = 0, bmi = 28), "at least one")
})
