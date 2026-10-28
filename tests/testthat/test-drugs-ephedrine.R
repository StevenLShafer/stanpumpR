# Ephedrine (R/drugs_ephedrine.R): one compartment derived from the oral
# population model of Csajka 2005 with bioavailability taken as complete,
# checked against Gurley 1998.  Drafted by Claude Code, 2026-10-28.

events <- data.frame(Time = double(), Event = character())

# Csajka 2005, Table 3
V_APP <- 181; CL_R <- 0.34; FE <- 0.78
BASE  <- 2 * 165.23 / 428.54     # base per unit of ephedrine sulfate

test_that("the model is Csajka's apparent set at F = 1, on a sulfate dose", {
  set <- getDrugPK("ephedrine", 70, 175, 40, "male", adjustToFFM = FALSE)$PK$default
  expect_equal_rounded(V_APP / FE / BASE, set$v1)
  expect_equal_rounded(CL_R  / FE / BASE, set$cl1)
  # one compartment
  expect_equal(set$cl2, 0)
  expect_equal(set$cl3, 0)
  expect_equal_rounded(0.7711, round(BASE, 4))
  expect_identical(getDrugPK("ephedrine", 70, 175, 40, "male")$Concentration.Units, "ng")
})

test_that("the half-life is Csajka's 6.1 h and does not depend on the salt", {
  set <- getDrugPK("ephedrine", 70, 175, 40, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(round(log(2) / (set$cl1 / set$v1) / 60, 1), 6.1)
  # V/CL is independent of the base factor and of F
  expect_equal_rounded(set$v1 / set$cl1, V_APP / CL_R)
})

test_that("Gurley 1998 (not fitted) checks the base-dose volume and clearance", {
  # Our model scales to a sulfate dose; in base-dose terms (what Gurley's
  # oral F = 1 would give) V = 181/0.78 L and CL = 0.34/0.78 L/min at 70 kg.
  set <- getDrugPK("ephedrine", 70, 175, 40, "male", adjustToFFM = FALSE)$PK$default
  vBasePerKg  <- set$v1  * BASE / 70
  clBasePerKg <- set$cl1 * BASE * 60 / 70
  # Gurley: Vss/F 2.98 (men) to 3.49 (women) L/kg; CL/F 0.37 (men) L/h/kg
  expect_gt(vBasePerKg, 2.98); expect_lt(vBasePerKg, 3.49)
  expect_equal(round(clBasePerKg, 2), 0.37)
})

test_that("a 10 mg sulfate bolus starts at the base over the central volume", {
  wide <- simulateDrugsWithCovariates(
    data.frame(Drug = "ephedrine", Time = 0, Dose = 10, Units = "mg"),
    events, 70, 175, 40, "male", 60, FALSE, adjustToFFM = FALSE)$ephedrine$wide
  set <- getDrugPK("ephedrine", 70, 175, 40, "male", adjustToFFM = FALSE)$PK$default
  # the base factor is already in v1, so 10 mg sulfate (7.71 mg base) over the
  # base-dose volume of 232 L gives 33 ng/mL
  expect_equal_rounded(10 / set$v1 * 1000, wide$Plasma[wide$Time == 0][1],
                       tolerance = 1e-4)
})

test_that("fixed parameters stand with the switch off and scale with it on", {
  size <- pkSizeFactors(110, 175, 50, "male", TRUE)
  on  <- ephedrine(110, 175, 50, "male", adjustToFFM = TRUE)$PK$default
  off <- ephedrine(110, 175, 50, "male", adjustToFFM = FALSE)$PK$default
  expect_equal_rounded(CL_R / FE * size$clearance / BASE, on$cl1)
  expect_equal_rounded(V_APP / FE * size$volume / BASE, on$v1)
  # off: the published values for everyone (legacyVolume = 1)
  expect_equal_rounded(V_APP / FE / BASE, off$v1)
})

test_that("ephedrine is a vasoactive drug with no band and no effect site", {
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "ephedrine", ]
  expect_identical(row$Category, VASOACTIVE_DRUG_CATEGORY)
  expect_equal(c(row$Lower, row$Upper, row$Typical, row$MEAC), c(0, 0, 0, 0))
  PK <- getDrugPK("ephedrine", 70, 170, 60, "male")
  expect_equal(PK$tPeak, 0)
  expect_match(PK$reference, "Csajka")
  # no renal term is applied
  expect_false("creatinine" %in% names(formals(ephedrine)))
})
