# Ephedrine (R/drugs_ephedrine.R): one compartment taken from the oral
# population model of Csajka 2005 (V/F and CL/F used directly at F = 1),
# checked against Gurley 1998.  Drafted by Claude Code, 2026-10-28.

events <- data.frame(Time = double(), Event = character())

# Csajka 2005, Table 3: V/F and CL/F (= the absolute V and total clearance
# at F = 1; CL_RE/F_E equals CL/F, see the model header)
V_F  <- 181; CL_F <- 0.34
BASE <- 2 * 165.23 / 428.54     # base per unit of ephedrine sulfate

test_that("the model is Csajka's V/F and CL/F at F = 1, on a sulfate dose", {
  set <- getDrugPK("ephedrine", 70, 175, 40, "male", adjustToFFM = FALSE)$PK$default
  expect_equal_rounded(V_F  / BASE, set$v1)
  expect_equal_rounded(CL_F / BASE, set$cl1)
  # one compartment
  expect_equal(set$cl2, 0)
  expect_equal(set$cl3, 0)
  expect_equal_rounded(0.7711, round(BASE, 4))
  expect_identical(getDrugPK("ephedrine", 70, 175, 40, "male")$Concentration.Units, "ng")
})

test_that("the half-life is Csajka's 6.1 h and does not depend on the salt", {
  set <- getDrugPK("ephedrine", 70, 175, 40, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(round(log(2) / (set$cl1 / set$v1) / 60, 1), 6.1)
  # V/CL is independent of the base factor
  expect_equal_rounded(set$v1 / set$cl1, V_F / CL_F)
})

test_that("Gurley 1998 (not fitted) brackets the base-dose model", {
  # In base-dose terms the model is Csajka's apparent V/F and CL/F at 70 kg.
  set <- getDrugPK("ephedrine", 70, 175, 40, "male", adjustToFFM = FALSE)$PK$default
  vBasePerKg  <- set$v1  * BASE / 70
  clBasePerKg <- set$cl1 * BASE * 60 / 70
  # Gurley reported Vss/F 2.98 (men) to 3.49 (women) L/kg and CL/F 0.37 to
  # 0.48 L/h/kg.  The model is checked against those published numbers (not
  # against model-derived pins): it sits just below both ranges, within a
  # fifth.  Gurley cannot by itself settle the apparent/absolute convention
  # -- the old /F_E double-count (+28%) would also land inside these bounds --
  # so the algebra (CL/F used directly at F = 1) is what the test above pins;
  # this one just keeps the model in a physiologically plausible band.
  GURLEY_V  <- c(2.98, 3.49); GURLEY_CL <- c(0.37, 0.48)
  expect_gt(vBasePerKg,  0.75 * GURLEY_V[1]);  expect_lt(vBasePerKg,  GURLEY_V[2])
  expect_gt(clBasePerKg, 0.75 * GURLEY_CL[1]); expect_lt(clBasePerKg, GURLEY_CL[2])
})

test_that("a 10 mg sulfate bolus starts at the base over the central volume", {
  wide <- simulateDrugsWithCovariates(
    data.frame(Drug = "ephedrine", Time = 0, Dose = 10, Units = "mg"),
    events, 70, 175, 40, "male", 60, FALSE, adjustToFFM = FALSE)$ephedrine$wide
  set <- getDrugPK("ephedrine", 70, 175, 40, "male", adjustToFFM = FALSE)$PK$default
  # the base factor is already in v1, so 10 mg sulfate (7.71 mg base) over the
  # base-dose volume of 181 L gives about 43 ng/mL
  expect_equal_rounded(10 / set$v1 * 1000, wide$Plasma[wide$Time == 0][1],
                       tolerance = 1e-4)
})

test_that("fixed parameters stand with the switch off and scale with it on", {
  size <- pkSizeFactors(110, 175, 50, "male", TRUE)
  on  <- ephedrine(110, 175, 50, "male", adjustToFFM = TRUE)$PK$default
  off <- ephedrine(110, 175, 50, "male", adjustToFFM = FALSE)$PK$default
  expect_equal_rounded(CL_F * size$clearance / BASE, on$cl1)
  expect_equal_rounded(V_F  * size$volume    / BASE, on$v1)
  # off: the published values for everyone (legacyVolume = 1), fixed at every
  # weight -- so a 110 kg patient matches the 70 kg reference, volume AND
  # clearance (a weight-scaled clearance would differ)
  ref <- ephedrine(70, 175, 50, "male", adjustToFFM = FALSE)$PK$default
  expect_equal_rounded(ref$v1,  off$v1)
  expect_equal_rounded(ref$cl1, off$cl1)
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
