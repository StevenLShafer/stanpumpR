# Procainamide (R/drugs_procainamide.R): Grasela and Sheiner 1984's
# one-compartment population model.  Drafted by Claude Code, 2026-10-19.
#
# The expectations are built from Table III, Equations 5-12, Table IV and
# Figure 2 of the published article, independently of the drug file.

# Grasela TH, Sheiner LB, Clin Pharmacokinet 1984;9:545-554, Table III (base)
CLR <- 14.4; CLA <- 10.1; CLO <- 1.2; VD <- 136   # L/h and L, 70 kg standard
BASE <- 235.33 / 271.79      # procainamide base per unit of hydrochloride

fr <- function(age, sex, scr) (140 - age) / ((113 + 20 * (sex == "male")) * scr)

events <- data.frame(Time = double(), Event = character())

test_that("the standard individual has Table III's parameters, as the hydrochloride", {
  # A healthy 70 kg 20-year-old man whose creatinine gives FR = 1
  scr <- 120 / 133
  PK <- getDrugPK("procainamide", 70, 170, 20, "male", adjustToFFM = FALSE,
                  creatinine = scr)
  set <- PK$PK$default
  expect_equal_rounded((CLR + CLA + CLO) / 60 / BASE, set$cl1)
  expect_equal_rounded(VD / BASE, set$v1)
  expect_equal(set$cl2, 0)
  expect_identical(PK$Concentration.Units, "mcg")
  expect_equal(PK$tPeak, 0)
  expect_equal(set$ke0, 0)
  # The half-life does not depend on the salt: 0.693 x 136 / 25.7 h
  expect_equal_rounded(log(2) * VD / (CLR + CLA + CLO) * 60,
                       log(2) / set$lambda_1)
})

test_that("the paper's average patient is reproduced (Table IV)", {
  # 72.4 kg, creatinine 1.2, 59.7 years, a man, no heart failure
  w <- 72.4 / 70
  clr <- CLR * w * fr(59.7, "male", 1.2)
  expect_equal_rounded(10.45, round(CLA * w, 2))      # Table IV 10.4
  expect_equal_rounded(140.7, round(VD * w, 1))       # Table IV 140.0
  expect_equal_rounded(7.49, round(clr, 2))           # Table IV 7.2
  expect_lt(abs(clr / 7.2 - 1), 0.05)
  # The sex term as printed fits Table IV; Cockcroft-Gault's direction does not
  expect_gt(abs(CLR * w * (59.7 - 140) / (-113 * 1.2) / 7.2 - 1), 0.2)
  # Total clearance and half-life, Table IV 18.8 L/h and 5.2 h
  clt <- clr + CLA * w + CLO * w
  expect_lt(abs(clt / 18.8 - 1), 0.03)
  expect_lt(abs(log(2) * VD * w / clt / 5.2 - 1), 0.03)
  set <- procainamide(72.4, 175, 59.7, "male", adjustToFFM = FALSE,
                      creatinine = 1.2)$PK$default
  expect_equal_rounded(clt / 60 / BASE, set$cl1)
})

test_that("the dose is the hydrochloride: Figure 2 is reproduced", {
  # Figure 2: 1000 mg procainamide hydrochloride (850 mg base) orally every
  # 6 hours, bioavailability 0.85, absorbed as a zero-order input over the
  # 0.7 h time to peak, in the standard individual.  The curve peaks at about
  # 7.3 mg/L and falls to about 2.7.  As intravenous hydrochloride, the
  # absorbed amount is 1000 x 0.85 mg over 42 minutes.
  scr <- 120 / 133
  times <- c(rbind(seq(0, by = 360, length.out = 12),
                   seq(42, by = 360, length.out = 12)))
  dose <- data.frame(Drug = "procainamide", Time = times,
                     Dose = rep(c(1000 * 0.85 / 42, 0), 12), Units = "mg/min")
  wide <- simulateDrugsWithCovariates(dose, events, 70, 170, 20, "male",
                                      12 * 360, FALSE, adjustToFFM = FALSE,
                                      creatinine = scr)$procainamide$wide
  last <- wide[wide$Time >= 11 * 360, ]
  expect_gt(max(last$Plasma), 6.9)
  expect_lt(max(last$Plasma), 7.9)
  trough <- last$Plasma[which.min(abs(last$Time - 11 * 360))]
  expect_gt(trough, 2.4)
  expect_lt(trough, 3.1)
})

test_that("renal function follows Equation 7 as printed, from the Patient Profile", {
  w <- 80 / 70
  for (sex in c("male", "female")) {
    set <- procainamide(80, 170, 50, sex, adjustToFFM = FALSE,
                        creatinine = 2.0)$PK$default
    expect_equal_rounded((CLR * w * fr(50, sex, 2.0) + (CLA + CLO) * w) / 60 / BASE,
                         set$cl1)
  }
  # A blank creatinine is the assumed normal for age and sex
  blank <- procainamide(70, 170, 50, "male", adjustToFFM = FALSE)$PK$default
  given <- procainamide(70, 170, 50, "male", adjustToFFM = FALSE,
                        creatinine = assumedCreatinine(50, "male"))$PK$default
  expect_equal(blank, given)
})

test_that("weight is linear, or the pharmacokinetic weight", {
  size <- pkSizeFactors(110, 175, 50, "male", TRUE)
  on <- procainamide(110, 175, 50, "male", adjustToFFM = TRUE,
                     creatinine = 1)$PK$default
  w <- size$pkWeight / 70
  expect_equal_rounded(VD * w / BASE, on$v1)
})

test_that("procainamide is a vasoactive and resuscitation drug with no band", {
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "procainamide", ]
  expect_identical(row$Category, VASOACTIVE_DRUG_CATEGORY)
  expect_equal(row$MEAC, 0)
  expect_equal(row$Lower, 0)
})

test_that("pregnancy: renal clearance x 1.5, only with a blank creatinine", {
  # The rise in glomerular filtration on CL_R alone.  A woman of 30 years,
  # 70 kg, 165 cm, fat-free-mass scaling on, creatinine blank.
  on  <- getDrugPK("procainamide", 70, 165, 30, SEX_FEMALE, pregnant = TRUE)
  off <- getDrugPK("procainamide", 70, 165, 30, SEX_FEMALE, pregnant = FALSE)
  expect_identical(off, getDrugPK("procainamide", 70, 165, 30, SEX_FEMALE))
  a <- on$PK$default; b <- off$PK$default
  # Acetylation and other clearance are unchanged; CL_R rises by 1.5
  w <- pkSizeFactors(70, 165, 30, SEX_FEMALE, TRUE)$pkWeight / 70
  nonRenal <- (CLA + CLO) * w / 60 / BASE
  expect_equal((a$cl1 - nonRenal) / (b$cl1 - nonRenal), PREGNANCY_GFR_FACTOR,
               tolerance = 1e-6)
  # FR = 110 / (113 x 0.8): 61% renal, so the total rises by about 1.30
  expect_equal(round(a$cl1 / b$cl1, 2), 1.30)
  expect_equal_rounded(0.583156037, a$cl1)
  expect_identical(a[c("v1", "v2", "v3", "cl2", "cl3")],
                   b[c("v1", "v2", "v3", "cl2", "cl3")])
  expect_match(on$reference, "pregnancy", fixed = TRUE)
  expect_no_match(off$reference, "pregnancy", fixed = TRUE)
  # A measured creatinine already carries the rise: no change
  m1 <- getDrugPK("procainamide", 70, 165, 30, SEX_FEMALE, creatinine = 0.6,
                  pregnant = TRUE)
  m0 <- getDrugPK("procainamide", 70, 165, 30, SEX_FEMALE, creatinine = 0.6)
  expect_identical(m1$PK, m0$PK)
  expect_no_match(m1$reference, "pregnancy", fixed = TRUE)
})
