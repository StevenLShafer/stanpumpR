# Intravenous digoxin (R/drugs_digoxinIV.R): Cheng 1997's regressions on
# creatinine clearance, with Hornestam 2003's time to peak effect.
# Drafted by Claude Code, 2026-10-19.

events <- data.frame(Time = double(), Event = character())

test_that("clearance and Vss follow Cheng's regressions on Cockcroft-Gault CLcr", {
  scr  <- adultEquivalentCreatinine(1.0, 60, "male")
  crcl <- creatinineClearanceCG(70, 60, "male", scr)
  set <- digoxinIV(70, 175, 60, "male", adjustToFFM = FALSE, creatinine = 1.0)$PK$default
  expect_equal_rounded((0.0676 + 0.0003 * crcl) * 70 / 60, set$cl1)
  expect_equal_rounded(0.6 * 70, set$v1)
  expect_equal_rounded((5.289 + 0.0115 * crcl - 0.6) * 70, set$v2)
  expect_equal_rounded(0.615 * 70 / 60, set$cl2)
})

test_that("clearance never reaches zero: the non-renal floor", {
  anuric <- digoxinIV(70, 175, 60, "male", adjustToFFM = FALSE, creatinine = 15)$PK$default
  expect_gt(anuric$cl1, 0.0676 * 70 / 60)
  expect_lt(anuric$cl1, 0.070 * 70 / 60)
  # held at 140 mL/min
  young <- digoxinIV(100, 190, 20, "male", adjustToFFM = FALSE, creatinine = 0.4)$PK$default
  expect_equal_rounded((0.0676 + 0.0003 * 140) * 100 / 60, young$cl1)
})

test_that("Cheng's normal-renal group: Q gives alpha 1.3 /h and a terminal half-life near 49 h", {
  # The group means CL 0.11, Vc 0.6, Vss 6.4 L/kg with Q 0.615 L/h/kg
  k10 <- 0.11 / 0.6; k12 <- 0.615 / 0.6; k21 <- 0.615 / 5.8
  s <- k10 + k12 + k21; p <- k10 * k21
  alpha <- (s + sqrt(s^2 - 4 * p)) / 2; beta <- (s - sqrt(s^2 - 4 * p)) / 2
  expect_equal(round(alpha, 2), 1.30)
  expect_lt(abs(log(2) / beta / 49.2 - 1), 0.07)
})

test_that("the effect site peaks 87 minutes after a bolus (Hornestam 2003)", {
  PK <- getDrugPK("digoxinIV", 70, 175, 60, "male")
  expect_equal(PK$tPeak, 87)
  wide <- simulateDrugsWithCovariates(
    data.frame(Drug = "digoxinIV", Time = 0, Dose = 0.5, Units = "mg"),
    events, 70, 175, 60, "male", 300, FALSE)$digoxinIV$wide
  expect_lt(abs(wide$Time[which.max(wide$`Effect Site`)] - 87), 3)
})

test_that("Hornestam's own model puts the peak at 87 minutes", {
  # CL 9.88 L/h, Vc 27.8 L, Q 71.8 L/h, Vp 444 L, ke0 0.184 /h; per hour
  k10 <- 9.88 / 27.8; k12 <- 71.8 / 27.8; k21 <- 71.8 / 444; ke0 <- 0.184
  s <- k10 + k12 + k21; p <- k10 * k21
  lam <- c((s + sqrt(s^2 - 4 * p)) / 2, (s - sqrt(s^2 - 4 * p)) / 2)
  A <- c(lam[1] - k21, k21 - lam[2]) / (lam[1] - lam[2])   # unit-dose fractions
  ce <- function(t) sum(A * ke0 / (ke0 - lam) * (exp(-lam * t) - exp(-ke0 * t)))
  expect_equal(round(optimize(ce, c(0.1, 10), maximum = TRUE)$maximum * 60), 87)
})

test_that("digoxinIV carries the 0.5-2 ng/mL band and reads creatinine", {
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "digoxinIV", ]
  expect_identical(row$Category, VASOACTIVE_DRUG_CATEGORY)
  expect_equal(c(row$Lower, row$Upper), c(0.5, 2))
  expect_true("creatinine" %in% names(formals(digoxinIV)))
})

test_that("pregnancy: clearance x 1.42, directly, with the volumes unchanged", {
  # Hebert 2008's total renal x 181/115 on the 73% of clearance that is renal.  A woman
  # of 30 years, 70 kg, 165 cm, fat-free-mass scaling on.
  on  <- getDrugPK("digoxinIV", 70, 165, 30, SEX_FEMALE, pregnant = TRUE)
  off <- getDrugPK("digoxinIV", 70, 165, 30, SEX_FEMALE, pregnant = FALSE)
  expect_identical(off, getDrugPK("digoxinIV", 70, 165, 30, SEX_FEMALE))
  a <- on$PK$default; b <- off$PK$default
  expect_equal(a$cl1 / b$cl1, DIGOXIN_IV_PREGNANCY_CL_FACTOR, tolerance = 1e-6)
  expect_equal(DIGOXIN_IV_PREGNANCY_CL_FACTOR, 1.42)
  expect_equal(round(0.73 * 181 / 115 + 0.27, 2), 1.42)
  expect_equal_rounded(0.1207117690 / 1.35 * 1.42, a$cl1)
  # Not through Cheng's regressions: Vss (V2) does not move
  expect_identical(a[c("v1", "v2", "v3", "cl2", "cl3")],
                   b[c("v1", "v2", "v3", "cl2", "cl3")])
  expect_identical(on$tPeak, off$tPeak)
  expect_match(on$reference, "Hebert", fixed = TRUE)
  expect_no_match(off$reference, "Hebert", fixed = TRUE)
  # Applied whatever the creatinine field holds (see the header)
  m1 <- getDrugPK("digoxinIV", 70, 165, 30, SEX_FEMALE, creatinine = 0.6,
                  pregnant = TRUE)$PK$default
  m0 <- getDrugPK("digoxinIV", 70, 165, 30, SEX_FEMALE, creatinine = 0.6)$PK$default
  expect_equal(m1$cl1 / m0$cl1, DIGOXIN_IV_PREGNANCY_CL_FACTOR, tolerance = 1e-6)
  expect_identical(m1$v2, m0$v2)
})
