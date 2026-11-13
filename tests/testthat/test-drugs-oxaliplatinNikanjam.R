# Oxaliplatin (R/drugs_oxaliplatinNikanjam.R): Nikanjam 2015, three
# compartments allometric in weight, serum creatinine on clearance and age on
# V1.  Drafted by Claude Code, 2026-11-06.

noEvents <- data.frame(Time = double(), Event = character())

test_that("Table 2 at the cohort's medians gives the values Deyme 2019 tabulates", {
  # 59 kg, creatinine 0.8 mg/dL, 20 years
  X <- oxaliplatinNikanjam(59, 170, 20, "male", adjustToFFM = FALSE, creatinine = 0.8)
  d <- X$PK$default
  expect_equal(round(d$cl1 * 60, 2), 11.76)
  expect_equal(round(d$cl2 * 60, 2), 67.48)
  expect_equal(round(d$cl3 * 60, 2), 4.68)
  expect_equal(round(d$v1, 2), 22.83)
  expect_equal(round(d$v2, 1), 367.0)
  expect_equal(round(d$v3, 1), 997.1)
  expect_equal(round(d$v1 + d$v2 + d$v3), 1387)
  PK <- getDrugPK("oxaliplatinNikanjam", 59, 170, 20, "male", adjustToFFM = FALSE,
                  creatinine = 0.8)$PK$default
  halfLives <- log(2) / c(PK$lambda_1, PK$lambda_2, PK$lambda_3) / 60
  expect_equal(round(halfLives, c(2, 1, 0)), c(0.18, 19.3, 214))
})

test_that("the covariate terms are the printed powers", {
  cl <- function(...) oxaliplatinNikanjam(...)$PK$default$cl1
  base <- cl(70, 170, 40, "male", adjustToFFM = FALSE, creatinine = 0.6)
  expect_equal_rounded(base * 60, 0.67 * 70^0.75)
  # the 6.7-fold range of clearance across the studied creatinine, 0.2-3.4 mg/dL
  expect_equal(round(cl(70, 170, 40, "male", FALSE, 0.2) /
                       cl(70, 170, 40, "male", FALSE, 3.4), 1), 6.7)
  # no sex term
  expect_equal(cl(70, 170, 40, "female", FALSE, 0.8), cl(70, 170, 40, "male", FALSE, 0.8))
  # age on V1 only: (86/0.6)^0.16, the 2.2-fold range
  v1 <- function(age) oxaliplatinNikanjam(70, 170, age, "male", FALSE, 0.8)$PK$default$v1
  expect_equal(round(v1(86) / v1(0.6), 1), 2.2)
  expect_equal(cl(70, 170, 86, "male", FALSE, 0.8), cl(70, 170, 20, "male", FALSE, 0.8))
})

test_that("the guards: age floored at 0.6 years, creatinine at 0.2 mg/dL", {
  X <- function(age, scr) oxaliplatinNikanjam(10, 75, age, "male", FALSE, scr)$PK$default
  expect_equal(X(0, 0.5)$v1, X(0.6, 0.5)$v1)
  expect_gt(X(1, 0.5)$v1, X(0.6, 0.5)$v1)
  expect_equal(X(2, 0)$cl1, X(2, 0.2)$cl1)
  expect_true(is.finite(X(0, 0)$cl1) && is.finite(X(0, 0)$v1))
})

test_that("a blank creatinine is the assumed normal, read as the patient's own", {
  cl <- function(scr) oxaliplatinNikanjam(70, 170, 40, "male", FALSE, scr)$PK$default$cl1
  expect_equal(cl(NULL), cl(SCR_ASSUMED_MALE))
  # a child's creatinine is used as measured (patientCreatinine), not rescaled
  child <- function(scr) oxaliplatinNikanjam(20, 115, 6, "male", FALSE, scr)$PK$default$cl1
  expect_equal_rounded(child(0.4) * 60, 0.67 * 20^0.75 * (0.6 / 0.4)^0.67)
})

test_that("130 mg/m2 over 2 h: AUC as the paper's simulations, the curve as Figure 1", {
  # 70-kg adult (Mosteller 1.82 m2), creatinine 0.8: dose/CL 17.7 (paper 17.3)
  bsa <- sqrt(70 * 170 / 3600)
  X <- oxaliplatinNikanjam(70, 170, 60, "male", FALSE, 0.8)
  expect_equal(round(130 * bsa / (X$PK$default$cl1 * 60), 1), 17.7)
  # 22-kg child at 0.5 mg/dL: 14.5 (paper 14.2 at 2-6 years)
  cbsa <- sqrt(22 * 120 / 3600)
  C <- oxaliplatinNikanjam(22, 120, 5, "male", FALSE, 0.5)
  expect_equal(round(130 * cbsa / (C$PK$default$cl1 * 60), 1), 14.5)

  dose <- data.frame(Drug = "oxaliplatinNikanjam", Time = c(0, 120),
                     Dose = c(130 * bsa / 2, 0), Units = "mg/hr")
  wide <- simulateDrugsWithCovariates(dose, noEvents, 70, 170, 60, "male", 504 * 60,
                                      FALSE, adjustToFFM = FALSE,
                                      creatinine = 0.8)$oxaliplatinNikanjam$wide
  at <- function(h) approx(wide$Time, wide$Plasma, h * 60)$y
  expect_equal(at(2), 1455, tolerance = 0.005)       # ng/mL, end of infusion
  expect_equal(at(168), 12.0, tolerance = 0.01)
  expect_equal(at(504), 3.85, tolerance = 0.01)
})

test_that("both switch positions are pinned", {
  on <- oxaliplatinNikanjam(70, 170, 35, "male")
  off <- oxaliplatinNikanjam(70, 170, 35, "male", adjustToFFM = FALSE)
  # the reference man: his pharmacokinetic weight is his weight
  expect_equal_rounded(on$PK$default$cl1, off$PK$default$cl1)
  obese <- oxaliplatinNikanjam(120, 170, 35, "male")
  obeseOff <- oxaliplatinNikanjam(120, 170, 35, "male", adjustToFFM = FALSE)
  expect_lt(obese$PK$default$v2, obeseOff$PK$default$v2)
  expect_equal_rounded(obeseOff$PK$default$v2, 6.22 * 120)
})

test_that("oxaliplatinNikanjam is a chemotherapy drug, plasma only, in ng/mL, with no band", {
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "oxaliplatinNikanjam", ]
  expect_true(is.na(row$Category) || row$Category == "")
  expect_true(row$Drug %in% unlist(lapply(MODEL_GROUPS, function(g) g$model)))  # a candidate under its drug's one name
  expect_identical(row$Concentration.Units, "ng")
  expect_equal(c(row$Lower, row$Upper, row$Typical, row$MEAC), c(0, 0, 0, 0))
  PK <- getDrugPK("oxaliplatinNikanjam", 70, 170, 60, "male")
  expect_equal(PK$tPeak, 0)
  expect_match(PK$reference, "Nikanjam")
  expect_identical(therapeuticRange("oxaliplatinNikanjam")$band_status, "not_applicable")
})
