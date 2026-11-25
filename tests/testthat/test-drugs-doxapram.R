# Doxapram (R/drugs_doxapram.R): Clements 1979's Subject 2, three
# compartments, with Roozekrans 2017's time to peak effect.  Drafted by
# Claude Code, 2026-11-20.

doxapramPlasma <- function(dose, weight, maximum, times, height = 175, age = 35,
                           sex = "male", adjustToFFM = FALSE)
{
  wide <- simulateDrugsWithCovariates(
    dose, data.frame(Time = double(), Event = character()),
    weight, height, age, sex, maximum, FALSE, adjustToFFM = adjustToFFM)$doxapram$wide
  stats::approx(wide$Time, wide$Plasma, xout = times)$y
}

test_that("the model reproduces Subject 2's triexponential exactly", {
  # Table 1: A 2.01, 1.14, 0.25 ug/mL and 0.0484, 0.0105, 0.0018 /min after
  # 1.5 mg/kg.  Rebuild the unit response from the compartments, by the
  # eigen decomposition of the rate matrix, independently of the converter.
  set <- doxapram(70, 175, 35, "male", adjustToFFM = FALSE)$PK$default
  k10 <- set$cl1 / set$v1; k12 <- set$cl2 / set$v1; k13 <- set$cl3 / set$v1
  k21 <- set$cl2 / set$v2; k31 <- set$cl3 / set$v3
  K <- matrix(c(-(k10 + k12 + k13), k21, k31,
                k12, -k21, 0,
                k13, 0, -k31), 3, byrow = TRUE)
  e <- eigen(K)      # the amounts evolve as dx/dt = K x
  lambda <- -e$values
  amp <- e$vectors[1, ] * solve(e$vectors, c(1, 0, 0))   # central amount, unit dose
  ord <- order(-lambda)
  coefPerMgPerKg <- Re(amp[ord]) / set$v1 * 70   # mg/L per mg/kg
  expect_equal(sort(Re(lambda), decreasing = TRUE), c(0.0484, 0.0105, 0.0018),
               tolerance = 1e-8)
  expect_equal(coefPerMgPerKg * 1.5, c(2.01, 1.14, 0.25), tolerance = 1e-8)
})

test_that("the paper's own V1 and clearance follow on the base basis", {
  # Table 1 prints V1 0.40 L/kg and TBC 4.78 mL/min/kg for Subject 2; they
  # are reproduced only with the 1.5 mg/kg converted to base (378.52/414.98)
  base <- 1.5 * 378.52 / 414.98
  A <- DOXAPRAM_COEF; l <- DOXAPRAM_EXP
  expect_equal(round(base / sum(A), 2), 0.40)
  expect_lt(abs(1000 * base / sum(A / l) / 4.78 - 1), 0.02)   # gamma printed to 2 figures
  # and the proceedings abstract's 6.5 mL/min/kg is Table 2's 5.93 on the
  # hydrochloride basis
  expect_equal(round(5.93 * 414.98 / 378.52, 1), 6.5)
})

test_that("returns the correct calculations", {
  # per kg from the triexponential: V1 1.5/3.40, then x 70
  actual <- doxapram(70, 175, 35, "male", adjustToFFM = FALSE)
  expected <- list(
    PK = list(default = list(
      v1 = 30.882353, v2 = 17.363788, v3 = 62.843891,
      cl1 = 0.36333536, cl2 = 0.44050762, cl3 = 0.19260346
    )),
    tPeak = 5.8, MEAC = 0,
    typical = 2, upperTypical = 3, lowerTypical = 1.5
  )
  expect_equal_rounded(actual[names(expected)], expected)
  expect_match(actual$reference, "Clements")
})

test_that("scales to fat-free mass for a 120 kg man, linearly with the switch off", {
  size <- pkSizeFactors(120, 170, 50, "male", TRUE)
  on <- doxapram(120, 170, 50, "male")$PK$default
  expect_equal(on$v1, 30.882353 * size$volume, tolerance = 1e-6)
  expect_equal(on$cl1, 0.36333536 * size$clearance, tolerance = 1e-6)
  off <- doxapram(120, 170, 50, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(off$v1, 30.882353 * 120 / 70, tolerance = 1e-6)
  expect_equal(off$cl3, 0.19260346 * 120 / 70, tolerance = 1e-6)
})

test_that("the time to peak effect is Roozekrans' model's, 5.8 min", {
  expect_equal(round(doxapramRoozekransTPeak(), 1), DOXAPRAM_TPEAK)
  # its standard error, 3.2 +- 2.0 min, spans about 3.5 to 8 min
  lo <- doxapramRoozekransTPeak(modifyList(DOXAPRAM_ROOZEKRANS, list(t12ke0 = 1.2)))
  hi <- doxapramRoozekransTPeak(modifyList(DOXAPRAM_ROOZEKRANS, list(t12ke0 = 5.2)))
  expect_true(lo > 3 && lo < 4.5 && hi > 7 && hi < 9)
  PK <- getDrugPK("doxapram", 70, 175, 35, "male")
  pk <- PK$PK$default
  coef <- c(pk$p_coef_bolus_l1, pk$p_coef_bolus_l2, pk$p_coef_bolus_l3)
  lambda <- c(pk$lambda_1, pk$lambda_2, pk$lambda_3)
  expect_equal(effectSitePeakTime(coef, lambda, pk$ke0), 5.8, tolerance = 1e-4)
})

test_that("Robson & Prescott's bolus area is reproduced (not fitted to it)", {
  # 1.5 mg/kg over 2.5 min to a 71-kg man; AUC 272 +- 101 ug min/mL (n = 6)
  dose <- data.frame(Drug = "doxapram", Time = c(0, 2.5),
                     Dose = c(1.5 * 71 / 2.5, 0), Units = "mg/min")
  wide <- simulateDrugsWithCovariates(
    dose, data.frame(Time = double(), Event = character()),
    71, 175, 35, "male", 6000, FALSE, adjustToFFM = FALSE)$doxapram$wide
  auc <- sum(diff(wide$Time) * (head(wide$Plasma, -1) + tail(wide$Plasma, -1)) / 2)
  expect_lt(abs(auc - 272), 101)
  # by construction, dose over clearance: 1.5 / sum(A/lambda) per kg
  expect_equal(auc, sum(DOXAPRAM_COEF / DOXAPRAM_EXP), tolerance = 0.02)
})

test_that("Komatsu's doubled stepped regimen gives what was measured, 2.4 to 2.6", {
  # 6, 4, 3, 2 mg/min/70 kg for 15, 15, 30, 60 min, then 1; 72 kg; measured
  # 2.4-2.6 ug/mL at thresholds reached over hours (times not printed)
  f <- 72 / 70
  dose <- data.frame(Drug = "doxapram", Time = c(0, 15, 30, 60, 120),
                     Dose = c(6, 4, 3, 2, 1) * f, Units = "mg/min")
  cp <- doxapramPlasma(dose, 72, 400, c(180, 240, 300, 360))
  expect_true(all(cp > 2.4 * 0.9 & cp < 2.6 * 1.15))
})

test_that("doxapram is plotted in mcg/mL, with the library's band, in Other", {
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "doxapram", ]
  expect_identical(row$Category, "Other")
  expect_identical(row$Concentration.Units, "mcg")
  expect_equal(c(row$Lower, row$Typical, row$Upper), c(1.5, 2, 3))
  expect_equal(row$MEAC, 0)
  expect_equal(row$endCe, 0)
})
