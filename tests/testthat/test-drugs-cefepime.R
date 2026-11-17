# Cefepime (R/drugs_cefepime.R): Barreto 2023, Table 3 and Table S2, with the
# 2012 CKD-EPI creatinine eGFR in place of the creatinine-cystatin C one
# (Shafer, 2026-11-17).  Drafted by Claude Code, 2026-11-17.  Expectations are
# computed here from the paper's equations and the CKD-EPI 2009/2012
# creatinine equation (Table S1), not from the drug file.

ckdepi <- function(scr, age, female = FALSE) {
  k <- if (female) 0.7 else 0.9
  a <- if (female) -0.329 else -0.411
  141 * min(scr / k, 1)^a * max(scr / k, 1)^-1.209 * 0.993^age * (if (female) 1.018 else 1)
}
dubois <- function(w, h) 0.007184 * w^0.425 * h^0.725
clBarreto <- function(egfrMlMin) 7.84 * (egfrMlMin * 60 / 1000) / 7.2   # Table S2

test_that("Barreto 2023's equations, switch off", {
  set <- cefepime(80, 175, 58, "male", adjustToFFM = FALSE, creatinine = 1.1)$PK$default
  egfr <- ckdepi(1.1, 58) * dubois(80, 175) / 1.73
  expect_equal(set$cl1, clBarreto(egfr) / 60)
  expect_equal(set$v1, 15.56 * 80 / 70)
  expect_equal(set$v2, 10.63)
  expect_equal(set$cl2, 7.09 / 60)
  set <- cefepime(60, 160, 75, "female", adjustToFFM = FALSE, creatinine = 0.8)$PK$default
  egfr <- ckdepi(0.8, 75, TRUE) * dubois(60, 160) / 1.73
  expect_equal(set$cl1, clBarreto(egfr) / 60)
  expect_equal(set$v1, 15.56 * 60 / 70)
})

test_that("clearance is 7.84 L/h at an eGFR of 120 mL/min, in proportion to it", {
  expect_equal(cefepimeClearance(120), 7.84)
  expect_equal(cefepimeClearance(60), 3.92)
})

test_that("clearance is held at the label's 0.92 L/h on hemodialysis", {
  expect_equal(round(CEFEPIME_CL_MIN, 3), 0.924)   # 18 L x ln 2 / 13.5 h
  expect_equal(cefepimeClearance(5), CEFEPIME_CL_MIN)
  expect_equal(round(CEFEPIME_CL_MIN / 7.84 * 120, 1), 14.1)   # the eGFR where it binds
  high <- cefepime(70, 170, 35, "male", creatinine = MAX_CREATININE)$PK$default
  expect_equal(high$cl1, CEFEPIME_CL_MIN / 60)
})

test_that("switch on: weight and body surface area at the pharmacokinetic weight", {
  # 120 kg, 170 cm, 50 y man: pharmacokinetic weight 70 x 1.3049067 kg, as
  # the other drugs' tests pin it
  pkW <- 70 * 1.3049067
  set <- cefepime(120, 170, 50, "male", creatinine = 1)$PK$default
  expect_equal_rounded(set$v1, 15.56 * pkW / 70)
  expect_equal_rounded(set$cl1, clBarreto(ckdepi(1, 50) * dubois(pkW, 170) / 1.73) / 60)
})

test_that("the reference man, against the label's healthy men (not fitted)", {
  p <- cefepime(70, 170, 35, "male")$PK$default
  # clearance 126 mL/min against the label's 120 (+/- 8)
  expect_equal(round(p$cl1 * 1000), 126)
  conc <- function(p, dose, t, tinf = 30) {
    A <- matrix(c(-(p$cl1 + p$cl2) / p$v1, p$cl2 / p$v1,
                  p$cl2 / p$v2, -p$cl2 / p$v2), 2, byrow = TRUE)
    e <- eigen(A); V <- e$vectors; l <- e$values; Vi <- solve(V)
    during <- function(s) V %*% diag((exp(l * s) - 1) / l) %*% Vi %*% c(dose / tinf, 0)
    x <- if (t <= tinf) during(t) else V %*% diag(exp(l * (t - tinf))) %*% Vi %*% during(tinf)
    x[1] / p$v1
  }
  expect_equal(round(conc(p, 1000, 30), 1), 51.9)
  expect_equal(round(conc(p, 2000, 30), 1), 103.7)
  expect_equal(round(1000 / (p$cl1 * 60), 1), 132.7)
  expect_equal(p$v1 + p$v2, 26.19)
})

test_that("pregnancy raises the eGFR only with a blank creatinine", {
  base <- cefepime(70, 165, 30, "female")$PK$default$cl1
  preg <- cefepime(70, 165, 30, "female", pregnant = TRUE)$PK$default$cl1
  expect_equal(preg, base * PREGNANCY_GFR_FACTOR)
  expect_equal(cefepime(70, 165, 30, "female", creatinine = 0.6, pregnant = TRUE)$PK$default$cl1,
               cefepime(70, 165, 30, "female", creatinine = 0.6)$PK$default$cl1)
})

test_that("the MIC line and no band", {
  mic <- antibioticMic("cefepime")
  expect_equal(c(mic$MIC, mic$FreeFraction, mic$Threshold), c(8, 0.80, 10))
  expect_true("cefepime" %in% antibioticNoBands()$Drug)
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "cefepime", ]
  expect_equal(c(row$Lower, row$Typical, row$Upper, row$endCe), c(0, 0, 0, 10))
  expect_identical(therapeuticRange("cefepime")$band_status, "none_established")
})
