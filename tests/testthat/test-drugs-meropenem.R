# Meropenem (R/drugs_meropenem.R): the meropenem model of the FDA's review of
# NDA 209776 (Vabomere).  Drafted by Claude Code, 2026-11-13.  Expectations are
# computed here from the review's equations (1)-(3) and Table 4.2-2, not from
# the drug file.

mdrd <- function(scr, age, female = FALSE)
  175 * scr^-1.154 * age^-0.203 * (if (female) 0.742 else 1)
clReview <- function(egfr, age) {
  clnr <- 3.78 * (if (egfr <= 30) 0.349 else 1)
  (clnr + 6.60 * egfr^1.94 / (40.8^1.94 + egfr^1.94)) * (age / 58)^-0.430
}

test_that("the review's equations, switch off", {
  # a 58-year-old 80-kg man: the reference values of the age and weight terms
  set <- meropenem(80, 175, 58, "male", adjustToFFM = FALSE, creatinine = 1.1)$PK$default
  expect_equal(set$cl1, clReview(mdrd(1.1, 58), 58) / 60)
  expect_equal(set$v1, 17.4)
  expect_equal(set$v2, 2.50)
  expect_equal(set$cl2, 1.52 / 60)
  # another weight and age
  set <- meropenem(60, 160, 75, "female", adjustToFFM = FALSE, creatinine = 1.4)$PK$default
  expect_equal(set$cl1, clReview(mdrd(1.4, 75, TRUE), 75) / 60)
  expect_equal(set$v1, 17.4 * (60 / 80)^0.487)
  expect_equal(set$v2, 2.50 * (60 / 80)^0.324)
})

test_that("the published checks (Trang 2021, Discussion)", {
  # "about 10 liters/h in a 58-year-old patient with an eGFR of 100"
  expect_equal(round(meropenemClearance(100, 58), 1), 9.4)
  # "approximately 7 liters/h" at an eGFR of 40
  expect_equal(round(meropenemClearance(40, 58), 1), 7.0)
  # renal fraction "approximately 59%" at 100
  expect_equal(round(6.60 * 100^1.94 / (40.8^1.94 + 100^1.94) / meropenemClearance(100, 58), 2), 0.60)
})

test_that("non-renal clearance falls to 0.349 at or below an eGFR of 30", {
  expect_equal(meropenemClearance(30, 58),
               3.78 * 0.349 + 6.60 * 30^1.94 / (40.8^1.94 + 30^1.94))
  expect_equal(meropenemClearance(31, 58),
               3.78 + 6.60 * 31^1.94 / (40.8^1.94 + 31^1.94))
  expect_lt(meropenemClearance(30, 58), meropenemClearance(30.001, 58))
})

test_that("the floors: creatinine 0.5 mg/dL, age 18", {
  lo <- meropenem(70, 170, 35, "male", adjustToFFM = FALSE, creatinine = 0.2)$PK$default
  at <- meropenem(70, 170, 35, "male", adjustToFFM = FALSE, creatinine = 0.5)$PK$default
  expect_equal(lo$cl1, at$cl1)
  young <- meropenem(20, 120, 6, "male", adjustToFFM = FALSE, creatinine = 0.9)$PK$default
  expect_true(is.finite(young$cl1) && young$cl1 > 0)
  expect_equal(meropenemClearance(96, 6), meropenemClearance(96, 18))
})

test_that("switch on: the weight terms at the pharmacokinetic weight", {
  # 120 kg, 170 cm, 50 y man: fat-free-mass factor 1.3049067, so the
  # pharmacokinetic weight is 70 x 1.3049067 = 91.343469 kg (as the other
  # drugs' tests pin it), worked out here rather than by pkSizeFactors()
  pkW <- 70 * 1.3049067
  set <- meropenem(120, 170, 50, "male", creatinine = 1)$PK$default
  expect_equal_rounded(set$v1, 17.4 * (pkW / 80)^0.487)
  expect_equal_rounded(set$v2, 2.50 * (pkW / 80)^0.324)
  # CL and CLd take the clearance factor, 1.3049067^0.75 = 1.2209126 (the
  # first-principles rule, 2026-11-19), anchored at Trang's Table 1 cohort
  cf <- cohortFactorsIndependent(MEROPENEM_COHORT)
  expect_equal_rounded(set$cl1, clReview(mdrd(1, 50), 50) / 60 * 1.2209126 / cf$clearance)
  expect_equal_rounded(set$cl2, 1.52 / 60 * 1.2209126 / cf$clearance)
})

test_that("size-free CL and CLd: anchored at Trang's Table 1, scaled by FFM^0.75", {
  # (a) The anchor is Trang 2021's Table 1 (75 kg, 168 cm, 53 years; 192 men,
  # 239 women; see BODY SIZE ANCHOR).  CL carries no sex term, so the pooled
  # (geometric, by numbers) man and woman receive the published CLd, and the
  # published CL at each one's own eGFR
  cf <- cohortFactorsIndependent(MEROPENEM_COHORT)
  expect_equal(round(c(cf$volume, cf$clearance), 4), c(0.9221, 0.9410))
  m <- meropenem(75, 168, 53, "male", creatinine = 1)$PK$default
  f <- meropenem(75, 168, 53, "female", creatinine = 1)$PK$default
  pooled <- function(x, y) exp((192 * log(x) + 239 * log(y)) / 431)
  expect_equal(pooled(m$cl2, f$cl2), 1.52 / 60)
  expect_equal(pooled(m$cl1 / clReview(mdrd(1, 53), 53),
                      f$cl1 / clReview(mdrd(1, 53, TRUE), 53)), 1 / 60)
  # the volumes keep the source's own weight terms, at each one's own
  # pharmacokinetic weight
  expect_equal(m$v1, 17.4 * (pkSizeFactors(75, 168, 53, "male")$pkWeight / 80)^0.487)
  # (b) a man of twice the reference fat-free mass: CL and CLd x 2^0.75; the
  # volumes keep the source's own weight exponents at 140 kg pharmacokinetic
  w <- uniroot(function(w) ffmAlSallami(w, 200, 35, "male") - 2 * FFM_REFERENCE,
               c(70, 400), tol = 1e-12)$root
  big <- meropenem(w, 200, 35, "male", creatinine = 1)$PK$default
  expect_equal(big$cl1, clReview(mdrd(1, 35), 35) / 60 * 2^0.75 / cf$clearance)
  expect_equal(big$cl2, 1.52 / 60 * 2^0.75 / cf$clearance)
  expect_equal(big$v1, 17.4 * (140 / 80)^0.487)
  expect_equal(big$v2, 2.50 * (140 / 80)^0.324)
  # (c) the switch off: as published, at total weight
  off <- meropenem(w, 200, 35, "male", adjustToFFM = FALSE, creatinine = 1)$PK$default
  expect_equal(off$cl1, clReview(mdrd(1, 35), 35) / 60)
  expect_equal(off$cl2, 1.52 / 60)
  expect_equal(off$v1, 17.4 * (w / 80)^0.487)
})

test_that("pregnancy raises the eGFR only with a blank creatinine", {
  base <- meropenem(70, 165, 30, "female")$PK$default$cl1
  preg <- meropenem(70, 165, 30, "female", pregnant = TRUE)$PK$default$cl1
  scr <- adultEquivalentCreatinine(NULL, 30, "female")
  # with the switch on CL takes the clearance factor (2026-11-19)
  sizeCl <- (ffmAlSallami(70, 165, 30, "female") / FFM_REFERENCE)^0.75 /
    cohortFactorsIndependent(MEROPENEM_COHORT)$clearance
  expect_equal(preg, clReview(mdrd(scr, 30, TRUE) * PREGNANCY_GFR_FACTOR, 30) / 60 * sizeCl)
  expect_equal(meropenem(70, 165, 30, "female", creatinine = 0.6, pregnant = TRUE)$PK$default$cl1,
               meropenem(70, 165, 30, "female", creatinine = 0.6)$PK$default$cl1)
  expect_gt(preg, base)
})

test_that("the Merrem label's healthy-volunteer peaks (not fitted)", {
  # an independent two-compartment solution of a 30-min infusion
  conc <- function(p, dose, t, tinf = 30) {
    A <- matrix(c(-(p$cl1 + p$cl2) / p$v1, p$cl2 / p$v1,
                  p$cl2 / p$v2, -p$cl2 / p$v2), 2, byrow = TRUE)
    e <- eigen(A); V <- e$vectors; l <- e$values; Vi <- solve(V)
    during <- function(s) V %*% diag((exp(l * s) - 1) / l) %*% Vi %*% c(dose / tinf, 0)
    x <- if (t <= tinf) during(t) else V %*% diag(exp(l * (t - tinf))) %*% Vi %*% during(tinf)
    x[1] / p$v1
  }
  p <- meropenem(70, 170, 35, "male")$PK$default
  expect_gte(conc(p, 1000, 30), 39); expect_lte(conc(p, 1000, 30), 58)   # 49 (39-58)
  expect_gte(conc(p, 500, 30), 14);  expect_lte(conc(p, 500, 30), 26)    # 23 (14-26)
  # "about 1 at 6 h": the published model (switch off) gives 0.80; with the
  # switch on the reference man's CL and CLd are anchored at Trang's cohort
  # (x 1/0.9410, 2026-11-19), and he gives 0.67, a third below the label
  off <- meropenem(70, 170, 35, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(conc(off, 500, 360), 1, tolerance = 0.25)
  expect_equal(round(conc(off, 500, 360), 2), 0.80)
  expect_equal(round(conc(p, 500, 360), 2), 0.67)
})

test_that("the MIC line and the MBC band", {
  mic <- antibioticMic("meropenem")
  expect_equal(c(mic$MIC, mic$FreeFraction, mic$Threshold), c(2, 0.98, 2))
  band <- antibioticMbcBand("meropenem")
  expect_equal(unname(band$free), c(0.03, 0.12, 16))
  expect_equal(unname(band$plotted), c(0.031, 0.12, 16))
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "meropenem", ]
  expect_equal(c(row$Lower, row$Typical, row$Upper, row$endCe), c(0.031, 0.12, 16, 2))
})
