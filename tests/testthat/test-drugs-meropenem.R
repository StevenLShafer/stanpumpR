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
  pkW <- pkSizeFactors(120, 170, 50, "male", TRUE)$pkWeight
  set <- meropenem(120, 170, 50, "male", creatinine = 1)$PK$default
  expect_equal(set$v1, 17.4 * (pkW / 80)^0.487)
  expect_equal(set$v2, 2.50 * (pkW / 80)^0.324)
  expect_equal(set$cl1, clReview(mdrd(1, 50), 50) / 60)
})

test_that("pregnancy raises the eGFR only with a blank creatinine", {
  base <- meropenem(70, 165, 30, "female")$PK$default$cl1
  preg <- meropenem(70, 165, 30, "female", pregnant = TRUE)$PK$default$cl1
  scr <- adultEquivalentCreatinine(NULL, 30, "female")
  expect_equal(preg, clReview(mdrd(scr, 30, TRUE) * PREGNANCY_GFR_FACTOR, 30) / 60)
  expect_equal(meropenem(70, 165, 30, "female", creatinine = 0.6, pregnant = TRUE)$PK$default$cl1,
               meropenem(70, 165, 30, "female", creatinine = 0.6)$PK$default$cl1)
  expect_gt(preg, base)
})
