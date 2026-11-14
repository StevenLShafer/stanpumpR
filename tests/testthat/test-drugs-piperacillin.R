# Piperacillin (R/drugs_piperacillin.R): Kong 2025's pooled two-compartment
# model.  Drafted by Claude Code, 2026-11-13.  Expectations are computed here
# from the source's NONMEM control stream (ESM3) and Table 3, not from the
# drug file.

kong <- function(wt, pmaYears, scr) {
  ref <- 35 + 40 / 52
  mat <- function(p) p^3.35 / (p^3.35 + (54.2 / 52)^3.35)
  dec <- function(p) 1 - p^1.92 / (p^1.92 + 89.1^1.92)
  std <- function(p) exp(1.42 - 1.17 * (p / 100)^-0.5 - 0.203 * (p / 100)^-0.5 * log(p / 100))
  10.6 * (wt / 70)^0.75 * mat(pmaYears) / mat(ref) * dec(pmaYears) / dec(ref) *
    exp(-0.346 * (scr - std(pmaYears)))
}

test_that("the reference values of the abstract and the Discussion", {
  ref <- 35 + 40 / 52
  expect_equal(round(piperacillinScrStd(ref), 3), 0.829)        # "SCR of 0.83"
  expect_equal(round(piperacillinScrStd(30 + 40 / 52), 3), 0.773)
  expect_equal(piperacillinClearance(70, ref, piperacillinScrStd(ref)), 10.6)
  expect_equal(round(piperacillinClearance(70, 30 + 40 / 52, piperacillinScrStd(30 + 40 / 52)), 1), 11.0)
})

test_that("the control stream's equations, switch off", {
  set <- piperacillin(70, 170, 35, "male", adjustToFFM = FALSE,
                      creatinine = piperacillinScrStd(35 + 40 / 52))$PK$default
  expect_equal(set$v1, 10.4)
  expect_equal(set$v2, 11.6)
  expect_equal(set$cl2, 15.2 / 60)
  expect_equal(set$cl1, 10.6 / 60)
  set <- piperacillin(55, 160, 72, "female", adjustToFFM = FALSE, creatinine = 1.6)$PK$default
  expect_equal(set$cl1, kong(55, 72 + 40 / 52, 1.6) / 60)
  expect_equal(set$v1, 10.4 * 55 / 70)
  expect_equal(set$v2, 11.6 * 55 / 70)
  expect_equal(set$cl2, 15.2 * (55 / 70)^0.75 / 60)
})

test_that("a premature infant: postmenstrual age from the gestational age", {
  set <- piperacillin(2, 45, 2 / 52, "female", adjustToFFM = FALSE,
                      creatinine = 0.6, gestationalAge = 30)$PK$default
  expect_equal(set$cl1, kong(2, (30 + 2) / 52, 0.6) / 60)
  expect_true(is.finite(set$cl1) && set$cl1 > 0)
})

test_that("the creatinine is held at the adults' highest, 5.82 mg/dL", {
  at <- piperacillin(70, 170, 50, "male", adjustToFFM = FALSE, creatinine = 5.82)$PK$default$cl1
  above <- piperacillin(70, 170, 50, "male", adjustToFFM = FALSE, creatinine = 12)$PK$default$cl1
  expect_equal(above, at)
  expect_equal(at, kong(70, 50 + 40 / 52, 5.82) / 60)
})

test_that("switch on: the weight terms at the pharmacokinetic weight", {
  # 120 kg, 170 cm, 50 y man: fat-free-mass factor 1.3049067, so the
  # pharmacokinetic weight is 70 x 1.3049067 = 91.343469 kg (as the other
  # drugs' tests pin it), worked out here rather than by pkSizeFactors()
  w <- 70 * 1.3049067
  set <- piperacillin(120, 170, 50, "male", creatinine = 1)$PK$default
  expect_equal_rounded(set$cl1, kong(w, 50 + 40 / 52, 1) / 60)
  expect_equal_rounded(set$v1, 10.4 * 1.3049067)
  expect_equal_rounded(set$v2, 11.6 * 1.3049067)
  expect_equal_rounded(set$cl2, 15.2 * 1.3049067^0.75 / 60)
})

test_that("pregnancy: the assumed creatinine divided by 1.5 only when blank", {
  scr <- patientCreatinine(NULL, 30, "female")
  preg <- piperacillin(70, 165, 30, "female", adjustToFFM = FALSE, pregnant = TRUE)$PK$default$cl1
  expect_equal(preg, kong(70, 30 + 40 / 52, scr / PREGNANCY_GFR_FACTOR) / 60)
  expect_equal(piperacillin(70, 165, 30, "female", creatinine = 0.6, pregnant = TRUE)$PK$default$cl1,
               piperacillin(70, 165, 30, "female", creatinine = 0.6)$PK$default$cl1)
})

test_that("Zosyn doses: the product's weight carries 4/4.5 piperacillin", {
  expect_true(all(piperacillinTazobactamUnits %in% allUnits))
  expect_true(all(doseRoute(piperacillinTazobactamUnits) == ROUTE_IV))
  expect_equal(isRateUnit(piperacillinTazobactamUnits), c(FALSE, FALSE, TRUE, TRUE))
  X <- piperacillin(70, 170, 40, "male")
  expect_equal(doseProductFactor(c("mg pip-tazo", "mg", "mg pip-tazo qid"),
                                 X$doseProducts), c(4 / 4.5, 1, 4 / 4.5))
  # 4.5 g of Zosyn plots the same as 4 g of piperacillin
  run <- function(dose, units) {
    dt <- data.frame(Drug = "piperacillin", Time = 0, Dose = dose, Units = units)
    events <- data.frame(Time = numeric(0), Event = character(0))
    simulateDrugsWithCovariates(dt, events, 70, 170, 40, "male", 480, FALSE)$piperacillin$wide
  }
  a <- run(4500, "mg pip-tazo")
  b <- run(4000, "mg")
  expect_equal(a, b)
})

test_that("the label's healthy-subject clearance (not fitted)", {
  # Zosyn label Table 7: CL 210 mL/min = 12.6 L/h after 4 g piperacillin
  cl <- piperacillin(75, 178, 25, "male", adjustToFFM = FALSE, creatinine = 0.9)$PK$default$cl1 * 60
  expect_equal(cl, 12.6, tolerance = 0.15)
})

test_that("the MIC line and the MBC band", {
  mic <- antibioticMic("piperacillin")
  expect_equal(c(mic$MIC, mic$FreeFraction, mic$Threshold), c(8, 0.70, 11.4))
  band <- antibioticMbcBand("piperacillin")
  expect_equal(unname(band$free), c(2, 16, 32))
  expect_equal(unname(band$plotted), c(2.9, 23, 46))
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "piperacillin", ]
  expect_equal(c(row$Lower, row$Typical, row$Upper, row$endCe), c(2.9, 23, 46, 11.4))
})
