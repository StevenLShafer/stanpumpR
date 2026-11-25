# Imipenem (R/drugs_imipenem.R): Patel 2022, Table 3 and its footnote.
# Drafted by Claude Code, 2026-11-25.  The expectations are computed here from
# the paper's equations and the Cockcroft-Gault equation, not from the drug
# file.

events <- data.frame(Time = numeric(0), Event = character(0), Fill = character(0))
cg <- function(w, age, scr, female = FALSE)
  (140 - age) * w / (72 * scr) * (if (female) 0.85 else 1)
patelCL <- function(crcl, w, pneu = 0)
  12.68 * (crcl / 105.5)^0.48 * (w / 75)^0.29 * (1 - 0.38 * pneu)
patelV1 <- function(w, pneu = 0, vent = 0)
  11.39 * (w / 75)^1.03 * (1 - 0.39 * pneu) * (1 + 0.23 * vent)

test_that("Patel 2022's equations, switch off, healthy", {
  set <- imipenem(85, 175, 60, "male", adjustToFFM = FALSE, creatinine = 1.2)$PK$default
  expect_equal(set$cl1, patelCL(cg(85, 60, 1.2), 85) / 60)
  expect_equal(set$v1, patelV1(85))
  expect_equal(set$v2, 7.79)
  expect_equal(set$cl2, 23.07 / 60)
  set <- imipenem(60, 160, 45, "female", adjustToFFM = FALSE, creatinine = 0.7)$PK$default
  expect_equal(set$cl1, patelCL(cg(60, 45, 0.7, TRUE), 60) / 60)
  expect_equal(set$v1, patelV1(60))
})

test_that("each infection takes Patel's factors", {
  f <- function(inf) imipenem(70, 170, 35, "male", adjustToFFM = FALSE,
                              creatinine = 1, infection = inf)$PK$default
  crcl <- cg(70, 35, 1)
  for (ref in c("none", "cUTI", "cIAI", "other")) {
    expect_equal(f(ref)$cl1, patelCL(crcl, 70) / 60, info = ref)
    expect_equal(f(ref)$v1, patelV1(70), info = ref)
  }
  expect_equal(f("pneumonia")$cl1, patelCL(crcl, 70, pneu = 1) / 60)
  expect_equal(f("pneumonia")$v1, patelV1(70, pneu = 1))
  expect_equal(f("pneumoniaVentilated")$cl1, patelCL(crcl, 70, pneu = 1) / 60)
  expect_equal(f("pneumoniaVentilated")$v1, patelV1(70, pneu = 1, vent = 1))
  expect_equal(f("pneumonia")$v2, 7.79)
  expect_equal(f("pneumonia")$cl2, 23.07 / 60)
  expect_error(imipenem(70, 170, 35, "male", infection = "sepsis"), "Invalid infection")
})

test_that("the reference man, and the switch on reads the pharmacokinetic weight", {
  set <- imipenem(70, 170, 35, "male")$PK$default
  expect_equal(set$cl1, patelCL(cg(70, 35, 0.9), 70) / 60)
  expect_equal(set$v1, patelV1(70))
  expect_equal(set$v2, 7.79)
  expect_equal(set$cl2, 23.07 / 60)
  expect_equal_rounded(12.86858, set$cl1 * 60)
  size <- pkSizeFactors(120, 170, 50, "female", TRUE)
  w <- size$pkWeight
  on <- imipenem(120, 170, 50, "female", creatinine = 0.8)$PK$default
  expect_equal(on$cl1, patelCL(cg(w, 50, 0.8, TRUE), w) / 60)
  expect_equal(on$v1, patelV1(w))
  expect_equal(on$v2, 7.79 * size$volume)
  expect_equal(on$cl2, 23.07 / 60 * size$clearance)
  off <- imipenem(120, 170, 50, "female", adjustToFFM = FALSE, creatinine = 0.8)$PK$default
  expect_equal(off$v1, patelV1(120))
  expect_equal(off$v2, 7.79)
})

test_that("pregnancy raises the creatinine clearance only when the creatinine is blank", {
  base <- imipenem(70, 165, 30, "female")$PK$default
  preg <- imipenem(70, 165, 30, "female", pregnant = TRUE)$PK$default
  expect_equal(preg$cl1 / base$cl1, PREGNANCY_GFR_FACTOR^0.48)
  expect_equal(preg$v1, base$v1)
  expect_equal(imipenem(70, 165, 30, "female", creatinine = 0.6, pregnant = TRUE)$PK,
               imipenem(70, 165, 30, "female", creatinine = 0.6)$PK)
})

test_that("the RECARBRIO label's steady-state exposures (the source model's own means)", {
  # RECARBRIO label Table 7, patients with CLcr >= 90 mL/min, 500 mg every 6 h
  # over 30 min: AUC0-24 570.6 uM.h (cUTI/cIAI) and 771 uM.h (HABP/VABP),
  # imipenem 299.35 g/mol.  A 50-year-old, 75-kg man with the assumed
  # creatinine.
  starts <- seq(0, 11 * 360, 360)
  dt <- data.frame(Drug = "imipenem", Time = c(rbind(starts, starts + 30)),
                   Dose = c(rbind(1000, 0)), Units = "mg/hr")
  auc24 <- function(inf) {
    w <- simulateDrugsWithCovariates(dt, events, 75, 170, 50, "male", 12 * 360, FALSE,
                                     infection = inf)$imipenem$wide
    last <- w$Time >= 11 * 360
    4 * sum(diff(w$Time[last]) * (head(w$Plasma[last], -1) + tail(w$Plasma[last], -1)) / 2) / 60
  }
  expect_equal(auc24("cUTI"), 570.6 * 0.29935, tolerance = 0.10)
  expect_equal(auc24("pneumonia"), 771 * 0.29935, tolerance = 0.20)
})

test_that("imipenem offers the PRIMAXIN units, and the MIC line is free imipenem at 2 mg/L", {
  dd <- getDrugDefaultsGlobal()
  units <- unlist(dd$Units[dd$Drug == "imipenem"])
  expect_true(all(c("mg", "mg qid", "mg tid", "mg/kg qid", "mg/hr") %in% units))
  mic <- antibioticMic("imipenem")
  expect_equal(mic$MIC, 2)
  expect_equal(mic$FreeFraction, 0.80)
  expect_equal(mic$Threshold, 2.5)
  expect_null(antibioticMbcBand("imipenem"))
  expect_true("imipenem" %in% antibioticNoBands()$Drug)
})
