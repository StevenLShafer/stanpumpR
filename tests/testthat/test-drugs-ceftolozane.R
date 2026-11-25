# Ceftolozane (R/drugs_ceftolozane.R), drawn for Zerbaxa: Zhang 2021, Table 2
# and its printed equations.  Drafted by Claude Code, 2026-11-25.  The
# expectations are computed here from the paper's equations and the
# Cockcroft-Gault equation, not from the drug file.

events <- data.frame(Time = numeric(0), Event = character(0), Fill = character(0))
cg <- function(w, age, scr, female = FALSE)
  (140 - age) * w / (72 * scr) * (if (female) 0.85 else 1)
zhangCL <- function(crcl, cuti = 0, ciai = 0) 4.84 * (crcl / 100)^0.701 * 1.18^cuti * 1.43^ciai
zhangVc <- function(w, cuti = 0, ciai = 0, pneu = 0, oinf = 0)
  9.23 * (w / 70)^0.684 * 1.25^cuti * 1.59^ciai * 2.00^pneu * 2.14^oinf
zhangVp <- function(w) 4.78 * (w / 70)^0.484

test_that("Zhang 2021's equations, switch off, healthy", {
  set <- ceftolozane(85, 175, 60, "male", adjustToFFM = FALSE, creatinine = 1.2)$PK$default
  expect_equal(set$cl1, zhangCL(cg(85, 60, 1.2)) / 60)
  expect_equal(set$v1, zhangVc(85))
  expect_equal(set$v2, zhangVp(85))
  expect_equal(set$cl2, 3.13 / 60)
  set <- ceftolozane(60, 160, 45, "female", adjustToFFM = FALSE, creatinine = 0.7)$PK$default
  expect_equal(set$cl1, zhangCL(cg(60, 45, 0.7, TRUE)) / 60)
  expect_equal(set$v1, zhangVc(60))
})

test_that("each infection takes Zhang's factors", {
  f <- function(inf) ceftolozane(70, 170, 35, "male", adjustToFFM = FALSE,
                                 creatinine = 1, infection = inf)$PK$default
  crcl <- cg(70, 35, 1)
  expect_equal(f("none")$cl1, zhangCL(crcl) / 60)
  expect_equal(f("cUTI")$cl1, zhangCL(crcl, cuti = 1) / 60)
  expect_equal(f("cUTI")$v1, zhangVc(70, cuti = 1))
  expect_equal(f("cIAI")$cl1, zhangCL(crcl, ciai = 1) / 60)
  expect_equal(f("cIAI")$v1, zhangVc(70, ciai = 1))
  # pneumonia, ventilated or not, takes the pneumonia factor; clearance is the
  # healthy one
  expect_equal(f("pneumonia")$v1, zhangVc(70, pneu = 1))
  expect_equal(f("pneumoniaVentilated")$v1, zhangVc(70, pneu = 1))
  expect_equal(f("pneumoniaVentilated")$cl1, zhangCL(crcl) / 60)
  expect_equal(f("other")$v1, zhangVc(70, oinf = 1))
  # the peripheral volume and Q carry no infection term
  expect_equal(f("other")$v2, zhangVp(70))
  expect_equal(f("cIAI")$cl2, 3.13 / 60)
  expect_error(ceftolozane(70, 170, 35, "male", infection = "sepsis"), "Invalid infection")
})

test_that("the reference man receives the published values", {
  set <- ceftolozane(70, 170, 35, "male")$PK$default
  expect_equal(set$v1, 9.23)
  expect_equal(set$v2, 4.78)
  expect_equal(set$cl2, 3.13 / 60)
  expect_equal(set$cl1, zhangCL(cg(70, 35, 0.9)) / 60)
  expect_equal_rounded(5.286871, set$cl1 * 60)
})

test_that("with the switch on, the weight terms and Cockcroft-Gault read the pharmacokinetic weight", {
  size <- pkSizeFactors(120, 170, 50, "female", TRUE)
  set <- ceftolozane(120, 170, 50, "female", creatinine = 0.8)$PK$default
  expect_equal(set$v1, zhangVc(size$pkWeight))
  expect_equal(set$v2, zhangVp(size$pkWeight))
  expect_equal(set$cl1, zhangCL(cg(size$pkWeight, 50, 0.8, TRUE)) / 60)
  expect_equal(set$cl2, 3.13 / 60 * size$clearance)
  off <- ceftolozane(120, 170, 50, "female", adjustToFFM = FALSE, creatinine = 0.8)$PK$default
  expect_equal(off$v1, zhangVc(120))
  expect_equal(off$cl2, 3.13 / 60)
})

test_that("pregnancy raises the creatinine clearance only when the creatinine is blank", {
  base <- ceftolozane(70, 165, 30, "female")$PK$default
  preg <- ceftolozane(70, 165, 30, "female", pregnant = TRUE)$PK$default
  expect_equal(preg$cl1 / base$cl1, PREGNANCY_GFR_FACTOR^0.701)
  expect_equal(preg$v1, base$v1)
  measured <- ceftolozane(70, 165, 30, "female", creatinine = 0.6, pregnant = TRUE)$PK$default
  expect_equal(measured$cl1,
               ceftolozane(70, 165, 30, "female", creatinine = 0.6)$PK$default$cl1)
})

test_that("the label's healthy clearance and Vss", {
  # Zerbaxa label sec. 12.3: plasma CL 4.10-6.73 L/h and Vss 13.5 L (21% CV)
  # after 1.5 g in healthy adult men
  set <- ceftolozane(70, 170, 35, "male")$PK$default
  expect_true(set$cl1 * 60 > 4.10 && set$cl1 * 60 < 6.73)
  expect_equal(set$v1 + set$v2, 13.5, tolerance = 0.05)
})

test_that("Zerbaxa is simulated as 2/3 of its weight of ceftolozane", {
  starts <- seq(0, 7 * 480, 480)
  dt <- data.frame(Drug = "Zerbaxa", Time = c(rbind(starts, starts + 60)),
                   Dose = c(rbind(1.5, 0)), Units = "g/hr")
  direct <- dt
  direct$Drug <- "ceftolozane"
  direct$Dose <- direct$Dose * 2 / 3
  a <- simulateDrugsWithCovariates(dt, events, 75, 170, 50, "male", 8 * 480, FALSE,
                                   infection = "cUTI")
  b <- simulateDrugsWithCovariates(direct, events, 75, 170, 50, "male", 8 * 480, FALSE,
                                   infection = "cUTI")
  expect_identical(names(a), "ceftolozane")
  expect_equal(a$ceftolozane$wide, b$ceftolozane$wide)
  # The label's Table 10, cIAI and cUTI at steady state on 1.5 g every 8 h:
  # Cmax 65.7 mcg/mL, AUC0-8 186 mcg.h/mL.  A 50-year-old 75-kg man with cUTI:
  w <- a$ceftolozane$wide
  last <- w$Time >= 7 * 480
  auc <- sum(diff(w$Time[last]) * (head(w$Plasma[last], -1) + tail(w$Plasma[last], -1)) / 2) / 60
  expect_equal(max(w$Plasma[last]), 65.7, tolerance = 0.05)
  expect_equal(auc, 186, tolerance = 0.10)
})

test_that("Zerbaxa offers the label's units and refuses a TCI target", {
  dd <- getDrugDefaultsGlobal()
  units <- strsplit(as.character(dd$Units[dd$Drug == "Zerbaxa"]), ",")[[1]]
  expect_true(all(c("g", "g tid", "mg/kg", "mg/kg tid", "g/hr", "mg/kg/hr") %in% units))
  expect_true(all(combinationUnitOk("Zerbaxa", units)))
  expect_false(combinationUnitOk("Zerbaxa", "Plasma target"))
  expect_false(combinationUnitOk("Zerbaxa", "tablet PO"))
  expect_identical(combinationComponents("Zerbaxa"), "ceftolozane")
  expect_error(expandCombinationProducts(data.frame(Drug = "Zerbaxa", Time = 0, Dose = 1,
                                                    Units = "Plasma target")),
               "units of mass")
  # mg/kg of the product becomes mg/kg of ceftolozane, as the label's
  # "30 mg/kg ... provides 20 mg/kg ceftolozane"
  x <- expandCombinationProducts(data.frame(Drug = "Zerbaxa", Time = 0, Dose = 30,
                                            Units = "mg/kg tid"))
  expect_equal(x$Dose, 20)
  expect_identical(as.character(x$Units), "mg/kg tid")
  expect_identical(combinationProductOf("ceftolozane", data.frame(Drug = "Zerbaxa")), "Zerbaxa")
})

test_that("the MIC line is free ceftolozane at 4 mg/L, with no band", {
  mic <- antibioticMic("ceftolozane")
  expect_equal(mic$MIC, 4)
  expect_equal(mic$Threshold, 4 / 0.80)
  expect_null(antibioticMbcBand("ceftolozane"))
  expect_true("ceftolozane" %in% antibioticNoBands()$Drug)
})
