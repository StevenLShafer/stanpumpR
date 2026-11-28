# Ceftazidime (R/drugs_ceftazidime.R), drawn for Avycaz: Franzese 2022, the
# control stream of its final ceftazidime model and Table S2.  Drafted by
# Claude Code, 2026-11-25.  The expectations are computed here from the
# source's equations, not from the drug file.

events <- data.frame(Time = numeric(0), Event = character(0), Fill = character(0))
cg <- function(w, age, scr, female = FALSE)
  (140 - age) * w / (72 * scr) * (if (female) 0.85 else 1)
mosteller <- function(w, h) sqrt(w * h / 3600)
renal <- function(n) {
  n <- min(n, 150)
  if (n < 100) 0.0103036 * n else 0.0103036 * 100 + 0.00125182 * (n - 100)
}
wtCL <- function(w) (w / 70) * (53.5 + 70) / (53.5 + w)
franzeseCL <- function(w, r, inf = 1) 7.75 * wtCL(w) * r * inf

test_that("Franzese 2022's equations in an adult, switch off", {
  set <- ceftazidime(85, 175, 60, "male", adjustToFFM = FALSE, creatinine = 1.2)$PK$default
  ncrcl <- cg(85, 60, 1.2) * 1.73 / mosteller(85, 175)
  expect_equal(set$cl1, franzeseCL(85, renal(ncrcl)) / 60)
  expect_equal(set$v1, 11.2 * 85 / 70)
  expect_equal(set$v2, 6.52 * 85 / 70)
  expect_equal(set$cl2, 5.33 * (85 / 70)^0.67 / 60)
  # a normalised clearance below the inflection, and one above the cap
  low <- ceftazidime(60, 160, 70, "female", adjustToFFM = FALSE, creatinine = 2)$PK$default
  n <- cg(60, 70, 2, TRUE) * 1.73 / mosteller(60, 160)
  expect_lt(n, 100)
  expect_equal(low$cl1, franzeseCL(60, 0.0103036 * n) / 60)
  high <- ceftazidime(70, 170, 20, "male", adjustToFFM = FALSE, creatinine = 0.4)$PK$default
  expect_equal(high$cl1, franzeseCL(70, renal(150)) / 60)
})

test_that("children: bedside Schwartz from two years, maturation at two and below", {
  set <- ceftazidime(25, 125, 8, "female", adjustToFFM = FALSE, creatinine = 0.4)$PK$default
  expect_equal(set$cl1, franzeseCL(25, renal(0.413 * 125 / 0.4)) / 60)
  expect_equal(set$v1, 11.2 * 25 / 70)
  inf <- ceftazidime(8, 70, 0.5, "male", adjustToFFM = FALSE, creatinine = 0.3)$PK$default
  pma <- 40 + 52 * 0.5
  expect_equal(inf$cl1, franzeseCL(8, pma^3.4 / (47.7^3.4 + pma^3.4)) / 60)
  # the creatinine is not read at two years and below; the gestational age is
  expect_equal(ceftazidime(8, 70, 0.5, "male", adjustToFFM = FALSE, creatinine = 1)$PK$default$cl1,
               inf$cl1)
  pre <- ceftazidime(8, 70, 0.5, "male", adjustToFFM = FALSE, gestationalAge = 30)$PK$default
  pma <- 30 + 52 * 0.5
  expect_equal(pre$cl1, franzeseCL(8, pma^3.4 / (47.7^3.4 + pma^3.4)) / 60)
})

test_that("each infection takes Franzese's factors", {
  f <- function(inf) ceftazidime(70, 170, 35, "male", adjustToFFM = FALSE,
                                 creatinine = 1, infection = inf)$PK$default
  base <- f("none")
  expect_equal(f("cUTI")$cl1, base$cl1)
  expect_equal(f("cUTI")$v1, base$v1 * 1.49)
  expect_equal(f("cIAI")$cl1, base$cl1 * 1.33)
  expect_equal(f("cIAI")$v1, base$v1 * 1.83)
  expect_equal(f("pneumonia")$cl1, base$cl1 * 1.10)
  expect_equal(f("pneumonia")$v1, base$v1 * 1.83)
  expect_equal(f("pneumoniaVentilated")$v1, base$v1 * 1.83 * 1.202)
  expect_equal(f("other")$v1, base$v1)
  expect_equal(f("other")$cl1, base$cl1)
  expect_equal(f("cIAI")$v2, base$v2)
  expect_equal(f("cIAI")$cl2, base$cl2)
  expect_error(ceftazidime(70, 170, 35, "male", infection = "sepsis"), "Invalid infection")
})

test_that("the reference man, and the switch on reads the pharmacokinetic weight", {
  set <- ceftazidime(70, 170, 35, "male")$PK$default
  expect_equal(set$v1, 11.2)
  expect_equal(set$v2, 6.52)
  expect_equal(set$cl2, 5.33 / 60)
  expect_equal(set$cl1, franzeseCL(70, renal(cg(70, 35, 0.9) * 1.73 / mosteller(70, 170))) / 60)
  expect_equal_rounded(8.062209, set$cl1 * 60)
  size <- pkSizeFactors(120, 170, 50, "female", TRUE)
  w <- size$pkWeight
  on <- ceftazidime(120, 170, 50, "female", creatinine = 0.8)$PK$default
  expect_equal(on$v1, 11.2 * w / 70)
  expect_equal(on$cl1, franzeseCL(w, renal(cg(w, 50, 0.8, TRUE) * 1.73 / mosteller(w, 170))) / 60)
  off <- ceftazidime(120, 170, 50, "female", adjustToFFM = FALSE, creatinine = 0.8)$PK$default
  expect_equal(off$v1, 11.2 * 120 / 70)
})

test_that("pregnancy raises the normalised creatinine clearance only when the creatinine is blank", {
  base <- ceftazidime(60, 165, 30, "female", creatinine = NULL)$PK$default
  preg <- ceftazidime(60, 165, 30, "female", pregnant = TRUE)$PK$default
  expect_gt(preg$cl1, base$cl1)
  expect_equal(preg$v1, base$v1)
  expect_equal(ceftazidime(60, 165, 30, "female", creatinine = 0.6, pregnant = TRUE)$PK,
               ceftazidime(60, 165, 30, "female", creatinine = 0.6)$PK)
})

test_that("the label's healthy clearance and Vss, and the source's phase III exposures", {
  # Avycaz label Table 14, healthy men on 2.5 g every 8 h: CL 6.86 L/h (15%
  # CV), Vss 17 L.  The model is 17% above the label's clearance.
  set <- ceftazidime(70, 170, 35, "male")$PK$default
  expect_equal(set$cl1 * 60, 6.86, tolerance = 0.20)
  expect_equal(set$v1 + set$v2, 17, tolerance = 0.05)
  # Avycaz 2.5 g every 8 h over 2 h, a 50-year-old 75-kg man with cUTI:
  # the corrected Table 2 gives the phase III cUTI geometric mean Cmax,ss
  # 80.7 mcg/mL
  starts <- seq(0, 7 * 480, 480)
  dt <- data.frame(Drug = "Avycaz", Time = c(rbind(starts, starts + 120)),
                   Dose = c(rbind(1.25, 0)), Units = "g/hr")
  a <- simulateDrugsWithCovariates(dt, events, 75, 170, 50, "male", 8 * 480, FALSE,
                                   infection = "cUTI")
  direct <- dt
  direct$Drug <- "ceftazidime"
  direct$Dose <- direct$Dose * 0.8
  b <- simulateDrugsWithCovariates(direct, events, 75, 170, 50, "male", 8 * 480, FALSE,
                                   infection = "cUTI")
  expect_identical(names(a), "ceftazidime")
  expect_equal(a$ceftazidime$wide, b$ceftazidime$wide)
  w <- a$ceftazidime$wide
  expect_equal(max(w$Plasma[w$Time >= 7 * 480]), 80.7, tolerance = 0.15)
})

test_that("Avycaz offers the label's units and draws 80% of each dose", {
  dd <- getDrugDefaultsGlobal()
  units <- unlist(dd$Units[dd$Drug == "Avycaz"])
  expect_true(all(c("g", "g tid", "mg/kg", "mg/kg tid", "g/hr", "mg/kg/hr") %in% units))
  expect_true(all(combinationUnitOk("Avycaz", units)))
  expect_false(combinationUnitOk("Avycaz", "Plasma target"))
  expect_identical(combinationComponents("Avycaz"), "ceftazidime")
  # the label's "62.5 mg/kg (ceftazidime 50 mg/kg and avibactam 12.5 mg/kg)"
  x <- expandCombinationProducts(data.frame(Drug = "Avycaz", Time = 0, Dose = 62.5,
                                            Units = "mg/kg tid"))
  expect_equal(x$Dose, 50)
  expect_identical(as.character(x$Units), "mg/kg tid")
  expect_identical(combinationProductOf("ceftazidime", data.frame(Drug = "Avycaz")), "Avycaz")
})

test_that("the MIC line is free ceftazidime at 8 mg/L, with no band", {
  mic <- antibioticMic("ceftazidime")
  expect_equal(mic$MIC, 8)
  expect_equal(mic$FreeFraction, 0.90)
  expect_equal(mic$Threshold, 8.9)
  expect_null(antibioticMbcBand("ceftazidime"))
  expect_true("ceftazidime" %in% antibioticNoBands()$Drug)
})

test_that("pinned: the reference man with the switch off, a woman of 120 kg with it on", {
  X <- ceftazidime(70, 170, 35, "male", adjustToFFM = FALSE)
  expect_equal_rounded(X$PK$default, list(v1 = 11.2, v2 = 6.52, v3 = 1,
                                          cl1 = 0.13437016, cl2 = 0.088833333, cl3 = 0))
  expect_equal(c(X$tPeak, X$MEAC, X$typical, X$upperTypical, X$lowerTypical), rep(0, 5))
  expect_match(X$reference, "Franzese", fixed = TRUE)
  on <- ceftazidime(120, 170, 50, "female", creatinine = 0.8)$PK$default
  expect_equal_rounded(on, list(v1 = 12.232717, v2 = 7.1211886, v3 = 1,
                                cl1 = 0.12771248, cl2 = 0.094241076, cl3 = 0))
})
