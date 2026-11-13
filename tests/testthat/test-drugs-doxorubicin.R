# Doxorubicin and doxorubicinol (R/drugs_doxorubicinJoerger.R,
# R/drugs_doxorubicinolJoerger.R): Joerger 2007.  Drafted by Claude Code,
# 2026-11-06.

f <- DOXORUBICIN_BASE_FRACTION
htRef <- 1.8^2 * 3600 / 70   # Mosteller BSA 1.8 m2 at 70 kg
noEvents <- data.frame(Time = double(), Event = character())

test_that("Table IV at the reference, the volumes and clearances divided by the base fraction", {
  set <- getDrugPK("doxorubicinJoerger", 70, htRef, 56, "female",
                   adjustToFFM = FALSE)$PK$default
  expect_equal_rounded(set$cl1 * 60 * f, 47.6)
  expect_equal_rounded(set$v1 * f, 12.3)
  expect_equal_rounded(set$cl2 * 60 * f, 60.3)
  expect_equal_rounded(set$v2 * f, 421)
  halfLives <- sort(log(2) / c(set$lambda_1, set$lambda_2) / 60)
  expect_equal(round(halfLives * c(60, 1), 1), c(4.7, 11.1))
  expect_equal(round((set$v1 + set$v2) * f), 433)
})

test_that("the text's covariate examples: BSA +0.2 m2 +16%, age +10 y -9%, CLcr +20 mL/min +13%", {
  cl <- function(bsa, age) doxorubicinJoerger(70, bsa^2 * 3600 / 70, age, "female",
                                       adjustToFFM = FALSE)$PK$default$cl1
  expect_equal(round(cl(2.0, 56) / cl(1.8, 56), 3), 1.159)
  expect_equal(round(cl(1.8, 66) / cl(1.8, 56), 3), 0.915)
  expect_equal(cl(1.8, 5), cl(1.8, 18))
  dol <- function(scr) doxorubicinolJoerger(70, 165, 56, "female", adjustToFFM = FALSE,
                                     creatinine = scr)$PK$default$cl1
  crcl <- function(scr) creatinineClearanceCG(70, 56, "female", scr)
  scr80 <- uniroot(function(s) crcl(s) - 80, c(0.3, 3))$root
  scr100 <- uniroot(function(s) crcl(s) - 100, c(0.3, 3))$root
  expect_equal(dol(scr80) * 60, 108, tolerance = 1e-4)
  expect_equal(round(dol(scr100) / dol(scr80), 3), 1.128)
})

test_that("a dose of the hydrochloride gives the base: the mean AUC of Table VII, 4.21 umol h/L", {
  # the median patient: BSA 1.81, age 56; 60 mg/m2 over 15 min
  ht <- 1.81^2 * 3600 / 70
  dose <- 60 * 1.81
  d <- data.frame(Drug = "doxorubicinJoerger", Time = c(0, 15), Dose = c(dose * 4, 0),
                  Units = "mg/hr")
  w <- simulateDrugsWithCovariates(d, noEvents, 70, ht, 56, "female", 21 * 1440,
                                   FALSE, adjustToFFM = FALSE)
  p <- w$doxorubicinJoerger$wide
  auc <- sum(diff(p$Time) * (head(p$Plasma, -1) + tail(p$Plasma, -1)) / 2) / 60
  # umol h/L of the base: dose / CL, 3.91 at AST 21 (4.03 at the median AST
  # 24), the cohort's mean 4.21 some 6% higher by between-patient variability
  clRef <- 47.6 * (1.81 / 1.8)^1.4
  expect_equal(auc / 543.52 * 1000, dose / 579.98 * 1000 / clRef, tolerance = 0.005)
  expect_equal(round(auc / 543.52 * 1000, 2), 3.91)
  # doxorubicinol is formed: all of the clearance, scaled by fm
  m <- w$doxorubicinolJoerger$wide
  aucM <- sum(diff(m$Time) * (head(m$Plasma, -1) + tail(m$Plasma, -1)) / 2) / 60
  clDol <- doxorubicinolJoerger(70, ht, 56, "female", adjustToFFM = FALSE)$PK$default$cl1 * 60
  expect_equal(aucM, dose * DOXORUBICINOL_PER_HYDROCHLORIDE / clDol, tolerance = 0.01)
})

test_that("doxorubicin is one name for three models, with the doxorubicinol of the same source", {
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "doxorubicin", ]
  expect_identical(row$Category, CHEMOTHERAPY_DRUG_CATEGORY)
  expect_equal(c(row$Lower, row$Upper, row$Typical, row$MEAC), c(0, 0, 0, 0))
  met <- dd[dd$Drug == "doxorubicinol", ]
  expect_true(is.na(met$Category) || met$Category == "")
  adult <- getDrugPK("doxorubicin", 70, 170, 60, "male")
  expect_equal(adult$tPeak, 0)
  expect_match(adult$reference, "Joerger")          # 59 patients, against Perez-Blanco's 44
  expect_identical(adult$PK$default$metabolite$name, "doxorubicinol")
  expect_match(getDrugPK("doxorubicinol", 70, 170, 60, "male")$reference, "Joerger")
  child <- getDrugPK("doxorubicin", 20, 115, 6, "female")
  expect_match(child$reference, "V.ller")
  expect_identical(child$PK$default$metabolite$name, "doxorubicinol")
  expect_match(getDrugPK("doxorubicinol", 20, 115, 6, "female")$reference, "V.ller")
  expect_identical(therapeuticRange("doxorubicin")$band_status, "not_applicable")
  expect_identical(therapeuticRange("doxorubicinol")$band_status, "not_applicable")
})

test_that("pregnancy: Janssen 2021's factors on the adult doxorubicin models, CL x 1.14 and V1 x 1.08", {
  for (f in list(doxorubicinJoerger, doxorubicinPerezBlanco)) {
    off <- f(70, 165, 30, "female", FALSE)
    on <- f(70, 165, 30, "female", FALSE, pregnant = TRUE)
    expect_equal(on$PK$default$cl1 / off$PK$default$cl1, 1.14)
    expect_equal(on$PK$default$v1 / off$PK$default$v1, 1.08)
    expect_equal(on$PK$default$v2, off$PK$default$v2)
  }
  expect_match(getDrugPK("doxorubicin", 70, 165, 30, "female", pregnant = TRUE)$reference,
               "Janssen 2021")
})
