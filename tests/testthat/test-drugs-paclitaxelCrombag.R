# Paclitaxel (R/drugs_paclitaxelCrombag.R): Crombag 2019, Joerger's structure
# refitted in 684 adults, and the paclitaxel group that chooses between it and
# Joerger 2006.  Drafted by Claude Code, 2026-11-13.

noEvents <- data.frame(Time = double(), Event = character())
refHeight <- uniroot(function(h) bsaMosteller(70, h) - 1.8, c(100, 250))$root

test_that("Table II's covariate model with age, at the median woman, in mg", {
  X <- paclitaxelCrombag(70, refHeight, 61, "female", FALSE)
  mg <- PACLITAXEL_MW / 1000
  mm <- X$michaelisMenten
  expect_equal(mm$vmax * 60 / mg, 31.7)
  expect_equal(mm$km / mg, 0.46)
  expect_equal(mm$saturableDistribution$vmax * 60 / mg, 177)
  expect_equal(mm$saturableDistribution$km / mg, 1.63)
  expect_equal(mm$saturableDistribution$k21 * 60, 1.20)
  d <- X$PK$default
  expect_equal(c(d$v1, d$v2, d$cl2 * 60), c(12.0, 267, 16.6))
})

test_that("the covariates on VMEL: a man x1.12, BSA^1.41, and 5% less per decade", {
  v <- function(...) paclitaxelCrombag(...)$michaelisMenten$vmax
  expect_equal(v(70, refHeight, 61, "male", FALSE) / v(70, refHeight, 61, "female", FALSE), 1.12)
  expect_equal(round(v(70, refHeight, 71, "female", FALSE) / v(70, refHeight, 61, "female", FALSE), 3),
               0.954)                                       # the text's 5%
  big <- uniroot(function(w) bsaMosteller(w, refHeight) - 2.2, c(50, 200))$root
  expect_equal(v(big, refHeight, 61, "female", FALSE) / v(70, refHeight, 61, "female", FALSE),
               (2.2 / 1.8)^1.41)
  # the age term floors at 18
  expect_equal(v(70, refHeight, 5, "female", FALSE), v(70, refHeight, 18, "female", FALSE))
})

test_that("80 mg/m2 over 1 h: ten years of age lengthen the time above 0.05 uM by under 10%", {
  tc <- function(sex, age) {
    dose <- data.frame(Drug = "paclitaxelCrombag", Time = c(0, 60),
                       Dose = c(80 * 1.8, 0), Units = "mg/hr")
    w <- simulateDrugsWithCovariates(dose, noEvents, 70, refHeight, age, sex, 72 * 60, FALSE,
                                     adjustToFFM = FALSE)$paclitaxelCrombag$wide
    above <- w$Plasma > 0.05 * PACLITAXEL_MW / 1000
    sum(diff(w$Time)[above[-1] & above[-length(above)]]) / 60
  }
  f <- c(tc("female", 61), tc("female", 71))
  m <- c(tc("male", 61), tc("male", 71))
  expect_equal(round(c(f, m), 2), c(10.29, 11.01, 8.85, 9.43))
  expect_lt(f[2] / f[1], 1.10)
  expect_lt(m[2] / m[1], 1.10)
})

test_that("both switch positions: only VMEL moves with size", {
  on <- paclitaxelCrombag(110, 175, 50, "male")
  off <- paclitaxelCrombag(110, 175, 50, "male", FALSE)
  expect_equal(c(on$PK$default$v1, on$PK$default$v2), c(off$PK$default$v1, off$PK$default$v2))
  w <- pkSizeFactors(110, 175, 50, "male", TRUE)$pkWeight
  expect_equal(on$michaelisMenten$vmax / off$michaelisMenten$vmax,
               (bsaMosteller(w, 175) / bsaMosteller(110, 175))^1.41)
})

test_that("pregnancy: Janssen 2021's factors on KMEL, V1 and V3", {
  off <- paclitaxelCrombag(70, 165, 30, "female", FALSE)
  on <- paclitaxelCrombag(70, 165, 30, "female", FALSE, pregnant = TRUE)
  expect_equal(on$michaelisMenten$km / off$michaelisMenten$km, 1.32)
  expect_equal(on$PK$default$v1 / off$PK$default$v1, 2.27)
  expect_equal(on$PK$default$v2 / off$PK$default$v2, 2.28)
  expect_equal(on$michaelisMenten$vmax, off$michaelisMenten$vmax)
})

test_that("the group chooses Crombag to 84 years and Joerger from 85", {
  pick <- function(age) paclitaxel(70, 170, age, "female")$modelChosen
  expect_match(pick(18), "Crombag")
  expect_match(pick(60), "Crombag")
  expect_match(pick(84), "Crombag")
  expect_match(pick(85), "Joerger")
  expect_match(pick(95), "Joerger")
  expect_match(pick(10), "Crombag")                        # nearest: 18 against 33
  expect_match(getDrugPK("paclitaxel", 70, 170, 60, "female")$reference, "Crombag")
  dd <- getDrugDefaultsGlobal()
  expect_identical(dd$Category[dd$Drug == "paclitaxel"], CHEMOTHERAPY_DRUG_CATEGORY)
  expect_true(is.na(dd$Category[dd$Drug == "paclitaxelCrombag"]) ||
                !nzchar(dd$Category[dd$Drug == "paclitaxelCrombag"]))
})
