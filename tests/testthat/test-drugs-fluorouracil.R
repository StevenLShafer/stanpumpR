# Fluorouracil (R/drugs_fluorouracil.R): Briki 2026, two compartments with
# parallel saturable and linear elimination.  Drafted by Claude Code,
# 2026-11-05.

noEvents <- data.frame(Time = double(), Event = character())
p <- FLUOROURACIL_BRIKI

test_that("the covariate terms reproduce the percentages the paper states", {
  vmaxAt <- function(age, sex)
    fluorouracilBriki(70, 170, age, sex, adjustToFFM = FALSE)$michaelisMenten$vmax
  # men's Vmax 18% higher than women's of the same age
  expect_equal(vmaxAt(50, "male") / vmaxAt(50, "female"), 1.183)
  # lower at 80 than at 40 years, same sex: 0.176, which the paper gives as 17%
  expect_equal(round(1 - vmaxAt(80, "male") / vmaxAt(40, "male"), 2), 0.18)
  # an 80-year-old woman's 30% lower than a 40-year-old man's
  expect_equal(round(1 - vmaxAt(80, "female") / vmaxAt(40, "male"), 2), 0.30)
  # BSA 1.5 against 2.0: linear clearance 53% lower
  expect_equal(round(1 - (1.5 / 2.0)^p$thetaBSA, 2), 0.53)
})

test_that("with the switch off the published equations hold at total weight", {
  w <- 80; h <- 180
  PK <- getDrugPK("fluorouracil", w, h, 60, "male", adjustToFFM = FALSE)
  mm <- PK$michaelisMenten
  set <- PK$PK$default
  bsa <- sqrt(w * h / 3600)                       # Mosteller
  expect_equal_rounded(mm$vmax, 429 * 1.183 / 60)  # age 60: no age term
  expect_equal_rounded(mm$km, 2.34)
  expect_equal_rounded(mm$clLinear, 52.6 * (bsa / 1.7)^2.61 / 60)
  expect_equal_rounded(set$v1, 9.21)
  expect_equal_rounded(set$v2, 14.6 * w / 70)
  expect_equal_rounded(set$cl2, 125 / 60)
  expect_identical(PK$Concentration.Units, "mcg")
})

test_that("with the switch on, size covariates take the pharmacokinetic weight", {
  w <- 90; h <- 165
  size <- pkSizeFactors(w, h, 50, "female", TRUE)
  m <- fluorouracilBriki(w, h, 50, "female")
  bsa <- sqrt(size$pkWeight * h / 3600)
  expect_equal_rounded(m$michaelisMenten$clLinear, 52.6 * (bsa / 1.7)^2.61 / 60)
  expect_equal_rounded(m$PK$default$v2, 14.6 * size$pkWeight / 70)
  expect_equal_rounded(m$PK$default$v1, 9.21 * size$volume)
  # Vmax carries sex and age, not fat-free mass
  expect_equal_rounded(m$michaelisMenten$vmax, 429 * (1 - 0.289 * (50 - 60) / 60) / 60)
})

test_that("age is held within the source's 24-90 years", {
  vmaxAt <- function(age) fluorouracilBriki(70, 170, age, "male")$michaelisMenten$vmax
  expect_equal(vmaxAt(5), vmaxAt(24))
  expect_equal(vmaxAt(99), vmaxAt(90))
  expect_true(is.finite(vmaxAt(0)))
})

test_that("a 46-h infusion reaches the steady state the two eliminations predict", {
  # The plateau C solves R = CLlin C + Vmax C / (Km + C), an independent check
  # of the numerical engine.  A 70-kg, 175-cm man of 60, 2400 mg/m2 over 46 h.
  w <- 70; h <- 175
  bsa <- sqrt(w * h / 3600)
  rate <- 2400 * bsa / 46                          # mg/h
  mm <- fluorouracilBriki(w, h, 60, "male", adjustToFFM = FALSE)$michaelisMenten
  vmax <- mm$vmax * 60; cl <- mm$clLinear * 60     # per hour
  css <- uniroot(function(C) cl * C + vmax * C / (mm$km + C) - rate, c(0, 50))$root
  wide <- simulateDrugsWithCovariates(
    data.frame(Drug = "fluorouracil", Time = c(0, 46 * 60), Dose = c(rate, 0),
               Units = "mg/hr"),
    noEvents, w, h, 60, "male", 50 * 60, FALSE, adjustToFFM = FALSE)$fluorouracil$wide
  at <- function(t) wide$Plasma[which.min(abs(wide$Time - t))]
  expect_equal(at(40 * 60), css, tolerance = 1e-3)
  # saturable: doubling the rate more than doubles the plateau
  css2 <- uniroot(function(C) cl * C + vmax * C / (mm$km + C) - 2 * rate, c(0, 50))$root
  expect_gt(css2 / css, 2)
  # and the plateau falls to almost nothing within an hour of the stop
  expect_lt(at(47 * 60), css / 20)
})

test_that("fluorouracil is a chemotherapy drug, plasma only, with no band", {
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "fluorouracil", ]
  expect_identical(row$Category, CHEMOTHERAPY_DRUG_CATEGORY)
  expect_equal(c(row$Lower, row$Upper, row$Typical, row$MEAC), c(0, 0, 0, 0))
  PK <- getDrugPK("fluorouracil", 70, 170, 60, "male")
  expect_equal(PK$tPeak, 0)
  expect_match(PK$reference, "Briki")
  expect_identical(therapeuticRange("fluorouracil")$band_status, "not_applicable")
})
