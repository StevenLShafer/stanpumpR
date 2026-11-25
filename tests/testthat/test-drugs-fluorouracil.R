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
})

# Briki 2026, Table 2 (page 6): medians per study (Gemini's chemotherapy
# audit of 2026-11-23, F22; docs/weight-adjustment.md, Anchoring at the
# source's cohort).  Claude Code, 2026-11-25.
test_that("the cohort is Briki's Table 2: three studies, 726 patients", {
  g <- FLUOROURACIL_BRIKI_COHORT$groups
  expect_equal(sum(vapply(g, function(x) x$men, 1)), 422)
  expect_equal(sum(vapply(g, function(x) x$women, 1)), 304)
  expect_equal(vapply(g, function(x) x$weight, 1), c(68, 76, 67))
  expect_equal(vapply(g, function(x) x$height, 1), c(168, 171, 170))
  expect_equal(vapply(g, function(x) x$age, 1), c(65, 63, 69))
})

test_that("V1 and Q are anchored at Briki's cohort with the switch on", {
  kc <- cohortFactorsIndependent(FLUOROURACIL_BRIKI_COHORT)
  expect_equal(round(c(kc$volume, kc$clearance), 4), c(0.8989, 0.9231))
  for (pt in list(list(70, 170, 35, "male"), list(90, 165, 50, "female"))) {
    size <- do.call(pkSizeFactors, c(pt, list(TRUE)))
    m <- do.call(fluorouracilBriki, pt)$PK$default
    # the old pins (published value x the reference man's factor) divided by
    # the cohort's
    expect_equal_rounded(m$v1, 9.21 * size$volume / kc$volume)
    expect_equal_rounded(m$cl2, 125 / 60 * size$clearance / kc$clearance)
  }
  # the reference man's V1 and Q rise by 11.3% and 8.3%
  m <- fluorouracilBriki(70, 170, 35, "male")$PK$default
  expect_equal(round(m$v1 / 9.21, 3), 1.113)
  expect_equal(round(m$cl2 * 60 / 125, 3), 1.083)
})

test_that("Vmax is anchored at the cohort's patient of the patient's own sex", {
  only <- function(sex) list(groups = lapply(FLUOROURACIL_BRIKI_COHORT$groups, function(g) {
    if (sex == "male") g$women <- 0 else g$men <- 0
    g
  }))
  km <- cohortFactorsIndependent(only("male"))$clearance
  kw <- cohortFactorsIndependent(only("female"))$clearance
  expect_equal(round(c(km, kw), 4), c(0.9842, 0.8447))
  for (pt in list(list(70, 170, 50, "male"), list(90, 165, 50, "female"),
                  list(55, 160, 70, "female"))) {
    on <- do.call(fluorouracilBriki, pt)$michaelisMenten$vmax
    off <- do.call(fluorouracilBriki, c(pt, list(adjustToFFM = FALSE)))$michaelisMenten$vmax
    k <- if (pt[[4]] == "male") km else kw
    expect_equal_rounded(on / off, do.call(pkSizeFactors, c(pt, list(TRUE)))$clearance / k)
  }
  # sex is not counted twice: at the cohort's typical man and woman (the
  # geometric mean of its groups' fat-free masses) Vmax is the published
  # value, so their ratio is the source's 1.183 alone
  expect_equal(fluorouracilBriki(70, 170, 50, "male", adjustToFFM = FALSE)$michaelisMenten$vmax /
                 fluorouracilBriki(70, 170, 50, "female", adjustToFFM = FALSE)$michaelisMenten$vmax,
               1.183)
})

test_that("with the switch off the cohort is ignored", {
  off <- fluorouracilBriki(55, 160, 70, "female", adjustToFFM = FALSE)
  expect_equal_rounded(off$PK$default$v1, 9.21)
  expect_equal_rounded(off$PK$default$cl2, 125 / 60)
  expect_equal_rounded(off$michaelisMenten$vmax, 429 * (1 - 0.289 * (70 - 60) / 60) / 60)
})

test_that("age is held within the source's 24-90 years", {
  # switch off: with it on, Vmax also follows fat-free mass, which changes
  # with age in a child (Al-Sallami's maturation), apart from the age term
  vmaxAt <- function(age)
    fluorouracilBriki(70, 170, age, "male", adjustToFFM = FALSE)$michaelisMenten$vmax
  expect_equal(vmaxAt(5), vmaxAt(24))
  expect_equal(vmaxAt(99), vmaxAt(90))
  expect_true(is.finite(vmaxAt(0)))
  expect_true(is.finite(fluorouracilBriki(3.5, 50, 0, "male")$michaelisMenten$vmax))
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
