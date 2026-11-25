# Fluorouracil (R/drugs_fluorouracilBriki.R): Briki 2026, two compartments with
# parallel saturable and linear elimination.  Drafted by Claude Code,
# 2026-11-05.

noEvents <- data.frame(Time = double(), Event = character())
# Since 2026-11-25 the model is the authors' alternative, Table S3 of the
# supplement, with lean body mass on Vmax in place of sex (Shafer's decision,
# PR #897).  Claude Code, 2026-11-25.
lbmJ <- function(w, h, sex) {           # Janmahasatian 2005, written out again
  bmi <- w / (h / 100)^2
  if (sex == "male") 9270 * w / (6680 + 216 * bmi) else 9270 * w / (8780 + 244 * bmi)
}

test_that("with the switch off the Table S3 equations hold at total weight", {
  for (pt in list(list(80, 180, 60, "male"), list(58, 160, 72, "female"))) {
    w <- pt[[1]]; h <- pt[[2]]; age <- pt[[3]]; sex <- pt[[4]]
    PK <- getDrugPK("fluorouracil", w, h, age, sex, adjustToFFM = FALSE)
    mm <- PK$michaelisMenten
    set <- PK$PK$default
    bsa <- sqrt(w * h / 3600)                       # Mosteller
    expect_equal_rounded(mm$vmax,
      411 * (1 - 0.292 * (age - 60) / 60) * (lbmJ(w, h, sex) / 55)^0.484 / 60)
    expect_equal_rounded(mm$km, 1.89)
    expect_equal_rounded(mm$clLinear, 59.2 * (bsa / 1.7)^1.62 / 60)
    expect_equal_rounded(set$v1, 9.58)
    expect_equal_rounded(set$v2, 13.6 * w / 70)
    expect_equal_rounded(set$cl2, 112 / 60)
    expect_identical(PK$Concentration.Units, "mcg")
  }
})

test_that("sex acts only through lean mass, and reproduces the main model's sex factor", {
  # the same lean mass gives the same Vmax whatever the sex
  target <- lbmJ(70, 170, "male")
  wf <- uniroot(function(w) lbmJ(w, 170, "female") - target, c(40, 200), tol = 1e-12)$root
  vm <- function(w, sex) fluorouracilBriki(w, 170, 60, sex, adjustToFFM = FALSE)$michaelisMenten$vmax
  expect_equal(vm(70, "male"), vm(wf, "female"), tolerance = 1e-9)
  # Fig S3: the cohort's men about 56 kg of lean mass, its women about 39.5;
  # the LBM exponent turns that into the main model's 18% (Table 3: 1.183)
  expect_equal(round((56 / 39.5)^0.484, 2), 1.18)
  # an 80-year-old's Vmax against a 40-year-old's of the same lean mass
  va <- function(age) fluorouracilBriki(70, 170, age, "male", adjustToFFM = FALSE)$michaelisMenten$vmax
  expect_equal(va(80) / va(40), (1 - 0.292 / 3) / (1 + 0.292 / 3))
})

test_that("with the switch on, the size terms read fat-free mass and the pharmacokinetic weight", {
  for (pt in list(list(90, 165, 50, "female"), list(70, 170, 35, "male"))) {
    w <- pt[[1]]; h <- pt[[2]]; age <- pt[[3]]; sex <- pt[[4]]
    size <- pkSizeFactors(w, h, age, sex, TRUE)
    m <- fluorouracilBriki(w, h, age, sex)
    bsa <- sqrt(size$pkWeight * h / 3600)
    expect_equal_rounded(m$michaelisMenten$clLinear, 59.2 * (bsa / 1.7)^1.62 / 60)
    expect_equal_rounded(m$PK$default$v2, 13.6 * size$pkWeight / 70)
    expect_equal_rounded(m$michaelisMenten$vmax,
      411 * (1 - 0.292 * (age - 60) / 60) * (size$ffm / 55)^0.484 / 60)
  }
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
    # the reference man's scaling divided by the cohort's factors
    expect_equal_rounded(m$v1, 9.58 * size$volume / kc$volume)
    expect_equal_rounded(m$cl2, 112 / 60 * size$clearance / kc$clearance)
  }
  # each study's typical man and woman, weighted by their numbers, receive
  # the published V1 and Q
  lv <- 0; lq <- 0; n <- 0
  for (x in FLUOROURACIL_BRIKI_COHORT$groups) for (sex in c("male", "female")) {
    k <- if (sex == "male") x$men else x$women
    m <- fluorouracilBriki(x$weight, x$height, x$age, sex)$PK$default
    lv <- lv + k * log(m$v1); lq <- lq + k * log(m$cl2); n <- n + k
  }
  expect_equal_rounded(exp(lv / n), 9.58)
  expect_equal_rounded(exp(lq / n) * 60, 112)
})

test_that("with the switch off the cohort is ignored", {
  off <- fluorouracilBriki(55, 160, 70, "female", adjustToFFM = FALSE)
  expect_equal_rounded(off$PK$default$v1, 9.58)
  expect_equal_rounded(off$PK$default$cl2, 112 / 60)
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
