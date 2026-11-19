# semaglutide: see R/drugs_semaglutide.R (Carlsson Petri 2018, ESM Table S3).
test_that("the reference subject, switch off, has the published parameters", {
  pk <- semaglutide(85, 165, 50, "female", adjustToFFM = FALSE)$PK$default
  expect_equal(pk$cl1, 0.0478 / 60)
  expect_equal(pk$v1, 12.2)
  expect_equal(pk$ka_SC, 0.0286 / 60)
  expect_equal(pk$bioavailability_SC, 1)
})
test_that("weight acts on clearance as (WT/85)^0.774, reproducing the paper's exposures", {
  cl <- function(w) semaglutide(w, 170, 50, "female", adjustToFFM = FALSE)$PK$default$cl1
  expect_equal(cl(85) / cl(55), 1.40, tolerance = 0.005)  # 55 kg: 40% more exposure
  expect_equal(cl(85) / cl(127), 0.73, tolerance = 0.005) # 127 kg: 27% less
})
test_that("men 1.04, and the two older age groups", {
  cl <- function(age, sex) semaglutide(85, 170, age, sex, adjustToFFM = FALSE)$PK$default$cl1 * 60
  expect_equal(cl(50, "male"), 0.0478 * 1.04)
  expect_equal(cl(64.9, "female"), 0.0478)
  expect_equal(cl(65, "female"), 0.0478 * 0.988)
  expect_equal(cl(74.9, "female"), 0.0478 * 0.988)
  expect_equal(cl(75, "female"), 0.0478 * 0.961)
})
test_that("switch on: the pharmacokinetic weight and the library volume factor", {
  # V/F is anchored at the phase 3 cohort (SEMAGLUTIDE_COHORT), so the volume
  # factor is divided by the cohort's; CL/F is not anchored.
  kc <- cohortFactorsIndependent(SEMAGLUTIDE_COHORT)
  size <- pkSizeFactors(85, 165, 50, "female", TRUE, legacyVolume = 1)
  pk <- semaglutide(85, 165, 50, "female")$PK$default
  expect_equal(pk$cl1, 0.0478 * (size$pkWeight / 85)^0.774 / 60)
  expect_equal(pk$v1, 12.2 * size$volume / kc$volume)
  expect_equal_rounded(pk$cl1 * 60, 0.0376559)
})
test_that("anchored at the phase 3 cohort: its typical patient receives the published V/F (A2-F4)", {
  # ESM Table S1, five trials: V/F carries no sex term, so the cohort's men
  # and women, each at their own trial's demographics, sit either side of
  # 12.2 L, and their weighted geometric mean is 12.2 L.
  lv <- 0; n <- 0
  for (g in SEMAGLUTIDE_COHORT$groups) {
    h <- 100 * sqrt(g$weight / g$bmi)
    lv <- lv + g$men * log(semaglutide(g$weight, h, g$age, "male")$PK$default$v1) +
      g$women * log(semaglutide(g$weight, h, g$age, "female")$PK$default$v1)
    n <- n + g$men + g$women
  }
  expect_equal(n, 1612)
  expect_equal(exp(lv / n), 12.2, tolerance = 1e-6)
  kc <- cohortFactorsIndependent(SEMAGLUTIDE_COHORT)
  h <- 100 * sqrt(71.3 / 26.3)
  m <- semaglutide(71.3, h, 58.4, "male")$PK$default
  r <- ffmRatioMaleIndependent(71.3, h, 58.4)
  expect_equal(m$v1, 12.2 * r / kc$volume, tolerance = 1e-6)
  # CL/F keeps its own weight term, unanchored
  expect_equal(m$cl1 * 60, 0.0478 * (70 * r / 85)^0.774 * 1.04, tolerance = 1e-6)
  # The switch off is unchanged: 12.2 L
  expect_equal(semaglutide(71.3, h, 58.4, "male", adjustToFFM = FALSE)$PK$default$v1, 12.2)
})
test_that("steady state at 1 mg weekly matches the published 29.8 nmol/L", {
  PK <- getDrugPK("semaglutide", 85, 165, 50, "female", adjustToFFM = FALSE)
  noEvents <- data.frame(Time = numeric(0), Event = character(0))
  d <- data.frame(Drug = "semaglutide", Time = 0, Dose = 1, Units = "mg SC qwk")
  w <- simCpCe(d, noEvents, PK, 20 * MINS_PER_WEEK, FALSE)$wide
  last <- w$Time >= 19 * MINS_PER_WEEK
  t <- w$Time[last]; cp <- w$Plasma[last]
  cavg <- sum(diff(t) * (head(cp, -1) + tail(cp, -1)) / 2) / MINS_PER_WEEK
  nmol <- cavg / 4.11358
  expect_equal(nmol, 30.3, tolerance = 0.01)     # model; paper 29.8 (CI 29.4-30.2)
  expect_equal(nmol, 29.8, tolerance = 0.03)
})
test_that("units: SC only, weekly offered", {
  units <- getDrugDefaults("semaglutide")$Units[[1]]
  expect_equal(units, c("mg SC", "mg SC qwk"))
  expect_true("semaglutide" %in% LONG_TERM_DRUGS)
})
