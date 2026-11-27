# Ciprofol (R/ciprofol.R): Liu 2024 for adults, Wang 2026 for children,
# chosen by age (Shafer, 2026-11-26).
# Drafted by Claude Code, 2026-11-26.

test_that("Liu's long-infusion, non-ICU, arterial parameters at the reference, switch off", {
  m <- ciprofolLiu(62, 165, 33, "male", adjustToFFM = FALSE)$PK$default
  expect_equal_rounded(3.53, m$v1)
  expect_equal_rounded(51.9, m$v2)
  expect_equal_rounded(419, m$v3)
  expect_equal_rounded(80.9 * 0.789 / 60, m$cl1)
  expect_equal_rounded(73.9 / 60, m$cl2)
  expect_equal_rounded(28.6 / 60, m$cl3)
  f <- ciprofolLiu(62, 165, 33, "female", adjustToFFM = FALSE)$PK$default
  expect_equal(f$cl1 / m$cl1, 1.09)
  expect_equal(f$v1, m$v1)
})

test_that("Liu's covariate terms", {
  a <- ciprofolLiu(124, 165, 66, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(a$cl1 / (80.9 * 0.789 / 60), 2^0.578)
  expect_equal(a$v1 / 3.53, 2^0.496)
  # the age term is held at 18
  y <- ciprofolLiu(62, 165, 5, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(y$v1, 3.53 * (18 / 33)^0.496)
})

test_that("the switch on anchors the size-free parameters at Liu's cohort", {
  s <- ciprofolLiu(100, 175, 50, "male")$PK$default
  cohort <- cohortSizeFactors(61.7, 38, 226, 236, height = 165)
  size <- pkSizeFactors(100, 175, 50, "male", TRUE, cohort = cohort)
  expect_equal(s$v2, 51.9 * size$volume)
  expect_equal(s$cl3, 28.6 / 60 * size$clearance)
  expect_equal(s$cl1, 80.9 * 0.789 / 60 * (size$pkWeight / 62)^0.578)
})

test_that("Wang's per-kilogram parameters, switch off", {
  m <- ciprofolWang(20, 115, 5, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(m$v1, 0.506 * 20)
  expect_equal(m$v2, 0.231 * 20)
  expect_equal(m$v3, 1.36 * 20)
  expect_equal(m$cl1, 0.0312 * 20)
  expect_equal(m$cl2, 0.0282 * 20)
  expect_equal(m$cl3, 0.0198 * 20)
})

test_that("with the switch on, Wang's parameters scale with the adjusted weight", {
  # a 30 kg, 140 cm, 8-year-old girl: the per-kg values on pkSizeFactors()'s
  # adjusted weight
  s <- ciprofolWang(30, 140, 8, "female")$PK$default
  w <- pkSizeFactors(30, 140, 8, "female", TRUE)$pkWeight
  expect_false(isTRUE(all.equal(w, 30)))
  expect_equal(s$v1, 0.506 * w)
  expect_equal(s$v2, 0.231 * w)
  expect_equal(s$v3, 1.36 * w)
  expect_equal(s$cl1, 0.0312 * w)
  expect_equal(s$cl2, 0.0282 * w)
  expect_equal(s$cl3, 0.0198 * w)
})

test_that("the group chooses Wang for children and Liu for adults", {
  expect_match(ciprofol(20, 115, 5, "male")$modelChosen, "ciprofolWang")
  expect_match(ciprofol(70, 170, 35, "male")$modelChosen, "ciprofolLiu")
  expect_match(ciprofol(45, 160, 15, "female")$modelChosen, "ciprofolLiu")
})

test_that("the time to peak effect is 2.94 min", {
  p <- getDrugPK("ciprofol", 62, 165, 33, "male")$PK$default
  peak <- effectSitePeakTime(c(p$p_coef_bolus_l1, p$p_coef_bolus_l2, p$p_coef_bolus_l3),
                             c(p$lambda_1, p$lambda_2, p$lambda_3), p$ke0)
  expect_equal(round(peak, 2), 2.94)
  # at Liu's reference, with the switch off, the solved ke0 is the paper's 7.04 /h
  r <- getDrugPK("ciprofol", 62, 165, 33, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(round(r$ke0 * 60, 2), 7.04)
})

test_that("the BIS % effect row is drawn and 0.4 mg/kg falls BIS by about half", {
  rows <- pdEffectRows("ciprofol")
  expect_identical(rows$endpoint, "BIS")
  dose <- data.frame(Drug = "ciprofol", Time = 0, Dose = 0.4, Units = "mg/kg")
  ev <- data.frame(Time = double(), Event = character())
  e <- simulateDrugsWithCovariates(dose, ev, 70, 170, 40, "male", 30, FALSE)$ciprofol
  top <- max(pdEffectSeries(e, rows)$Y, na.rm = TRUE)
  expect_gt(top, 30)
  expect_lt(top, 90)
})

test_that("ciprofol is a hypnotic in the library, in ng/mL, without TCI", {
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "ciprofol", ]
  expect_identical(row$Category, "Hypnotics and sedatives")
  expect_identical(row$Concentration.Units, "ng")
  expect_false(grepl("target", row$Units))
})
