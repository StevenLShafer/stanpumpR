# Levosimendan (R/drugs_levosimendan.R): Jonsson 2003, two compartments in
# 190 patients with heart failure, weight linear on CL and V1.
# Drafted by Claude Code, 2026-11-25.

events <- data.frame(Time = double(), Event = character())

test_that("Table 5 at 76 kg, and the linear weight terms, with the switch off", {
  s <- levosimendan(76, 173, 61, "male", adjustToFFM = FALSE)$PK$default
  expect_equal_rounded(13.3 / 60, s$cl1)
  expect_equal_rounded(16.8, s$v1)
  expect_equal_rounded(2.33 / 60, s$cl2)
  expect_equal_rounded(30.0, s$v2)
  s100 <- levosimendan(100, 173, 61, "male", adjustToFFM = FALSE)$PK$default
  expect_equal_rounded(13.3 / 60 * (1 + 0.015 * 24), s100$cl1)
  expect_equal_rounded(16.8 * (1 + 0.009 * 24), s100$v1)
  expect_equal_rounded(2.33 / 60, s100$cl2)   # no weight term in the source
})

test_that("outside 46-130 kg the weight terms continue allometrically and stay positive", {
  f46 <- levosimendanWeightFactors(46)
  f10 <- levosimendanWeightFactors(10)
  expect_equal(f10$cl, f46$cl * (10 / 46)^0.75)
  expect_equal(f10$v1, f46$v1 * 10 / 46)
  for (w in c(0.5, 3, 9.33, 200, 300)) {
    f <- levosimendanWeightFactors(w)
    expect_gt(f$cl, 0); expect_gt(f$v1, 0)
  }
  s <- getDrugPK("levosimendan", 3, 50, 0.1, "female")$PK$default
  expect_true(all(is.finite(c(s$cl1, s$v1, s$cl2, s$v2))) && s$cl1 > 0)
})

test_that("with the switch on, a patient like the cohort's gets the published Q and V2", {
  coh <- levosimendan(76, 173, 61, "male", adjustToFFM = TRUE)$PK$default
  f <- do.call(cohortSizeFactors, LEVOSIMENDAN_COHORT)
  man <- pkSizeFactors(76, 173, 61, "male", TRUE)
  expect_equal(coh$v2, 30.0 * man$volume / f$volume)
  expect_equal(coh$cl1, LEVOSIMENDAN_CL * levosimendanWeightFactors(man$pkWeight)$cl)
})

test_that("with the switch on, a 120 kg man's four parameters are pinned", {
  # 120 kg, 175 cm, 50 years: pharmacokinetic weight 94.39 kg.  CL and V1 are
  # Table 5's linear terms at that weight, 13.3/60 x (1 + 0.015 x 18.39) and
  # 16.8 x (1 + 0.009 x 18.39); Q and V2, size-free in the source, take the
  # clearance and volume factors relative to the cohort's 76 kg patient.
  s <- levosimendan(120, 175, 50, "male", adjustToFFM = TRUE)$PK$default
  expect_equal_rounded(0.28281552, s$cl1)
  expect_equal_rounded(19.580664, s$v1)
  expect_equal_rounded(0.047572966, s$cl2)
  expect_equal_rounded(39.324391, s$v2)
})

test_that("the typical parameters give a 0.74-h phase carrying 99% of a bolus and a small 10.6-h tail", {
  p <- getDrugPK("levosimendan", 76, 173, 61, "male", adjustToFFM = FALSE)$PK$default
  hl <- log(2) / c(p$lambda_1, p$lambda_2) / 60
  expect_equal(round(hl, 2), c(0.74, 10.63))
  a <- c(p$p_coef_bolus_l1, p$p_coef_bolus_l2)
  expect_gt(a[1] / sum(a), 0.98)
})

test_that("a day's infusion comes within 4% of the steady state its clearance sets, and 90% is gone 6 h after", {
  dose <- data.frame(Drug = "levosimendan", Time = c(0, 1440),
                     Dose = c(0.1, 0), Units = "mcg/kg/min")
  wide <- simulateDrugsWithCovariates(dose, events, 76, 173, 61, "male", 1800,
                                      FALSE, adjustToFFM = FALSE)$levosimendan$wide
  ss <- 0.1 * 76 / (13.3 / 60)            # mcg/min over L/min = ng/mL
  late <- wide$Plasma[which.min(abs(wide$Time - 1440))]
  expect_lt(abs(late / ss - 1), 0.04)
  expect_lt(wide$Plasma[which.min(abs(wide$Time - 1440 - 360))] / late, 0.11)
})

test_that("levosimendan is a vasoactive drug, plasma only, with no band and no US label", {
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "levosimendan", ]
  expect_identical(row$Category, VASOACTIVE_DRUG_CATEGORY)
  expect_equal(c(row$MEAC, row$Lower, row$Upper), c(0, 0, 0))
  expect_equal(getDrugPK("levosimendan", 70, 170, 50, "male")$tPeak, 0)
})
