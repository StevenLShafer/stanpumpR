# Esketamine (R/drugs_esketamine.R): Sigtermans 2009, S(+)-ketamine, three
# compartments per 70 kg, sex on CL1.  Drafted by Claude Code, 2026-11-25.

events <- data.frame(Time = double(), Event = character())

test_that("Table 2's combined model at 70 kg, with the switch off", {
  m <- esketamine(70, 177, 22, "male", adjustToFFM = FALSE)$PK$default
  expect_equal_rounded(15.4, m$v1)
  expect_equal_rounded(26.6, m$v2)
  expect_equal_rounded(121, m$v3)
  expect_equal_rounded(75.1 / 60, m$cl1)
  expect_equal_rounded(80.6 / 60, m$cl2)
  expect_equal_rounded(146.0 / 60, m$cl3)
  f <- esketamine(70, 165, 22, "female", adjustToFFM = FALSE)$PK$default
  expect_equal_rounded(97.3 / 60, f$cl1)       # sex on CL1 only
  expect_equal(f$v1, m$v1)
  expect_equal(f$cl2, m$cl2)
  # linear in total weight with the switch off
  m35 <- esketamine(35, 177, 22, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(m35$v3, m$v3 / 2)
  expect_equal(m35$cl1, m$cl1 / 2)
})

test_that("with the switch on, a 120 kg man's six parameters are pinned", {
  # 120 kg, 175 cm, 50 years: pharmacokinetic weight 94.39 kg, so every
  # parameter is Table 2's per-70-kg value times 94.39/70 = 1.348.
  s <- esketamine(120, 175, 50, "male", adjustToFFM = TRUE)$PK$default
  expect_equal_rounded(20.765939, s$v1)
  expect_equal_rounded(35.868441, s$v2)
  expect_equal_rounded(163.16095, s$v3)
  expect_equal_rounded(1.6877944, s$cl1)
  expect_equal_rounded(1.8114012, s$cl2)
  expect_equal_rounded(3.2811982, s$cl3)
})

test_that("the effect site peaks at ketamine's 3 minutes, and there is no band", {
  out <- esketamine(70, 170, 35, "male")
  expect_null(out$ke0)
  expect_equal(out$tPeak, ketamine(70, 170, 35, "male")$tPeak)
  expect_equal(out$tPeak, 3)
  expect_equal(c(out$MEAC, out$typical, out$lowerTypical, out$upperTypical),
               rep(0, 4))
  p <- getDrugPK("esketamine", 70, 170, 35, "male")$PK$default
  peak <- effectSitePeakTime(c(p$p_coef_bolus_l1, p$p_coef_bolus_l2, p$p_coef_bolus_l3),
                             c(p$lambda_1, p$lambda_2, p$lambda_3), p$ke0)
  expect_equal(peak, 3, tolerance = 1e-3)
})

test_that("esketamine is cleared faster than the racemic library ketamine", {
  # The reason it is a drug of its own: Sigtermans' S(+)-ketamine clearance
  # is 36% (men) and 76% (women) above Domino's racemic 55 L/h at 70 kg.
  for (sex in c("male", "female")) {
    e <- getDrugPK("esketamine", 70, 170, 35, sex, adjustToFFM = FALSE)$PK$default
    k <- getDrugPK("ketamine", 70, 170, 35, sex, adjustToFFM = FALSE)$PK$default
    expect_gt(e$cl1 / k$cl1, 1.3)
  }
})

test_that("a day's infusion reaches the steady state its clearance sets", {
  dose <- data.frame(Drug = "esketamine", Time = 0, Dose = 1, Units = "mg/hr")
  wide <- simulateDrugsWithCovariates(dose, events, 70, 177, 22, "male", 1440,
                                      FALSE, adjustToFFM = FALSE)$esketamine$wide
  ss <- (1 / 60) / (75.1 / 60)                 # mg/min over L/min = mcg/mL
  late <- wide$Plasma[which.min(abs(wide$Time - 1440))]
  expect_lt(abs(late / ss - 1), 0.02)
  expect_lt(abs(wide$`Effect Site`[which.min(abs(wide$Time - 1440))] / late - 1), 0.01)
})

test_that("esketamine is a hypnotic in the library with no US label for its route", {
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "esketamine", ]
  expect_identical(row$Category, "Hypnotics and sedatives")
  expect_equal(c(row$MEAC, row$Lower, row$Upper), c(0, 0, 0))
})
