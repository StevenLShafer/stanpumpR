# Linezolid (R/drugs_linezolid.R): Boak 2014's one compartment with three
# absorption lag compartments.  Drafted by Claude Code, 2026-11-13.
# Expectations from Table 2 and equations 12-13 of the source and the Zyvox
# label, not from the drug file.

events <- data.frame(Time = double(), Event = character())

test_that("Table 2 at the standard 65 kg and a GFR of 120 mL/min", {
  # a 65-kg man whose ideal body weight exceeds 65 kg, and whose 65-kg
  # Cockcroft-Gault GFR is 120: (140 - 40) / SCR x 65/72 = 120
  scr <- (140 - 40) * 65 / 72 / 120
  p <- linezolid(65, 185, 40, "male", adjustToFFM = FALSE, creatinine = scr)$PK$default
  expect_equal(p$cl1 * 60, 4.55 + 2.17)
  expect_equal(p$v1, 44.3)
  expect_equal(p$ka_PO, log(2) / 10.3)
  expect_equal(p$bioavailability_PO, 1)
  pk <- linezolid(65, 185, 40, "male", adjustToFFM = FALSE, creatinine = scr)
  expect_equal(pk$transitChain$n, 4L)
  expect_equal(pk$transitChain$ktr, 3 / 69.2)
  # renal clearance scales with GFR; nonrenal does not
  p2 <- linezolid(65, 185, 40, "male", adjustToFFM = FALSE, creatinine = 2 * scr)$PK$default
  expect_equal(p2$cl1 * 60, 4.55 + 2.17 / 2)
})

test_that("size: the lower of ideal and total body weight", {
  # 120 kg, 170 cm man: WT is his Devine ideal weight, 65.9 kg
  ibw <- 50 + 2.3 * (170 / 2.54 - 60)
  p <- linezolid(120, 170, 40, "male", adjustToFFM = FALSE, creatinine = 1)$PK$default
  expect_equal(p$v1, 44.3 * ibw / 65)
  # a child: total weight
  q <- linezolid(20, 110, 6, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(q$v1, 44.3 * 20 / 65)
})

test_that("the Zyvox label's exposure at 600 mg every 12 hours", {
  # Table 8, intravenous: AUC0-12 89.7 (SD 31.0) mg.h/L, half-life 4.8 h
  d <- data.frame(Drug = "linezolid", Time = 0, Dose = 600, Units = "mg bid")
  w <- simulateDrugsWithCovariates(d, events, 70, 170, 35, "male", 4 * 1440,
                                   FALSE)$linezolid$wide
  s <- w[w$Time >= 3 * 1440 & w$Time <= 3 * 1440 + 720, ]
  auc <- sum(diff(s$Time) * (head(s$Plasma, -1) + tail(s$Plasma, -1)) / 2) / 60
  expect_gt(auc, 89.7 - 31); expect_lt(auc, 89.7 + 31)
  p <- linezolid(70, 170, 35, "male")$PK$default
  expect_equal(log(2) * p$v1 / p$cl1 / 60, 4.8, tolerance = 0.05)
  # oral: complete absorption, so the same steady-state AUC
  d$Units <- "mg PO bid"
  w2 <- simulateDrugsWithCovariates(d, events, 70, 170, 35, "male", 4 * 1440,
                                    FALSE)$linezolid$wide
  s2 <- w2[w2$Time >= 3 * 1440 & w2$Time <= 3 * 1440 + 720, ]
  auc2 <- sum(diff(s2$Time) * (head(s2$Plasma, -1) + tail(s2$Plasma, -1)) / 2) / 60
  expect_equal(auc2, auc, tolerance = 0.01)
})

test_that("switch on, a 120-kg man: ideal body weight is the size, by hand", {
  # 170 cm man: Devine 50 + 2.3 x (170/2.54 - 60) = 65.94 kg, below his
  # pharmacokinetic weight, so WT is the ideal weight; GFR at 65 kg with a
  # serum creatinine of 1.0: (140 - 50) x 65 / 72 = 81.25 mL/min
  ibw <- 50 + 2.3 * (170 / 2.54 - 60)
  expect_lt(ibw, 70 * ffmRatioMaleIndependent(120, 170, 50))
  p <- linezolid(120, 170, 50, "male", creatinine = 1)$PK$default
  expect_equal_rounded(44.3 * ibw / 65, p$v1)
  expect_equal_rounded((ibw / 65)^0.75 * (81.25 / 120 * 2.17 + 4.55) / 60, p$cl1)
})
