# Dofetilide (R/drugs_dofetilide.R): the phase III population model of the
# FDA review of NDA 20-931.  Drafted by Claude Code, 2026-11-12.
# Expectations from the review's text and the Tikosyn label, not from the
# drug file.

events <- data.frame(Time = double(), Event = character())

test_that("the review's equations: CL/F 14.1 L/h at the median CLcr 75 and 80 kg", {
  # a man whose Cockcroft-Gault clearance is 75 mL/min at 80 kg
  scr <- (140 - 60) * 80 / (72 * 75)
  p <- dofetilide(80, 175, 60, "male", adjustToFFM = FALSE,
                  creatinine = scr)$PK$default
  expect_equal(p$cl1 * 60, 4.5 + 0.128 * 75)
  expect_equal(p$cl1 * 60, 14.1, tolerance = 0.001)
  expect_equal(p$v1, 247)
  expect_equal(p$ka_PO * 60, 1.69)
  expect_equal(p$bioavailability_PO, 1)
  # V/F changes 18.5 L per 10 kg
  q <- dofetilide(90, 175, 60, "male", adjustToFFM = FALSE, creatinine = scr)$PK$default
  expect_equal(q$v1 - p$v1, 18.5)
  # women 12% lower, at the same creatinine clearance
  w <- dofetilide(80, 175, 60, "female", adjustToFFM = FALSE,
                  creatinine = scr * 0.85)$PK$default
  expect_equal(w$cl1 / p$cl1, 0.88)
})

test_that("Allen 2000's healthy men at 500 mcg twice daily", {
  # day 5: trough 1.24 ng/mL, AUC 25.39 ng.h/mL (peak 3.80; a one-compartment
  # fit to sparse data runs low there)
  d <- data.frame(Drug = "dofetilide", Time = 0, Dose = 500, Units = "mcg PO bid")
  w <- simulateDrugsWithCovariates(d, events, 68, 175, 25, "male", 5 * 1440,
                                   FALSE, adjustToFFM = FALSE)$dofetilide$wide
  s <- w[w$Time >= 4 * 1440 & w$Time <= 4 * 1440 + 720, ]
  auc <- sum(diff(s$Time) * (head(s$Plasma, -1) + tail(s$Plasma, -1)) / 2) / 60
  expect_equal(auc, 25.39, tolerance = 0.03)
  expect_equal(tail(s$Plasma, 1), 1.24, tolerance = 0.03)
  expect_lt(max(s$Plasma), 3.80)
  # and within the label's 1-3.5 ng/mL
  expect_gt(min(s$Plasma), 1); expect_lt(max(s$Plasma), 3.5)
})

test_that("half-life near the label's 10 h; finite at any renal function", {
  p <- dofetilide(70, 170, 35, "male")$PK$default
  expect_equal(log(2) * p$v1 / p$cl1 / 60, 8.3, tolerance = 0.01)
  r <- dofetilide(70, 170, 35, "male", creatinine = 15)$PK$default
  expect_gt(r$cl1, 4.5 / 60)
})
