# Ambrisentan (R/drugs_ambrisentan.R): Okour 2022's two-compartment model,
# allometric in weight.  Drafted by Claude Code, 2026-11-16.  Expectations are
# computed here from Table 2, not from the drug file.

events <- data.frame(Time = double(), Event = character())

test_that("Table 2, switch off", {
  set <- ambrisentan(70, 170, 40, "female", adjustToFFM = FALSE)$PK$default
  expect_equal(set$cl1 * 60, 1.17)
  expect_equal(set$v1, 12.3)
  expect_equal(set$cl2 * 60, 0.457)
  expect_equal(set$v2, 81.3)
  expect_equal(set$ka_PO * 60, 2.46)
  expect_equal(set$tlag_PO, 0.525 * 60)
  # allometric weight, exponents 0.75 and 1 fixed
  set <- ambrisentan(35, 140, 12, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(set$cl1 * 60, 1.17 * 0.5^0.75)
  expect_equal(set$v2, 81.3 * 0.5)
})

test_that("the label (adults with PAH, not fitted) and Okour's adult comparison", {
  dt <- data.frame(Drug = "ambrisentan", Time = 0, Dose = 10, Units = "mg PO qd")
  w <- simulateDrugsWithCovariates(dt, events, 70, 170, 50, "male", 41 * 1440, FALSE,
                                   adjustToFFM = FALSE)$ambrisentan$wide
  auc <- function(a, b) {
    k <- w$Time >= a & w$Time <= b
    sum(diff(w$Time[k]) * (head(w$Plasma[k], -1) + tail(w$Plasma[k], -1)) / 2) / 60
  }
  d <- 40 * 1440
  k <- w$Time >= d & w$Time <= d + 1440
  # label: CL/F 19 mL/min (1.14 L/h); here 1.17
  expect_equal(1.17 * 1000 / 60, 19.5)
  # steady-state AUC: Okour's adult model 9.12 ug.h/mL
  expect_equal(auc(d, d + 1440) / 1000, 9.12, tolerance = 0.10)
  # peak "approximately 2 hours" after a dose
  tmax <- (w$Time[k][which.max(w$Plasma[k])] - d) / 60
  expect_gt(tmax, 1); expect_lt(tmax, 3)
  # trough "about 15% of the mean peak"
  trough <- w$Plasma[which.min(abs(w$Time - (d + 1440)))]
  expect_equal(trough / max(w$Plasma[k]), 0.15, tolerance = 0.2)
})

test_that("switch on: the weight terms at the pharmacokinetic weight", {
  f <- 1.3049067                       # 120 kg, 170 cm, 50 y man
  set <- ambrisentan(120, 170, 50, "male")$PK$default
  expect_equal_rounded(set$v1, 12.3 * f)
  expect_equal_rounded(set$cl1 * 60, 1.17 * f^0.75)
})
