# Everolimus (R/drugs_everolimus.R): the FDA reviewer's two-compartment model
# of NDA 22-334.  Drafted by Claude Code, 2026-11-12.  Expectations from the
# review's Table 4 and text and the Zortress label, not from the drug file.

events <- data.frame(Time = double(), Event = character())

test_that("the reviewer's Table 4 parameters", {
  p <- everolimus(70, 170, 35, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(c(p$v1, p$v2), c(172, 495))
  expect_equal(c(p$cl1, p$cl2) * 60, c(14.8, 48))
  expect_equal(p$ka_PO * 60, 6.07)
  expect_equal(p$bioavailability_PO, 1)
  # the reference man takes the published values with the switch on too
  q <- everolimus(70, 170, 35, "male")$PK$default
  expect_equal(c(q$v1, q$cl1), c(p$v1, p$cl1))
})

test_that("half-life in the range the review reports for patients", {
  # the review: 30.7 (8.4) h in patients, 36.9 (7.5) h in healthy subjects
  p <- everolimus(70, 170, 35, "male")$PK$default
  k10 <- p$cl1 / p$v1; k12 <- p$cl2 / p$v1; k21 <- p$cl2 / p$v2
  A <- matrix(c(-(k10 + k12), k21, k12, -k21), 2, byrow = TRUE)
  hl <- max(log(2) / -eigen(A)$values / 60)
  expect_gt(hl, 30.7 - 8.4); expect_lt(hl, 36.9 + 7.5)
})

test_that("10 mg daily: the C2240 patients' clearance; transplant exposure lower", {
  d <- data.frame(Drug = "everolimus", Time = 0, Dose = 10, Units = "mg PO qd")
  w <- simulateDrugsWithCovariates(d, events, 70, 170, 35, "male", 15 * 1440,
                                   FALSE)$everolimus$wide
  s <- w[w$Time >= 14 * 1440, ]
  auc <- sum(diff(s$Time) * (head(s$Plasma, -1) + tail(s$Plasma, -1)) / 2) / 60
  # CL/F 15.4 (5.3) L/h on day 15 in the patients sampled intensively
  expect_equal(10e3 / auc, 15.4, tolerance = 0.06)
  # Zortress 0.75 mg bid with cyclosporine: AUC 75 ng.h/mL per 12 h; without
  # cyclosporine the model gives less
  d <- data.frame(Drug = "everolimus", Time = 0, Dose = 0.75, Units = "mg PO bid")
  w <- simulateDrugsWithCovariates(d, events, 70, 170, 35, "male", 15 * 1440,
                                   FALSE)$everolimus$wide
  s <- w[w$Time >= 14 * 1440 & w$Time <= 14 * 1440 + 720, ]
  auc <- sum(diff(s$Time) * (head(s$Plasma, -1) + tail(s$Plasma, -1)) / 2) / 60
  expect_equal(auc, 750 / 14.8, tolerance = 0.02)
  expect_lt(auc, 75)
})

test_that("the band is Zortress's 3-8 ng/mL", {
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "everolimus", ]
  expect_equal(c(row$Lower, row$Upper, row$Typical), c(3, 8, 5.5))
})
