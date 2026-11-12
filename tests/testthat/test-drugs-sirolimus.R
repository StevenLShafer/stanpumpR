# Sirolimus (R/drugs_sirolimus.R): Dansirikul 2006's two compartments,
# apparent oral, whole blood.  Drafted by Claude Code, 2026-11-12.
# Expectations from Table 4 of the source and the Rapamune label, not from the
# drug file.

events <- data.frame(Time = double(), Event = character())

test_that("Table 4's parameters; CL/F 12.5 L/h at the median age, 44", {
  p <- sirolimus(70, 170, 44, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(p$cl1 * 60, 16.9 * 0.742)
  expect_equal(p$cl1 * 60, 12.5, tolerance = 0.005)
  expect_equal(c(p$cl2 * 60, p$v1, p$v2), c(20.4, 117, 583))
  expect_equal(p$ka_PO * 60, 2.305)
  expect_equal(p$bioavailability_PO, 1)
  # the age term, held at 18 below
  expect_equal(sirolimus(70, 170, 88, "male", adjustToFFM = FALSE)$PK$default$cl1 * 60,
               16.9 * 0.742^2)
  expect_equal(sirolimus(30, 140, 10, "male", adjustToFFM = FALSE)$PK$default$cl1,
               sirolimus(30, 140, 18, "male", adjustToFFM = FALSE)$PK$default$cl1)
})

test_that("a terminal half-life near the label's 62 +/- 16 h", {
  p <- sirolimus(70, 170, 44, "male", adjustToFFM = FALSE)$PK$default
  k10 <- p$cl1 / p$v1; k12 <- p$cl2 / p$v1; k21 <- p$cl2 / p$v2
  A <- matrix(c(-(k10 + k12), k21, k12, -k21), 2, byrow = TRUE)
  hl <- log(2) / -eigen(A)$values / 60
  expect_equal(max(hl), 56, tolerance = 0.01)
  expect_gt(max(hl), 62 - 16)
})

test_that("2 mg daily: below the label's exposure with cyclosporine", {
  # Rapamune Table 4 (tablets, with cyclosporine, which slows clearance):
  # AUC 230 ng.h/mL, trough 7.6 ng/mL.  This cohort was mostly without it.
  d <- data.frame(Drug = "sirolimus", Time = 0, Dose = 2, Units = "mg PO qd")
  w <- simulateDrugsWithCovariates(d, events, 70, 170, 35, "male", 30 * 1440,
                                   FALSE)$sirolimus$wide
  s <- w[w$Time >= 29 * 1440, ]
  auc <- sum(diff(s$Time) * (head(s$Plasma, -1) + tail(s$Plasma, -1)) / 2) / 60
  expect_equal(auc, 2e3 / (sirolimus(70, 170, 35, "male")$PK$default$cl1 * 60),
               tolerance = 0.01)
  expect_lt(auc, 230)
  expect_gt(auc, 230 / 2)
})

test_that("the band is the label's maintenance range", {
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "sirolimus", ]
  expect_equal(c(row$Lower, row$Upper, row$Typical), c(12, 20, 16))
})
