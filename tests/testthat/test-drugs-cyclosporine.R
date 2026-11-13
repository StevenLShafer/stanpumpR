# Cyclosporine (R/drugs_cyclosporine.R): Fanta 2007's three compartments.
# Drafted by Claude Code, 2026-11-13.  Expectations from Table 2 of the
# source and the Neoral and Sandimmune labels, not from the drug file.

events <- data.frame(Time = double(), Event = character())

test_that("Table 2 at 13 kg, and its allometric scaling", {
  p <- cyclosporine(13, 90, 3, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(c(p$cl1, p$cl2, p$cl3) * 60, c(6.1, 1.5, 3.0))
  expect_equal(c(p$v1, p$v2, p$v3), c(5.3, 19.6, 4.4))
  expect_equal(p$ka_PO * 60, 0.68)
  expect_equal(p$bioavailability_PO, 0.36 / 0.77)
  q <- cyclosporine(52, 160, 14, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(q$cl1 / p$cl1, 4^0.75)
  expect_equal(q$v1 / p$v1, 4)
})

test_that("intravenous clearance and half-life match the labels", {
  p <- cyclosporine(70, 170, 35, "male", adjustToFFM = FALSE)$PK$default
  # Neoral: 5 to 7 mL/min/kg in adult transplant recipients (5.1 here)
  clmlminkg <- p$cl1 * 1000 / 70
  expect_gt(clmlminkg, 5 * 0.98); expect_lt(clmlminkg, 7)
  # Sandimmune: terminal half-life about 19 h (10-27)
  k10 <- p$cl1 / p$v1; k12 <- p$cl2 / p$v1; k13 <- p$cl3 / p$v1
  k21 <- p$cl2 / p$v2; k31 <- p$cl3 / p$v3
  A <- matrix(c(-(k10 + k12 + k13), k21, k31, k12, -k21, 0, k13, 0, -k31), 3, byrow = TRUE)
  hl <- max(log(2) / -Re(eigen(A)$values) / 60)
  expect_gt(hl, 10); expect_lt(hl, 27)
})

test_that("oral exposure: below the Neoral label's adults, as the help page says", {
  # stable renal recipients, 344 mg/day: AUC 6035 ng.h/mL per interval,
  # trough 251 ng/mL
  d <- data.frame(Drug = "cyclosporine", Time = 0, Dose = 172, Units = "mg PO bid")
  w <- simulateDrugsWithCovariates(d, events, 70, 170, 35, "male", 7 * 1440,
                                   FALSE)$cyclosporine$wide
  s <- w[w$Time >= 6 * 1440 & w$Time <= 6 * 1440 + 720, ]
  auc <- sum(diff(s$Time) * (head(s$Plasma, -1) + tail(s$Plasma, -1)) / 2) / 60
  expect_equal(auc, 172e3 * 0.36 / 0.77 /
                 (cyclosporine(70, 170, 35, "male")$PK$default$cl1 * 60), tolerance = 0.02)
  expect_gt(6035 / auc, 1.3); expect_lt(6035 / auc, 1.7)
})

test_that("the band is the Sandimmune label's 100-200 ng/mL", {
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "cyclosporine", ]
  expect_equal(c(row$Lower, row$Upper, row$Typical), c(100, 200, 150))
})
