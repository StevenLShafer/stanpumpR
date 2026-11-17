# Flumazenil (R/drugs_flumazenil.R): Fiset 1995's three compartments and EEG
# effect site.  Drafted by Claude Code, 2026-11-12.  Expectations from Tables
# III and IV of the source and the Romazicon label, not from the drug file.

events <- data.frame(Time = double(), Event = character())

test_that("Table IV's values for the source's mean man, either switch position", {
  for (ffm in c(TRUE, FALSE)) {
    p <- flumazenil(79, 177.2, 31.7, "male", adjustToFFM = ffm)$PK$default
    expect_equal(c(p$v1, p$v2, p$v3), c(5.6, 15.93, 39.44))
    expect_equal(c(p$cl1, p$cl2, p$cl3), c(1.12, 1.79, 0.86))
  }
  # switch off: the published values at any size
  p <- flumazenil(50, 160, 70, "female", adjustToFFM = FALSE)$PK$default
  expect_equal(c(p$v1, p$cl1), c(5.6, 1.12))
})

test_that("the derived half-lives Table IV prints", {
  p <- flumazenil(79, 177.2, 31.7, "male")$PK$default
  k10 <- p$cl1 / p$v1; k12 <- p$cl2 / p$v1; k13 <- p$cl3 / p$v1
  k21 <- p$cl2 / p$v2; k31 <- p$cl3 / p$v3
  A <- matrix(c(-(k10 + k12 + k13), k21, k31,
                k12, -k21, 0,
                k13, 0, -k31), 3, byrow = TRUE)
  hl <- sort(log(2) / -Re(eigen(A)$values))
  expect_equal(hl, c(0.9, 11.4, 63.2), tolerance = 0.05)
})

test_that("ke0 is Table III's mean t1/2 ke0, 4.46 min; peak effect 3.3 min", {
  pk <- getDrugPK("flumazenil", 79, 177.2, 31.7, "male",
                  drugDefaults = getDrugDefaultsGlobal())
  expect_equal(flumazenil(79, 177.2, 31.7, "male")$ke0, log(2) / 4.46)
  expect_equal(pk$tPeak, 3.31, tolerance = 0.01)
})

test_that("the label's AUC after 1 mg over 5 min", {
  # Romazicon label: AUC 15 ng.h/mL (venous; its Cmax, 24 ng/mL, is lower
  # than this arterial model's, as the header explains)
  d <- data.frame(Drug = "flumazenil", Time = c(0, 5), Dose = c(12, 0),
                  Units = "mg/hr")
  w <- simulateDrugsWithCovariates(d, events, 79, 177.2, 31.7, "male", 600,
                                   FALSE)$flumazenil$wide
  auc <- sum(diff(w$Time) * (head(w$Plasma, -1) + tail(w$Plasma, -1)) / 2) / 60
  expect_equal(auc, 15, tolerance = 0.02)
})

test_that("the band is the label's complete-antagonism range", {
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "flumazenil", ]
  expect_equal(c(row$Lower, row$Upper, row$Typical), c(12, 28, 20))
})
