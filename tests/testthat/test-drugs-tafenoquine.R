# Tafenoquine (R/drugs_tafenoquine.R): Thakkar 2018's two-compartment model,
# Table 3.  Drafted by Claude Code, 2026-11-20.  Expectations are computed
# here from the paper and the labels, not from the drug file.

thakkar <- function(w) list(
  cl = 2.96 * (w / 70)^0.75, q = 5.09 * (w / 70)^0.75,
  v2 = 915 * w / 70, v3 = 664 * w / 70)

test_that("Thakkar 2018, Table 3, switch off (patients, tablet)", {
  for (w in c(45, 70, 110)) {
    set <- tafenoquine(w, 170, 35, "male", adjustToFFM = FALSE)$PK$default
    ref <- thakkar(w)
    expect_equal(set$cl1, ref$cl / 60)
    expect_equal(set$cl2, ref$q / 60)
    expect_equal(set$v1, ref$v2)
    expect_equal(set$v2, ref$v3)
  }
  set <- tafenoquine(70, 170, 35, "female", adjustToFFM = FALSE)$PK$default
  expect_equal(set$ka_PO, 0.252 / 60)
  expect_equal(set$tlag_PO, 0.908 * 60)
  expect_equal(set$bioavailability_PO, 1)
  expect_equal(set$cl3, 0)
})

test_that("switch on: weight at the pharmacokinetic weight", {
  pkW <- pkSizeFactors(120, 170, 50, "male")$pkWeight
  set <- tafenoquine(120, 170, 50, "male")$PK$default
  expect_equal(set$cl1, thakkar(pkW)$cl / 60)
  expect_equal(set$v1, thakkar(pkW)$v2)
  expect_equal_rounded(pkW, 91.34347)
})

test_that("the labels' clearance, volume and half-life", {
  set <- tafenoquine(70, 170, 35, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(set$cl1 * 60, 2.96)              # Krintafel: about 3 L/h
  expect_equal(set$v1 + set$v2, 1579)           # Krintafel: about 1,600 L
  k10 <- set$cl1 / set$v1; k12 <- set$cl2 / set$v1; k21 <- set$cl2 / set$v2
  a <- k10 + k12 + k21
  lambdaZ <- (a - sqrt(a^2 - 4 * k10 * k21)) / 2
  expect_equal(round(log(2) / lambdaZ / 1440, 1), 17.2)   # labels: 15, 16.5 days
  # Arakoda Table 5: AUCinf 70 h.mcg/mL after 200 mg
  expect_equal(round(200 / (set$cl1 * 60), 1), 67.6)
})

test_that("simulated peak after 300 mg (Krintafel: 12 to 15 hours)", {
  dt <- data.frame(Drug = "tafenoquine", Time = 0, Dose = 300, Units = "mg PO")
  events <- data.frame(Time = numeric(0), Event = character(0), Fill = character(0))
  w <- simulateDrugsWithCovariates(dt, events, 70, 170, 35, "male", 10 * 1440, FALSE,
                                   adjustToFFM = FALSE)$tafenoquine$wide
  i <- which.max(w$Plasma)
  expect_equal(round(w$Plasma[i]), 291)
  expect_gt(w$Time[i] / 60, 14)
  expect_lt(w$Time[i] / 60, 17)
  # AUC to 10 days against the closed form dose/CL less what remains
  auc <- sum(diff(w$Time) * (head(w$Plasma, -1) + tail(w$Plasma, -1)) / 2) / 60 / 1000
  expect_lt(auc, 300 / 2.96)
  expect_gt(auc, 0.4 * 300 / 2.96)
})

test_that("no band, no effect site", {
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "tafenoquine", ]
  expect_equal(c(row$Lower, row$Typical, row$Upper, row$MEAC, row$endCe), c(0, 0, 0, 0, 0))
  expect_identical(therapeuticRange("tafenoquine")$band_status, "none_established")
  expect_equal(tafenoquine(70, 170, 40, "male")$tPeak, 0)
})
