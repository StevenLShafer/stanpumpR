# cenobamate: see R/drugs_cenobamate.R (Vashi 2023, the sponsor's population
# model, parameters from the FDA clinical pharmacology review of NDA 212839;
# FFM in place of the source's linear weight and body-surface-area terms).
noEvents <- data.frame(Time = numeric(0), Event = character(0))

test_that("reference patient, switch off: the review's Table 2", {
  a <- cenobamate(70, 170, 40, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(a$v1, 42.4); expect_equal(a$v2, 7.27)
  expect_equal(a$cl1, 0.533 / 60); expect_equal(a$cl2, 1.84 / 60)
  expect_equal(a$ka_PO, 2.74 / 60)
  expect_equal(a$tlag_PO, 14.4)
  expect_equal(a$bioavailability_PO, 1)
  # the published values for everyone with the switch off
  b <- cenobamate(120, 160, 70, "female", adjustToFFM = FALSE)$PK$default
  expect_equal(b[c("v1", "v2", "cl1", "cl2")], a[c("v1", "v2", "cl1", "cl2")])
})

test_that("FFM scaling, switch on", {
  a <- cenobamate(120, 170, 50, "male", adjustToFFM = TRUE)$PK$default
  s <- pkSizeFactors(120, 170, 50, "male", TRUE)
  expect_equal(a$v1, 42.4 * s$volume); expect_equal(a$v2, 7.27 * s$volume)
  expect_equal(a$cl1, 0.533 * s$clearance / 60)
  expect_equal(a$cl2, 1.84 * s$clearance / 60)
  # the reference man receives the published values
  r <- cenobamate(70, 170, 35, "male", adjustToFFM = TRUE)$PK$default
  expect_equal(r$v1, 42.4); expect_equal(r$cl1, 0.533 / 60)
})

test_that("half-life and volume agree with the label", {
  a <- cenobamate(70, 170, 35, "male")$PK$default
  k10 <- a$cl1 / a$v1; k12 <- a$cl2 / a$v1; k21 <- a$cl2 / a$v2
  s <- k10 + k12 + k21
  beta <- (s - sqrt(s^2 - 4 * k10 * k21)) / 2
  expect_equal_rounded(log(2) / beta / 60, 65.0098, tolerance = 1e-5)  # h
  expect_equal(a$v1 + a$v2, 49.67)       # L, label Vd/F 40-50 L
})

test_that("200 mg once daily reaches the phase 3 steady state", {
  days <- 28
  w <- simulateDrugsWithCovariates(
    data.frame(Drug = "cenobamate", Time = 0, Dose = 200, Units = "mg PO qd"),
    noEvents, 70, 170, 35, "male", days * 1440, TRUE)$cenobamate$wide
  last <- w[w$Time >= (days - 1) * 1440, ]
  # Analytic steady state: trough 13.68, peak 17.86 mg/L, average 15.63
  # (200 mg / (0.533 L/h x 24 h)); the phase 3 mean at 200 mg/day was
  # 15.5 mg/L.  Four weeks is 10 half-lives, within 0.1% of steady state.
  expect_equal(min(last$Plasma), 13.68, tolerance = 0.01)
  expect_equal(max(last$Plasma), 17.86, tolerance = 0.01)
  expect_equal(mean(last$Plasma), 15.6, tolerance = 0.03)
  # peak about 1.6 h after the dose (label median Tmax 1-4 h)
  tPeak <- (last$Time[which.max(last$Plasma)] - (days - 1) * 1440) / 60
  expect_gt(tPeak, 1); expect_lt(tPeak, 2.5)
  # nothing before the 0.24 h lag on the first day
  expect_equal(w$Plasma[w$Time > 0 & w$Time < 14][1], 0)
})
