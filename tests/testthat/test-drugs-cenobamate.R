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

# Vernillet 2020 (healthy volunteers, capsules), geometric means.  These
# studies are probably within the pooled fit, so they check consistency; the
# header gives the numbers.
cenobamateCurve <- function(dose, units, days) {
  w <- simulateDrugsWithCovariates(
    data.frame(Drug = "cenobamate", Time = 0, Dose = dose, Units = units),
    noEvents, 70, 170, 35, "male", days * 1440, TRUE)$cenobamate$wide
  w[, c("Time", "Plasma")]
}
cenobamateAUC <- function(w, from, to) {
  s <- w[w$Time >= from & w$Time <= to, ]
  sum(diff(s$Time) * (head(s$Plasma, -1) + tail(s$Plasma, -1)) / 2) / 60
}

test_that("the first day of daily dosing matches Vernillet's Supplemental Table 5", {
  # the model is linear, so one 100 mg curve scales to every dose
  w <- cenobamateCurve(100, "mg PO", 1)
  perMg <- cenobamateAUC(w, 0, 1440) / 100
  dose <- c(50, 100, 150, 150, 200, 250, 300)
  observed <- c(21.0, 40.3, 70.3, 73.0, 93.1, 104, 142)   # AUC0-24, mcg h/mL
  ratio <- dose * perMg / observed
  expect_true(all(ratio > 0.85 & ratio < 1.07))
})

test_that("steady state is about 20% below Vernillet's healthy volunteers", {
  # Table 3: AUCtau on day 14 of daily dosing; the gap is the healthy-versus-
  # patient difference the header describes, and it does not depend on dose
  dose <- c(50, 200, 500)
  observed <- c(96.7, 482, 1110)
  ratio <- sapply(seq_along(dose), function(i) {
    w <- cenobamateCurve(dose[i], "mg PO qd", 14)
    cenobamateAUC(w, 13 * 1440, 14 * 1440) / observed[i]
  })
  expect_true(all(ratio > 0.72 & ratio < 0.96))
  expect_equal(exp(mean(log(ratio))), 0.84, tolerance = 0.05)
})
