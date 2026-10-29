# cenobamate: see R/drugs_cenobamate.R (Vashi 2023, the sponsor's population
# model, Supplemental Table 2 = the FDA review of NDA 212839, Table 2;
# FFM in place of the source's linear weight and body-surface-area terms).
noEvents <- data.frame(Time = numeric(0), Event = character(0))

test_that("reference patient, switch off: Supplemental Table 2", {
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
# studies are within the pooled fit (Vashi 2023 Table 1), so they check
# consistency; the header gives the numbers.
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

test_that("the size scaling gives the weight effect the EMA report describes", {
  # EMA/CHMP/160820/2021 p 59: ~25% more exposure at low weights, ~20% less
  # at high weights than at the mean; exposure at steady state is dose/CL
  auc <- function(w) 1 / cenobamate(w, 170, 35, "male")$PK$default$cl1
  ref <- auc(70)
  expect_equal(auc(50) / ref, 1.16, tolerance = 0.02)
  expect_equal(auc(110) / ref, 0.84, tolerance = 0.02)
})

test_that("the concentration per dose matches two clinic TDM series", {
  # Kork (Steinhoff 2024) and Oslo (Johannessen Landmark 2026), adults with
  # refractory epilepsy, not in the fit: mean concentration / mean dose,
  # mg/L per mg/day.  The model's steady-state average is 1 / (CL/F x 24 h).
  perDose <- 1 / (cenobamate(70, 170, 35, "male")$PK$default$cl1 * 1440)
  expect_equal(perDose, 0.0782, tolerance = 1e-3)
  observed <- c(
    korkSeizureFree  = 17.8 / 240.3,
    korkNonResponder = 20.9 / 260.8,
    oslo             = 53.7 * 267.67 / 1000 / 164   # umol/L to mg/L
  )
  expect_true(all(abs(perDose / observed - 1) < 0.12))
  # Oslo, Figure 1D: means of the patients' own ratios, umol/L per mg/day,
  # without a strong inducer or inhibitor, and with an inducer
  osloGroups <- c(neutral = 0.339, inducer = 0.311) * 267.67 / 1000
  expect_true(all(perDose / osloGroups > 0.85 & perDose / osloGroups < 1))
  # with valproate or cannabidiol (0.384) the inhibition is not modelled
  expect_equal(perDose / (0.384 * 267.67 / 1000), 0.76, tolerance = 0.02)
})

test_that("the band is the AGNP 2026 range, and the troughs match the trials", {
  row <- getDrugDefaults("cenobamate")   # the defaults row draws the band
  # Hart 2026, Table 5: 10-25 mg/L, typical its midpoint
  expect_equal(c(row$Lower, row$Typical, row$Upper), c(10, 18, 25))
  x <- cenobamate(70, 170, 35, "male")
  expect_equal(c(x$lowerTypical, x$typical, x$upperTypical), c(10, 18, 25))
  # linear model: troughs scale with dose from the 200 mg steady state; the
  # usual 200 mg/day trough is inside the consensus range, 100 and 400 are not
  troughs <- 13.68 * c(100, 200, 400) / 200
  expect_true(troughs[2] > row$Lower && troughs[2] < row$Upper)
  expect_true(troughs[1] < row$Lower && troughs[3] > row$Upper)
  # Greene and Kamin, AAN 2021: central 90% of the maintenance troughs in
  # the 50% responders and in the seizure-free, at 100, 200, 400 mg/day
  responders  <- rbind(c(4.6, 9.3), c(11.3, 18.7), c(20.8, 35.2))
  seizureFree <- rbind(c(5.1, 9.7), c(10.8, 19.8), c(21.9, 35.8))
  for (r in list(responders, seizureFree)) {
    expect_true(all(troughs > r[, 1] & troughs < r[, 2]))
    expect_true(all(troughs / rowMeans(r) > 0.88 & troughs / rowMeans(r) < 1))
  }
  # C021 (AAN 2022): mean troughs on 200 mg/day, all patients, 50%
  # responders, seizure-free for 12 months or more
  expect_true(all(troughs[2] / c(15.1, 15.2, 14.5) > 0.88 &
                  troughs[2] / c(15.1, 15.2, 14.5) < 0.96))
})
