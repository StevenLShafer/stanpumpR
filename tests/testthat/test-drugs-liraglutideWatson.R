# liraglutideWatson: see R/drugs_liraglutideWatson.R (Watson 2010, Table II).
noEvents <- data.frame(Time = numeric(0), Event = character(0))
test_that("Table II, per kilogram, with the type 2 diabetes absorption", {
  m <- liraglutideWatson(80, 175, 55, "male", adjustToFFM = FALSE)
  pk <- m$PK$default
  expect_equal(pk$v1, 0.16 * 80)
  expect_equal(pk$cl1 * 60, 0.013 * 80)
  expect_equal(pk$ka_SC * 60, 0.154)
  expect_equal(pk$tlag_SC, 8 * 60)
  expect_equal(m$zeroOrderSC$fraction, 0.154 * 8 / (1 + 0.154 * 8))
  expect_equal(m$zeroOrderSC$duration, 8 * 60)
  size <- pkSizeFactors(80, 175, 55, "male", TRUE)
  expect_equal(liraglutideWatson(80, 175, 55, "male")$PK$default$v1, 0.16 * size$pkWeight)
})
test_that("the sequential absorption is Watson's Equations 1 and 2", {
  PK <- getDrugPK("liraglutideWatson", 70, 170, 40, "male", adjustToFFM = FALSE)
  w <- simCpCe(data.frame(Drug = "liraglutideWatson", Time = 0, Dose = 1.2, Units = "mg SC"),
               noEvents, PK, 3 * MINS_PER_DAY, FALSE)$wide
  # exact solution, in minutes and ng/mL (1.2 mg = 1.2e6 ng over V in L)
  D <- 1.2e3; V <- 0.16 * 70; CL <- 0.013 * 70 / 60; k <- CL / V
  ka <- 0.154 / 60; T0 <- 480
  R <- D * ka / (1 + ka * T0)                  # the depot's constant outflow, mcg/min
  t <- w$Time
  zero <- ifelse(t <= T0, R / CL * (1 - exp(-k * t)),
                 R / CL * (1 - exp(-k * T0)) * exp(-k * (t - T0)))
  A0 <- D / (1 + ka * T0)
  first <- ifelse(t > T0, A0 * ka / (V * (ka - k)) *
                    (exp(-k * (t - T0)) - exp(-ka * (t - T0))), 0)
  expect_equal(w$Plasma, zero + first, tolerance = 1e-6)
  # mass balance: AUC = dose / CL
  auc <- sum(diff(t) * (head(w$Plasma, -1) + tail(w$Plasma, -1)) / 2)
  expect_equal(auc, D / CL, tolerance = 0.02)
  # 1.2 mg peaks at 50 ng/mL (13 nmol/L) at 11.8 h; the paper's Figure 1:
  # about 11-13 nmol/L at about 12 h for 15-17.5 mcg/kg
  expect_equal(w$Time[which.max(w$Plasma)] / 60, 11.8, tolerance = 0.02)
  expect_equal_rounded(max(w$Plasma), 49.96025, tolerance = 1e-6)
})
