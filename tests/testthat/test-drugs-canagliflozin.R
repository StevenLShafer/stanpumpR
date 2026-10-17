# canagliflozin: see R/drugs_canagliflozin.R (Hoeben 2016, Table 5; the
# reference values from the supplement's Table 2).
noEvents <- data.frame(Time = numeric(0), Event = character(0))
# creatinine giving an MDRD eGFR of 87 (the reference) at 45 years
scr87 <- function(sex) uniroot(function(s) egfrMDRD(45, sex, s) - 87, c(0.3, 3),
                               tol = 1e-12)$root
wRef <- 75.8                       # kg, the phase I median weight
hRef <- 100 * sqrt(wRef / 26.1)    # cm: the median BMI, 26.1, at that weight

test_that("Table 5 at the reference values: CL/F 14.4 L/h in men, 12.0 in women", {
  m <- canagliflozin(wRef, hRef, 45, "male", FALSE, creatinine = scr87("male"))
  f <- canagliflozin(wRef, hRef, 45, "female", FALSE, creatinine = scr87("female"))
  pm <- m$PK$default
  expect_equal(pm$cl1 * 60, 0.145 * 99.3, tolerance = 1e-8)
  expect_equal(f$PK$default$v1, 82.6, tolerance = 1e-12)
  expect_equal(round(pm$cl1 * 60, 1), 14.4)
  expect_equal(round(f$PK$default$cl1 * 60, 1), 12.0)
  expect_equal(pm$cl2 / pm$v1 * 60, 0.101)
  expect_equal(pm$cl2 / pm$v2 * 60, 0.0856)
  expect_equal(pm$ka_PO * 60, 3.68)
  expect_equal(pm$tlag_PO, 0.147 * 60)
  expect_equal(m$zeroOrderPO$duration, 0.604 * 60)
})

test_that("covariates: weight and age on Vc/F, eGFR on ke, BMI on ka and lag", {
  p <- canagliflozin(90, 160, 70, "female", FALSE, creatinine = 1.4)$PK$default
  vc <- 82.6 * (90 / 75.8)^0.583 * (70 / 45)^-0.167
  egfr <- egfrMDRD(70, "female", 1.4)
  expect_equal(p$v1, vc)
  expect_equal(p$cl1 * 60, 0.145 * (egfr / 87)^0.261 * vc)
  b <- 90 / 1.6^2 / 26.1
  expect_equal(p$ka_PO * 60, 3.68 * b^-1.41)
  expect_equal(p$tlag_PO, 0.147 * b^1.41 * 60)
  # a higher BMI absorbs later and more slowly (Table 2)
  lean <- canagliflozin(55, 175, 40, "male", FALSE)$PK$default
  expect_gt(lean$ka_PO, p$ka_PO)
  expect_lt(lean$tlag_PO, p$tlag_PO)
  # switch on: the pharmacokinetic weight on Vc/F; BMI stays the patient's own
  size <- pkSizeFactors(120, 175, 50, "male", TRUE)
  on <- canagliflozin(120, 175, 50, "male", creatinine = 1)$PK$default
  expect_equal(on$v1, 99.3 * (size$pkWeight / 75.8)^0.583 * (50 / 45)^-0.167)
  expect_equal(on$ka_PO,
               canagliflozin(120, 175, 50, "male", FALSE, creatinine = 1)$PK$default$ka_PO)
  # age held at 18
  expect_equal(canagliflozin(40, 150, 12, "male", FALSE, creatinine = 0.6)$PK$default$v1,
               99.3 * (40 / 75.8)^0.583 * (18 / 45)^-0.167)
  # BMI held to the phase I range, 18.0 to 44.5
  expect_equal(canagliflozin(160, 170, 50, "male", FALSE)$PK$default$ka_PO * 60,
               3.68 * (44.5 / 26.1)^-1.41)
  expect_equal(canagliflozin(45, 170, 50, "male", FALSE)$PK$default$tlag_PO,
               0.147 * (18.0 / 26.1)^1.41 * 60)
  expect_equal_rounded(canagliflozin(70, 170, 40, "male")$PK$default$cl1 * 60, 14.2831238)
})

test_that("the lag, then a zero-order release into the gut, matches a numerical solution", {
  PK <- getDrugPK("canagliflozin", wRef, hRef, 45, "male", adjustToFFM = FALSE,
                  creatinine = scr87("male"))
  expect_true(PK$zeroOrderPO)
  w <- simCpCe(data.frame(Drug = "canagliflozin", Time = 0, Dose = 100, Units = "mg PO"),
               noEvents, PK, 1440, FALSE)$wide
  s <- PK$PK$default
  k10 <- s$cl1 / s$v1; k12 <- s$cl2 / s$v1; k21 <- s$cl2 / s$v2
  D1 <- 0.604 * 60
  f <- function(t, A) {
    input <- if (t >= s$tlag_PO && t < s$tlag_PO + D1) 100 / D1 else 0
    c(input - s$ka_PO * A[1], s$ka_PO * A[1] - (k10 + k12) * A[2] + k21 * A[3],
      k12 * A[2] - k21 * A[3])
  }
  # RK4, with the step boundaries on the corners of the input
  h <- 0.05
  tt <- seq(0, 1440, by = h)
  A <- c(0, 0, 0)
  cp <- numeric(length(tt))
  for (j in seq_along(tt)) {
    cp[j] <- A[2] / s$v1 * 1000
    t <- tt[j]
    k1 <- f(t, A); k2 <- f(t + h / 2, A + h / 2 * k1)
    k3 <- f(t + h / 2, A + h / 2 * k2); k4 <- f(t + h, A + h * k3)
    A <- A + h / 6 * (k1 + 2 * k2 + 2 * k3 + k4)
  }
  ref <- approx(tt, cp, w$Time)$y
  expect_lt(max(abs(w$Plasma - ref)) / max(ref), 1e-3)
  # peak 1-2 h after the dose (paper)
  peak <- w$Time[which.max(w$Plasma)] / 60
  expect_gt(peak, 1)
  expect_lt(peak, 2)
  expect_true(all(is.na(w$`Effect Site`)))
})

test_that("300 mg daily: the steady-state AUC is dose / CL/F", {
  PK <- getDrugPK("canagliflozin", 90, 170, 60, "male", adjustToFFM = FALSE, creatinine = 1)
  w <- simCpCe(data.frame(Drug = "canagliflozin", Time = 0, Dose = 300, Units = "mg PO qd"),
               noEvents, PK, 10 * MINS_PER_DAY, FALSE)$wide
  last <- w$Time >= 9 * MINS_PER_DAY
  t <- w$Time[last]; cp <- w$Plasma[last]
  auc <- sum(diff(t) * (head(cp, -1) + tail(cp, -1)) / 2) / 60
  expect_equal(auc, 300000 / (PK$PK$default$cl1 * 60), tolerance = 0.005)
})

test_that("units: oral, daily; the release is refused where it cannot run", {
  expect_equal(getDrugDefaults("canagliflozin")$Units[[1]], c("mg PO", "mg PO qd"))
  expect_true("creatinine" %in% names(formals(canagliflozin)))
  X <- list(PK = list(default = list(ka_PO = 0.05, ka_PO2 = 0.01)))
  expect_error(validateZeroOrderPO(list(duration = 30), X, "x"), "second oral depot")
  X <- list(PK = list(default = list(ka_PO = 0.05, ka_SC = 0.01)))
  expect_error(validateZeroOrderPO(list(duration = 30), X, "x"), "tissue route")
  X <- list(PK = list(default = list(ka_PO = 0)))
  expect_error(validateZeroOrderPO(list(duration = 30), X, "x"), "absorbed orally")
  X <- list(PK = list(default = list(ka_PO = 0.05)))
  expect_error(validateZeroOrderPO(list(duration = 0), X, "x"), "positive duration")
})
