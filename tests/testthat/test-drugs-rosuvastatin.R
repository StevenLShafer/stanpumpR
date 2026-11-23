# rosuvastatin: see R/drugs_rosuvastatin.R (Courlet 2021, Table 2).
noEvents <- data.frame(Time = numeric(0), Event = character(0))

test_that("Table 2 with the switch off: the published apparent parameters", {
  actual <- rosuvastatin(70, 170, 35, "male", adjustToFFM = FALSE)
  expected <- list(
    PK = list(default = list(
      v1 = 144, v2 = 1610, v3 = 1,
      cl1 = 122 / 60, cl2 = 69 / 60, cl3 = 0,
      ka_PO = 0.306 / 60, bioavailability_PO = 1, tlag_PO = 0
    )),
    tPeak = 0, MEAC = 0,
    typical = 0, upperTypical = 0, lowerTypical = 0,
    zeroOrderPO = list(duration = 0.461 * 60),
    reference = paste0(
      "Courlet 2021 (65 adults living with HIV taking rosuvastatin 5-20 mg daily, ",
      "Swiss HIV Cohort Study): two compartments, apparent oral parameters, ",
      "CL/F 122 L/h, Vc/F 144 L, Vp/F 1610 L, Q/F 69 L/h; release into the gut over ",
      "0.461 h, then ka 0.306/h; no covariates; dose as labelled (rosuvastatin); ",
      "Asian ancestry, SLCO1B1 and ABCG2 genotypes and severe renal impairment not ",
      "represented; concentration only. ",
      "https://doi.org/10.1007/s40262-020-00946-3"
    )
  )
  expect_equal_rounded(actual, expected)
  # The switch off ignores size: no covariate, fixed published values
  expect_equal(rosuvastatin(120, 190, 70, "female", adjustToFFM = FALSE)$PK,
               actual$PK)
})

test_that("scales to fat-free mass for a 120 kg man (reference anchor)", {
  # 120 kg, 170 cm, 50 y male: volumes x 1.3049067, clearances x 1.2209126
  # (docs/adding-a-drug.md); the cohort anchor is not applied (see the header)
  actual <- rosuvastatin(120, 170, 50, "male")
  expected <- list(
    v1 = 144 * 1.3049067, v2 = 1610 * 1.3049067,
    cl1 = 122 / 60 * 1.2209126, cl2 = 69 / 60 * 1.2209126
  )
  expect_equal_rounded(actual$PK$default[names(expected)], expected)
  # absorption does not scale
  expect_equal(actual$PK$default$ka_PO, 0.306 / 60)
  expect_equal(actual$zeroOrderPO$duration, 0.461 * 60)
})

test_that("the zero-order release then first-order absorption matches a numerical solution", {
  PK <- getDrugPK("rosuvastatin", 70, 170, 35, "male", adjustToFFM = FALSE)
  expect_true(PK$zeroOrderPO)
  dose <- 20
  w <- simCpCe(data.frame(Drug = "rosuvastatin", Time = 0, Dose = dose, Units = "mg PO"),
               noEvents, PK, 2880, FALSE)$wide
  # Courlet Table 2, written out independently of the model function
  k10 <- 122 / 144 / 60; k12 <- 69 / 144 / 60; k21 <- 69 / 1610 / 60
  ka <- 0.306 / 60; D1 <- 0.461 * 60
  f <- function(t, A) {
    input <- if (t < D1) dose / D1 else 0
    c(input - ka * A[1], ka * A[1] - (k10 + k12) * A[2] + k21 * A[3],
      k12 * A[2] - k21 * A[3])
  }
  # RK4, with a step boundary on the corner of the input
  h <- 0.0461
  tt <- seq(0, 2880 + h, by = h)
  A <- c(0, 0, 0)
  cp <- numeric(length(tt))
  for (j in seq_along(tt)) {
    cp[j] <- A[2] / 144 * 1000
    t <- tt[j]
    k1 <- f(t, A); k2 <- f(t + h / 2, A + h / 2 * k1)
    k3 <- f(t + h / 2, A + h / 2 * k2); k4 <- f(t + h, A + h * k3)
    A <- A + h / 6 * (k1 + 2 * k2 + 2 * k3 + k4)
  }
  ref <- approx(tt, cp, w$Time)$y
  expect_lt(max(abs(w$Plasma - ref)) / max(ref), 1e-3)
  # The peak the header records: 1.7 h, 20.7 ng/mL after 20 mg (earlier than
  # the label's 3 to 5 h; see THE PEAK DISAGREES WITH THE LABEL)
  expect_equal(w$Time[which.max(w$Plasma)] / 60, 1.7, tolerance = 0.03)
  expect_equal(max(w$Plasma), 20.7, tolerance = 0.01)
  expect_true(all(is.na(w$`Effect Site`)))
})

test_that("20 mg daily: the steady-state AUC is Courlet's simulated median, 164 ng.h/mL", {
  PK <- getDrugPK("rosuvastatin", 70, 170, 35, "male", adjustToFFM = FALSE)
  w <- simCpCe(data.frame(Drug = "rosuvastatin", Time = 0, Dose = 20, Units = "mg PO qd"),
               noEvents, PK, 20 * MINS_PER_DAY, FALSE)$wide
  last <- w$Time >= 19 * MINS_PER_DAY
  t <- w$Time[last]; cp <- w$Plasma[last]
  auc <- sum(diff(t) * (head(cp, -1) + tail(cp, -1)) / 2) / 60
  expect_equal(auc, 20000 / 122, tolerance = 0.005)
  expect_equal(auc, 164, tolerance = 0.005)
})

test_that("terminal half-life 25.6 h from Table 2", {
  k10 <- 122 / 144; k12 <- 69 / 144; k21 <- 69 / 1610
  a <- k10 + k12 + k21
  beta <- (a - sqrt(a^2 - 4 * k10 * k21)) / 2
  expect_equal(log(2) / beta, 25.6, tolerance = 0.002)
})

test_that("units: oral only, apparent parameters", {
  expect_equal(getDrugDefaults("rosuvastatin")$Units[[1]], c("mg PO", "mg PO qd"))
  expect_equal(getDrugDefaults("rosuvastatin")$Category, "Other")
  expect_equal(rosuvastatin(70, 170, 35, "male")$PK$default$bioavailability_PO, 1)
})
