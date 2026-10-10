# mirtazapine: see the header of R/drugs_mirtazapine.R for the model (Yan
# 2026), why it is oral only, why the BMI step is kept alongside the
# fat-free-mass scaling, and why the within-interval profile is unreliable.
# Pins are the published Table 2 values converted by hand (L/h / 60,
# /h / 60); the fat-free-mass factors for the 120 kg man are those worked out
# by hand for the other drug tests (volumes x 1.3049067, clearances x
# 1.2209126).
#
# SOURCE DISCREPANCY: Yan's abstract gives CL/F 29.3 L/h and V/F 348 L; Table
# 2 gives 28.9 and 310, which are used and pinned here.

noEvents <- data.frame(Time = numeric(0), Event = character(0))

YAN_REFERENCE_PK <- list(default = list(
  v1 = 310, v2 = 1, v3 = 1,
  cl1 = 0.4816666667,          # 28.9 L/h
  cl2 = 0, cl3 = 0,
  ka_PO = 0.02,                # 1.2 /h
  bioavailability_PO = 1,
  tlag_PO = 0
))


test_that("the reference man receives Yan's Table 2 values, either switch position", {
  # 70 kg, 170 cm: BMI 24.2, below the threshold
  for (adjust in c(TRUE, FALSE)) {
    actual <- mirtazapine(70, 170, 35, "male", adjustToFFM = adjust)
    expected <- list(
      PK = YAN_REFERENCE_PK,
      tPeak = 0, MEAC = 0,
      typical = 50, upperTypical = 80, lowerTypical = 30,
      reference = actual$reference,
      sourceDiscrepancy = actual$sourceDiscrepancy
    )
    expect_equal_rounded(actual, expected)
  }
})


test_that("the abstract/Table 2 discrepancy is recorded, and Table 2 is used", {
  x <- mirtazapine(70, 170, 35, "male", adjustToFFM = FALSE)
  expect_match(x$sourceDiscrepancy, "28.9", fixed = TRUE)
  expect_match(x$sourceDiscrepancy, "29.3", fixed = TRUE)
  expect_match(x$sourceDiscrepancy, "310", fixed = TRUE)
  expect_match(x$sourceDiscrepancy, "348", fixed = TRUE)
  expect_false(isTRUE(all.equal(x$PK$default$cl1, 29.3 / 60)))
  expect_false(isTRUE(all.equal(x$PK$default$v1, 348)))
  # getDrugPK() ignores the extra field
  pk <- getDrugPK("mirtazapine", 70, 170, 35, "male")
  expect_null(pk$sourceDiscrepancy)
})


test_that("BMI of 28 or more lowers clearance by 29.4%, a step at the threshold", {
  # 200 cm, so that the BMI is exact: 111.6 kg is 27.9, 112 kg is 28.0.
  below <- mirtazapine(111.6, 200, 35, "male", adjustToFFM = FALSE)$PK$default
  at    <- mirtazapine(112,   200, 35, "male", adjustToFFM = FALSE)$PK$default
  expect_equal_rounded(below$cl1, 0.4816666667)            # 28.9 L/h
  expect_equal_rounded(at$cl1,    0.3400566667)            # 28.9 x 0.706
  # The volume carries no BMI term
  expect_equal(below$v1, 310)
  expect_equal(at$v1, 310)
})


test_that("size scales to fat-free mass with the switch on, and the BMI step still applies", {
  # 120 kg, 170 cm, 50 y man: BMI 41.5, so the flag applies.
  # CL = 28.9 x 0.706 / 60 x 1.2209126; V = 310 x 1.3049067.
  on <- mirtazapine(120, 170, 50, "male")$PK$default
  expect_equal_rounded(on[c("v1", "cl1", "ka_PO")],
                       list(v1 = 404.521077, cl1 = 0.4151794690, ka_PO = 0.02))
  expect_equal(on[c("v2", "v3", "cl2", "cl3")],
               list(v2 = 1, v3 = 1, cl2 = 0, cl3 = 0))
  # Switch off: Yan's model exactly (V 310, CL 20.4034 L/h)
  off <- mirtazapine(120, 170, 50, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(off$v1, 310)
  expect_equal_rounded(off$cl1, 0.3400566667)
})


test_that("mirtazapine is offered by mouth only, in Antidepressants", {
  dd <- getDrugDefaultsGlobal(FALSE)
  row <- dd[dd$Drug == "mirtazapine", ]
  units <- strsplit(row$Units, ",")[[1]]
  expect_equal(units, c("mg PO", "mg PO qd", "mg PO bid"))
  expect_true(all(doseRoute(units) == ROUTE_PO))
  expect_equal(row$Category, "Antidepressants")
  x <- mirtazapine(70, 170, 35, "male")
  expect_equal(c(row$Lower, row$Upper, row$Typical, row$MEAC, row$endCe),
               c(x$lowerTypical, x$upperTypical, x$typical, 0, 0))
})


test_that("30 mg daily: the one-compartment oral solution, and the steady-state average D/tau/CL", {
  # The steady-state average is what Yan's trough data identify:
  # 30 mg / 1440 min / (28.9 / 60) L/min = 43.25 ng/mL.
  maximum <- 7 * 1440
  dt <- data.frame(Drug = "mirtazapine", Time = 0, Dose = 30, Units = "mg PO qd")
  w <- simulateDrugsWithCovariates(dt, noEvents, 70, 170, 35, "male", maximum,
                                   FALSE)$mirtazapine$wide
  V <- 310; CL <- 28.9 / 60; k <- CL / V; ka <- 1.2 / 60
  one <- function(t) ifelse(t > 0, 30 / V * 1000 * ka / (ka - k) *
                              (exp(-k * t) - exp(-ka * t)), 0)
  starts <- seq(0, maximum - 1, by = 1440)
  expected <- rowSums(sapply(starts, function(t0) one(w$Time - t0)))
  expect_equal(w$Plasma, expected, tolerance = 1e-6)
  expect_true(all(is.na(w$"Effect Site")))

  F1 <- function(t) ifelse(t > 0, 30 / V * 1000 * ka / (ka - k) *
                             ((1 - exp(-k * t)) / k - (1 - exp(-ka * t)) / ka), 0)
  auc <- sum(F1(maximum - starts) - F1(maximum - 1440 - starts))
  expect_equal(auc / 1440, 43.25260, tolerance = 1e-4)
  expect_equal(30 * 1000 / 1440 / CL, 43.25260, tolerance = 1e-6)

  # An obese patient (BMI >= 28, switch off): 30 / 1440 / (20.4034 / 60)
  w2 <- simulateDrugsWithCovariates(dt, noEvents, 120, 170, 50, "male", maximum,
                                    FALSE, adjustToFFM = FALSE)$mirtazapine$wide
  CL2 <- 28.9 * 0.706 / 60
  expected2 <- rowSums(sapply(starts, function(t0) {
    t <- w2$Time - t0
    ifelse(t > 0, 30 / V * 1000 * ka / (ka - CL2 / V) *
             (exp(-CL2 / V * t) - exp(-ka * t)), 0)
  }))
  expect_equal(w2$Plasma, expected2, tolerance = 1e-6)
  expect_equal(30 * 1000 / 1440 / CL2, 61.26430, tolerance = 1e-6)
})


test_that("half-life is 7.4 h, far below the label's 20 to 40 h (a trough-data artefact)", {
  x <- mirtazapine(70, 170, 35, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(log(2) * x$v1 / x$cl1 / 60, 7.435143, tolerance = 1e-6)
})
