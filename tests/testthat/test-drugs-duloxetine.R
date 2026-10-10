# duloxetine: see the header of R/drugs_duloxetine.R for the model (Zhong
# 2026), why it is oral only, and why there is no effect site.  Pins are the
# published values converted by hand (L/h / 60, /h / 60); the fat-free-mass
# factors for the 120 kg man are those worked out by hand for the other drug
# tests (volumes x 1.3049067, clearances x 1.2209126).  The engine checks are
# against the one-compartment oral solution written out below.

noEvents <- data.frame(Time = numeric(0), Event = character(0))

ZHONG_REFERENCE_PK <- list(default = list(
  v1 = 1530, v2 = 1, v3 = 1,
  cl1 = 1.0766666667,          # 64.6 L/h
  cl2 = 0, cl3 = 0,
  ka_PO = 0.0028,              # 0.168 /h
  bioavailability_PO = 1,
  tlag_PO = 0
))


test_that("the reference man receives Zhong's published values, either switch position", {
  for (adjust in c(TRUE, FALSE)) {
    actual <- duloxetine(70, 170, 35, "male", adjustToFFM = adjust)
    expected <- list(
      PK = ZHONG_REFERENCE_PK,
      tPeak = 0, MEAC = 0,
      typical = 60, upperTypical = 120, lowerTypical = 30,
      reference = actual$reference
    )
    expect_equal_rounded(actual, expected)
  }
  expect_match(duloxetine(70, 170, 35, "male")$reference, "10.13699/j.cnki.1001-6821.2026.10.009",
               fixed = TRUE)
})


test_that("women clear duloxetine 25% more slowly, as published", {
  # 64.6 x 0.75 = 48.45 L/h; the volume is unchanged.
  off <- duloxetine(70, 170, 35, "female", adjustToFFM = FALSE)$PK$default
  expect_equal_rounded(off$cl1, 0.8075)
  expect_equal(off$v1, 1530)
  # With the switch off, size changes nothing.
  expect_equal(duloxetine(120, 160, 80, "female", adjustToFFM = FALSE)$PK$default,
               off)
  expect_equal(duloxetine(120, 160, 80, "male", adjustToFFM = FALSE)$PK$default,
               ZHONG_REFERENCE_PK$default)
})


test_that("size scales to fat-free mass with the switch on", {
  # 120 kg, 170 cm, 50 y man
  on <- duloxetine(120, 170, 50, "male")$PK$default
  expect_equal_rounded(on[c("v1", "cl1", "ka_PO")],
                       list(v1 = 1996.507251, cl1 = 1.314515899, ka_PO = 0.0028))
  expect_equal(on[c("v2", "v3", "cl2", "cl3")],
               list(v2 = 1, v3 = 1, cl2 = 0, cl3 = 0))
})


test_that("duloxetine is offered by mouth only, in Antidepressants", {
  # Apparent parameters predict oral concentrations correctly and intravenous
  # ones wrong by 1/F.
  dd <- getDrugDefaultsGlobal(FALSE)
  row <- dd[dd$Drug == "duloxetine", ]
  units <- strsplit(row$Units, ",")[[1]]
  expect_equal(units, c("mg PO", "mg PO qd", "mg PO bid"))
  expect_true(all(doseRoute(units) == ROUTE_PO))
  expect_equal(row$Category, "Antidepressants")
  # The CSV band (AGNP 2018) and the model agree
  x <- duloxetine(70, 170, 35, "male")
  expect_equal(c(row$Lower, row$Upper, row$Typical, row$MEAC, row$endCe),
               c(x$lowerTypical, x$upperTypical, x$typical, 0, 0))
})


test_that("60 mg daily: the one-compartment oral solution, and the steady-state average D/tau/CL", {
  # 60 mg qd for 14 days in the reference man.  Superposition of
  # D/V ka/(ka - k) (e^-kt - e^-ka t), in ng/mL.
  maximum <- 14 * 1440
  dt <- data.frame(Drug = "duloxetine", Time = 0, Dose = 60, Units = "mg PO qd")
  w <- simulateDrugsWithCovariates(dt, noEvents, 70, 170, 35, "male", maximum,
                                   FALSE)$duloxetine$wide
  V <- 1530; CL <- 64.6 / 60; k <- CL / V; ka <- 0.168 / 60
  one <- function(t) ifelse(t > 0, 60 / V * 1000 * ka / (ka - k) *
                              (exp(-k * t) - exp(-ka * t)), 0)
  expected <- rowSums(sapply(seq(0, maximum - 1, by = 1440),
                             function(t0) one(w$Time - t0)))
  expect_equal(w$Plasma, expected, tolerance = 1e-6)
  expect_true(all(is.na(w$"Effect Site")))

  # Average over the last day: 60 mg / 1440 min / CL = 38.70 ng/mL.  The
  # analytic integral of the superposed curve over that day, not a
  # trapezoid on the plot grid.
  F1 <- function(t) ifelse(t > 0, 60 / V * 1000 * ka / (ka - k) *
                             ((1 - exp(-k * t)) / k - (1 - exp(-ka * t)) / ka), 0)
  starts <- seq(0, maximum - 1, by = 1440)
  auc <- sum(F1(maximum - starts) - F1(maximum - 1440 - starts))
  expect_equal(auc / 1440, 38.6997, tolerance = 2e-3)   # near steady state
  expect_equal(60 * 1000 / 1440 / CL, 38.69969, tolerance = 1e-6)
  # The plotted curve straddles it on the last day
  last <- w$Time >= maximum - 1440
  expect_lt(min(w$Plasma[last]), 38.7)
  expect_gt(max(w$Plasma[last]), 38.7)
})


test_that("half-life is 16.4 h in a man and 21.9 h in a woman", {
  m <- duloxetine(70, 170, 35, "male", adjustToFFM = FALSE)$PK$default
  f <- duloxetine(70, 170, 35, "female", adjustToFFM = FALSE)$PK$default
  expect_equal(log(2) * m$v1 / m$cl1 / 60, 16.41664, tolerance = 1e-6)
  expect_equal(log(2) * f$v1 / f$cl1 / 60, 21.88886, tolerance = 1e-6)
})
