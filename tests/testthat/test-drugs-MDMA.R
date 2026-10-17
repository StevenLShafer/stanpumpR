# MDMA: see the header of R/drugs_MDMA.R.  Huestis 2025's one-compartment oral
# model, whose published input is a zero-order filling of the depot over D1
# after a lag ALAG1, then first-order ka.  The engine has first-order depots
# only, so the zero-order window is discretised as two half doses at its
# quarter points; the test that matters most here measures that discretisation
# against a numerical evaluation of the exact solution.

noEvents <- data.frame(Time = numeric(0), Event = character(0))

# Huestis 2025, Table 2, as published (hours and L/h)
CL_F <- 41.5; V_F <- 462; KA <- 1.17; ALAG1 <- 0.322; D1 <- 0.304

test_that("returns the published parameters with the fat-free-mass switch off", {
  actual <- MDMA(70, 171, 50, "male", adjustToFFM = FALSE)

  expected <- list(
    PK = list(
      default = list(
        v1 = 462,              # V2/F, L
        v2 = 1,
        v3 = 1,
        cl1 = 0.69166667,      # CL/F, L/min, from 41.5 L/h
        cl2 = 0,
        cl3 = 0,
        ka_PO = 0.0195,        # 1.17 /h
        bioavailability_PO = 1,
        tlag_PO = 23.88,       # (ALAG1 + D1/4) in minutes
        ka_PO2 = 0.0195,
        fraction_PO2 = 0.5,
        tlag_PO2 = 33.00       # (ALAG1 + 3*D1/4) in minutes
      )
    ),
    tPeak = 0, MEAC = 0,
    typical = 0, upperTypical = 0, lowerTypical = 0,
    reference = actual$reference
  )
  expect_equal_rounded(actual, expected)
})

test_that("the two depots are the source's zero-order window, halved at its quarter points", {
  x <- MDMA(70, 171, 50, "male", adjustToFFM = FALSE)$PK$default
  # Same absorption rate in both: this is one input discretised, not two paths
  expect_equal(x$ka_PO, x$ka_PO2)
  expect_equal(x$fraction_PO2, 0.5)
  # The two lags straddle the midpoint of the window, a quarter-width apart
  expect_equal(mean(c(x$tlag_PO, x$tlag_PO2)), (ALAG1 + D1 / 2) * 60)
  expect_equal(x$tlag_PO2 - x$tlag_PO, D1 / 2 * 60)
  # ... and the first one starts after the published lag, not at it
  expect_gt(x$tlag_PO, ALAG1 * 60)
})

test_that("the discretised input matches the exact zero-order-then-first-order solution", {
  # The exact solution: a unit oral dose filling the depot at a constant rate
  # over D1 after ALAG1, the depot emptying first order with ka into a
  # one-compartment central volume.  Written here from the source's structure,
  # independently of the code under test, and integrated numerically.
  k <- CL_F / V_F
  impulse <- function(t)                       # one-compartment oral impulse
    ifelse(t <= 0, 0, KA / (KA - k) * (exp(-k * t) - exp(-KA * t)) / V_F)
  exact <- function(t) vapply(t, function(tt)
    stats::integrate(function(s) impulse(tt - ALAG1 - s) / D1,
                     0, D1, rel.tol = 1e-10)$value, numeric(1))

  x <- MDMA(70, 171, 50, "male", adjustToFFM = FALSE)$PK$default
  discretised <- function(t)                   # what the model actually does
    0.5 * impulse(t - x$tlag_PO / 60) + 0.5 * impulse(t - x$tlag_PO2 / 60)

  hours <- seq(0, 24, by = 1 / 120)
  e <- exact(hours)
  d <- discretised(hours)

  # The peak, which is what a reader looks at, to better than a tenth of a
  # percent; and the whole curve after the input window to better than 1%.
  expect_equal(max(d), max(e), tolerance = 1e-3)
  after <- hours > ALAG1 + D1 + 0.05
  expect_lt(max(abs(d[after] - e[after]) / e[after]), 0.01)
  # Both peak at the same time to within the grid
  expect_equal(hours[which.max(d)], hours[which.max(e)], tolerance = 0.02)
})

test_that("the parameters reproduce the published half-life and exposure", {
  x <- MDMA(70, 171, 50, "male", adjustToFFM = FALSE)$PK$default
  k <- x$cl1 / x$v1
  expect_equal(k * 60, CL_F / V_F)                      # 1/h
  expect_equal(log(2) / k / 60, 7.716, tolerance = 1e-3)   # h
  # Area under the curve is dose over apparent clearance, exactly, for any
  # linear model: 120 mg MDMA HCl gives 2892 ng.h/mL at the reference weight.
  w <- simulateDrugsWithCovariates(
    data.frame(Drug = "MDMA", Time = 0, Dose = 120, Units = "mg PO"),
    noEvents, 70, 171, 50, "male", 4320, TRUE)$MDMA$wide
  auc <- sum(diff(w$Time) * (head(w$Plasma, -1) + tail(w$Plasma, -1)) / 2) / 60
  expect_equal(auc, 120 / CL_F * 1000, tolerance = 0.01)   # ng.h/mL
  # The source's own observed 120 mg arm gave 3388 (fasted) and 3318 (fed)
  # ng.h/mL, in the study the model gives 36% LOWER apparent clearance, which
  # is not carried here; so this reference-individual curve sits below them.
  expect_lt(auc, 3388)
  expect_gt(auc, 0.7 * 3388)
})

test_that("a 120 mg dose peaks where the model's own constants put it", {
  w <- simulateDrugsWithCovariates(
    data.frame(Drug = "MDMA", Time = 0, Dose = 120, Units = "mg PO"),
    noEvents, 70, 171, 50, "male", 1440, TRUE)$MDMA$wide
  # The sum of the two lagged half doses peaks at 2.86 h: later than the
  # 2.77 h the first half alone would give, which is the zero-order window
  # showing up in the peak time.  The output grid reports its nearest point.
  expect_equal(w$Time[which.max(w$Plasma)] / 60, 2.86, tolerance = 0.03)
  # Between the published fasted and fed medians of 2.1 and 4.0 h: the
  # parameters are the light-breakfast reference, and food is not carried.
  expect_gt(w$Time[which.max(w$Plasma)] / 60, 2.1)
  expect_lt(w$Time[which.max(w$Plasma)] / 60, 4.0)
  # Nothing reaches plasma before the published lag
  expect_true(all(w$Plasma[w$Time < ALAG1 * 60] == 0))
})

test_that("the source's own weight covariates are carried, not the package's", {
  for (wt in c(50, 70, 120)) {
    x <- MDMA(wt, 170, 40, "female", adjustToFFM = FALSE)$PK$default
    expect_equal(x$v1, 462 * (wt / 70)^0.885)
    expect_equal(x$cl1 * 60, 41.5 * (wt / 70)^1.03)
  }
  # With the switch on, the same covariates are evaluated at the fat-free-mass
  # weight instead of total body weight.
  size <- pkSizeFactors(120, 170, 50, "male", TRUE)
  x <- MDMA(120, 170, 50, "male")$PK$default
  expect_equal(x$v1, 462 * (size$pkWeight / 70)^0.885)
  expect_equal(x$cl1 * 60, 41.5 * (size$pkWeight / 70)^1.03)
})

test_that("exposure is dose-proportional, which the real drug is not", {
  one <- simulateDrugsWithCovariates(
    data.frame(Drug = "MDMA", Time = 0, Dose = 60, Units = "mg PO"),
    noEvents, 70, 171, 50, "male", 1440, TRUE)$MDMA$wide
  two <- simulateDrugsWithCovariates(
    data.frame(Drug = "MDMA", Time = 0, Dose = 120, Units = "mg PO"),
    noEvents, 70, 171, 50, "male", 1440, TRUE)$MDMA$wide
  expect_equal(two$Plasma, 2 * one$Plasma)
  # This linearity is the model's, not the drug's: MDMA inhibits its own
  # CYP2D6 metabolism, so a real second dose accumulates more than this
  # predicts.  The header and the help page say so.
})

test_that("MDMA is offered orally only, plasma only, with no band", {
  dd <- getDrugDefaultsGlobal(FALSE)
  row <- dd[dd$Drug == "MDMA", ]
  units <- strsplit(row$Units, ",")[[1]]
  expect_setequal(doseRoute(units), ROUTE_PO)
  expect_setequal(units, "mg PO")
  expect_true(is.na(row$Bolus.Units))
  expect_true(is.na(row$Infusion.Units))
  expect_equal(row$MEAC, 0)
  expect_equal(row$endCe, 0)
  expect_equal(c(row$Lower, row$Upper, row$Typical), c(0, 0, 0))
  expect_equal(row$Category, ILLICIT_DRUG_CATEGORY)
  expect_equal(getDrugPK("MDMA", 70, 171, 50, "male")$PK$default$ke0, 0)
})
