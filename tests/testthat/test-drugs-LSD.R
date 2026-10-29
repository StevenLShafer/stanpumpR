# LSD: see the header of R/drugs_LSD.R.  Dolder 2017's one-compartment oral
# model, 100 microgram arm, with apparent clearance and volume.  The pins are
# the published parameters; the reproduction tests are the paper's own
# published half-life (2.6 h) and peak time (1.4 h), which the parameters must
# reproduce.

noEvents <- data.frame(Time = numeric(0), Event = character(0))

test_that("returns Dolder's 100 microgram parameters with the fat-free-mass switch off", {
  actual <- LSD(70, 171, 50, "male", adjustToFFM = FALSE)

  expected <- list(
    PK = list(
      default = list(
        v1 = 46,              # V/F, L
        v2 = 1,
        v3 = 1,
        cl1 = 0.205,          # CL/F, L/min, from 12.3 L/h
        cl2 = 0,
        cl3 = 0,
        ka_PO = 0.023333333,  # 1.4 /h
        bioavailability_PO = 1,
        tlag_PO = 0
      )
    ),
    tPeak = 0, MEAC = 0,
    typical = 0, upperTypical = 0, lowerTypical = 0,
    reference = actual$reference
  )
  expect_equal_rounded(actual, expected)
})

test_that("the source fitted no lag time, and apparent parameters carry F = 1", {
  x <- LSD(70, 171, 50, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(x$tlag_PO, 0)
  # Apparent parameters already contain the bioavailability
  # (docs/adding-a-drug.md), so it must not be applied twice.
  expect_equal(x$bioavailability_PO, 1)
})

test_that("the parameters reproduce the paper's published half-life and peak time", {
  x <- LSD(70, 171, 50, "male", adjustToFFM = FALSE)$PK$default
  k <- x$cl1 / x$v1
  expect_equal(k * 60, 0.267, tolerance = 2e-3)            # 1/h, paper: 0.27
  expect_equal(log(2) / k / 60, 2.6, tolerance = 0.02)     # h, paper: 2.6
  tmax <- log(x$ka_PO / k) / (x$ka_PO - k)
  expect_equal(tmax / 60, 1.4, tolerance = 0.07)           # h, paper: 1.4
})

test_that("100 micrograms gives the magnitude the paper's model predicted", {
  w <- simulateDrugsWithCovariates(
    data.frame(Drug = "LSD", Time = 0, Dose = 100, Units = "mcg PO"),
    noEvents, 70, 171, 50, "male", 1440, TRUE)$LSD$wide
  # the analytic peak is at 1.46 h; the output grid reports the nearest point
  expect_equal(w$Time[which.max(w$Plasma)] / 60, 1.43, tolerance = 0.04)
  # The paper's compartmental model predicted a geometric mean peak of
  # 1.3 ng/mL; this typical-value curve peaks at 1.46 ng/mL, the expected
  # direction for a typical value against a geometric mean of individual fits.
  expect_gt(max(w$Plasma), 1.2)
  expect_lt(max(w$Plasma), 1.6)
  # Nothing before time zero, and no lag
  expect_gt(w$Plasma[which(w$Time > 0)[1]], 0)
})

test_that("exposure is dose-proportional, as a linear model requires", {
  one <- simulateDrugsWithCovariates(
    data.frame(Drug = "LSD", Time = 0, Dose = 100, Units = "mcg PO"),
    noEvents, 70, 171, 50, "male", 1440, TRUE)$LSD$wide
  two <- simulateDrugsWithCovariates(
    data.frame(Drug = "LSD", Time = 0, Dose = 200, Units = "mcg PO"),
    noEvents, 70, 171, 50, "male", 1440, TRUE)$LSD$wide
  expect_equal(two$Plasma, 2 * one$Plasma)
  # The 200 microgram arm was fitted separately (ka 1.2 /h, CL/F 9.9 L/h,
  # V/F 37 L) and is NOT implemented, so a 200 microgram dose here is the
  # 100 microgram model scaled.  The help page says so.
})

test_that("LSD is offered orally only, plasma only, with no band", {
  dd <- getDrugDefaultsGlobal(FALSE)
  row <- dd[dd$Drug == "LSD", ]
  units <- strsplit(row$Units, ",")[[1]]
  expect_setequal(doseRoute(units), ROUTE_PO)
  expect_setequal(units, "mcg PO")
  expect_true(is.na(row$Bolus.Units))
  expect_true(is.na(row$Infusion.Units))
  expect_equal(row$MEAC, 0)
  expect_equal(row$endCe, 0)
  expect_equal(c(row$Lower, row$Upper, row$Typical), c(0, 0, 0))
  expect_equal(row$Category, ILLICIT_DRUG_CATEGORY)
  expect_equal(getDrugPK("LSD", 70, 171, 50, "male")$PK$default$ke0, 0)
})
