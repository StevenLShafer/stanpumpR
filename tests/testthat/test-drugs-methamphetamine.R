# methamphetamine (d-): see the header of R/drugs_methamphetamine.R.  The
# disposition is Harris 2003's clearance and steady-state volume read as one
# compartment; the intranasal absorption rate constant is derived from his
# observed 2.7 h peak.  Pins were worked out from the published numbers by
# hand, not from the code under test.

noEvents <- data.frame(Time = numeric(0), Event = character(0))

test_that("returns Harris's per-kilogram parameters with the fat-free-mass switch off", {
  actual <- methamphetamine(70, 171, 50, "male", adjustToFFM = FALSE)

  expected <- list(
    PK = list(
      default = list(
        v1 = 294,             # 4.2 L/kg x 70 kg
        v2 = 1,
        v3 = 1,
        cl1 = 0.31733333,     # 0.272 L/h/kg x 70 kg / 60
        cl2 = 0,
        cl3 = 0,
        ka_IN = 0.018677950,  # 1.120677 /h, derived from tmax = 2.7 h
        bioavailability_IN = 0.79,
        tlag_IN = 0
      )
    ),
    tPeak = 0, MEAC = 0,
    typical = 0, upperTypical = 0, lowerTypical = 0,
    reference = actual$reference
  )
  expect_equal_rounded(actual, expected)
})

test_that("the per-kilogram values are reproduced at any weight with the switch off", {
  for (wt in c(50, 70, 120)) {
    x <- methamphetamine(wt, 170, 40, "female", adjustToFFM = FALSE)$PK$default
    expect_equal(x$v1 / wt, 4.2)                  # L/kg
    expect_equal(x$cl1 * 60 / wt, 0.272)          # L/h/kg
  }
})

test_that("Harris's three summaries are mutually consistent with one compartment", {
  x <- methamphetamine(70, 171, 50, "male", adjustToFFM = FALSE)$PK$default
  k <- x$cl1 / x$v1
  # Half-life 10.70 h, against the 10.7 h intranasal and 11.4 h intravenous
  # half-lives Harris reported.
  expect_equal(log(2) / k / 60, 10.703, tolerance = 1e-3)
  # Mean residence time V/CL = 16.2 h, against his reported 16 h
  expect_equal(x$v1 / x$cl1 / 60, 16.2, tolerance = 0.05)
  expect_equal(x$v2, 1); expect_equal(x$v3, 1)
  expect_equal(x$cl2, 0); expect_equal(x$cl3, 0)
})

test_that("the derived ka puts the intranasal peak at Harris's observed 2.7 hours", {
  x <- methamphetamine(70, 171, 50, "male", adjustToFFM = FALSE)$PK$default
  k <- x$cl1 / x$v1
  ka <- x$ka_IN
  tmax <- log(ka / k) / (ka - k)            # min, one compartment, first order
  expect_equal(tmax / 60, 2.7, tolerance = 1e-4)
  expect_equal(x$bioavailability_IN, 0.79)
})

test_that("a 50 mg intranasal dose peaks at 2.7 hours in the simulation", {
  w <- simulateDrugsWithCovariates(
    data.frame(Drug = "methamphetamine", Time = 0, Dose = 50, Units = "mg IN"),
    noEvents, 70, 171, 50, "male", 2880, TRUE)$methamphetamine$wide
  expect_equal(w$Time[which.max(w$Plasma)] / 60, 2.7, tolerance = 0.02)
  expect_equal(max(w$Plasma), 112.2, tolerance = 0.01)
})

test_that("methamphetamine is offered intranasally only, plasma only, with no band", {
  dd <- getDrugDefaultsGlobal(FALSE)
  row <- dd[dd$Drug == "methamphetamine", ]
  units <- strsplit(row$Units, ",")[[1]]
  expect_setequal(doseRoute(units), ROUTE_IN)
  expect_true(is.na(row$Bolus.Units))
  expect_true(is.na(row$Infusion.Units))
  expect_equal(row$MEAC, 0)
  expect_equal(row$endCe, 0)
  expect_equal(c(row$Lower, row$Upper, row$Typical), c(0, 0, 0))
  expect_equal(row$Category, ILLICIT_DRUG_CATEGORY)
  expect_equal(getDrugPK("methamphetamine", 70, 171, 50, "male")$PK$default$ke0, 0)
})

test_that("it is a different drug from the oral prescription amphetamine product", {
  dd <- getDrugDefaultsGlobal(FALSE)
  expect_true(all(c("methamphetamine", "mixedAmphetamineSalts") %in% dd$Drug))
  expect_equal(dd$Category[dd$Drug == "mixedAmphetamineSalts"], "Stimulants")
  expect_false(isIllicitDrug("mixedAmphetamineSalts", dd))
  expect_true(isIllicitDrug("methamphetamine", dd))
})
