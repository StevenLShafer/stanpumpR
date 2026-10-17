# cocaine: see the header of R/drugs_cocaine.R for what the model is (Chow
# 1985's disposition reduced to one compartment, Jeffcoat 1989's intranasal
# absorption) and what it leaves out.  Pins were worked out from the published
# numbers and the fat-free-mass factors by hand, not from the code under test.

noEvents <- data.frame(Time = numeric(0), Event = character(0))

test_that("returns the published parameters with the fat-free-mass switch off", {
  actual <- cocaine(70, 171, 50, "male", adjustToFFM = FALSE)

  expected <- list(
    PK = list(
      default = list(
        v1 = 131.8,          # Chow's Vss, used as a one-compartment volume
        v2 = 1,
        v3 = 1,
        cl1 = 2.10,          # Chow's elimination clearance, L/min
        cl2 = 0,
        cl3 = 0,
        ka_IN = 0.059243351, # log(2) / 11.7, Jeffcoat's absorption half-time
        bioavailability_IN = 0.80,
        tlag_IN = 0
      )
    ),
    tPeak = 0, MEAC = 0,
    typical = 0, upperTypical = 0, lowerTypical = 0,
    reference = actual$reference
  )
  expect_equal_rounded(actual, expected)
})

test_that("the disposition reproduces Chow's half-life, between Javaid's and his own", {
  x <- cocaine(70, 171, 50, "male", adjustToFFM = FALSE)$PK$default
  k <- x$cl1 / x$v1
  expect_equal(k, 0.0159332, tolerance = 1e-5)         # 1/min
  halfLife <- log(2) / k
  expect_equal(halfLife, 43.50, tolerance = 1e-3)      # min
  # Javaid's independent one-compartment intravenous fit was 41.4 min and
  # Chow's own terminal half-life 48 min; the reduction sits between them.
  expect_gt(halfLife, 41.4)
  expect_lt(halfLife, 48)
  # One compartment: nothing is distributed
  expect_equal(x$v2, 1); expect_equal(x$v3, 1)
  expect_equal(x$cl2, 0); expect_equal(x$cl3, 0)
})

test_that("Jeffcoat's intranasal absorption is carried as published", {
  x <- cocaine(70, 171, 50, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(log(2) / x$ka_IN, 11.7, tolerance = 1e-6)   # absorption half-time, min
  expect_equal(x$bioavailability_IN, 0.80)
  expect_equal(x$tlag_IN, 0)
})

test_that("scales to fat-free mass for a 120 kg man", {
  # 120 kg, 170 cm, 50 y male: pkSizeFactors() gives a fat-free-mass ratio
  # whose volume factor is applied to v1 and whose 0.75 power is applied to
  # cl1.  ka is not scaled.
  size <- pkSizeFactors(120, 170, 50, "male", TRUE)
  x <- cocaine(120, 170, 50, "male")$PK$default
  expect_equal(x$v1, 131.8 * size$volume)
  expect_equal(x$cl1, 2.10 * size$clearance)
  expect_equal(x$ka_IN, log(2) / 11.7)
})

test_that("cocaine is offered intranasally only, plasma only, with no band", {
  dd <- getDrugDefaultsGlobal(FALSE)
  row <- dd[dd$Drug == "cocaine", ]
  units <- strsplit(row$Units, ",")[[1]]
  expect_setequal(doseRoute(units), ROUTE_IN)
  expect_true(is.na(row$Bolus.Units))
  expect_true(is.na(row$Infusion.Units))
  expect_equal(row$MEAC, 0)
  expect_equal(row$endCe, 0)
  expect_equal(c(row$Lower, row$Upper, row$Typical), c(0, 0, 0))
  expect_equal(row$Category, ILLICIT_DRUG_CATEGORY)
  # Plasma only: no effect site in the model
  expect_equal(getDrugPK("cocaine", 70, 171, 50, "male")$PK$default$ke0, 0)
})

test_that("a 100 mg intranasal dose peaks where the model's own constants put it", {
  w <- simulateDrugsWithCovariates(
    data.frame(Drug = "cocaine", Time = 0, Dose = 100, Units = "mg IN"),
    noEvents, 70, 171, 50, "male", 480, TRUE)$cocaine$wide
  # One compartment, first-order input: tmax = ln(ka/k)/(ka-k) = 30.3 min,
  # and Cmax = F*D/V * (k/ka)^(k/(ka-k)) = 372 ng/mL.
  expect_equal(w$Time[which.max(w$Plasma)], 30.3, tolerance = 0.05)
  expect_equal(max(w$Plasma), 372.4, tolerance = 0.01)
  # Absorption, not distribution, governs the rise: the peak is later than
  # the 11.7 min absorption half-time and earlier than one half-life.
  expect_gt(w$Time[which.max(w$Plasma)], 11.7)
  expect_lt(w$Time[which.max(w$Plasma)], 43.5)
})

test_that("the intravenous, oral and smoked routes are registered but not offered", {
  registry <- utils::read.csv(
    system.file("extdata", "illicitRouteRegistry.csv", package = "stanpumpR"),
    stringsAsFactors = FALSE)
  rows <- registry[registry$drug == "cocaine", ]
  expect_setequal(rows$route, c("IN", "IV", "PO", "smoked"))
  expect_equal(rows$offered[rows$route == "IN"], "yes")
  expect_true(all(rows$offered[rows$route != "IN"] == "no"))
})
