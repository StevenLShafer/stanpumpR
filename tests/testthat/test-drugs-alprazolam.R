# alprazolam: see the header of R/drugs_alprazolam.R for the model (DeVane
# 1993, read from the full paper) and the effect site (Venkatakrishnan 2005).
# Pins were worked out from the published numbers by hand (Python, with the
# fat-free-mass formula written out again), not from the code under test.

noEvents <- data.frame(Time = numeric(0), Event = character(0))


test_that("the reference patient receives DeVane's typical values", {
  # 70 kg man under 60: CL/F 0.05 x 70 = 3.5 L/h, V/F 0.7 x 70 = 49 L.  His
  # pharmacokinetic weight is 70 kg, so the switch changes nothing.
  for (adjust in c(TRUE, FALSE)) {
    actual <- alprazolam(70, 170, 35, "male", adjustToFFM = adjust)
    expected <- list(
      PK = list(default = list(
        v1 = 49, v2 = 1, v3 = 1,
        cl1 = 3.5 / 60, cl2 = 0, cl3 = 0,
        ka_PO = 1.1 / 60,
        bioavailability_PO = 1,
        tlag_PO = 0
      )),
      tPeak = 0,
      ke0 = log(2) / 4.8,
      MEAC = 0,
      typical = 30,
      upperTypical = 40,
      lowerTypical = 20,
      reference = actual$reference
    )
    expect_equal_rounded(actual, expected)
  }
})


test_that("DeVane's age term applies and his sex term does not", {
  # 60 kg, 160 cm, 40 y woman, switch off: V 42 L, CL 0.05 x 60 L/h, with no
  # +59% for sex (dropped by decision; see the header)
  off <- alprazolam(60, 160, 40, "female", adjustToFFM = FALSE)$PK$default
  expect_equal_rounded(off[c("v1", "cl1")], list(v1 = 42, cl1 = 0.05))
  expect_equal(off, alprazolam(60, 160, 40, "male", adjustToFFM = FALSE)$PK$default)
  # Switch on: the pharmacokinetic weight is 49.999 kg
  on <- alprazolam(60, 160, 40, "female")$PK$default
  expect_equal_rounded(on[c("v1", "cl1")],
                       list(v1 = 34.9993756050, cl1 = 0.0416659233))
  # Only the sex term is dropped: with the switch on, a woman's fat-free mass
  # is lower than a man's of the same weight, height and age, and so is her
  # clearance (header and help: 2.88 against 3.50 L/h; pharmacokinetic weight
  # 57.682 kg, Al-Sallami written out again in Python)
  f <- alprazolam(70, 170, 35, "female")$PK$default$cl1 * 60
  m <- alprazolam(70, 170, 35, "male")$PK$default$cl1 * 60
  expect_equal(c(f, m), c(2.8841074, 3.5), tolerance = 1e-7)
  # Over 60: clearance x 0.77; at exactly 60, no change ("older than 60")
  old <- alprazolam(70, 170, 70, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(old$cl1, 0.0449166667, tolerance = 1e-9)
  sixty <- alprazolam(70, 170, 60, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(sixty$cl1, 3.5 / 60)
})


test_that("DeVane's weight terms run on the pharmacokinetic weight with the switch on", {
  # 120 kg, 170 cm, 50 y man: pharmacokinetic weight 91.343 kg
  on <- alprazolam(120, 170, 50, "male")$PK$default
  expect_equal_rounded(on[c("v1", "cl1")],
                       list(v1 = 63.9404256604, cl1 = 0.0761195544))
  off <- alprazolam(120, 170, 50, "male", adjustToFFM = FALSE)$PK$default
  expect_equal_rounded(off[c("v1", "cl1")], list(v1 = 84, cl1 = 0.1))
})


test_that("the simulation is the one-compartment oral solution with its effect site", {
  dt <- data.frame(Drug = "alprazolam", Time = c(0, 480, 960),
                   Dose = c(1, 0.5, 0.5), Units = "mg PO")
  w <- simulateDrugsWithCovariates(dt, noEvents, 70, 170, 35, "male", 1440,
                                   FALSE)$alprazolam$wide
  k <- 3.5 / 60 / 49; ka <- 1.1 / 60; ke0 <- log(2) / 4.8
  cp <- function(t, D) {
    s <- pmax(t, 0)
    D / 49 * 1000 * ka / (ka - k) * (exp(-k * s) - exp(-ka * s))
  }
  ce <- function(t, D) {
    s <- pmax(t, 0)
    A <- D / 49 * 1000 * ka / (ka - k)
    A * ke0 * (exp(-k * s) / (ke0 - k) - exp(-ka * s) / (ke0 - ka) -
                 (1 / (ke0 - k) - 1 / (ke0 - ka)) * exp(-ke0 * s))
  }
  doses <- function(f) f(w$Time, 1) + f(w$Time - 480, 0.5) + f(w$Time - 960, 0.5)
  expect_equal(w$Plasma, doses(cp), tolerance = 1e-8)
  expect_equal(w$"Effect Site", doses(ce), tolerance = 1e-8)
})


test_that("1 mg by mouth, and 1 mg/day at steady state", {
  # Analytic peak 16.88 ng/mL at 2.66 h (observed 12-22 ng/mL at 0.7-1.8 h:
  # the height agrees, the time is late; see the header).  1 mg/day averages
  # 1000 / (3.5 x 24) = 11.9 ng/mL (observed 10-12 per mg/day).
  k <- 3.5 / 49; ka <- 1.1
  tmax <- log(ka / k) / (ka - k)
  cmax <- 1 / 49 * 1000 * ka / (ka - k) * (exp(-k * tmax) - exp(-ka * tmax))
  expect_equal(tmax, 2.658, tolerance = 1e-3)
  expect_equal(cmax, 16.88, tolerance = 1e-3)
  expect_equal(1000 / (3.5 * 24), 11.905, tolerance = 1e-4)
  pk <- getDrugPK("alprazolam", 70, 170, 35, "male")
  expect_equal(pk$PK$default$ke0, log(2) / 4.8)
})


test_that("alprazolam is offered by mouth only, in Hypnotics and sedatives", {
  dd <- getDrugDefaultsGlobal(FALSE)
  units <- strsplit(dd$Units[dd$Drug == "alprazolam"], ",")[[1]]
  expect_equal(units, c("mg PO", paste("mg PO", names(SCHEDULE_INTERVALS))))
  expect_true(all(doseRoute(units) == ROUTE_PO))
  expect_true(is.na(dd$Bolus.Units[dd$Drug == "alprazolam"]))
  expect_equal(dd$Category[dd$Drug == "alprazolam"], "Hypnotics and sedatives")
})


test_that("the CSV row agrees with the drug function", {
  dd  <- getDrugDefaultsGlobal(FALSE)
  row <- dd[dd$Drug == "alprazolam", ]
  x   <- alprazolam(70, 170, 35, "male")
  expect_equal(row$Lower, x$lowerTypical)
  expect_equal(row$Upper, x$upperTypical)
  expect_equal(row$Typical, x$typical)
  expect_equal(row$MEAC, 0)
  expect_equal(row$endCe, 0)
  expect_equal(row$Concentration.Units, "ng")
  # The help page says there is no default threshold, not "0 ng/mL"
  expect_match(helpDrugPageHTML("alprazolam"),
               "None by default; a threshold set under Drug Thresholds is timed on the effect site",
               fixed = TRUE)
})
