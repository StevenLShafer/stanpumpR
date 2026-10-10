# paroxetine: see the header of R/drugs_paroxetine.R for the model (Kim 2015)
# and how its power of the daily dose on clearance is carried.  Pins were
# worked out from the published numbers and the fat-free-mass factors
# (volumes x 1.3049066461, clearances x 1.2209126185 for the 120 kg man), not
# from the code under test.

noEvents <- data.frame(Time = numeric(0), Event = character(0))

paroxetineDose <- function(time, dose, units = "mg PO") {
  data.frame(Drug = "paroxetine", Time = time, Dose = dose, Units = units)
}

oralParoxetine <- function(dt, maximum = 1440, age = 50, adjustToFFM = FALSE) {
  simulateDrugsWithCovariates(dt, noEvents, 70, 171, age, "male", maximum,
                              FALSE, adjustToFFM = adjustToFFM)$paroxetine$wide
}

# Kim's CL/F, L/h, at a daily dose D (mg) and age (y)
kimCL <- function(D, age) 13.1 * (D / 25)^-0.363 * (age / 71)^-0.702

# Mean concentration over the last day of a simulation, ng/mL (trapezoids)
lastDayMean <- function(w, maximum) {
  last <- w$Time >= maximum - 1440
  x <- w$Time[last]; y <- w$Plasma[last]
  sum(diff(x) * (utils::head(y, -1) + utils::tail(y, -1)) / 2) / 1440
}


test_that("returns Kim's parameters at the reference daily dose, switch off", {
  # 50 y: CL/F = 13.1 x (50/71)^-0.702 = 16.756284 L/h
  actual <- paroxetine(70, 171, 50, "male", adjustToFFM = FALSE)
  expected <- list(
    PK = list(default = list(
      v1 = 1020, v2 = 1, v3 = 1,
      cl1 = 0.2792714005, cl2 = 0, cl3 = 0,
      ka_PO = 0.908 / 60,
      bioavailability_PO = 1,
      tlag_PO = 0
    )),
    tPeak = 0,
    tPeakRoute = ROUTE_PO,
    MEAC = 0,
    typical = 40,
    upperTypical = 65,
    lowerTypical = 20,
    reference = actual$reference,
    oralSaturation = list(form = "power", exponent = 0.363, Dref = 25,
                          exampleDoses = c(10, 20, 25, 40, 60))
  )
  expect_equal_rounded(actual, expected)
  # At the reference age the published 13.1 L/h
  expect_equal(paroxetine(70, 171, 71, "male", adjustToFFM = FALSE)$PK$default$cl1,
               13.1 / 60)
})


test_that("scales to fat-free mass for a 120 kg man", {
  actual <- paroxetine(120, 170, 50, "male")
  expected <- list(
    v1 = 1331.00477902,       # 1020 x 1.3049066461
    v2 = 1, v3 = 1,
    cl1 = 0.3409659769,        # 0.2792714005 x 1.2209126185
    cl2 = 0, cl3 = 0
  )
  expect_equal_rounded(actual$PK$default[names(expected)], expected)
  off <- paroxetine(120, 170, 50, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(off$v1, 1020)
})


test_that("the oralSaturation block is valid and is Kim's dose power", {
  x <- paroxetine(70, 171, 50, "male")
  expect_identical(validateOralSaturation(x$oralSaturation, "paroxetine"),
                   x$oralSaturation)
  f <- oralSaturationFraction(c(10, 20, 25, 40, 50, 60), x$oralSaturation)
  expect_equal(f, (c(10, 20, 25, 40, 50, 60) / 25)^0.363)
  expect_equal(f[5], 1.2860975, tolerance = 1e-7)
})


test_that("once daily, the steady-state mean is Kim's D / CL(D)", {
  # Proof (header): D x (D/25)^0.363 / CL25 = D / CL(D).  Algebraically:
  for (D in c(10, 20, 40, 50))
    expect_equal(D * (D / 25)^0.363 / kimCL(25, 50), D / kimCL(D, 50))

  # And by simulation: 50 mg once daily for 30 days at 50 y (half-life 42 h),
  # mean over the last day against 50 / 24 / CL(50) in ng/mL = 159.90.
  maximum <- 30 * 1440
  w <- oralParoxetine(paroxetineDose(0, 50, "mg PO qd"), maximum)
  expected <- 50 / 24 / (13.1 * 2^-0.363 * (50 / 71)^-0.702) * 1000
  expect_equal(expected, 159.9024, tolerance = 1e-6)
  expect_equal(lastDayMean(w, maximum), expected, tolerance = 2e-3)

  # 20 mg daily averages 46 ng/mL, inside the AGNP band
  w20 <- oralParoxetine(paroxetineDose(0, 20, "mg PO qd"), maximum)
  expect_equal(lastDayMean(w20, maximum), 20 / 24 / kimCL(20, 50) * 1000,
               tolerance = 2e-3)
})


test_that("twice daily dosing reads each half-dose as the day's dose", {
  # 25 mg bid against 50 mg qd: daily exposure 2^-0.363 = 0.778 of Kim's.
  maximum <- 30 * 1440
  qd  <- oralParoxetine(paroxetineDose(0, 50, "mg PO qd"), maximum)
  bid <- oralParoxetine(paroxetineDose(0, 25, "mg PO bid"), maximum)
  expect_equal(lastDayMean(bid, maximum) / lastDayMean(qd, maximum),
               2^-0.363, tolerance = 2e-3)
})


test_that("half-life is the 25 mg/day value; peak at 4.8 h", {
  # At the reference age 71: ln2 x 1020 / 13.1 = 53.97 h; tmax ln(ka/k)/(ka-k)
  pk <- getDrugPK("paroxetine", 70, 171, 71, "male", adjustToFFM = FALSE)
  d  <- pk$PK$default
  k  <- d$cl1 / d$v1
  expect_equal(log(2) / k / 60, 53.97024, tolerance = 1e-6)
  expect_equal(log(d$ka_PO / k) / (d$ka_PO - k) / 60, 4.757194, tolerance = 1e-6)
  expect_equal(pk$PK$default$ke0, 0)            # plasma only
  # At 50 y, 42.19 h
  d50 <- paroxetine(70, 171, 50, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(log(2) * d50$v1 / d50$cl1 / 60, 42.19373, tolerance = 1e-6)
})


test_that("the simulation is the one-compartment oral solution with the dose power", {
  w <- oralParoxetine(rbind(paroxetineDose(0, 20), paroxetineDose(1440, 40)),
                      maximum = 2880)
  V <- 1020; CL <- kimCL(25, 50) / 60; k <- CL / V; ka <- 0.908 / 60
  oral <- function(t, t0, D) {
    s <- pmax(t - t0, 0)
    1000 * D * (D / 25)^0.363 / V * ka / (ka - k) * (exp(-k * s) - exp(-ka * s))
  }
  expect_equal(w$Plasma, oral(w$Time, 0, 20) + oral(w$Time, 1440, 40),
               tolerance = 1e-8)
})


test_that("young ages give large but finite clearance", {
  # Kim's age power is unbounded below; children are outside the data.
  for (age in c(0.01, 1, 5, 12)) {
    d <- paroxetine(10, 75, age, "male")$PK$default
    expect_true(is.finite(d$cl1) && d$cl1 > 0, info = age)
  }
  expect_equal((0.01 / 71)^-0.702, 505.0, tolerance = 1e-3)
})


test_that("paroxetine is offered orally only", {
  dd <- getDrugDefaultsGlobal(FALSE)
  units <- strsplit(dd$Units[dd$Drug == "paroxetine"], ",")[[1]]
  expect_true(all(doseRoute(units) == ROUTE_PO))
  expect_true(is.na(dd$Bolus.Units[dd$Drug == "paroxetine"]))
  expect_true(is.na(dd$Infusion.Units[dd$Drug == "paroxetine"]))
})


test_that("the CSV row agrees with the drug function", {
  dd  <- getDrugDefaultsGlobal(FALSE)
  row <- dd[dd$Drug == "paroxetine", ]
  x   <- paroxetine(70, 171, 50, "male")
  expect_equal(row$Lower, x$lowerTypical)
  expect_equal(row$Upper, x$upperTypical)
  expect_equal(row$Typical, x$typical)
  expect_equal(row$MEAC, 0)
  expect_equal(row$endCe, 0)
  expect_equal(row$Concentration.Units, "ng")
})
