# zolpidem: see the header of R/drugs_zolpidem.R for the model (Kim 2026),
# how the transit absorption is represented, and why there is no effect site.
# Pins were worked out from the published numbers by hand (Python, with the
# fat-free-mass formula written out again), not from the code under test.

noEvents <- data.frame(Time = numeric(0), Event = character(0))

oralZolpidem <- function(dose, maximum = 720, weight = 70, height = 170,
                         age = 35, sex = "male", adjustToFFM = TRUE) {
  dt <- data.frame(Drug = "zolpidem", Time = 0, Dose = dose, Units = "mg PO")
  simulateDrugsWithCovariates(dt, noEvents, weight, height, age, sex, maximum,
                              FALSE, adjustToFFM = adjustToFFM)$zolpidem$wide
}


test_that("the reference patient receives Kim's published values", {
  for (adjust in c(TRUE, FALSE)) {
    actual <- zolpidem(70, 170, 35, "male", adjustToFFM = adjust)
    expected <- list(
      PK = list(default = list(
        v1 = 64, v2 = 1, v3 = 1,
        cl1 = 18 / 60, cl2 = 0, cl3 = 0,
        ka_PO = 11.7 / 60,
        bioavailability_PO = 1,
        tlag_PO = 15
      )),
      tPeak = 0,
      tPeakRoute = ROUTE_PO,
      MEAC = 0,
      typical = 120,
      upperTypical = 200,
      lowerTypical = 80,
      reference = actual$reference
    )
    expect_equal_rounded(actual, expected)
  }
})


test_that("size scales to fat-free mass with the switch on, not at all with it off", {
  # 120 kg, 170 cm, 50 y man: volumes x 1.3049066, clearances x 1.2209126
  on <- zolpidem(120, 170, 50, "male")$PK$default
  expect_equal_rounded(on[c("v1", "cl1")],
                       list(v1 = 83.5140253523, cl1 = 0.3662737855))
  off <- zolpidem(120, 170, 50, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(off$v1, 64)
  expect_equal(off$cl1, 0.3)
})


test_that("the lag reproduces Kim's transit absorption after the first 20 minutes", {
  # Kim's typical subject, 10 mg: a gamma-distributed input (shape NN + 1 =
  # 20.4, rate (NN + 1) / MTT = 81.6 /h; Savic 2007) into a depot emptied at
  # ka, convolved here by numerical integration, against the engine's lag of
  # MTT followed by the same ka.
  CL <- 18; V <- 64; ka <- 11.7; NN <- 19.4; MTT <- 0.25; k <- CL / V
  ktr <- (NN + 1) / MTT
  transit <- function(tH) {
    if (tH <= 0) return(0)
    f <- function(s) stats::dgamma(s, shape = NN + 1, rate = ktr) *
      ka / (ka - k) * (exp(-k * (tH - s)) - exp(-ka * (tH - s)))
    10 / V * 1000 * stats::integrate(f, 0, tH, rel.tol = 1e-10)$value
  }
  w <- oralZolpidem(10)
  late <- w$Time >= 45
  ref <- vapply(w$Time[late] / 60, transit, numeric(1))
  expect_lt(max(abs(w$Plasma[late] - ref)), 0.2)
  # and the peak height agrees within 1%
  tFine <- seq(0.3, 1, by = 0.005)
  expect_equal(max(w$Plasma), max(vapply(tFine, transit, numeric(1))),
               tolerance = 0.01)
})


test_that("the simulation is the one-compartment oral solution, plasma only", {
  # 10 mg at 0 and 10 mg at 24 h, against the solution written out with the
  # 15 min lag.  No effect site: the column is NA.
  dt <- data.frame(Drug = "zolpidem", Time = c(0, 1440), Dose = c(10, 10),
                   Units = "mg PO")
  w <- simulateDrugsWithCovariates(dt, noEvents, 70, 170, 35, "male", 2880,
                                   FALSE)$zolpidem$wide
  k <- 0.3 / 64; ka <- 11.7 / 60
  one <- function(t) {
    s <- pmax(t - 15, 0)
    10 / 64 * 1000 * ka / (ka - k) * (exp(-k * s) - exp(-ka * s))
  }
  expected <- one(w$Time) + ifelse(w$Time >= 1440, one(w$Time - 1440), 0)
  expect_equal(w$Plasma, expected, tolerance = 1e-8)
  expect_true(all(is.na(w$"Effect Site")))
})


test_that("10 mg in the reference man matches the published peak and half-life", {
  # Analytic peak of the lag model: 142.54 ng/mL at 34.6 min.  Label 121
  # (58-272) ng/mL; Greenblatt 2006 about 140.  Half-life 2.46 h (label 2.5).
  k <- 0.3 / 64; ka <- 11.7 / 60
  tmax <- 15 + log(ka / k) / (ka - k)
  cmax <- 10 / 64 * 1000 * ka / (ka - k) *
    (exp(-k * (tmax - 15)) - exp(-ka * (tmax - 15)))
  expect_equal(tmax, 34.62, tolerance = 1e-3)
  expect_equal(cmax, 142.54, tolerance = 1e-3)
  expect_equal(log(2) / k / 60, 2.4645, tolerance = 1e-3)
  expect_lte(max(oralZolpidem(10)$Plasma), cmax)
})


test_that("zolpidem is offered by mouth only, in Hypnotics and sedatives", {
  dd <- getDrugDefaultsGlobal(FALSE)
  units <- strsplit(dd$Units[dd$Drug == "zolpidem"], ",")[[1]]
  expect_equal(units, c("mg PO", "mg PO qd"))
  expect_true(all(doseRoute(units) == ROUTE_PO))
  expect_true(is.na(dd$Bolus.Units[dd$Drug == "zolpidem"]))
  expect_equal(dd$Category[dd$Drug == "zolpidem"], "Hypnotics and sedatives")
})


test_that("the CSV row agrees with the drug function, threshold at 50 ng/mL", {
  dd  <- getDrugDefaultsGlobal(FALSE)
  row <- dd[dd$Drug == "zolpidem", ]
  x   <- zolpidem(70, 170, 35, "male")
  expect_equal(row$Lower, x$lowerTypical)
  expect_equal(row$Upper, x$upperTypical)
  expect_equal(row$Typical, x$typical)
  expect_equal(row$MEAC, 0)
  # FDA 2013: driving impaired above about 50 ng/mL; read against plasma
  expect_equal(row$endCe, 50)
  expect_equal(row$Concentration.Units, "ng")
})


test_that("time until the 50 ng/mL driving threshold is blank during the lag", {
  dd <- getDrugDefaultsGlobal()
  PK <- getDrugPK("zolpidem", 70, 170, 35, "male", dd[dd$Drug == "zolpidem", ])
  PK$endCe <- 50
  DT <- data.frame(Drug = "zolpidem", Time = 0, Dose = 10, Units = "mg PO")
  w <- simCpCe(DT, noEvents, PK, 720, TRUE)$wide
  expect_true(all(is.na(w$Recovery[w$Time < 15])))
  expect_false(anyNA(w$Recovery[w$Time >= 15]))
  # Plasma falls below 50 ng/mL at t where 160.10 exp(-k (t - 15)) = 50 once
  # absorption is complete: 263.5 min after the dose.
  k <- 0.3 / 64; ka <- 11.7 / 60
  tBelow <- 15 + log(10 / 64 * 1000 * ka / (ka - k) / 50) / k
  at <- which(w$Time >= 120)[1]
  expect_equal(w$Time[at] + w$Recovery[at], tBelow, tolerance = 0.01)
})
