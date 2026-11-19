# Altitude and the inhaled anesthetics (R/altitude.R; Shafer, 2026-11-18)

gasTable <- function(drug, dose)
  data.frame(Drug = drug, Time = 0, Dose = dose, stringsAsFactors = FALSE)

lastState <- function(sim, gas, state = 2)
  sim$state[[gas]][length(sim$timeLine), state]

test_that("West 1996's model atmosphere sets the barometric pressure", {
  expect_equal(barometricPressure(0), exp(6.63268))
  # 7000 m: 324 Torr (the ICAO standard atmosphere's 308 reads too low)
  expect_equal(round(barometricPressure(7000), 1), 324.2)
  expect_equal(round(barometricPressure(3000), 1), exp(6.63268 - 0.3336 - 0.01341) |> round(1))
  expect_identical(pressureRatio(0), 1)
  expect_equal(pressureRatio(7000), barometricPressure(7000) / barometricPressure(0))
  expect_identical(gasPressureRatio(getGasBody(70)), 1)
  expect_error(checkAltitude(-1), "Invalid altitude")
  expect_error(checkAltitude(7001), "Invalid altitude")
  expect_error(checkAltitude(c(0, 1)), "Invalid altitude")
  expect_identical(checkAltitude(NULL), ALTITUDE_DEFAULT)
  expect_identical(altitudeFromInput(NULL), ALTITUDE_DEFAULT)
  expect_identical(altitudeFromInput("abc"), ALTITUDE_DEFAULT)
  expect_identical(altitudeFromInput(9000), ALTITUDE_MAX)
  expect_identical(altitudeFromInput(2500), 2500)
})

test_that("sea level is exactly what it was", {
  dt <- gasTable(c("oxygen", "nitrousOxide", "sevoflurane", "ventilation"), c(1, 2, 2, 5))
  a <- simulateGases(dt, maximum = 60)
  b <- simulateGases(dt, maximum = 60, altitude = 0)
  expect_identical(a$state, b$state)
  expect_null(b$body$altitude)
})

test_that("in partial pressures, the kinetics do not depend on the pressure", {
  # With the volume terms off (no uptake coupling, no oxygen volume), a
  # variable-bypass vaporizer's partial pressure gives the same curve at any
  # altitude, but for the humidification of the inspired gas: the whole
  # patient side is w times the sea-level curve
  dt <- gasTable(c("oxygen", "sevoflurane", "ventilation"), c(2, 2, 5))
  sea <- simulateGases(dt, maximum = 60, uptakeEffect = FALSE, oxygenUptake = FALSE)
  high <- simulateGases(dt, maximum = 60, uptakeEffect = FALSE, oxygenUptake = FALSE,
                        altitude = 7000)
  w <- gasHumidityRatio(high$body)
  expect_equal(high$state$sevoflurane[, -1], w * sea$state$sevoflurane[, -1],
               tolerance = 1e-12)
  expect_equal(high$body$altitude, 7000)
})

test_that("water vapour takes its 47 Torr of the inspired gas at any altitude", {
  expect_identical(gasHumidityRatio(getGasBody(70)), 1)
  expect_identical(gasHumidityRatio(list(altitude = 0)), 1)
  P <- barometricPressure(7000); P0 <- barometricPressure(0)
  expect_equal(gasHumidityRatio(list(altitude = 7000)),
               ((P - 47) / P) / ((P0 - 47) / P0))
  expect_equal(round(gasHumidityRatio(list(altitude = 7000)), 3), 0.911)
  # A dry inspired partial pressure of nitrous oxide reaches the alveoli
  # humidified: without the uptake coupling the system is linear and starts
  # empty, so every tension is r x w times its sea-level value
  dt <- gasTable(c("oxygen", "nitrousOxide", "ventilation"), c(3, 7, 5))
  run <- function(h) simulateGases(dt, maximum = 120, uptakeEffect = FALSE,
                                   oxygenUptake = FALSE, altitude = h)
  s <- run(5000)
  expect_equal(s$state$nitrousOxide[, -1],
               pressureRatio(5000) * gasHumidityRatio(s$body) * run(0)$state$nitrousOxide[, -1],
               tolerance = 1e-10)
})

test_that("flowmeter gases and the desflurane vaporizer fall with the pressure", {
  r <- pressureRatio(5000)
  # No rebreathing (fresh gas above the minute ventilation) and no coupling:
  # the alveolar tension tends to the inspired partial pressure
  open <- function(dt, h) simulateGases(dt, maximum = 600, uptakeEffect = FALSE,
                                        oxygenUptake = FALSE, altitude = h)
  n2o <- gasTable(c("oxygen", "nitrousOxide", "ventilation"), c(3, 7, 5))
  expect_equal(lastState(open(n2o, 5000), "nitrousOxide", 1),
               r * lastState(open(n2o, 0), "nitrousOxide", 1), tolerance = 1e-9)
  expect_equal(lastState(open(n2o, 0), "nitrousOxide", 1), 70, tolerance = 1e-9)
  des <- gasTable(c("oxygen", "desflurane", "ventilation"), c(10, 6, 5))
  expect_equal(lastState(open(des, 5000), "desflurane", 1), 6 * r, tolerance = 1e-9)
  sevo <- gasTable(c("oxygen", "sevoflurane", "ventilation"), c(10, 2, 5))
  expect_equal(lastState(open(sevo, 5000), "sevoflurane", 1), 2, tolerance = 1e-9)
  # The vapour displaces carrier: oxygen is the rest of the ambient pressure
  expect_equal(lastState(open(sevo, 5000), "oxygen", 1),
               100 * r - 2, tolerance = 1e-9)
})

test_that("the patient starts in room air at the ambient pressure", {
  dt <- gasTable(c("air", "ventilation"), c(5, 5))
  r <- pressureRatio(7000)
  s <- simulateGases(dt, maximum = 30, altitude = 7000)
  w <- gasHumidityRatio(s$body)
  expect_equal(s$state$nitrogen[1, ], rep(AIR_FRACTION_N2 * 100 * r * w, gasStateCount(s$body)))
  expect_equal(s$state$oxygen[1, ], rep(AIR_FRACTION_O2 * 100 * r * w, 2))
  # The alveolar oxygen at the same ventilation: the inspired partial
  # pressure falls, the oxygen consumed takes the same partial pressure
  sea <- simulateGases(dt, maximum = 30)
  dropSea <- AIR_FRACTION_O2 * 100 - lastState(sea, "oxygen")
  dropHigh <- AIR_FRACTION_O2 * 100 * r * w - lastState(s, "oxygen")
  expect_lt(lastState(s, "oxygen"), lastState(sea, "oxygen") * 0.25)
  expect_equal(dropHigh, dropSea, tolerance = 0.25)
})

test_that("the uptake is an ambient volume, 1/r of it at sea level", {
  dt <- gasTable(c("oxygen", "nitrousOxide", "ventilation"), c(3, 7, 5))
  body <- getGasBody(70)
  high <- body; high$altitude <- 6000
  state <- list(nitrousOxide = c(0, 50, 10, 5, 1), nitrogen = c(0, 5, 20, 30, 40),
                oxygen = c(30, 30))
  props <- getGasProperties()
  expect_equal(gasTotalUptake(state, props, high, 5),
               gasTotalUptake(state, props, body, 5) / pressureRatio(6000))
})

test_that("the washout and the time until threshold run at altitude", {
  dt <- data.frame(Drug = c("oxygen", "sevoflurane", "ventilation", "sevoflurane"),
                   Time = c(0, 0, 0, 30), Dose = c(2, 2, 5, 0))
  for (h in c(0, 7000)) {
    sim <- simulateGases(dt, maximum = 90, altitude = h)
    w <- gasWashout(sim, dt[, c("Time", "Drug", "Dose")])
    expect_false(is.null(w))
    entries <- gasDrugEntries(sim, dt, getDrugDefaultsGlobal(), 90, washout = w,
                              age = 50, macThreshold = 0.5)
    expect_true(length(entries) > 0)
  }
})

test_that("the altitude slider is in the physiology panel and resets", {
  html <- as.character(physiologyPanel())
  expect_match(html, 'id="physiology_altitude"', fixed = TRUE)
  expect_match(html, sprintf('data-max="%d"', ALTITUDE_MAX), fixed = TRUE)
  rows <- physiologyCovariateRows(NULL, NULL, 7000)
  expect_equal(rows$Value[grepl("Altitude", rows$Covariate)], "7000 (324 Torr)")
  shiny::testServer(app_server, {
    session$setInputs(physiology_altitude = 4000)
    session$elapse(PHYSIOLOGY_DEBOUNCE_MS + 100)
    expect_equal(altitude(), 4000)
    session$setInputs(physiology_altitude = "junk")
    session$elapse(PHYSIOLOGY_DEBOUNCE_MS + 100)
    expect_equal(altitude(), ALTITUDE_DEFAULT)
  })
})
