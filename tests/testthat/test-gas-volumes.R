# Three volumes, three conditions (Steven L. Shafer, 2026-11-28: "Convert
# both, assuming 20 degrees C at the flowmeter").  The gas engine counts gas
# in the lungs; the fresh gas is read at the flowmeters dry at 20 C, and the
# oxygen consumed is an STPD volume (R/gasProperties.R: gasFreshGasExpansion(),
# gasStpdFactor(), gasOxygenUptake()).

volumeRows <- function(...) {
  rows <- list(...)
  data.frame(Time = vapply(rows, function(r) r[[1]], 0),
             Drug = vapply(rows, function(r) r[[2]], ""),
             Dose = vapply(rows, function(r) r[[3]], 0), stringsAsFactors = FALSE)
}

test_that("the two factors are the gas laws'", {
  body <- getGasBody(70)
  # STPD to 37 C at the engine's 713 mmHg per 100%
  expect_equal(gasStpdFactor(), (760 / 713) * 310.15 / 273.15)
  expect_equal(gasStpdFactor(37), 1.2103, tolerance = 1e-4)
  # Its counterpart in mmHg and mL is the textbooks' 0.863
  expect_equal(760 * 310.15 / 273.15 / 1000, 0.863, tolerance = 1e-3)
  # A flowmeter's litre, dry at 20 C: warmed 5.8%, humidified 6.6%
  expect_equal(GAS_FLOWMETER_TEMPERATURE, 20)
  expect_equal(gasFreshGasExpansion(body), (760 / 713) * 310.15 / 293.15)
  expect_equal(gasFreshGasExpansion(body), 1.1277, tolerance = 1e-4)
  expect_equal(gasOxygenUptake(body), 0.245 * gasStpdFactor())
  # In a cold patient the gas warms less; at altitude the same water vapour
  # is a larger share of the pressure
  cold <- body; cold$temperature <- 32
  expect_equal(gasFreshGasExpansion(cold) / gasFreshGasExpansion(body), 305.15 / 310.15)
  expect_equal(gasOxygenUptake(cold) / gasOxygenUptake(body), 305.15 / 310.15)
  high <- body; high$altitude <- 5000
  P <- barometricPressure(5000); P0 <- barometricPressure(0)
  expect_equal(gasFreshGasExpansion(high) / gasFreshGasExpansion(body),
               (P / (P - 47)) / (P0 / (P0 - 47)))
  expect_gt(gasFreshGasExpansion(high), gasFreshGasExpansion(body))
  # Litre for litre, as GasMan takes them
  asStated <- body; asStated$volumesAsStated <- TRUE
  expect_equal(gasFreshGasExpansion(asStated), 1)
  expect_equal(gasOxygenUptake(asStated), 0.245)
})

test_that("rebreathing stops when the flowmeters read the ventilation over the expansion", {
  e <- gasFreshGasExpansion(getGasBody(70))
  run <- function(Q) advanceClosedFormGas(
    volumeRows(list(0, "oxygen", Q), list(0, "ventilation", 4), list(0, "sevoflurane", 2)),
    weight = 70, maximum = 30, uptakeEffect = FALSE, oxygenUptake = FALSE)
  inspired <- function(sim) sim$state$sevoflurane[-1, 1]
  # 4 / 1.1277 = 3.547 L/min on the flowmeters fills a ventilation of 4
  at <- run(4 / e + 1e-9)
  expect_equal(inspired(at), rep(2, length(inspired(at))))
  below <- run(0.97 * 4 / e)
  expect_true(all(inspired(below) < 2))
  # Litre for litre the same flow is rebreathed, and 4 L/min is the threshold
  old <- advanceClosedFormGas(
    volumeRows(list(0, "oxygen", 4 / e), list(0, "ventilation", 4), list(0, "sevoflurane", 2)),
    weight = 70, maximum = 30, uptakeEffect = FALSE, oxygenUptake = FALSE,
    volumeConversion = FALSE)
  expect_true(all(inspired(old) < 2))
  expect_true(isTRUE(old$body$volumesAsStated))
  expect_null(at$body$volumesAsStated)
  # GasMan's own circuit is always litre for litre
  semi <- advanceClosedFormGas(
    volumeRows(list(0, "oxygen", 4), list(0, "ventilation", 4), list(0, "sevoflurane", 2)),
    weight = 70, maximum = 5, circuit = "semi-closed")
  expect_true(isTRUE(semi$body$volumesAsStated))
})

test_that("on room air the alveolar oxygen is the alveolar gas equation's", {
  # 70 kg, alveolar ventilation 4 L/min, air well above the ventilation so
  # nothing is rebreathed.  PACO2 = 0.863 VCO2 / VA; PAO2 = PIO2 - PACO2
  # (FIO2 + (1 - FIO2) / R) (Fenn, Rahn and Otis 1946).
  MV <- 4 / (1 - GAS_DEAD_SPACE_FRACTION)
  sim <- simulateGases(volumeRows(list(0, "air", 12), list(0, "ventilation", MV)),
                       weight = 70, age = 40, maximum = 60)
  PAO2 <- utils::tail(sim$state$oxygen[, 2], 1) / 100 * GAS_ALVEOLAR_TORR
  VCO2 <- GAS_RESPIRATORY_QUOTIENT * 245
  PACO2 <- 0.863 * VCO2 / 4
  expect_equal(PACO2, 42.3, tolerance = 1e-3)
  PIO2 <- AIR_FRACTION_O2 * GAS_ALVEOLAR_TORR
  expected <- PIO2 - PACO2 * (AIR_FRACTION_O2 + (1 - AIR_FRACTION_O2) / GAS_RESPIRATORY_QUOTIENT)
  expect_equal(expected, 98.6, tolerance = 2e-3)
  # (air's 1% of argon is not carried, which the equation does not know)
  expect_equal(PAO2, expected, tolerance = 5e-3)
  # Litre for litre it read 107
  old <- simulateGases(volumeRows(list(0, "air", 12), list(0, "ventilation", MV)),
                       weight = 70, age = 40, maximum = 60, volumeConversion = FALSE)
  expect_equal(utils::tail(old$state$oxygen[, 2], 1) / 100 * GAS_ALVEOLAR_TORR, 107.4,
               tolerance = 2e-3)
})

test_that("a closed circuit's oxygen: 0.25 L/min on the flowmeter is 0.233 L/min STPD", {
  body <- getGasBody(70)
  inLungs <- 0.25 * gasFreshGasExpansion(body)
  expect_equal(inLungs / gasStpdFactor(), 0.25 * 273.15 / 293.15)
  expect_equal(inLungs / gasStpdFactor(), 0.233, tolerance = 2e-3)
  # 5% short of the 0.245 L/min a 70 kg patient consumes; 0.27 covers it
  expect_lt(inLungs, gasOxygenUptake(body))
  expect_gt(0.27 * gasFreshGasExpansion(body), gasOxygenUptake(body))
  # In the engine: at 0.25 L/min the oxygen runs out, at 0.30 it does not
  run <- function(Q) simulateGases(
    volumeRows(list(0, "oxygen", Q), list(0, "ventilation", 5.7)),
    weight = 70, age = 40, maximum = 1440)
  expect_lt(utils::tail(run(0.25)$state$oxygen[, 2], 1), 1)
  expect_gt(utils::tail(run(0.30)$state$oxygen[, 2], 1), 50)
})

test_that("the Visualizer shows the flowmeters' flow and counts the agent in the lungs' litres", {
  DT <- volumeRows(list(0, "sevoflurane", 2), list(0, "oxygen", 2), list(0, "ventilation", 5.7))
  sim <- simulateGases(DT, weight = 70, age = 50, maximum = 60)
  E <- gasDrugEntries(sim, DT, getDrugDefaultsGlobal(), 60, age = 50)
  r <- viewerGasRun(E$sevoflurane, 0, 60)
  e <- gasFreshGasExpansion(sim$body)
  expect_equal(unique(r$freshFlow), 2)
  # 2 L/min x 2% x 60 min = 2.4 L of vapour at the flowmeters, e times it in
  # the lungs' litres
  expect_equal(utils::tail(r$delivered, 1), 10 * 2 * e * 2 * 60)
  # ... and the same mass either way
  expect_equal(r$greenhouse$kgPerLitre * e, gasGreenhouse("sevoflurane")$kgPerLitre)
})
