# ChatGPT's round-2 audit of the gas engine (2026-11-26, of stanpumpR 8c1ea1b;
# docs/gas-engine-audit/chatgpt-2026-11-26-round2-assessment.md).  Each test
# failed at 8c1ea1b.

viewerGasCase <- function(events, maximum = 60)
{
  dt <- data.frame(Drug = c("oxygen", "sevoflurane", "ventilation"), Time = 0,
                   Dose = c(6, 2, 6))
  s <- simulateGases(dt, age = 40, maximum = maximum, physiologyEvents = events)
  gasDrugEntries(s, dt, maximum = maximum, age = 40)$sevoflurane
}

physiologyEventRows <- function(times, settings)
{
  rows <- lapply(settings, physiologyEventDecode)
  data.frame(Time = times, do.call(rbind, rows))
}

test_that("E-01: an event outside the Visualizer's window does not refuse the window", {
  plain <- viewerGasRun(viewerGasCase(NULL), 0, 60)
  # A cooling event at 120 minutes, after the window: every row drawn is at
  # 37 C, and the run is the plain one's
  later <- viewerGasRun(viewerGasCase(physiologyEventRows(120, "temperature=32")), 0, 60)
  expect_equal(later$capacity, plain$capacity)
  expect_equal(later$p, plain$p)
  expect_length(later$physiologySteps, 1)
  # A window wholly after a cooling event at 20 minutes is drawn at 32 C
  entry <- viewerGasCase(physiologyEventRows(20, "temperature=32"))
  after <- viewerGasRun(entry, 30, 60)
  pr <- gasPropertiesAt(32)
  ltg <- gasPartitionTissueGas(pr[pr$gas == "sevoflurane", ])
  expect_equal(after$partition$mg, ltg[["muscle"]])
  expect_false(isTRUE(all.equal(after$partition$mg, plain$partition$mg)))
  # A window across the event is still refused, as before
  expect_error(viewerGasRun(entry, 0, 60), "body temperature")
})

test_that("E-01: a tube in force only after the window sends no arterial values", {
  run <- viewerGasRun(viewerGasCase(physiologyEventRows(120, "endobronchial=1"),
                                    maximum = 60), 0, 60)
  expect_null(run$arterial)
  expect_null(run$saturation)
  expect_null(run$paO2)
  # In the window, it does
  inside <- viewerGasRun(viewerGasCase(physiologyEventRows(30, "endobronchial=1")), 0, 60)
  expect_false(is.null(inside$arterial))
  expect_false(is.null(inside$saturation))
})

test_that("E-01: the physiology steps stop at the end of the run", {
  events <- physiologyEventRows(c(10, 90), c("cardiac=0.5", "cardiac=2"))
  steps <- viewerPhysiologySteps(events, 0, end = 60)
  expect_equal(vapply(steps, function(s) s$time, 0), c(0, 600))
  expect_length(viewerPhysiologySteps(events, 0), 3)
})

test_that("B-02: the semi-closed circuit takes no physiology GasMan lacks", {
  # Steven Shafer, 2026-11-26: "we should not permit it to reflect any
  # physiology not available to GasMan"
  normal <- physiologyEventNormal()
  high <- normal
  high[["altitude"]] <- 3000
  ev <- data.frame(Time = c(0, 30))
  for (nm in names(normal)) ev[[nm]] <- c(high[[nm]], normal[[nm]])
  d <- data.frame(Drug = c("oxygen", "ventilation"), Time = 0, Dose = c(0.5, 6))
  semi <- function(...) simulateGases(d, weight = 70, age = 40, maximum = 31,
                                      circuit = "semi-closed", ...)
  refused <- "takes no physiology GasMan lacks"
  # The step in altitude created gas: 0.60 L of nitrogen here at 8c1ea1b
  expect_error(semi(physiologyEvents = ev), refused)
  expect_error(semi(altitude = 3000), "altitude")
  expect_error(semi(temperature = 32), "body temperature")
  expect_error(semi(endobronchial = TRUE), "endobronchial")
  expect_error(semi(physiology = c(cardiac = 0.5)), "cardiac-output slider")
  expect_error(semi(haemoglobin = 8), "haemoglobin")
  expect_error(semi(height = 160, sex = "female"), "body composition")
  cardiac <- ev
  cardiac$altitude <- 0
  cardiac$cardiac <- c(1, 0.5)
  expect_error(semi(physiologyEvents = cardiac), "cardiac-output slider")
  # What GasMan has is taken: its own cardiac output, the reference man's
  # composed body (GasMan's standard), linear scaling, and normal settings
  expect_silent(semi(cardiacOutput = 4))
  expect_silent(simulateGases(d, weight = 70, age = 35, maximum = 31, circuit = "semi-closed",
                              height = 170, sex = "male"))
  expect_silent(semi(height = 160, sex = "female", adjustToFFM = FALSE))
  expect_silent(semi(temperature = 37, altitude = 0))
  # The ideal circuit, which the app runs, takes all of it as before
  expect_silent(simulateGases(d, weight = 70, age = 40, maximum = 31, physiologyEvents = ev,
                              temperature = 32, endobronchial = TRUE,
                              height = 160, sex = "female"))
})
