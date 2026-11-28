# Fresh gas adds to the delivered ventilation, the I:E ratio, and the
# ventilation the time until threshold runs at (the fourth piece of the gas
# engine expansion, docs/gas-engine-expansion.md).  Steven L. Shafer,
# 2026-11-27: "Fresh gas adds to delivered ventilation", "I:E ratio should be
# a user input, default 1:2", and of the time until threshold "It should use
# the higher ventilation as well."

ventRows <- function(flow = 2, vent = 6, agent = 2)
  data.frame(Drug = c("sevoflurane", "oxygen", "ventilation"), Time = 0,
             Dose = c(agent, flow, vent), stringsAsFactors = FALSE)

ventAt <- function(sim, site, t, drug = "sevoflurane")
{
  r <- sim$results[sim$results$Drug == drug & sim$results$Site == site, ]
  stats::approx(r$Time, r$Y, t, ties = "ordered")$y
}

test_that("the I:E ratio is one of five, 1:2 by default", {
  expect_equal(unname(GAS_IE_RATIOS), c(1, 1.5, 2, 3, 4))
  expect_equal(GAS_IE_DEFAULT, 2)
  expect_equal(checkIERatio(NULL), 2)
  expect_equal(checkIERatio(1.5), 1.5)
  expect_error(checkIERatio(2.5), "Invalid I:E ratio")
  expect_error(checkIERatio("2"), "Invalid I:E ratio")
  expect_error(checkIERatio(c(1, 2)), "Invalid I:E ratio")
  # The panel's choice, read: a link is untrusted
  expect_equal(ieRatioFromInput("3"), 3)
  expect_equal(ieRatioFromInput(1), 1)
  expect_equal(ieRatioFromInput(NULL), 2)
  expect_equal(ieRatioFromInput("junk"), 2)
  expect_equal(ieRatioFromInput(list(1, 2)), 2)
  expect_equal(ieRatioFromInput("7"), 2)
  # The inspiratory share of the breath
  body <- getGasBody(70)
  expect_equal(gasInspiratoryFraction(body), 0)
  for (ie in unname(GAS_IE_RATIOS)) {
    body$ieRatio <- ie
    expect_equal(gasInspiratoryFraction(body), 1 / (1 + ie))
  }
  # None in GasMan's mode
  body$volumesAsStated <- TRUE
  expect_equal(gasInspiratoryFraction(body), 0)
})

test_that("the delivered ventilation is the setting plus the inspiratory share of the fresh gas", {
  bySetting <- split(ventRows(), ventRows()$Drug)
  plain <- gasSettingsAt(bySetting, 0)
  expect_equal(plain$MV, 6)
  expect_equal(plain$MVset, 6)
  expect_equal(plain$VA, 6 * (1 - GAS_DEAD_SPACE_FRACTION))
  third <- gasSettingsAt(bySetting, 0, inspiratory = 1 / 3)
  expect_equal(third$MVset, 6)
  expect_equal(third$MV, 6 + 2 / 3)
  expect_equal(third$VA, (6 + 2 / 3) * (1 - GAS_DEAD_SPACE_FRACTION))
  # Both in the lungs' litres: the fresh gas as expanded
  x <- gasFreshGasExpansion(getGasBody(70))
  both <- gasSettingsAt(bySetting, 0, expansion = x, inspiratory = 1 / 3)
  expect_equal(both$Q, 2 * x)
  expect_equal(both$MV, 6 + 2 * x / 3)
  # The brief's table (no expansion): 6 L/min set, 1:2
  for (row in list(c(2, 6.7), c(6, 8.0), c(10, 9.3))) {
    s <- gasSettingsAt(split(ventRows(row[1]), ventRows(row[1])$Drug), 0, inspiratory = 1 / 3)
    expect_equal(round(s$MV, 1), row[2])
  }
})

test_that("no fresh gas joins a ventilation of 0 (Devin's review of PR 1029)", {
  # Fresh gas from time 0, the ventilator from minute five: until then there
  # is no inspiration for it to join
  d <- data.frame(Drug = c("sevoflurane", "oxygen", "ventilation"), Time = c(0, 0, 5),
                  Dose = c(2, 6, 6), stringsAsFactors = FALSE)
  bySetting <- split(d, d$Drug)
  early <- gasSettingsAt(bySetting, 2, inspiratory = 1 / 3)
  expect_equal(c(early$MV, early$MVset, early$VA), c(0, 0, 0))
  expect_equal(gasSettingsAt(bySetting, 6, inspiratory = 1 / 3)$MV, 8)
  # ... so the run is the run at the ventilation as set until minute five
  sim <- simulateGases(d, weight = 70, age = 40, maximum = 10)
  plain <- gasAtSetVentilation(simulateGases(d, weight = 70, age = 40, maximum = 10))
  early <- sim$timeLine < 5
  expect_equal(sim$state$sevoflurane[early, ], plain$state$sevoflurane[early, ], tolerance = 1e-12)
  expect_gt(max(abs(sim$state$sevoflurane[!early, 2] - plain$state$sevoflurane[!early, 2])), 0.01)
})

test_that("simulateGases() carries the I:E ratio, and GasMan's mode adds no fresh gas", {
  expect_equal(simulateGases(ventRows(), maximum = 5)$body$ieRatio, 2)
  expect_equal(simulateGases(ventRows(), maximum = 5, ieRatio = 4)$body$ieRatio, 4)
  expect_error(simulateGases(ventRows(), maximum = 5, ieRatio = 5), "Invalid I:E ratio")
  # GasMan's mode: the ratio is carried and does nothing
  a <- simulateGases(ventRows(), weight = 70, age = 40, maximum = 20, volumeConversion = FALSE)
  b <- simulateGases(ventRows(), weight = 70, age = 40, maximum = 20, volumeConversion = FALSE,
                     ieRatio = 1)
  expect_identical(a$results$Y, b$results$Y)
  semi <- simulateGases(ventRows(), weight = 70, age = 40, maximum = 20, circuit = "semi-closed")
  semi1 <- simulateGases(ventRows(), weight = 70, age = 40, maximum = 20, circuit = "semi-closed",
                         ieRatio = 1)
  expect_identical(semi$results$Y, semi1$results$Y)
  # With physiology events every body carries it
  events <- physiologyEventsFrom(eventTableRows(
    Time = 10, Event = PHYSIOLOGY_EVENT_NAME, Settings = physiologyEventEncode(c(shunt = 0.2))))
  stepped <- simulateGases(ventRows(), maximum = 20, ieRatio = 3, physiologyEvents = events)
  expect_equal(vapply(stepped$bodySteps$body, function(b) b$ieRatio, 0), c(3, 3))
})

test_that("more inspiration, or more fresh gas, is more ventilation: end-tidal rises faster", {
  run <- function(...) simulateGases(ventRows(...), weight = 70, age = 40, maximum = 30)
  ie <- function(x) simulateGases(ventRows(), weight = 70, age = 40, maximum = 30, ieRatio = x)
  # A longer inspiratory share delivers more of the fresh gas
  early <- vapply(c(1, 1.5, 2, 3, 4), function(x) ventAt(ie(x), "Alveolar", 2), 0)
  expect_true(all(diff(early) < 0))
  # The engine with the coupling is the engine at the delivered ventilation
  # without it: 6 L/min set with 2 L/min of fresh gas at 1:2 is 6 + 2x/3
  x <- gasFreshGasExpansion(getGasBody(70))
  coupled <- simulateGases(ventRows(), weight = 70, age = 40, maximum = 30)
  body <- gasShuntBody(70)
  same <- advanceClosedFormGas(
    data.frame(Time = 0, Drug = c("sevoflurane", "oxygen", "ventilation"),
               Dose = c(2, 2, 6 + 2 * x / 3)), weight = 70, age = 40, maximum = 30, body = body)
  expect_equal(coupled$results$Y, same$results$Y, tolerance = 1e-12)
  # The help page's numbers: 2% sevoflurane, 5.7 L/min set
  at30 <- function(flow, ieRatio = NULL) {
    s <- simulateGases(ventRows(flow, 5.7), weight = 70, age = 40, maximum = 30, ieRatio = ieRatio)
    round(c(ventAt(s, "Alveolar", 30), ventAt(s, "Brain", 30)), 3)
  }
  expect_equal(at30(2), c(1.565, 1.520))
  expect_equal(at30(2, 1), c(1.571, 1.526))
  expect_equal(at30(2, 4), c(1.561, 1.516))
})

test_that("the time until threshold runs at the ventilation the least flow without rebreathing delivers", {
  body <- gasShuntBody(70)
  expect_equal(gasWashoutVentilation(6, 0.3, body), 6 * 0.7)
  body$ieRatio <- 2
  # 1.5 times the setting at 1:2
  expect_equal(gasWashoutVentilation(6, 0.3, body), 1.5 * 6 * 0.7)
  body$ieRatio <- 1
  expect_equal(gasWashoutVentilation(6, 0.3, body), 2 * 6 * 0.7)
  # So a longer inspiratory share is a shorter time until threshold
  d <- ventRows(2, 5.7)
  time <- vapply(c(1, 2, 4), function(x) {
    s <- simulateGases(d, weight = 70, age = 40, maximum = 30, ieRatio = x)
    utils::tail(gasRecoveryTime(gasWashout(s, d), "sevoflurane", 0.18), 1)
  }, 0)
  expect_true(all(diff(time) > 0))
  expect_equal(round(time, 1), c(7.8, 8.5, 9.1))
  # ... and it does not depend on the fresh gas flow in use at the time,
  # only through the state it left
  s <- simulateGases(d, weight = 70, age = 40, maximum = 30)
  w <- gasWashout(s, d)
  bySetting <- split(d, d$Drug)
  set <- gasSettingsAt(bySetting, 0, GAS_DEAD_SPACE_FRACTION, 1, gasFreshGasExpansion(s$body),
                       gasInspiratoryFraction(s$body))
  expect_gt(gasWashoutVentilation(set$MVset, GAS_DEAD_SPACE_FRACTION, s$body), set$VA)
})

test_that("the panel offers the I:E ratio, and the workbook records it", {
  html <- as.character(physiologyPanel())
  expect_match(html, 'id="physiology_ie"', fixed = TRUE)
  for (label in names(GAS_IE_RATIOS)) expect_match(html, paste0(">", label, "<"), fixed = TRUE)
  rows <- physiologyCovariateRows(PHYSIOLOGY_DEFAULT)
  expect_equal(rows$Value[rows$Covariate == "I:E ratio, inhaled anesthetics"], "1:2")
  expect_equal(physiologyCovariateRows(PHYSIOLOGY_DEFAULT, ieRatio = 1.5)$Value[
    rows$Covariate == "I:E ratio, inhaled anesthetics"], "1:1.5")
  # With physiology events the ratio is still recorded: it is no part of one
  events <- data.frame(Time = 10)
  for (f in PHYSIOLOGY_EVENT_FIELDS) events[[f]] <- physiologyEventNormal()[[f]]
  withEvents <- physiologyCovariateRows(PHYSIOLOGY_DEFAULT, events = events, ieRatio = 4)
  expect_equal(withEvents$Value[withEvents$Covariate == "I:E ratio, inhaled anesthetics"], "1:4")
  # Shown with a ventilation row, an anesthetic or not (Devin's review of PR
  # 1029): the ratio moves the oxygen curve of a run with oxygen alone
  expect_match(html, 'id="field_ventilation"', fixed = TRUE)
  expect_true(relevantFields(c("oxygen", "ventilation"))[["ventilation"]])
  expect_true(relevantFields(c("sevoflurane", "oxygen", "ventilation"))[["ventilation"]])
  expect_false(relevantFields(c("propofol"))[["ventilation"]])
  expect_false(relevantFields(c("oxygen", "ventilation"))[["inhaledAgents"]])
  shiny::testServer(app_server, {
    expect_equal(ieRatio(), 2)
    session$setInputs(physiology_ie = "4")
    expect_equal(ieRatio(), 4)
    session$setInputs(physiology_ie = "nonsense")
    expect_equal(ieRatio(), 2)
  })
})
