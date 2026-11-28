# The shunt under anesthesia (R/endobronchial.R; the second piece of the gas
# engine expansion, docs/gas-engine-expansion.md).  Steven L. Shafer,
# 2026-11-27: "10% shunt under anesthesia", for every gas ("If the anesthetic
# is entering from the pulmonary arteries, then yes.  It flows through the
# lungs unaffected."), and "One shunt slider, 10% normally, 50% with the
# endobronchial tube."

shuntTable <- function(agent = 2, oxygen = 6, air = 0, ventilation = 6)
{
  d <- data.frame(Drug = c("sevoflurane", "oxygen", "air", "ventilation"), Time = 0,
                  Dose = c(agent, oxygen, air, ventilation),
                  Units = c("%", "L/min", "L/min", "L/min"), stringsAsFactors = FALSE)
  d[d$Dose > 0 | d$Drug != "air", ]
}

shuntSeries <- function(sim, site, drug = "sevoflurane")
{
  r <- sim$results[sim$results$Drug == drug & sim$results$Site == site, ]
  r[order(r$Time), ]
}

test_that("the default shunt is 10% under anesthesia and 50% with the tube", {
  expect_equal(GAS_SHUNT_ANESTHESIA, 0.10)
  expect_equal(gasShuntDefault(FALSE), 0.10)
  expect_equal(gasShuntDefault(TRUE), ENDOBRONCHIAL_SHUNT_FRACTION)
  expect_equal(checkShunt(NULL), 0.10)
  expect_equal(checkShunt(NULL, TRUE), 0.5)
  expect_equal(checkShunt(0.2, TRUE), 0.2)
  expect_error(checkShunt(0.6), "Invalid shunt fraction")
  # The slider, read: an unreadable value is the state's default
  expect_equal(shuntFromInput(NULL), 0.10)
  expect_equal(shuntFromInput("x", TRUE), 0.5)
  expect_equal(shuntFromInput(25), 0.25)
  expect_equal(shuntFromInput(80), 0.5)

  expect_equal(gasShunt(simulateGases(shuntTable(), maximum = 5)$body), 0.10)
  expect_equal(gasShunt(simulateGases(shuntTable(), maximum = 5, endobronchial = TRUE)$body), 0.5)
  expect_equal(gasShunt(simulateGases(shuntTable(), maximum = 5, shunt = 0.3)$body), 0.3)
  # A body the engine is handed directly carries none: GasMan's
  expect_equal(gasShunt(getGasBody(70)), 0)
})

test_that("GasMan's mode has no shunt unless one is asked for", {
  expect_equal(gasShunt(simulateGases(shuntTable(), maximum = 5,
                                      volumeConversion = FALSE)$body), 0)
  expect_equal(gasShunt(simulateGases(shuntTable(), maximum = 5,
                                      circuit = "semi-closed")$body), 0)
  expect_equal(gasShunt(simulateGases(shuntTable(), maximum = 5, shunt = 0.1,
                                      volumeConversion = FALSE)$body), 0.1)
  # The semi-closed circuit is GasMan's and takes no physiology GasMan lacks
  expect_error(simulateGases(shuntTable(), maximum = 5, circuit = "semi-closed", shunt = 0.1),
               "a shunt")
  # simulateGases(shunt = 0) is the engine handed GasMan's body
  a <- simulateGases(shuntTable(), weight = 70, age = 40, maximum = 20, shunt = 0)
  b <- advanceClosedFormGas(
    data.frame(Time = 0, Drug = c("sevoflurane", "oxygen", "ventilation"), Dose = c(2, 6, 6)),
    weight = 70, age = 40, maximum = 20)
  expect_equal(a$results$Y, b$results$Y, tolerance = 1e-12)
})

test_that("the shunt delays the brain and leaves a little more in the alveoli", {
  none <- simulateGases(shuntTable(), weight = 70, age = 40, maximum = 30, shunt = 0)
  ten <- simulateGases(shuntTable(), weight = 70, age = 40, maximum = 30)
  at <- function(sim, site, t) { s <- shuntSeries(sim, site); stats::approx(s$Time, s$Y, t, ties = "ordered")$y }
  for (t in c(2, 10, 30)) {
    expect_lt(at(ten, "Brain", t), at(none, "Brain", t))
    expect_gt(at(ten, "Alveolar", t), at(none, "Alveolar", t))
  }
  # The help page's numbers (inst/app-help/models/physiology.md, gas-differences.md)
  expect_equal(round(at(ten, "Brain", 10), 2), 1.50)
  expect_equal(round(at(none, "Brain", 10), 2), 1.55)
  expect_equal(round(at(ten, "Brain", 30), 2), 1.71)
  expect_equal(round(at(none, "Brain", 30), 2), 1.74)
  expect_equal(round(at(ten, "Alveolar", 30), 3), 1.753)
  expect_equal(round(at(none, "Alveolar", 30), 3), 1.748)
})

test_that("arterial blood is (1 - s) alveolar + s mixed venous for every gas", {
  sim <- simulateGases(shuntTable(), weight = 70, age = 40, maximum = 10)
  body <- sim$body
  expect_equal(gasShunt(body), 0.1)
  m <- sim$state[["sevoflurane"]]
  y <- as.numeric(m[nrow(m), ])
  expect_gt(length(y), 4)
  expect_lt(gasArterial(y, body), y[[2]])
  expect_equal(gasArterial(y, body), 0.9 * y[[2]] + 0.1 * gasMixedVenous(y, body))
  # At the first moment the venous blood holds no agent: arterial is 0.9 of
  # the alveolar
  y0 <- c(1, 1, rep(0, length(y) - 2))
  expect_equal(gasArterial(y0, body), 0.9)
  # During washout arterial is above the alveolar
  yw <- c(0, 0.2, rep(1, length(y) - 2))
  expect_gt(gasArterial(yw, body), yw[[2]])
})

test_that("the agent is conserved with the shunt", {
  # Delivered less held is what was vented, which never runs backwards: the
  # Visualizer's balance, in millilitres at the machine
  DT <- shuntTable(oxygen = 2)
  sim <- simulateGases(DT, weight = 70, age = 40, maximum = 30)
  E <- gasDrugEntries(sim, DT, getDrugDefaultsGlobal(), 30, age = 40)
  run <- viewerGasRun(E$sevoflurane, 0, 30)
  held <- Reduce(`+`, lapply(names(run$capacity), function(k)
    run$amountScale * run$capacity[[k]] * run$p[[k]]))
  vented <- run$delivered - held
  expect_true(all(vented > -1e-6 * max(run$delivered)))
  expect_true(all(diff(vented) > -1e-6 * max(run$delivered)))
  # Arterial blood is sent, below the lungs' partial pressure during uptake
  expect_false(is.null(run$arterial))
  expect_equal(length(run$arterial), length(run$p$lungs))
  expect_gt(length(held), 10)
  expect_true(all(run$arterial <= run$p$lungs + 1e-12))
  expect_false(is.null(run$saturation))
})

test_that("the arterial oxygen falls with the shunt (the help page's table)", {
  ao <- function(DT, ...) {
    sim <- simulateGases(DT, weight = 70, age = 40, maximum = 30, ...)
    a <- gasRunArterialOxygen(sim)
    c(PaO2 = a$PaO2[nrow(a)], SaO2 = a$SaO2[nrow(a)])
  }
  oxygen <- shuntTable(); air <- shuntTable(oxygen = 0, air = 6)
  expect_equal(round(ao(oxygen)[["PaO2"]]), 478)
  expect_equal(round(ao(air)), c(PaO2 = 77, SaO2 = 95))
  expect_equal(round(ao(oxygen, shunt = 0.2)[["PaO2"]]), 259)
  expect_equal(round(ao(air, shunt = 0.03)), c(PaO2 = 91, SaO2 = 97))
  expect_equal(round(ao(air, shunt = 0.3)), c(PaO2 = 54, SaO2 = 88))
  # No shunt, no arterial line: arterial is the alveolar
  expect_null(gasRunArterialOxygen(simulateGases(oxygen, maximum = 5, shunt = 0)))
  # Haemoglobin and altitude, on air at 10%
  expect_equal(round(ao(air, haemoglobin = 7)[["SaO2"]]), 93)
  expect_equal(round(ao(air, haemoglobin = 17)[["SaO2"]]), 96)
  expect_equal(round(ao(air, altitude = 3000)[["SaO2"]]), 84)
  expect_equal(round(ao(air, altitude = 5000)[["SaO2"]]), 48)
})

test_that("a physiology event records the shunt only away from its state's default", {
  expect_equal(physiologyEventNormal()[["shunt"]], 0.10)
  expect_equal(physiologyEventEncode(physiologyEventNormal()), "normal")
  expect_equal(physiologyEventEncode(c(shunt = 0.2)), "shunt=0.2")
  # The tube's own default is not recorded, and one not given is the state's
  expect_equal(physiologyEventEncode(c(endobronchial = 1)), "endobronchial=1")
  expect_equal(checkPhysiologyEvent(c(endobronchial = 1))[["shunt"]], 0.5)
  expect_equal(physiologyEventEncode(c(endobronchial = 1, shunt = 0.5)), "endobronchial=1")
  expect_equal(physiologyEventEncode(c(endobronchial = 1, shunt = 0.1)),
               "endobronchial=1;shunt=0.1")
  # A link made when the shunt existed only with the tube reads as it did
  expect_equal(physiologyEventDecode("endobronchial=1")[["shunt"]], 0.5)
  expect_equal(physiologyEventDecode("endobronchial=1;shunt=0.3")[["shunt"]], 0.3)
  expect_equal(physiologyEventDecode("cardiac=0.5")[["shunt"]], 0.10)
  expect_equal(physiologyEventDecode("shunt=0.2")[["shunt"]], 0.2)
  expect_match(physiologyEventSummary(c(shunt = 0.2), gas = TRUE), "Tracheal intubation, shunt 20%")
})

test_that("the bodies of a run with physiology events carry each state's shunt", {
  events <- physiologyEventsFrom(eventTableRows(
    Time = c(10, 20), Event = PHYSIOLOGY_EVENT_NAME,
    Settings = c(physiologyEventEncode(c(endobronchial = 1)), physiologyEventEncode(c(shunt = 0.2)))))
  steps <- gasBodySteps(70, events)
  expect_equal(vapply(steps$body, gasShunt, 0), c(0.10, 0.5, 0.2))
})

test_that("with events GasMan's mode still has no shunt (Devin's review of PR 1022)", {
  events <- physiologyEventsFrom(eventTableRows(
    Time = c(10, 20), Event = PHYSIOLOGY_EVENT_NAME,
    Settings = c(physiologyEventEncode(c(temperature = 36)), physiologyEventEncode(c(shunt = 0.2)))))
  sim <- simulateGases(shuntTable(), maximum = 30, volumeConversion = FALSE,
                       physiologyEvents = events)
  expect_equal(vapply(sim$bodySteps$body, gasShunt, 0), c(0, 0, 0.2))
  usual <- simulateGases(shuntTable(), maximum = 30, physiologyEvents = events)
  expect_equal(vapply(usual$bodySteps$body, gasShunt, 0), c(0.1, 0.1, 0.2))
})

test_that("the Visualizer says the shunt only away from its state's default", {
  body <- simulateGases(shuntTable(), maximum = 5)$body
  expect_equal(viewerPhysiology(NULL, body)$shunt, 10)
  expect_false(viewerPhysiology(NULL, body)$endobronchial)
  tube <- simulateGases(shuntTable(), maximum = 5, endobronchial = TRUE, shunt = 0.3)$body
  expect_equal(viewerPhysiology(NULL, tube)$shunt, 30)
})

test_that("one shunt slider: 10% normally, 50% with the tube (Shafer, 2026-11-27)", {
  shiny::testServer(app_server, {
    session$setInputs(endobronchialIntubation = FALSE, physiology_shunt = 10)
    session$elapse(PHYSIOLOGY_DEBOUNCE_MS + 100)
    expect_equal(shunt(), 0.10)
    # Ticking the box moves it to 50% at once, with no wait for the slider
    session$setInputs(endobronchialIntubation = TRUE)
    expect_equal(shunt(), 0.5)
    # The slider lowers it, standing in for hypoxic vasoconstriction
    session$setInputs(physiology_shunt = 30)
    session$elapse(PHYSIOLOGY_DEBOUNCE_MS + 100)
    expect_equal(shunt(), 0.3)
    expect_equal(physiologyPanelSettings()[["shunt"]], 0.3)
    # Unticking moves it back to 10%
    session$setInputs(endobronchialIntubation = FALSE)
    expect_equal(shunt(), 0.10)
    # Without the tube the slider is the shunt under anesthesia
    session$setInputs(physiology_shunt = 20)
    session$elapse(PHYSIOLOGY_DEBOUNCE_MS + 100)
    expect_equal(shunt(), 0.2)
    expect_equal(physiologyPanelSettings()[["shunt"]], 0.2)
  })
})

test_that("the shunt and haemoglobin sliders are shown with the inhaled agents, not only the tube", {
  html <- as.character(physiologyPanel())
  expect_match(html, 'id="physiology_shunt"', fixed = TRUE)
  expect_match(html, 'data-from="10"', fixed = TRUE)
  expect_false(grepl("input.endobronchialIntubation", html, fixed = TRUE))
})
