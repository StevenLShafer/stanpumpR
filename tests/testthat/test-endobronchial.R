# Endobronchial intubation and the inhaled anesthetics (R/endobronchial.R;
# Shafer, 2026-11-19, from Eger and Shafer's chapter 8)

# The two-lung comparison is the chapter's, a patient with no shunt: since
# 2026-11-28 the engine's own default without the tube is the 10% shunt under
# anesthesia (test-gas-shunt.R)
gasRun <- function(drug, dose, endobronchial, maximum = 60,
                   shunt = if (isTRUE(endobronchial)) NULL else 0, ...) {
  dt <- if (drug == "nitrousOxide")
    data.frame(Drug = c("oxygen", "nitrousOxide", "ventilation"), Time = 0, Dose = c(2, 4, 6))
  else
    data.frame(Drug = c("oxygen", drug, "ventilation"), Time = 0, Dose = c(6, dose, 6))
  simulateGases(dt, maximum = maximum, endobronchial = endobronchial, shunt = shunt, ...)
}
at <- function(sim, gas, t) {
  y <- sim$state[[gas]][which.min(abs(sim$timeLine - t)), ]
  c(et = y[[2]], art = gasArterial(y, sim$body), brain = y[[3]])
}

test_that("without the tube nothing changes", {
  a <- gasRun("sevoflurane", 2, FALSE)
  b <- gasRun("sevoflurane", 2, NULL)
  expect_identical(a$state, b$state)
  expect_identical(gasShunt(a$body), 0)
  y <- a$state$sevoflurane[10, ]
  expect_identical(gasArterial(y, a$body), y[[2]])
})

test_that("the tube shunts half the blood past one ventilated lung", {
  body <- gasBodyEndobronchial(getGasBody(70), TRUE)
  expect_equal(gasShunt(body), ENDOBRONCHIAL_SHUNT_FRACTION)
  expect_equal(body$V_alveolar, getGasBody(70)$V_alveolar * ENDOBRONCHIAL_VENTILATED_VOLUME_SHARE)
  # Arterial is the mixture of ventilated-lung and shunted blood
  y <- c(0, 50, 10, 5, 1)
  expect_equal(gasArterial(y, body), 0.5 * 50 + 0.5 * gasMixedVenous(y, body))
  # The uptake is the ventilated lung's alone
  state <- list(nitrousOxide = y, oxygen = c(30, 30))
  props <- getGasProperties()
  expect_equal(gasTotalUptake(state, props, body, 5),
               0.5 * gasTotalUptake(state, props, getGasBody(70), 5))
})

test_that("end-tidal reads high and arterial lags, most for the insoluble agents (Eger)", {
  ratio <- function(g, dose, t) {
    eb <- at(gasRun(g, dose, TRUE), g, t)
    two <- at(gasRun(g, dose, FALSE), g, t)
    c(art = eb[["art"]] / two[["art"]], et = eb[["et"]] / two[["et"]],
      gap = eb[["et"]] - eb[["art"]])
  }
  n2o <- ratio("nitrousOxide", NA, 2)
  iso <- ratio("isoflurane", 1.2, 2)
  # End-tidal above normal, arterial below it
  expect_gt(n2o[["et"]], 1)
  expect_lt(n2o[["art"]], 0.8)
  # The more soluble agent is less delayed (the chapter's ether limit)
  expect_gt(iso[["art"]], n2o[["art"]])
  # The gap closes as the shunted blood comes to equilibrium
  late <- ratio("nitrousOxide", NA, 60)
  expect_gt(late[["art"]], n2o[["art"]])
  expect_gt(late[["art"]], 0.9)
})

test_that("the washout and the time until threshold run with the tube", {
  dt <- data.frame(Drug = c("oxygen", "sevoflurane", "ventilation", "sevoflurane"),
                   Time = c(0, 0, 6, 30), Dose = c(6, 2, 6, 0))
  dt$Time[3] <- 0
  sim <- simulateGases(dt, maximum = 90, endobronchial = TRUE)
  w <- gasWashout(sim, dt[, c("Time", "Drug", "Dose")])
  entries <- gasDrugEntries(sim, dt, getDrugDefaultsGlobal(), 90, washout = w,
                            age = 50, macThreshold = 0.5)
  expect_true(length(entries) > 0)
})

test_that("the box shows only with an inhaled agent, and the Visualizer is told", {
  shown <- relevantFields(c("propofol"))
  expect_false(shown[["inhaledAgents"]])
  expect_true(relevantFields(c("sevoflurane"))[["inhaledAgents"]])
  expect_false(relevantFields(c("oxygen", "air"))[["inhaledAgents"]])
  html <- as.character(physiologyPanel())
  field <- regexpr('id="field_inhaledAgents"', html, fixed = TRUE)
  expect_gt(field, 0)
  tail <- substring(html, field)
  expect_match(tail, 'id="physiology_altitude"', fixed = TRUE)
  expect_match(tail, 'id="endobronchialIntubation"', fixed = TRUE)
  # The run carries the panel, and with the tube its own arterial line
  sim <- gasRun("sevoflurane", 2, TRUE, altitude = 3000)
  rows <- data.frame(Drug = c("oxygen", "sevoflurane", "ventilation"), Time = 0,
                     Dose = c(6, 2, 6))
  entries <- gasDrugEntries(sim, rows, getDrugDefaultsGlobal(), 60, age = 50)
  run <- viewerGasRun(entries[["sevoflurane"]], 0, 60,
                      physiology = c(liver = 0.5, renal = 1, cardiac = 1, gi = 1, temperature = 36))
  expect_true(run$physiology$endobronchial)
  expect_equal(run$physiology$altitude, 3000)
  expect_equal(run$physiology$pressure, round(barometricPressure(3000)))
  expect_equal(run$physiology$liver, 0.5)
  expect_equal(run$physiology$temperature, 36)
  expect_false(is.null(run$arterial))
  expect_true(all(run$arterial <= run$p$lungs + 1e-9))
  shiny::testServer(app_server, {
    session$setInputs(endobronchialIntubation = TRUE)
    expect_true(endobronchial())
    session$setInputs(endobronchialIntubation = "yes")
    expect_false(endobronchial())
  })
})

test_that("the Shunt fraction slider sets the tube's shunt, 0 to 50% (Shafer, 2026-11-25)", {
  # With the tube the default and the maximum is the chapter's half
  expect_equal(checkShunt(NULL, TRUE), 0.5)
  expect_equal(checkShunt(0), 0)
  expect_error(checkShunt(0.6), "shunt")
  expect_error(checkShunt(-0.1), "shunt")
  expect_equal(shuntFromInput(30), 0.3)
  expect_equal(shuntFromInput(80), 0.5)
  expect_equal(shuntFromInput(-5), 0)
  expect_equal(shuntFromInput("junk", TRUE), 0.5)
  expect_equal(shuntFromInput(NULL, TRUE), 0.5)
  # The body carries it, with the tube or without (the shunt under
  # anesthesia, 2026-11-28); only the tube halves the ventilated gas volume
  body <- gasBodyEndobronchial(getGasBody(70), TRUE, 0.2)
  expect_equal(gasShunt(body), 0.2)
  plain <- gasBodyEndobronchial(getGasBody(70), FALSE, 0.2)
  expect_equal(gasShunt(plain), 0.2)
  expect_equal(plain$V_alveolar, getGasBody(70)$V_alveolar)
  expect_equal(body$V_alveolar, getGasBody(70)$V_alveolar * ENDOBRONCHIAL_VENTILATED_VOLUME_SHARE)
  expect_false(identical(gasRun("sevoflurane", 2, FALSE, shunt = 0.2)$state,
                         gasRun("sevoflurane", 2, FALSE)$state))
  # The default is the old tube exactly
  expect_identical(gasRun("sevoflurane", 2, TRUE, shunt = 0.5)$state,
                   gasRun("sevoflurane", 2, TRUE)$state)
  # Lowering it brings arterial toward end-tidal and the oxygen up
  art <- function(s) {
    a <- at(gasRun("nitrousOxide", NA, TRUE, shunt = s), "nitrousOxide", 5)
    a[["art"]] / a[["et"]]
  }
  expect_lt(art(0.5), art(0.25))
  expect_lt(art(0.25), art(0.1))
  expect_equal(art(0), 1)
  oxygen <- function(s) {
    o <- gasArterialOxygen(gasRun("sevoflurane", 2, TRUE, shunt = s))
    o$SaO2[nrow(o)]
  }
  expect_lt(oxygen(0.5), oxygen(0.2))
  expect_lt(oxygen(0.2), oxygen(0.05))
  # At 0 nothing is shunted: arterial oxygen is end-capillary, and the
  # Visualizer is sent no arterial line of its own
  o0 <- gasArterialOxygen(gasRun("sevoflurane", 2, TRUE, shunt = 0))
  expect_equal(o0$PaO2, o0$PAO2)
  expect_null(gasRunArterialOxygen(gasRun("sevoflurane", 2, TRUE, shunt = 0)))
  # A physiology event records it, reads it back and runs on it
  s <- physiologyEventEncode(c(endobronchial = 1, shunt = 0.3))
  expect_equal(s, "endobronchial=1;shunt=0.3")
  expect_equal(physiologyEventDecode(s)[["shunt"]], 0.3)
  expect_null(physiologyEventDecode("endobronchial=1;shunt=0.7"))
  expect_match(physiologyEventSummary(physiologyEventDecode(s)), "shunt 30%", fixed = TRUE)
  ev <- data.frame(Time = 10, t(physiologyEventDecode(s)))
  steps <- gasBodySteps(70, ev)
  expect_equal(gasShunt(steps$body[[2]]), 0.3)
  # An event written before the slider existed keeps the chapter's half
  expect_equal(physiologyEventDecode("endobronchial=1")[["shunt"]], 0.5)
  # The panel shows it with the box, and the workbook records it
  html <- as.character(physiologyPanel())
  expect_match(html, 'id="physiology_shunt"', fixed = TRUE)
  expect_lt(regexpr('id="endobronchialIntubation"', html, fixed = TRUE),
            regexpr('id="physiology_shunt"', html, fixed = TRUE))
  rows <- physiologyCovariateRows(PHYSIOLOGY_DEFAULT, endobronchial = TRUE, shunt = 0.3)
  expect_equal(rows$Value[rows$Covariate == "Shunt fraction (%), inhaled anesthetics"], "30")
  expect_equal(physiologyCovariateRows(PHYSIOLOGY_DEFAULT)$Value[5 + 3], "10")
  # The Visualizer is told
  sim <- gasRun("sevoflurane", 2, TRUE, shunt = 0.3)
  expect_equal(viewerPhysiology(NULL, sim$body)$shunt, 30)
  # The server reads the slider, and an event records it (test-gas-shunt.R
  # for the box moving the slider)
  shiny::testServer(app_server, {
    session$setInputs(endobronchialIntubation = TRUE, physiology_shunt = 25)
    session$elapse(PHYSIOLOGY_DEBOUNCE_MS + 100)
    expect_equal(shunt(), 0.25)
    expect_equal(physiologyPanelSettings()[["shunt"]], 0.25)
  })
})
