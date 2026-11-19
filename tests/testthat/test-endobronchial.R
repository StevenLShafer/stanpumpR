# Endobronchial intubation and the inhaled anesthetics (R/endobronchial.R;
# Shafer, 2026-11-19, from Eger and Shafer's chapter 8)

gasRun <- function(drug, dose, endobronchial, maximum = 60, ...) {
  dt <- if (drug == "nitrousOxide")
    data.frame(Drug = c("oxygen", "nitrousOxide", "ventilation"), Time = 0, Dose = c(2, 4, 6))
  else
    data.frame(Drug = c("oxygen", drug, "ventilation"), Time = 0, Dose = c(6, dose, 6))
  simulateGases(dt, maximum = maximum, endobronchial = endobronchial, ...)
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
