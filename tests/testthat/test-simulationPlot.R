test_that("simulationPlot yields desired objects", {

  local_mocked_bindings(outputComments = function(...) {})

  doseTable <- data.frame(
    Drug = getDrugDefaultsGlobal(FALSE)$Drug[1],
    Time = 0,
    Dose = 1,
    Units = "mg"
  )

  eventTable <- data.frame(
    Time = 0,
    Event = "Event",
    Fill = "black"
  )

  ## from server.R

  age <- 50
  weight <- 60
  height <- 66*2.54
  sex <- "female"

  plotMaximum <- 60
  plotRecovery <- TRUE
  plotEvents <- TRUE

  newDrugs <- recalculatePK(
    NULL,
    getDrugDefaultsGlobal(FALSE),
    doseTable,
    age, weight, height, sex
  )

  drugs <- processdoseTable(
    doseTable,
    eventTable,
    newDrugs,
    plotMaximum,
    plotRecovery
  )

  p <- simulationPlot(
    drugs = drugs,
    events = eventTable,
    drugDefaults = getDrugDefaultsGlobal(FALSE),
    eventDefaults = getEventDefaults(),
    plotEvents = plotEvents,
    plotRecovery = plotRecovery
  )

  expect_equal(names(p), c("plotObject","allResults","plotResults","nFacets"))
})
