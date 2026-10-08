test_that("suggest yields a table with the appropriate columns", {

  local_mocked_bindings(outputComments = function(...) {})

  drugList <- getDrugDefaultsGlobal(FALSE)$Drug

  input <- list(referenceTime="none", targetDrug="propofol")

  doseTable <- data.frame(Drug=input$targetDrug,Time=0,Dose=0,Units="mg")

  eventTable <- data.frame(
    Time = 0,
    Event = "Event"
  )

  age <- 50
  weight <- 60
  height <- 66*2.54
  sex <- "female"

  plotMaximum <- 60
  plotRecovery <- FALSE

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

  targetTable <- data.frame(
      Time = c("2","20",rep("",4)),
      Target = c("2","2",rep("",4))
  )

  endTime <- 60

  testTable <- suggest(input$targetDrug,
                       targetTable,
                       endTime,
                       drugs,
                       drugList,
                       eventTable,
                       input$referenceTime)

  expect_equal(names(testTable), c("Time", "Dose", "Units", "resultTime", "Drug"))
})

test_that("a target at time 0 is a target, not a blank row", {
  # Blank rows were told apart by their cleaned time being 0, which dropped a
  # real target at 0, and the doses then started at the next target.  The app
  # passes suggest() minutes, so a target at the procedure start is "0" too.
  local_mocked_bindings(outputComments = function(...) {})
  defaults <- getDrugDefaultsGlobal(FALSE)
  doseTable <- data.frame(Drug = "propofol", Time = 0, Dose = 0, Units = "mg")
  eventTable <- data.frame(Time = 0, Event = "Event")
  drugs <- processdoseTable(
    doseTable, eventTable,
    recalculatePK(NULL, defaults, doseTable, 50, 60, 66 * 2.54, "female"),
    60, FALSE
  )
  targetTable <- data.frame(Time = c("0", "20", rep("", 4)), Target = c("2", "3", rep("", 4)))
  testTable <- suggest("propofol", targetTable, 60, drugs, defaults$Drug, eventTable, REFERENCE_TIME_NONE)
  expect_equal(min(testTable$Time), 0)
  # one bolus per target
  expect_equal(sum(testTable$Units == defaults$Bolus.Units[defaults$Drug == "propofol"]), 2)
})
