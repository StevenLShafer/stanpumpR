## TODO factors -> strings

outputComments <<- function(..., echo) {
## just silence
## print(paste("outputComments:", ...))
}

DEBUG <<- FALSE

test_that("suggest yields a table with the appropriate columns", {

drugList <- getDrugDefaultsGlobal(FALSE)$Drug

input <- list(referenceTime="none", targetDrug="propofol")

doseTable <- data.frame(Drug=input$targetDrug,Time=0,Dose=0,Units="mg")

eventTable <- data.frame(
  Time = 0,
  Event = "Event",
  Fill = "black"
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
                     input$referenceTime,
                     DEBUG=FALSE)

expect_equal(names(testTable), c("Time", "Dose", "Units", "resultTime", "Drug"))

})
