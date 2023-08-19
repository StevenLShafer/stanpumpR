library(pkgload)
library(here)

outputComments <<- function(..., echo) {
## just silence
## print(paste("outputComments:", ...))
}

DEBUG <<- FALSE

test_that("simulationPlot yields desired objects", {

load_all(here())

doseTable <- data.frame(
  Drug = drugDefaults_global$Drug[1],
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
  drugDefaults_global,
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
  drugDefaults = drugDefaults_global,
  eventDefaults = eventDefaults,
  plotEvents = plotEvents,
  plotRecovery = plotRecovery
)

expect_equal(names(p), c("plotObject","allResults","plotResults"))

})

