# "Stop" in the plot's dose dialogs (R/app_server.R; the table rewrite is
# stopAdministration() in R/stop-administration.R, tested in
# test-stop-administration.R), and the Min time slider.  Drafted by Claude
# Code, 2026-10-24, at the request of Steven L. Shafer.  Driven with
# shiny::testServer(), as test-skip-dose-server.R.

local_mocked_bindings(outputComments = function(...) {})

oldConfig <- .sprglobals$config
.sprglobals$config <- DEFAULT_CONFIG

stopAppInputs <- list(
  timeUnits = "minutes", timeMode = "relative", referenceTime = "08:00", maximum = "240",
  plotWidth = 800, yaxisHeight = 200, plasmaLinetype = "blank", effectsiteLinetype = "solid",
  weight = 70, weightUnit = "1", height = 170, heightUnit = "1", age = 40, ageUnit = "1",
  sex = "male", showThreshold = FALSE, normalization = "none", typical = "Range", logY = FALSE
)

infusion <- function() {
  dt <- doseTableInit
  dt[1, ] <- list("propofol", "0", "100", "mcg/kg/min")
  dt[2:6, ] <- list("", "", "", "")
  dt
}

test_that("the dose dialogs offer Stop only while something runs, and it writes a 0", {
  shiny::testServer(app_server, {
    do.call(session$setInputs, stopAppInputs)
    setDoseTable(infusion(), timeFormat("minutes", "relative"))
    session$flushReact()

    # A click at 60 minutes on propofol: the infusion runs
    showAddDrugModal("propofol", 60)
    session$flushReact()
    html <- as.character(output$stopAdministrationUI$html)
    expect_match(html, "Stop propofol at 60")
    expect_match(html, "the infusion \\(mcg/kg/min\\)")

    # Moving the dialog to a drug with nothing running withdraws the button
    session$setInputs(addDoseDrug = "fentanyl")
    expect_false(any(grepl("stopAdministrationBtn", as.character(output$stopAdministrationUI$html))))
    session$setInputs(addDoseDrug = "propofol")

    session$setInputs(stopAdministrationBtn = 1)
    dt <- doseTable()
    filled <- dt[nzchar(dt$Drug), ]
    expect_equal(filled$Time, c("0", "60"))
    expect_equal(filled$Dose, c("100", "0"))
    expect_equal(filled$Units, c("mcg/kg/min", "mcg/kg/min"))
    expect_equal(dt$Drug[nrow(dt)], "")

    # After the stop the edit dialog offers nothing to stop
    DrugTimeUnits(list(drug = "propofol", time = 90, units = c("", "")))
    showEditDrugModal("propofol")
    session$flushReact()
    expect_false(any(grepl("stopAdministrationBtn", as.character(output$stopAdministrationUI$html))))
    # but before it, it still does
    DrugTimeUnits(list(drug = "propofol", time = 30, units = c("", "")))
    showEditDrugModal("propofol")
    session$flushReact()
    expect_match(as.character(output$stopAdministrationUI$html), "Stop propofol at 30")
  })
})

test_that("Min time moves the start of the axis and refines the window", {
  shiny::testServer(app_server, {
    do.call(session$setInputs, stopAppInputs)
    dt <- infusion()
    dt[1, ] <- list("fentanyl", "0", "100", "mcg")
    setDoseTable(dt, timeFormat("minutes", "relative"))
    session$flushReact()
    expect_equal(plotInfo()$plotMinimum, 0)
    before <- drugs()$fentanyl$results
    session$setInputs(minimum = 180)
    session$elapse(500)
    expect_equal(plotInfo()$plotMinimum, 180)
    expect_equal(plotInfo()$plotMaximum, 240)
    # ticks for a one-hour window, at their multiples
    expect_equal(plotInfo()$steps, 10)
    after <- drugs()$fentanyl$results
    expect_gt(sum(after$Time >= 180), sum(before$Time >= 180))
    # Min time dragged up to Max time: the plot waits for the observer to
    # pull them apart rather than draw an empty window
    session$setInputs(minimum = 240)
    session$elapse(500)
    expect_error(plotInfo(), class = "shiny.silent.error")
    session$setInputs(maximum = 245)
    session$elapse(500)
    expect_equal(plotInfo()$plotMinimum, 240)
    expect_equal(plotInfo()$plotMaximum, 245)
  })
})

.sprglobals$config <- oldConfig
