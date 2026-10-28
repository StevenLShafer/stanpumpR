# "Skip next dose" in the plot's dose dialogs (R/app_server.R; the table
# rewrite is skipNextScheduledDose() in R/scheduled.R, tested in
# test-scheduled.R).  Drafted by Claude Code, 2026-10-24, at the request of
# Steven L. Shafer.  Driven with shiny::testServer(), as
# test-time-units-server.R.

local_mocked_bindings(outputComments = function(...) {})

oldConfig <- .sprglobals$config
.sprglobals$config <- DEFAULT_CONFIG

skipAppInputs <- list(
  timeUnits = "minutes", timeMode = "clock", referenceTime = "08:00", maximum = "1440",
  plotWidth = 800, yaxisHeight = 200, plasmaLinetype = "blank", effectsiteLinetype = "solid",
  weight = 70, weightUnit = "1", height = 170, heightUnit = "1", age = 40, ageUnit = "1",
  sex = "male", showThreshold = FALSE, normalization = "none", typical = "Range", logY = FALSE
)

oralSchedule <- function() {
  dt <- doseTableInit
  dt[1, ] <- list("carbamazepine", "0", "200", "mg PO tid")
  dt[2:6, ] <- list("", "", "", "")
  dt
}

test_that("the dose dialogs offer Skip next dose only while an oral schedule runs", {
  shiny::testServer(app_server, {
    do.call(session$setInputs, skipAppInputs)
    startFormatFromInputs(session)
    doseTable(oralSchedule())
    session$flushReact()
    # Minutes on the clock, the app's defaults: the procedure starts at 08:00
    expect_equal(doseTableFormat(), timeFormat("minutes", "clock"))

    # A click at 5 h on carbamazepine: the next dose is the tid repeat at 8 h
    showAddDrugModal("carbamazepine", 300)
    session$flushReact()
    expect_match(as.character(output$skipNextDoseUI$html), "Skip next dose \\(16:00\\)")

    # Moving the dialog to a drug with no oral schedule withdraws the button
    session$setInputs(addDoseDrug = "propofol")
    expect_false(any(grepl("skipNextDoseBtn", as.character(output$skipNextDoseUI$html))))
    session$setInputs(addDoseDrug = "carbamazepine")

    session$setInputs(skipNextDoseBtn = 1)
    dt <- doseTable()
    filled <- dt[nzchar(dt$Drug), ]
    expect_equal(filled$Time, c("0", "16:00", "00:00"))
    expect_equal(filled$Dose, c("200", "0", "200"))
    expect_equal(filled$Units, c("mg PO tid", "mg PO tid", "mg PO tid"))
    expect_equal(dt$Drug[nrow(dt)], "")
    # The simulation gives every tid dose but the one at 8 h
    session$flushReact()
    given <- doseTableClean()
    expect_equal(sort(as.numeric(given$Time)), c(0, 480, 960))

    # The edit dialog (a double click) offers the next one, at 16 h (midnight)
    DrugTimeUnits(list(drug = "carbamazepine", time = 600, units = c("", "")))
    showEditDrugModal("carbamazepine")
    session$flushReact()
    expect_match(as.character(output$skipNextDoseUI$html), "Skip next dose \\(00:00\\)")
  })
})

.sprglobals$config <- oldConfig
