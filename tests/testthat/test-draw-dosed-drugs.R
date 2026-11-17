# Only the drugs given a dose above 0 are drawn (Steven L. Shafer,
# 2026-11-16).  A table of every drug of the startup menu, each at 0 until a
# dose is typed, took 51 s to draw, 35 s of it ggplot drawing 231 empty
# panels (ChatGPT's production audit, 2026-11-15).  A drug whose every row is
# 0 is now not simulated or drawn, and a metabolite a drawn parent forms is
# drawn whatever its own rows say.  Drafted by Claude Code, 2026-11-16.

local_mocked_bindings(outputComments = function(...) {})

oldConfig <- .sprglobals$config
.sprglobals$config <- DEFAULT_CONFIG

G <- getDrugDefaultsGlobal()

drawInputs <- list(
  timeUnits = "minutes", timeMode = "relative", referenceTime = "08:00", maximum = "600",
  plotWidth = 800, yaxisHeight = 200, plasmaLinetype = "solid", effectsiteLinetype = "solid",
  weight = 70, weightUnit = "1", height = 170, heightUnit = "1", age = 50, ageUnit = "1",
  sex = "male", showThreshold = FALSE, normalization = "none", typical = "Range", logY = FALSE,
  bayesPKVariability = 25, bayesResidualVariability = 5
)
drawStart <- function(session, dt) {
  do.call(session$setInputs, drawInputs)
  session$env$setDoseTable(dt, timeFormat("minutes", "relative"))
  session$flushReact()
}
drawPK <- function(DT, ...) {
  recalculatePK(NULL, G, DT, age = 50, weight = 70, height = 170, sex = "male", ...)
}
plottedDrugs <- function(retval) unique(as.character(retval$plotResults$Drug))

test_that("the drugs given a dose above 0", {
  expect_identical(positiveDoseDrugs(NULL), character(0))
  DT <- data.frame(Drug = c("propofol", "propofol", "fentanyl", "rocuronium", "remifentanil"),
                   Time = c(0, 10, 0, 0, 0), Dose = c("0", "2", "0", NA, "abc"),
                   Units = c("mg", "mg", "mcg", "mg", "mcg"), stringsAsFactors = FALSE)
  expect_identical(positiveDoseDrugs(DT), "propofol")
  DT$Dose <- c(0, 0, 50, NA, 0)
  expect_identical(positiveDoseDrugs(DT), "fentanyl")
  expect_identical(positiveDoseDrugs(DT[0, ]), character(0))
})

test_that("a drug whose doses are all 0 is not drawn; a metabolite a drawn parent forms is", {
  DT <- data.frame(Drug = c("codeine", "morphine", "fentanyl"), Time = 0,
                   Dose = c(30, 0, 0), Units = c("mg PO", "mg", "mcg"), stringsAsFactors = FALSE)
  drugs <- drawPK(DT)
  expect_setequal(names(drawnDrugEntries(drugs, positiveDoseDrugs(DT))), c("codeine", "morphine"))
  # The parent at 0 too: nothing is drawn
  DT$Dose[1] <- 0
  expect_length(drawnDrugEntries(drugs, positiveDoseDrugs(DT)), 0)
  # A companion is drawn with its drug, and only with it
  DT2 <- data.frame(Drug = c("propofol", "remifentanil"), Time = 0, Dose = c(100, 0),
                    Units = c("mg", "mcg"), stringsAsFactors = FALSE)
  drugs2 <- drawPK(DT2, alternativeModels = TRUE)
  expect_true(all(c("propofol (Schnider)", "remifentanil (Minto)") %in% names(drugs2)))
  expect_setequal(names(drawnDrugEntries(drugs2, positiveDoseDrugs(DT2))),
                  c("propofol", "propofol (Schnider)"))
  expect_length(drawnDrugEntries(NULL, "propofol"), 0)
})

test_that("the opening table's drugs at 0 draw nothing and say so; a dose draws its drug alone", {
  shiny::testServer(app_server, {
    drawStart(session, doseTableInit)
    expect_length(drugs(), 0)
    expect_identical(plotMessage(), NOTHING_TO_DRAW_MESSAGE)
    # No plot, and no error from building one ("replacement has 1 row, data
    # has 0" stopped simulationPlot() on an empty list)
    expect_null(simulationPlotRetval())
    expect_null(plotResultsOrNull()$plot)

    dt <- doseTableInit
    dt$Dose[1] <- "100"
    session$env$setDoseTable(dt, timeFormat("minutes", "relative"))
    session$flushReact()
    expect_identical(names(drugs()), "propofol")
    expect_null(plotMessage())
    expect_identical(plottedDrugs(simulationPlotRetval()), "propofol")

    # An empty table says the same
    session$env$setDoseTable(doseTableBlank, timeFormat("minutes", "relative"))
    session$flushReact()
    expect_identical(plotMessage(), NOTHING_TO_DRAW_MESSAGE)
  })
})

test_that("a table of every startup drug at 0 builds no PK and draws nothing (ChatGPT's audit)", {
  every <- unlist(startupDrugChoices(G), use.names = FALSE)
  shiny::testServer(app_server, {
    drawStart(session, startupDoseTable(every, G))
    expect_gt(length(unique(doseTableClean()$Drug)), 200)
    elapsed <- system.time(d <- drugs())[["elapsed"]]
    expect_length(d, 0)
    expect_length(bayesUpdate()$drugs, 0)
    expect_identical(plotMessage(), NOTHING_TO_DRAW_MESSAGE)
    # recalculatePK() alone took 2 s on these drugs, processdoseTable() 2.7 s,
    # and the plot 35 s
    expect_lt(elapsed, 1)

    # One dose among them draws that drug alone
    dt <- startupDoseTable(every, G)
    dt$Dose[dt$Drug == "fentanyl"] <- "100"
    session$env$setDoseTable(dt, timeFormat("minutes", "relative"))
    session$flushReact()
    expect_identical(names(drugs()), "fentanyl")
    expect_identical(plottedDrugs(simulationPlotRetval()), "fentanyl")
  })
})

test_that("a measured metabolite with a 0 mg row beside its parent is still updated and drawn", {
  shiny::testServer(app_server, {
    dt <- doseTableInit
    dt[1:2, ] <- list(c("codeine", "morphine"), "0", c("60", "0"), c("mg PO", "mg"))
    dt[3:6, ] <- list("", "", "", "")
    drawStart(session, dt)
    expect_setequal(names(drugs()), c("codeine", "morphine"))

    session$setInputs(bayesTable = list(
      rows = list(list("morphine", "120", "0.004"), list("morphine", "240", "0.003"),
                  list("", "", "")),
      tag = "minutes/relative", seq = 1))
    session$elapse(2000)
    fit <- bayesUpdate()$fits$morphine
    expect_false(is.null(fit))
    expect_setequal(names(drugs()), c("codeine", "morphine", "morphine (population)"))
    expect_equal(drugs()$morphine$bayesian$clearance, fit$clearance)
    expect_true(nrow(drugs()[["morphine (population)"]]$results) > 0)
    expect_setequal(plottedDrugs(simulationPlotRetval()),
                    c("codeine", "morphine", "morphine (population)"))

    # A measured drug with no dose above 0 and no parent drawn is said, not
    # drawn
    dt[3, ] <- list("fentanyl", "0", "0", "mcg")
    session$env$setDoseTable(dt, timeFormat("minutes", "relative"))
    session$setInputs(bayesTable = list(
      rows = list(list("fentanyl", "120", "1"), list("", "", "")),
      tag = "minutes/relative", seq = 2))
    session$elapse(2000)
    expect_match(paste(bayesUpdate()$problems, collapse = " "),
                 "fentanyl has no dose above 0 yet", fixed = TRUE)
    expect_false("fentanyl" %in% names(drugs()))
  })
})

test_that("Suggest Dosing works for a drug in the table whose doses are all 0", {
  # The opening table's propofol is at 0, so not in drugs(); Suggest Dosing
  # read it there for any drug in the table
  shiny::testServer(app_server, {
    drawStart(session, doseTableInit)
    expect_false("propofol" %in% names(drugs()))
    errors <- character(0)
    session$sendNotification <- function(type, message) {
      if (is.list(message) && identical(message$type, "error"))
        errors <<- c(errors, as.character(message$html))
    }
    session$setInputs(targetDrug = "propofol", targetEndTime = "60",
                      targetTableHTML = tableGridInputValue(
                        c(list(list("0", 3)), rep(list(list("", "")), 5))))
    session$setInputs(targetOK = 1)
    expect_false(session$isClosed())
    expect_length(errors, 0)
    dt <- doseTable()
    expect_true(any(dt$Drug == "propofol" & suppressWarnings(as.numeric(dt$Dose)) > 0))
    session$flushReact()
    expect_true("propofol" %in% names(drugs()))
  })
})

.sprglobals$config <- oldConfig
