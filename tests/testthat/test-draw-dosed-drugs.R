# Every drug in the dose table is drawn, at 0 too (Steven L. Shafer,
# 2026-11-19, rescinding his rule of 2026-11-16 that drew only the drugs
# given a dose above 0: a handful of drugs chosen at the startup menu opened
# on an empty plot, which on a phone looked like a failure).  The plot area
# says NOTHING_TO_DRAW_MESSAGE only for a table with no drug.  Drafted by
# Claude Code, 2026-11-16; revised 2026-11-19.

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

test_that("the drugs of a dose table, every one drawn", {
  expect_identical(tableDrugs(NULL), character(0))
  DT <- data.frame(Drug = c("propofol", "propofol", "fentanyl", "", NA),
                   Time = 0, Dose = c("0", "2", "0", "1", "1"),
                   Units = "mg", stringsAsFactors = FALSE)
  expect_identical(tableDrugs(DT), c("propofol", "fentanyl"))
  expect_identical(tableDrugs(DT[0, ]), character(0))
})

test_that("a drug whose doses are all 0 is drawn, with the metabolite it forms and its companions", {
  DT <- data.frame(Drug = c("codeine", "fentanyl"), Time = 0,
                   Dose = c(0, 0), Units = c("mg PO", "mcg"), stringsAsFactors = FALSE)
  drugs <- drawPK(DT)
  expect_setequal(names(drawnDrugEntries(drugs, tableDrugs(DT))), c("codeine", "morphine", "fentanyl"))
  DT2 <- data.frame(Drug = c("propofol", "remifentanil"), Time = 0, Dose = c(100, 0),
                    Units = c("mg", "mcg"), stringsAsFactors = FALSE)
  drugs2 <- drawPK(DT2, alternativeModels = TRUE)
  expect_setequal(names(drawnDrugEntries(drugs2, tableDrugs(DT2))),
                  c("propofol", "propofol (Schnider)", "remifentanil", "remifentanil (Minto)"))
  expect_length(drawnDrugEntries(NULL, "propofol"), 0)
})

test_that("the opening table's drugs at 0 are each drawn, flat; an empty table says why it is empty", {
  shiny::testServer(app_server, {
    drawStart(session, doseTableInit)
    opening <- unique(doseTableInit$Drug[nzchar(doseTableInit$Drug)])
    expect_setequal(names(drugs()), opening)
    expect_null(plotMessage())
    expect_setequal(plottedDrugs(simulationPlotRetval()), opening)
    for (d in opening) {
      w <- drugs()[[d]]$results
      expect_true(nrow(w) > 0)
      expect_true(all(w$Y[w$Site %in% c("Plasma", "Effect Site")] == 0))
    }

    # Deleting a drug's doses removes its panel (Edit doses, Delete All
    # Doses, without asking while every dose is 0)
    session$env$deleteDrugDoses(opening[1])
    session$flushReact()
    expect_false(opening[1] %in% names(drugs()))

    # An empty table says the plot area is empty because the table is
    session$env$setDoseTable(doseTableBlank, timeFormat("minutes", "relative"))
    session$flushReact()
    expect_identical(plotMessage(), NOTHING_TO_DRAW_MESSAGE)
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

    # A measured drug with no dose above 0 and no parent is drawn flat, and
    # its levels are said to have no dose to explain them
    dt[3, ] <- list("fentanyl", "0", "0", "mcg")
    session$env$setDoseTable(dt, timeFormat("minutes", "relative"))
    session$setInputs(bayesTable = list(
      rows = list(list("fentanyl", "120", "1"), list("", "", "")),
      tag = "minutes/relative", seq = 2))
    session$elapse(2000)
    expect_match(paste(bayesUpdate()$problems, collapse = " "),
                 "fentanyl has no dose above 0 yet", fixed = TRUE)
    expect_true("fentanyl" %in% names(drugs()))
  })
})

test_that("Suggest Dosing works for a drug in the table whose doses are all 0", {
  # The opening table's propofol is at 0 (drawn flat since 2026-11-19; until
  # then not in drugs(), where Suggest Dosing read it)
  shiny::testServer(app_server, {
    drawStart(session, doseTableInit)
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
