# The failures of the adversarial UI review of 2026-11 (an external LLM's
# audit of stanpumpR.io; every finding below was reproduced here before it
# was fixed, and two that did not reproduce are not tested).  Each was a user
# action that ended the session ("Disconnected from the server"), left the
# page behind a spinner, or broke the plot past repair.  A Shiny session ends
# on an error raised in an observer; MockShinySession closes itself the same
# way (unhandledError()), so session$isClosed() is the test.  Drafted by
# Claude Code, 2026-11-02, at the request of Steven L. Shafer.

local_mocked_bindings(outputComments = function(...) {})

oldConfig <- .sprglobals$config
.sprglobals$config <- DEFAULT_CONFIG

reviewInputs <- list(
  timeUnits = "minutes", timeMode = "relative", referenceTime = "08:00", maximum = "240",
  plotWidth = 800, yaxisHeight = 200, plasmaLinetype = "blank", effectsiteLinetype = "solid",
  weight = 70, weightUnit = "1", height = 170, heightUnit = "1", age = 40, ageUnit = "1",
  sex = "male", showThreshold = FALSE, normalization = "none", typical = "Range", logY = FALSE
)

# A dose table of the rows given (each list(drug, time, dose, units)) and the
# trailing blank row
reviewTable <- function(...) {
  rows <- list(...)
  dt <- doseTableInit[seq_along(rows), ]
  for (i in seq_along(rows)) dt[i, ] <- rows[[i]]
  dt[nrow(dt) + 1, ] <- list("", "", "", "")
  rownames(dt) <- NULL
  dt
}

startReview <- function(session, dt) {
  do.call(session$setInputs, reviewInputs)
  session$env$setDoseTable(dt, timeFormat("minutes", "relative"))
  session$flushReact()
}

# What the browser sends for a dialog's grid (inst/www/table-grid.js, through
# the JSON Shiny reads it from): its rows, null for an empty cell, and the tag
# it was drawn with.  helpers.R: tableGridInputValue().
targetGrid <- function(...) {
  rows <- list(...)
  rows <- c(rows, rep(list(list("", "")), max(0, 6 - length(rows))))
  tableGridInputValue(rows)
}

# The tag is the drug editDosesDialogGrid() drew the grid for
editDosesGrid <- function(drug, rows) tableGridInputValue(rows, tag = drug)

recordNotices <- function(session) {
  sent <- new.env()
  sent$notifications <- list()
  sent$inputs <- list()
  session$sendNotification <- function(type, message) {
    sent$notifications[[length(sent$notifications) + 1]] <- message
  }
  session$sendInputMessage <- function(inputId, message) {
    sent$inputs[[length(sent$inputs) + 1]] <- list(id = inputId, message = message)
  }
  sent
}
errorsShown <- function(sent) {
  unlist(lapply(sent$notifications, function(n) {
    if (is.list(n) && identical(n$type, "error")) as.character(n$html) else NULL
  }))
}

# The page spinner, counted: shown and hidden must balance
local_mocked_bindings(
  showPageSpinner = function(...) spinner$shown <- spinner$shown + 1,
  hidePageSpinner = function(...) spinner$hidden <- spinner$hidden + 1,
  .package = "shinycssloaders"
)
spinner <- new.env()
resetSpinner <- function() { spinner$shown <- 0; spinner$hidden <- 0 }

test_that("Suggest Dosing says what is wrong with the target table (findings 1 and 2)", {
  shiny::testServer(app_server, {
    sent <- recordNotices(session)
    startReview(session, reviewTable(list("propofol", "0", "100", "mg")))
    resetSpinner()

    # A time with no target
    session$setInputs(targetDrug = "propofol", targetEndTime = "60",
                      targetTableHTML = targetGrid(list("0", 2), list("30", NULL)))
    session$setInputs(targetOK = 1)
    expect_false(session$isClosed())
    expect_match(tail(errorsShown(sent), 1), "needs a target concentration")
    expect_equal(spinner$shown, 0)

    # A target above the limit
    session$setInputs(targetTableHTML = targetGrid(list("0", 2e9)))
    session$setInputs(targetOK = 2)
    expect_false(session$isClosed())
    expect_match(tail(errorsShown(sent), 1), "within the permitted limit")

    # A row inserted through the context menu arrives as nulls: it is blank
    session$setInputs(targetTableHTML = targetGrid(list(NULL, NULL), list("0", 2)))
    session$setInputs(targetOK = 3)
    expect_false(session$isClosed())
    expect_equal(spinner$shown, spinner$hidden)
    expect_true(any(doseTable()$Drug == "propofol" & doseTable()$Units == "mcg/kg/min"))
  })
})

test_that("validateTargetTableInput reads a missing cell as an empty one", {
  rows <- data.frame(Time = c(NA, "0", ""), Target = c(NA, "2", ""))
  expect_true(validateTargetTableInput(rows))
  expect_null(targetTableProblem(rows))
  expect_match(targetTableProblem(data.frame(Time = "30", Target = "")), "needs a target")
  expect_match(targetTableProblem(data.frame(Time = "30", Target = NA)), "needs a target")
})

test_that("Suggest Dosing adds a drug in its own unit (finding 3)", {
  shiny::testServer(app_server, {
    sent <- recordNotices(session)
    startReview(session, reviewTable(list("propofol", "0", "100", "mg")))
    resetSpinner()
    session$setInputs(targetDrug = "vasopressin", targetEndTime = "60",
                      targetTableHTML = targetGrid(list("0", 50)))
    session$setInputs(targetOK = 1)
    expect_false(session$isClosed())
    expect_equal(spinner$shown, spinner$hidden)
    units <- doseTable()$Units[doseTable()$Drug == "vasopressin"]
    expect_gt(length(units), 0)
    expect_true(all(units %in% c("U", "U/min")))
  })
})

test_that("Edit doses refuses a blank unit, and Suggest Dosing copes with one (finding 4)", {
  shiny::testServer(app_server, {
    sent <- recordNotices(session)
    startReview(session, reviewTable(list("propofol", "0", "100", "mg"), list("fentanyl", "0", "50", "mcg")))
    DrugTimeUnits(list(drug = "fentanyl", time = 10, units = c("", "")))
    session$setInputs(editPriorDosesTable = editDosesGrid("fentanyl", list(list(FALSE, "0", "50", ""))))
    session$setInputs(editDosesOK = 1)
    expect_match(tail(errorsShown(sent), 1), "Every dose needs a unit")
    expect_equal(doseTable()$Units[doseTable()$Drug == "fentanyl"], "mcg")

    # The main grid can still leave a unit blank: the row is ignored by the
    # simulation, and Suggest Dosing adds the drug rather than stopping
    dt <- doseTable()
    dt$Units[dt$Drug == "fentanyl"] <- ""
    doseTable(dt)
    resetSpinner()
    session$setInputs(targetDrug = "fentanyl", targetEndTime = "60",
                      targetTableHTML = targetGrid(list("0", 2)))
    session$setInputs(targetOK = 1)
    expect_false(session$isClosed())
    expect_equal(spinner$shown, spinner$hidden)
    expect_true(all(nzchar(doseTable()$Units[doseTable()$Drug == "fentanyl"])))
  })
})

test_that("a row inserted in the dose grid is blank, not NA (finding 5)", {
  shiny::testServer(app_server, {
    sent <- recordNotices(session)
    dt <- reviewTable(list("propofol", "0", "100", "mg"))
    startReview(session, dt)
    # a row with nothing in it (dose-grid.js sends "", a forged message null)
    session$setInputs(doseGrid = doseGridInputValue(
      list(list(NULL, NULL, NULL, NULL), list("propofol", "0", "100", "mg"), list("", "", "", ""))
    ))
    session$flushReact()
    expect_false(anyNA(doseTableDraft()))
    doseTable(doseTableDraft())   # Apply
    session$flushReact()

    DrugTimeUnits(list(drug = "propofol", time = 10, units = c("", "")))
    session$setInputs(editPriorDosesTable = editDosesGrid("propofol", list(list(FALSE, "0", "200", "mg"))))
    session$setInputs(editDosesOK = 1)
    expect_false(session$isClosed())
    expect_equal(doseTable()$Dose[doseTable()$Drug == "propofol"], "200")
  })
  # and a table that holds one anyway is read as blank
  dt <- reviewTable(list("propofol", "0", "100", "mg"))
  dt[3, ] <- NA
  expect_equal(nrow(cleanDoseTable(dt)), 1L)
  expect_false(anyNA(blankMissingCells(dt)))
})

test_that("Edit doses applies only its own drug's grid (finding 6)", {
  shiny::testServer(app_server, {
    sent <- recordNotices(session)
    startReview(session, reviewTable(list("air", "0", "4", "L/min"), list("ventilation", "0", "5", "L/min"),
                                     list("propofol", "0", "100", "mg")))
    before <- doseTable()
    # No grid yet: nothing happens
    DrugTimeUnits(list(drug = "oxygen", time = 10, units = c("", "")))
    session$setInputs(editDosesOK = 1)
    expect_false(session$isClosed())
    expect_identical(doseTable(), before)
    # The grid left from propofol's dialog is not written as oxygen
    session$setInputs(editPriorDosesTable = editDosesGrid("propofol", list(list(FALSE, "0", "100", "mg"))))
    session$setInputs(editDosesOK = 2)
    expect_identical(doseTable(), before)
    expect_false("oxygen" %in% doseTable()$Drug)
  })
})

test_that("a dose or time beyond the table's limits is refused where it is typed (finding 7)", {
  expect_equal(validateDose(as.character(MAX_DOSE_VALUE)), "1000000000")
  expect_equal(validateDose("1000000001"), "")
  expect_equal(validateDose("1e10"), "")
  expect_equal(validateDose(1e10), "")
  expect_equal(validateTime(strrep("1", MAX_TIME_STRING_LENGTH)), strrep("1", MAX_TIME_STRING_LENGTH))
  expect_equal(validateTime(strrep("1", MAX_TIME_STRING_LENGTH + 1L)), "")
  # an hour count past 2^53, refused by the grid too
  expect_equal(validateTime("9007199254740991:00"), "9007199254740991:00")
  expect_equal(validateTime("99999999999999999999:00"), "")

  # The grid's own copy of the limits (inst/www/grid-funs.js)
  js <- readLines(system.file("www", "grid-funs.js", package = "stanpumpR"))
  limit <- function(name) {
    line <- grep(paste0("^var ", name, " = "), js, value = TRUE)
    expect_length(line, 1)
    as.numeric(sub(".*= *([0-9.e]+);.*", "\\1", line))
  }
  expect_equal(limit("MAX_DOSE_VALUE"), MAX_DOSE_VALUE)
  expect_equal(limit("MAX_TIME_STRING_LENGTH"), MAX_TIME_STRING_LENGTH)

  shiny::testServer(app_server, {
    sent <- recordNotices(session)
    startReview(session, reviewTable(list("propofol", "0", "100", "mg")))
    before <- doseTable()
    session$setInputs(addDoseDrug = "fentanyl", addDoseTime = "0", addDoseAmount = "10000000000",
                      addDoseUnits = "mcg")
    session$setInputs(addDoseBtn = 1)
    expect_identical(doseTable(), before)
    expect_match(tail(errorsShown(sent), 1), "1,000,000,000")
  })
})

test_that("Edit events refuses a blank event (finding 8)", {
  shiny::testServer(app_server, {
    sent <- recordNotices(session)
    startReview(session, reviewTable(list("propofol", "0", "100", "mg")))
    event <- eventDefaults()$Event[1]
    eventTable(data.frame(Time = 10, Event = event))
    showEditEventsModal()
    session$flushReact()
    session$setInputs(editEventsTableHTML = tableGridInputValue(list(list(FALSE, "10", ""))))
    session$setInputs(editEventsOK = 1)
    expect_false(session$isClosed())
    expect_match(tail(errorsShown(sent), 1), "event chosen from the list")
    expect_equal(eventTable()$Event, event)
  })
})

test_that("the Drug Library refuses an emptied unit (finding 9)", {
  dd <- getDrugDefaultsGlobal()
  grid <- drugLibraryGridData(dd)
  expect_length(drugLibraryProblems(grid, dd), 0)
  for (col in c("Concentration.Units", "Bolus.Units", "Infusion.Units", "Default.Units")) {
    edited <- grid
    edited[[col]][edited$Drug == "propofol"] <- ""
    expect_match(drugLibraryProblems(edited, dd), paste("propofol", col), info = col)
  }
  # a choice from the list is accepted; a default not among the drug's units is not
  edited <- grid
  edited$Concentration.Units[edited$Drug == "propofol"] <- "ng"
  expect_length(drugLibraryProblems(edited, dd), 0)
  edited$Default.Units[edited$Drug == "propofol"] <- "mcg/kg"
  expect_match(drugLibraryProblems(edited, dd), "propofol Default.Units")
  # an entry left as it was is not checked: levodopaEntacaponeIntestinalGel
  # has no bolus unit, NA from the library and "" once emptied in the grid
  expect_true(is.na(grid$Bolus.Units[grid$Drug == "levodopaEntacaponeIntestinalGel"]))
  edited <- grid
  edited$Bolus.Units[edited$Drug == "levodopaEntacaponeIntestinalGel"] <- ""
  expect_length(drugLibraryProblems(edited, dd), 0)
})

test_that("Suggest Dosing and Email Slide say why the simulation cannot run (findings 10 and 11)", {
  alerts <- character(0)
  local_mocked_bindings(shinyalert = function(title, text = "", ...) alerts <<- c(alerts, text),
                        .package = "shinyalert")
  shiny::testServer(app_server, {
    sent <- recordNotices(session)
    startReview(session, reviewTable(list("propofol", "0", "100", "mg")))
    session$setInputs(age = NA)
    resetSpinner()
    session$setInputs(targetDrug = "propofol", targetEndTime = "60",
                      targetTableHTML = targetGrid(list("0", 2)))
    session$setInputs(targetOK = 1)
    expect_false(session$isClosed())
    expect_equal(spinner$shown, 0)
    expect_match(tail(errorsShown(sent), 1), "Suggest Dosing cannot run: .*Age")

    session$setInputs(recipient = "user@example.com", emailComments = "", commentSafe = FALSE)
    session$setInputs(sendSlide = 1)
    expect_false(session$isClosed())
    expect_match(tail(alerts, 1), "Age")
    expect_equal(spinner$shown, spinner$hidden)

    # An empty dose table
    session$setInputs(age = 40)
    doseTable(doseTableBlank)
    session$flushReact()
    session$setInputs(sendSlide = 2)
    expect_false(session$isClosed())
    expect_match(tail(alerts, 1), "enter a dose first")
  })
})

test_that("a worksheet name fits Excel's 31 characters and is unique (finding 12)", {
  expect_equal(workbookSheetName("propofol PK"), "propofol PK")
  long <- workbookSheetName("levodopaEntacaponeIntestinalGel PK")
  expect_lte(nchar(long), 31)
  a <- workbookSheetName("carbamazepine (no interactions) PK")
  b <- workbookSheetName("carbamazepine (no interactions) PKX", a)
  expect_lte(nchar(b), 31)
  expect_false(tolower(a) == tolower(b))
  expect_equal(workbookSheetName("a/b[c]:d*e?f\\g"), "a_b_c__d_e_f_g")
  # openxlsx takes every drug's name and every antiseizure companion's
  wb <- openxlsx::createWorkbook()
  dd <- getDrugDefaultsGlobal()
  for (drug in c(dd$Drug, paste(dd$Drug, "(no interactions)"))) {
    expect_no_error(openxlsx::addWorksheet(wb, workbookSheetName(paste(drug, "PK"), names(wb))))
  }
})

test_that("Delete checkboxes and the thresholds' box are read safely (finding 13)", {
  expect_equal(tickedRows(c(TRUE, FALSE, NA)), c(TRUE, FALSE, FALSE))
  expect_equal(tickedRows(NULL), logical(0))
  shiny::testServer(app_server, {
    sent <- recordNotices(session)
    startReview(session, reviewTable(list("propofol", "0", "100", "mg")))
    # Apply before a dialog's grid has reported in does nothing
    for (id in c("editEventsOK", "drugEditsOK", "thresholdEditsOK")) {
      do.call(session$setInputs, stats::setNames(list(1), id))
      expect_false(session$isClosed(), info = id)
    }
    # The box left from an earlier dialog does not change Show time until
    # threshold when this dialog, under normalization, had none
    session$setInputs(normalization = "MEAC", showThresholdModal = TRUE)
    showModalCount <- length(sent$inputs)
    shown <- thresholdTableForDisplay(drugDefaults(), 40, macThreshold())
    session$setInputs(editThresholdsTable = tableGridInputValue(
      lapply(seq_len(nrow(shown)), function(i) list(shown$Drug[i], format(shown$Threshold[i])))
    ))
    session$setInputs(thresholdEditsOK = 1)
    ids <- vapply(sent$inputs[seq_along(sent$inputs) > showModalCount], `[[`, character(1), "id")
    expect_false("showThreshold" %in% ids)
  })
})

# ChatGPT's fourth review of PR #683 (2026-11-06), each reproduced there in a
# browser with Shiny.setInputValue(): a dialog's Apply stored a table its grid
# could never have sent.  The grids keep these limits in the browser; the
# server must keep them too, since every value from the browser is untrusted.
test_that("Edit doses refuses a table past the dose table's limit (fourth review, F02)", {
  shiny::testServer(app_server, {
    sent <- recordNotices(session)
    startReview(session, reviewTable(list("propofol", "0", "100", "mg"), list("fentanyl", "0", "50", "mcg")))
    before <- doseTable()
    DrugTimeUnits(list(drug = "propofol", time = 0, units = c("", "")))
    many <- function(n) editDosesGrid("propofol", lapply(seq_len(n), function(i) list(FALSE, as.character(i), "1", "mg")))
    # 600 rows, cut to the 499 the reader takes
    session$setInputs(editPriorDosesTable = many(600), editDosesOK = 1)
    expect_false(session$isClosed())
    expect_identical(doseTable(), before)
    expect_match(tail(errorsShown(sent), 1), "at most 499 doses")
    # 499 of propofol's, which with fentanyl's makes 500
    session$setInputs(editPriorDosesTable = many(MAX_DOSE_ROWS - 1L), editDosesOK = 2)
    expect_identical(doseTable(), before)
    # 498 fit, and the table stays within MAX_DOSE_ROWS with its blank row
    session$setInputs(editPriorDosesTable = many(MAX_DOSE_ROWS - 2L), editDosesOK = 3)
    expect_equal(sum(doseTable()$Drug == "propofol"), MAX_DOSE_ROWS - 2L)
    expect_lte(nrow(doseTable()), MAX_DOSE_ROWS)
    expect_true(validateDoseTableInput(doseTable()))
  })
})

test_that("Edit events refuses more than MAX_EVENT_ROWS events (fourth review, F04)", {
  shiny::testServer(app_server, {
    sent <- recordNotices(session)
    startReview(session, reviewTable(list("propofol", "0", "100", "mg")))
    event <- eventDefaults()$Event[1]
    eventTable(data.frame(Time = 10, Event = event))
    showEditEventsModal()
    session$flushReact()
    rows <- function(n) tableGridInputValue(lapply(seq_len(n), function(i) list(FALSE, as.character(i), event)))
    session$setInputs(editEventsTableHTML = rows(MAX_EVENT_ROWS + 1L), editEventsOK = 1)
    expect_false(session$isClosed())
    expect_equal(eventTable()$Time, 10)
    expect_match(tail(errorsShown(sent), 1), "at most 100 events")
    session$setInputs(editEventsTableHTML = rows(MAX_EVENT_ROWS), editEventsOK = 2)
    expect_equal(nrow(eventTable()), MAX_EVENT_ROWS)
  })
})

test_that("the Drug Library keeps its drugs, all of them, in order (fourth review, F03)", {
  dd <- getDrugDefaultsGlobal()
  grid <- drugLibraryGridData(dd)
  expect_length(drugLibraryProblems(grid, dd), 0)
  refused <- "cannot add, remove or rename drugs"
  expect_match(drugLibraryProblems(grid[grid$Drug == "propofol", ], dd), refused)
  renamed <- grid
  renamed$Drug[1] <- "<img src=x onerror=alert(1)>"
  expect_match(drugLibraryProblems(renamed, dd), refused)
  expect_match(drugLibraryProblems(grid[rev(seq_len(nrow(grid))), ], dd), refused)
  expect_match(drugLibraryProblems(rbind(grid, grid[1, ]), dd), refused)
  # a refused entry is quoted back, but not at any length
  long <- grid
  long$Color[long$Drug == "propofol"] <- strrep("x", 200000)
  expect_lt(nchar(paste(drugLibraryProblems(long, dd), collapse = "")), 500)

  shiny::testServer(app_server, {
    sent <- recordNotices(session)
    startReview(session, reviewTable(list("propofol", "0", "100", "mg")))
    before <- drugDefaults()
    shown <- drugLibraryGridData(before)
    cells <- function(x) lapply(seq_len(nrow(x)), function(i) lapply(x[i, ], function(v) if (is.na(v)) "" else as.character(v)))
    session$setInputs(editDrugsHTML = tableGridInputValue(cells(shown[shown$Drug == "propofol", ])), drugEditsOK = 1)
    expect_identical(drugDefaults(), before)
    expect_match(tail(errorsShown(sent), 1), refused)
    # one row too many is cut by the reader, and refused
    session$setInputs(editDrugsHTML = tableGridInputValue(cells(rbind(shown, shown[1, ]))), drugEditsOK = 2)
    expect_identical(drugDefaults(), before)
    expect_match(tail(errorsShown(sent), 1), "cannot add drugs")
    expect_false(session$isClosed())
  })
})

test_that("a threshold past the grid's ceiling is refused (fourth review, F05)", {
  dd <- getDrugDefaultsGlobal()
  over <- thresholdTableToDefaults(data.frame(Drug = "propofol", Threshold = "10000000000"), dd, 40)
  expect_equal(over$invalid, "propofol")
  expect_identical(over$drugDefaults, dd)
  at <- thresholdTableToDefaults(data.frame(Drug = "propofol", Threshold = format(MAX_DOSE_VALUE, scientific = FALSE)), dd, 40)
  expect_length(at$invalid, 0)
  expect_equal(at$drugDefaults$endCe[at$drugDefaults$Drug == "propofol"], MAX_DOSE_VALUE)
})

# Devin's stress test (2026-11-08), on Shafer's decision: a Visualizer button
# beside Add new drug.  The drug it names comes from the browser, so it is
# untrusted like every input: one name of a drug the Visualizer can draw now.
test_that("the plot's Visualizer button lists the drawable drugs, and a forged drug is refused", {
  expect_null(viewerPlotButton(character(0)))
  one <- as.character(viewerPlotButton("propofol"))
  expect_match(one, 'data-viewer-drug="propofol"')
  expect_false(grepl("dropdown", one))
  two <- as.character(viewerPlotButton(c("propofol", "fentanyl")))
  expect_match(two, "dropup")
  expect_match(two, 'data-viewer-drug="fentanyl"')
  shiny::testServer(app_server, {
    sent <- recordNotices(session)
    messages <- list()
    session$sendCustomMessage <- function(type, message) messages[[length(messages) + 1]] <<- list(type = type, message = message)
    startReview(session, reviewTable(list("propofol", "0", "140", "mg"), list("fentanyl", "0", "100", "mcg")))
    html <- as.character(output$viewerButtonUI$html)
    expect_match(html, 'data-viewer-drug="propofol"')
    expect_match(html, 'data-viewer-drug="fentanyl"')
    # Forged: not a list, two drugs, a drug not drawn
    session$setInputs(openViewerDrug = "propofol")
    session$setInputs(openViewerDrug = list(token = "1", drug = c("propofol", "fentanyl")))
    expect_match(tail(errorsShown(sent), 1), "cannot be drawn now")
    session$setInputs(openViewerDrug = list(token = "2", drug = "<b>notadrug</b>"))
    expect_match(tail(errorsShown(sent), 1), "cannot be drawn now")
    expect_false(session$isClosed())
    # A drawable drug: its run goes to the pop-up, with the click's token.
    session$setInputs(openViewerDrug = list(token = "3", drug = "fentanyl"))
    run <- Filter(function(m) identical(m$type, "stanpumpR-viewer-run"), messages)
    expect_length(run, 1)
    expect_equal(run[[1]]$message$token, "3")
    expect_equal(run[[1]]$message$run$drug, "fentanyl")
  })
})

# Gemini's production audit of the new grids (2026-11-08), each confirmed in
# the code before it was fixed.  Findings 1, 5 and 7 are the browser's
# (tests/playwright), finding 3 too, the test server making no bookmark.
libraryCells <- function(x) lapply(seq_len(nrow(x)), function(i)
  lapply(x[i, ], function(v) if (is.na(v)) "" else as.character(v)))

test_that("a blank MEAC is no MEAC, and the simulation still runs (production audit, finding 2)", {
  shiny::testServer(app_server, {
    sent <- recordNotices(session)
    startReview(session, reviewTable(list("fentanyl", "0", "100", "mcg")))
    shown <- drugLibraryGridData(drugDefaults())
    for (col in c("MEAC", "Lower", "Upper", "Typical")) shown[[col]][shown$Drug == "fentanyl"] <- ""
    session$setInputs(editDrugsHTML = tableGridInputValue(libraryCells(shown)), drugEditsOK = 1)
    expect_equal(drugDefaults()$MEAC[drugDefaults()$Drug == "fentanyl"], 0)
    expect_no_error(drugs())
    expect_false(session$isClosed())
  })
  # and simCpCe() itself takes a missing MEAC as none
  original <- getDrugDefaults
  local_mocked_bindings(getDrugDefaults = function(drug) {
    d <- original(drug)
    d$MEAC <- NA_real_
    d
  })
  sim <- simulateDrugsWithCovariates(data.frame(Drug = "fentanyl", Time = 0, Dose = 100, Units = "mcg"),
                                     eventTableInit, 70, 170, 40, "male", 60, FALSE)
  expect_true(all(sim$fentanyl$equiSpace$MEAC == 0))
})

test_that("the Drug Library's numbers are number cells, and Class cannot be edited (production audit, finding 4)", {
  dd <- getDrugDefaultsGlobal()
  cols <- drugLibraryGridColumns(drugLibraryGridData(dd))
  types <- stats::setNames(vapply(cols, `[[`, "", "type"), vapply(cols, `[[`, "", "name"))
  expect_equal(unname(types[c("Lower", "Upper", "Typical", "MEAC")]), rep("number", 4))
  expect_equal(unname(types["Class"]), "readonly")
  grid <- drugLibraryGridData(dd)
  over <- grid
  over$Lower[over$Drug == "propofol"] <- "10000000000"
  expect_match(drugLibraryProblems(over, dd), "from 0 to 1,000,000,000")
  shiny::testServer(app_server, {
    sent <- recordNotices(session)
    startReview(session, reviewTable(list("propofol", "0", "100", "mg")))
    shown <- drugLibraryGridData(drugDefaults())
    shown$Class[shown$Drug == "propofol"] <- "gas"
    shown$Class[shown$Drug == "sevoflurane"] <- ""
    session$setInputs(editDrugsHTML = tableGridInputValue(libraryCells(shown)), drugEditsOK = 1)
    expect_true(is.na(drugDefaults()$Class[drugDefaults()$Drug == "propofol"]) ||
                  drugDefaults()$Class[drugDefaults()$Drug == "propofol"] != "gas")
    expect_equal(drugDefaults()$Class[drugDefaults()$Drug == "sevoflurane"], "gas")
  })
})

test_that("a cut table or an unknown drug is refused in the targets and thresholds (production audit, finding 6)", {
  dd <- getDrugDefaultsGlobal()
  # a row naming no drug of the library is not read, nor quoted back
  forged <- thresholdTableToDefaults(data.frame(Drug = c("propofol", "<b>notadrug</b>"),
                                                Threshold = c("2", "bad")), dd, 40)
  expect_length(forged$invalid, 0)
  expect_equal(forged$drugDefaults$endCe[forged$drugDefaults$Drug == "propofol"], 2)
  shiny::testServer(app_server, {
    sent <- recordNotices(session)
    startReview(session, reviewTable(list("propofol", "0", "100", "mg")))
    before <- drugDefaults()
    rows <- rep(list(list("propofol", "3")), MAX_DOSE_ROWS)
    session$setInputs(editThresholdsTable = tableGridInputValue(rows), thresholdEditsOK = 1)
    expect_identical(drugDefaults(), before)
    expect_match(tail(errorsShown(sent), 1), "more rows than the library has drugs")
    dt <- doseTable()
    session$setInputs(setTarget = 1)
    session$setInputs(targetDrug = "propofol", targetEndTime = "60",
                      targetTableHTML = targetGrid(), targetOK = 1)
    session$setInputs(targetTableHTML = tableGridInputValue(rep(list(list("0", "3")), MAX_TARGET_ROWS + 1L)),
                      targetOK = 2)
    expect_identical(doseTable(), dt)
    expect_match(tail(errorsShown(sent), 1), "at most 20 targets")
    expect_false(session$isClosed())
  })
})

# ChatGPT's production audit of the new grids (2026-11-08, F02): a forged pair
# of End times made an if() of length 2 in the OK handler, which ended the
# session; a forged drug list fared no better.
test_that("Suggest Dosing reads its End time and drug as one value each (ChatGPT's production audit, F02)", {
  shiny::testServer(app_server, {
    sent <- recordNotices(session)
    startReview(session, reviewTable(list("propofol", "0", "100", "mg")))
    before <- doseTable()
    session$setInputs(targetDrug = "propofol", targetEndTime = c("60", "70"),
                      targetTableHTML = targetGrid(list("0", 2)), targetOK = 1)
    expect_false(session$isClosed())
    expect_identical(doseTable(), before)
    session$setInputs(targetDrug = c("propofol", "fentanyl"), targetEndTime = "60", targetOK = 2)
    expect_false(session$isClosed())
    expect_match(tail(errorsShown(sent), 1), "Choose a drug from the list")
    session$setInputs(targetDrug = "notadrug", targetOK = 3)
    expect_match(tail(errorsShown(sent), 1), "Choose a drug from the list")
    expect_identical(doseTable(), before)
  })
})

.sprglobals$config <- oldConfig
