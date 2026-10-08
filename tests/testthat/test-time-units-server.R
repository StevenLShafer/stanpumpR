# Time units in the running app (R/app_server.R, "Time units"): the dose
# table's format record, the conversion observer, the grid's stamp, scenarios,
# the Max time list, and the rule for TCI and inhaled agents.  Drafted by
# Claude Code, 2026-10-07, at the request of Steven L. Shafer.
#
# Driven with shiny::testServer().  session$setInputs() plays the browser,
# including its echo of the server's own updates to the selectors, which a
# recorder (recordMessages()) captures from the session.

local_mocked_bindings(outputComments = function(...) {})

oldConfig <- .sprglobals$config
.sprglobals$config <- DEFAULT_CONFIG

# The inputs a browser reports when the app opens, as app_ui() builds them
appInputs <- function(...) {
  utils::modifyList(list(
    timeUnits = "minutes", timeMode = "clock", referenceTime = "08:00", maximum = "60",
    plotWidth = 800, yaxisHeight = 200, plasmaLinetype = "blank", effectsiteLinetype = "solid",
    weight = 70, weightUnit = "1", height = 170, heightUnit = "1", age = 40, ageUnit = "1",
    sex = "male", showThreshold = FALSE, normalization = "none", typical = "Range", logY = FALSE
  ), list(...))
}

# Record what the server sends: input updates, notifications and dialogs
recordMessages <- function(session) {
  sent <- new.env()
  sent$inputs <- list()
  sent$notifications <- list()
  sent$modals <- list()
  session$sendInputMessage <- function(inputId, message) {
    sent$inputs[[length(sent$inputs) + 1]] <- list(id = inputId, message = message)
  }
  session$sendNotification <- function(type, message) {
    sent$notifications[[length(sent$notifications) + 1]] <- list(type = type, message = message)
  }
  session$sendModal <- function(type, message) {
    sent$modals[[length(sent$modals) + 1]] <- list(type = type, message = message)
  }
  sent
}

# The last value the server sent for an input, or NULL
lastSent <- function(sent, id) {
  hits <- Filter(function(m) identical(m$id, id), sent$inputs)
  if (length(hits) == 0) NULL else hits[[length(hits)]]$message
}

# The browser's echo: report every input the server has updated since
# `since`, as the browser does
echo <- function(session, sent, since = 0) {
  new <- sent$inputs[seq_along(sent$inputs) > since]
  values <- list()
  for (m in new) if (!is.null(m$message$value)) values[[m$id]] <- m$message$value
  if (length(values)) do.call(session$setInputs, values)
}

# The notifications shown or removed with this id (a removal's message is
# the id alone); notified() is the last of them, or NULL
notifications <- function(sent, id) {
  idOf <- function(n) if (is.list(n$message)) n$message$id else n$message
  Filter(function(n) identical(idOf(n), id), sent$notifications)
}
notified <- function(sent, id) {
  hits <- notifications(sent, id)
  if (length(hits) == 0) NULL else hits[[length(hits)]]
}
anyErrorShown <- function(sent) {
  any(vapply(sent$notifications, function(n) is.list(n$message) && identical(n$message$type, "error"),
             logical(1)))
}

# Open the app as the browser does (the UI's defaults: minutes, clock), then
# make the user's changes.  Inputs set in the same call as the first ones
# would be initial values, which the conversion observer does not see.
startApp <- function(session, ...) {
  do.call(session$setInputs, appInputs())
  changes <- list(...)
  if (length(changes)) do.call(session$setInputs, changes)
}

withTimes <- function(...) {
  dt <- doseTableInit
  times <- c(...)
  dt$Time[seq_along(times)] <- times
  dt
}

test_that("a change of unit converts the table, keeps the minutes and starts a fresh history", {
  shiny::testServer(app_server, {
    sent <- recordMessages(session)
    startApp(session)
    doseTable(withTimes("90", "09:30", "1.5"))
    session$flushReact()
    before <- doseTableClean()

    session$setInputs(timeUnits = "hours")
    expect_equal(doseTable()$Time[1:3], c("1.5", "09:30", "0.025"))
    expect_equal(doseTableFormat(), timeFormat("hours", "clock"))
    expect_identical(doseTableDraft(), doseTable())
    expect_false(doseTableHistory()$can_undo)
    # The engine sees the same minutes, so nothing is re-simulated
    expect_identical(doseTableClean(), before)
    expect_match(notified(sent, "timeConverted")$message$html, "converted to hours")
    # minutes and hours share their Max times: 1 hour is kept
    expect_equal(lastSent(sent, "maximum")$value, "60")
    echo(session, sent)
    expect_equal(plotInfo()$plotMaximum, 120)   # lengthened past the dose at 90
    expect_equal(plotInfo()$steps, 15)

    # and back: every string as it was
    session$setInputs(timeUnits = "minutes")
    expect_equal(doseTable()$Time[1:3], c("90", "09:30", "1.5"))
    expect_identical(doseTableClean(), before)
  })
})

test_that("unapplied edits are applied, converted, by the switch", {
  shiny::testServer(app_server, {
    sent <- recordMessages(session)
    startApp(session, timeMode = "relative")
    doseTable(withTimes("0"))
    session$flushReact()
    doseTableHistory()$do(withTimes("120"))
    session$setInputs(timeUnits = "hours")
    expect_equal(doseTable()$Time[1], "2")
    expect_identical(doseTableDraft(), doseTable())
  })
})

test_that("clock times become elapsed times; days and weeks force elapsed", {
  shiny::testServer(app_server, {
    sent <- recordMessages(session)
    startApp(session, timeUnits = "hours")
    doseTable(withTimes("09:30", "2"))
    session$flushReact()
    expect_equal(doseTableFormat(), timeFormat("hours", "clock"))
    session$setInputs(timeMode = "relative")
    expect_equal(doseTable()$Time[1:2], c("1.5", "2"))
    expect_match(notified(sent, "timeConverted")$message$html, "Clock times converted to elapsed hours")

    session$setInputs(timeMode = "clock")
    session$setInputs(timeUnits = "days")
    # the browser still says clock; the server says elapsed, the only choice
    expect_equal(doseTableFormat(), timeFormat("days", "relative"))
    mode <- lastSent(sent, "timeMode")
    expect_equal(mode$value, "relative")
    expect_false(grepl("clock", paste(unlist(mode$options), collapse = " ")))
    expect_equal(doseTable()$Time[1:2], c("0.0625", "0.08333333333"))
    # Max time: 1 hour is not a choice in days; the shortest is
    expect_equal(lastSent(sent, "maximum")$value, maxTimeValue(2 * MINS_PER_DAY))
    n <- length(sent$inputs)
    echo(session, sent)
    expect_equal(doseTable()$Time[1:2], c("0.0625", "0.08333333333"))  # the echo converts nothing
    expect_equal(plotInfo()$plotMaximum, 2 * MINS_PER_DAY)
    expect_equal(plotInfo()$steps, MINS_PER_DAY / 2)
    # back to hours: both choices again
    session$setInputs(timeUnits = "hours")
    expect_true(grepl("clock", paste(unlist(lastSent(sent, "timeMode")$options), collapse = " ")))
  })
})

test_that("a switch that cannot read the clock times is refused and put back", {
  shiny::testServer(app_server, {
    sent <- recordMessages(session)
    startApp(session)
    doseTable(withTimes("09:30", "90"))
    session$flushReact()
    session$setInputs(referenceTime = "")
    session$setInputs(timeUnits = "days")
    expect_length(sent$modals, 1)
    expect_equal(doseTable()$Time[1:2], c("09:30", "90"))
    expect_equal(doseTableFormat(), timeFormat("minutes", "clock"))
    expect_equal(lastSent(sent, "timeUnits")$value, "minutes")
    expect_equal(lastSent(sent, "timeMode")$value, "clock")
    echo(session, sent)
    expect_equal(doseTableFormat(), timeFormat("minutes", "clock"))
    expect_equal(doseTable()$Time[1:2], c("09:30", "90"))
  })
})

test_that("a change that leaves every string as it was still clears the history", {
  # Times of 0 read the same in every unit, so doseTable() does not change;
  # the redo history, in the old format, must still go.
  shiny::testServer(app_server, {
    sent <- recordMessages(session)
    startApp(session, timeMode = "relative")
    # The table the startup menu gives with its defaults: times of 0
    setDoseTable(doseTableInit, doseTableFormat())
    session$flushReact()
    doseTableHistory()$do(withTimes("30"))
    doseTableHistory()$undo()
    expect_true(doseTableHistory()$can_redo)
    session$setInputs(timeUnits = "days")
    expect_equal(doseTableFormat(), timeFormat("days", "relative"))
    expect_identical(doseTable(), doseTableInit)
    expect_false(doseTableHistory()$can_redo)
    expect_false(doseTableHistory()$can_undo)
  })
})

test_that("an edit from a grid drawn before the switch is converted", {
  shiny::testServer(app_server, {
    sent <- recordMessages(session)
    startApp(session, timeMode = "relative")
    doseTable(withTimes("1440"))
    session$flushReact()
    # The grid on screen was drawn for minutes ...
    stale <- withTimes("1440", "2880")
    x <- createHOT(stale, getDrugDefaultsGlobal(), timeFormat("minutes", "relative"))$x
    # ... and the table is now in days
    session$setInputs(timeUnits = "days")
    expect_equal(doseTable()$Time[1], "1")
    refresh <- doseTableRefresh()
    session$setInputs(doseTableHTML = list(
      data = lapply(seq_len(nrow(stale)), function(i) as.list(unname(unlist(stale[i, ])))),
      changes = list(event = "afterChange", source = "calculate"),
      params = x
    ))
    expect_equal(doseTableDraft()$Time[1:2], c("1", "2"))
    expect_gt(doseTableRefresh(), refresh)   # and the grid is redrawn
    # applied, the doses are at 1 and 2 days, not 1440 and 2880 days
    session$setInputs(dosetable_apply = 1)
    expect_equal(sort(doseTableClean()$Time[doseTableClean()$Drug == "propofol"]), c(1440, 2880))
  })
})

test_that("a scenario in days loads without conversion, once the browser catches up", {
  s <- helpScenario("long-one", "Long", "Opioids", "x",
                    doses = helpDoses(c("morphine", 0, 10, "mg"), c("morphine", 30240, 10, "mg")),
                    timeUnits = "days", maximum = 28 * MINS_PER_DAY)
  shiny::testServer(app_server, {
    sent <- recordMessages(session)
    startApp(session)
    session$flushReact()
    applyHelpScenario(session, s, doseTable, eventTable, timeApi)
    session$flushReact()
    expect_equal(doseTableFormat(), timeFormat("days", "relative"))
    expect_equal(doseTable()$Time[1:2], c("0", "21"))
    # the selectors still say minutes: the plot waits, silently
    expect_error(plotInfo(), class = "shiny.silent.error")
    expect_equal(lastSent(sent, "timeUnits")$value, "days")
    expect_equal(lastSent(sent, "maximum")$value, maxTimeValue(28 * MINS_PER_DAY))
    # the browser reports the unit before the Max time: still waiting
    session$setInputs(timeUnits = "days")
    expect_error(plotInfo(), class = "shiny.silent.error")
    session$setInputs(timeMode = "relative", maximum = maxTimeValue(28 * MINS_PER_DAY))
    expect_equal(doseTable()$Time[1:2], c("0", "21"))   # nothing converted
    expect_equal(plotInfo()$plotMaximum, 28 * MINS_PER_DAY)
    expect_equal(sort(doseTableClean()$Time), c(0, 30240))
  })
})

test_that("TCI rows and inhaled agents are not simulated beyond a week", {
  shiny::testServer(app_server, {
    sent <- recordMessages(session)
    startApp(session, timeMode = "relative")
    dt <- doseTableInit[c(1, 7:12), ]
    dt[1, ] <- list("propofol", "0", "3", "Effect site target")
    doseTable(dt)
    session$flushReact()
    expect_null(timeUnitViolation())
    session$setInputs(timeUnits = "days")
    echo(session, sent)
    session$setInputs(maximum = maxTimeValue(14 * MINS_PER_DAY))
    expect_match(timeUnitViolation(), "propofol effect site target")
    expect_match(timeUnitViolation(), "7 days or less")
    expect_error(drugs(), class = "shiny.silent.error")
    expect_equal(notified(sent, "timeUnitRule")$type, "show")
    expect_error(output$PlotSimulation, "7 days or less")
    # back to two days: allowed, and the TCI schedule is simulated
    session$setInputs(maximum = maxTimeValue(2 * MINS_PER_DAY))
    expect_null(timeUnitViolation())
    expect_equal(notified(sent, "timeUnitRule")$type, "remove")
    expect_false(is.null(drugs()[["propofol"]]$tci))
  })
})

test_that("removing the rows the week rule names clears it, for an agent from the startup menu", {
  # The menu adds oxygen, with its flow left blank, before an inhaled agent;
  # the cleaned table leaves that row out, but while it is there the gas rules
  # keep adding ventilation back, so the message has to name it.
  shiny::testServer(app_server, {
    sent <- recordMessages(session)
    startApp(session, timeMode = "relative")
    chosen <- list("sevoflurane", "cefazolin")
    names(chosen) <- startupDrugInputId(c("Inhaled anesthetics", "Antibiotics"))
    do.call(session$setInputs, chosen)
    session$setInputs(startup_ok = 1)
    expect_setequal(doseTable()$Drug[nzchar(doseTable()$Drug)],
                    c("oxygen", "sevoflurane", "cefazolin", "ventilation"))
    session$setInputs(timeUnits = "days")
    echo(session, sent)
    session$setInputs(maximum = maxTimeValue(14 * MINS_PER_DAY))
    violation <- timeUnitViolation()
    for (drug in c("oxygen", "sevoflurane", "ventilation")) expect_match(violation, drug)
    expect_no_match(violation, "cefazolin")
    # the user removes the rows named and applies
    dt <- doseTable()
    doseTableHistory()$do(dt[!dt$Drug %in% c("oxygen", "sevoflurane", "ventilation"), ])
    session$setInputs(dosetable_apply = 1)
    expect_null(timeUnitViolation())
    expect_identical(names(drugs()), "cefazolin")
  })
})

test_that("the plot is not lengthened past the unit's longest Max time", {
  shiny::testServer(app_server, {
    sent <- recordMessages(session)
    startApp(session, timeMode = "relative")
    doseTable(withTimes("0", "2000"))
    session$flushReact()
    expect_equal(plotInfo()$plotMaximum, MINS_PER_DAY)
    expect_equal(plotInfo()$beyond[["doses"]], 1)
    expect_match(notified(sent, "timeBeyondPlot")$message$html, "1 dose row falls after the end of the plot \\(24 hours\\)")
    # in days there is room for it
    session$setInputs(timeUnits = "days")
    echo(session, sent)
    expect_equal(plotInfo()$plotMaximum, 2 * MINS_PER_DAY)
    expect_equal(notified(sent, "timeBeyondPlot")$type, "remove")
  })
})

test_that("the dialogs show and read times in the dose table's format", {
  shiny::testServer(app_server, {
    sent <- recordMessages(session)
    startApp(session)
    session$flushReact()
    # an event at a clock time is stored in minutes after the start; one that
    # cannot be read (there is no 25:00) is refused, and the table untouched
    session$setInputs(clickTimeEvent = "09:30", clickEvent = "Induction", addEventBtn = 1)
    expect_identical(eventTable()$Time, 90)
    session$setInputs(clickTimeEvent = "25:00", addEventBtn = 2)
    expect_identical(eventTable()$Time, 90)
    expect_true(anyErrorShown(sent))

    session$setInputs(timeUnits = "days")
    echo(session, sent)
    # an event entered in days is stored in minutes
    session$setInputs(clickTimeEvent = "1.5", addEventBtn = 3)
    expect_identical(eventTable()$Time, c(90, 2160))

    # edit events: a row not edited keeps its exact minutes
    eventTable(data.frame(Time = c(1440 / 7, 2160), Event = c("Induction", "Intubation")))
    showEditEventsModal()
    x <- editEventsHOT()$x
    rows <- jsonlite::fromJSON(x$data, simplifyDataFrame = FALSE)
    # shown to 0.001 minute: 205.714 minutes
    expect_equal(vapply(rows, `[[`, character(1), "Time"), c("0.1428569444", "1.5"))
    asInput <- function(rows) list(
      data = lapply(rows, function(r) list(r$Delete, r$Time, r$Event)),
      changes = list(event = "afterChange", source = "edit"),
      params = x
    )
    session$setInputs(editEventsTableHTML = asInput(rows), editEventsOK = 1)
    expect_identical(eventTable()$Time, c(1440 / 7, 2160))
    rows[[2]]$Time <- "3"
    showEditEventsModal()
    session$setInputs(editEventsTableHTML = asInput(rows), editEventsOK = 2)
    expect_identical(eventTable()$Time, c(1440 / 7, 4320))

    # the edit-doses dialog sorts by time, not as text
    doseTable(withTimes("10", "9", "1.5"))
    DrugTimeUnits(list(drug = "propofol"))
    TT <- data.frame(Delete = FALSE, Time = c("10", "9"), Dose = c("1", "2"), Units = c("mg", "mg"))
    hot <- rhandsontable::rhandsontable(TT)$x
    session$setInputs(
      editPriorDosesTable = list(
        data = lapply(seq_len(nrow(TT)), function(i) unname(as.list(TT[i, ]))),
        changes = list(event = "afterChange", source = "edit"),
        params = hot
      ),
      editDosesOK = 1
    )
    timed <- doseTable()[nzchar(doseTable()$Drug), ]
    expect_equal(displayTimeToMinutes(timed$Time, REFERENCE_TIME_NONE, "days"),
                 sort(displayTimeToMinutes(timed$Time, REFERENCE_TIME_NONE, "days")))
  })
})

test_that("a long-term drug on a short plot offers a year", {
  local_mocked_bindings(LONG_TERM_DRUGS = "morphine")
  shiny::testServer(app_server, {
    sent <- recordMessages(session)
    startApp(session, timeMode = "relative")
    dt <- doseTableInit[c(1, 7:12), ]
    dt[1, ] <- list("morphine", "0", "10", "mg")
    doseTable(dt)
    session$flushReact()
    expect_equal(notified(sent, "longTermPrompt")$type, "show")
    # once per addition, not on every change
    session$setInputs(maximum = "120")
    expect_length(notifications(sent, "longTermPrompt"), 1)
    session$setInputs(showLongTermTime = 1)
    expect_equal(lastSent(sent, "timeUnits")$value, "days")
    expect_equal(lastSent(sent, "maximum")$value, maxTimeValue(MINS_PER_YEAR))
    echo(session, sent)
    expect_equal(doseTableFormat(), timeFormat("days", "relative"))
    expect_equal(plotInfo()$plotMaximum, MINS_PER_YEAR)
  })
})

test_that("Show 365 days closes an open dialog before converting the table", {
  # The notification is drawn above any open dialog, so it can be clicked
  # while, say, the add-dose dialog shows a time in minutes; that time would
  # then be read in days.  The click must close the dialog first.
  local_mocked_bindings(LONG_TERM_DRUGS = "morphine")
  shiny::testServer(app_server, {
    sent <- recordMessages(session)
    startApp(session, timeMode = "relative")
    dt <- doseTableInit[c(1, 7:12), ]
    dt[1, ] <- list("morphine", "0", "10", "mg")
    doseTable(dt)
    session$flushReact()
    before <- length(sent$modals)
    session$setInputs(showLongTermTime = 1)
    modalTypes <- vapply(sent$modals[seq_along(sent$modals) > before], `[[`, character(1), "type")
    expect_true("remove" %in% modalTypes)
  })
})

test_that("the long-term prompt is not offered again while the table is unreadable", {
  # A half-typed Procedure start makes doseTableClean() wait (req).  That is
  # not an empty table: the drug never left, so the prompt must not return.
  local_mocked_bindings(LONG_TERM_DRUGS = "morphine")
  shiny::testServer(app_server, {
    sent <- recordMessages(session)
    startApp(session)
    dt <- doseTableInit[c(1, 7:12), ]
    dt[1, ] <- list("morphine", "08:00", "10", "mg")
    doseTable(dt)
    session$flushReact()
    expect_equal(notified(sent, "longTermPrompt")$type, "show")
    for (typed in c("", "0", "09", "09:", "09:0", "09:00")) session$setInputs(referenceTime = typed)
    expect_length(notifications(sent, "longTermPrompt"), 1)

    # Removing the drug and adding it back is a new addition, and is offered
    doseTable(doseTableInit[c(1, 7:12), ])
    session$flushReact()
    doseTable(dt)
    session$flushReact()
    expect_length(notifications(sent, "longTermPrompt"), 2)
  })
})

.sprglobals$config <- oldConfig
