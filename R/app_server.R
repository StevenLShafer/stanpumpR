####################################################
# stanpumpR                                        #
# Copyright 2019-2026 Steven L. Shafer, MD,        #
# and contributors. MIT License: see LICENSE.md.   #
####################################################

app_server <- function(input, output, session) {
  config <- .sprglobals$config

  # Always one number, DEBUG_LEVEL_OFF to DEBUG_LEVEL_VERBOSE (debugLevel()):
  # every outputComments() and profileCode() compares it, and ?debug=abc or
  # ?debug=, read as NA, made the first of those comparisons an error that
  # ended the session as it started (keyboard review of 2026-11-02).
  session$userData$debug <- reactiveVal({
    query <- parseQueryString(isolate(session$clientData$url_search))
    debugLevel(query[["debug"]], debugLevel(config$debug))
  })
  observeEvent(input$debug_level, ignoreInit = TRUE, {
    session$userData$debug(debugLevel(input$debug_level, isolate(session$userData$debug())))
    # Rewrite the address bar with the new level (withDebugQuery())
    session$doBookmark()
  })

  # Write out logs to the log section
  observeEvent(session$userData$debug(), {
    shinyjs::toggle("debug_area", condition = (session$userData$debug() > DEBUG_LEVEL_OFF))
    updateSelectInput(session, "debug_level", selected = session$userData$debug())
  })
  commentsLog <- reactiveVal("")
  output$logContent <- renderText({
    commentsLog()
  })
  # Register the comments log with this user's session, to use outside the server
  session$userData$commentsLog <- commentsLog
  # The first line of the log names the build (stanpumpRBuild())
  outputComments(stanpumpRBuild()$label)

  profileRecords <- reactiveVal(data.frame(name = character(0), ms = numeric(0), time = character(0)))

  profileCode <- function(expr, name, threshold = NULL) {
    if (isolate(session$userData$debug()) == DEBUG_LEVEL_OFF) {
      return(force(expr))
    }
    if (is.null(threshold)) {
      threshold <- isolate(input$profiler_threshold)
    }
    # The Debug section's box may be empty (NA) or hold anything typed: 0
    # then, every call recorded.  An NA here stopped every profiled observer,
    # the bookmarking one included, so emptying the box ended the session at
    # the next change (keyboard review of 2026-11-02).
    threshold <- asScalarNumeric(threshold)
    if (!is_valid_number(threshold)) threshold <- 0
    start_time <- proc.time()[["elapsed"]]
    value <- force(expr)
    end_time <- proc.time()[["elapsed"]]
    elapsed_time <- (end_time - start_time) * 1000
    if (elapsed_time > threshold) {
      new_row <- data.frame(name = name, ms = round(elapsed_time, 3), time = format(Sys.time(), "%H:%M:%S"))
      isolate(profileRecords(rbind(profileRecords(), new_row)))
    }
    value
  }

  output$profiling <- renderText({
    outputComments("In output$profiling", level = DEBUG_LEVEL_VERBOSE)
    records <- profileRecords()
    if (nrow(records) == 0) {
      return("No profiling records yet.")
    }
    paste(
      # %s: ms is round(, 3), and %d stops on a fraction
      sprintf("[%s] %s (%s ms)", records$time, records$name, format(records$ms, trim = TRUE)),
      collapse = "\n"
    )
  })

  #############################################################################
  #                           Initialization                                  #
  #############################################################################

  outputComments(getInstalledPackagesInfo())

  outputComments(
    "**********************************************************************\n",
    "*                       Initializing                                 *\n",
    "**********************************************************************",
    sep = ""
  )

  # The plot; NULL while there is nothing to draw; or the error that stopped the
  # simulation or the plot.  Shiny's own "nothing to show yet" (req()) is
  # swallowed, as it always was, but any other error -- a model parameter that
  # is not usable at an extreme weight, a dose table the validation refuses, a
  # plot too tall to draw -- is handed to renderPlot() below, which says so in
  # the plot area.  Until 2026-10-31 every error was swallowed and the plot
  # simply went blank, with nothing to say why.
  main_plot <- reactive({
    outputComments("In main_plot", level = DEBUG_LEVEL_VERBOSE)
    tryCatch({
      if (is.null(doseTableClean()) || is.null(drugs()) || is.null(plotObjectReactive())) {
        NULL
      } else {
        plotObjectReactive()
      }
    }, error = function(err) {
      if (inherits(err, "shiny.silent.error")) NULL else err
    })
  })

  # The message for the plot area when main_plot() is an error, else NULL
  plotFailure <- function(plot) {
    if (!inherits(plot, "error")) return(NULL)
    outputComments("No plot:", conditionMessage(plot))
    paste0(PLOT_FAILURE_PREFIX, conditionMessage(plot))
  }

  # renderPlot() asks for the height before it draws, so an error in the
  # simulation surfaced here, as red text in the plot area, although main_plot()
  # deliberately shows nothing for it.  Treat it the same way.
  plotHeightOrNothing <- function() {
    height <- tryCatch(plotHeight(), error = function(err) {
      if (!inherits(err, "shiny.silent.error")) {
        outputComments("No plot:", conditionMessage(err))
      }
      NULL
    })
    req(height)
  }

  # What the plot area says instead of a plot, or NULL: the time-units rule, an
  # unreadable patient field or procedure start, or the error that stopped the
  # simulation.  In that order, since the earlier ones explain the later.
  plotMessage <- reactive({
    violation <- timeUnitViolation()
    if (!is.null(violation)) return(violation)
    schedule <- scheduleProblem()
    if (!is.null(schedule)) return(schedule)
    gasRows <- gasRowsProblem()
    if (!is.null(gasRows)) return(gasRows)
    problem <- patientEntryProblem()
    if (!is.null(problem)) return(problem)
    start <- referenceTimeProblem()
    if (!is.null(start)) return(start)
    nothing <- nothingToDraw()
    if (!is.null(nothing)) return(nothing)
    plotFailure(main_plot())
  })

  # What the plot area says while it has nothing to draw: no drug has a dose
  # above 0 (positiveDoseDrugs(), R/drug-pipeline.R) and no inhaled agent or
  # carrier gas is set above 0 (reportableGases()), so a table of drugs at 0,
  # as the startup menu writes it, is not a blank plot area.  NULL while the
  # table cannot be read yet, or not at all, which main_plot() then says.
  nothingToDraw <- reactive({
    DT <- tryCatch(doseTableClean(), error = function(e) e)
    if (inherits(DT, "error")) return(NULL)
    if (!is.null(DT)) {
      gas <- isGasDrug(DT$Drug)
      if (length(positiveDoseDrugs(DT[!gas, , drop = FALSE])) > 0) return(NULL)
      if (length(reportableGases(DT[gas, , drop = FALSE])) > 0) return(NULL)
    }
    NOTHING_TO_DRAW_MESSAGE
  })

  # Why nothing can be simulated, or NULL: what plotMessage() says before it
  # draws, and the error validating either table would raise.  For the
  # observers that simulate for themselves (Suggest Dosing, Export),
  # which ask it before they start: in an observer a req() ends the work in
  # silence, and an error ends the session (adversarial UI review, 2026-11,
  # findings 7, 8, 10 and 11).
  simulationBlocked <- function() {
    tryCatch({
      problem <- timeUnitViolation()
      if (is.null(problem)) problem <- scheduleProblem()
      if (is.null(problem)) problem <- gasRowsProblem()
      if (is.null(problem)) problem <- patientEntryProblem()
      if (is.null(problem)) problem <- referenceTimeProblem()
      if (is.null(problem)) {
        doseTableClean()
        eventTableClean()
      }
      problem
    },
    shiny.silent.error = function(e) NULL,
    error = function(e) conditionMessage(e))
  }

  output$PlotSimulation <- renderPlot({
    outputComments("In output$PlotSimulation", level = DEBUG_LEVEL_VERBOSE)
    # Said here, in the plot area, because main_plot() swallows errors and the
    # plot would otherwise just stay as it was.
    message <- plotMessage()
    validate(need(is.null(message), message))
    req(main_plot(), cancelOutput = TRUE)
    main_plot()
  }, height = function() {
    # renderPlot() measures before it draws, and with a message there is no
    # plot to measure: a fixed height lets the message above through.
    if (!is.null(plotMessage())) 150 else plotHeightOrNothing()
  })

  # Make drugs and events local to session
  outputComments("Setting Drug and Event Defaults")
  drugDefaults <- reactiveVal(getDrugDefaultsGlobal())

  # Threshold for the MAC series' "time until threshold".  MAC is not a drug and
  # has no row in drugDefaults, so it is kept here; it is edited in the Drug
  # Thresholds dialog with the rest.
  macThreshold <- reactiveVal(GAS_MAC_THRESHOLD)
  eventDefaults <- reactiveVal(getEventDefaults())
  # Alphabetical, for the drug pickers; never index drugDefaults() with it
  drugList <- sortDrugNames(getDrugDefaultsGlobal()$Drug)

  # The drugs offered when a drug is chosen (the dose-table autocomplete and
  # the Add a dose dialog): every drug when the illicit-drug opt-in is on, the
  # non-illicit drugs when it is off (R/illicit-drugs.R).  Alphabetical.
  drugChoices <- reactive(
    sortDrugNames(visibleDrugNames(isTRUE(input$showIllicitDrugs), drugDefaults()))
  )

  # Empty until the drugs are chosen in the startup menu, or a bookmark
  # restores its own (see Startup, below)
  doseTable <- reactiveVal(doseTableBlank)

  emailSendCount <- reactiveVal(0)

  eventTable <- reactiveVal(eventTableInit)

  #############################################################################
  #                              Time units                                   #
  #############################################################################
  # See R/utils-time.R.  The time unit (input$timeUnits) and the time display
  # (input$timeMode) decide how times are shown and typed; the engine works in
  # minutes whatever they are.  The dose table holds its times as typed, so
  # the format they were typed in is kept beside it, in doseTableFormat().
  # doseTable() and its draft are always in that format, and everything that
  # reads or writes their Time strings -- doseTableClean(), the grid, and the
  # add-dose, edit-doses, add-event, edit-events and Suggest Dosing dialogs --
  # goes by it, never by the selectors.  What is drawn (the axis, the hover,
  # the recovery labels) goes by the selectors.  When the selectors change, one
  # observer rewrites the table into the new format, and plotInfo() waits,
  # silently, while the two disagree: nothing is ever simulated from a table
  # read in the wrong unit.

  # The chosen time unit; minutes until the control reports, and for anything
  # that is not a unit.
  timeUnit <- reactive(validTimeUnit(input$timeUnits))

  # The format the selectors ask for.  Days and weeks are always elapsed.
  selectorFormat <- reactive({
    timeFormat(timeUnit(), if (is.null(input$timeMode)) TIME_MODE_DEFAULT else input$timeMode)
  })

  # Starts as the selectors start: minutes and elapsed time, as the UI opens, or
  # a restored bookmark's settings.  The session starts with an empty table
  # (doseTableBlank), the startup menu's Start writes times that are all "0",
  # the same in every format, and onRestored() converts a restored table itself.
  # (Starting from the UI's defaults instead would leave the plot waiting for
  # good whenever a session starts with other settings and nothing converts
  # the table: a browser reconnecting in days, say, since the conversion
  # observer ignores the initial inputs.)
  doseTableFormat <- reactiveVal(isolate(selectorFormat()))

  # The procedure start that times in `format` are read against.  The box is
  # read only in clock mode, so typing in it does not disturb elapsed times.
  referenceFor <- function(format) {
    if (isClockFormat(format)) procedureStart() else REFERENCE_TIME_NONE
  }
  # The Procedure start box, as one string: a link or a forged input could
  # send a vector or a null there, which took the plot down (Carbon's third
  # UI audit, 2026-11-15, M5)
  procedureStart <- function() asScalarString(input$referenceTime)
  # Has the browser's clock, which fills an empty Procedure start, arrived?
  clockArrived <- reactiveVal(FALSE)

  # As referenceFor(), but waiting while a clock-mode procedure start cannot
  # be read (it is empty until the browser's clock arrives, and may be half
  # typed), rather than reading every clock time as missing.
  reqReference <- function(format) {
    reference <- referenceFor(format)
    if (isClockFormat(format)) req(isValidReferenceTime(reference))
    reference
  }

  # Replace the dose table and its format together.  The draft and its undo
  # history are reset here, not left to the doseTable() observer: when dt is
  # identical() to doseTable() that reactiveVal does not invalidate, and the
  # draft would keep strings in the old format.  $do() then $clear(): $clear()
  # alone keeps the current draft.
  setDoseTable <- function(dt, format) {
    doseTable(dt)
    doseTableFormat(format)
    doseTableHistory()$do(dt)$clear()
  }

  # Which unit the Max time and Min time sliders and the Time Display choices
  # were last set up for.  Plain variables, not reactive values: they record
  # what the browser was told.  The initial inputs (a restored bookmark's too)
  # are in place before the server function runs, and the UI built them for
  # that unit.
  sliderUnit <- isolate(timeUnit())
  modeChoicesUnit <- isolate(timeUnit())
  # While a slider is dragged, the browser reports a new input$maximum (or
  # input$minimum) every ~250ms, and each one used to invalidate plotInfo()
  # and the whole simulate-and-plot chain below it.  On a heavy dose table
  # (many scheduled drugs near MAX_SCHEDULED_DOSES) a drag through several
  # still-simulatable lengths could queue enough full resimulations, on this
  # single-threaded R process, to disconnect the session -- reproduced
  # intermittently dragging Max time down on a guarded q4h plot (production
  # audit, 2026-11-01).  plotInfo() reads these debounced values, so only the
  # value a slider settles on triggers real work.
  maximumDebounced <- debounce(reactive(input$maximum), 400)
  minimumDebounced <- debounce(reactive(input$minimum), 400)

  # TRUE from when syncTimeSliders() sends the sliders new values until the
  # browser reports them and the debounced values above have caught up: were
  # it cleared on the raw echo, plotInfo() would simulate a scenario or
  # bookmark once with the previous Max time before the debounce delivered
  # the new one.
  sliderPending <- reactiveVal(FALSE)
  observeEvent(list(maximumDebounced(), minimumDebounced()), ignoreInit = TRUE, {
    sliderPending(FALSE)
  })

  # Give the Max time and Min time sliders the unit's range and step
  # (timeSliderRange()), with their values snapped to it.  Without `maximum`
  # or `minimum` the current value is kept, snapped.  The values are minutes
  # in every unit, so a change of unit leaves them meaning what they meant;
  # only the step, and so the snapping, changes.
  syncTimeSliders <- function(unit, maximum = NULL, minimum = NULL) {
    if (identical(unit, sliderUnit) && is.null(maximum) && is.null(minimum)) return(invisible())
    if (is.null(maximum)) maximum <- isolate(input$maximum)
    if (is.null(minimum)) minimum <- isolate(input$minimum)
    maximum <- snapMaximum(maximum, unit)
    minimum <- snapMinimum(minimum, maximum, unit)
    # New values make the plot wait until the browser reports them, rather
    # than simulate first with the old ones (plotInfo())
    # One number each, whatever the browser or a link sent: a vector made the
    # && below an error, which ended the session at a change of time unit
    # and stopped a bookmark's restore half way (Carbon's third UI audit,
    # 2026-11-15, C2)
    shownMaximum <- asScalarNumeric(isolate(input$maximum))
    shownMinimum <- asScalarNumeric(isolate(input$minimum))
    if (is.na(shownMinimum)) shownMinimum <- 0
    if (!isTRUE(maximum == shownMaximum && minimum == shownMinimum)) sliderPending(TRUE)
    r <- timeSliderRange(unit)
    updateSliderInput(session, "maximum", value = maximum, min = r$step, max = r$max, step = r$step)
    updateSliderInput(session, "minimum", value = minimum, min = r$min, max = r$max, step = r$step)
    sliderUnit <<- unit
    invisible()
  }

  # The window must have a length: Min time is kept a step below Max time.
  # Dragging Max time down to Min time pulls Min time down with it; dragging
  # Min time up to Max time pushes Max time up, or, at the end of the
  # slider, stops a step short.
  observeEvent(input$maximum, ignoreInit = TRUE, {
    maximum <- asScalarNumeric(input$maximum)
    minimum <- asScalarNumeric(input$minimum)
    if (!is_valid_number(maximum) || !is_valid_number(minimum) || minimum < maximum) return()
    updateSliderInput(session, "minimum", value = snapMinimum(minimum, maximum, timeUnit()))
  })
  observeEvent(input$minimum, ignoreInit = TRUE, {
    maximum <- asScalarNumeric(input$maximum)
    minimum <- asScalarNumeric(input$minimum)
    if (!is_valid_number(maximum) || !is_valid_number(minimum) || minimum < maximum) return()
    r <- timeSliderRange(timeUnit())
    if (minimum + r$step <= r$max) {
      updateSliderInput(session, "maximum", value = snapMaximum(minimum + r$step, timeUnit()))
    } else {
      updateSliderInput(session, "minimum", value = r$max - r$step)
    }
  })

  syncTimeModeChoices <- function(unit, selected) {
    updateSelectizeInput(session, "timeMode", choices = timeModeChoices(unit), selected = selected)
    modeChoicesUnit <<- unit
    invisible()
  }

  # The repeating-oral switch of the time settings (see its observer, below)
  repeatingOralSwitch <- new.env(parent = emptyenv())
  repeatingOralSwitch$settled <- FALSE

  # What the server uses to change the time settings itself: loading a
  # scenario, restoring a bookmark, the long-term drug prompt, the
  # repeating-oral switch.  Setting the
  # selectors this way and writing the table with setDoseTable() in the format
  # they will report means their echo finds nothing to convert.
  timeApi <- list(
    setDoseTable = setDoseTable,
    showTimeSettings = function(unit, mode, maximum, minimum = 0) {
      # Time settings chosen for the session (a scenario, a restored link,
      # the long-term prompt) are never replaced by the repeating-oral default
      repeatingOralSwitch$settled <- TRUE
      format <- timeFormat(unit, mode)
      updateSelectInput(session, "timeUnits", selected = format[["unit"]])
      syncTimeModeChoices(format[["unit"]], format[["mode"]])
      syncTimeSliders(format[["unit"]], maximum = maximum, minimum = minimum)
    }
  )

  # The selectors changed: rewrite the dose table into the new format.
  # priority = 10 runs this before the bookmarking observer, so a bookmark
  # never pairs the new selectors with the old table.  The switch applies any
  # unapplied edits in the dose table, as it always has.
  observeEvent(list(input$timeUnits, input$timeMode), ignoreInit = TRUE, priority = 10, {
    target <- selectorFormat()
    unit <- target[["unit"]]
    # Days and weeks have no clock mode: the Time Display is set to elapsed,
    # the only choice it then offers.  Back in minutes or hours it offers both.
    if (!identical(input$timeMode, target[["mode"]]) ||
        (unit %in% CLOCK_TIME_UNITS) != (modeChoicesUnit %in% CLOCK_TIME_UNITS)) {
      syncTimeModeChoices(unit, target[["mode"]])
    }

    from <- doseTableFormat()
    if (identical(from, target)) {
      # The echo of a change the server made (a scenario, a restored bookmark,
      # the long-term prompt, a refused switch): the table is already in it.
      syncTimeSliders(unit)
      return()
    }

    draft <- doseTableDraft()
    res <- rebaseDoseTimes(draft, from, target, procedureStart())
    if (!res$ok) {
      # Clock times cannot be converted without the procedure start.  Put the
      # selectors back; the table is untouched.
      showModal(modalDialog(
        title = "Set a valid procedure start first",
        "The dose table has clock times (HH:MM), which are read from the Procedure start.",
        "Enter the procedure start as HH:MM, then change the time settings again.",
        easyClose = TRUE
      ))
      updateSelectInput(session, "timeUnits", selected = from[["unit"]])
      syncTimeModeChoices(from[["unit"]], from[["mode"]])
      return()
    }

    setDoseTable(res$table, target)
    syncTimeSliders(unit)
    note <- if (unit != from[["unit"]]) {
      paste0("Dose times converted to ", unit, ".")
    } else if (!identicalTable(res$table, draft)) {
      if (isClockFormat(target)) {
        paste0("Elapsed times converted to ", unit, " after the procedure start.")
      } else {
        paste0("Clock times converted to elapsed ", unit, ".")
      }
    }
    if (!is.null(note)) showNotification(note, id = "timeConverted", type = "message")
  })

  # The Time panel's header reads its settings out while it is collapsed:
  # "(minutes, Actual time)", "(days, Elapsed time)".  It follows the
  # selectors, as the axis does.
  output$timeSummary <- renderText({
    timeSettingsText(selectorFormat())
  })

  # The Doses card header says what the times in the table are:
  # "(times in days)", "(HH:MM or minutes)"
  output$doseTimeUnits <- renderText({
    format <- doseTableFormat()
    if (isClockFormat(format)) {
      paste0("(", timeEntryUnitText(format), ")")
    } else {
      paste0("(times in ", timeEntryUnitText(format), ")")
    }
  })

  # The Help tab: see R/help-server.R.  Loading a teaching scenario from the
  # help writes doseTable() and eventTable() directly, as a URL restore does,
  # and sets the time settings through timeApi.
  # The Bayesian Updating panel (R/bayesian-server.R): the measured
  # concentrations and the two variabilities.  Created before the help, whose
  # scenarios can carry measurements, because a ?scenario= link loads its
  # scenario while this function is still running (below).  Its arguments are
  # functions, read only once the session runs, so nothing they name needs to
  # exist yet.
  bayes <- bayesianServer(
    input, output, session,
    doseTableClean = function() doseTableClean(),
    doseTableFormat = function() doseTableFormat(),
    drugDefaults = function() drugDefaults(),
    result = function() bayesUpdate()
  )

  # A scenario is a new patient, whose measured concentrations are not the
  # last one's: they are cleared and the variabilities reset, and then the
  # scenario's own, if it has any, are put in.
  helpApi <- helpServer(input, output, session, doseTable, eventTable, drugDefaults,
                        timeApi = timeApi,
                        onLoad = function() bayes$clear(),
                        restoreMeasurements = bayes$restore)

  # Single Paper Review, a tab of its own that shares nothing with the
  # simulator (R/paper-review-app.R).
  paperReviewModuleServer()
  helpPage <- helpApi$page

  ###########
  # Startup #
  ###########

  # A fresh session opens on the drug menu (R/startup-drugs.R).  A bookmark
  # carries its own dose table, which onRestored() puts in, so it skips the
  # menu unless that table is empty.  Shown now, as the session starts, rather
  # than in reply to the browser.
  #
  # A ?scenario= or ?help= link (R/deep-links.R) opens on the scenario or the
  # page instead of the menu.  A bookmark's own dose table wins over
  # ?scenario=.  The link is acted on below, once the dose table's history
  # exists; here it only decides whether the menu opens.
  restoreValues <- session$restoreContext$values
  deepLink <- deepLinkFromQuery(isolate(session$clientData$url_search),
                                isolate(drugDefaults()))
  if (isBookmarkRestore(restoreValues)) deepLink$scenario <- NULL
  startupMenuOpen <- opensOnStartupMenu(restoreValues) && !deepLinkOpens(deepLink)
  if (startupMenuOpen) {
    showModal(startupDrugModal(startupDrugChoices(isolate(drugDefaults()))))
  }
  for (problem in deepLink$problems) {
    outputComments("Link:", problem)
    showNotification(problem, type = "warning", duration = 10)
  }

  # app.js asks for the welcome on a first visit, or when a week has passed
  # since the last.  With the menu open it goes at the top of the menu; a
  # modal replacing the menu would leave nothing to choose the drugs with.
  startupWelcome <- reactiveVal(FALSE)
  output$startup_welcome <- renderUI({
    req(startupWelcome())
    startupWelcomeUI()
  })
  observeEvent(input$show_intro_modal, {
    if (startupMenuOpen) {
      startupWelcome(TRUE)
    } else if (!deepLinkOpens(deepLink)) {
      # Someone sent to a scenario or a page by a link is not greeted with
      # the welcome on top of it
      showIntroModal()
    }
  }, once = TRUE)

  startWithChosenDrugs <- function() {
    if (!startupMenuOpen) return(FALSE)
    startupMenuOpen <<- FALSE
    removeModal()
    chosen <- unlist(lapply(DRUG_CATEGORIES, function(category) {
      input[[startupDrugInputId(category)]]
    }), use.names = FALSE)
    outputComments("Starting with:", paste(chosen, collapse = ", "))
    # Its times are all "0", the same in every format: written in the
    # format the table already has.
    setDoseTable(startupDoseTable(chosen, drugDefaults()), doseTableFormat())
    TRUE
  }
  observeEvent(input$startup_ok, {
    startWithChosenDrugs()
  })
  observeEvent(input$startup_tour, {
    if (startWithChosenDrugs()) {
      helpPage("quick-start")
      bslib::nav_select("mainNav", "Help", session = session)
    }
  })

  outputComments("Setup Complete")

  # Get reference time from client
  # The reference time is passed from app.js on event shiny:sessioninitialized
  # Read safely: input$referenceTime != '' on a value that was not one
  # string, and getReferenceTime() on a client_time that was not, ended the
  # session (Carbon's UI audit, 2026-11-14, F6E)
  observeEvent(input$client_time, {
    clockArrived(TRUE)
    if (nzchar(procedureStart())) {
      return()
    }
    outputComments("In observeEvent(input$client_time,...", level = DEBUG_LEVEL_VERBOSE)
    time <- asScalarString(input$client_time)
    outputComments("Reference time from client:", time)
    start <- getReferenceTime(time)
    outputComments("Calculated reference time:", start)
    updateNumericInput(session, "referenceTime", value = start)
  }, ignoreNULL = TRUE, once = TRUE)

  # A change of the Procedure start in clock time moves the doses, whose
  # times are clock times read from it, but not the events, which are kept
  # in minutes after it: moving the start from 08:00 to 07:30 put every event
  # half an hour earlier against the doses (Carbon's third UI audit,
  # 2026-11-15, M2).  So the events are moved as the doses are
  # (eventTimesForStart()): one in the 24 hours after the start keeps its
  # clock time, one later its elapsed time.  The first version added the
  # old start read against the new as one shift, which wrapped across
  # midnight when the start moved later: 08:00 to 08:30 moved every event a
  # day on, and typing a start ratcheted them days ahead (Carbon's fourth
  # UI audit, 2026-11-15, H3).
  lastProcedureStart <- NULL
  observeEvent(input$referenceTime, {
    start <- procedureStart()
    if (!isValidReferenceTime(start)) return()
    old <- lastProcedureStart
    lastProcedureStart <<- start
    if (is.null(old) || identical(old, start) || !isClockFormat(selectorFormat())) return()
    events <- eventTable()
    if (is.null(events) || nrow(events) == 0) return()
    moved <- eventTimesForStart(events$Time, old, start)
    if (identical(moved, as.numeric(events$Time))) return()
    events$Time <- moved
    eventTable(events)
  }, ignoreNULL = TRUE)

  # The procedure start for what is drawn (the axis and hover labels), which
  # follow the selectors: REFERENCE_TIME_NONE when times are elapsed, and in
  # clock mode a readable "HH:MM" (it waits until there is one).
  referenceTime <- reactive({
    reqReference(selectorFormat())
  })

  # The last panel clicked (imgDrugTime()), NULL before any click
  DrugTimeUnits <- reactiveVal(NULL)
  # Its drug, or NULL: what the Edit doses dialog's buttons are about (they
  # need no time)
  clickedDrug <- function() {
    x <- DrugTimeUnits()
    drug <- if (is.list(x)) asScalarString(x$drug) else ""
    if (nzchar(drug)) drug else NULL
  }

  ##########################################################
  # Code to save state in url and then restore from url

  url <- reactiveVal("")

  # Nothing of a reviewed paper goes into the URL (its model names, the
  # patient and doses simulated): Single Paper Review's inputs are all
  # excluded (paperReviewModuleServer()).  Set before every doBookmark(),
  # which reads the exclusions before it calls onBookmark().
  excludeFromBookmark <- function(names)
    setBookmarkExclude(c(bookmarksToExclude,
                         grep(paste0("^", PAPER_REVIEW_MODULE_ID, "-"), names, value = TRUE)))

  # Every time an input changes, and every time a table onBookmark() saves
  # changes, after the observers that change them: the link was made from
  # the input alone, at the default priority, so it ran before Apply's
  # observer (priority -1) stored the draft and kept the table from before
  # Apply until another input changed, in the address bar and in the link
  # the Export panel emails (Gemini's production audit, 2026-11-08, finding 3).
  observe({
    profileCode({
      excludeFromBookmark(names(reactiveValuesToList(input)))
      doseTable(); doseTableFormat(); eventTable(); macThreshold(); drugDefaults()
      bayes$rows()
      session$doBookmark()
    }, name = "doBookmark observer")
  }, priority = -10)

  # This gets called before bookmarking to prepare values that need to be saved
  onBookmark(function(state) {
    profileCode({
      state$values$DT <- doseTable()
      # The format DT's Time strings are in ("days/relative"); see doseTableFormat()
      state$values$doseTableFormat <- timeFormatString(doseTableFormat())
      state$values$ET <- eventTable()
      # Edited thresholds travel with the URL, so a restored session reports
      # the same times until threshold.  endCe is saved as stored (the gases'
      # at the reference age), keyed by drug, so a changed drug list does not
      # misalign it.
      state$values$macThreshold <- macThreshold()
      state$values$endCe <- changedThresholds(drugDefaults())
      # And the Drug Library's changes, which the dose table's units, targets
      # and measured levels are in: a link without them reopened a drug moved
      # to ng in mcg, its converted target 1000 times too high (Carbon's
      # third UI audit, 2026-11-15, C4).  Only the changed rows.
      state$values$drugLibrary <- changedDrugLibrary(drugDefaults())
      # The Bayesian update's measured concentrations, in minutes (complete
      # rows only); saved only when there are some, to keep links short.
      stored <- bayes$rows()
      measured <- bayesRowsToObservations(stored$rows, stored$format, procedureStart())
      if (nrow(measured) > 0) state$values$bayes <- measured
      excludeFromBookmark(isolate(names(input)))
    }, name = "onBookmark()")
  })

  # This gets called after bookmarking is completed
  onBookmarked(function(url) {
    profileCode({
      # A bookmark longer than the server accepts cannot be reopened, so the
      # address bar is left at the plain app address rather than at a link
      # that fails, and the user is told (bookmarkForAddressBar()).
      link <- bookmarkForAddressBar(url)
      url <- link$url
      if (!is.null(link$problem)) {
        showNotification(link$problem, id = "bookmarkTooLong", type = "warning", duration = 10)
      }
      # The address bar keeps ?debug= across reloads; url(), which is
      # emailed with a slide, does not carry it.
      updateQueryString(withDebugQuery(url, isolate(session$userData$debug()), config$debug))
      url(url)
    }, name = "onBookmarked()")
  })

  onRestored(function(state) {
    # Shiny calls this for any query string; ?debug=1 alone has no dose table,
    # and restoring its absence would leave the dose table unusable (and turn
    # off adjustToFFM, below, as if for an old bookmark).
    if (!isBookmarkRestore(state$values)) return()
    # A restored link brings its own time settings: the repeating-oral switch
    # is not applied to them
    repeatingOralSwitch$settled <- TRUE
    profileCode({
      outputComments(
        "***************************************************************************\n",
        "*                       Restoring Session from URL                        *\n",
        "***************************************************************************",
        sep = ""
      )
      # The table is written in the format it was saved in; a bookmark made
      # before time units existed was in minutes, clock or elapsed as its Time
      # Display was.  The selectors were restored by the UI (app_ui(), which
      # also picks a unit for an old bookmark from its Max time), so the table
      # is converted to what they show.  The conversion observer does not run
      # for restored inputs, so it is done here.
      saved <- parseTimeFormat(state$values$doseTableFormat)
      # A bookmark that saved neither carries the clock time it always fell
      # back to: it was made while the app opened in clock time, so it is not
      # read with today's TIME_MODE_DEFAULT
      if (is.null(saved)) {
        saved <- timeFormat(TIME_UNIT_DEFAULT, asScalarString(state$input$timeMode, "clock"))
      }
      # The link's Drug Library first, since its units are the dose table's
      # and its targets are in its concentration units (restoredDrugLibrary(),
      # held to the dialog's own rules)
      restoredLibrary <- restoredDrugLibrary(state$values$drugLibrary)
      if (!identical(restoredLibrary$table, drugDefaults())) drugDefaults(restoredLibrary$table)
      if (length(restoredLibrary$problems) > 0) {
        showNotification(paste(restoredLibrary$problems, collapse = " "), id = "bookmarkLibrary",
                         type = "warning", duration = 15)
      }
      # Always there: a link without a dose table returned above.  Read as
      # untrusted input: a link is text anyone can write (R/bookmark-restore.R).
      restoredDT <- restoredDoseTable(state$values$DT, drugDefaults())
      DT <- restoredDT$table
      # A drug whose library row the link could not apply keeps the units in
      # force, and the link's numbers for it, written in its own units, are
      # converted into them (restoredLinkFactors()): a target, a threshold, a
      # measured level (Carbon's fourth UI audit, 2026-11-15, C2)
      linkFactors <- restoredLinkFactors(restoredLibrary$linkUnits, drugDefaults())
      if (length(linkFactors) > 0) {
        converted <- doseTableRescaleTargets(DT, linkFactors)
        if (is.null(converted)) {
          # A target past the ceiling in the units in force is left out
          drop <- as.character(DT$Drug) %in% names(linkFactors) & isTciUnit(DT$Units)
          DT <- DT[!drop, , drop = FALSE]
          showNotification("A target of the link's could not be converted to the units in force, and was left out.",
                           id = "bookmarkTargets", type = "warning", duration = 15)
        } else {
          DT <- converted
        }
      }
      restoredET <- restoredEventTable(state$values$ET, getEventDefaults(), eventTableInit)
      if (restoredDT$changed || restoredET$changed) {
        showNotification(paste("The link's dose or event table had entries that could not be read.",
                               "They were left out; check the tables."),
                         id = "bookmarkUnreadable", type = "warning", duration = 10)
      }
      target <- selectorFormat()
      restoredMaximum <- asScalarNumeric(state$input$maximum)
      restoredMinimum <- asScalarNumeric(state$input$minimum)
      if (is.na(restoredMinimum)) restoredMinimum <- 0
      res <- rebaseDoseTimes(DT, saved, target, procedureStart())
      if (res$ok) {
        setDoseTable(res$table, target)
        if (!isTRUE(restoredMaximum == snapMaximum(restoredMaximum, target[["unit"]]))) {
          # A Max time the unit's slider cannot show (an old bookmark's 16
          # weeks in days, 100 minutes from before the slider in hours): put
          # it on the slider's steps.
          syncTimeSliders(target[["unit"]], maximum = restoredMaximum, minimum = restoredMinimum)
        }
      } else {
        # Clock times and no readable procedure start: keep the table as
        # saved, and show the selectors that match it.
        setDoseTable(DT, saved)
        timeApi$showTimeSettings(saved[["unit"]], saved[["mode"]], restoredMaximum, restoredMinimum)
        showNotification("The dose table's clock times need a procedure start (HH:MM).",
                         type = "warning", duration = 10)
      }
      outputComments("doseTable:")
      outputComments(DT)
      ET <- restoredET$table
      eventTable(ET)
      # The link's events are in minutes after the link's own start, which
      # is the box's now: a later edit moves them from there
      if (isValidReferenceTime(procedureStart())) lastProcedureStart <<- procedureStart()
      outputComments("eventTable:")
      outputComments(ET)
      # Thresholds, when the bookmark has them; bookmarks made before they
      # were saved keep the defaults.
      linkEndCe <- state$values$endCe
      if (length(linkFactors) > 0 && length(linkEndCe) > 0 && !is.null(names(linkEndCe))) {
        moved <- names(linkEndCe) %in% names(linkFactors)
        value <- linkNumbers(linkEndCe)
        value[moved] <- value[moved] * linkFactors[names(linkEndCe)[moved]]
        linkEndCe <- stats::setNames(value, names(linkEndCe))
      }
      restored <- restoreThresholds(drugDefaults(), linkEndCe,
                                    state$values$macThreshold)
      drugDefaults(restored$drugDefaults)
      macThreshold(restored$macThreshold)
      outputComments("macThreshold:", restored$macThreshold)
      # Bookmarks made before the fat-free-mass switch existed were simulated
      # on total body weight; restore them that way so their output is unchanged.
      if (is.null(state$input$adjustToFFM)) {
        updateCheckboxInput(session, "adjustToFFM", value = FALSE)
      }
      # The Bayesian update's measured concentrations, read as untrusted
      # input like the tables, into the table in the format now shown
      restoredBayes <- restoredBayesObservations(state$values$bayes, drugDefaults()$Drug)
      if (length(linkFactors) > 0 && nrow(restoredBayes$table) > 0) {
        moved <- restoredBayes$table$Drug %in% names(linkFactors)
        restoredBayes$table$Concentration[moved] <- restoredBayes$table$Concentration[moved] *
          linkFactors[restoredBayes$table$Drug[moved]]
      }
      if (restoredBayes$changed) {
        showNotification("The link's measured concentrations had entries that could not be read. They were left out.",
                         id = "bookmarkBayesUnreadable", type = "warning", duration = 10)
      }
      if (nrow(restoredBayes$table) > 0) bayes$restore(restoredBayes$table)
      # A restored dose table with an illicit drug in it turns the opt-in on, so
      # the drug is not hidden from the table it is part of (R/illicit-drugs.R).
      if (any(isIllicitDrug(DT$Drug, drugDefaults()))) {
        updateCheckboxInput(session, "showIllicitDrugs", value = TRUE)
      }
    }, name = "onRestored()")
  })


  ######################
  ## Dose Table Loop  ##
  ######################

  # Holds the current state of the table as the user edits it without applying
  # it, to allow the user to make many successive edits quickly and undo/redo edits
  doseTableHistory <- undomanager::undomanager(type = "data.frame")$reactive()

  doseTableDraft <- reactive(doseTableHistory()$value)

  # What the browser tests (tests/playwright) read back from the server: the
  # draft, the committed table and its format.  exportTestValues() does
  # nothing unless the app runs with options(shiny.testmode = TRUE).
  exportTestValues(
    doseTableDraft = doseTableDraft(),
    doseTable = doseTable(),
    doseTableFormat = timeFormatString(doseTableFormat()),
    canUndo = doseTableHistory()$can_undo,
    canRedo = doseTableHistory()$can_redo,
    # what the dialogs' grids write (tests/playwright/table-grid.test.js)
    eventTable = eventTable(),
    endCe = as.list(stats::setNames(drugDefaults()$endCe, drugDefaults()$Drug)),
    colors = as.list(stats::setNames(drugDefaults()$Color, drugDefaults()$Drug))
  )

  # Open on what a ?scenario= or ?help= link asks for (R/deep-links.R).  Done
  # here rather than at Startup because setDoseTable() writes the history
  # above.  A ?help= link alone leaves the simulator behind the help empty,
  # as the session started; a bookmark keeps its own table (onRestored()).
  if (!startupMenuOpen && !is.null(deepLink$scenario) && !isBookmarkRestore(restoreValues)) {
    isolate({
      outputComments("Opening on scenario", deepLink$scenario$id, "from the URL")
      helpApi$loadScenario(deepLink$scenario)
    })
  }
  if (!is.null(deepLink$help)) {
    outputComments("Opening on help page", deepLink$help, "from the URL")
    helpPage(deepLink$help)
    bslib::nav_select("mainNav", "Help", session = session)
  }

  # Bumped to force the dose table to re-render when applyGasTableRules()
  # puts back exactly what the draft already held (e.g. the user deleted the
  # ventilation row): the reactiveVal would not invalidate on an identical value,
  # and the table on screen would then disagree with the draft.
  doseTableRefresh <- reactiveVal(0)

  # Weight for the default ventilation.  Must not block or error if the weight
  # box is momentarily empty, so fall back to 70 kg.
  gasVentilationWeight <- function() {
    tryCatch(isolate(weight()), error = function(e) 70)
  }

  showDoseTableFull <- function() {
    showNotification(sprintf("The dose table holds at most %d doses. Delete some doses first.",
                             MAX_DOSE_ROWS - 1L), type = "error", duration = 10)
  }

  # A table a dialog's button built (Add a dose, Skip next dose, Stop, Edit
  # doses, Suggest Dosing), written to doseTable(): the gas rules applied
  # first, as the doseTable() observer below would, so that the rows they add
  # are counted, then held to the row limit (doseTableWithinLimit()).
  # FALSE, said and nothing written, when the doses would be more than the
  # table holds; the caller then leaves its dialog open.  Each wrote its table
  # unchecked, and one row past the limit cost the dose grid, the plot and
  # the undo history (Carbon's UI audit, 2026-11-14, F3).
  commitDoseTable <- function(dt) {
    dt <- doseTableWithinLimit(applyGasTableRules(dt, gasVentilationWeight()))
    if (is.null(dt)) {
      showDoseTableFull()
      return(FALSE)
    }
    doseTable(dt)
    TRUE
  }

  # A table that holds an inhaled agent without the oxygen and ventilation
  # rows it needs, which the gas rules could not add for want of room (a
  # link's full table, below): nothing is simulated, where the gas engine read
  # the missing ventilation as 0 (CodeRabbit on PR #730)
  gasRowsProblem <- reactive({
    dt <- doseTable()
    if (is.null(dt) || !any(isGasDrug(dt$Drug))) return(NULL)
    if (identicalTable(applyGasTableRules(dt, gasVentilationWeight()), dt)) return(NULL)
    sprintf(paste("An inhaled agent needs its oxygen and ventilation rows, and the dose table,",
                  "which holds at most %d doses, has no room for them. Delete some doses."),
            MAX_DOSE_ROWS - 1L)
  })

  observeEvent(doseTable(), {
    # Every route into doseTable() -- Apply Changes, the add-dose dialog,
    # Suggest Dosing, restoring a bookmark -- passes through here, so this is
    # where the gas rules (see applyGasTableRules()) are enforced.  Fixing the
    # table sets doseTable() again, which brings this observer straight back.
    fixed <- applyGasTableRules(doseTable(), gasVentilationWeight())
    if (!identicalTable(fixed, doseTable())) {
      # The dialogs' tables arrive with the rules applied and within the row
      # limit (commitDoseTable()); a link's table (restoredDoseTable() keeps
      # MAX_DOSE_ROWS - 1 rows) may have no room for the rows they add, and
      # is then kept as it came rather than stored past the limit, which
      # would cost the grid and the plot.
      fixed <- doseTableWithinLimit(fixed)
      if (!is.null(fixed)) {
        doseTable(fixed)
        return()
      }
      showNotification(sprintf(paste(
        "The dose table holds at most %d doses, which leaves no room for the oxygen or",
        "ventilation row an inhaled agent needs. Delete some doses."), MAX_DOSE_ROWS - 1L),
        type = "warning", duration = NULL)
    }
    # A newly applied or restored table becomes the draft and starts a fresh
    # history: there are no earlier drafts to step back to.
    doseTableHistory()$do(doseTable())$clear()
  })

  observe({
    shinyjs::toggleState(
      "dosetable_apply",
      condition = !identicalTable(doseTableDraft(), doseTable())
    )
  })

  observe({
    shinyjs::toggleState("dosetable_undo", condition = doseTableHistory()$can_undo)
    shinyjs::toggleState("dosetable_redo", condition = doseTableHistory()$can_redo)
  })

  # After the grid's own observer (priority 0): a cell committed as the focus
  # left it for the button arrives with the click, and is in the draft that
  # is applied
  observeEvent(input$dosetable_apply, {
    shinyjs::disable("dosetable_apply")
    doseTable(doseTableDraft())
  }, priority = -1)

  observeEvent(input$dosetable_undo, {
    doseTableHistory()$undo()
  })

  observeEvent(input$dosetable_redo, {
    doseTableHistory()$redo()
  })

  # An edited dose table from the grid, made in the time format `stamp`,
  # stored as the new draft: what either grid sends goes through here
  # (R/dose-grid.R).  Returns TRUE if the draft changed.
  storeGridEdit <- function(data, stamp) {
    # An edit made in a grid drawn before a change of time unit or display
    # arrives in the old format: in minutes, say, when the table is now in
    # days.  Stored as it came, Apply would move every dose 1440-fold, so it
    # is converted; and the grid is redrawn, in the current format.
    format <- doseTableFormat()
    if (!is.null(stamp) && !identical(stamp, format)) {
      doseTableRefresh(doseTableRefresh() + 1)
      res <- rebaseDoseTimes(data, stamp, format, procedureStart())
      if (!res$ok) return(FALSE)
      data <- res$table
    }

    # make sure that table has changed before updating doseTable reactive
    if (identicalTable(doseTableDraft(), data)) return(FALSE)
    # add empty row at the bottom if needed: not once the table holds its
    # MAX_DOSE_ROWS - 1 doses, the grid's own rule, which offers none there
    # (a dose typed in it was cut; Gemini's production audit, 2026-11-08,
    # finding 7), and never past MAX_DOSE_ROWS - 1 rows in all, blank ones
    # included, which is what restoredDoseTable() keeps (CodeRabbit, PR #691)
    if (nrow(data) == 0 ||
        (nzchar(utils::tail(data, 1)$Drug) && nrow(data) < MAX_DOSE_ROWS - 1L)) {
      data[nrow(data) + 1, ] <- ""
    }

    # As soon as a gas is entered, apply the gas rules: oxygen alongside
    # nitrous oxide, flows rounded to 0.1 L/min, and a ventilation row.
    withVentilation <- applyGasTableRules(data, gasVentilationWeight())
    if (!identicalTable(withVentilation, data)) {
      # The rows the gas rules add must fit too: nitrous oxide typed into
      # the last rows took the draft past the limit, and the grid showed only
      # an error (Carbon's UI audit, 2026-11-14, F3).  Refused, the grid is
      # sent the draft again.
      data <- doseTableWithinLimit(withVentilation)
      if (is.null(data)) {
        showDoseTableFull()
        return(FALSE)
      }
      doseTableRefresh(doseTableRefresh() + 1)
    }

    doseTableHistory()$do(data)
    TRUE
  }

  # The dose grid (R/dose-grid.R, inst/www/dose-grid.js).  It sends the whole table once per committed edit, with the
  # number of edits it has sent (seq) and its time format; it is sent the
  # draft whenever the draft changes, with the seq of the last edit read
  # (ack), and ignores a draft sent before its latest edit was read.  A
  # draft that equals what it holds is not redrawn.
  {
    doseGridAck <- 0

    observe({
      session$sendInputMessage("doseGrid", doseGridDrugsMessage(drugDefaults(), drugChoices()))
    })

    observe({
      doseTableRefresh()
      draft <- doseTableDraft()
      format <- doseTableFormat()
      req(draft)
      # As the old grid's renderer did, a draft that fails the
      # table's own checks is not drawn: the grid says why instead.  (An
      # error raised here would end the session.)
      problem <- tryCatch({ validateDoseTableInput(draft); "" }, error = function(e) conditionMessage(e))
      if (nzchar(problem)) {
        session$sendInputMessage("doseGrid", list(error = problem))
        return()
      }
      outputComments("Sending the draft to the dose grid", level = DEBUG_LEVEL_VERBOSE)
      msg <- doseGridRowsMessage(draft, format, doseGridAck)
      msg$error <- ""
      session$sendInputMessage("doseGrid", msg)
    })

    # Ctrl+Z / Ctrl+Y in the grid (dose-grid.js): the Undo and Redo buttons'
    # work, read safely (an error here would end the session)
    observeEvent(input$doseGrid_history, {
      value <- input$doseGrid_history
      action <- if (is.list(value)) doseGridCell(value$action) else ""
      history <- doseTableHistory()
      if (identical(action, "undo") && isTRUE(history$can_undo)) history$undo()
      if (identical(action, "redo") && isTRUE(history$can_redo)) history$redo()
    })

    observeEvent(input$doseGrid, {
      profileCode({
        value <- doseGridValue(input$doseGrid)
        if (is.null(value)) return()
        # Only ever rises: the draft sent back answers the latest edit read
        doseGridAck <<- max(doseGridAck, value$seq)
        accepted <- doseGridAccepted(value$table, drugDefaults())
        if (accepted$changed) {
          showNotification(
            "Part of the dose table could not be read and was left blank. Check the table.",
            type = "warning", duration = 8
          )
        }
        if (value$truncated) {
          showNotification(
            sprintf("The dose table holds at most %d doses; the rows past them were not added.",
                    MAX_DOSE_ROWS - 1L),
            type = "warning", duration = 8
          )
        }
        value$table <- accepted$table
        if (nrow(value$table) == 0) {
          doseTableRefresh(doseTableRefresh() + 1)
          doseTableHistory()$do(blankDoseTableRow(doseTableDraft()))
          return()
        }
        # A draft left unchanged (an edit refused, or one that converts to
        # the draft already held) is sent again, so the grid shows it.
        if (!storeGridEdit(value$table, value$format)) doseTableRefresh(doseTableRefresh() + 1)
      }, name = "input$doseGrid observer")
    })
  }

  # Each one number, or nothing yet (req()): a vector sent for Age reached
  # assumedCreatinine()'s if() through the creatinine placeholder's observer
  # and ended the session (Carbon's UI audit, 2026-11-14, F6E).  An entry
  # that is not one number is said in the plot area (patientEntryProblem()).
  scalarInput <- function(x) {
    x <- asScalarNumeric(x)
    req(is.finite(x))
    x
  }
  weightUnit <- reactive(scalarInput(input$weightUnit))
  heightUnit <- reactive(scalarInput(input$heightUnit))
  ageUnit <- reactive(scalarInput(input$ageUnit))
  weight <- reactive(scalarInput(input$weight) * weightUnit())
  height <- reactive(scalarInput(input$height) * heightUnit())
  age <- reactive(scalarInput(input$age) * ageUnit())
  sex <- reactive({
    req(input$sex)
    if (length(input$sex) != 1L || !input$sex %in% SEX_VALUES) {
      stop(safeError("Invalid sex value."))
    }
    input$sex
  })

  # The Opioid tolerance field: each opioid's MEAC times its factor
  # (OPIOID_TOLERANCE_FACTORS).  "none" until the field reports, and for
  # bookmarks made before it existed.
  opioidTolerance <- reactive({
    x <- input$opioidTolerance
    if (is.null(x) || !x %in% OPIOID_TOLERANCE_VALUES) OPIOID_TOLERANCE_DEFAULT else x
  })

  adjustToFFM <- reactive({
    # TRUE until the checkbox reports, so the first simulation uses the default.
    if (is.null(input$adjustToFFM)) TRUE else isTRUE(input$adjustToFFM)
  })

  # The Minto / Schnider switch (R/alternativeModels.R): run the Schnider
  # propofol and Minto remifentanil models beside the library's own, as
  # companion entries of the drug list.  FALSE until the box reports, and for
  # bookmarks made before it existed.
  mintoSchnider <- reactive(isTRUE(input$mintoSchnider))
  # The Hamberg box (R/alternativeModels.R): run Hamberg's warfarin genotype
  # model beside the library's (Xue), a second INR line.  FALSE until the box
  # reports, and for bookmarks made before it existed.
  hamberg <- reactive(isTRUE(input$hamberg))
  # The antiseizure drug interactions (R/antiseizureInteractions.R)
  antiseizureInteractions <- reactive(isTRUE(input[[ASM_INTERACTIONS_INPUT]]))
  # The companion groups the user has turned on, as group keys for
  # recalculatePK() and the problem notices.
  companionGroupsOn <- reactive(c(
    if (mintoSchnider()) "mintoSchnider",
    if (hamberg()) "hamberg"
  ))

  # The companion groups actually drawn, as opposed to merely switched on: a
  # group counts only when a companion entry for one of its drugs was built (the
  # box is on, a group drug is dosed, and the model is defined for the patient).
  # The export reports this, so a ticked-but-disabled box -- left checked after
  # its drug was removed -- does not claim a line that is not on the plot.
  companionGroupsShown <- reactive({
    entries <- tryCatch(Filter(isAlternativeModelEntry, drugs()),
                        error = function(e) list())
    unique(vapply(entries, function(e) {
      # A companion that names its own group (the antiseizure interactions'
      # "no interactions" line) is not one of the alternative models.
      if (!is.null(e$companionGroup)) e$companionGroup else companionModelGroup(e$companionOf)
    }, character(1)))
  })

  # The companion entries of a drug in the current simulation (none unless
  # the switch is on and the drug has an alternative model)
  companionsOf <- function(drug) {
    Filter(function(entry) identical(entry$companionOf, drug), drugs())
  }

  # Each companion box is usable only while a drug in its group is in the dose
  # table; ticked while it is not, it simply has nothing to add.
  observe({
    DT <- tryCatch(doseTableClean(), error = function(e) NULL)
    for (group in names(COMPANION_MODEL_GROUPS)) {
      shinyjs::toggleState(
        COMPANION_MODEL_GROUPS[[group]]$input,
        condition = !is.null(DT) && any(DT$Drug %in% companionGroupDrugs(group))
      )
    }
  })

  # The antiseizure interactions box is usable while the dose table holds an
  # interaction the model applies (a dosed victim with a dosed perpetrator, or
  # carbamazepine, which induces itself).
  observe({
    DT <- tryCatch(doseTableClean(), error = function(e) NULL)
    shinyjs::toggleState(
      ASM_INTERACTIONS_INPUT,
      condition = !is.null(DT) &&
        nrow(activeAntiseizureInteractions(unique(DT$Drug))) > 0
    )
  })

  # Show only the Patient Profile fields and Graph Options controls that apply
  # to a drug in the dose table, committed or still being edited
  # (relevantFields() in R/patient-fields.R).  A hidden control keeps its
  # value, which does nothing while no drug reads it.
  observe({
    drugs <- c(doseTable()$Drug,
               tryCatch(doseTableDraft()$Drug, error = function(e) NULL))
    shown <- relevantFields(drugs, drugDefaults())
    for (field in setdiff(names(shown), c("plotMEAC", "plotInteraction")))
      shinyjs::toggle(id = paste0("field_", field), condition = shown[[field]])
    # The two optional Additional plots, inside their checkbox group
    choice <- function(value) sprintf("#addedPlots .checkbox:has(input[value='%s'])", value)
    shinyjs::toggle(selector = choice(PLOT_ID_MEAC), condition = shown[["plotMEAC"]])
    shinyjs::toggle(selector = choice(PLOT_ID_INTERACTION), condition = shown[["plotInteraction"]])
  })

  # The Serum creatinine field is optional, and a blank box says nothing about
  # what a blank MEANS: each renal model substitutes the normal creatinine for
  # the patient's age and sex (assumedCreatinine(), R/renalFunction.R).  The
  # placeholder shows that value in grey while the field is empty, and follows
  # age and sex, so the assumption is visible without hovering for the tip.
  # It is only a hint: it vanishes as soon as anything is typed, and the
  # engine still receives NULL for an empty field.
  observe({
    a <- tryCatch(age(), error = function(e) NULL)
    x <- tryCatch(sex(), error = function(e) NULL)
    req(a, x)
    shinyjs::runjs(sprintf(
      "var e = document.getElementById('creatinine'); if (e) e.placeholder = '%s';",
      creatininePlaceholder(a, x)))
  })

  # Vaporisers that together exceed 100% are a fresh gas that cannot exist.
  # The engine refuses them (gasSettingsAt()), which drugs() logs and
  # swallows, so the gas curves would simply vanish: say why instead.
  observe({
    DT <- tryCatch(doseTableClean(), error = function(e) NULL)
    problems <- if (is.null(DT)) character(0) else gasVaporiserProblems(DT)
    if (length(problems) == 0) {
      removeNotification("gasVaporisers")
    } else {
      showNotification(paste(problems, collapse = " "), id = "gasVaporisers",
                       type = "warning", duration = NULL)
    }
  })

  # An alternative model whose covariate terms give a negative volume or
  # clearance for this patient (a small child on Schnider's model, say) is
  # not drawn; say so rather than leave the second line silently missing.
  observe({
    problems <- character(0)
    DT <- tryCatch(doseTableIV(), error = function(e) NULL)
    groups <- companionGroupsOn()
    if (length(groups) > 0 && !is.null(DT)) {
      patient <- tryCatch(list(weight(), height(), age(), sex(), pregnant()), error = function(e) NULL)
      if (!is.null(patient)) {
        # Only the drugs whose group is turned on; a drug in a switched-off
        # group is not drawn, so its model is not in question.
        # and only the drugs drawn, those given a dose above 0
        drugsOn <- positiveDoseDrugs(DT)
        drugsOn <- drugsOn[companionModelGroup(drugsOn) %in% groups]
        cyp2c9 <- genotypes()$cyp2c9
        vkorc1 <- genotypes()$vkorc1
        problems <- alternativeModelProblems(drugsOn, patient[[1]], patient[[2]], patient[[3]], patient[[4]],
                                             pregnant = patient[[5]], cyp2c9 = cyp2c9, vkorc1 = vkorc1)
      }
    }
    if (length(problems) == 0) {
      removeNotification("alternativeModels")
    } else {
      showNotification(paste(problems, collapse = " "), id = "alternativeModels",
                       type = "warning", duration = NULL)
    }
  })

  osmolality <- reactive({
    # The default until the control reports, and for bookmarks made before it
    # existed (Shiny restores only the inputs a bookmark saved); one number,
    # whatever a forged input sent
    if (is.null(input$osmolality)) OSMOLALITY_DEFAULT else asScalarNumeric(input$osmolality)
  })

  # The genotypes, each one of its control's own choices: NULL on the first
  # pass, before the control has reported in, and anything at all from a link
  # or a forged input ("Invalid cyp2d6" took the plot down and made a click on
  # it end the session, and an empty value broke the emailed workbook:
  # Carbon's third UI audit, 2026-11-15, C1 and M5)
  genotypes <- reactive(list(
    cyp2d6 = choiceOf(input$cyp2d6, CYP2D6_VALUES, CYP2D6_DEFAULT),
    cyp2c19 = choiceOf(input$cyp2c19, CYP2C19_VALUES, CYP2C19_DEFAULT),
    cyp2c9 = choiceOf(input$cyp2c9, CYP2C9_VALUES, CYP2C9_DEFAULT),
    vkorc1 = choiceOf(input$vkorc1, VKORC1_VALUES, VKORC1_DEFAULT),
    cyp4f2 = choiceOf(input$cyp4f2, CYP4F2_VALUES, CYP4F2_DEFAULT),
    cyp2b6 = choiceOf(input$cyp2b6, CYP2B6_VALUES, CYP2B6_DEFAULT)
  ))

  # The Pregnant control reports "TRUE" or "FALSE" (NULL before it reports,
  # and in a bookmark made before it was live).  The flag is on only while
  # the control says so AND the patient is a woman of child-bearing age, the
  # rule that shows the control (R/pregnancy.R): a control hidden by a change
  # of age or sex keeps its last value, which must not keep the flag on.
  pregnant <- reactive({
    isTRUE(as.logical(input$pregnant)) && pregnancyPossible(age(), sex())
  })

  # The Pulmonary hypertension control reports "TRUE" or "FALSE" (NULL before
  # it reports, and in a bookmark made before it existed); anything else is
  # read as no.  Only the models of a drug with a separate PAH population
  # read it (R/pulmonaryHypertension.R).  A hidden control keeps its value,
  # as every hidden control does, and changes nothing while no drug reads it.
  pulmonaryHypertension <- reactive({
    isTRUE(as.logical(asScalarString(input$pulmonaryHypertension)))
  })

  # NULL when the field is blank: the renal models then assume a normal value.
  creatinine <- reactive({
    x <- input$creatinine
    # NaN is not blank: it is passed on, and the covariate check rejects it.
    if (is.null(x) || length(x) != 1 || (is.na(x) && !is.nan(x))) NULL else x
  })

  # NULL when the field is blank: the neonatal models then assume a term birth.
  gestationalAge <- reactive({
    x <- input$gestationalAge
    if (is.null(x) || length(x) != 1 || (is.na(x) && !is.nan(x))) NULL else x
  })

  # NULL when the field is blank: the albumin models then hold their own
  # reference value.
  albumin <- reactive({
    x <- input$albumin
    if (is.null(x) || length(x) != 1 || (is.na(x) && !is.nan(x))) NULL else x
  })

  # Likewise the AST: blank, and the AST models hold their reference.
  ast <- reactive({
    x <- input$ast
    if (is.null(x) || length(x) != 1 || (is.na(x) && !is.nan(x))) NULL else x
  })

  # The message for the plot area when Age, Weight or Height is not a number.
  # A blank field, or an unfinished entry such as a lone ".", reaches the
  # server as NULL or NA, and req() in age() etc. then stops everything
  # silently: the plot went blank with no word of why.  (An out-of-range
  # number is reported by checkNumericCovariates() in testCovariates().)
  patientEntryProblem <- reactive({
    fields <- c(age = "Age", weight = "Weight", height = "Height")
    unreadable <- vapply(names(fields), function(id) {
      x <- input[[id]]
      !is.numeric(x) || length(x) != 1 || is.na(x)
    }, logical(1))
    if (!any(unreadable)) return(NULL)
    paste0(
      "Enter a number for ", paste(fields[unreadable], collapse = ", "),
      " in the Patient Profile to see the simulation."
    )
  })

  # A Procedure start that cannot be read while times are clock times.
  # reqReference() waits for a readable one, so without this the plot would
  # simply stay blank, with "99:99" left in the box.  Blank is a problem only
  # once the browser's clock has arrived, which fills it: emptied after that,
  # the plot stayed blank and said nothing (Carbon's third UI audit,
  # 2026-11-15, M2).
  referenceTimeProblem <- reactive({
    if (!isClockFormat(selectorFormat())) return(NULL)
    reference <- procedureStart()
    if (isValidReferenceTime(reference)) return(NULL)
    if (!nzchar(trimws(reference))) {
      if (!clockArrived()) return(NULL)
      return("Enter the Procedure start as HH:MM, such as 08:30, to see the simulation in clock time.")
    }
    paste0("The Procedure start \"", reference, "\" is not a clock time. ",
           "Enter it as HH:MM, such as 08:30.")
  })

  testCovariates <- reactive({
    profileCode({
      outputComments("In testCovariates", level = DEBUG_LEVEL_VERBOSE)
      req(weight(), height(), age(), sex())
      errorFxn <- function(msg) showModal(modalDialog(title = NULL, msg))
      checkNumericCovariates(age(), weight(), height(), errorFxn,
                             osmolality = osmolality(),
                             creatinine = creatinine(),
                             gestationalAge = gestationalAge(),
                             albumin = albumin(),
                             ast = ast())
    }, name = "testCovariates() reactive")
  })

  # The inhaled gases are simulated by a separate engine and must be kept out of
  # the intravenous path: recalculatePK() would call eval(call("air", ...)) and
  # fail, because the gases have no drugs_*.R covariate function, and simCpCe()
  # converts every dose to a mass, which a gas tension is not.
  doseTableIV <- reactive({
    DT <- doseTableClean()
    if (is.null(DT)) return(NULL)
    DT <- DT[!isGasDrug(DT$Drug), , drop = FALSE]
    if (nrow(DT) == 0) NULL else DT
  })

  # Every gas is simulated in ONE call, because they share the breathing circuit
  # and the alveolar ventilation: total fresh gas flow is the sum of the air,
  # oxygen and nitrous oxide rows, so changing any one of them changes every gas
  # trajectory.  There is deliberately no per-gas change detection.
  gases <- reactive({
    profileCode({
      outputComments("In gases", level = DEBUG_LEVEL_VERBOSE)
      req(testCovariates(), is.null(timeUnitViolation()), is.null(gasRowsProblem()))
      DT <- doseTableClean()
      if (is.null(DT)) return(list())
      sim <- simulateGases(
        DT,
        weight  = weight(),
        physiology = physiology(),
        temperature = bodyTemperature(),
        altitude = altitude(),
        endobronchial = endobronchial(),
        age     = age(),
        maximum = plotMaximum()
      )
      gasRows <- DT[isGasDrug(DT$Drug), , drop = FALSE]

      # "Time until threshold" for the gases is only worked out when it is
      # being shown: it is an eigen-decomposition per gas and a root-find per
      # plotted point, which is cheap but not free.
      washout <- NULL
      if (isTRUE(plotRecovery()) && !is.null(sim)) {
        washout <- gasWashout(
          sim,
          data.frame(Time = as.numeric(gasRows$Time), Drug = gasRows$Drug,
                     Dose = as.numeric(gasRows$Dose)),
          weight = weight()
        ) |> profileCode("gasWashout() in gases()")
      }

      gasDrugEntries(
        sim,
        gasRows,
        drugDefaults(),
        plotMaximum(),
        washout = washout,
        age     = age(),
        macThreshold = macThreshold()
      )
    }, name = "gases() reactive")
  })

  # The intravenous drugs as last simulated, so that processdoseTable() only
  # re-simulates the drugs whose inputs changed.  A plain variable, not a
  # reactive value: reading it must not make drugs() depend on itself.
  ivSimulationCache <- NULL

  # The PK of each drug in a dose table (times in minutes), at the session's
  # patient and options.  drugs() reads it for the dose table; Suggest Dosing
  # reads it for a drug the table does not hold yet, which it once wrote into
  # the table as a placeholder row -- and a write to doseTable() discards the
  # unapplied edits and the undo history, so a suggestion that then failed
  # lost them (CodeRabbit on #674, fixed 2026-11-04).
  # The physiology sliders (R/physiology.R), as multiples of normal function.
  # Debounced, so a slider dragged across its range resolves the PK once it
  # rests rather than at every step.
  physiology <- debounce(reactive({
    out <- physiologyFromInputs(stats::setNames(
      lapply(PHYSIOLOGY_FACTORS, function(f) input[[physiologyInputId(f)]]),
      PHYSIOLOGY_FACTORS
    ))
    # Body temperature lowers the intravenous drugs' elimination capacity too
    # (physiologyTemperatureFactor())
    out[["temperature"]] <- bodyTemperatureFromInput(input$physiology_temperature)
    out
  }), PHYSIOLOGY_DEBOUNCE_MS)

  # Body temperature, C (R/physiology.R): the inhaled agents' solubility and
  # MAC.  An unreadable value is 37.
  bodyTemperature <- debounce(reactive({
    bodyTemperatureFromInput(input$physiology_temperature)
  }), PHYSIOLOGY_DEBOUNCE_MS)

  # Altitude, metres (R/altitude.R): the barometric pressure the inhaled
  # anesthetics are given at.  An unreadable value is sea level.
  altitude <- debounce(reactive({
    altitudeFromInput(input$physiology_altitude)
  }), PHYSIOLOGY_DEBOUNCE_MS)

  # Endobronchial intubation (R/endobronchial.R): one lung ventilated
  endobronchial <- reactive(endobronchialFromInput(input$endobronchialIntubation))

  observeEvent(input$physiologyReset, {
    for (f in PHYSIOLOGY_FACTORS)
      updateSliderInput(session, physiologyInputId(f), value = 0)
    updateSliderInput(session, "physiology_temperature",
                      value = PHYSIOLOGY_TEMPERATURE_DEFAULT)
    updateSliderInput(session, "physiology_altitude", value = ALTITUDE_DEFAULT)
    updateCheckboxInput(session, "endobronchialIntubation", value = FALSE)
  })

  # A link or a scenario that sets the physiology away from normal shows the
  # sliders, which otherwise wait behind physiologyIntro() (Shafer,
  # 2026-11-17): a changed slider is never hidden.
  observeEvent(list(physiology(), altitude(), endobronchial()), {
    if (!physiologyIsNormal(physiology()) || altitude() != ALTITUDE_DEFAULT ||
        endobronchial())
      session$sendCustomMessage("physiology_show", list())
  })

  output$physiologyNote <- renderUI({
    # The drugs whose model chosen for this patient the sliders cannot scale
    entries <- tryCatch(bayesUpdate()$drugs, error = function(e) NULL)
    skipped <- names(Filter(function(e) isTRUE(e$slidersSkipped), entries))
    physiologyNote(physiology(), doseTableClean()$Drug,
                   temperature = bodyTemperature(), skipped = skipped)
  })

  # `normalPhysiology`: build at normal physiology, whatever the sliders say
  # (the Bayesian update's fit, bayesUpdate())
  drugPK <- function(doseTable, individual = NULL, normalPhysiology = FALSE) {
    recalculatePK(
        NULL,
        drugDefaults(),
        doseTable,
        age = age(),
        weight = weight(),
        height = height(),
        sex = sex(),
        # NULL on the first pass, before the control has reported in; and
        # one of the control's own choices, which a link or a forged input
        # replaced with anything ("Invalid cyp2d6" took the plot down and
        # made a click on it end the session: Carbon's third UI audit,
        # 2026-11-15, C1)
        cyp2d6 = genotypes()$cyp2d6,
        cyp2c19 = genotypes()$cyp2c19,
        cyp2c9 = genotypes()$cyp2c9,
        vkorc1 = genotypes()$vkorc1,
        cyp4f2 = genotypes()$cyp4f2,
        cyp2b6 = genotypes()$cyp2b6,
        osmolality = osmolality(),
        creatinine = creatinine(),
        gestationalAge = gestationalAge(),
        albumin = albumin(),
        ast = ast(),
        pregnant = pregnant(),
        pulmonaryHypertension = pulmonaryHypertension(),
        adjustToFFM = adjustToFFM(),
        opioidTolerance = opioidTolerance(),
        alternativeModels = companionGroupsOn(),
        antiseizureInteractions = antiseizureInteractions(),
        individual = individual,
        physiology = if (normalPhysiology) NULL else physiology()
      )
  }

  # The fits of the last update, reused for any drug whose inputs have not
  # changed (bayesianUpdateDrugs()).  A plain variable, as ivSimulationCache.
  bayesFitCache <- NULL

  # The drugs' PK, with any drug that has measured concentrations updated to
  # the individual (the maximum a posteriori estimate of its clearance and
  # volume, R/bayesian.R) and the model's own kept beside it as
  # "<drug> (population)".  Without measurements, the drugs' PK as it was.
  bayesUpdate <- reactive({
    profileCode({
      req(testCovariates(), doseTableClean(), is.null(timeUnitViolation()), is.null(scheduleProblem()))
      DT <- doseTableIV()
      observations <- bayes$observations()
      # The PK of the drugs drawn, those given a dose above 0, and of the
      # drugs measured, whose levels are said to have no dose to explain
      # them rather than to be missing from the table.  A drug whose every
      # row is 0 is not drawn (drawnDrugEntries(), R/drug-pipeline.R), and
      # building the PK of the 223 intravenous drugs of the startup menu
      # took 2 s.
      if (!is.null(DT))
        DT <- DT[DT$Drug %in% c(positiveDoseDrugs(DT), observations$Drug), , drop = FALSE]
      priors <- drugPK(DT) |> profileCode("recalculatePK() in bayesUpdate()")
      if (nrow(observations) == 0) return(list(drugs = priors, fits = list(), problems = character(0)))
      # The fit is made at normal physiology and the sliders are applied to
      # the individual afterwards (Shafer, 2026-11-16; bayesianAtPhysiology())
      normal <- physiologyIsNormal(physiology())
      fitPriors <- if (normal) priors else drugPK(DT, normalPhysiology = TRUE)
      # A search longer than a few seconds is covered by the page's Working
      # cover (app.js), which replaced a spinner of its own here
      result <- bayesianUpdateDrugs(
        fitPriors, DT, eventTableClean(), observations,
        pkVariability = bayes$pkVariability(),
        residualVariability = bayes$residualVariability(),
        rebuild = function(drug, individual) {
          drugPK(DT[DT$Drug == drug, , drop = FALSE], individual = individual,
                 normalPhysiology = TRUE)[[drug]]
        },
        antiseizureInteractions = antiseizureInteractions(),
        fits = bayesFitCache,
        # A level after the end of the plot is set aside, so the fit never
        # simulates further than the plot does, whose limits are kept above
        maximum = plotMaximum()
      )
      # The failures too, so that a fit that ran out of time is not tried
      # again until its inputs change (bayesianUpdateDrugs())
      bayesFitCache <<- result$cache
      if (!normal)
        result$drugs <- bayesianAtPhysiology(result$drugs, priors, function(drug, individual) {
          drugPK(DT[DT$Drug == drug, , drop = FALSE], individual = individual)[[drug]]
        })
      result
    }, name = "bayesUpdate() reactive")
  })

  # The measured concentrations the plot draws: those of the drugs it draws
  bayesPoints <- reactive({
    observations <- bayes$observations()
    DT <- tryCatch(doseTableIV(), error = function(e) NULL)
    if (is.null(DT)) return(NULL)
    observations[observations$Drug %in% DT$Drug, , drop = FALSE]
  })

  drugs <- reactive({
    profileCode({
      outputComments("In drugs", level = DEBUG_LEVEL_VERBOSE)
      # Nothing is simulated while the time-units rule is broken: the plot
      # area says why (output$PlotSimulation).
      req(testCovariates(), doseTableClean(), is.null(timeUnitViolation()), is.null(scheduleProblem()))

      # The PK, updated from any measured concentrations (bayesUpdate()), of
      # the drugs drawn: those given a dose above 0 and the metabolites they
      # form, with their rows (R/drug-pipeline.R)
      DT <- doseTableIV()
      newDrugs <- drawnDrugEntries(bayesUpdate()$drugs, positiveDoseDrugs(DT))
      if (!is.null(DT)) DT <- DT[DT$Drug %in% names(newDrugs), , drop = FALSE]

      newDrugs <- processdoseTable(
        DT,
        eventTableClean(),
        newDrugs,
        plotMaximum(),
        plotRecovery(),
        cache = ivSimulationCache,
        plotMinimum = plotMinimum(),
        antiseizureInteractions = antiseizureInteractions()
      ) |> profileCode("processdoseTable() in drugs()")
      ivSimulationCache <<- newDrugs

      # The inhaled gases are simulated as one group and appended as their own
      # entries, so that simulationPlot() treats them like any other series and
      # a dose table containing only inhaled agents still plots.  A failure in
      # the gas engine is logged and leaves the intravenous drugs plotting;
      # Shiny's own "stop, nothing to show yet" signal is let through.
      gasEntries <- tryCatch(gases(), error = function(e) {
        if (inherits(e, "shiny.silent.error")) stop(e)
        outputComments("Gas engine failed:", conditionMessage(e))
        list()
      })

      # Optionally let the opioids lower MAC.  Done here rather than in gases()
      # because it needs the opioids' effect-site concentrations, and this is
      # the first place both are in hand; it also keeps the gas simulation from
      # re-running when only the tick box or an opioid dose changes.
      if (isTRUE(input$opioidMacInteraction)) {
        gasEntries <- applyOpioidMacInteraction(gasEntries, newDrugs)
      }

      c(newDrugs, gasEntries)
    }, name = "drugs() reactive")
  })

  ###########################
  ## Main Observation Loop ##
  ###########################

  doseTableClean <- reactive({
    profileCode({
      outputComments("In doseTableClean", level = DEBUG_LEVEL_VERBOSE)
      validateDoseTableInput(doseTable(), drugDefaults())
      DT <- cleanDoseTable(doseTable())
      # Read in the format the table was typed in, never the selectors, which
      # run ahead of the table while it is being converted.
      format <- doseTableFormat()
      DT$Time <- displayTimeToMinutes(DT$Time, reqReference(format), format[["unit"]])
      DT <- DT[
        DT$Drug  != "" &
          DT$Units != "" &
          !is.na(DT$Dose) &
          !is.na(DT$Time), ]
      if (nrow(DT) == 0) {
        DT <- NULL
      } else {
        DT <- DT[order(DT$Drug, DT$Time), ]
      }

      DT
    }, name = "doseTableClean() reactive")
  })

  eventTableClean <- reactive({
    profileCode({
      outputComments("In eventTableClean", level = DEBUG_LEVEL_VERBOSE)
      validateEventTableInput(eventTable(), eventDefaults())
      ET <- eventTable()
      # Event times are stored in minutes whatever the time settings; the
      # dialogs convert them.  (Not through as.character(), which keeps only
      # 15 significant digits.)
      if (!is.numeric(ET$Time)) ET$Time <- as.numeric(as.character(ET$Time))
      ET
    }, name = "eventTableClean() reactive")
  })


  plotInfo <- reactive({
    profileCode({
      req(doseTableClean())
      # While the selectors and the dose table's format disagree the table is
      # being converted (or a scenario or bookmark is being put in place):
      # wait for the browser and the conversion to catch up.
      req(identical(doseTableFormat(), selectorFormat()))

      requestedMaximum <- suppressWarnings(as.numeric(maximumDebounced()))
      # Not checked against a list of lengths any more, the slider being
      # continuous, but still bounded: from the shortest step to the longest
      # length any unit offers
      if (!is_valid_number(requestedMaximum, min(TIME_SLIDER_STEPS), max(MAX_TIME_VALUES))) {
        stop(safeError("Invalid maximum simulation time."))
      }
      unit <- timeUnit()
      slider <- timeSliderRange(unit)
      # A Max time past this unit's slider: the browser has not yet caught up
      # with a change of unit (syncTimeSliders()).
      req(requestedMaximum <= slider$max)
      # Min time, where the axis starts.  Everything is still simulated from
      # 0; only the window drawn, and the resolution inside it
      # (simulationTimeGrid()), move.  A Min time at or past Max time is a
      # moment before the observers above pull them apart.
      plotMinimum <- suppressWarnings(as.numeric(minimumDebounced()))
      if (!is_valid_number(plotMinimum, 0)) plotMinimum <- 0
      req(plotMinimum < requestedMaximum)
      # The server has just set the sliders (a change of unit, a scenario, a
      # restored bookmark): wait for the browser to report the new values.
      req(!sliderPending())
      maxTimes <- MAX_TIMES[[unit]]

      plotMaximum <- requestedMaximum
      steps <- axisTimeStep(plotMinimum, plotMaximum, unit)
      maxTime <- max(as.numeric(doseTableClean()$Time),
                     as.numeric(eventTableClean()$Time),
                     na.rm = TRUE)

      # Lengthen the plot when the last dose or event comes within a margin of
      # its end, using the ticks of the unit's next longer Max time (minutes
      # and hours: 30 minutes, as always).  Never past the unit's longest Max
      # time: a longer plot is a matter of choosing a larger unit.
      margin <- TIME_EXTEND_MARGIN[[unit]]
      longest <- slider$max
      if ((maxTime + margin - 1) >= plotMaximum) {
        steps <- maxTimes$steps[maxTimes$times >= (maxTime + margin)][1]
        if (is.na(steps)) steps <- utils::tail(maxTimes$steps, 1)
        plotMaximum <- ceiling((maxTime + margin)/steps) * steps
        if (plotMaximum > longest) {
          plotMaximum <- longest
          steps <- utils::tail(maxTimes$steps, 1)
        }
        # A window that does not start at 0 is ticked for its own width
        if (plotMinimum > 0) steps <- axisTimeStep(plotMinimum, plotMaximum, unit)
      }

      # Doses and events the plot cannot reach (see the observer below)
      beyond <- c(
        doses = sum(doseTableClean()$Time >= plotMaximum),
        events = sum(as.numeric(eventTableClean()$Time) >= plotMaximum)
      )
      list(plotMinimum = plotMinimum, plotMaximum = plotMaximum, steps = steps, beyond = beyond)
    }, name = "plotInfo() reactive")
  })

  plotMaximum <- reactive(plotInfo()$plotMaximum)
  # Where the axis starts: Min time, 0 unless the slider is moved
  plotMinimum <- reactive(plotInfo()$plotMinimum)
  # Tick spacing, in minutes, for the current time unit
  steps       <- reactive(plotInfo()$steps)

  # The length of a plot in words, as the Max time list says it ("14 days"),
  # or, for a plot lengthened past the last dose, as a duration
  plotLengthLabel <- function(maximum) {
    label <- maxTimeLabel(maximum, timeUnit())
    if (is.na(label)) formatMinutes(maximum) else label
  }

  observe({
    info <- tryCatch(plotInfo(), error = function(e) NULL)
    if (is.null(info)) return()  # waiting: leave things as they are
    beyond <- info$beyond
    if (sum(beyond) == 0) {
      removeNotification("timeBeyondPlot")
      return()
    }
    what <- c(if (beyond[["doses"]] > 0) pluralNoun(beyond[["doses"]], "dose row"),
              if (beyond[["events"]] > 0) pluralNoun(beyond[["events"]], "event"))
    showNotification(
      paste0(paste(what, collapse = " and "), if (sum(beyond) == 1) " falls" else " fall",
             " after the end of the plot (", plotLengthLabel(info$plotMaximum),
             "). Choose a longer Max time or a larger time unit."),
      id = "timeBeyondPlot", type = "warning", duration = NULL
    )
  })

  # Part of the time-units rules: TCI target rows and the inhaled agents are
  # simulated only on plots of a week or less (ACUTE_MAX_PLOT_MINUTES).  Over
  # weeks the TCI controller would write tens of thousands of rate changes, and
  # the gas engine's uptake coupling is frozen over steps of maximum/601.
  # NULL when the rule is kept, else the message to show; the plot is then
  # replaced by the message and nothing is simulated.
  timeUnitViolation <- reactive({
    DT <- tryCatch(doseTableClean(), error = function(e) NULL)
    if (is.null(DT)) return(NULL)
    acute <- DT$Units %in% tciUnits | isGasDrug(DT$Drug)
    if (!any(acute)) return(NULL)
    maximum <- tryCatch(plotMaximum(), error = function(e) NULL)
    if (is.null(maximum) || maximum <= ACUTE_MAX_PLOT_MINUTES) return(NULL)
    rows <- ifelse(DT$Units[acute] %in% tciUnits,
                   paste(DT$Drug[acute], tolower(DT$Units[acute])),
                   DT$Drug[acute])
    # A gas row with its flow left blank (the oxygen the startup menu adds
    # with an inhaled agent) is not in the cleaned table, but while it is in
    # the dose table the gas rules keep adding ventilation back: name it too,
    # so that removing the rows named clears the rule.
    allDrugs <- as.character(doseTable()$Drug)
    rows <- c(rows, allDrugs[isGasDrug(allDrugs)])
    paste0(
      "Target-controlled infusions and inhaled agents are simulated only on plots of ",
      "7 days or less, and this plot is ", plotLengthLabel(maximum), " (",
      paste(unique(rows), collapse = ", "),
      "). Remove those rows or choose a Max time of 7 days or less."
    )
  })

  # The repeats a scheduled dose expands to are each simulated, so a frequent
  # schedule over a long plot is refused before it is simulated; see
  # MAX_SCHEDULED_DOSES.  Said in the plot area (plotMessage()), like the rule
  # above, and drugs() waits while it stands.
  scheduleProblem <- reactive({
    DT <- tryCatch(doseTableClean(), error = function(e) NULL)
    if (is.null(DT)) return(NULL)
    maximum <- tryCatch(plotMaximum(), error = function(e) NULL)
    if (is.null(maximum)) return(NULL)
    n <- scheduledDoseCount(DT, maximum)
    if (n <= MAX_SCHEDULED_DOSES) return(NULL)
    paste0(
      "The scheduled doses repeat ", format(n, big.mark = ","), " times over this plot, more than the ",
      format(MAX_SCHEDULED_DOSES, big.mark = ","), " the app can simulate. Shorten the Max time, ",
      "choose a less frequent schedule, or simulate fewer scheduled drugs."
    )
  })

  observe({
    violation <- timeUnitViolation()
    if (is.null(violation)) {
      removeNotification("timeUnitRule")
    } else {
      showNotification(violation, id = "timeUnitRule", type = "warning", duration = NULL)
    }
  })

  # The time settings open as an intravenous plot, minutes from 0 to
  # TIME_MAXIMUM_DEFAULT (app_ui()).  The first time a repeating oral dose is
  # prescribed (hasRepeatingOralDose(): a scheduled PO or SL dose, or a daily
  # oral rate) while they are still exactly that, they switch to
  # REPEATING_ORAL_TIME_UNIT from 0 to REPEATING_ORAL_PLOT_MINUTES, once per
  # session (Shafer, 2026-11-13).  A drug given both ways starts on the
  # intravenous plot and moves only when the repeating oral dose appears.
  # Never again after that, nor once a scenario, a restored link or the
  # long-term prompt has set the time (showTimeSettings()), nor over settings
  # the user has changed: these are defaults, not rules.  An environment, not
  # a reactiveVal, so that settling it invalidates nothing; created with
  # timeApi, which settles it.
  observe({
    if (repeatingOralSwitch$settled) return()
    DT <- tryCatch(doseTableClean(), error = function(e) NULL)
    if (!hasRepeatingOralDose(DT)) return()
    minimum <- suppressWarnings(as.numeric(isolate(input$minimum)))
    atDefault <- identical(isolate(timeUnit()), TIME_UNIT_DEFAULT) &&
      identical(isolate(input$timeMode), TIME_MODE_DEFAULT) &&
      isTRUE(suppressWarnings(as.numeric(isolate(input$maximum))) == TIME_MAXIMUM_DEFAULT) &&
      (length(minimum) == 0 || isTRUE(minimum == 0))
    if (!atDefault) {
      # The user chose the time settings; the default no longer applies
      repeatingOralSwitch$settled <- TRUE
      return()
    }
    # As for Show 365 days below: a dialog showing times in minutes would be
    # read in days once the table is converted, so close it first
    removeModal()
    timeApi$showTimeSettings(REPEATING_ORAL_TIME_UNIT, "relative", REPEATING_ORAL_PLOT_MINUTES)
    showNotification(
      paste0("A repeating oral dose was prescribed, so the time units are now days, from 0 to ",
             REPEATING_ORAL_PLOT_MINUTES / MINS_PER_DAY,
             " days. Change them in the Time panel; this happens only once."),
      id = "repeatingOralTime", type = "message", duration = 10
    )
  })

  # A drug that acts over weeks to months (LONG_TERM_DRUGS) added to a plot
  # shorter than a week: offer, once per addition, to show a year.  Not done
  # automatically: the user may have meant a short plot.  The button goes
  # through timeApi$showTimeSettings(), the path a scenario takes, and the
  # conversion observer then converts the dose table.
  longTermSeen <- character(0)  # plain variable: the long-term drugs already handled
  observe({
    # FALSE, not NULL, while the table cannot be read (req() waiting on a
    # half-typed Procedure start, or a validation error): NULL is a table with
    # no rows, and reading the two alike would empty longTermSeen and offer the
    # prompt again for a drug that never left the table.
    DT <- tryCatch(doseTableClean(), error = function(e) FALSE)
    if (isFALSE(DT)) return()
    present <- intersect(LONG_TERM_DRUGS, DT$Drug)
    longTermSeen <<- intersect(longTermSeen, present)  # a drug removed may be added again
    added <- setdiff(present, longTermSeen)
    if (length(added) == 0) return()
    maximum <- tryCatch(plotMaximum(), error = function(e) NULL)
    if (is.null(maximum)) return()  # waiting; this runs again when it is known
    longTermSeen <<- c(longTermSeen, added)
    if (maximum >= MINS_PER_WEEK) return()
    showNotification(
      paste0(paste(added, collapse = " and "), if (length(added) == 1) " acts" else " act",
             " over weeks to months; this plot is ", plotLengthLabel(maximum), "."),
      action = actionLink("showLongTermTime", "Show 365 days"),
      id = "longTermPrompt", type = "message", duration = NULL
    )
  })

  observeEvent(input$showLongTermTime, {
    # The notification sits above any open dialog (Shiny's notification panel
    # is drawn over Bootstrap's modals), so this can be clicked while the
    # add-dose, add-event, edit or Suggest Dosing dialog is open.  Each of those
    # reads what was typed in the format of the dose table when it is
    # submitted, so a time typed as 30 minutes would be stored as 30 days once
    # the table had been converted.  Close the dialog first.
    removeModal()
    removeNotification("longTermPrompt")
    timeApi$showTimeSettings("days", "relative", LONG_TERM_PLOT_MINUTES)
  })


  # Off under normalization, where the box is hidden (app_ui()) but keeps its
  # value: the time until threshold is not drawn then, and computing it for
  # every drug of a long plot is the costliest step of the simulation.
  plotRecovery <- reactive({
    isTRUE(input$showThreshold) && identical(input$normalization, NORMALIZE_NONE)
  })

  # The % MEAC overlay (R/meacOverlay.R) and the time until threshold share
  # the panels' right-hand edge, so ticking either unticks the other.  Until
  # the untick arrives the threshold wins, as simulationPlot() also enforces.
  plotMEACOverlay <- reactive({
    isTRUE(input$showMEAC) && !isTRUE(input$showThreshold) &&
      identical(input$normalization, NORMALIZE_NONE)
  })
  observeEvent(input$showThreshold, {
    if (isTRUE(input$showThreshold) && isTRUE(input$showMEAC))
      updateCheckboxInput(session, "showMEAC", value = FALSE)
  })
  observeEvent(input$showMEAC, {
    if (isTRUE(input$showMEAC) && isTRUE(input$showThreshold))
      updateCheckboxInput(session, "showThreshold", value = FALSE)
  })

  # The plot's own controls, held to their choices (choiceOf()): a link or a
  # forged input carried a vector or an unknown value into setLinetypes() and
  # simulationPlot() (Carbon's third UI audit, 2026-11-15, C1)
  normalizationInput <- reactive(choiceOf(input$normalization, PLOT_NORMALIZATIONS, NORMALIZE_NONE))
  typicalInput <- reactive(choiceOf(input$typical, PLOT_TYPICAL_CHOICES, "Range"))
  linetypes <- reactive({
    setLinetypes(normalizationInput(),
                 choiceOf(input$plasmaLinetype, PLOT_LINE_TYPES, "blank"),
                 choiceOf(input$effectsiteLinetype, PLOT_LINE_TYPES, "solid"))
  })

  simulationPlotRetval <- reactive({
    req(input$plotWidth)
    if (!is_valid_number(input$plotWidth, MIN_PLOT_WIDTH, MAX_PLOT_WIDTH)) {
      stop(safeError("Invalid plot width."))
    }
    if (!is_valid_number(input$yaxisHeight, MIN_YAXIS_HEIGHT, MAX_YAXIS_HEIGHT)) {
      stop(safeError("Invalid plot height."))
    }
    profileCode({
      outputComments("In simulationPlotRetval", level = DEBUG_LEVEL_VERBOSE)
      req(doseTableClean(), testCovariates(),
          length(input$plasmaLinetype) > 0, length(input$effectsiteLinetype) > 0)

      DT <- doseTableClean()
      ET <- eventTableClean()

      # The breaks are minutes, like the data; only the labels and the title
      # are in the display unit, or clock time (utils-time-display.R).  The
      # axis runs from plotMinimum() to plotMaximum() even when the tick step
      # divides neither.
      xBreaks <- axisTimeBreaks(plotMinimum(), plotMaximum(), steps())
      xLabels <- axisTimeLabels(xBreaks, timeUnit(), referenceTime())
      xAxisLabel <- timeAxisTitle(timeUnit(), referenceTime())
      if (referenceTime() != REFERENCE_TIME_NONE) {
        # Tidies the procedure start as typed ("8:00" becomes "08:00").  It
        # is written from the reference itself, never from an axis label,
        # which in any other mode would be a number of hours or days.
        updateNumericInput(session, "referenceTime",
                           value = deltaToClockTime(referenceTime(), 0))
      }

      plotMEAC              <- PLOT_ID_MEAC        %in% input$addedPlots
      plotInteraction        <- PLOT_ID_INTERACTION %in% input$addedPlots
      plotCost               <- "Cost"                %in% input$addedPlots
      plotEvents             <- PLOT_ID_EVENTS      %in% input$addedPlots
      normalization          <- normalizationInput()
      typical                <- typicalInput()
      logY                   <- isTRUE(input$logY)
      if (plotRecovery() || plotMEACOverlay() || plotEvents || plotInteraction) logY <- FALSE

      plasmaLinetype <- linetypes()$plasmaLinetype
      effectsiteLinetype <- linetypes()$effectsiteLinetype

      simulationPlot(
        drugs = drugs(),
        events = ET,
        drugDefaults = drugDefaults(),
        eventDefaults = eventDefaults(),
        xBreaks = xBreaks,
        xLabels = xLabels,
        xAxisLabel = xAxisLabel,
        xMinimum = plotMinimum(),
        xMaximum = plotMaximum(),
        plasmaLinetype = plasmaLinetype,
        effectsiteLinetype = effectsiteLinetype,
        normalization = normalization,
        plotMEAC = plotMEAC,
        plotInteraction = plotInteraction,
        plotCost = plotCost,
        plotEvents = plotEvents,
        plotRecovery = plotRecovery(),
        plotMEACOverlay = plotMEACOverlay(),
        plotCompartments = isTRUE(input$showCompartments),
        typical = typical,
        logY = logY,
        yAxisHeight = input$yaxisHeight,
        width = input$plotWidth,
        # renderPlot() draws at the screen's pixel ratio, which is what the
        # graphics device's limit is measured against (maxPlotDevicePixels())
        pixelRatio = if (is.null(session$clientData$pixelratio)) 1 else session$clientData$pixelratio,
        # The Bayesian update's measured concentrations, as points
        observations = bayesPoints()
      )
    }, name = "simulationPlotRetval() reactive")
  })

  plotObjectReactive <- reactive({
    simulationPlotRetval()$plotObject
  })

  allResultsReactive <- reactive({
    simulationPlotRetval()$allResults
  })

  plotResultsReactive <- reactive({
    simulationPlotRetval()$plotResults
  })

  # Both, or NULL while the simulation or the plot cannot be drawn
  plotResultsOrNull <- function() {
    tryCatch(list(all = allResultsReactive(), plot = plotResultsReactive()),
             error = function(err) NULL)
  }

  plotHeight <- reactive({
    simulationPlotRetval()$plotHeight
  })

  # Export -----------------------------
  # The Export panel offers the email's three files for download, one at a
  # time -- the PowerPoint deck, the spreadsheet and the plot as an image --
  # and still emails all three (R/sendSlide.R writes each).  Both ask
  # exportProblem() first and read the simulation through exportValues().

  # Why there is no plot to export, or NULL: the simulation cannot run, there
  # is no dose, or the plot failed or is not drawn yet.  Said, where before
  # the email observer stopped in silence (a req()), reported the server's
  # generic failure, or ended the session (validateDoseTableInput();
  # adversarial UI review, 2026-11, findings 7 and 11).  `verb` is what the
  # message says cannot be done ("send", "export").
  exportProblem <- function(verb = "export") {
    error <- simulationBlocked()
    # Nor with no dose above 0: only the drugs given one are drawn
    # (positiveDoseDrugs(), R/drug-pipeline.R), and an empty table is the same
    if (is.null(error) && !is.null(nothingToDraw())) {
      error <- paste0("There is no plot to ", verb, ": enter a dose or target above 0 first.")
    }
    if (is.null(error)) {
      # main_plot() is NULL, not an error, when it is still waiting
      plot <- main_plot()
      error <- plotFailure(plot)
      if (is.null(error) && is.null(plot)) error <- paste0("There is no plot to ", verb, " yet.")
    }
    error
  }

  # What the workbook and the email say about the simulation: the patient,
  # the doses, the options that change what was simulated, and the time
  # units.  `comments` is the email's; a download has none.
  exportValues <- function(comments = "") {
    list(
      comments = comments,
      DT = doseTableClean(),
      url = url(),
      ageUnit = ageUnit(),
      weightUnit = weightUnit(),
      heightUnit = heightUnit(),
      age = age(),
      weight = weight(),
      height = height(),
      sex = sex(),
      pregnant = pregnant(),
      pulmonaryHypertension = pulmonaryHypertension(),
      adjustToFFM = adjustToFFM(),
      osmolality = osmolality(),
      creatinine = creatinine(),
      gestationalAge = gestationalAge(),
      albumin = albumin(),
      ast = ast(),
      cyp2d6 = genotypes()$cyp2d6,
      cyp2c19 = genotypes()$cyp2c19,
      cyp2c9 = genotypes()$cyp2c9,
      vkorc1 = genotypes()$vkorc1,
      cyp4f2 = genotypes()$cyp4f2,
      cyp2b6 = genotypes()$cyp2b6,
      # The physiology sliders the simulation was run at (R/physiology.R)
      physiology = physiology(),
      temperature = bodyTemperature(),
      altitude = altitude(),
      endobronchial = endobronchial(),
      # Whether each companion model was actually drawn, not just its box
      # ticked (companionGroupsShown()).
      mintoSchnider = "mintoSchnider" %in% companionGroupsShown(),
      hamberg = "hamberg" %in% companionGroupsShown(),
      # Whether an antiseizure interaction actually changed a drug
      antiseizureInteractions = ASM_INTERACTIONS_INPUT %in% companionGroupsShown(),
      # The workbook's times stay in minutes; these add the display unit
      # beside them and say what the plot showed (sendSlide.R).
      timeUnit = timeUnit(),
      maximum = plotMaximum()
    )
  }

  # A download of one file.  `write(file)` writes it.  A download cannot be
  # refused before it starts, so when there is nothing to export the alert
  # says why and the download fails (the browser lists it as failed); a
  # failure while writing is said the same way, its detail going only to the
  # server's log, as the email's does.  An error in a download ends only that
  # request, never the session.  No page spinner: the request keeps the
  # session busy, so a long one gets the Working cover (inst/www/app.js),
  # which replaced the page spinners.
  exportDownload <- function(extension, write) {
    downloadHandler(
      filename = function() exportFileName(extension),
      content = function(file) {
        problem <- exportProblem()
        if (!is.null(problem)) {
          shinyalert::shinyalert("Nothing to export", problem, type = "error", closeOnClickOutside = TRUE)
          stop(problem, call. = FALSE)
        }
        written <- tryCatch(
          {
            write(file)
            TRUE
          },
          error = function(e) {
            base::message("stanpumpR Export (", extension, ") failed: ", conditionMessage(e))
            outputComments("Export failed; the detail is in the server log")
            FALSE
          }
        )
        if (!written) {
          shinyalert::shinyalert("Export failed", EXPORT_FAILURE_MESSAGE, type = "error", closeOnClickOutside = TRUE)
          stop(EXPORT_FAILURE_MESSAGE, call. = FALSE)
        }
      }
    )
  }

  output$exportPowerPoint <- exportDownload("pptx", function(file) {
    writeSlideDeck(plotObjectReactive(), file)
  })

  output$exportSpreadsheet <- exportDownload("xlsx", function(file) {
    writeWorkbook(exportValues(), allResultsReactive(), plotResultsReactive(), drugs(), file)
  })

  output$exportImage <- exportDownload("png", function(file) {
    writePlotImage(plotObjectReactive(), file, height = plotHeight(), width = input$plotWidth)
  })

  # Email: the three files, with a link that reopens the simulation.
  # The comments as one string: a value no text box sends (NULL, a vector)
  # is none.  Read as it came, the checks below stopped on it and ended the
  # session (Carbon's UI audit, 2026-11-14, F6D).
  emailComments <- function() asScalarString(input$emailComments)

  # The attestation is about the comment as it stands: an edit unticks it,
  # so a comment changed after the box was ticked (a patient's name added)
  # is never sent on the earlier promise (Carbon's repository audit,
  # 2026-11-18).
  lastComment <- reactiveVal("")
  observeEvent(input$emailComments, {
    comment <- emailComments()
    shinyjs::toggle("commentSafe", condition = nzchar(comment))
    if (!identical(comment, lastComment())) {
      lastComment(comment)
      if (isTRUE(input$commentSafe)) updateCheckboxInput(session, "commentSafe", value = FALSE)
    }
  })

  observe({
    shinyjs::toggleState("sendSlide", condition = isEmailValid(input$recipient))
  })

  observeEvent(
    input$sendSlide,
    {
      outputComments("input$sendSlide",input$sendSlide)

      error <- NULL
      if (!isEmailValid(input$recipient)) {
        error <- "Please enter a valid recipient email address."
      } else if (nzchar(emailComments()) && !isTRUE(input$commentSafe)) {
        error <- "Please click the box confirming there is no PHI in the comments."
      } else if (nchar(emailComments()) > MAX_INPUT_TEXT) {
        error <- glue::glue("Comment is too long, please limit to {MAX_INPUT_TEXT} characters.")
      } else if (emailSendCount() >= EMAIL_SESSION_LIMIT) {
        error <- "This session has reached its email limit."
      } else if (!is.null(emailRateProblem(input$recipient))) {
        # Shared by every session of this worker (R/email-limits.R)
        error <- emailRateProblem(input$recipient)
      } else {
        error <- exportProblem("send")
      }
      if (!is.null(error)) {
        shinyalert::shinyalert("Error",error, type = "error", closeOnClickOutside = TRUE)
        return()
      }

      values <- exportValues(comments = emailComments())

      recordEmailSend(input$recipient)
      shinycssloaders::showPageSpinner(background = "#FFFFFFEE", caption = "Sending email...")
      # The spinner always comes down, and a failure is said, not a
      # disconnection.  sendSlide() catches its own errors; the reactives
      # read for its arguments do not.
      emailRetval <- tryCatch(
        sendSlide(
          values = values,
          recipient = input$recipient,
          plotObject = plotObjectReactive(),
          allResults = allResultsReactive(),
          plotResults = plotResultsReactive(),
          height = plotHeight(),
          width = input$plotWidth,
          slide = as.numeric(input$sendSlide),
          drugs = drugs(),
          email_username = config$email_username,
          email_password = config$email_password
        ) |> profileCode("sendSlide()"),
        error = function(e) {
          outputComments("sendSlide() arguments failed:", conditionMessage(e))
          # Nothing reached the mail server, so the send is not counted
          # (forgetEmailSend(), as sendSlide() does for its own failures)
          try(forgetEmailSend(input$recipient), silent = TRUE)
          EMAIL_FAILURE_MESSAGE
        },
        finally = shinycssloaders::hidePageSpinner()
      )

      if (isTRUE(emailRetval)) {
        emailSendCount(emailSendCount() + 1)
        shinyalert::shinyalert("Email sent", type = "success", closeOnClickOutside = TRUE)
      } else {
        shinyalert::shinyalert("Error sending email", emailRetval, type = "error", closeOnClickOutside = TRUE)
      }
    }
  )


  # Hover control ############################################################
  output$hover_info <- renderUI({
    hover <- input$plot_hover
    text <- xy_str(hover) |> profileCode("xy_str() in input$plot_hover")
    req(text)
    div(
      id = "hover_info_box",
      tagList(
        lapply(strsplit(text, ",")[[1]], function(part) {
          tagList(htmltools::htmlEscape(part), tags$br())
        })
      )
    )
  })

  output$drug_references <- renderUI({
    # A simulation that stopped is said in the plot area (plotMessage());
    # here it printed the bare error where the references go (Carbon's UI
    # audit, 2026-11-14, F6F)
    failed <- FALSE
    simulatedDrugs <- tryCatch(drugs(), shiny.silent.error = function(err) NULL,
                               error = function(err) { failed <<- TRUE; NULL })
    if (failed) {
      return(span("The references are listed once the simulation can be drawn.", class = "text-muted"))
    }
    if (length(simulatedDrugs) == 0) {
      return(span("No drugs in the current simulation.", class = "text-muted"))
    }
    items <- lapply(names(simulatedDrugs), function(drug) {
      entry <- simulatedDrugs[[drug]]
      # Opens the drug's page in the Help tab (handled in app.js); a companion
      # entry (the Schnider, Minto or Hamberg model) opens the page about its
      # group of models.
      page <- if (isAlternativeModelEntry(entry)) companionHelpPage(entry) else paste0("drugs/", drug)
      tagList(
        citationItemHTML(drug, entry$reference, entry$Color),
        tags$a(href = "#", class = "small ms-1", `data-help-page` = page,
               "About this model")
      )
    })
    tags$ul(class = "mb-0", lapply(items, tags$li))
  })

  # Display Time, CE, or total opioid
  #
  # e$x is in minutes (data coordinates), whatever the axis labels say.  Every
  # time shown goes through formatPlotTime(), so it reads in the display unit
  # ("3.47 weeks"), or as HH:MM in clock mode -- the MEAC and interaction
  # panels used to say "minutes" even then.  See utils-time-display.R.
  xy_str <- function(e) {
    e <- plotEvent(e)
    if (is.null(e)) return()
    outputComments("In xy_str")
    outputComments("e$panelvar1 = ", e$panelvar1)

    yaxis <- gsub("\n"," ", e$panelvar1)
    hoverTime <- function(minutes) formatPlotTime(minutes, timeUnit(), referenceTime())

    # Nothing to read while the plot itself failed: the plot area says why
    # (main_plot()), and the error, read here, ended the session at a click
    # and printed itself under the pointer (Carbon's third UI audit,
    # 2026-11-15, C1)
    plotResults <- plotResultsOrNull()$plot
    if (is.null(plotResults)) return()
    if (yaxis == PLOT_NAME_MEAC)
    {
      TO <- plotResults$Drug == "total opioid"
      if (sum(TO) == 0)
      {
        TO <- plotResults$Wrap == PLOT_NAME_MEAC
        outputComments("Elements found in search of plotResults$Wrap", sum(TO))
      }
      if (sum(TO) < 2) return(NULL)
      return(
        paste0("Time: ", hoverTime(e$x), ", ", plotResults$Drug[TO][1], ": ",
               signif(panelSeriesAt(plotResults[TO, ], e$x), 2), " ", PLOT_NAME_MEAC)
      )
    }
    if (yaxis == PLOT_NAME_INTERACTION)
    {
      TO <- plotResults$Drug == PLOT_NAME_INTERACTION
      if (sum(TO) < 2) return(NULL)
      return(
        paste0("Time: ", hoverTime(e$x), ", P (response): ",
               signif(panelSeriesAt(plotResults[TO, ], e$x), 2))
      )
    }

    if (yaxis == PLOT_NAME_EVENTS)
    {
      return("Click to enter events, Double click to edit events")
    }

    # The panel title is "<name>\n(<units>)".  The series behind it is looked
    # up through plotResults rather than by splitting the title on spaces,
    # because a title may itself contain one ("MAC equivalents").
    drug <- as.character(plotResults$Drug[as.character(plotResults$Wrap) == e$panelvar1])[1]
    if (is.na(drug)) return(NULL)
    # A panel is read through the library's own drug.  A companion drawn in it
    # (the Schnider or Minto model; R/alternativeModels.R) is reported after
    # it, by the model's name.
    drug <- alternativeModelParent(drug)
    outputComments("Drug identified in xy_str() is", drug)

    # A compartment panel (R/compartments.R): the concentration in V2 or V3,
    # the tension in the muscle or fat group, or the amount in the depot.
    compartment <- compartmentSeries(drug)
    if (!is.na(compartment$drug))
    {
      cm <- drugs()[[compartment$drug]]$compartments
      column <- cm[[compartment$compartment]]
      if (is.null(column) || length(column) < 2) return(NULL)
      units <- sub("^.*\\((.*)\\)\\s*$", "\\1", yaxis)
      shown <- compartmentPanelLabel(paste(drug))
      return(paste0("Time: ", hoverTime(e$x), ", ", compartment$drug, " ", shown, ": ",
                    signif(stats::approx(cm$Time, column, e$x, rule = 2)$y, 3), " ", units))
    }

    # A TCI rate panel: report the pump rate in force at that moment, and the
    # companion pump's after it.
    if (grepl(" TCI$", drug))
    {
      drug <- sub(" TCI$", "", drug)
      if (is.null(drugs()[[drug]]$tci$rates)) return(NULL)
      pumpAt <- function(entry, name) {
        rates <- entry$tci$rates
        if (is.null(rates)) return(NULL)
        # Before the pump starts it delivers nothing: max(c(), 1) read the
        # first row there, the loading dose (Carbon's third UI audit,
        # 2026-11-15, M6)
        started <- which(rates$Time <= e$x)
        if (length(started) == 0) return(paste0(name, ": not started"))
        j <- started[length(started)]
        if (rates$Bolus[j]) {
          b <- entry$tci$boluses
          k <- which(b$Time == rates$Time[j])[1]
          return(paste0(name, " loading dose: ", signif(b$Amount[k], 3), " ", b$Units[k]))
        }
        paste0(name, " rate: ", signif(rates$Rate[j], 3), " ", rates$Units[j])
      }
      parts <- c(
        pumpAt(drugs()[[drug]], paste(drug, "TCI")),
        unlist(lapply(companionsOf(drug), function(entry) pumpAt(entry, entry$modelLabel)))
      )
      return(paste0("Time: ", hoverTime(e$x), ", ", paste(parts, collapse = ", ")))
    }

    # if the panel's drug was just removed, drugs()[[drug]] will be NULL until
    # the plot re-renders
    if (!drug %in% names(drugs())) return(NULL)
    entry <- drugs()[[drug]]

    # Read at the hovered time itself, interpolated in the drug's full series
    # rather than snapped to the nearest of 100 equispaced points, and as Cp
    # for a drug with no effect site (hoverConcentration()).
    x <- c(sub("\\s*\\(.*$", "", yaxis),                 # the name as shown
           sub("^.*\\((.*)\\)\\s*$", "\\1", yaxis))      # the units, unbracketed
    normalization <- normalizationInput()
    conc <- hoverConcentration(entry$results, e$x, normalization)
    returnText <- paste0("Time: ", hoverTime(e$x), ", ", x[1], " ", conc$label, ": ",
                         signif(conc$value, 2), " ", x[2])
    # The same reading from each companion model drawn in this panel
    for (companion in companionsOf(drug))
    {
      concC <- hoverConcentration(companion$results, e$x, normalization)
      returnText <- paste0(returnText, ", ", companion$modelLabel, " ", concC$label, ": ",
                           signif(concC$value, 2), " ", x[2])
    }
    # Not under normalization, which hides the line (simulationPlot()).
    if (plotRecovery() && identical(normalization, NORMALIZE_NONE))
    {
      # Missing means a dose has been given that has not begun to be absorbed,
      # and is said in words.  A time is written in the unit of this panel's
      # recovery labels, and as "more than ..." where the engine's search
      # stopped at its horizon (formatRecovery()).
      returnText <- paste0(
        returnText, ", Time until threshold: ",
        formatRecovery(
          hoverRecovery(entry, e$x),
          maxRecovery = entry$max$Recovery,
          horizon = recoveryHorizonFor(entry, plotMaximum())
        )
      )
    }
    # The opioid's own % MEAC, the value the overlay draws (R/meacOverlay.R)
    if (plotMEACOverlay())
    {
      pct <- meacPercentAt(entry, e$x)
      if (!is.null(pct))
        returnText <- paste0(returnText, ", ", signif(pct, 3), "% MEAC")
    }
    return(returnText)
  }

  # Click and Double Click Control ##########################################################
  # get date and time from image

  # Response to single click
  observeEvent(
    input$plot_click,
    {
      profileCode({
        outputComments("in click()")
        x <- imgDrugTime(input$plot_click) |> profileCode("imgDrugTime() in input$plot_click")
        outputComments("in click(), returning from imgDrugTime()")
        if (!isPlotTarget(x)) return()
        DrugTimeUnits(x)

        # The MAC panel is a derived series, not a drug: no dose to add.
        if (x$drug == "MAC") return()

        if (x$drug %in% c(PLOT_ID_MEAC, PLOT_ID_INTERACTION)) {
          showRemoveAddedPlotModal(x$drug)
        } else if (x$drug == PLOT_ID_EVENTS) {
          showAddEventModal(x$time)
        } else {
          showAddDrugModal(x$drug, x$time)
        }
      }, name = "input$plot_click observer")
    })

  # Response to double click
  observeEvent(
    input$plot_dblclick,
    {
      profileCode({
        outputComments("in double click routine")
        x <- imgDrugTime(input$plot_dblclick)
        if (!isPlotTarget(x)) return()
        DrugTimeUnits(x)

        if (x$drug %in% c(PLOT_ID_MEAC, PLOT_ID_INTERACTION, "MAC"))
        {
          return()
        } else if (x$drug == PLOT_ID_EVENTS)
        {
          showEditEventsModal()
        } else if (!x$drug %in% doseTable()$Drug) {
          # A panel with no rows of its own to edit: oxygen, drawn whenever
          # any gas is given, with only air in the table.  Its edit dialog
          # had no grid, and Apply ended the session (adversarial UI review,
          # 2026-11, finding 6).  Offered as a single click is, to add one.
          showAddDrugModal(x$drug, x$time)
        } else {
          showEditDrugModal(x$drug)
        }
      }, name = "input$plot_dblclick observer")
    })

  # Get the time, drug, and units from the image mouse event
  imgDrugTime <- function(e)
  {
    outputComments("in imgDrugTime()")
    e <- plotEvent(e)
    if (is.null(e)) return(NULL)
    results <- plotResultsOrNull()
    if (is.null(results)) return(NULL)
    allResults <- results$all
    plotResults <- results$plot
    plottedDrugs <- unique(allResults$Drug)
    plottedAll   <- unique(as.character(plotResults$Drug))
    outputComments("plottedDrugs", plottedDrugs)
    outputComments("plottedAll", plottedAll)

    # The time clicked, in MINUTES (the plot's x data are minutes whatever
    # the time unit; the dialogs format it).  Read from the click itself
    # rather than snapped to the 100-point display grid, which is 3.7 days
    # coarse on a 52-week plot.  Kept on the plot, and rounded: to 0.1 minute
    # in minutes, as always, otherwise to a quarter of the unit.
    unit <- doseTableFormat()[["unit"]]
    time <- min(max(as.numeric(e$x), plotMinimum()), plotMaximum())
    time <- if (unit == "minutes") {
      round(time, 1)
    } else {
      round(time / TIME_UNITS[[unit]] * 4) / 4 * TIME_UNITS[[unit]]
    }

    # Get Drug
    yaxis <- gsub("\n", " ", e$panelvar1)
    if (yaxis == PLOT_NAME_MEAC) {
      drug <- PLOT_ID_MEAC
    } else if (yaxis == PLOT_NAME_INTERACTION) {
      drug <- PLOT_ID_INTERACTION
    } else if (yaxis == PLOT_NAME_EVENTS) {
      drug <- PLOT_ID_EVENTS
    } else {
      # Looked up through the panel title rather than its first word, since a
      # title may contain a space ("MAC equivalents").
      drug <- as.character(plotResults$Drug[as.character(plotResults$Wrap) == e$panelvar1])[1]
      if (is.na(drug)) drug <- unlist(strsplit(yaxis, " "))[1]
      # A click on a drug's TCI rate panel is a click on that drug, and a
      # click on a companion's line (R/alternativeModels.R) on its parent.
      drug <- sub(" TCI$", "", drug)
      # And a click on one of its compartment panels (R/compartments.R)
      compartment <- compartmentSeries(drug)
      if (!is.na(compartment$drug)) drug <- compartment$drug
      drug <- alternativeModelParent(drug)
      # And a click on a metabolite that is only ever formed (no dose units)
      # on the drug that forms it
      drug <- formedMetaboliteParent(drug, drugs(), drugDefaults())
    }
    outputComments("drug from panelvar1", drug)

    # Get Units
    if (drug %in% c(PLOT_ID_EVENTS, PLOT_ID_MEAC, PLOT_ID_INTERACTION))
    {
      units <- c("","")
    } else {
      i <- match(drug, drugDefaults()$Drug)
      units <- c(drugDefaults()$Bolus.Units[i], drugDefaults()$Infusion.Units[i])
    }
    outputComments("Exiting imgDrugTime()")
    return(
      list(
        drug = drug,
        time = time,
        units = units
      )
    )
  }

  showRemoveAddedPlotModal <- function(plot) {
    showModal(
      modalDialog(
        title = paste("Remove", plot, "plot?"),
        paste("Do you want to remove the", plot, "plot?"),
        br(), br(),
        actionButton(paste0("confirmRemove", plot), "Remove", class = "btn-primary"),
        tags$button(
          type = "button",
          class = "btn float-right",
          `data-bs-dismiss` = "modal",
          "Cancel"
        ),
        footer = NULL,
        easyClose = TRUE,
        size = "s"
      )
    )
  }

  observeEvent(input$confirmRemoveMEAC, {
    removeModal()
    updateCheckboxGroupInput(session, "addedPlots",
      selected = setdiff(input$addedPlots, PLOT_ID_MEAC)
    )
  })

  observeEvent(input$confirmRemoveInteraction, {
    removeModal()
    updateCheckboxGroupInput(session, "addedPlots",
      selected = setdiff(input$addedPlots, PLOT_ID_INTERACTION)
    )
  })

  #################################### Single Click Response ##################################


  # The label of a dose field: "Dose or Target" for a drug that offers TCI
  # target units (tciUnits), since the number is then a concentration when a
  # target unit is chosen; "Dose" otherwise.
  doseFieldLabel <- function(drug) {
    units <- unlist(drugDefaults()$Units[drugDefaults()$Drug == drug])
    if (any(isTciUnit(units))) "Dose or Target" else "Dose"
  }

  # `time` is in minutes (imgDrugTime()); the dialog shows it, and the dose
  # table stores what is typed, in the dose table's format.  `drug` is ""
  # for the Add new drug button: the drug box then opens empty, so typing
  # the first letters of a name finds it (a preselected drug would have to
  # be deleted first).
  showAddDrugModal <- function(drug, time) {
    skipTarget(list(drug = drug, time = time))
    thisDrug     <- which(drug == drugDefaults()$Drug)
    initialUnits <- unlist(drugDefaults()$Units[thisDrug])
    selectedUnit <- drugDefaults()$Default.Units[thisDrug]
    format <- doseTableFormat()

    showModal(
      modalDialog(
        `data-submit-btn` = "addDoseBtn",
        title = "Add a dose",
        selectizeInput(
          inputId = "addDoseDrug",
          label = "Drug",
          # The visible drugs, honouring the illicit-drug opt-in; the clicked
          # drug is always included (it is already in the table).  "" first,
          # so that an empty box can be selected.
          choices = c("", union(drug[nzchar(drug)], drugChoices())),
          selected = drug,
          options = list(placeholder = "Type a drug name")
        ),
        textInput(
          inputId = "addDoseTime",
          label = timeEntryLabel(format),
          value = minutesToEntryTime(time, format, procedureStart())
        ),
        textInput(
          inputId = "addDoseAmount",
          label = doseFieldLabel(drug),
          placeholder = "Enter dose"
        ) |> modalFocus(),
        selectInput(
          inputId = "addDoseUnits",
          label = "Units",
          choices = initialUnits,
          selected = selectedUnit
        ),
        actionButton("addDoseBtn", "Add", class = "btn-primary"),
        tags$button(
          type = "button",
          class = "btn float-right",
          `data-bs-dismiss` = "modal",
          "Cancel"
        ),
        uiOutput("skipNextDoseUI"),
        uiOutput("stopAdministrationUI"),
        footer = NULL,
        easyClose = TRUE,
        size = "s"
      )
    )
  }

  # The Add new drug button under the plot: the same dialog as a click on
  # the plot, with the drug box empty (Steven L. Shafer, 2026-10-28: a
  # suggested drug defeated typing a name), at the start of the plot.
  observeEvent(input$addNewDrug, {
    profileCode({
      showAddDrugModal("", 0)
    }, name = "input$addNewDrug observer")
  })

  observeEvent(input$addDoseDrug, {
    # One drug of the library: the selector sends no other, but a forged
    # vector reached skipTarget() and ended the session at Skip next dose or
    # Stop (Carbon's second UI audit, 2026-11-15, F4)
    drug <- asScalarString(input$addDoseDrug)
    req(drug %in% drugDefaults()$Drug)
    target <- isolate(skipTarget())
    if (!is.null(target)) skipTarget(list(drug = drug, time = target$time))
    thisDrug     <- which(drug == drugDefaults()$Drug)
    units        <- unlist(drugDefaults()$Units[thisDrug])
    selectedUnit <- drugDefaults()$Default.Units[thisDrug]
    updateSelectInput(session, "addDoseUnits", choices = units, selected = selectedUnit)
    updateTextInput(session, "addDoseAmount", label = doseFieldLabel(drug))
  })

  # validateTime() for a dose time, but "" also for a time that names no time
  # in the dose table's format: a clock time such as 25:00 (lubridate reads
  # 24:30 as 00:30), or any clock time while the procedure start cannot be
  # read.  doseTableClean() would drop such a row without a word, so the dose
  # dialogs refuse it, as the add-event dialog does.
  validateDoseTime <- function(x) {
    out <- validateEntry(x, validateTime)
    if (!nzchar(out)) return(out)
    format <- doseTableFormat()
    minutes <- displayTimeToMinutes(out, referenceFor(format), format[["unit"]])
    if (is.na(minutes)) "" else out
  }

  observeEvent(input$addDoseBtn, {
    profileCode({
      addDoseTime <- validateDoseTime(input$addDoseTime)
      addDoseAmount <- validateEntry(input$addDoseAmount, validateDose)
      # A time or dose that could not be read ("", see R/validate-input.R and
      # validateDoseTime()) leaves the dialog open to be corrected, as the
      # add-event dialog does, rather than adding a row the simulation would
      # ignore.
      if (!nzchar(addDoseTime)) {
        showNotification(paste0("That time could not be read: enter it as ",
                                timeEntryUnitText(doseTableFormat()), "."), type = "error")
        return()
      }
      if (!nzchar(addDoseAmount)) {
        showNotification(paste("That dose could not be read: enter it as a number from 0 to",
                               "1,000,000,000, such as 2.5."), type = "error")
        return()
      }
      drug <- asScalarString(input$addDoseDrug)
      if (!drug %in% drugDefaults()$Drug) {
        showNotification("Choose a drug.", type = "error")
        return()
      }
      # The units list follows the drug a moment later (the addDoseDrug
      # observer); Enter pressed in between sends no units, or the last
      # drug's, and dt$Units[idx] <- NULL stopped this observer and the
      # session (keyboard review of 2026-11-02, reproduced in testServer()).
      drugUnits <- unlist(drugDefaults()$Units[drugDefaults()$Drug == drug])
      if (length(input$addDoseUnits) != 1 || !input$addDoseUnits %in% drugUnits) {
        showNotification("Choose the units.", type = "error")
        return()
      }
      dt <- doseTable()
      # The dose goes into the first blank row; a table with none (a
      # restored bookmark, or its blank row deleted) gets one, as
      # stopAdministration() does, or the dose would be silently dropped.
      blank <- which(as.character(dt$Drug) == "")
      if (length(blank) == 0) {
        dt <- rbind(dt, doseTableNewRow)
        blank <- nrow(dt)
      }
      idx <- blank[1]
      dt$Drug[idx]  <- drug
      dt$Time[idx]  <- addDoseTime
      dt$Dose[idx]  <- addDoseAmount
      dt$Units[idx] <- input$addDoseUnits
      if (dt$Drug[nrow(dt)] != "" ) {
        dt <- rbind(dt, doseTableNewRow)
      }

      # With the table full the dialog stays open, and nothing is written
      if (commitDoseTable(dt)) removeModal()
    }, name = "input$addDoseBtn observer")
  })

  # "Skip next dose" (R/scheduled.R, skipNextScheduledDose()): offered in both
  # dose dialogs while the clicked drug has a scheduled dose, on any route, at
  # or after
  # the time clicked.  skipTarget() is the drug and time the dialog is about;
  # the Add a dose dialog's drug selector moves it to another drug.
  skipTarget <- reactiveVal(NULL)

  skipNextDose <- function(target = skipTarget()) {
    if (is.null(target) || !nzchar(asScalarString(target$drug))) return(NULL)
    format <- doseTableFormat()
    skipNextScheduledDose(
      doseTable(), target$drug, target$time, plotMaximum(), format,
      referenceFor(format)
    )
  }

  output$skipNextDoseUI <- renderUI({
    skip <- tryCatch(skipNextDose(), error = function(e) NULL)
    if (is.null(skip)) return(NULL)
    format <- doseTableFormat()
    tags$div(
      class = "mt-3",
      actionButton(
        "skipNextDoseBtn",
        paste0("Skip next dose (", minutesToEntryTime(skip$time, format, referenceFor(format)), ")"),
        class = "btn-outline-secondary w-100"
      ),
      tags$p(
        class = "small text-muted mt-1 mb-0",
        "A missed or vomited dose: the schedule then carries on as before."
      )
    )
  })

  observeEvent(input$skipNextDoseBtn, {
    profileCode({
      target <- skipTarget()
      skip <- skipNextDose(target)
      if (is.null(skip)) {
        removeModal()
        return()
      }
      dt <- skip$table
      if (dt$Drug[nrow(dt)] != "") dt <- rbind(dt, doseTableNewRow)
      # The skip writes two rows; with no room the dialog stays open
      if (!commitDoseTable(dt)) return()
      removeModal()
      format <- doseTableFormat()
      at <- function(m) minutesToEntryTime(m, format, referenceFor(format))
      showNotification(paste0(
        "Skipped the ", target$drug, " dose at ", at(skip$time), ". ",
        if (is.na(skip$resumes)) "The next dose in the table follows at its own time." else
          paste0("The schedule (", skip$dose, " ", skip$units, ") resumes at ", at(skip$resumes), ".")
      ), type = "message")
    }, name = "input$skipNextDoseBtn observer")
  })

  # "Stop" (R/stop-administration.R): offered in both dose dialogs while the
  # clicked drug has an infusion, a TCI target, a repeating dose or an inhaled
  # agent's setting running at the time clicked.  It writes the 0 row the
  # dose table's own rules read as a stop, at that time.
  stopTarget <- function(target = skipTarget()) {
    if (is.null(target) || !nzchar(asScalarString(target$drug))) return(NULL)
    format <- doseTableFormat()
    stopAdministration(doseTable(), target$drug, target$time, plotMaximum(), format,
                       referenceFor(format))
  }

  output$stopAdministrationUI <- renderUI({
    ended <- tryCatch(stopTarget(), error = function(e) NULL)
    if (is.null(ended)) return(NULL)
    format <- doseTableFormat()
    target <- skipTarget()
    tags$div(
      class = "mt-3",
      actionButton(
        "stopAdministrationBtn",
        paste0("Stop ", target$drug, " at ", minutesToEntryTime(target$time, format, referenceFor(format))),
        icon = icon("stop"),
        class = "btn-outline-danger w-100"
      ),
      tags$p(
        class = "small text-muted mt-1 mb-0",
        paste0("Ends ", stoppedText(ended$stopped$label), ", by adding a dose of 0 to the dose table.")
      )
    )
  })

  observeEvent(input$stopAdministrationBtn, {
    profileCode({
      target <- skipTarget()
      ended <- stopTarget(target)
      if (is.null(ended)) {
        removeModal()
        return()
      }
      # With no room for the stop's rows the dialog stays open
      if (!commitDoseTable(ended$table)) return()
      removeModal()
      format <- doseTableFormat()
      showNotification(paste0(
        "Stopped ", stoppedText(ended$stopped$label), " of ", target$drug, " at ",
        minutesToEntryTime(target$time, format, referenceFor(format)), "."
      ), type = "message")
    }, name = "input$stopAdministrationBtn observer")
  })

  # The Visualizer (R/viewerRun.R, inst/www/viewer-launch.js,
  # inst/www/stanpumpR-viewer.html; source tools/viewer): the Edit doses
  # dialog's button opens the popup at once in the browser (a window opened
  # later, when the server answers, would be stopped by popup blockers) and
  # closes the dialog, so the app is free while the visualizer plays. The run
  # is the plot's own: the drug's concentrations from drugs(), from the
  # plot's Min time to its Max time, re-solved with the plot's PK only where
  # its points are too far apart to animate (viewerPlotEntry()). A snapshot
  # of the committed dose table at the click.
  viewerAvailable <- function(drug) {
    isTRUE(tryCatch(is.null(viewerProblem(isolate(drugs())[[drug]])), error = function(e) FALSE))
  }

  # The drugs the Visualizer can draw: every plotted drug's panel but a
  # companion model's, for which viewerProblem() is NULL -- the inhaled
  # anesthetics among them (2026-11-09), but not oxygen or the summed MAC.
  viewerDrugs <- function(plotted) {
    keep <- vapply(names(plotted), function(d) {
      e <- plotted[[d]]
      is.null(e$companionOf) &&
        isTRUE(tryCatch(is.null(viewerProblem(e)), error = function(err) FALSE))
    }, logical(1))
    names(plotted)[keep]
  }

  # Build one drug's run and send it to the pop-up the browser has opened.
  # token: the click's, echoed so the browser can drop an earlier click's answer.
  launchViewer <- function(drug, token) {
      # Told in the app and in the viewer's window, which would otherwise
      # wait for a run that is not coming.
      fail <- function(message) {
        showNotification(paste("Visualizer:", message), type = "error")
        session$sendCustomMessage("stanpumpR-viewer-error", list(token = token, message = message))
      }
      blocked <- simulationBlocked()
      if (!is.null(blocked)) return(fail(blocked))
      run <- tryCatch({
        plotted <- drugs()
        defaults <- drugDefaults()
        category <- defaults$Category[match(drug, defaults$Drug)]
        if (isGasDrug(drug)) {
          # An inhaled anesthetic: the gas engine's own states for the plot.
          viewerGasRun(plotted[[drug]], plotMinimum(), plotMaximum(), category = category,
                       physiology = physiology())
        } else {
          plotted <- plotted[!isGasDrug(names(plotted))]
          entry <- viewerPlotEntry(drug, plotted[[drug]], doseTableIV(), eventTableClean(), plotted,
                                   plotMinimum(), plotMaximum(), antiseizureInteractions())
          viewerRun(entry, plotMinimum(), plotMaximum(), category = category,
                    physiology = physiology())
        }
      }, error = function(e) {
        fail(if (inherits(e, "shiny.silent.error")) "the plot is not ready yet; try again." else conditionMessage(e))
        NULL
      })
      if (!is.null(run)) session$sendCustomMessage("stanpumpR-viewer-run", list(token = token, run = run))
  }

  observeEvent(input$openViewer, {
    profileCode({
      drug <- skipTarget()$drug
      token <- as.character(input$openViewer)
      removeModal()
      launchViewer(drug, token)
    }, name = "input$openViewer observer")
  })

  # The plot's own Visualizer button (beside Add new drug; Devin's stress
  # test, 2026-11-08, on Shafer's decision): one drawable drug opens it at
  # once, several drop up a list. The browser sends the drug with the token,
  # which is untrusted like every input, so it must be one name of a drug the
  # Visualizer can draw now.
  output$viewerButtonUI <- renderUI({
    drawable <- tryCatch(viewerDrugs(drugs()), error = function(e) character(0))
    viewerPlotButton(drawable)
  })
  observeEvent(input$openViewerDrug, {
    profileCode({
      v <- input$openViewerDrug
      if (!is.list(v)) return()
      token <- doseGridCell(v$token)
      drug <- doseGridCell(v$drug)
      if (!nzchar(token)) return()
      if (!drug %in% tryCatch(viewerDrugs(drugs()), error = function(e) character(0))) {
        message <- "that drug cannot be drawn now."
        showNotification(paste("Visualizer:", message), type = "error")
        session$sendCustomMessage("stanpumpR-viewer-error", list(token = token, message = message))
        return()
      }
      launchViewer(drug, token)
    }, name = "input$openViewerDrug observer")
  })

  # The browser blocked the visualizer's window (inst/www/viewer-launch.js).
  observeEvent(input$viewerBlocked, {
    showNotification("Visualizer: the browser blocked the window. Allow pop-ups for this site and click again.",
                     type = "warning", duration = 10)
  })

  showEditDrugModal <- function(drug) {
    skipTarget(list(drug = drug, time = DrugTimeUnits()$time))
    showModal(
      modalDialog(
        title = paste("Edit", drug, "doses"),
        # The column must stay headed "Time" (the grid's hooks find it by
        # name), so the unit is said above it
        tags$p(class = "small text-muted mb-1", timeEntryLabel(doseTableFormat())),
        isolate(editDosesDialogGrid(drug)),
        actionButton("editDosesOK", "Apply", class = "btn-primary"),
        actionButton("deleteAllDosesBtn", "Delete All Doses", class = "btn-outline-danger"),
        # The Visualizer (R/viewerRun.R): only for a drug it can draw.
        # Not an actionButton: inst/www/viewer-launch.js opens the pop-up and
        # only then reports input$openViewer, so a blocked pop-up leaves this
        # dialog open and asks nothing of the server.
        if (viewerAvailable(drug)) tags$button(
          id = "openViewer", type = "button", class = "btn btn-default btn-outline-primary",
          icon("cube"), "Visualizer"
        ),
        tags$button(
          type = "button",
          class = "btn float-right",
          `data-bs-dismiss` = "modal",
          "Cancel"
        ),
        uiOutput("skipNextDoseUI"),
        uiOutput("stopAdministrationUI"),
        footer = NULL,
        easyClose = TRUE,
        # "m", not "s": the grid's Units column needs the room for a unit
        # with a frequency ("mg PO liquid qid")
        size = "m"
      )
    )
  }

  observeEvent(input$deleteAllDosesBtn, {
    drug <- clickedDrug()
    removeModal()
    if (is.null(drug)) return()
    if (drugHasNonZeroDoses(doseTable(), drug)) {
      showModal(
        modalDialog(
          title = paste("Delete", drug, "doses?"),
          "Are you sure you want to delete all doses for",
          tags$strong(drug, .noWS = "after"), "?",
          br(), br(),
          actionButton("confirmDeleteAllDoses", "Yes", class = "btn-primary"),
          tags$button(
            type = "button",
            class = "btn float-right",
            `data-bs-dismiss` = "modal",
            "Cancel"
          ),
          footer = NULL,
          easyClose = TRUE,
          size = "m"
        )
      )
    } else {
      deleteDrugDoses(drug)
    }
  })

  observeEvent(input$confirmDeleteAllDoses, {
    removeModal()
    drug <- clickedDrug()
    if (!is.null(drug)) deleteDrugDoses(drug)
  })

  # The Edit doses dialog's grid, drawn into the dialog as it opens (as Edit
  # events' is), or NULL for a drug with no rows.  It was a renderUI output
  # shared by every drug's dialog, and the browser showed the output's last
  # value, the previous drug's rows, under the new drug's title until the
  # server's new one arrived (ChatGPT's fifth review, N01; Apply was already
  # safe, the grid's tag naming the drug it was drawn for).
  editDosesDialogGrid <- function(drug) {
    dt <- doseTable()
    rows <- dt[dt$Drug %in% drug, ]
    if (!length(drug) || !nzchar(drug) || nrow(rows) == 0) return(NULL)
    possibleUnits <- unlist(drugDefaults()$Units[drugDefaults()$Drug == drug])
    rows$Delete <- FALSE
    # The drug the grid was drawn for is its tag, returned with every edit,
    # so that Apply cannot write one drug's rows as another's from a grid
    # left over from an earlier dialog (editDosesOK)
    tableGridInput("editPriorDosesTable", editDosesGridColumns(possibleUnits, doseFieldLabel(drug)),
                   rows[, c("Delete", "Time", "Dose", "Units")], tag = drug, height = 260,
                   label = paste(drug, "doses"))
  }

  observeEvent(
    input$editDosesOK,
    {
      profileCode({
        drug <- clickedDrug()
        if (is.null(drug)) return()
        # Only a grid drawn for this drug: none is drawn for a drug with no
        # rows, and the input then still holds the last dialog's grid, which
        # Apply wrote as this drug's doses or, with none, stopped on NULL
        # and ended the session (adversarial UI review, 2026-11, finding 6).
        req(input$editPriorDosesTable)
        if (!identical(tableGridTag(input$editPriorDosesTable), drug)) return()
        TT <- tableGridRead(input$editPriorDosesTable, editDosesGridColumns(character(0)))
        req(TT)
        # No grid sends more rows than it holds, and the drug's rows and
        # everyone else's must fit the dose table together: stored past
        # MAX_DOSE_ROWS the table could not be used and the plot was lost,
        # with the dialog already closed (ChatGPT's fourth review, F02).
        others <- doseTable()
        others <- sum(nzchar(as.character(others$Drug)) & !others$Drug %in% drug, na.rm = TRUE)
        if (tableGridTruncated(TT) || sum(!TT$Delete) + others > MAX_DOSE_ROWS - 1L) {
          showNotification(sprintf("The dose table holds at most %d doses.", MAX_DOSE_ROWS - 1L),
                           type = "error")
          return()
        }
        # Every dose kept needs a time and a dose that can be read.  A blank
        # counts as unreadable here: the grid clears an entry it cannot read
        # (inst/www/grid-funs.js), and validateTime() and validateDose() below
        # would make the blank 0.  So does a time that names no time in the
        # table's format (validateDoseTime()).  The dialog stays open to be
        # corrected.
        kept <- TT[!TT$Delete, , drop = FALSE]
        unreadable <- function(x, validate) {
          vapply(x, function(v) isBlankEntry(v) || !nzchar(validate(v)), logical(1))
        }
        if (any(unreadable(kept$Time, validateDoseTime)) || any(unreadable(kept$Dose, validateDose))) {
          showNotification(paste0("Every dose needs a time, entered as ",
                                  timeEntryUnitText(doseTableFormat()),
                                  ", and a dose, entered as a number from 0 to 1,000,000,000."),
                           type = "error")
          return()
        }
        # And a unit the drug offers.  The dropdown takes Delete, which left
        # the unit blank: the simulation then ignored the drug without a
        # word, and Suggest Dosing for it ended the session (finding 4).
        drugUnits <- unlist(drugDefaults()$Units[drugDefaults()$Drug == drug])
        if (!all(kept$Units %in% drugUnits)) {
          showNotification(paste0("Every dose needs a unit: ",
                                  paste(drugUnits, collapse = ", "), "."), type = "error")
          return()
        }
        removeModal()
        outputComments("In ObserveEvent for editDosesOK")
        TT$Drug <- drug
        outputComments("TT:")
        outputComments(TT)
        outputComments("doseTable:")
        outputComments(doseTable())
        dt <- blankMissingCells(doseTable())
        dt <- rbind(
          TT[!TT$Delete,c("Drug","Time","Dose","Units")],
          dt[!dt$Drug %in% drug, ]
        )

        for (i in seq_len(nrow(dt)))
        {
          if (nzchar(dt$Drug[i]))
          {
            dt$Time[i] <- validateTime(dt$Time[i])
            dt$Dose[i] <- validateDose(dt$Dose[i]) # should work for target too
          }
        }

        # Sort by time, by drug, but put blanks at the bottom.  By the time
        # each string stands for, in the dose table's format: sorted as text,
        # "10" came before "9", and "1.5" days before "10" hours.
        outputComments(toString(unique(dt$Time)))
        format <- doseTableFormat()
        minutes <- displayTimeToMinutes(dt$Time, referenceFor(format), format[["unit"]])
        dt <- dt[order(is.na(minutes), minutes, dt$Drug), ]
        # The blank rows sort last; past MAX_DOSE_ROWS there is no room for them
        dt <- dt[seq_len(min(nrow(dt), MAX_DOSE_ROWS)), , drop = FALSE]

        outputComments("doseTable after update:")
        outputComments(dt)
        # The check above counts the doses; this counts the rows the gas
        # rules add as well
        commitDoseTable(dt)
      }, name = "input$editDosesOK observer")
    })

  # `time` is in minutes (imgDrugTime()), shown in the dose table's format;
  # events are stored in minutes.
  showAddEventModal <- function(time) {
    format <- doseTableFormat()
    showModal(
      modalDialog(
        `data-submit-btn` = "addEventBtn",
        title = paste("Enter a new event"),
        textInput(
          inputId = "clickTimeEvent",
          label = timeEntryLabel(format),
          value = minutesToEntryTime(time, format, procedureStart())
        ) |> modalFocus(),
        selectInput(
          inputId = "clickEvent",
          label = "Event",
          choices = eventDefaults()$Event
        ),
        actionButton("addEventBtn", "Add", class = "btn-primary"),
        tags$button(
          type = "button",
          class = "btn float-right",
          `data-bs-dismiss` = "modal",
          "Cancel"
        ),
        footer = NULL,
        easyClose = TRUE,
        size = "s"
      )
    )
  }

  observeEvent(
    input$addEventBtn,
    {
      profileCode({
        # Read as the dose table's times are.  A time that cannot be read (a
        # clock time of 24:00 or more, or with no readable procedure start)
        # leaves the dialog open: stored as NA it would stop every plot.
        format <- doseTableFormat()
        clickTime <- displayTimeToMinutes(validateEntry(input$clickTimeEvent, validateTime),
                                          referenceFor(format), format[["unit"]])
        if (is.na(clickTime)) {
          showNotification(paste0("That time could not be read: enter it as ",
                                  timeEntryUnitText(format), "."), type = "error")
          return()
        }

        # One event from the list, and room for it.  An empty or vector value
        # stopped data.frame() below and ended the session, and an event not
        # in the list, or a 101st, made the plot an error, taking with it the
        # Events panel, the only way back to the dialog that deletes events
        # (Carbon's UI audit, 2026-11-14, F3 and F6C).  The dialog stays open.
        clickEvent <- input$clickEvent
        if (!is.character(clickEvent) || length(clickEvent) != 1 ||
            !isTRUE(clickEvent %in% eventDefaults()$Event)) {
          showNotification("Choose an event from the list.", type = "error")
          return()
        }
        ET <- eventTable()
        if (nrow(ET) >= MAX_EVENT_ROWS) {
          showNotification(sprintf("The event table holds at most %d events.", MAX_EVENT_ROWS),
                           type = "error")
          return()
        }
        ET <- data.frame(
          Time  = c(ET$Time, clickTime),
          Event = c(ET$Event, clickEvent)
        )
        ET <- ET[order(ET$Time,ET$Event),]
        eventTable(ET)
        removeModal()
      }, name = "input$addEventBtn observer")
    })

  # The times as the edit-events dialog showed them, and the minutes behind
  # them (editedTimesToMinutes()).  A plain variable: set when the dialog opens.
  editEventsShown <- NULL

  showEditEventsModal <- function()
  {
    tempTable <- eventTable()
    hasEvents <- nrow(tempTable) > 0
    format <- doseTableFormat()

    if (hasEvents) {
      tempTable <- tempTable[,c("Time", "Event")]
      # Shown as text in the dose table's format (events are stored in
      # minutes).  A numeric column would also be rounded to four digits on
      # its way back from the browser.
      minutes <- tempTable$Time
      if (!is.numeric(minutes)) minutes <- as.numeric(as.character(minutes))
      tempTable$Time <- minutesToEntryTime(minutes, format, procedureStart())
      editEventsShown <<- list(text = tempTable$Time, minutes = minutes)
      tempTable$Delete <- FALSE
      eventsGrid <- tableGridInput("editEventsTableHTML", editEventsGridColumns(eventDefaults()$Event),
                                   tempTable[, c("Delete", "Time", "Event")],
                                   maxRows = MAX_EVENT_ROWS, height = 260, label = "Events")
    }

    showModal(
      modalDialog(
        title = "Edit Events",
        if (hasEvents)
          tagList(
            tags$p(class = "small text-muted mb-1", timeEntryLabel(format)),
            eventsGrid
          )
        else
          tags$p("There are no events yet."),
        if (hasEvents) actionButton("editEventsOK", "Apply", class = "btn-primary"),
        if (hasEvents) actionButton("deleteAllEventsBtn", "Delete All Events", class = "btn-outline-danger"),
        tags$button(
          type = "button",
          class = "btn float-right",
          `data-bs-dismiss` = "modal",
          "Cancel"
        ),
        footer = NULL,
        easyClose = TRUE,
        size = "s"
      )
    )
  }

  observeEvent(input$deleteAllEventsBtn, {
    removeModal()
    if (nrow(eventTable()) > 0) {
      showModal(
        modalDialog(
          title = "Delete all events?",
          "Are you sure you want to delete all events?",
          br(), br(),
          actionButton("confirmDeleteAllEvents", "Yes", class = "btn-primary"),
          tags$button(
            type = "button",
            class = "btn float-right",
            `data-bs-dismiss` = "modal",
            "Cancel"
          ),
          footer = NULL,
          easyClose = TRUE,
          size = "m"
        )
      )
    } else {
      eventTable(eventTableInit)
    }
  })

  observeEvent(input$confirmDeleteAllEvents, {
    removeModal()
    eventTable(eventTableInit)
  })

  observeEvent(
    input$editEventsOK,
    {
      profileCode({
        req(input$editEventsTableHTML)
        ET <- tableGridRead(input$editEventsTableHTML, editEventsGridColumns(character(0)),
                            maxRows = MAX_EVENT_ROWS)
        req(ET)
        # The grid holds MAX_EVENT_ROWS events at most: a table cut to fit was
        # never one the grid sent, and stored whole it replaced the plot with
        # the row-limit error after the dialog had closed (fourth review, F04).
        if (tableGridTruncated(ET)) {
          showNotification(sprintf("The event table holds at most %d events.", MAX_EVENT_ROWS),
                           type = "error")
          return()
        }
        format <- doseTableFormat()
        shown <- editEventsShown
        minutes <- editedTimesToMinutes(ET$Time, shown$text, shown$minutes,
                                        format, referenceFor(format))
        keep <- !tickedRows(ET$Delete)
        if (any(is.na(minutes[keep]))) {
          # The dialog stays open to be corrected
          showNotification(paste0("Every event needs a time, entered as ",
                                  timeEntryUnitText(format), "."), type = "error")
          return()
        }
        # And an event from the list.  The dropdown takes Delete, which
        # stored a blank event: the plot then said "unknown event" and could
        # not be drawn, with the Events panel, the way back to this dialog,
        # gone with it (adversarial UI review, 2026-11, finding 8).
        if (!all(as.character(ET$Event[keep]) %in% eventDefaults()$Event)) {
          showNotification("Every event needs an event chosen from the list.", type = "error")
          return()
        }
        removeModal()
        ET <- data.frame(Time = minutes[keep], Event = as.character(ET$Event[keep]))
        ET <- ET[order(ET$Time,ET$Event),]
        eventTable(ET)
      }, name = "input$editEventsOK observer")
    })

  deleteDrugDoses <- function(drug) {
    dt <- doseTable()
    dt <- dt[!dt$Drug %in% drug, ]
    doseTable(dt)
  }

  # Target Drug Dosing (TCI Like) ###########################################
  # Event to trigger calculation to set doses for a target

  observeEvent(
    input$setTarget,
    {
      profileCode({
        # Suggest Dosing is an induction and maintenance tool: its infusion
        # times are whole minutes, and its fit is over the plot's equispaced
        # points.  It is offered on the minute and hour plots only.
        format <- doseTableFormat()
        if (!format[["unit"]] %in% CLOCK_TIME_UNITS) {
          showNotification("Suggest Dosing is available when the Time units are minutes or hours.",
                           type = "warning")
          return()
        }
        targetTable <-  data.frame(
          Time = rep("",6),
          Target = rep("", 6)
        )
        # Rows can be inserted and removed (Shift+F10, the context-menu key
        # or a right click), up to MAX_TARGET_ROWS
        targetGrid <- tableGridInput("targetTableHTML", targetGridColumns(), targetTable,
                                     rowMenu = TRUE, maxRows = MAX_TARGET_ROWS, height = 260,
                                     label = "Targets")
        showModal(
          modalDialog(
            `data-submit-btn` = "targetOK",
            title = paste("Enter Target Effect Site Concentrations"),
            div(
              class = "fw-bold text-danger",
              "Enter time and target concentration below. Decreasing targets are not supported: a target lower than the one before is raised to it. Doses are found with non-linear regression, which takes a moment to calculate. The suggestion will be good, but better algorithms likely exist."
            ),
            selectInput(
              inputId = "targetDrug",
              label = "Drug",
              # Only the drugs it can target: an effect site, and intravenous
              # bolus and infusion units; alphabetical, as the other pickers
              choices = sortDrugNames(suggestDrugChoices(drugDefaults()))
            ),
            tags$p(class = "small text-muted mb-1", timeEntryLabel(format)),
            targetGrid,
            textInput(
              inputId = "targetEndTime",
              label = paste("End", timeEntryLabel(format)),
              value = ""
            ),
            conditionalPanel(
              condition = "input.targetEndTime != ''",
              actionButton(
                inputId = "targetOK",
                label = "OK",
                style = "
              color: #fff;
              background-color: #337ab7;
              border-color: #2e6da4;
              float: left;
              margin: 0px 5px 5px 5px;
           "
              )
            ),
            tags$button(
              type = "button",
              class = "btn btn-warning float-right",
              style = "margin: 0px 5px 5px 5px;",
              `data-bs-dismiss` = "modal",
              "Cancel"
            ),
            footer = NULL,
            easyClose = TRUE,
            size="s"
          )
        )
      }, name = "input$setTarget observer")
    })

  # Evaluate target concentration
  observeEvent(
    input$targetOK,
    {
      profileCode({
        # One value each, whatever the browser sent: the End time box and the
        # drug list send one string, but a forged pair of End times made the
        # test below an if() of length 2, which ended the session (ChatGPT's
        # production audit, 2026-11-08, F02).  doseGridCell() makes anything
        # that is not one value blank.
        endText <- doseGridCell(input$targetEndTime)
        targetDrug <- doseGridCell(input$targetDrug)
        if (!targetDrug %in% suggestDrugChoices(drugDefaults())) {
          showNotification("Choose a drug from the list.", type = "error")
          return()
        }
        # Tested before validateTime(), which turns a blank into "0"
        if (!nzchar(trimws(endText)))
        {
          outputComments("No endtime")
          return()
        }
        # Not before the grid has reported in (a click on OK while it draws)
        req(input$targetTableHTML)
        targetTable <- tableGridRead(input$targetTableHTML, targetGridColumns(), maxRows = MAX_TARGET_ROWS)
        req(targetTable)
        # A table cut to MAX_TARGET_ROWS was never one the grid sent (Gemini's
        # production audit, 2026-11-08, finding 6)
        if (tableGridTruncated(targetTable)) {
          showNotification(sprintf("The target table holds at most %d targets.", MAX_TARGET_ROWS),
                           type = "error")
          return()
        }
        # What is wrong with the table is said, and the dialog stays open:
        # raised here, the error ended the session (adversarial UI review,
        # 2026-11, findings 1 and 2: a time with no target, an inserted row).
        problem <- targetTableProblem(targetTable)
        if (!is.null(problem)) {
          showNotification(problem, type = "error")
          return()
        }
        # Nothing can be suggested while the simulation cannot run: the plot
        # area says why.  Found after the spinner, drugs()'s req() ended the
        # observer silently and left the page behind "Calculating doses..."
        # for good (finding 10).
        blocked <- simulationBlocked()
        # The patient's ranges, which drugs() asks of testCovariates() and a
        # drug not yet in the table, whose PK is built here by drugPK(),
        # would otherwise skip (CodeRabbit on #681): a weight above
        # MAX_WEIGHT is a number, so simulationBlocked() passes it.
        if (is.null(blocked)) {
          covariateProblem <- NULL
          ok <- checkNumericCovariates(age(), weight(), height(),
                                       function(msg) covariateProblem <<- msg,
                                       osmolality = osmolality(),
                                       creatinine = creatinine(),
                                       gestationalAge = gestationalAge(),
                                       albumin = albumin(),
                                       ast = ast())
          if (!isTRUE(ok)) blocked <- if (is.null(covariateProblem)) "the Patient Profile is out of range." else covariateProblem
        }
        if (!is.null(blocked)) {
          showNotification(paste("Suggest Dosing cannot run:", blocked), type = "error", duration = 10)
          return()
        }

        # The times are typed in the dose table's format.  suggest() is given
        # them as minutes, and its suggested doses are written back in that
        # format.
        format <- doseTableFormat()
        reference <- referenceFor(format)
        toMinutes <- function(x) {
          displayTimeToMinutes(validateTime(x), reference, format[["unit"]])
        }
        endTime <- toMinutes(endText)
        targetTimes <- as.character(targetTable$Time)
        typed <- !is.na(targetTimes) & nzchar(trimws(targetTimes))
        targetMinutes <- vapply(targetTimes[typed], toMinutes, numeric(1))
        if (is.na(endTime) || anyNA(targetMinutes)) {
          # The dialog stays open to be corrected
          showNotification(paste0("A time could not be read: enter times as ",
                                  timeEntryUnitText(format), "."), type = "error")
          return()
        }
        targetTimes[typed] <- minutesToDisplayTime(targetMinutes, "minutes")
        targetTable$Time <- targetTimes

        removeModal()
        # suggest() reads only the drug's PK set and simulates its own
        # regimens.  A drug the simulation does not yet carry has its PK built
        # here, from a 0 dose, rather than written into the dose table as a
        # placeholder row: that write reset the draft, so a suggestion that
        # failed lost the user's unapplied edits and the undo history
        # (drugPK()).  Asked of the rows the simulation reads: a row whose
        # Units were cleared is ignored by it, and suggest() stopped with
        # "unknown drug" (finding 4).  In the drug's own bolus unit, not "mg":
        # vasopressin is dosed in units, and a mass unit stopped simCpCe()
        # (finding 3).  Nothing is written to doseTable() unless the regimen
        # is written.
        # A drug in the table whose doses are all 0 is not drawn, and so not
        # in drugs() either (drawnDrugEntries(), R/drug-pipeline.R): its PK
        # is built here too, which, with no dose above 0 to fit, is the
        # model's, as drugs() would have carried it.
        inTable <- targetDrug %in% cleanDoseTable(doseTable())$Drug
        suggestDrugs <- function() {
          if (inTable) {
            drawn <- drugs()
            if (targetDrug %in% names(drawn)) return(drawn)
          }
          bolusUnit <- drugDefaults()$Bolus.Units[match(targetDrug, drugDefaults()$Drug)]
          drugPK(data.frame(Drug = targetDrug, Time = 0, Dose = 0, Units = bolusUnit,
                            stringsAsFactors = FALSE))
        }

        # Any error from here is said: an error ended the session, and a req()
        # left the spinner it then had up for good (the page's Working cover,
        # app.js, now covers a long calculation).
        testTable <- tryCatch(
          suggest(targetDrug,
                  targetTable,
                  endTime,
                  suggestDrugs(),
                  drugList,
                  eventTable(),
                  REFERENCE_TIME_NONE),
          shiny.silent.error = function(e) structure(list(), class = "suggestFailed",
            message = "the simulation cannot run; the plot area says why."),
          error = function(e) structure(list(), class = "suggestFailed",
            message = conditionMessage(e))
        )
        if (inherits(testTable, "suggestFailed")) {
          showNotification(paste("Suggest Dosing failed:", attr(testTable, "message")),
                           type = "error", duration = 10)
          return()
        }

        if (is.null(testTable)) {
          # No target fell between the start and the end time (issue #270):
          # say so rather than close the dialog with nothing done.
          showNotification(SUGGEST_NO_TARGETS_MESSAGE, type = "warning", duration = 10)
          return()
        }

        outputComments("Setting doseTable")
        # Numeric minutes from suggest(), into the table's format: numbers of
        # the unit, which clock mode reads as offsets from the start, as these
        # rows always were (a clock time would round a dose at 2.5 minutes to
        # the minute).  rbind() would otherwise turn them into strings with
        # as.character(), which writes 1e5 as "1e+05".
        testTable$Time <- minutesToDisplayTime(testTable$Time, format[["unit"]])
        dt <- doseTable()
        dt <- dt[!dt$Drug %in% targetDrug, ]

        dt <- rbind(
          testTable[,c("Drug","Time","Dose","Units")],
          dt
        )
        # A regimen the dose table cannot hold is not written.  A target of
        # 1e8 mcg/mL suggested a dose above MAX_DOSE_VALUE, which replaced the
        # dose grid and the plot with "Dose must be finite..." -- and writing
        # the table clears the undo history, so nothing was left to edit or
        # undo (keyboard review of 2026-11-02).
        refused <- tryCatch({ validateDoseTableInput(dt, drugDefaults()); NULL },
                            error = function(e) conditionMessage(e))
        if (!is.null(refused)) {
          showNotification(paste("Suggest Dosing found doses the dose table cannot hold:", refused,
                                 "Choose lower targets."), type = "error", duration = 10)
          return()
        }
        commitDoseTable(dt)
      }, name = "input$targetOK observer")
    })

  editDrugsTrigger <- makeReactiveTrigger()
  observeEvent(input$editDrugs, {
    editDrugsTrigger$trigger()
    showModal(
      modalDialog(
        title = paste("Edit Drug Defaults"),
        div(
          class = "fw-bold text-danger",
          "This is primarily intended for stanpumpR collaborators. If you believe some drug defaults are incorrect, please contact",
          tags$a("steven.shafer@stanford.edu", href = "mailto:steven.shafer@stanford.edu"),
          ". Also, you can easily break your session by entering crazy things. If so, just reload your session."
        ),
        br(),
        shinycssloaders::withSpinner(uiOutput("editDrugsUI")),
        br(),
        actionButton("drugEditsOK", "Apply", class = "btn-primary"),
        tags$button(
          type = "button",
          class = "btn float-right",
          `data-bs-dismiss` = "modal",
          "Cancel"
        ),
        footer = NULL,
        easyClose = TRUE,
        size="l"
      )
    )
  })

  output$editDrugsUI <- renderUI({
    profileCode({
      editDrugsTrigger$depend()
      x <- drugLibraryGridData(drugDefaults())
      tableGridInput("editDrugsHTML", drugLibraryGridColumns(x), x, height = 350, label = "Drug Library")
    }, name = "output$editDrugsUI")
  })

  # Evaluate target concentration
  observeEvent(input$drugEditsOK, {
    profileCode({
      current      <- drugDefaults()
      req(input$editDrugsHTML)
      newDrugDefaults <- tableGridRead(input$editDrugsHTML, drugLibraryGridColumns(drugLibraryGridData(current)),
                                       maxRows = nrow(current))
      req(newDrugDefaults)
      if (tableGridTruncated(newDrugDefaults)) {
        showNotification("The Drug Library cannot add drugs.", type = "error", duration = 10)
        return()
      }
      # An entry the plot or the dose table cannot use is refused, and the
      # dialog stays open to be corrected (drugLibraryProblems()).
      problems <- drugLibraryProblems(newDrugDefaults, current,
                                      inUse = rbind(dosedUnits(doseTable()), dosedUnits(doseTableDraft())))
      if (length(problems) > 0) {
        showNotification(paste(problems, collapse = " "), type = "error", duration = 10)
        return()
      }
      edited <- newDrugDefaults
      newDrugDefaults <- drugLibraryTable(edited, current)
      # A drug moved between mcg and ng keeps its range, MEAC and threshold,
      # converted (drugLibraryRescaleThresholds()), and its TCI targets and
      # measured concentrations, concentrations in its units too (Carbon's
      # second UI audit, 2026-11-15, G6).  Each must still be one the tables
      # can hold, and a dose table with edits not yet applied is applied or
      # undone first: converted only in the committed table, a target typed
      # in the draft was left in the old units, and the committed table's
      # write erased the draft and its undo history (Carbon's third UI
      # audit, 2026-11-15, C4).  Refused, the dialog stays open.
      rescaled <- drugLibraryRescaleThresholds(newDrugDefaults, edited, current)
      newDrugDefaults <- rescaled$table
      if (length(rescaled$drugs) > 0) {
        refuse <- function(why) {
          showNotification(paste("The concentration units were not changed:", why),
                           type = "error", duration = 10)
        }
        dtNow <- doseTable()
        if (!identicalTable(doseTableDraft(), dtNow)) {
          refuse("the dose table has changes not yet applied. Apply or undo them first.")
          return()
        }
        dtRescaled <- doseTableRescaleTargets(dtNow, rescaled$factors)
        if (is.null(dtRescaled)) {
          refuse(paste("a TCI target of", paste(rescaled$drugs, collapse = ", "),
                       "would be above 1,000,000,000 in the new units."))
          return()
        }
        if (!bayes$canRescale(rescaled$factors)) {
          refuse(paste("a measured concentration of", paste(rescaled$drugs, collapse = ", "),
                       "would be above 1,000,000,000 in the new units."))
          return()
        }
        # A measured value whose drug is not chosen yet cannot be converted,
        # and would be read in the new units once it is (Carbon's fourth UI
        # audit, 2026-11-15, C1)
        if (bayes$hasValueWithoutDrug()) {
          refuse("a measured concentration has no drug chosen yet. Choose its drug or clear it first.")
          return()
        }
        # Nor may a converted range, MEAC or threshold pass the ceiling: a
        # saved link then could not apply the library, and its targets were
        # read in the other units (the same audit, C2)
        moved <- newDrugDefaults$Drug %in% rescaled$drugs
        numbers <- as.matrix(newDrugDefaults[moved, intersect(c(DRUG_LIBRARY_NUMERIC_COLUMNS, "endCe"),
                                                              names(newDrugDefaults)), drop = FALSE])
        if (any(is.finite(numbers) & numbers > MAX_DOSE_VALUE)) {
          refuse(paste("a range, MEAC or threshold of", paste(rescaled$drugs, collapse = ", "),
                       "would be above 1,000,000,000 in the new units."))
          return()
        }
        # The range in its final units: one bound typed in the new units and
        # the other converted can cross (1-2 mcg/mL to ng/mL with Upper
        # typed as 500 gave 1000-500; CodeRabbit on PR #730)
        lowerFinal <- suppressWarnings(as.numeric(newDrugDefaults$Lower[moved]))
        upperFinal <- suppressWarnings(as.numeric(newDrugDefaults$Upper[moved]))
        crossed <- is.finite(lowerFinal) & is.finite(upperFinal) & lowerFinal > upperFinal
        if (any(crossed)) {
          refuse(paste0("Lower must not be above Upper in the new units (",
                        paste(newDrugDefaults$Drug[moved][crossed], collapse = ", "), ")."))
          return()
        }
        # The converted targets are written first, and nothing else changes
        # if they cannot be: a full table whose inhaled agent lacks a row
        # refused them, and the library and the levels moved to the new
        # units without them (CodeRabbit on PR #730)
        if (!identical(dtRescaled, dtNow) && !isTRUE(commitDoseTable(dtRescaled))) {
          refuse("its converted TCI targets could not be written to the dose table.")
          return()
        }
      }
      removeModal()
      if (length(rescaled$drugs) > 0) {
        showNotification(paste0(
          "The range, MEAC, threshold, TCI targets and measured concentrations of ",
          paste(rescaled$drugs, collapse = ", "),
          " were converted to the new concentration units."), type = "message", duration = 10)
        bayes$rescale(rescaled$factors)
      }
      # A change of units also ends the dose table's undo and redo history:
      # an entry undone before the change and redone after it came back in
      # the old units (a target of 3000 ng/mL as 3000 mcg/mL) or with a unit
      # the edit had removed (Carbon's fourth UI audit, 2026-11-15, C1)
      unitColumns <- c("Concentration.Units", "Units", "Bolus.Units", "Infusion.Units", "Default.Units")
      if (!identical(newDrugDefaults[unitColumns], current[unitColumns]))
        doseTableHistory()$do(doseTableDraft())$clear()

      drugDefaults(newDrugDefaults)
    }, name = "input$drugEditsOK observer")
  })

  drugThresholdsTrigger <- makeReactiveTrigger()

  observeEvent(input$editThresholds, {
    drugThresholdsTrigger$trigger()
    showModal(
      modalDialog(
        title = "Drug Thresholds",
        p("Set the threshold concentration for each drug."),
        p(class = "small text-muted",
          "Inhaled agents are in %, shown for this patient's age; the MAC row is in MAC equivalents (multiples of the age-adjusted MAC)."),
        # identical() and isTRUE(): either value, not as a browser sends it,
        # stopped this observer and ended the session (Carbon's UI audit,
        # 2026-11-14, F6E)
        if (identical(input$normalization, NORMALIZE_NONE))
          checkboxInput("showThresholdModal", "Show time until threshold",
                        value = isTRUE(input$showThreshold)),
        shinycssloaders::withSpinner(uiOutput("editThresholdsUI")),
        br(),
        actionButton("thresholdEditsOK", "Apply", class = "btn-primary"),
        tags$button(
          type = "button",
          class = "btn float-right",
          `data-bs-dismiss` = "modal",
          "Cancel"
        ),
        footer = NULL,
        easyClose = TRUE,
        size = "s"
      )
    )
  })

  output$editThresholdsUI <- renderUI({
    drugThresholdsTrigger$depend()
    # Shown at the patient's age for the volatile agents, and with a row for
    # MAC; see thresholdTableForDisplay().  isolate(): opening the dialog is
    # what refreshes it, not a change of age behind it.
    x <- thresholdTableForDisplay(
      drugDefaults(),
      tryCatch(isolate(age()), error = function(e) 40),
      isolate(macThreshold())
    )
    tableGridInput("editThresholdsTable", thresholdGridColumns(), x, height = 350,
                   label = "Drug thresholds")
  })

  observeEvent(input$thresholdEditsOK, {
    req(input$editThresholdsTable)
    # No more rows than the dialog drew (Carbon's third UI audit, 2026-11-15, L1)
    tt <- tableGridRead(input$editThresholdsTable, thresholdGridColumns(),
                        maxRows = nrow(drugDefaults()) + 1L)
    req(tt)
    # Nor a thresholds table cut to fit (the same audit, finding 6)
    if (tableGridTruncated(tt)) {
      showNotification("Threshold not changed: the table holds more rows than the library has drugs.",
                       id = "thresholdInvalid", type = "error", duration = 10)
      return()
    }
    updated <- thresholdTableToDefaults(
      tt, drugDefaults(),
      tryCatch(isolate(age()), error = function(e) 40),
      macThreshold()
    )
    # An entry that is not a number is refused, not read as 0 (no threshold):
    # the dialog stays open, with every threshold as it was, until corrected.
    if (length(updated$invalid) > 0) {
      showNotification(
        paste0(
          "Threshold not changed: enter a number from 0 to 1,000,000,000 for ",
          paste(unique(updated$invalid), collapse = ", "),
          ". Enter 0 for no threshold."
        ),
        id = "thresholdInvalid", type = "error", duration = 10
      )
      return()
    }
    removeNotification("thresholdInvalid")
    removeModal()
    drugDefaults(updated$drugDefaults)
    macThreshold(updated$macThreshold)
    # The dialog offers the box only without normalization; otherwise
    # input$showThresholdModal is the value left from an earlier dialog, and
    # is not this one's (adversarial UI review, 2026-11, finding 13).
    if (identical(input$normalization, NORMALIZE_NONE) && !is.null(input$showThresholdModal)) {
      updateCheckboxInput(session, "showThreshold", value = isTRUE(input$showThresholdModal))
    }
  })

  outputComments("Reached the end of server()")
}
