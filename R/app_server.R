####################################################
# stanpumpR                                        #
# Copyright, 2023, Steven L. Shafer, MD            #
# May be freely distributed, modified, or adapted  #
# in derivative works for non-commercial purposes. #
####################################################

app_server <- function(input, output, session) {
  config <- .sprglobals$config

  session$userData$debug <- reactiveVal({
    query <- parseQueryString(isolate(session$clientData$url_search))
    if (!is.null(query[["debug"]])) {
      as.numeric(query[["debug"]])
    } else {
      config$debug
    }
  })
  observeEvent(input$debug_level, ignoreInit = TRUE, {
    session$userData$debug(input$debug_level)
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

  profileRecords <- reactiveVal(data.frame(name = character(0), ms = numeric(0), time = character(0)))

  profileCode <- function(expr, name, threshold = NULL) {
    if (isolate(session$userData$debug()) == DEBUG_LEVEL_OFF) {
      return(force(expr))
    }
    if (is.null(threshold)) {
      threshold <- isolate(input$profiler_threshold)
    }
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
      sprintf("[%s] %s (%d ms)", records$time, records$name, records$ms),
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

  main_plot <- reactive({
    outputComments("In main_plot", level = DEBUG_LEVEL_VERBOSE)
    tryCatch({
      if (is.null(doseTableClean()) || is.null(drugs()) || is.null(plotObjectReactive())) {
        # nothingtoPlot
      } else {
        plotObjectReactive()
      }
    }, error = function(err) {
      NULL
    })
  })

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

  output$PlotSimulation <- renderPlot({
    outputComments("In output$PlotSimulation", level = DEBUG_LEVEL_VERBOSE)
    # Said here, in the plot area, because main_plot() swallows errors and the
    # plot would otherwise just stay as it was.
    violation <- timeUnitViolation()
    validate(need(is.null(violation), violation))
    problem <- patientEntryProblem()
    validate(need(is.null(problem), problem))
    req(main_plot(), cancelOutput = TRUE)
    main_plot()
  }, height = function() {
    # renderPlot() measures before it draws, and with a violation there is no
    # plot to measure: a fixed height lets the message above through.
    if (!is.null(timeUnitViolation()) || !is.null(patientEntryProblem())) 150 else plotHeightOrNothing()
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

  # Routine to output doseTableHTML from doseTable
  output$doseTableHTML <- rhandsontable::renderRHandsontable({
    req(doseTableDraft())
    req(validateDoseTableInput(doseTableDraft()))
    doseTableRefresh()
    # A change of format that leaves every string as it was (no times, or
    # only clock times) must still redraw the grid, to renew its stamp.
    format <- doseTableFormat()

    profileCode({
      outputComments("Rendering doseTableHTML")

      createHOT(doseTableDraft(), drugDefaults(), format, drugChoices())
    }, name = "createHOT() from doseTableHTML")
  })

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
    timeFormat(timeUnit(), if (is.null(input$timeMode)) "clock" else input$timeMode)
  })

  # Starts as the selectors start: minutes and clock time, as the UI opens, or
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
    if (isClockFormat(format)) input$referenceTime else REFERENCE_TIME_NONE
  }

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

  # Which unit the Max time and Time Display choices were last sent for.
  # Plain variables, not reactive values: they record what the browser was
  # told.  The initial inputs (a restored bookmark's too) are in place before
  # the server function runs, and the UI built both lists for them.
  maxChoicesUnit <- isolate(timeUnit())
  modeChoicesUnit <- isolate(timeUnit())

  # Offer the unit's Max time choices.  The choices and the selection are
  # always sent together: selectize ignores a selection that is not among the
  # options it has, and will not send an empty value.  Without `selected` the
  # current Max time is kept if the unit offers it, else snapped to the
  # nearest longer choice.
  syncMaxTimeChoices <- function(unit, selected = NULL) {
    if (identical(unit, maxChoicesUnit) && is.null(selected)) return(invisible())
    if (is.null(selected)) selected <- snapMaximum(isolate(input$maximum), unit)
    updateSelectInput(session, "maximum", choices = maxTimeChoices(unit),
                      selected = maxTimeValue(snapMaximum(selected, unit)))
    maxChoicesUnit <<- unit
    invisible()
  }

  syncTimeModeChoices <- function(unit, selected) {
    updateSelectizeInput(session, "timeMode", choices = timeModeChoices(unit), selected = selected)
    modeChoicesUnit <<- unit
    invisible()
  }

  # What the server uses to change the time settings itself: loading a
  # scenario, restoring a bookmark, the long-term drug prompt.  Setting the
  # selectors this way and writing the table with setDoseTable() in the format
  # they will report means their echo finds nothing to convert.
  timeApi <- list(
    setDoseTable = setDoseTable,
    showTimeSettings = function(unit, mode, maximum) {
      format <- timeFormat(unit, mode)
      updateSelectInput(session, "timeUnits", selected = format[["unit"]])
      syncTimeModeChoices(format[["unit"]], format[["mode"]])
      syncMaxTimeChoices(format[["unit"]], selected = maximum)
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
      syncMaxTimeChoices(unit)
      return()
    }

    draft <- doseTableDraft()
    res <- rebaseDoseTimes(draft, from, target, input$referenceTime)
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
    syncMaxTimeChoices(unit)
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
  helpPage <- helpServer(input, output, session, doseTable, eventTable, drugDefaults,
                         timeApi = timeApi)

  ###########
  # Startup #
  ###########

  # A fresh session opens on the drug menu (R/startup-drugs.R).  A bookmark
  # carries its own dose table, which onRestored() puts in, so it skips the
  # menu unless that table is empty.  Shown now, as the session starts, rather
  # than in reply to the browser.
  startupMenuOpen <- opensOnStartupMenu(session$restoreContext$values)
  if (startupMenuOpen) {
    showModal(startupDrugModal(startupDrugChoices(isolate(drugDefaults()))))
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
    } else {
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
  observeEvent(input$client_time, {
    if (input$referenceTime != '') {
      return()
    }
    outputComments("In observeEvent(input$client_time,...", level = DEBUG_LEVEL_VERBOSE)
    time <- input$client_time
    outputComments("Reference time from client:", time)
    start <- getReferenceTime(time)
    outputComments("Calculated reference time:", start)
    updateNumericInput(session, "referenceTime", value = start)
  }, ignoreNULL = TRUE, once = TRUE)

  # The procedure start for what is drawn (the axis and hover labels), which
  # follow the selectors: REFERENCE_TIME_NONE when times are elapsed, and in
  # clock mode a readable "HH:MM" (it waits until there is one).
  referenceTime <- reactive({
    reqReference(selectorFormat())
  })

  DrugTimeUnits <- reactiveVal("")

  ##########################################################
  # Code to save state in url and then restore from url

  url <- reactiveVal("")

  observe({
    profileCode({
      # Trigger this observer every time an input changes
      reactiveValuesToList(input)
      session$doBookmark()
    }, name = "doBookmark observer")
  })

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
      state$values$endCe <- stats::setNames(drugDefaults()$endCe, drugDefaults()$Drug)
      setBookmarkExclude(bookmarksToExclude)
    }, name = "onBookmark()")
  })

  # This gets called after bookmarking is completed
  onBookmarked(function(url) {
    profileCode({
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
      if (is.null(saved)) {
        saved <- timeFormat(TIME_UNIT_DEFAULT,
                            if (is.null(state$input$timeMode)) "clock" else state$input$timeMode)
      }
      # Always there: a link without a dose table returned above
      DT <- as.data.frame(state$values$DT)
      target <- selectorFormat()
      restoredMaximum <- suppressWarnings(as.numeric(state$input$maximum))
      res <- rebaseDoseTimes(DT, saved, target, input$referenceTime)
      if (res$ok) {
        setDoseTable(res$table, target)
        if (!isTRUE(restoredMaximum %in% MAX_TIMES[[target[["unit"]]]]$times)) {
          # A Max time the unit does not offer (an old bookmark's 16 weeks):
          # the browser fell back to the first choice; take the nearest
          # longer one instead.
          syncMaxTimeChoices(target[["unit"]], selected = snapMaximum(restoredMaximum, target[["unit"]]))
        }
      } else {
        # Clock times and no readable procedure start: keep the table as
        # saved, and show the selectors that match it.
        setDoseTable(DT, saved)
        timeApi$showTimeSettings(saved[["unit"]], saved[["mode"]], restoredMaximum)
        showNotification("The dose table's clock times need a procedure start (HH:MM).",
                         type = "warning", duration = 10)
      }
      outputComments("doseTable:")
      outputComments(DT)
      ET <- as.data.frame(state$values$ET)
      if (ncol(ET) == 0) {
        ET <- eventTableInit
      } else {
        ET <- ET[, c("Time", "Event")]
      }
      eventTable(ET)
      outputComments("eventTable:")
      outputComments(ET)
      # Thresholds, when the bookmark has them; bookmarks made before they
      # were saved keep the defaults.
      restored <- restoreThresholds(drugDefaults(), state$values$endCe,
                                    state$values$macThreshold)
      drugDefaults(restored$drugDefaults)
      macThreshold(restored$macThreshold)
      outputComments("macThreshold:", restored$macThreshold)
      # Bookmarks made before the fat-free-mass switch existed were simulated
      # on total body weight; restore them that way so their output is unchanged.
      if (is.null(state$input$adjustToFFM)) {
        updateCheckboxInput(session, "adjustToFFM", value = FALSE)
      }
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

  observeEvent(doseTable(), {
    # Every route into doseTable() -- Apply Changes, the add-dose dialog,
    # Suggest Dosing, restoring a bookmark -- passes through here, so this is
    # where the gas rules (see applyGasTableRules()) are enforced.  Fixing the
    # table sets doseTable() again, which brings this observer straight back.
    fixed <- applyGasTableRules(doseTable(), gasVentilationWeight())
    if (!identicalTable(fixed, doseTable())) {
      doseTable(fixed)
      return()
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

  observeEvent(input$dosetable_apply, {
    shinyjs::disable("dosetable_apply")
    doseTable(doseTableDraft())
  })

  observeEvent(input$dosetable_undo, {
    doseTableHistory()$undo()
  })

  observeEvent(input$dosetable_redo, {
    doseTableHistory()$redo()
  })

  observeEvent(input$doseTableHTML, {
    profileCode({
      outputComments("In observeEvent(input$doseTableHTML,...", level = DEBUG_LEVEL_VERBOSE)
      data <- input$doseTableHTML

      if (is.null(data$changes$source)) {
        if ("changes" %in% names(data) &&
            "event" %in% names(data$changes) &&
            data$changes$event %in% c("afterCreateRow", "afterRemoveRow")) {
          # If we get here because a row was added or removed, keep going
        } else {
          return()
        }
      }

      if ("changes" %in% names(data) &&
          "source" %in% names(data$changes) &&
          data$changes$source == "edit") {
        return()
      }

      # Because of a bug in hot_to_r(), we can't use it directly. We need to manually change
      # the row names for it to work
      nrows <- length(data$data)
      data$params$rRowHeaders <- as.character(seq.int(nrows))
      # The format of the grid the edit was made in (createHOT() stamps it).
      stamp <- parseTimeFormat(data$params$timeFormat)
      data <- rhandsontable::hot_to_r(data) |> profileCode("hot_to_r() in input$doseTableHTML observer")

      # An edit made in a grid drawn before a change of time unit or display
      # arrives in the old format: in minutes, say, when the table is now in
      # days.  Stored as it came, Apply would move every dose 1440-fold, so it
      # is converted; and the grid is redrawn, in the current format.
      format <- doseTableFormat()
      if (!is.null(stamp) && !identical(stamp, format)) {
        doseTableRefresh(doseTableRefresh() + 1)
        res <- rebaseDoseTimes(data, stamp, format, input$referenceTime)
        if (!res$ok) return()
        data <- res$table
      }

      # make sure that table has changed before updating doseTable reactive
      if ( !identicalTable(doseTableDraft(), data) ) {
        # add empty row at the bottom if needed
        if (nzchar(utils::tail(data, 1)$Drug)) {
          data[nrow(data) + 1, ] <- ""
        }

        # As soon as a gas is entered, apply the gas rules: oxygen alongside
        # nitrous oxide, flows rounded to 0.1 L/min, and a ventilation row.
        withVentilation <- applyGasTableRules(data, gasVentilationWeight())
        if (!identicalTable(withVentilation, data)) {
          data <- withVentilation
          doseTableRefresh(doseTableRefresh() + 1)
        }

        doseTableHistory()$do(data)
      }
    }, name = "input$doseTableHTML observer")
  })

  weightUnit <- reactive({
    req(input$weightUnit)
    as.numeric(input$weightUnit)
  })
  heightUnit <- reactive({
    req(input$heightUnit)
    as.numeric(input$heightUnit)
  })
  ageUnit <- reactive({
    req(input$ageUnit)
    as.numeric(input$ageUnit)
  })
  weight <- reactive({
    req(input$weight)
    input$weight * weightUnit()
  })
  height <- reactive({
    req(input$height)
    input$height * heightUnit()
  })
  age <- reactive({
    req(input$age)
    input$age * ageUnit()
  })
  sex <- reactive({
    req(input$sex)
    if (length(input$sex) != 1L || !input$sex %in% SEX_VALUES) {
      stop(safeError("Invalid sex value."))
    }
    input$sex
  })

  adjustToFFM <- reactive({
    # TRUE until the checkbox reports, so the first simulation uses the default.
    if (is.null(input$adjustToFFM)) TRUE else isTRUE(input$adjustToFFM)
  })

  osmolality <- reactive({
    # The default until the control reports, and for bookmarks made before it
    # existed (Shiny restores only the inputs a bookmark saved).
    if (is.null(input$osmolality)) OSMOLALITY_DEFAULT else input$osmolality
  })

  # NULL when the field is blank: the renal models then assume a normal value.
  creatinine <- reactive({
    x <- input$creatinine
    # NaN is not blank: it is passed on, and the covariate check rejects it.
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

  testCovariates <- reactive({
    profileCode({
      outputComments("In testCovariates", level = DEBUG_LEVEL_VERBOSE)
      req(weight(), height(), age(), sex())
      errorFxn <- function(msg) showModal(modalDialog(title = NULL, msg))
      checkNumericCovariates(age(), weight(), height(), errorFxn,
                             osmolality = osmolality(),
                             creatinine = creatinine())
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
      req(testCovariates(), is.null(timeUnitViolation()))
      DT <- doseTableClean()
      if (is.null(DT)) return(list())
      sim <- simulateGases(
        DT,
        weight  = weight(),
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

  drugs <- reactive({
    profileCode({
      outputComments("In drugs", level = DEBUG_LEVEL_VERBOSE)
      # Nothing is simulated while the time-units rule is broken: the plot
      # area says why (output$PlotSimulation).
      req(testCovariates(), doseTableClean(), is.null(timeUnitViolation()))

      newDrugs <- recalculatePK(
        NULL,
        drugDefaults(),
        doseTableIV(),
        age = age(),
        weight = weight(),
        height = height(),
        sex = sex(),
        # NULL on the first pass, before the control has reported in
        cyp2d6 = if (is.null(input$cyp2d6)) CYP2D6_DEFAULT else input$cyp2d6,
        cyp2c19 = if (is.null(input$cyp2c19)) CYP2C19_DEFAULT else input$cyp2c19,
        cyp2c9 = if (is.null(input$cyp2c9)) CYP2C9_DEFAULT else input$cyp2c9,
        osmolality = osmolality(),
        creatinine = creatinine(),
        adjustToFFM = adjustToFFM()
      ) |> profileCode("recalculatePK() in drugs()")

      newDrugs <- processdoseTable(
        doseTableIV(),
        eventTableClean(),
        newDrugs,
        plotMaximum(),
        plotRecovery(),
        cache = ivSimulationCache
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

      requestedMaximum <- suppressWarnings(as.numeric(input$maximum))
      if (!is_valid_number(requestedMaximum) || !requestedMaximum %in% MAX_TIME_VALUES) {
        stop(safeError("Invalid maximum simulation time."))
      }
      # A Max time of another unit: the choices for this one are on their way.
      unit <- timeUnit()
      maxTimes <- MAX_TIMES[[unit]]
      req(requestedMaximum %in% maxTimes$times)

      plotMaximum <- requestedMaximum
      steps <- maxTimes$steps[maxTimes$times == plotMaximum]
      maxTime <- max(as.numeric(doseTableClean()$Time),
                     as.numeric(eventTableClean()$Time),
                     na.rm = TRUE)

      # Lengthen the plot when the last dose or event comes within a margin of
      # its end, using the ticks of the unit's next longer Max time (minutes
      # and hours: 30 minutes, as always).  Never past the unit's longest Max
      # time: a longer plot is a matter of choosing a larger unit.
      margin <- TIME_EXTEND_MARGIN[[unit]]
      longest <- max(maxTimes$times)
      if ((maxTime + margin - 1) >= plotMaximum) {
        steps <- maxTimes$steps[maxTimes$times >= (maxTime + margin)][1]
        if (is.na(steps)) steps <- utils::tail(maxTimes$steps, 1)
        plotMaximum <- ceiling((maxTime + margin)/steps) * steps
        if (plotMaximum > longest) {
          plotMaximum <- longest
          steps <- utils::tail(maxTimes$steps, 1)
        }
      }

      # Doses and events the plot cannot reach (see the observer below)
      beyond <- c(
        doses = sum(doseTableClean()$Time >= plotMaximum),
        events = sum(as.numeric(eventTableClean()$Time) >= plotMaximum)
      )
      list(plotMaximum = plotMaximum, steps = steps, beyond = beyond)
    }, name = "plotInfo() reactive")
  })

  plotMaximum <- reactive(plotInfo()$plotMaximum)
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

  observe({
    violation <- timeUnitViolation()
    if (is.null(violation)) {
      removeNotification("timeUnitRule")
    } else {
      showNotification(violation, id = "timeUnitRule", type = "warning", duration = NULL)
    }
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


  plotRecovery <- reactive({
    input$showThreshold
  })

  linetypes <- reactive({
    setLinetypes(input$normalization,input$plasmaLinetype,input$effectsiteLinetype)
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
      # axis runs to plotMaximum() even when the tick step does not divide it.
      xBreaks <- seq(0, plotMaximum(), by = steps())
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
      plasmaLinetype         <- input$plasmaLinetype
      effectsiteLinetype     <- input$effectsiteLinetype
      normalization          <- input$normalization
      typical                <- input$typical
      logY                   <- input$logY
      if (plotRecovery() || plotEvents || plotInteraction) logY <- FALSE

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
        xMaximum = plotMaximum(),
        plasmaLinetype = plasmaLinetype,
        effectsiteLinetype = effectsiteLinetype,
        normalization = normalization,
        plotMEAC = plotMEAC,
        plotInteraction = plotInteraction,
        plotCost = plotCost,
        plotEvents = plotEvents,
        plotRecovery = plotRecovery(),
        typical = typical,
        logY = logY,
        yAxisHeight = input$yaxisHeight,
        width = input$plotWidth
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

  plotHeight <- reactive({
    simulationPlotRetval()$plotHeight
  })

  # Send Slide -----------------------------
  observeEvent(input$emailComments, {
    shinyjs::toggle("commentSafe", condition = nzchar(input$emailComments))
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
      } else if (nzchar(input$emailComments) && !input$commentSafe) {
        error <- "Please click the box confirming there is no PHI in the comments."
      } else if (nchar(input$emailComments) > MAX_INPUT_TEXT) {
        error <- glue::glue("Comment is too long, please limit to {MAX_INPUT_TEXT} characters.")
      } else if (emailSendCount() >= EMAIL_SESSION_LIMIT) {
        error <- "This session has reached its email limit. Please reload the page to send more."
      } else if (!is.null(timeUnitViolation())) {
        error <- timeUnitViolation()  # there is no plot to send
      }
      if (!is.null(error)) {
        shinyalert::shinyalert("Error",error, type = "error", closeOnClickOutside = TRUE)
        return()
      }

      values <- list(
        comments = input$emailComments,
        DT = doseTableClean(),
        url = url(),
        ageUnit = ageUnit(),
        weightUnit = weightUnit(),
        heightUnit = heightUnit(),
        age = age(),
        weight = weight(),
        height = height(),
        sex = sex(),
        adjustToFFM = adjustToFFM(),
        osmolality = osmolality(),
        creatinine = creatinine(),
        # The workbook's times stay in minutes; these add the display unit
        # beside them and say what the plot showed (sendSlide.R).
        timeUnit = timeUnit(),
        maximum = plotMaximum()
      )

      shinycssloaders::showPageSpinner(background = "#FFFFFFEE", caption = "Sending email...")
      emailRetval <-
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
          drugDefaults = drugDefaults(),
          email_username = config$email_username,
          email_password = config$email_password
        ) |> profileCode("sendSlide()")
      shinycssloaders::hidePageSpinner()

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
    simulatedDrugs <- tryCatch(drugs(), shiny.silent.error = function(err) NULL)
    if (length(simulatedDrugs) == 0) {
      return(span("No drugs in the current simulation.", class = "text-muted"))
    }
    items <- lapply(names(simulatedDrugs), function(drug) {
      tagList(
        citationItemHTML(drug, simulatedDrugs[[drug]]$reference, simulatedDrugs[[drug]]$Color),
        # Opens the drug's page in the Help tab (handled in app.js)
        tags$a(href = "#", class = "small ms-1", `data-help-page` = paste0("drugs/", drug),
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
    if (is.null(e$panelvar1)) return()
    outputComments("In xy_str")
    outputComments("e$panelvar1 = ", e$panelvar1)

    yaxis <- gsub("\n"," ", e$panelvar1)
    hoverTime <- function(minutes) formatPlotTime(minutes, timeUnit(), referenceTime())

    plotResults <- plotResultsReactive()
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
    outputComments("Drug identified in xy_str() is", drug)

    # A TCI rate panel: report the pump rate in force at that moment.
    if (grepl(" TCI$", drug))
    {
      drug <- sub(" TCI$", "", drug)
      rates <- drugs()[[drug]]$tci$rates
      if (is.null(rates)) return(NULL)
      j <- max(which(rates$Time <= e$x), 1)
      time <- hoverTime(e$x)
      if (rates$Bolus[j]) {
        b <- drugs()[[drug]]$tci$boluses
        k <- which(b$Time == rates$Time[j])[1]
        return(paste0("Time: ", time, ", ", drug, " TCI loading dose: ", signif(b$Amount[k], 3), " ", b$Units[k]))
      }
      return(paste0("Time: ", time, ", ", drug, " TCI rate: ", signif(rates$Rate[j], 3), " ", rates$Units[j]))
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
    normalization <- if (is.null(input$normalization)) NORMALIZE_NONE else input$normalization
    conc <- hoverConcentration(entry$results, e$x, normalization)
    returnText <- paste0("Time: ", hoverTime(e$x), ", ", x[1], " ", conc$label, ": ",
                         signif(conc$value, 2), " ", x[2])
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
        DrugTimeUnits(x)

        if (x$drug %in% c(PLOT_ID_MEAC, PLOT_ID_INTERACTION, "MAC"))
        {
          return()
        } else if (x$drug == PLOT_ID_EVENTS)
        {
          showEditEventsModal()
        } else {
          showEditDrugModal(x$drug)
        }
      }, name = "input$plot_dblclick observer")
    })

  # Get the time, drug, and units from the image mouse event
  imgDrugTime <- function(e)
  {
    outputComments("in imgDrugTime()")
    allResults <- allResultsReactive()
    plotResults <- plotResultsReactive()
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
    time <- min(max(as.numeric(e$x), 0), plotMaximum())
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
      # A click on a drug's TCI rate panel is a click on that drug.
      drug <- sub(" TCI$", "", drug)
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


  # `time` is in minutes (imgDrugTime()); the dialog shows it, and the dose
  # table stores what is typed, in the dose table's format.
  showAddDrugModal <- function(drug, time) {
    thisDrug     <- which(drug == drugDefaults()$Drug)
    initialUnits <- unlist(drugDefaults()$Units[thisDrug])
    selectedUnit <- drugDefaults()$Default.Units[thisDrug]
    format <- doseTableFormat()

    showModal(
      modalDialog(
        `data-submit-btn` = "addDoseBtn",
        title = "Add a dose",
        selectInput(
          inputId = "addDoseDrug",
          label = "Drug",
          # The visible drugs, honouring the illicit-drug opt-in; the clicked
          # drug is always included (it is already in the table).
          choices = union(drug, drugChoices()),
          selected = drug
        ),
        textInput(
          inputId = "addDoseTime",
          label = timeEntryLabel(format),
          value = minutesToEntryTime(time, format, input$referenceTime)
        ),
        textInput(
          inputId = "addDoseAmount",
          label = "Dose",
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
        footer = NULL,
        easyClose = TRUE,
        size = "s"
      )
    )
  }

  observeEvent(input$addDoseDrug, {
    req(input$addDoseDrug)
    thisDrug     <- which(input$addDoseDrug == drugDefaults()$Drug)
    units        <- unlist(drugDefaults()$Units[thisDrug])
    selectedUnit <- drugDefaults()$Default.Units[thisDrug]
    updateSelectInput(session, "addDoseUnits", choices = units, selected = selectedUnit)
  })

  # validateTime() for a dose time, but "" also for a time that names no time
  # in the dose table's format: a clock time such as 25:00 (lubridate reads
  # 24:30 as 00:30), or any clock time while the procedure start cannot be
  # read.  doseTableClean() would drop such a row without a word, so the dose
  # dialogs refuse it, as the add-event dialog does.
  validateDoseTime <- function(x) {
    out <- validateTime(x)
    if (!nzchar(out)) return(out)
    format <- doseTableFormat()
    minutes <- displayTimeToMinutes(out, referenceFor(format), format[["unit"]])
    if (is.na(minutes)) "" else out
  }

  observeEvent(input$addDoseBtn, {
    profileCode({
      addDoseTime <- validateDoseTime(input$addDoseTime)
      addDoseAmount <- validateDose(input$addDoseAmount)
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
        showNotification("That dose could not be read: enter it as a number, such as 2.5.",
                         type = "error")
        return()
      }
      removeModal()
      thisDrug <- which(drugDefaults()$Drug == input$addDoseDrug)

      dt <- doseTable()
      idx <- which(dt$Drug == "")[1]
      dt$Drug[idx]  <- input$addDoseDrug
      dt$Time[idx]  <- addDoseTime
      dt$Dose[idx]  <- addDoseAmount
      dt$Units[idx] <- input$addDoseUnits
      if (dt$Drug[nrow(dt)] != "" ) {
        dt <- rbind(dt, doseTableNewRow)
      }

      doseTable(dt)
    }, name = "input$addDoseBtn observer")
  })

  showEditDrugModal <- function(drug) {
    showModal(
      modalDialog(
        title = paste("Edit", drug, "doses"),
        # The column must stay headed "Time" (the grid's hooks find it by
        # name), so the unit is said above it
        tags$p(class = "small text-muted mb-1", timeEntryLabel(doseTableFormat())),
        rhandsontable::rHandsontableOutput("editPriorDosesTable"),
        actionButton("editDosesOK", "Apply", class = "btn-primary"),
        actionButton("deleteAllDosesBtn", "Delete All Doses", class = "btn-outline-danger"),
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

  observeEvent(input$deleteAllDosesBtn, {
    drug <- DrugTimeUnits()$drug
    removeModal()
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
    deleteDrugDoses(DrugTimeUnits()$drug)
  })

  output$editPriorDosesTable <- rhandsontable::renderRHandsontable({
    profileCode({
      dt <- doseTable()
      drug <- DrugTimeUnits()$drug
      req(drug)
      editPriorDosesTable <- dt[dt$Drug == drug, ]
      req(nrow(editPriorDosesTable) > 0)
      possibleUnits <- drugDefaults() %>%
        dplyr::filter(Drug == drug) %>%
        dplyr::pull("Units") %>%
        unlist()
      editPriorDosesTable$Delete <- FALSE

      editPriorDosesTableHOT <- rhandsontable::rhandsontable(
        editPriorDosesTable[ , c("Delete","Time","Dose","Units")],
        overflow = 'visible',
        rowHeaders = NULL,
        height = 220,
        stretchH = "all"
      ) %>%
        rhandsontable::hot_col(
          col = "Delete",
          type = "checkbox",
          halign = "htRight"
        ) %>%
        rhandsontable::hot_col(
          col = "Time",
          halign = "htRight"
        ) %>%
        rhandsontable::hot_col(
          col = "Dose",
          # Text, not numeric: a numeric column parses a pasted entry itself,
          # before hookSanitize() (inst/www/hot_funs.js) sees it, and read
          # "1,000" as 1 and "1,5" as 1.5.  The hook reads it as written.
          type = "text",
          halign = "htRight"
        ) %>%
        rhandsontable::hot_col(
          col = "Units",
          type = "dropdown",
          source = possibleUnits,
          strict = TRUE,
          halign = "htLeft",
          valign = "vtMiddle",
          allowInvalid = FALSE
        ) %>%
        rhandsontable::hot_table(contextMenu = FALSE) %>%
        rhandsontable::hot_rows(rowHeights = 10) %>%
        rhandsontable::hot_cols(colWidths = c(50,55,55,90)) %>%
        addHotHooks(filterKeys = TRUE, sanitize = TRUE)

      editPriorDosesTableHOT
    }, name = "output$editPriorDosesTable")
  })

  observeEvent(
    input$editDosesOK,
    {
      profileCode({
        TT <- rhandsontable::hot_to_r(input$editPriorDosesTable)
        # Every dose kept needs a time and a dose that can be read.  A blank
        # counts as unreadable here: the grid clears an entry it cannot read
        # (inst/www/hot_funs.js), and validateTime() and validateDose() below
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
                                  ", and a dose, entered as a number."), type = "error")
          return()
        }
        removeModal()
        outputComments("In ObserveEvent for editDosesOK")
        TT$Drug <- DrugTimeUnits()$drug
        outputComments("TT:")
        outputComments(TT)
        outputComments("doseTable:")
        outputComments(doseTable())
        dt <- doseTable()
        dt <- rbind(
          TT[!TT$Delete,c("Drug","Time","Dose","Units")],
          dt[dt$Drug != DrugTimeUnits()$drug,]
        )

        for (i in 1:nrow(dt))
        {
          if (dt$Drug[i] > "")
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

        outputComments("doseTable after update:")
        outputComments(dt)
        doseTable(dt)
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
          value = minutesToEntryTime(time, format, input$referenceTime)
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
        clickTime <- displayTimeToMinutes(validateTime(input$clickTimeEvent),
                                          referenceFor(format), format[["unit"]])
        if (is.na(clickTime)) {
          showNotification(paste0("That time could not be read: enter it as ",
                                  timeEntryUnitText(format), "."), type = "error")
          return()
        }

        clickEvent <- input$clickEvent
        ET <- eventTable()
        ET <- data.frame(
          Time  = c(ET$Time, clickTime),
          Event = c(ET$Event, clickEvent)
        )
        ET <- ET[order(ET$Time,ET$Event),]
        eventTable(ET)
        removeModal()
      }, name = "input$addEventBtn observer")
    })

  # Edit prior drug doses
  editEventsHOT <- reactiveVal(NULL)

  output$editEventsTableHTML <- rhandsontable::renderRHandsontable({
    req(editEventsHOT())
    editEventsHOT()
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
      tempTable$Time <- minutesToEntryTime(minutes, format, input$referenceTime)
      editEventsShown <<- list(text = tempTable$Time, minutes = minutes)
      tempTable$Delete <- FALSE
      tempTableHOT <- rhandsontable::rhandsontable(
        tempTable[,c("Delete","Time","Event")],
        overflow = 'visible',
        rowHeaders = NULL,
        height = 220,
        stretchH = "all"
      ) %>%
        rhandsontable::hot_col(
          col = "Delete",
          type="checkbox",
          halign = "htRight"
        ) %>%
        rhandsontable::hot_col(
          col = "Time",
          halign = "htRight"
        ) %>%
        rhandsontable::hot_col(
          col = "Event",
          type = "dropdown",
          source = eventDefaults()$Event,
          strict = TRUE,
          halign = "htLeft",
          valign = "vtMiddle",
          allowInvalid = FALSE
        ) %>%
        rhandsontable::hot_table(contextMenu = FALSE) %>%
        rhandsontable::hot_rows(rowHeights = 10) %>%
        rhandsontable::hot_cols(colWidths = c(60,65,100)) %>%
        addHotHooks(filterKeys = TRUE, sanitize = TRUE)

      editEventsHOT(NULL)  # force re-render even if table data is identical
      editEventsHOT(tempTableHOT)
    }

    showModal(
      modalDialog(
        title = "Edit Events",
        if (hasEvents)
          tagList(
            tags$p(class = "small text-muted mb-1", timeEntryLabel(format)),
            rhandsontable::rHandsontableOutput(outputId = "editEventsTableHTML")
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
        ET <- rhandsontable::hot_to_r(input$editEventsTableHTML)
        format <- doseTableFormat()
        shown <- editEventsShown
        minutes <- editedTimesToMinutes(ET$Time, shown$text, shown$minutes,
                                        format, referenceFor(format))
        keep <- !ET$Delete
        if (any(is.na(minutes[keep]))) {
          # The dialog stays open to be corrected
          showNotification(paste0("Every event needs a time, entered as ",
                                  timeEntryUnitText(format), "."), type = "error")
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
    dt <- dt[dt$Drug != drug, ]
    doseTable(dt)
  }

  # Target Drug Dosing (TCI Like) ###########################################
  # Event to trigger calculation to set doses for a target
  targetHOTVal <- reactiveVal(NULL)

  output$targetTableHTML <- rhandsontable::renderRHandsontable({
    req(targetHOTVal())
    targetHOTVal()
  })

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
        targetHOT <- rhandsontable::rhandsontable(
          targetTable,
          overflow = 'visible',
          rowHeaders = NULL,
          height = 220
        ) %>%
          rhandsontable::hot_col(
            col = "Time",
            halign = "htRight"
          ) %>%
          rhandsontable::hot_col(
            col = "Target",
            type = "numeric",
            halign = "htRight"
          ) %>%
          rhandsontable::hot_context_menu(
            allowRowEdit = TRUE,
            allowColEdit = FALSE
          ) %>%
          rhandsontable::hot_rows(
            rowHeights = 10
          ) %>%
          rhandsontable::hot_cols(
            colWidths = c(70,70)
          ) %>%
          addHotHooks(filterKeys = TRUE, sanitize = TRUE)

        targetHOTVal(NULL)
        targetHOTVal(targetHOT)
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
            rhandsontable::rHandsontableOutput(
              outputId = "targetTableHTML"
            ),
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
        # Tested before validateTime(), which turns a blank into "0"
        if (!nzchar(trimws(input$targetEndTime)))
        {
          outputComments("No endtime")
          return()
        }
        targetTable <- rhandsontable::hot_to_r(input$targetTableHTML)
        validateTargetTableInput(targetTable)

        # The times are typed in the dose table's format.  suggest() is given
        # them as minutes, and its suggested doses are written back in that
        # format.
        format <- doseTableFormat()
        reference <- referenceFor(format)
        toMinutes <- function(x) {
          displayTimeToMinutes(validateTime(x), reference, format[["unit"]])
        }
        endTime <- toMinutes(input$targetEndTime)
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
        shinycssloaders::showPageSpinner(background = "#FFFFFFEE", caption = "Calculating doses...")
        if (!any(doseTable()$Drug==input$targetDrug)) {
          outputComments("Updating doseTable for new drug")
          doseTable(rbind(doseTable(),
                          data.frame(Drug=input$targetDrug,Time="0",Dose="0",Units="mg")))
          outputComments(doseTable())
        }

        testTable <- suggest(input$targetDrug,
                             targetTable,
                             endTime,
                             drugs(),
                             drugList,
                             eventTable(),
                             REFERENCE_TIME_NONE)

        if (is.null(testTable)) {
          shinycssloaders::hidePageSpinner()
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
        dt <- dt[dt$Drug != input$targetDrug,]

        dt <- rbind(
          testTable[,c("Drug","Time","Dose","Units")],
          dt
        )
        shinycssloaders::hidePageSpinner()
        doseTable(dt)
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
        shinycssloaders::withSpinner(rhandsontable::rHandsontableOutput("editDrugsHTML", height = 350)),
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

  output$editDrugsHTML <- rhandsontable::renderRHandsontable({
    profileCode({
      editDrugsTrigger$depend()
      x <- drugDefaults()
      x$Units <- drugUnitsSimplify(x$Units)
      # endCe is managed via the Drug Thresholds modal.  Category only groups
      # the startup menu, which has closed by now.
      x <- x[, !names(x) %in% c("endCe", "Category")]
      drugsHOT <- rhandsontable::rhandsontable(
        x,
        overflow = 'visible',
        rowHeaders = NULL,
        height = 350
      ) %>%
        rhandsontable::hot_col(
          col = 1,
          halign = "htRight",
          readOnly = TRUE
        ) %>%
        rhandsontable::hot_col(
          col = 2,
          type = "dropdown",
          source = c("mcg","ng"),
          strict = TRUE,
          halign = "htLeft",
          valign = "vtMiddle",
          allowInvalid=FALSE
        ) %>%
        rhandsontable::hot_col(
          col = 3,
          type = "dropdown",
          source = bolusUnits,
          strict = TRUE,
          halign = "htLeft",
          valign = "vtMiddle",
          allowInvalid=FALSE
        ) %>%
        rhandsontable::hot_col(
          col = 4,
          type = "dropdown",
          source = infusionUnits,
          strict = TRUE,
          halign = "htLeft",
          valign = "vtMiddle",
          allowInvalid=FALSE
        ) %>%
        rhandsontable::hot_col(
          col = 5,
          type = "dropdown",
          source = allUnits,
          strict = TRUE,
          halign = "htLeft",
          valign = "vtMiddle",
          allowInvalid=FALSE
        ) %>%
        rhandsontable::hot_col(col = 6,  halign = "htLeft") %>%
        rhandsontable::hot_col(col = 7,  halign = "htRight") %>%
        rhandsontable::hot_col(col = 8,  halign = "htRight") %>%
        rhandsontable::hot_col(col = 9,  halign = "htRight") %>%
        rhandsontable::hot_col(col = 10, halign = "htRight") %>%
        rhandsontable::hot_col(col = 11, halign = "htRight") %>%
        rhandsontable::hot_table(contextMenu = FALSE)
      drugsHOT
    }, name = "output$editDrugsHTML")
  })

  # Evaluate target concentration
  observeEvent(input$drugEditsOK, {
    profileCode({
      removeModal()
      current      <- drugDefaults()
      newDrugDefaults <- rhandsontable::hot_to_r(input$editDrugsHTML)
      newDrugDefaults$Drug                 <- as.character(newDrugDefaults$Drug)
      newDrugDefaults$Concentration.Units  <- as.character(newDrugDefaults$Concentration.Units)
      newDrugDefaults$Bolus.Units          <- as.character(newDrugDefaults$Bolus.Units)
      newDrugDefaults$Infusion.Units       <- as.character(newDrugDefaults$Infusion.Units)
      newDrugDefaults$Default.Units        <- as.character(newDrugDefaults$Default.Units)
      newDrugDefaults$Units                <- as.character(newDrugDefaults$Units)
      newDrugDefaults$Color                <- as.character(newDrugDefaults$Color)
      newDrugDefaults$Lower                <- as.numeric(newDrugDefaults$Lower)
      newDrugDefaults$Upper                <- as.numeric(newDrugDefaults$Upper)
      newDrugDefaults$Typical              <- as.numeric(newDrugDefaults$Typical)
      newDrugDefaults$MEAC                 <- as.numeric(newDrugDefaults$MEAC)

      # endCe and Category are not in the table; restore from current values
      newDrugDefaults$endCe     <- current$endCe[match(newDrugDefaults$Drug, current$Drug)]
      newDrugDefaults$Category  <- current$Category[match(newDrugDefaults$Drug, current$Drug)]

      newDrugDefaults$Units <- drugUnitsExpand(newDrugDefaults$Units)
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
        if (input$normalization == NORMALIZE_NONE)
          checkboxInput("showThresholdModal", "Show time until threshold", value = input$showThreshold),
        shinycssloaders::withSpinner(rhandsontable::rHandsontableOutput("editThresholdsTable", height = 350)),
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

  output$editThresholdsTable <- rhandsontable::renderRHandsontable({
    drugThresholdsTrigger$depend()
    # Shown at the patient's age for the volatile agents, and with a row for
    # MAC; see thresholdTableForDisplay().  isolate(): opening the dialog is
    # what refreshes it, not a change of age behind it.
    x <- thresholdTableForDisplay(
      drugDefaults(),
      tryCatch(isolate(age()), error = function(e) 40),
      isolate(macThreshold())
    )
    rhandsontable::rhandsontable(x, overflow = 'visible', rowHeaders = NULL, height = 350) %>%
      rhandsontable::hot_col(col = 1, halign = "htLeft", readOnly = TRUE) %>%
      rhandsontable::hot_col(col = 2, halign = "htRight", type = "numeric") %>%
      rhandsontable::hot_table(contextMenu = FALSE)
  })

  observeEvent(input$thresholdEditsOK, {
    tt <- rhandsontable::hot_to_r(input$editThresholdsTable)
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
          "Threshold not changed: enter a number of 0 or more for ",
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
    updateCheckboxInput(session, "showThreshold", value = input$showThresholdModal)
  })

  outputComments("Reached the end of server()")
}
