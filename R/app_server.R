####################################################
# stanpumpR                                        #
# Copyright, 2023, Steven L. Shafer, MD            #
# May be freely distributed, modified, or adapted  #
# in derivative works for non-commercial purposes. #
####################################################

app_server <- function(input, output, session) {
  config <- .sprglobals$config

  observeEvent(input$show_intro_modal, {
    showIntroModal()
  }, once = TRUE)

  session$userData$debug <- reactiveVal(config$debug)
  observeEvent(input$debug_level, ignoreInit = TRUE, {
    session$userData$debug(input$debug_level)
  })
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query[["debug"]])) {
      session$userData$debug(as.numeric(query[["debug"]]))
    }
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

  outputComments(
    "**********************************************************************\n",
    "*                       Initializing                                 *\n",
    "**********************************************************************",
    sep = ""
  )

  main_plot <- reactive({
    outputComments("In main_plot", level = DEBUG_LEVEL_VERBOSE)
    #    tryCatchLog({
    tryCatch({
      if (is.null(doseTableClean()) || is.null(drugs()) || is.null(plotObjectReactive())) {
        #        nothingtoPlot
      } else {
        plotObjectReactive()
      }
    }, error = function(err) {
      NULL
    })
  })

  output$PlotSimulation <- renderPlot({
    outputComments("In output$PlotSimulation", level = DEBUG_LEVEL_VERBOSE)
    req(main_plot(), cancelOutput = TRUE)
    main_plot()
  }, height = function() plotHeight())

  # Make drugs and events local to session
  outputComments("Setting Drug and Event Defaults")
  drugDefaults <- reactiveVal(getDrugDefaultsGlobal())
  eventDefaults <- reactiveVal(getEventDefaults())
  drugList <- getDrugDefaultsGlobal()$Drug

  doseTable <- reactiveVal(doseTableInit)

  # Routine to output doseTableHTML from doseTable
  output$doseTableHTML <- renderRHandsontable({
    req(doseTableDraft())

    profileCode({
      outputComments("Rendering doseTableHTML")

      createHOT(doseTableDraft(), drugDefaults())
    }, name = "createHOT() from doseTableHTML")
  })

  eventTable <- reactiveVal(eventTableInit)

  outputComments("Setup Complete")

  # Get reference time from client
  # The reference time is passed from app.js on event shiny:connected
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

  referenceTime <- reactive({
    if (input$timeMode == "relative") {
      "none"
    } else {
      input$referenceTime
    }
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
      state$values$ET <- eventTable()
      setBookmarkExclude(bookmarksToExclude)
    }, name = "onBookmark()")
  })

  # This gets called after bookmarking is completed
  onBookmarked(function(url) {
    profileCode({
      updateQueryString(url)
      url(url)
    }, name = "onBookmarked()")
  })

  onRestored(function(state) {
    profileCode({
      outputComments(
        "***************************************************************************\n",
        "*                       Restoring Session from URL                        *\n",
        "***************************************************************************",
        sep = ""
      )
      DT <- as.data.frame(state$values$DT)
      doseTable(DT)
      outputComments("doseTable:")
      outputComments(DT)
      ET <- as.data.frame(state$values$ET)
      if (ncol(ET) == 0) {
        ET <- eventTableInit
      }
      eventTable(ET)
      outputComments("eventTable:")
      outputComments(ET)
    }, name = "onRestored()")
  })


  ######################
  ## Dose Table Loop  ##
  ######################

  # This is used to hold the current state of the table as the user edits it
  # without applying it, to allow the user to make many successive edits quickly
  doseTableDraft <- reactiveVal()

  doseTableUndo <- reactiveVal(list())
  doseTableRedo <- reactiveVal(list())

  observeEvent(doseTable(), {
    doseTableDraft(doseTable())
  })

  observe({
    shinyjs::toggleState(
      "dosetable_apply",
      condition = !identicalTable(doseTableDraft(), doseTable())
    )
  })

  observe({
    shinyjs::toggleState("dosetable_undo", condition = length(doseTableUndo()) > 0)
    shinyjs::toggleState("dosetable_redo", condition = length(doseTableRedo()) > 0)
  })

  observeEvent(input$dosetable_apply, {
    shinyjs::disable("dosetable_apply")
    doseTable(doseTableDraft())
    doseTableUndo(list())
    doseTableRedo(list())
  })

  observeEvent(input$dosetable_undo, {
    req(length(doseTableUndo()) > 0)

    doseTableRedo( c(doseTableRedo(), list(doseTableDraft())) )
    doseTableDraft( utils::tail(doseTableUndo(), 1)[[1]] )
    doseTableUndo( head(doseTableUndo(), -1) )
  })

  observeEvent(input$dosetable_redo, {
    req(length(doseTableRedo()) > 0)

    doseTableUndo( c(doseTableUndo(), list(doseTableDraft())) )
    doseTableDraft( utils::tail(doseTableRedo(), 1)[[1]] )
    doseTableRedo( head(doseTableRedo(), -1) )
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
      data <- hot_to_r(data) |> profileCode("hot_to_r() in input$doseTableHTML observer")

      # make sure that table has changed before updating doseTable reactive
      if ( !identicalTable(doseTableDraft(), data) ) {
        doseTableUndo( c(doseTableUndo(), list(doseTableDraft())) )
        doseTableRedo(list())

        # add empty row at the bottom if needed
        if (nzchar(tail(data, 1)$Drug)) {
          data[nrow(data) + 1, ] <- ""
        }

        doseTableDraft(data)
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
    input$sex
  })

  testCovariates <- reactive({
    profileCode({
      outputComments("In testCovariates", level = DEBUG_LEVEL_VERBOSE)
      req(weight(), height(), age(), sex())
      errorFxn <- function(msg) showModal(modalDialog(title = NULL, msg))
      checkNumericCovariates(age(), weight(), height(), errorFxn)
    }, name = "testCovariates() reactive")
  })

  drugs <- reactive({
    profileCode({
      outputComments("In drugs", level = DEBUG_LEVEL_VERBOSE)
      req(testCovariates(), doseTableClean())

      newDrugs <- NULL

      newDrugs <- recalculatePK(
        newDrugs,
        drugDefaults(),
        doseTableClean(),
        age = age(),
        weight = weight(),
        height = height(),
        sex = sex()
      ) |> profileCode("recalculatePK() in drugs()")

      newDrugs <- processdoseTable(
        doseTableClean(),
        eventTableClean(),
        newDrugs,
        plotMaximum(),
        plotRecovery()
      ) |> profileCode("processdoseTable() in drugs()")
      newDrugs
    }, name = "drugs() reactive")
  })

  ###########################
  ## Main Observation Loop ##
  ###########################

  doseTableClean <- reactive({
    profileCode({
      outputComments("In doseTableClean", level = DEBUG_LEVEL_VERBOSE)
      DT <- cleanDT(doseTable())
      DT$Time <- clockTimeToDelta(referenceTime(), DT$Time)
      DT <- DT[
        DT$Drug  != "" &
          DT$Units != "" &
          !is.na(DT$Dose) &
          !is.na(DT$Time), ]
      if (input$maximum == 10) {
        DT <- DT[DT$Time <= 10, ]
      }
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
      ET <- eventTable()
      if (length(ET$Time) > 0) {
        ET$Time <- as.character(ET$Time)
        ET$Time <- clockTimeToDelta(referenceTime(), ET$Time)
        if (input$maximum == 10) {
          ET <- ET[ET$Time <= 10, ]
        }
      }
      ET
    }, name = "eventTableClean() reactive")
  })


  observeEvent(input$timeMode, {
    doseTable(doseTableDraft())
    doseTableUndo(list())
    doseTableRedo(list())

    # When switching to relative time, convert any clock times (HH:MM) to minutes
    if (input$timeMode == "relative") {
      dt <- doseTable()
      to_update <- (dt$Time != "" & !is.na(dt$Time) & grepl(":", dt$Time))
      if (any(to_update)) {
        dt$Time[to_update] <- clockTimeToDelta(input$referenceTime, dt$Time[to_update])
      }
      doseTable(dt)
    }
  }, ignoreInit = TRUE)


  plotInfo <- reactive({
    profileCode({
      req(doseTableClean())

      plotMaximum <- as.numeric(input$maximum)
      steps <- maxtimes$steps[maxtimes$times == input$maximum]
      maxTime <- max(as.numeric(doseTableClean()$Time),
                     as.numeric(eventTableClean()$Time),
                     na.rm = TRUE)

      if (input$maximum != 10 && (maxTime + 29) >= plotMaximum) {
        steps <- maxtimes$steps[maxtimes$times >= (maxTime + 30)][1]
        if (is.na(steps)) steps <- tail(maxtimes$steps, 1)
        plotMaximum <- ceiling((maxTime + 30)/steps) * steps
      }
      list(plotMaximum = plotMaximum, steps = steps)
    }, name = "plotInfo() reactive")
  })

  plotMaximum <- reactive(plotInfo()$plotMaximum)
  steps       <- reactive(plotInfo()$steps)


  plotRecovery <- reactive({
    input$showThreshold
  })

  linetypes <- reactive({
    setLinetypes(input$normalization,input$plasmaLinetype,input$effectsiteLinetype)
  })

  simulationPlotRetval <- reactive({
    req(input$plotWidth)
    profileCode({
      outputComments("In simulationPlotRetval", level = DEBUG_LEVEL_VERBOSE)
      req(doseTableClean(), testCovariates(),
          length(input$plasmaLinetype) > 0, length(input$effectsiteLinetype) > 0)

      DT <- doseTableClean()
      ET <- eventTableClean()

      xBreaks <- 0:(plotMaximum()/steps()) * steps()
      xLabels <- deltaToClockTime(referenceTime(), xBreaks)
      if (referenceTime() == "none") {
        xAxisLabel <- "Time (Minutes)"
      } else {
        xAxisLabel <- "Time"
        updateNumericInput(session, "referenceTime", value = xLabels[1])
      }

      plotMEAC               <- "MEAC"        %in% input$addedPlots
      plotInteraction        <- "Interaction" %in% input$addedPlots
      plotCost               <- "Cost"        %in% input$addedPlots
      plotEvents             <- DRUG_NAME_EVENTS      %in% input$addedPlots
      plasmaLinetype         <- input$plasmaLinetype
      effectsiteLinetype     <- input$effectsiteLinetype
      normalization          <- input$normalization
      title                  <- input$title
      typical                <- input$typical
      logY                   <- input$logY
      if (plotRecovery() || plotEvents || plotInteraction) logY <- FALSE

      plasmaLinetype <- linetypes()$plasmaLinetype
      effectsiteLinetype <- linetypes()$effectsiteLinetype

      printCaption <- input$caption
      if (printCaption == "") {
        printCaption <- paste0(
          "Age: ",
          round(age(), 1),
          " years, weight: ",
          round(weight(), 2),
          " kg, height: ",
          round(height(), 2),
          " cm, sex: ",
          sex()
        )
      }

      # try tryCatchLog if something goes wrong here for a better traceback

      #    tryCatchLog({
      simulationPlot(
        drugs = drugs(),
        events = ET,
        drugDefaults = drugDefaults(),
        eventDefaults = eventDefaults(),
        xBreaks = xBreaks,
        xLabels = xLabels,
        xAxisLabel = xAxisLabel,
        plasmaLinetype = plasmaLinetype,
        effectsiteLinetype = effectsiteLinetype,
        normalization = normalization,
        plotMEAC = plotMEAC,
        plotInteraction = plotInteraction,
        plotCost = plotCost,
        plotEvents = plotEvents,
        plotRecovery = plotRecovery(),
        title = title,
        caption = printCaption,
        typical = typical,
        logY = logY,
        yAxisHeight = input$yaxisHeight,
        width = input$plotWidth
      )
    }, name = "simulationPlotRetval() reactive")
  })
  #  })

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

  observe({
    shinyjs::toggleState("sendSlide", condition = isEmailValid(input$recipient))
  })

  # Send Slide -----------------------------
  observeEvent(
    input$sendSlide,
    {
      outputComments("input$sendSlide",input$sendSlide)

      values <- list(
        title = input$title,
        DT = doseTableClean(),
        url = url(),
        ageUnit = ageUnit(),
        weightUnit = weightUnit(),
        heightUnit = heightUnit(),
        age = age(),
        weight = weight(),
        height = height(),
        sex = sex()
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
        shinyalert::shinyalert("Email sent", type = "success", closeOnClickOutside = TRUE)
      } else {
        shinyalert::shinyalert("Error sending email", emailRetval, type = "error", closeOnClickOutside = TRUE)
      }
    }
  )


  # Hover control ############################################################
  output$hover_info <- renderUI({
    hover <- input$plot_hover
    req(hover, hover$panelvar1)
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

  # Display Time, CE, or total opioid
  xy_str <- function(e) {
    if(is.null(e)) return()
    if(is.null(e$panelvar1)) return()
    outputComments("In xy_str")
    outputComments("e$panelvar1 = ", e$panelvar1)
    yaxis <- gsub("\n"," ", e$panelvar1)

    plotResults <- plotResultsReactive()
    if (yaxis == "% MEAC")
    {
      TO <- plotResults$Drug == "total opioid"
      if (sum(TO) == 0)
      {
        TO <- plotResults$Wrap == "% MEAC"
        outputComments("Elements found in search of plotResults$Wrap", sum(TO))
      }
      j <- which.min(abs(e$x - plotResults$Time[TO]))
      return(
        paste0("Time: ", round(plotResults$Time[TO][j], 1), " minutes, ", plotResults$Drug[TO][j], ": ", signif(plotResults$Y[TO][j], 2), " % MEAC")
      )
    }
    if (yaxis == "p response")
    {
      TO <- plotResults$Drug == "p response"
      j <- which.min(abs(e$x - plotResults$Time[TO]))
      return(
        paste0("Time: ", round(plotResults$Time[TO][j], 1), " minutes, P (response): ", signif(plotResults$Y[TO][j], 2))
      )
    }

    if (yaxis == DRUG_NAME_EVENTS)
    {
      return("Click to enter events, Double click to edit events")
    }

    x <- unlist(strsplit(yaxis," "))
    drug <- x[1]
    outputComments("Drug identified in xy_str() is", drug)
    i <- which(x[1] == drugList)
    if (is.null(drugs()[[drug]])) return()
    j <- which.min(abs(e$x - drugs()[[drug]]$equiSpace$Time))
    x[2] <- substr(x[2],2,10)
    x[2] <- substr(x[2],1,nchar(x[2])-1)
    time <- round(drugs()[[drug]]$equiSpace$Time[j], 1)
    if (referenceTime() == "none")
    {
      time = paste(time, "minutes")
    } else {
      time <- deltaToClockTime(referenceTime(), time)
    }
    returnText <- paste0("Time: ", time, ", ",x[1], " Ce: ", signif(drugs()[[drug]]$equiSpace$Ce[j], 2), " ", x[2])
    if (plotRecovery())
    {
      returnText <- paste0(returnText,", Time until threshold: ",round(drugs()[[drug]]$equiSpace$Recovery[j], 1), " minutes")
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

        if (x$drug == DRUG_NAME_EVENTS) {
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

        if (x$drug == DRUG_NAME_EVENTS)
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

    # Get Time
    if (is.null(e$x) || is.null(e) || length(plottedDrugs) == 0)
    {
      if (e$coords_img$x < 300)
      {
        time <- 0
      } else {
        time <- plotMaximum()
        outputComments("time is plotMaximum:", time)
      }
    } else {
      i <- which(plottedDrugs[1] == drugList)
      drug <- plottedDrugs[1]
      outputComments("Drug in imgDrugTime() is", drug)
      j <- which.min(abs(e$x - drugs()[[drug]]$equiSpace$Time))
      time <- round(drugs()[[drug]]$equiSpace$Time[j], 1)
    }
    if (referenceTime() == "none")
    {
      time <- as.character(time)
    } else {
      time <- deltaToClockTime(referenceTime(), time)
    }

    if(is.null(e) || length(plottedDrugs) == 0)
    {
      echoComments("Returning because length(plottedDrugs = 0")
      return(
        list(
          drug = "propofol",
          time = time,
          units = c("mg", "mcg/kg/min")))
    }

    #  whichDrugs <- which(unlist(lapply(drugs(),function(x) {if (is.null(x$DT)) FALSE else TRUE})))

    # Get Drug
    drug <- unlist(strsplit(gsub("\n", " ", e$panelvar1), " "))[1]
    outputComments("drug from panelvar1", drug)
    if (drug %in% c("% MEAC", "p no response"))
      drug <- utils::tail(plottedDrugs,1)

    # Get Units
    if (drug == DRUG_NAME_EVENTS)
    {
      units <- c("","")
    } else {
      i <- which(drug == drugList)
      units <- c(drugDefaults()$Bolus.Units[i], drugDefaults()$Infusion.Units[i])
    }
    outputComments("Exiting imgDrugTime()")
    return(
      list(
        drug=drug,
        time = time,
        units = units
      )
    )
  }

  #################################### Single Click Response ##################################


  showAddDrugModal <- function(drug, time) {
    thisDrug     <- which(drug == drugDefaults()$Drug)
    initialUnits <- unlist(drugDefaults()$Units[thisDrug])
    selectedUnit <- drugDefaults()$Default.Units[thisDrug]

    showModal(
      modalDialog(
        `data-submit-btn` = "addDoseBtn",
        title = "Add a dose",
        selectInput(
          inputId = "addDoseDrug",
          label = "Drug",
          choices = drugList,
          selected = drug
        ),
        textInput(
          inputId = "addDoseTime",
          label = "Time",
          value = time
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
        fade = TRUE,
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

  observeEvent(input$addDoseBtn, {
    profileCode({
      addDoseTime <- validateTime(input$addDoseTime)
      addDoseAmount <- validateDose(input$addDoseAmount)
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
        rHandsontableOutput("editPriorDosesTable"),
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
        fade = TRUE,
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
          fade = TRUE,
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

  output$editPriorDosesTable <- renderRHandsontable({
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

      editPriorDosesTableHOT <- rhandsontable(
        editPriorDosesTable[ , c("Delete","Time","Dose","Units")],
        overflow = 'visible',
        rowHeaders = NULL,
        height = 220,
        stretchH = "all"
      ) %>%
        hot_col(
          col = "Delete",
          type = "checkbox",
          halign = "htRight"
        ) %>%
        hot_col(
          col = "Time",
          halign = "htRight"
        ) %>%
        hot_col(
          col = "Dose",
          type = "numeric",
          halign = "htRight"
        ) %>%
        hot_col(
          col = "Units",
          type = "dropdown",
          source = possibleUnits,
          strict = TRUE,
          halign = "htLeft",
          valign = "vtMiddle",
          allowInvalid = FALSE
        ) %>%
        hot_table(contextMenu = FALSE) %>%
        hot_rows(rowHeights = 10) %>%
        hot_cols(colWidths = c(50,55,55,90)) %>%
        addHotHooks(filterKeys = TRUE, sanitize = TRUE)

      editPriorDosesTableHOT
    }, name = "output$editPriorDosesTable")
  })

  observeEvent(
    input$editDosesOK,
    {
      profileCode({
        removeModal()
        TT <- hot_to_r(input$editPriorDosesTable)
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

        # Sort by time, by drug, but put blanks at the bottom
        outputComments(toString(unique(dt$Time)))
        dt$Time[dt$Time == ""] <- "zzzzz"
        dt <- dt[order(dt$Time, dt$Drug),]
        dt$Time[dt$Time == "zzzzz"] <- ""

        outputComments("doseTable after update:")
        outputComments(dt)

        for (i in 1:nrow(dt))
        {
          if (dt$Drug[i] > "")
          {
            dt$Time[i] <- validateTime(dt$Time[i])
            dt$Dose[i] <- validateDose(dt$Dose[i]) # should work for target too
          }
        }
        doseTable(dt)
      }, name = "input$editDosesOK observer")
    })

  showAddEventModal <- function(time) {
    showModal(
      modalDialog(
        `data-submit-btn` = "addEventBtn",
        title = paste("Enter a new event"),
        textInput(
          inputId = "clickTimeEvent",
          label = "Time",
          value = time
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
        clickTime <- validateTime(input$clickTimeEvent)
        if (referenceTime() == "none")
        {
          clickTime <- as.numeric(clickTime)
        } else {
          if (nchar(clickTime) == 5)
          {
            clickTime <- clockTimeToDelta(referenceTime(), clickTime)
          } else {
            clickTime <- as.numeric(clickTime)
          }
        }

        clickEvent <- input$clickEvent
        ET <- eventTable()
        ET <- data.frame(
          Time  = c(ET$Time, clickTime),
          Event = c(ET$Event, clickEvent),
          Fill = c(ET$Fill, eventDefaults()$Color[clickEvent == eventDefaults()$Event])
        )
        ET <- ET[order(ET$Time,ET$Event),]
        eventTable(ET)
        removeModal()
      }, name = "input$addEventBtn observer")
    })

  # Edit prior drug doses
  editEventsHOT <- reactiveVal(NULL)

  output$editEventsTableHTML <- renderRHandsontable({
    req(editEventsHOT())
    editEventsHOT()
  })

  showEditEventsModal <- function()
  {
    tempTable <- eventTable()
    hasEvents <- nrow(tempTable) > 0

    if (hasEvents) {
      tempTable <- tempTable[,c("Time", "Event")]
      tempTable$Delete <- FALSE
      tempTableHOT <- rhandsontable(
        tempTable[,c("Delete","Time","Event")],
        overflow = 'visible',
        rowHeaders = NULL,
        height = 220,
        stretchH = "all"
      ) %>%
        hot_col(
          col = "Delete",
          type="checkbox",
          halign = "htRight"
        ) %>%
        hot_col(
          col = "Time",
          halign = "htRight"
        ) %>%
        hot_col(
          col = "Event",
          type = "dropdown",
          source = eventDefaults()$Event,
          strict = TRUE,
          halign = "htLeft",
          valign = "vtMiddle",
          allowInvalid = FALSE
        ) %>%
        hot_table(contextMenu = FALSE) %>%
        hot_rows(rowHeights = 10) %>%
        hot_cols(colWidths = c(60,65,100))

      editEventsHOT(NULL)  # force re-render even if table data is identical
      editEventsHOT(tempTableHOT)
    }

    showModal(
      modalDialog(
        title = "Edit Events",
        if (hasEvents)
          rHandsontableOutput(outputId = "editEventsTableHTML")
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
        fade = TRUE,
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
          fade = TRUE,
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
        removeModal()
        ET <- hot_to_r(input$editEventsTableHTML)
        ET <- ET[!ET$Delete,c("Time","Event")]
        CROWS <- match(ET$Event, eventDefaults()$Event)
        ET$Fill <- eventDefaults()$Color[CROWS]
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

  output$targetTableHTML <- renderRHandsontable({
    req(targetHOTVal())
    targetHOTVal()
  })

  observeEvent(
    input$setTarget,
    {
      profileCode({
        targetTable <-  data.frame(
          Time = rep("",6),
          Target = rep("", 6)
        )
        targetHOT <- rhandsontable(
          targetTable,
          overflow = 'visible',
          rowHeaders = NULL,
          height = 220
        ) %>%
          hot_col(
            col = "Time",
            halign = "htRight"
          ) %>%
          hot_col(
            col = "Target",
            type = "numeric",
            halign = "htRight"
          ) %>%
          hot_context_menu(
            allowRowEdit = TRUE,
            allowColEdit = FALSE
          ) %>%
          hot_rows(
            rowHeights = 10
          ) %>%
          hot_cols(
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
              "Enter time and target concentration below. Decreasing targets are not yet supported, and will be removed. Doses are found with non-linear regression, which takes a moment to calculate. The suggestion will be good, but better algorithms likely exist."
            ),
            selectInput(
              inputId = "targetDrug",
              label = "Drug",
              choices = drugList
            ),
            rHandsontableOutput(
              outputId = "targetTableHTML"
            ),
            textInput(
              inputId = "targetEndTime",
              label = "End Time",
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
            fade=TRUE,
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
        removeModal()
        shinycssloaders::showPageSpinner(background = "#FFFFFFEE", caption = "Calculating doses...")
        endTime <- validateTime(input$targetEndTime)
        if ((endTime) == "")
        {
          outputComments("No endtime")
          return()
        }
        targetTable <- hot_to_r(input$targetTableHTML)

        tryCatchLog({

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
                               referenceTime())

        })

        if (is.null(testTable)) return()

        outputComments("Setting doseTable")
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
        shinycssloaders::withSpinner(rHandsontableOutput("editDrugsHTML", height = 350)),
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
        fade=TRUE,
        size="l"
      )
    )
  })

  output$editDrugsHTML <- renderRHandsontable({
    profileCode({
      editDrugsTrigger$depend()
      x <- drugDefaults()
      x$Units <- drugUnitsSimplify(x$Units)
      # endCe is managed via the Drug Thresholds modal
      x <- x[, !names(x) %in% "endCe"]
      drugsHOT <- rhandsontable(
        x,
        overflow = 'visible',
        rowHeaders = NULL,
        height = 350
      ) %>%
        hot_col(
          col = 1,
          halign = "htRight",
          readOnly = TRUE
        ) %>%
        hot_col(
          col = 2,
          type = "dropdown",
          source = c("mcg","ng"),
          strict = TRUE,
          halign = "htLeft",
          valign = "vtMiddle",
          allowInvalid=FALSE
        ) %>%
        hot_col(
          col = 3,
          type = "dropdown",
          source = bolusUnits,
          strict = TRUE,
          halign = "htLeft",
          valign = "vtMiddle",
          allowInvalid=FALSE
        ) %>%
        hot_col(
          col = 4,
          type = "dropdown",
          source = infusionUnits,
          strict = TRUE,
          halign = "htLeft",
          valign = "vtMiddle",
          allowInvalid=FALSE
        ) %>%
        hot_col(
          col = 5,
          type = "dropdown",
          source = allUnits,
          strict = TRUE,
          halign = "htLeft",
          valign = "vtMiddle",
          allowInvalid=FALSE
        ) %>%
        hot_col(col = 6,  halign = "htLeft") %>%
        hot_col(col = 7,  halign = "htRight") %>%
        hot_col(col = 8,  halign = "htRight") %>%
        hot_col(col = 9,  halign = "htRight") %>%
        hot_col(col = 10, halign = "htRight") %>%
        hot_col(col = 11, halign = "htRight") %>%
        hot_table(contextMenu = FALSE)
      drugsHOT
    }, name = "output$editDrugsHTML")
  })

  # Evaluate target concentration
  observeEvent(input$drugEditsOK, {
    profileCode({
      removeModal()
      current      <- drugDefaults()
      newDrugDefaults <- hot_to_r(input$editDrugsHTML)
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

      # endCe is not in the table; restore from current values
      newDrugDefaults$endCe     <- current$endCe[match(newDrugDefaults$Drug, current$Drug)]

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
        br(),
        shinycssloaders::withSpinner(rHandsontableOutput("editThresholdsTable", height = 350)),
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

  output$editThresholdsTable <- renderRHandsontable({
    drugThresholdsTrigger$depend()
    x <- drugDefaults()[, c("Drug", "endCe")]
    names(x)[2] <- "Threshold"
    rhandsontable(x, overflow = 'visible', rowHeaders = NULL, height = 350) %>%
      hot_col(col = 1, halign = "htLeft", readOnly = TRUE) %>%
      hot_col(col = 2, halign = "htRight", type = "numeric") %>%
      hot_table(contextMenu = FALSE)
  })

  observeEvent(input$thresholdEditsOK, {
    removeModal()
    tt <- hot_to_r(input$editThresholdsTable)
    newDrugDefaults <- drugDefaults()
    newDrugDefaults$endCe <- as.numeric(tt$Threshold)[match(newDrugDefaults$Drug, tt$Drug)]
    drugDefaults(newDrugDefaults)
  })

  outputComments("Reached the end of server()")
}
