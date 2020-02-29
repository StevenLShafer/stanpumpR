####################################################
# stanpumpR                                        #
# Copyright, 2019, Steven L. Shafer, MD            #
# May be freely distributed, modified, or adapted  #
# in derivative works for non-commercial purposes. #
####################################################

# server ##########################################
function(input, output, session)
{

  showModal(
    modalDialog(
      title = "Welcome to stanpumpR",
      p(
        "stanpumpR, derived from the original STANPUMP program developed at
        Stanford University,  performs pharmacokinetic simulations
        based on mathematical models published in the peer-reviewed
        literature. stanpumpR is intended to help clinicians and investigators
        better understand the mathematical implications of published models.
        stanpumpR is only an advisory program. How these models are applied to
        individual patients is a matter of clinical judgment by the health care
        provider."
      ),
      tags$button(
        type = "button",
        class = "btn btn-warning",
        `data-dismiss` = "modal",
        "OK"
      ),
      footer = NULL,
      easyClose = TRUE,
      fade=TRUE,
      size="s"
    )
  )

  options(error = function ()
  {
    x <- geterrmessage()
    if (!isShinyLocal)
    {
      cat("Error detected in stanpumpR\n")
      cat(x,"\n")
      cat("URL: ", prior$url,"\n")
      sendError(url = prior$url, errorMessage = x)
    }
    options(error = NULL)
  })

  # Write out logs to the log section
  initLogMsg <- "Comments Log"
  commentsLog <- reactiveVal(initLogMsg)
  output$logContent <- renderUI({
    shinyjs::delay(0, shinyjs::js$scrollLogger())
    HTML(commentsLog())
  })
  session$userData$commentsLog <- commentsLog

  #############################################################################
  #                           Initialization                                  #
  #############################################################################

  outputComments("**********************************************************************")
  outputComments("*                       Initializing                                 *")
  outputComments("**********************************************************************")

  main_plot <- reactiveVal(introductionPlot)

  output$PlotSimulation <- renderPlot({
    main_plot()
  })

  # Make drugs and events local to session
  cat("Setting drugDefaults and eventDefaults\n")
  drugDefaults <- drugDefaults_global
  drugList <- drugDefaults$Drug
  colorList <- drugDefaults$Color

  outputComments("Initializing prior and current")

  prior <- reactiveValues()

  isolate(
    {
      # Prior data
      prior$age                <- 0
      prior$weight             <- 0
      prior$height             <- 0
      prior$sex                <- 0
      prior$ageUnit            <- 0
      prior$heightUnit         <- ""
      prior$weightUnit         <- ""
      prior$plotMaximum        <- 60
      prior$plasmaLinetype     <- "blank"
      prior$effectsiteLinetype <- "solid"
      prior$normalization      <- "none"
      prior$title              <- 0
      prior$caption            <- ""
      prior$typical            <- "Range"
      prior$logY               <- FALSE
      prior$DT                 <- NULL # Used to determine of replot flag needs to be set, e.g., sameTable(DT, prior$DT)
      # Set to DT after processDoseTable. Note that the NULL assignment is assumed without
      # being explicit, as done here. This is mostly to help keep inventory of the prior
      # variables
      prior$ET                 <- NULL # Used to determine if plot needs to be set.
      prior$referenceTime      <- "none"
      prior$plotMEAC           <- FALSE
      prior$plotInteraction    <- FALSE
      prior$plotCost           <- FALSE
      prior$plotEvents         <- FALSE
      prior$plotRecovery       <- FALSE
      prior$holdPlot           <- FALSE
      prior$DrugTimeUnits      <- ""
      prior$timeDT             <- 0
      prior$Refresh            <- 0 # Used to see if the refresh button has been clicked
      prior$RefreshFlag        <- FALSE # Used to force simulationPlot to update with refresh button
      prior$referenceTime      <- "none"
    }
  )


  # This always runs, because on restore it fully restarts server()
  current <- reactiveValues(
    DT = doseTableInit
  )

  # Examples below are for debugging specific PK advance routines (e.g., advanceClosedForm0())
  # current$DT <- data.frame(
  #   Drug = c("dexmedetomidine",""),
  #   Time = c("0",""),
  #   Dose = c("1",""),
  #   Units = c("mcg",""),
  #   stringsAsFactors = FALSE
  # )

  # current$DT <- data.frame(
  #   Drug = c("hydromorphone"),
  #   Time = as.character(0:6*240),
  #   Dose = c("1"),
  #   Units = c("mg PO"),
  #   stringsAsFactors = FALSE
  # )

  # current$DT <- data.frame(
  #   Drug = drugDefaults$Drug,
  #   Time = "0",
  #   Dose = "1",
  #   Units = drugDefaults$Bolus.Units,
  #   stringsAsFactors = FALSE
  # )
  doseTable <- reactiveVal(doseTableInit)
  isolate(
    {
      prior$DT <- doseTable()
      outputComments("prior$DT at the end of initialization")
      outputComments(prior$DT)
    }
  )

  # Routine to output doseTableHTML from doseTable
  isolate({
    output$doseTableHTML <- renderRHandsontable({
      outputComments("Setting up doseTableHTML.")
      createHOT(doseTable(), drugDefaults)
    })
  })

  # End Initialize current$DT

  # Initialize prior$ET
  isolate(
    {
      outputComments("Initializing prior$ET")
      if (is.null(prior$ET))
      {
        # This creates a NULL Table
        prior$ET <- data.frame(
          Time = 0,
          Event = "",
          Fill = "",
          stringsAsFactors = FALSE
        )[FALSE,]
      }
      eventTable <- reactiveVal(
        prior$ET
      )
      cat ("Done with prior$ET\n")
    }
  )


  # Finishing Setup
  outputComments("Completing setup")
  plotObjectReactive  <- reactiveVal(NULL)
  allResultsReactive  <- reactiveVal(NULL)
  plotResultsReactive <- reactiveVal(NULL)
  PK_set <- reactiveVal(FALSE)

  # Initialize drug table (PK for each drug)
  outputComments("Initializing Drugs")
  drugs <- reactiveValues()
  isolate({
    for (i in 1:nrow(drugDefaults)) {
      drug <- drugDefaults$Drug[i]
      drugs[[drug]] <- NULL
      drugs[[drug]]$Color <- drugDefaults$Color[i]
      drugs[[drug]]$endCe <- drugDefaults$endCe[i]
      drugs[[drug]]$endCeText <- drugDefaults$endCeText[i]
    }
    cat("Unique names",names(drugs),"\n")
  })

  newDrugDefaultsFlag <- reactiveVal(FALSE)
  updatedDoseTableFlag <- reactiveVal(FALSE) # Forces a new plot

  outputComments("Setup Complete")


  # Get reference time from client
  output$getReferenceTime <- renderUI({
    outputComments("Getting reference time from client.")
    time <- input$client_time
    outputComments(paste("Reference time from client:", time), echo = TRUE)

    time <- gsub("[^[:digit:]:. APM]","",time) # Get rid of strange formatting characters
    x <- unlist(strsplit(time, " "))
    if (length(x) == 1) x <- c(x, "AM") # European time doesn't use PM
    y <- unlist(strsplit(x[1],":"))
    time <- (as.numeric(y[1]) + 12 * (x[2] == "PM")) * 60 + as.numeric(y[2]) - 60
    if (time < 0) time <- time + 1440
    time <- floor(time / 15) * 15
    HH   <- floor(time / 60)
    MM   <- time %% 60
    start <- sprintf("%02d:%02d",HH,MM)
    prior$referenceTime <- start
    column(
      width = 4,
      selectInput(
        inputId = "referenceTime",
        label = NULL, #"Reference Time",
        selected = start,
        choices = c(
          "none",
          "06:00","06:15","06:30","06:45",
          "07:00","07:15","07:30","07:45",
          "08:00","08:15","08:30","08:45",
          "09:00","09:15","09:30","09:45",
          "10:00","10:15","10:30","10:45",
          "11:00","11:15","11:30","11:45",
          "12:00","12:15","12:30","12:45",
          "13:00","13:15","13:30","13:45",
          "14:00","14:15","14:30","14:45",
          "15:00","15:15","15:30","15:45",
          "16:00","16:15","16:30","16:45",
          "17:00","17:15","17:30","17:45",
          "18:00","18:15","18:30","18:45",
          "19:00","19:15","19:30","19:45",
          "20:00","20:15","20:30","20:45",
          "21:00","21:15","21:30","21:45",
          "22:00","22:15","22:30","22:45",
          "23:00","23:15","23:30","23:45",
          "00:00","00:15","00:30","00:45",
          "01:00","01:15","01:30","01:45",
          "02:00","02:15","02:30","02:45",
          "03:00","03:15","03:30","03:45",
          "04:00","04:15","04:30","04:45",
          "05:00","05:15","05:30","05:45"
        )
      ),
      bsTooltip(
        id = "referenceTime",
        title = 'Time is selected based on your local time. Select "none" for absolute time.',
        placement = "right",
        options = list(container = "body")
      )
    )
  }
  )


  ##########################################################
  # Code to save state in url and then restore from url
  bookmarksToExclude <- c(
    "doseTableHTML",
    "doseTableHTML_select",
    "setTarget",
    "targetDrug",
    "targetDrug-selectized",
    "targetEndTime",
    "targetOK",
    "plot_click",
    "plot_dblclick",
    "plot_hover",
    "sidebarCollapsed",
    "sidebarItemExpanded",
    "simType",
    "effectsiteLinetype-selectized",
    "maximum-selectized",
    "plasmaLinetype-selectized",
    "referenceTime-selectized",
    "targetTableHTML",
    "targetTableHTML_select",
    "tempTableHTML_select",
    "tempTableHTML",
    "clickDose",
    "clickEvent",
    "clickEvent-selectized",
    "clickOKDrug",
    "clickOKEvent",
    "clickTimeDrug",
    "clickTimeEvent",
    "clickUnits",
    "dblclickDrug",
    "dblclickDrug-selectized",
    "dblclickTime",
    "dblclickDose",
    "dblclickUnits",
    "dblclickOK",
    "dblclickDelete",
    "editDoses",
    "editDosesOK",
    "editEvents",
    "editEventsOK",
    "sendSlide",
    "recipient",
    "drugEditsOK",
    "editDrugsHTML",
    "editDrugsHTML_select",
    "editDrugs",
    "newEndCe",
    "hoverInfo"
  )

  observe({
    # Trigger this observer every time an input changes
    reactiveValuesToList(input)
    session$doBookmark()
  })

  onBookmark(function(state) {  #
    state$values$DT <- current$DT
    state$values$ET <- prior$ET
    excludeDelay <- names(input)[grep("delay", names(input))]
    setBookmarkExclude(c(bookmarksToExclude, excludeDelay))
  })

  onBookmarked(
    function(url) # Without this, the session$doBookmark actually displays it
    {
      updateQueryString(url)
      prior$url <- url
    }
  )

  onRestored(function(state) {
    outputComments("***************************************************************************")
    outputComments("*                       Restoring Session from URL                        *")
    outputComments("***************************************************************************")
    isolate({
      prior$DT   <- as.data.frame(state$values$DT, stringsAsFactors=FALSE)
      outputComments("Prior$DT:")
      outputComments(prior$DT)
      doseTable(prior$DT)
      current$DT <- prior$DT
      prior$ET <- as.data.frame(state$values$ET, stringsAsFactors=FALSE)
      if (ncol(prior$ET) == 0)
      {
        prior$ET <- data.frame(
          Time = 0,
          Event = "",
          Fill = "",
          stringsAsFactors = FALSE
        )[FALSE,]
      }
      outputComments("prior$ET after restoring state")
      outputComments(prior$ET)
      eventTable(prior$ET)
    })
  })


  ######################
  ## Dose Table Loop  ##
  ######################

  observeEvent(input$doseTableHTML, {
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
      return(NULL)
    }

    # Because of a bug in hot_to_r(), we can't use it directly. We need to manually change
    # the row names for it to work
    nrows <- length(data$data)
    data$params$rRowHeaders <- as.character(seq.int(nrows))
    data <- hot_to_r(data)

    # make sure that table has changed before updating doseTable reactive
    if( !identical(doseTable(), data) ) {
      # Convert NA values to empty (when a new row gets added using the javascript API,
      # the new row gets NA values and having NA as well as "" values leads to issues later on)
      data[is.na(data)] <- ""
      current$DT <- data
      doseTable(data)
    }
  })



  ###########################
  ## Main Observation Loop ##
  ###########################

  observe({
    DEBUG <- TRUE
    outputComments("\n*********************************************************")
    outputComments("Starting Main Observation Loop")
    #    outputComments(paste0("input$sex: ", input$sex,"."), echo = FALSE)
    #    outputComments(paste0("input$age: ", input$age,"."), echo = FALSE)
    #    outputComments(paste0("input$ageUnit: ", input$ageUnit,"."), echo = FALSE)
    #    outputComments(paste0("input$weight: ", input$weight,"."), echo = FALSE)
    #    outputComments(paste0("input$weightUnit: ", input$weightUnit,"."), echo = FALSE)
    #    outputComments(paste0("input$height: ", input$height,"."), echo = FALSE)
    #    outputComments(paste0("input$heightUnit: ", input$heightUnit,"."), echo = FALSE)
    #    outputComments(paste0("input$maximum: ", input$maximum,"."), echo = FALSE)
    #    outputComments(paste0("Is doseTableHTML NULL? ", is.null(input$doseTableHTML), "."), echo = FALSE)
    #    outputComments(paste0("input$referenceTime: ", input$referenceTime,"."), echo = FALSE)
    #    outputComments(paste0("Is input$referenceTime NULL? ", is.null(input$referenceTime), "."), echo = FALSE)

    req(
      input$sex,
      input$age,
      input$ageUnit,
      input$weight,
      input$weightUnit,
      input$height,
      input$heightUnit,
      input$maximum
    )
    outputComments("Main loop requirements are met.", echo = DEBUG)
    if (
      is.numeric(input$age)             &&
      is.numeric(input$weight)          &&
      is.numeric(input$height)          &&
      input$age > 0                     &&
      input$age <= 110                  &&
      input$weight > 1                  &&
      input$weight < 500                &&
      input$height > 10                 &&
      input$height < 200
      )

      {
      recalculatePKFlag <- FALSE
      replotFlag <- FALSE

      #######################################################################
      # Do we need to recalculate the pharmacokinetics of all of the drugs? #
      weightUnit <- as.numeric(input$weightUnit)
      heightUnit <- as.numeric(input$heightUnit)
      ageUnit    <- as.numeric(input$ageUnit)
      weight     <- input$weight * weightUnit
      height     <- input$height * heightUnit
      age        <- input$age    * ageUnit
      sex        <- input$sex
      if (
        weight      != prior$weight     ||
        height      != prior$height     ||
        age         != prior$age        ||
        sex         != prior$sex        ||
        weightUnit  != prior$weightUnit ||
        heightUnit  != prior$heightUnit ||
        ageUnit     != prior$ageUnit    ||
        newDrugDefaultsFlag()           ||
        updatedDoseTableFlag()
        )
      {
        outputComments("Something changed requiring a full dose calculation.", echo = DEBUG)
        PK_set(TRUE)
        recalculatePKFlag <- TRUE
        for (i in 1:nrow(drugDefaults))
          {
          drug <- drugDefaults$Drug[i]
          outputComments(paste("Getting PK for", drug), echo = DEBUG)
            drugs[[drug]] <- modifyList(
              drugs[[drug]],
              getDrugPK(
                drug = drug,
                weight = input$weight * as.numeric(input$weightUnit),
                height = input$height * as.numeric(input$heightUnit),
                age = input$age    * as.numeric(input$ageUnit),
                sex = input$sex,
                drugDefaults = drugDefaults[i,]
                )
            )
          drugs[[drug]]$DT <- NULL # Remove old dose table, if any
          drugs[[drug]]$CpCe <- NULL # Remove old simulations results, if any
          drugs[[drug]]$equiSpace <- NULL # Ditto
          drugs[[drug]]$maxCp <- 1
          drugs[[drug]]$maxCe <- 1
          drugs[[drug]]$recovery <- 1
        }
        prior$weight      <- weight
        prior$height      <- height
        prior$age         <- age
        prior$sex         <- sex
        prior$weightUnit  <- weightUnit
        prior$heightUnit  <- heightUnit
        prior$ageUnit     <- ageUnit
      }

    #######################################################################
    # Has the dose table changed?                                         #
    #######################################################################
    DT <- doseTable()   # Here is where the reactivity is forced
    outputComments("doseTable() in simulation plot", echo = DEBUG)
    outputComments(DT, DEBUG)
    DT$Drug    <- as.character(DT$Drug)
    DT$Units   <- as.character(DT$Units)
    DT$Dose    <- as.numeric(DT$Dose)
    DT$Time    <- as.character(DT$Time)  # Stored as factors... Arrgh.....
    DT <- DT[DT$Drug != "" & !is.na(DT$Dose) & DT$Time != "" & DT$Units != "",]
    outputComments("Contents of DT in main observe()", echo = DEBUG)
    outputComments(DT, echo = DEBUG)
    outputComments("Contents of prior$DT in main Observe()", echo = DEBUG)
    outputComments(prior$DT, echo = DEBUG)

    # Remove blank values of DT
    if (is.null(input$referenceTime))
    {
      referenceTime <- "none"
    } else {
      referenceTime <- input$referenceTime
    }
    if (referenceTime == "none")
    {
      DT$Time <- as.numeric(DT$Time)
    } else {
      DT$Time    <- clockTimeToDelta(referenceTime, DT$Time)
    }
    DT <- DT[
      DT$Drug  != "" &
        DT$Units != "" &
        !is.na(DT$Dose) &
        !is.na(DT$Time), ]
    if (input$maximum == 10)
    {
      DT <- DT[DT$Time <= 10,]
    }
    if (nrow(DT) == 0)
      {
      outputComments("Dose table is empty in main loop.", echo = DEBUG)
      output$optionFlag <- renderText("")
      main_plot(nothingtoPlot)  # reactiveVal
      return()
    }
    DT <- DT[order(DT$Drug, DT$Time), ]

    #######################################################################
    # Has the event table changed?
    ET <- eventTable()
    ET$Time    <- as.character(ET$Time)
    if (referenceTime == "none")
    {
      ET$Time <- as.numeric(ET$Time)
    } else {
      ET$Time    <- clockTimeToDelta(referenceTime, ET$Time)
    }
    if (input$maximum == 10)
    {
      ET <- ET[ET$Time <= 10,]
    }

    # Adjust maximum to include dose times
    plotMaximum <- as.numeric(input$maximum)
    steps <- maxtimes$steps[maxtimes$times == input$maximum]
    maxTime <- max(as.numeric(DT$Time), as.numeric(ET$Time), na.rm=TRUE)

    if (input$maximum != 10 && (maxTime + 29) >= plotMaximum)
    {
      steps <- maxtimes$steps[maxtimes$times >= (maxTime + 30)][1]
      plotMaximum <- ceiling((maxTime + 30)/steps) * steps
    }

    xBreaks <- 0:(plotMaximum/steps) * steps
    if (referenceTime == "none")
    {
      xLabels <- xBreaks
      xAxisLabel <- "Time (Minutes)"
    } else
    {
      xLabels <- deltaToClockTime(referenceTime, xBreaks)
      xAxisLabel <- "Time"
    }
    if (prior$RefreshFlag)
    {
      outputComments("Plotting because RefreshFlag requested", echo = DEBUG)
      prior$RefreshFlag <- FALSE
      replotFlag <- TRUE
    }
    if (recalculatePKFlag)
    {
      outputComments("Plotting because recalculatePKFlag is TRUE\n", echo = DEBUG)
      replotFlag <- TRUE
    }
    if (!isTRUE(all_equal(DT, prior$DT))) {
      outputComments("Plotting because doseTable changed.", echo = DEBUG)
      replotFlag <- TRUE
    } else {
      outputComments("doseTable remains unchanged in main observe()", echo = DEBUG)
    }
    if (!isTRUE(all_equal(ET, prior$ET))) {
      outputComments("Plotting because eventTable changed.", echo = DEBUG)
      replotFlag <- TRUE
    }

    if (plotMaximum != prior$plotMaximum) {
      outputComments(paste("Plotting because plotMaximum changed to ", plotMaximum), echo = DEBUG)
      replotFlag <- TRUE
    }
    plotRecovery           <- "Time Until"    %in% input$addedPlots
    if (plotRecovery != prior$plotRecovery)
    {
      if (plotRecovery == TRUE)
      {
        outputComments("Plotting because plotRecovery changed to TRUE", echo = DEBUG)
        replotFlag <- TRUE
      }
    }

    if (replotFlag)
    {
      outputComments("calling processdoseTable", echo = DEBUG)
      x <- processdoseTable(
        DT,
        ET,
        drugs,
        plotMaximum,
        prior,
        plotRecovery
      )
#      drugs     <- x$drugs
      outputComments("Completed processdoseTable", echo = DEBUG)

      # Setting prior$DT
      outputComments("Setting prior$DT in main observe()", echo = DEBUG)
      prior$DT  <- DT
      prior$ET  <- ET
      outputComments(prior$DT, echo = DEBUG)
      prior$plotMaximum <- plotMaximum
    }
    if (length(input$plasmaLinetype) == 0 ||
        length(input$effectsiteLinetype) == 0
    )
    {
      outputComments("Waiting for line types to exist before making figure.", echo = DEBUG)
      return()
    }
    if (!PK_set())
    {
      outputComments("Can't plot - PK aren't yet set", echo = DEBUG)
      return()
    }

    # Place reactive elements here, rather than in the plotting routine, so that the only way to the
    # plotting routine is through this checkpoint
    plotMEAC               <- "MEAC"        %in% input$addedPlots
    plotInteraction        <- "Interaction" %in% input$addedPlots
    plotCost               <- "Cost"        %in% input$addedPlots
    plotEvents             <- "Events"      %in% input$addedPlots
    plasmaLinetype         <- input$plasmaLinetype
    effectsiteLinetype     <- input$effectsiteLinetype
    normalization          <- input$normalization
    title                  <- input$title
    caption                <- input$caption
    typical                <- input$typical
    logY                   <- input$logY
    if (plotRecovery) logY <- FALSE

    if(replotFlag                                          ||
        plasmaLinetype         != prior$plasmaLinetype     ||
        effectsiteLinetype     != prior$effectsiteLinetype ||
        normalization          != prior$normalization      ||
        title                  != prior$title              ||
        caption                != prior$caption            ||
        typical                != prior$typical            ||
        logY                   != prior$logY               ||
        referenceTime          != prior$referenceTime      ||
        plotMEAC               != prior$plotMEAC           ||
        plotInteraction        != prior$plotInteraction    ||
        plotCost               != prior$plotCost           ||
        plotEvents             != prior$plotEvents         ||
        plotRecovery           != prior$plotRecovery
    )
    {
      outputComments('****************** SIMULATION PLOT CALLED BY ********************', echo = DEBUG)
      if (replotFlag)          cat("New plot triggered by replotFlag\n")
      if (plasmaLinetype         != prior$plasmaLinetype) outputComments("New plot triggered by plasmaLinetype != prior$plasmaLinetype")
      if (effectsiteLinetype     != prior$effectsiteLinetype) outputComments("New plot triggered by effectsiteLinetype != prior$effectsiteLinetype")
      if (normalization          != prior$normalization) outputComments("New plot triggered by normalization != prior$normalization")
      if (title                  != prior$title) outputComments("New plot triggered by title != prior$title")
      if (caption                != prior$caption) outputComments("New plot triggered by caption != prior$caption")
      if (typical                != prior$typical) outputComments("New plot triggered by typical != prior$typical")
      if (logY                   != prior$logY) outputComments("New plot triggered by logY != prior$logY")
      if (referenceTime          != prior$referenceTime) outputComments("New plot triggered by referenceTime != prior$referenceTime")
      if (plotMEAC               != prior$plotMEAC) outputComments("New plot triggered by plotMEAC != prior$plotMEAC")
      if (plotInteraction        != prior$plotInteraction) outputComments("New plot triggered by plotInteraction != prior$plotInteraction")
      if (plotCost               != prior$plotCost) outputComments("New plot triggered by plotCost != prior$plotCost")
      if (plotEvents             != prior$plotEvents) outputComments("New plot triggered by plotEvents != prior$plotEvents")
      if (plotRecovery           != prior$plotRecovery) outputComments("New plot triggered by ploRecovery != prior$plotRecovery")
      if (newDrugDefaultsFlag() )  outputComments("New plot triggered new Drug Defaults")
      if (updatedDoseTableFlag() ) outputComments("New plot triggered new Dose Table")

      outputComments(" ")

      if (normalization != prior$normalization)
       {
        X <- setLinetypes(normalization)
        plasmaLinetype <- X$plasmaLinetype
        effectsiteLinetype <- X$effectsiteLinetype
      }
      if (caption != "")
      {
        printCaption <- caption
      } else {
        printCaption <- paste0(
          "Age: ",
          round(input$age * as.numeric(input$ageUnit), 1),
          " years, weight: ",
          round(input$weight * as.numeric(input$weightUnit), 2),
          " kg, height: ",
          round(input$height * as.numeric(input$heightUnit), 2),
          " cm, sex: ",
          input$sex
          )
      }
      simulationFlag <- FALSE
      X <- simulationPlot(
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
        plotRecovery = plotRecovery,
        title = title,
        caption = printCaption,
        aspect = ASPECT,
        typical = typical,
        logY = logY,
        drugs = reactiveValuesToList(drugs),
        events = ET,
        eventDefaults = eventDefaults,
        drugDefaults = drugDefaults
      )
      if (is.null(X))
      {
        plotObjectReactive(NULL)
        allResultsReactive(NULL)
        plotResultsReactive(NULL)
      } else {
        plotObjectReactive(X$plotObject)
        allResultsReactive(X$allResults)
        plotResultsReactive(X$plotResults)
      }
      prior$plasmaLinetype      <- plasmaLinetype
      prior$effectsiteLinetype  <- effectsiteLinetype
      prior$normalization       <- normalization
      prior$title               <- title
      prior$typical             <- typical
      prior$logY                <- logY
      prior$plotMaximum         <- plotMaximum
      prior$referenceTime       <- referenceTime
      prior$plotMEAC            <- plotMEAC
      prior$plotInteraction     <- plotInteraction
      prior$plotCost            <- plotCost
      prior$plotEvents          <- plotEvents
      prior$plotRecovery        <- plotRecovery
      prior$caption             <- caption

      newDrugDefaultsFlag(FALSE)
      updatedDoseTableFlag(FALSE)

      if (is.null(plotObjectReactive()))
      {
        output$optionFlag <- renderText("")
        outputComments("Null plot after calling simulation Plot()", echo = DEBUG)
        main_plot(nothingtoPlot) # reactiveVal
      } else {
        output$optionFlag <- renderText("Graph Options")
        main_plot(plotObjectReactive()) # reactiveVal
      }
    } else {
      outputComments("Nothing flagged to make new plot", echo = DEBUG)
      }
    } else {
      outputComments("Test for numeric covariates failed", echo = DEBUG)
      outputComments(paste("input$sex =",input$sex), echo = DEBUG)
      outputComments(paste("input$ageUnit =",input$ageUnit, is.numeric(input$ageUnit),typeof(input$ageUnit)), echo = DEBUG)
      outputComments(paste("input$weightUnit =",input$weightUnit,is.numeric(input$weightUnit)), echo = DEBUG)
      outputComments(paste("input$heightUnit =",input$heightUnit,is.numeric(input$heightUnit)), echo = DEBUG)
      return(NULL)
    }
    outputComments("Exiting main observation loop.", echo = DEBUG)
  },
  priority = -1
  )

  # email address --------------------------------------
  output$EmailButton <- renderUI({
    if (input$recipient == "") return(p(""))
    regex_email <- "^\\w+([-+.']\\w+)*@\\w+([-.]\\w+)*\\.\\w{2,}([-.]\\w+)*$"
    if (nchar(input$recipient) == attr(regexpr(regex_email, input$recipient, perl=FALSE),"match.length"))
    {
      cat("Address is OK\n")
      return(
        div(
          style = "padding-top: 25px;",
          actionButton(
            inputId = "sendSlide",
            label = "GO!",
            icon=icon("far fa-envelope")
          ),
          bsTooltip(
            id = "sendSlide",
            title = "Click ONCE to send slide",
            placement = "top",
            options = list(container = "body")
          )
        )
      )
    }  else {
      cat("Address is Not OK\n")
      return(
        div(
          style = "padding-top: 25px;",
          "Check address"
          )
        )
    }
  })

processNormalization <- observeEvent(
  input$normalization,
  priority=10,
  {
    #cat("Inside observeEvent for Linetypes\n")
    X <- setLinetypes(input$normalization)
    prior$plasmaLinetype <- X$plasmaLinetype
    prior$effectsiteLinetype <- X$effectsiteLinetype
    output$Linetype <- renderUI({
    div(
      selectInput(
        inputId = "plasmaLinetype",
        label = "Plasma",
        selected = prior$plasmaLinetype,
        choices = c("solid",
                    "dashed",
                    "longdash",
                    "dotted",
                    "dotdash",
                    "twodash",
                    "blank")
      ),
      bsTooltip(
        id = "plasmaLinetype",
        title = "Line type for plasma concentrations",
        placement = "top",
        options = list(container = "body")
      ),
      selectInput(
        inputId = "effectsiteLinetype",
        label = "Effect site",
        selected = prior$effectsiteLinetype,
        choices = c("solid",
                    "dashed",
                    "longdash",
                    "dotted",
                    "dotdash",
                    "twodash",
                    "blank")
      ),
      bsTooltip(
        id = "effectsiteLinetype",
        title = "Line type for effect site concentrations",
        placement = "bottom",
        options = list(container = "body")
      )
    )
    })
  }
)

  # Send Slide -----------------------------
  observeEvent(
    input$sendSlide,
    {
      DEBUG <- TRUE
      # output$EmailButton <- renderUI({
      #       p("Processing slide")
      # })
      outputComments(paste("input$sendSlide",input$sendSlide), echo = DEBUG)
      img <- sendSlide(
        prior = prior,
        recipient = input$recipient,
        plotObject = plotObjectReactive(),
        allResults = allResultsReactive(),
        plotResults = plotResultsReactive(),
        isShinyLocal = isShinyLocal,
        slide = as.numeric(input$sendSlide),
        drugs,
        email_username = config$email_username,
        email_password = config$email_password
      )
      output$sentPlot <- renderImage(
        list(src = img)
        )
      # Plot(
      #   plotObject + labs(title=paste("Image sent to",input$recipient))
      # )
    }
  )


# Hover control ############################################################
observeEvent(
  input$plot_hover,
  {
    hover <- input$plot_hover
    if (is.null(hover$panelvar1))
    {
      output$hover_info <- NULL
      return()
    }
    text <- xy_str(hover)
    output$hover_info <- renderUI({
      style <- paste0("position:absolute; padding:0; margin:0; z-index:100; font-size: 10px; background-color: rgba(245, 245, 245, 0.85); ",
                      "left:", hover$coords_css$x+25, "px; top:", hover$coords_css$y+10, "px;")

      # actual tooltip created as wellPanel
      wellPanel(
        style = style,
        HTML(
          gsub(
            ",",
            "<br>",
            text
          )
        )
      )
    })
  }
)

# Display Time, CE, or total opioid
xy_str <- function(e) {
  DEBUG <- FALSE
  if(is.null(e)) return(NULL)
  if(is.null(e$panelvar1)) return(NULL)
  outputComments("In xy_str", echo = DEBUG)
  outputComments(paste("e$panelvar1 = ", e$panelvar1), echo = DEBUG)
  yaxis <- gsub("\n"," ", e$panelvar1)
  allResults <- allResultsReactive()
  plotResults <- plotResultsReactive()
  if (yaxis == "% MEAC")
  {
    TO <- plotResults$Drug == "total opioid"
    if (sum(TO) == 0)
    {
      TO <- plotResults$Wrap == "% MEAC"
      outputComments(paste("Elements found in search of plotResults$Wrap", sum(TO)), echo = DEBUG)
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

  if (yaxis == "Events")
  {
    return("Click to enter events, double click to edit events.")
  }

  x <- unlist(strsplit(yaxis," "))
  drug <- x[1]
  outputComments(paste("Drug identified in xy_str() is",drug), echo = DEBUG)
  i <- which(x[1] == drugList)
  j <- which.min(abs(e$x - drugs[[drug]]$equiSpace$Time))
  x[2] <- substr(x[2],2,10)
  x[2] <- substr(x[2],1,nchar(x[2])-1)
  # cat("in xy_str()\n")
  # cat("i = ", i, "\n")
  # cat("j = ",j,"\n")
  # cat(str(drugs[[drug]]$equiSpace$Time), "\n")
  time <- round(drugs[[drug]]$equiSpace$Time[j], 1)
  if (input$referenceTime != "none")
  {
    time <- deltaToClockTime(input$referenceTime, time)
  } else {
    time = paste(time, "minutes")
  }
  returnText <- paste0("Time: ", time, ", ",x[1], " Ce: ", signif(drugs[[drug]]$equiSpace$Ce[j], 2), " ", x[2])
  if (prior$plotRecovery)
  {
    returnText <- paste0(returnText,", Time until ",drugDefaults$endCeText[i], " ",round(drugs[[drug]]$equiSpace$Recovery[j], 1), " minutes")
  }
  return(returnText)
}

# Click and Double Click Control ##########################################################
# get date and time from image

# Response to single click
observeEvent(
  input$plot_click,
  {
    DEBUG <- FALSE
    outputComments("in click()", echo = DEBUG)
    x <- imgDrugTime(input$plot_click)
    outputComments("in click(), returning from imgDrugTime()", echo = DEBUG)
    prior$DrugTimeUnits <- x

    if (x$drug == "Events")
    {
      clickPopupEvent(failed = "", x)
    } else {
      clickPopupDrug(failed = "", x)
    }
  }
)

# Response to double click
observeEvent(
  input$plot_dblclick,
  {
    DEBUG <- FALSE
    outputComments("in double click routine\n", echo = DEBUG)
    x <- imgDrugTime(input$plot_dblclick)
    prior$DrugTimeUnits <- x

    if (x$drug == "Events")
    {
      clickPopupEvent(failed = "", x)
    } else {
      dblclickPopupDrug(failed = "", x)
    }
  }
)

# Get the time, drug, and units from the image
imgDrugTime <- function(e = "")
{
  DEBUG <- FALSE
  outputComments("in imgDrugTime()", echo=DEBUG)
  allResults <- allResultsReactive()
  plotResults <- plotResultsReactive()
  plottedDrugs <- unique(allResults$Drug)
  plottedAll   <- unique(as.character(plotResults$Drug))
  outputComments(paste("plottedDrugs", plottedDrugs), echo=DEBUG)
  outputComments(paste("plottedAll", plottedAll), echo=DEBUG)

  # Get Time
  if (is.null(e$x) || is.null(e) || length(plottedDrugs) == 0)
  {
    if (e$coords_img$x < 300)
    {
      time <- 0
    } else {
      time <- prior$plotMaximum
      outputComments(paste("time is plotMaximum:", time), echo = DEBUG)
    }
  } else {
    i <- which(plottedDrugs[1] == drugList)
    drug <- plottedDrugs[1]
    outputComments(paste("Drug in imgDrugTime() is", drug), echo = DEBUG)
    j <- which.min(abs(e$x - drugs[[drug]]$equiSpace$Time))
    time <- round(drugs[[drug]]$equiSpace$Time[j], 1)
#    cat("Time from x axis = ",time,"\n")
  }
  if (input$referenceTime == "none")
  {
    time <- as.character(time)
  } else {
    time <- deltaToClockTime(input$referenceTime, time)
  }

  if(is.null(e) || length(plottedDrugs) == 0)
  {
  echoComments("Retuning because length(plottedDrugs = 0", echo = DEBUG)
    return(
      list(
        drug = "propofol",
        time = time,
        units = c("mg", "mcg/kg/min")))
  }

#  whichDrugs <- which(unlist(lapply(drugs,function(x) {if (is.null(x$DT)) FALSE else TRUE})))

  # Get Drug
  # cat("e$coords_img$y = ", e$coords_img$y,"\n")
  drugY <- c(170 + 1:(length(plottedAll)-1) * (1000- 170 - 200)/length(plottedAll),1001)
  # cat("drugY = ", drugY, "\n")
  drug <- plottedAll[e$coords_img$y < drugY][1]
  # cat("drug from drugY = ", drug, "\n")
  if (!is.null(e$panelvar1))
  {
    drug <-  unlist(
      strsplit(
      gsub("\n"," ", e$panelvar1)
      ," "))[1]
    outputComments(paste("drug from panelvar1",drug), echo = DEBUG)
  }
  if (drug %in% c("% MEAC", "p no response"))
    drug <- drug <- tail(plottedDrugs,1)

  # Get Units
  if (drug == "Events")
  {
    units <- c("","")
  } else {
    i <- which(drug == drugList)
    units <- c(drugDefaults$Bolus.Units[i], drugDefaults$Infusion.Units[i])
  }
  outputComments("Exiting imgDrugTime()", echo = DEBUG)
  return(
    list(
      drug=drug,
      time = time,
      units = units
      )
    )
}

#################################### Single Click Response ##################################

# Primary response
clickPopupDrug <- function(
  failed = "",
  x
  )
  {
  drug <- x$drug
  time <- x$time
  units <- x$units
  thisDrug <- which(drug == drugList)
  units <- unlist(drugDefaults$Units[thisDrug])
  selectedUnits <- drugDefaults$Default.Units[thisDrug]
  endCe <- drugDefaults$endCe[thisDrug]
  endCeText <- drugDefaults$endCeText[thisDrug]
  showModal(
    modalDialog(
    title = paste("Enter a new dose for ", drug),
    if (failed != "")
      tags$div(
        HTML(paste(tags$span(style="color:red; font-weight:bold ", failed), sep = ""))
      ),
    textInput(
      inputId = "clickTimeDrug",
      label = "Time",
      value = time
    ),
    textInput(
      inputId = "clickDose",
      label = "Dose",
      placeholder = "Enter dose"
    ),
    radioButtons(
      inputId = "clickUnits",
      label = "Units",
      choices = units,
      selected = selectedUnits,
      inline = TRUE
    ),
    numericInput(
      inputId = "newEndCe",
      label = paste("Set", endCeText, "concentration"),
      value = endCe,
      min = 0,
      max = 1000
    ),
    actionButton(
      inputId = "clickOKDrug",
      label = "OK",
      style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
    ),
    tags$button(
      type = "button",
      class = "btn btn-warning",
      `data-dismiss` = "modal",
      "Cancel"
    ),
    actionButton(
      inputId = "editDoses",
      label = paste("Edit prior doses"),
      style="color: #fff; background-color: #00C000; border-color: ##008000"
    ),
    footer = NULL,
    easyClose = TRUE,
    fade=TRUE,
    size="s"
  )
  )
}

# When OK button is pressed, attempt to load the data set. If successful,
# remove the modal. If not show another modal, but this time with a failure
# message.
# Note to Dean: The event below allows users to enter a new dose by clicking on the graph.
# This is another way in which the table can be changed. I don't think this needs to be
# edited, but it explains why there will always be some updating of the doseTable from the
# server.
observeEvent(
  input$clickOKDrug,
  {
    # Check that data object exists and is data frame.
    modelOK <- TRUE
    clickTime <- validateTime(input$clickTimeDrug)
    clickDose <- validateDose(input$clickDose)
    if (clickTime == "" & clickDose != "")
    {
      modelOK <- FALSE
      failed = "Missing time"
    }
    if (modelOK)
    {
      removeModal()
      thisDrug <- which(drugDefaults$Drug == prior$DrugTimeUnits$drug)
      if (drugDefaults$endCe[thisDrug] != input$newEndCe)
      {
        drugDefaults$endCe[thisDrug] <<- input$newEndCe
        newDrugDefaultsFlag(TRUE)
      }

      if (clickTime != "" && clickDose != "")
      {
        i <- which(current$DT$Drug == "")[1]
        current$DT$Drug[i]  <- prior$DrugTimeUnits$drug
        current$DT$Time[i]  <- clickTime
        current$DT$Dose[i]  <- clickDose
        current$DT$Units[i] <- input$clickUnits
        if (current$DT$Drug[nrow(current$DT)] != "" )
        {
          current$DT <- rbind(current$DT, doseTableNewRow)
        }
        doseTable(current$DT)
      }
    } else {
      clickPopupDrug(
        failed,
        prior$DrugTimeUnits
      )
    }
  }
)


# Edit prior drug doses
observeEvent(
  input$editDoses,
  {
    removeModal()
    tempTable <-  current$DT[current$DT$Drug ==  prior$DrugTimeUnits$drug,]
    tempTable$Delete <- FALSE
    tempTableHOT <- rhandsontable(
      tempTable[,c("Delete","Time","Dose","Units")],
      overflow = 'visible',
      rowHeaders = NULL,
      height = 220,
      selectCallback = TRUE
    ) %>%
      hot_col(
        col = "Delete",
        type="checkbox",
        halign = "htRight",
        allowInvalid = FALSE,
        strict = TRUE
      ) %>%
      hot_col(
        col = "Time",
        type="autocomplete",
        halign = "htRight",
        allowInvalid = TRUE,
        strict = FALSE
      ) %>%
      hot_col(
        col = "Dose",
        type = "autocomplete",
        halign = "htRight",
        allowInvalid = TRUE,
        strict = FALSE
      ) %>%
      hot_col(
        col = "Units",
        type = "dropdown",
        source = units,
        strict = TRUE,
        halign = "htLeft",
        valign = "vtMiddle",
        allowInvalid=FALSE
      ) %>%
      hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) %>%
      hot_rows(rowHeights = 10) %>%
      hot_cols(colWidths = c(50,55,55,90))
    output$tempTableHTML <- renderRHandsontable(tempTableHOT)
    showModal(
      modalDialog(
        title = paste("Edit", prior$DrugTimeUnits$drug,"doses:"),
        rHandsontableOutput(outputId = "tempTableHTML"),
        actionButton(
          inputId = "editDosesOK",
          label = "OK",
          style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
        ),
        tags$button(
          type = "button",
          class = "btn btn-warning",
          `data-dismiss` = "modal",
          "Cancel"
        ),
        footer = NULL,
        easyClose = TRUE,
        fade=TRUE,
        size="s"
      )
    )
  }
)

observeEvent(
  input$editDosesOK,
  {
    removeModal()
    TT <- hot_to_r(input$tempTableHTML)
    cat("In ObserveEvent for editDosesOK\n")
    TT$Drug <- prior$DrugTimeUnits$drug
    cat("TT:\n")
    print(TT)
    cat("current$DT:\n")
    print(current$DT)
    current$DT <- rbind(
      TT[!TT$Delete,c("Drug","Time","Dose","Units")],
      current$DT[current$DT$Drug != prior$DrugTimeUnits$drug,]
    )

    # Sort by time, by drug, but put blanks at the bottom
    print(unique(current$DT$Time))
    current$DT$Time[current$DT$Time == ""] <- "zzzzz"
    current$DT <- current$DT[order(current$DT$Time, current$DT$Drug),]
    current$DT$Time[current$DT$Time == "zzzzz"] <- ""

    cat("current$DT after update:\n")
    print(current$DT)

    for (i in 1:nrow(current$DT))
    {
      if (current$DT$Drug[i] > "")
      {
        current$DT$Time[i] <- validateTime(current$DT$Time[i])
        current$DT$Dose[i] <- validateDose(current$DT$Dose[i]) # should work for target too
      }
    }
    doseTable(current$DT) # Set reactive doseTable
  }
)

# Single Click - Events
clickPopupEvent <- function(
  failed = "",
  x
)
{
  time <- x$time
  showModal(
    modalDialog(
      title = paste("Enter a new event"),
      if (failed != "")
        tags$div(
          HTML(paste(tags$span(style="color:red; font-weight:bold ", failed), sep = ""))
        ),
      textInput(
        inputId = "clickTimeEvent",
        label = "Time",
        value = time
      ),
      selectInput(
        inputId = "clickEvent",
        label = "Event",
        choices = eventDefaults$Event
      ),
      actionButton(
        inputId = "clickOKEvent",
        label = "OK",
        style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
      ),
      tags$button(
        type = "button",
        class = "btn btn-warning",
        `data-dismiss` = "modal",
        "Cancel"
      ),
      actionButton(
        inputId = "editEvents",
        label = paste("Edit prior events"),
        style="color: #fff; background-color: #00C000; border-color: ##008000"
      ),
      footer = NULL,
      easyClose = TRUE,
      fade=TRUE,
      size="s"
    )
  )
}

observeEvent(
  input$clickOKEvent,
  {
    # Check that data object exists and is data frame.
    modelOK <- TRUE
    clickTime <- validateTime(input$clickTimeEvent)
    if (input$referenceTime == "none")
    {
      clickTime <- as.numeric(clickTime)
    } else {
      if (is.na(input$referenceTime))
      {
        referenceTime <- prior$referenceTime
        cat("input$referenceTime is NA. Using", referenceTime,"\n")
      } else {
        referenceTime <- input$referenceTime
      }
      if (nchar(clickTime) == 5)
        {
        clickTime <- clockTimeToDelta(referenceTime, clickTime)
      } else {
        clickTime <- as.numeric(clickTime)
      }
    }

    clickEvent <- input$clickEvent
    if (clickTime == "")
    {
      modelOK <- FALSE
      failed = "Missing time"
    }
    if (modelOK)
    {
      ET <- eventTable()
      ET <- data.frame(
        Time  = c(ET$Time, clickTime),
        Event = c(ET$Event, clickEvent),
        Fill = c(ET$Fill, eventDefaults$Color[clickEvent == eventDefaults$Event]),
        stringsAsFactors = FALSE
      )
      ET <- ET[order(ET$Time,ET$Event),]
      eventTable(ET)
      removeModal()
    } else {
      clickPopupEvent(
        failed = failed,
        prior$DrugTimeUnits
      )
    }
  }
)

# Edit prior drug doses
observeEvent(
  input$editEvents,
  {
    removeModal()
    tempTable <-  eventTable()
    tempTable <- tempTable[,c("Time", "Event")]
    tempTable$Delete <- FALSE
    tempTableHOT <- rhandsontable(
      tempTable[,c("Delete","Time","Event")],
      overflow = 'visible',
      rowHeaders = NULL,
      height = 220,
      selectCallback = TRUE
    ) %>%
      hot_col(
        col = "Delete",
        type="checkbox",
        halign = "htRight",
        allowInvalid = FALSE,
        strict = TRUE
      ) %>%
      hot_col(
        col = "Time",
        type="autocomplete",
        halign = "htRight",
        allowInvalid = TRUE,
        strict = FALSE
      ) %>%
      hot_col(
        col = "Event",
        type = "dropdown",
        source = eventDefaults$Event,
        strict = TRUE,
        halign = "htLeft",
        valign = "vtMiddle",
        allowInvalid=FALSE
      ) %>%
      hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) %>%
      hot_rows(rowHeights = 10) %>%
      hot_cols(colWidths = c(50,55,90))
    output$tempTableHTML <- renderRHandsontable(tempTableHOT)
    showModal(
      modalDialog(
        title = paste("Edit Events"),
        rHandsontableOutput(outputId = "tempTableHTML"),
        actionButton(
          inputId = "editEventsOK",
          label = "OK",
          style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
        ),
        tags$button(
          type = "button",
          class = "btn btn-warning",
          `data-dismiss` = "modal",
          "Cancel"
        ),
        footer = NULL,
        easyClose = TRUE,
        fade=TRUE,
        size="s"
      )
    )
  }
)

observeEvent(
  input$editEventsOK,
  {
    removeModal()
    ET <- hot_to_r(input$tempTableHTML)
    ET <- ET[!ET$Delete,c("Time","Event")]
    CROWS <- match(ET$Event, eventDefaults$Event)
    ET$Fill <- eventDefaults$Color[CROWS]
    ET <- ET[order(ET$Time,ET$Event),]
    eventTable(ET)
  }
)


# Double Click Response ################################
dblclickPopupDrug <- function(
  failed = "",
  x
)
{
  drug <- x$drug
  time <- x$time
  units <- sort(unique(unlist(drugDefaults$Units)))
  showModal(
    modalDialog(
      title = paste("Select a drug and enter dose and time"),
      if (failed != "")
        tags$div(
          HTML(paste(tags$span(style="color:red; font-weight:bold ", failed), sep = ""))
        ),
      selectInput(
        inputId = "dblclickDrug",
        label = "Drug",
        choices = drugList,
        selected = drug
      ),
      textInput(
        inputId = "dblclickTime",
        label = "Time",
        value = time
      ),
      textInput(
        inputId = "dblclickDose",
        label = "Dose",
        placeholder = "Enter dose"
      ),
      radioButtons(
        inputId = "dblclickUnits",
        label = "Units",
        choices = c(units),
        selected = drugDefaults$Bolus.Units[i],
        inline = TRUE
      ),
      actionButton(
        inputId = "dblclickOK",
        label = "OK",
        style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
      ),
      tags$button(
        type = "button",
        class = "btn btn-warning",
        `data-dismiss` = "modal",
        "Cancel"
      ),
      actionButton(
        inputId = "dblclickDelete",
        label = paste("Delete ",drug),
        style="color: #fff; background-color: #00C000; border-color: ##008000"
      ),
      footer = NULL,
      easyClose = TRUE,
      fade=TRUE,
      size="s"
    )
  )
}

observeEvent(
  input$dblclickOK,
  {
    # Check that data object exists and is data frame.
    modelOK <- TRUE
    clickTime <- validateTime(input$dblclickTime)
    clickDose <- validateDose(input$dblclickDose)
    if (clickTime == "")
    {
      modelOK <- FALSE
      failed = "Missing time"
    } else {
      if (clickDose == "")
      {
        modelOK <- FALSE
        failed = "Missing dose"
      }
    }
    if (modelOK)
    {
      removeModal()
      if (clickTime != "" && clickDose != "")
      {
        i <- which(current$DT$Drug == "")[1]
        current$DT$Drug[i]  <- input$dblclickDrug
        current$DT$Time[i]  <- clickTime
        current$DT$Dose[i]  <- clickDose
        current$DT$Units[i] <- input$dblclickUnits
        if (current$DT$Drug[nrow(current$DT)] != "" )
        {
          current$DT <- rbind(current$DT, doseTableNewRow)
        }
        doseTable(current$DT) # Call reactive variable
      }
    } else {
      dblclickPopupDrug(
        failed = failed,
        prior$DrugTimeUnits
      )
    }
  }
)

observeEvent(
  input$dblclickDelete,
  {
    removeModal()
    current$DT <- current$DT[current$DT$Drug != prior$DrugTimeUnits$drug,]
    doseTable(current$DT)
  }
)

# Target Drug Dosing (TCI Like) ###########################################
# Event to trigger calculation to set doses for a target
observeEvent(
  input$setTarget,
  {
    targetTable <-  data.frame(
      Time = rep("",6),
      Target = rep("", 6)
    )
    targetHOT <- rhandsontable(
        targetTable,
        overflow = 'visible',
        rowHeaders = NULL,
        height = 220,
        selectCallback = TRUE
    ) %>%
    hot_col(
      col = "Time",
      type="autocomplete",
      halign = "htRight",
      allowInvalid = TRUE,
      strict = FALSE
    ) %>%
    hot_col(
      col = "Target",
      type = "autocomplete",
      halign = "htRight",
      allowInvalid = TRUE,
      strict = FALSE
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
    )
    output$targetTableHTML <- renderRHandsontable(targetHOT)
    showModal(
      modalDialog(
        title = paste("Enter Target Effect Site Concentrations"),
        tags$div(
          HTML(
            paste(
              tags$span(
                style="
                  color:red; font-weight:bold ",
                "Enter time and target concentration below.
                Decreasing targets are not yet supported,
                and will be removed. Doses are found with
                non-linear regression, which takes a moment
                to calculate. The suggestion will be good,
                but better algorithms likely exist."
              ),
              sep = ""
            )
          )
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
         class = "btn btn-warning",
         style = "margin: 0px 5px 5px 5px;",
         `data-dismiss` = "modal",
         "Cancel"
       ),
       footer = NULL,
       easyClose = TRUE,
       fade=TRUE,
       size="s"
      )
    )
  }
)

# Evaluate target concentration
observeEvent(
  input$targetOK,
  {
    DEBUG <- TRUE
    removeModal()
    endTime <- validateTime(input$targetEndTime)
    if ((endTime) == "")
    {
      outputComments("No endtime", echo = DEBUG)
      return()
    }
    targetTable <- hot_to_r(input$targetTableHTML)
    targetTable$Time <- as.character(targetTable$Time)
    targetTable$Target <- as.character(targetTable$Target)
    print(str(targetTable))
    # Process and clean up targetTable
    for (i in seq_len(nrow(targetTable)))
    {
      targetTable$Time[i] <- validateTime(targetTable$Time[i])
      targetTable$Target[i] <- validateDose(targetTable$Target[i]) # should work for target too
    }
    # cat("After validating time and dose\n")
    # print(str(targetTable))
    targetTable$Target    <- as.numeric(targetTable$Target)
    targetTable$Time    <- as.character(targetTable$Time)  # Stored as factors... Arrgh.....
    targetTable <- targetTable[!is.na(targetTable$Target) & targetTable$Time != "",]
    outputComments("structure of targetTable", echo = DEBUG)
    outputComments(targetTable, echo=DEBUG)
    # # Remove blank values of targetTable
    if (input$referenceTime == "none")
    {
      targetTable$Time <- as.numeric(targetTable$Time)
      endTime <- as.numeric(endTime)
    } else {
      if (is.na(input$referenceTime))
      {
        referenceTime <- prior$referenceTime
        cat("input$referenceTime is NA. Using", referenceTime,"\n")
      } else {
        referenceTime <- input$referenceTime
      }
      targetTable$Time    <- clockTimeToDelta(referenceTime, targetTable$Time)
      endTime <- clockTimeToDelta(referenceTime, endTime)
    }
    outputComments(paste("End Time =", endTime), echo = DEBUG)
    outputComments("Structure of targetTable after processing time", echo = DEBUG)
    outputComments(targetTable, echo = DEBUG)
    targetTable <- targetTable[
        !is.na(targetTable$Target) &
        !is.na(targetTable$Time), ]
    targetTable <- targetTable[targetTable$Time < endTime,]
    if (nrow(targetTable) == 0)
    {
      outputComments("Target table is empty", echo = DEBUG)
      return()
    }
    targetTable <- targetTable[order(targetTable$Time), ]
    # Remove decreasing
    if (nrow(targetTable) > 1)
    {
      for (i in 2:nrow(targetTable))
      {
        targetTable$Target[i] <- max(targetTable$Target[i], targetTable$Target[i-1])
      }
    }
    # Calculate time offset (table must start at time 0)
    offsetTime <- min(targetTable$Time)
    targetTable$Time <- targetTable$Time - offsetTime
    endTime <- endTime - offsetTime

    # print(str(targetTable))
    #
    outputComments("Ready to search for the target dose", echo = DEBUG)
    drug <- drugList[which(input$targetDrug == drugList)]

    outputComments(paste("Drug is", drug), echo = DEBUG)

    infusionT1 <- round(c(targetTable$Time + drugs[[drug]]$tPeak, endTime), 0)
    infusionT2 <- round(infusionT1[1:(length(infusionT1)-1)] +
                        (infusionT1[2:(length(infusionT1))] - infusionT1[1:(length(infusionT1)-1)]) / 5
                    ,0)

    outputComments(paste("Drug is", drug), echo = DEBUG)
    testTable <- data.frame(
      Time = c(targetTable$Time[],infusionT1, infusionT2),
      Dose = 1,
      Units = c(rep(drugs[[drug]]$Bolus.Units, nrow(targetTable)),
                rep(drugs[[drug]]$Infusion.Units,nrow(targetTable)*2+1)),
      stringsAsFactors = FALSE)
    testTable <- testTable[order(testTable$Time),]
    testTable$Dose[nrow(testTable)] <- 0

    outputComments("Structure of testTable", echo = DEBUG)
    outputComments(testTable, echo = DEBUG)
    ET <- eventTable()
    outputComments("In Target, ET =", echo = DEBUG)
    outputComments(ET, echo = DEBUG)
    results <- simCpCe(
      testTable,
      ET,
      drugs[[drug]],
      endTime,
      plotRecovery = FALSE)$equiSpace[,c("Time","Ce")]
    # plot <- ggplot(results,aes(x=Time, y=Ce)) +
    #   geom_line() +
    #   labs(title="First pass")
    # print(plot)


    # Now calcualte the infusion rate that would based on the end infusion concentrations
    USE <- 1:(nrow(testTable)-1)
    for (x in 1:10)
    {
      results <- simCpCe(testTable, ET, drugs[[drug]] ,endTime, plotRecovery = FALSE)$equiSpace[,c("Time","Ce")]
      for (i in USE)
      {
        testTable$resultTime[i] <- max(results$Time[results$Time < testTable$Time[i+1]])
        t <- max(results$Time[results$Time < testTable$Time[i+1]])
        Ce <- results$Ce[results$Time == t]
        t <- max(targetTable$Time[targetTable$Time <= testTable$Time[i]])
        Target <- targetTable$Target[targetTable$Time == t]

        testTable$Dose[i] <- testTable$Dose[i] / Ce * Target
      }
    }
    # plot <- ggplot(results,aes(x=Time, y=Ce)) +
    #  geom_line() +
    #  labs(title="After 10 iterations")
    # print(plot)

    # Now set up for nlm
    obj <- function(Dose, Time, Units, PK, maximum)
    {
      Dose[Dose < 0] <- 0  # Prevent 0 doses
      DT <- data.frame(
        Time = Time,
        Dose = Dose,
        Units = Units
      )
      ce <- simCpCe(
        DT,
        ET,
        PK,
        endTime,
        plotRecovery = FALSE
      )$equiSpace[,c("Time","Ce")]

      target <- sapply(
        results$Time,
        function(x)
        {
          targetTable$Target[
            which(
              targetTable$Time == max(
                targetTable$Time[targetTable$Time <= x]
              )
            )
          ]
        }
      )
      return(sum((ce-target)^2))
    }

    outputComments("About to run nlm", echo = DEBUG)
    testTable$Dose <- nlm(
      obj,
      testTable$Dose,
      testTable$Time,
      testTable$Units,
      drugs[[drug]],
      endTime
     )$estimate

    testTable$Dose[testTable$Dose < 0] <- 0
    testTable$Dose <- signif(testTable$Dose,3)
    results <- simCpCe(
      testTable,
      ET,
      drugs[[drug]],
      endTime,
      plotRecovery = FALSE
    )$equiSpace[,c("Time","Ce")]

    # Interim plot
    # plot <- ggplot(results,aes(x=Time, y=Ce)) +
    #   geom_line() +
    #   labs(title="After nlm")
    # print(plot)

    testTable$Drug <- input$targetDrug
    testTable$Time <- testTable$Time + offsetTime
    # print(str(testTable))

    outputComments("Setting current$DT", echo = DEBUG)
    current$DT <- current$DT[current$DT$Drug != input$targetDrug,]
    # print(str(doseTable))

    current$DT <- rbind(
      testTable[,c("Drug","Time","Dose","Units")],
      current$DT
    )
    doseTable(current$DT)
  }
)

observeEvent(
  input$editDrugs,
  {
    x <- drugDefaults
    x$Units <- originalUnits
    drugsHOT <- rhandsontable(
      x,
      overflow = 'visible',
      rowHeaders = NULL,
      height = 220,
      selectCallback = TRUE
    ) %>%
    hot_col(
        col = 1,
        type="autocomplete",
        halign = "htRight",
        allowInvalid = TRUE,
        strict = FALSE,
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
      hot_col(
        col = 6,
        type = "autocomplete",
        strict = FALSE,
        allowInvalid = TRUE,
        halign = "htLeft",
      ) %>%
      hot_col(
        col = 7,
        type = "autocomplete",
        halign = "htRight",
        allowInvalid = TRUE,
        strict = FALSE
      ) %>%
      hot_col(
        col = 8,
        type = "autocomplete",
        halign = "htRight",
        allowInvalid = TRUE,
        strict = FALSE
      ) %>%
      hot_col(
        col = 9,
        type = "autocomplete",
        halign = "htRight",
        allowInvalid = TRUE,
        strict = FALSE
      ) %>%
      hot_col(
        col = 10,
        type = "autocomplete",
        halign = "htRight",
        allowInvalid = TRUE,
        strict = FALSE
      ) %>%
      hot_col(
        col = 11,
        type = "autocomplete",
        halign = "htRight",
        allowInvalid = TRUE,
        strict = FALSE
      ) %>%
      hot_col(
        col = 12,
        type = "autocomplete",
        halign = "htRight",
        allowInvalid = TRUE,
        strict = FALSE
      ) %>%
      hot_col(
        col = 13,
        type = "autocomplete",
        halign = "htRight",
        allowInvalid = TRUE,
        strict = FALSE
      ) %>%
      hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) %>%
      hot_rows(rowHeights = 10)
    output$editDrugsHTML <- renderRHandsontable(drugsHOT)
    showModal(
      modalDialog(
        title = paste("Edit Drug Defaults"),
        tags$div(
          HTML(
            paste(
              tags$span(
                style="color:red; font-weight:bold ",
                      "This is primarily intended for stanpumpR collaborators. ",
                      "If you believe some drug defaults are incorrect, please contact ",
                      "steven.shafer@stanford.edu. ",
                      "Also, you can easily break your session by entering crazy things. ",
                      "If so, just reload your session. "), sep = ""))
        ),
        rHandsontableOutput(
          outputId = "editDrugsHTML"
        ),
        actionButton(
          inputId = "drugEditsOK",
          label = "OK",
          style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
        ),
        tags$button(
          type = "button",
          class = "btn btn-warning",
          `data-dismiss` = "modal",
          "Cancel"
        ),
        footer = NULL,
        easyClose = TRUE,
        fade=TRUE,
        size="l"
      )
    )
  }
)

# Evaluate target concentration
observeEvent(
  input$drugEditsOK,
  {
    removeModal()
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
    newDrugDefaults$endCe                <- as.numeric(newDrugDefaults$endCe)
    newDrugDefaults$endCeText            <- as.character(newDrugDefaults$endCeText)

    drugDefaults <<- newDrugDefaults
    originalUnits <<- drugDefaults$Units
    drugDefaults$Units <<- strsplit(drugDefaults$Units, ",")
    drugList <<- drugDefaults$Drug
    colorList <<- drugDefaults$Color
    for (i in 1:nrow(drugDefaults))
    {
      drug <- drugDefaults$Drug[i]
      drugs[[drug]]$Color <- drugDefaults$Color[i]
      drugs[[drug]]$endCe <- drugDefaults$endCe[i]
      drugs[[drug]]$endCeText <- drugDefaults$endCeText[i]
    }
    newDrugDefaultsFlag(TRUE)
  }
)
### modalDialog ###
observeEvent(
  input$age,
  {
    if(!is.na(input$age) && (input$age > 110 | input$age <= 0))
    {
      showModal(
        modalDialog(
          title = NULL,
          "Age must be between 0 and 110."
        )
      )
    }
  }
)

observeEvent(
  input$weight,
  {
    if(!is.na(input$weight) && (input$weight > 500 | input$weight <= 0.1))
    {
      showModal(
        modalDialog(
          title = NULL,
          "Weight must be between 0.1 and 500"
        )
      )
    }
  }
)

observeEvent(
  input$height,
  {
    if(!is.na(input$height) && (input$height > 200 | input$height <= 10))
    {
      showModal(
        modalDialog(
        title = NULL,
        "Height must be between 1 and 200"
        )
      )
    }
  }
)

  outputComments("Reached the end of server()")
}
