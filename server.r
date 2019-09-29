# stanpumpR  ------------------------

# isShinyLocal <- Sys.getenv('SHINY_PORT') == ""
# if (isShinyLocal) {
#   setwd("c:/google drive/projects/stanpumpR")
# }
# 
source("global.r")

# server function ----------------------------------------------
server <- function(input, output, session)
{
  
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
  
  # Make drugs and events local to session
  drugDefaults <- drugDefaults_global
  originalUnits <- drugDefaults$Units
  drugDefaults$Units <- strsplit(drugDefaults$Units, ",")
  drugList <- drugDefaults$Drug
  colorList <- drugDefaults$Color
  
  eventDefaults <- eventDefaults_global
  
  # Prior data
  prior                    <- NULL
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
                                   # Set to DT after processDoseTable
  prior$ET                 <- NULL
  prior$referenceTime      <- "none"
  prior$plotMEAC           <- FALSE
  prior$plotInteraction    <- FALSE
  prior$plotCost           <- FALSE
  prior$plotEvents         <- FALSE
  prior$plotRecovery       <- FALSE
  prior$holdPlot           <- FALSE
  prior$DrugTimeUnits      <- ""

  clickNote                <- "Click a plot to add  doses, double click any plot to add drugs."
  plot                     <- NULL
  current                  <- NULL
  current$DT               <- NULL # Dose Table that agrees with handsontable  
  
  ##########################################################
  # Code to save state in url and then restore from url
  setBookmarkExclude(
    c(
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
      "editDrugs"
      )) 

  observe({
    # Trigger this observer every time an input changes
    reactiveValuesToList(input)
    session$doBookmark()
  })
  
  onBookmark(function(state) {  # 
    state$values$DT <- current$DT
    state$values$ET <- prior$ET
  })
  
  onBookmarked(function(url) {  # Without this, the session$doBookmark actually displays it
    updateQueryString(url)
    prior$url <<- url
  })
  
  onRestored(function(state) {
    doseTable <- as.data.frame(state$values$DT, stringsAsFactors=FALSE)
    current$DT <<- doseTable
    output$doseTableHTML <<- renderRHandsontable({createHOT(doseTable, drugDefaults)})
    prior$ET <<- as.data.frame(state$values$ET, stringsAsFactors=FALSE)
    if (nrow(prior$ET) == 0)
    {
      prior$ET <- NULL
    } else {
      eventTable <<- reactiveVal(prior$ET)
    }
    # cat("prior$ET = ", prior$ET,"\n")
    # cat("is prior$ET NULL ", is.null(prior$ET),"\n")
    # print(str(prior$ET))
  })
  
  ######################################################################
  # Initialization
  if (is.null(current$DT))
  {
    doseTable <- data.frame(
      Drug = c("propofol","fentanyl","remifentanil","rocuronium",""),
      Time = c(as.character(rep(0,4)), ""),
      Dose = c(as.character(rep(0,4)), ""),
      Units = c("mg","mcg","mcg/kg/min","mg",""),
      stringsAsFactors = FALSE
    )
    doseTableNewRow <-  doseTable[5,]
    
    # doseTable <- data.frame(
    #   Drug = c("oxycodone",""),
    #   Time = c("0", ""),
    #   Dose = c("10", ""),
    #   Units = c("mg po",""),
    #   stringsAsFactors = FALSE
    # )
    if (FALSE)
    {
      doseTable <- data.frame(
        Drug = drugDefaults$Drug,
        Time = "0",
        Dose = "1",
        Units = drugDefaults$Bolus.Units,
        stringsAsFactors = FALSE
      )
    }
    isolate({
      output$doseTableHTML <- renderRHandsontable({createHOT(doseTable, drugDefaults)})
      current$DT <- doseTable
    })
  }
 
  if (is.null(prior$ET))
  {
    eventTable <- data.frame(
      Time = 0,
      Event = "Start",
      Fill = "Blue",
      stringsAsFactors = FALSE
    )
    eventTable <- eventTable[FALSE,]

    prior$ET <- eventTable
  }
  eventTable <- reactiveVal(
    prior$ET
    )

  plotObject <- NULL
  allResults <- NULL
  plotResults <- NULL
  PK_set <- FALSE
  
  newDrugDefaultsFlag <- reactiveVal(FALSE)

  # Initialize drug table (PK for each drug)
  drugs <- vector("list", length = nrow(drugDefaults))
  names(drugs) <- drugDefaults$Drug
  for (i in 1:length(drugs))
  {
    drugs[[i]]$Color <- drugDefaults$Color[i]
  } 
  
  output$PlotSimulation <- renderPlot({introductionPlot})
  output$optionFlag <- renderText("")
  
  ########################################################################
  # Dose Table Loop                                                      #
  ########################################################################
  observe(
    {
    req(input$doseTableHTML)
    cat("Beginning of doseTable Loop\n")
    isolate({
      doseTable <- hot_to_r(input$doseTableHTML)
    })
    cat("Current$DT")
    print(current$DT)
    cat("Dose Table extracted from input$doseTableHTML\n")
    print(doseTable)

    if (nrow(doseTable) == 0)
    {
      cat("nrow(doseTable) == 0\n")
      doseTable <- rbind(doseTableNewRow)
      isolate({
        output$doseTableHTML <<- renderRHandsontable({createHOT(doseTable, drugDefaults)})
      })
      current$DT <<- doseTable
      return()
    }
    
    if (sameTable(doseTable, current$DT))
    {
      cat("The doseTable matched the current$DT\n")
      if (doseTable$Drug[nrow(doseTable)] != "" )
      {
        cat("Adding another row\n")
        doseTable <- rbind(doseTable, doseTableNewRow)
        isolate({
          current$DT <<- doseTable
          output$doseTableHTML <<- renderRHandsontable({createHOT(doseTable, drugDefaults)})
        })
      }
      return()
    }
    isolate({
    row <- input$doseTableHTML_select$select$r
    column <- input$doseTableHTML_select$select$c
    })
    
    cat("Row", row,"Column", column,"\n")

    if (is.null(row) | is.null(column))
    {
      cat("Either the row or column is null\n")
      current$DT <<- doseTable
      return()
    }
    
    thisDrug <- which(doseTable$Drug[row] == drugDefaults$Drug)

    switch(
      column,
      {
        # Xolumn 1 = drug
        cat("Value of Drug: ", doseTable$Drug[row], "\n")
        if (doseTable$Drug[row] != current$DT$Drug[row])
        {
          cell <- list(row = row - 1, col = 3)
          if (doseTable$Drug[row] == "")
          {
            cat("setting row ", row, "to blank\n")
            doseTable$Time[row] <- ""
            doseTable$Dose[row] <- ""
            doseTable$Units[row] <- ""
          } else {
            cat("Setting row ", row, "to initial zeros\n")
            doseTable$Time[row] <- "0"
            doseTable$Dose[row] <- "0"
            cat("drugDefaults$Default.Units[thisDrug]", 
                drugDefaults$Default.Units[thisDrug], "\n")
            doseTable$Units[row] <- drugDefaults$Default.Units[thisDrug]
            cat("This is what the doseTable should look like\n")
            print(doseTable)
          }
        }
      },
      {
        # Column 2 = Time 
        cat("Value of Time:", doseTable$Time[row],"\n")
        if (is.na(doseTable$Time[row])) doseTable$Time[row] <- ""
        x = as.character(validateTime(doseTable$Time[row]))
        if (doseTable$Time[row] != x)
        {
          doseTable$Time[row] <- x
        }
      },
      {
        # Column 3 = Dose
        cat("Value of Dose:", doseTable$Dose[row],"\n")
        if (is.na(doseTable$Dose[row])) doseTable$Dose[row] <- ""
        x = as.character(validateDose(doseTable$Dose[row]))
        if (doseTable$Dose[row] != x)
        {
          doseTable$Dose[row] <- x
        }
      }
    )
    cat("\nEnd of the loop\n")
    cat("Here is the desired doseTable\n")
    print(doseTable)
    cat("calling renderRHandsontable\n")
    isolate({
      current$DT <<- doseTable
      output$doseTableHTML <<- renderRHandsontable({createHOT(doseTable, drugDefaults)})
    })
    isolate({
      test <- hot_to_r(input$doseTableHTML)
      })
    cat("Value of input$doseTableHTML\n")
    print(test)
    cat("\nExiting loop\n")
    },
    priority = 1
  )
  
  ####################################################################################
  ## Main Observation Loop
  observe({
#    cat("Starting Main Observation Loop\n")
    req(
      input$sex, 
      input$age, 
      input$ageUnit, 
      input$weight, 
      input$weightUnit, 
      input$height, 
      input$heightUnit,
      input$doseTableHTML
      )
    if (
      is.numeric(input$age)             &
      is.numeric(input$weight)          &
      is.numeric(input$height)          
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
        weight      != prior$weight     |
        height      != prior$height     |
        age         != prior$age        |
        sex         != prior$sex        |
        weightUnit  != prior$weightUnit |
        heightUnit  != prior$heightUnit |
        ageUnit     != prior$ageUnit    |
        newDrugDefaultsFlag()
        )
      {
        PK_set <<- TRUE
        recalculatePKFlag <- TRUE
        for (i in 1:length(drugs))
          {
            cat("Getting PK for ", names(drugs)[i],"\n")
            drugs[[i]] <<- modifyList(
              drugs[[i]], 
              getDrugPK(
                drug = names(drugs)[i],
                weight = input$weight * as.numeric(input$weightUnit),
                height = input$height * as.numeric(input$heightUnit),
                age = input$age    * as.numeric(input$ageUnit),
                sex = input$sex,
                drugDefaults = drugDefaults
                )
            )
          drugs[[i]]$DT <<- NULL # Remove old dose table, if any
          drugs[[i]]$CpCe <<- NULL # Remove old simulations results, if any
          drugs[[i]]$equiSpace <<- NULL # Ditto
          drugs[[i]]$maxCp <<- 1
          drugs[[i]]$maxCe <<- 1
          drugs[[i]]$recovery <<- 1
        }
        # plotTable <- as.data.frame(
        #   cbind(
        #     map_chr(drugs, "drug"),
        #     map_chr(drugs, "typical"),
        #     map_chr(drugs, "lowerTypical"),
        #     map_chr(drugs, "upperTypical"),
        #     map_chr(drugs, "MEAC"),
        #     map_chr(drugs, "tPeak")
        #   ),stringsAsFactors = FALSE)
        # names(plotTable) <- c("Drug","typical", "lowerTypical","upperTypical", "MEAC","tPeak")
        # plotTable$Typical <- as.numeric(plotTable$typical)
        # plotTable$Lower <- as.numeric(plotTable$lowerTypical)
        # plotTable$Upper <- as.numeric(plotTable$upperTypical)
        # plotTable$MEAC  <- as.numeric(plotTable$MEAC)
        # plotTable$tPeak  <- as.numeric(plotTable$tPeak)
        # write.xlsx(plotTable[,c("Drug","Lower","Upper","Typical","MEAC","tPeak")],file="PlotTable.xlsx")
        
        # cat("****** Time to set up drug PK *********\n")
        # print(x)
        # cat("\n")
        prior$weight      <<- weight
        prior$height      <<- height
        prior$age         <<- age
        prior$sex         <<- sex
        prior$weightUnit  <<- weightUnit
        prior$heightUnit  <<- heightUnit
        prior$ageUnit     <<- ageUnit
      }
    
    #######################################################################
    # Has the dose table changed?                                         #
    DT <- current$DT
    DT$Drug    <- as.character(DT$Drug)
    DT$Units   <- as.character(DT$Units)
    DT$Dose    <- as.numeric(DT$Dose)
    DT$Time    <- as.character(DT$Time)  # Stored as factors... Arrgh.....
    DT <- DT[DT$Drug != "" & !is.na(DT$Dose)  & DT$Time != "" & DT$Units != "",]
    cat("structure of DT\n")
    print(str(DT))
    
    # Remove blank values of DT
    if (input$referenceTime == "none")
    {
      DT$Time <- as.numeric(DT$Time)
    } else {
      if (is.na(input$referenceTime))
      {
        referenceTime <- prior$referenceTime
        cat("input$referenceTime is NA. Using", referenceTime,"\n")
      } else {
        referenceTime <- input$referenceTime
      }
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
      cat("Dose table is empty\n")
      output$optionFlag <- renderText("")
      output$PlotSimulation <- renderPlot(nothingtoPlot)
      output$plotInfo <- renderText("")
      return()
    }
    DT <- DT[order(DT$Drug, DT$Time), ]

    #######################################################################
    # Has the event table changed?
    ET <- eventTable()
    ET$Time    <- as.character(ET$Time)
    if (input$referenceTime == "none")
    {
      ET$Time <- as.numeric(ET$Time)
    } else {
      if (is.na(input$referenceTime))
      {
        referenceTime <- prior$referenceTime
        cat("input$referenceTime is NA. Using", referenceTime,"\n")
      } else {
        referenceTime <- input$referenceTime
      }
      ET$Time    <- clockTimeToDelta(referenceTime, ET$Time)
    }
    if (input$maximum == 10)
    {
      ET <- ET[ET$Time <= 10,]
    }
    
    # Adjust maximum to include dose times
    plotMaximum <- as.numeric(input$maximum)
    steps <- maxtimes$steps[maxtimes$times == input$maximum]
    maxTime <- max(as.numeric(DT$Time), as.numeric(ET$Time))
    if (input$maximum != 10 & (maxTime + 29) >= plotMaximum) 
    {
      steps <- maxtimes$steps[maxtimes$times >= (maxTime + 30)][1]
      plotMaximum <- ceiling((maxTime + 30)/steps) * steps
    }
    xBreaks <- 0:(plotMaximum/steps) * steps
    if (input$referenceTime == "none")
    {
      xLabels <- xBreaks
      xAxisLabel <- "Time (Minutes)"
    } else
    {
      if (is.na(input$referenceTime))
      {
        referenceTime <- prior$referenceTime
        cat("input$referenceTime is NA. Using", referenceTime,"\n")
      } else {
        referenceTime <- input$referenceTime
      }
      xLabels <- deltaToClockTime(referenceTime, xBreaks)
      xAxisLabel <- "Time"
    }
    if (recalculatePKFlag) {
      cat("plotting because recalculatePKFlag is TRUE\n")
      replotFlag <- TRUE
    }
    if (!sameTable(DT, prior$DT)) {
      cat("plotting because doseTable changed\n")
      replotFlag <- TRUE
    } else {
      cat("doseTable remains unchanged\n")
    }
    if (!sameTable(ET, prior$ET)) {
      cat("plotting because eventTable changed\n")
      replotFlag <- TRUE
    }
    
    if (plotMaximum != prior$plotMaximum) {
      cat("plotting because plotMaximum changed to ", plotMaximum, "\n")
      replotFlag <- TRUE
    }
    plotRecovery           <- "Time to Emergence"    %in% input$addedPlots
    if (plotRecovery != prior$plotRecovery) {
      if (plotRecovery == TRUE)
      {
        cat("plotting because plotRecovery changed to TRUE\n")
        replotFlag <- TRUE
      }
    }
    
    if (replotFlag)
    {
      x <- processdoseTable(
        DT, 
        ET,
        drugs, 
        plotMaximum, 
        prior,
        plotRecovery
      )
      drugs     <<- x$drugs
      prior$DT  <<- DT
      prior$ET  <<- ET
      prior$plotMaximum <<- plotMaximum
    }
    if (length(input$plasmaLinetype) == 0 |
        length(input$effectsiteLinetype) == 0
    )
    {
      cat("Waiting for line types to exist before making figure\n")
      return()
    }
    if (!PK_set)
    {
      cat("Can't plot - PK aren't yet set\n")
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
    referenceTime          <- input$referenceTime
    if (plotRecovery) logY <- FALSE

    if(replotFlag                                          |
        plasmaLinetype         != prior$plasmaLinetype     |
        effectsiteLinetype     != prior$effectsiteLinetype |
        normalization          != prior$normalization      |
        title                  != prior$title              |
        caption                != prior$caption            |
        typical                != prior$typical            |
        logY                   != prior$logY               |
        referenceTime          != prior$referenceTime      |
        plotMEAC               != prior$plotMEAC           |
        plotInteraction        != prior$plotInteraction    |
        plotCost               != prior$plotCost           |
        plotEvents             != prior$plotEvents         |
        plotRecovery           != prior$plotRecovery
    )
    {
      cat('****************** SIMULATION PLOT CALLED BY ********************\n')
      if (replotFlag)          cat("New plot triggered by replotFlag\n")
      if (plasmaLinetype         != prior$plasmaLinetype) cat("New plot triggered by plasmaLinetype != prior$plasmaLinetype\n")
      if (effectsiteLinetype     != prior$effectsiteLinetype) cat("New plot triggered by effectsiteLinetype != prior$effectsiteLinetype\n")
      if (normalization          != prior$normalization) cat("New plot triggered by normalization != prior$normalization\n")
      if (title                  != prior$title) cat("New plot triggered by title != prior$title\n")
      if (caption                != prior$caption) cat("New plot triggered by caption != prior$caption\n")
      if (typical                != prior$typical) cat("New plot triggered by typical != prior$typical\n")
      if (logY                   != prior$logY) cat("New plot triggered by logY != prior$logY\n")
      if (referenceTime          != prior$referenceTime) cat("New plot triggered by referenceTime != prior$referenceTime\n")
      if (plotMEAC               != prior$plotMEAC) cat("New plot triggered by plotMEAC != prior$plotMEAC\n")
      if (plotInteraction        != prior$plotInteraction) cat("New plot triggered by plotInteraction != prior$plotInteraction\n")
      if (plotCost               != prior$plotCost) cat("New plot triggered by plotCost != prior$plotCost\n")
      if (plotEvents             != prior$plotEvents) cat("New plot triggered by plotEvents != prior$plotEvents\n")
      if (plotRecovery           != prior$plotRecovery) cat("New plot triggered by ploRecovery != prior$plotRecovery\n")
      if (newDrugDefaultsFlag() )  cat("New plot triggered new Drug Defaults\n")
      
      cat("\n")

      if (normalization != prior$normalization)
       {
        X <- setLinetypes(normalization)
        plasmaLinetype <- X$plasmaLinetype
        effectsiteLinetype <- X$effectsiteLinetype
      }
      output$PlotSimulation <- renderPlot({
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
          caption = caption,
          aspect = aspect,
          typical = typical,
          logY = logY,
          drugs = drugs,
          events = ET,
          eventDefaults = eventDefaults
        )
        if (is.null(X))
        {
          plotObject <<- NULL
          allResults <<- NULL
          plotResults <<- NULL
        } else {
          plotObject <<- X$plotObject
          allResults <<- X$allResults
          plotResults <<- X$plotResults
        }
        prior$plasmaLinetype      <<- plasmaLinetype
        prior$effectsiteLinetype  <<- effectsiteLinetype
        prior$normalization       <<- normalization
        prior$title               <<- title
        prior$caption             <<- caption
        prior$typical             <<- typical
        prior$logY                <<- logY
        prior$plotMaximum         <<- plotMaximum
        prior$referenceTime       <<- referenceTime
        prior$plotMEAC            <<- plotMEAC
        prior$plotInteraction     <<- plotInteraction
        prior$plotCost            <<- plotCost
        prior$plotEvents          <<- plotEvents
        prior$plotRecovery        <<- plotRecovery
        isolate({
          newDrugDefaultsFlag(FALSE)
        })
  
          if (is.null(plotObject))
          {
            output$optionFlag <- renderText("")
            cat("Null plot after calling simulation Plot()\n")
            output$plotInfo <- renderText("")
            return(nothingtoPlot)
          } else {
            output$optionFlag <- renderText("Graph Options")
            output$plotInfo <- renderText(clickNote)
            return(plotObject)
          }
        })
      }
    } else {
      cat("Test for numeric covariates failed\n")
      cat("input$sex = ",input$sex,"\n")
      cat("input$ageUnit = ",input$ageUnit, is.numeric(input$ageUnit),typeof(input$ageUnit), "\n")
      cat("input$weightUnit = ",input$weightUnit,is.numeric(input$weightUnit),"\n")
      cat("input$heightUnit = ",input$heightUnit,is.numeric(input$heightUnit),"\n")
      return(NULL)
    }
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
        p(
          actionButton(
            inputId = "sendSlide", 
            label = "Send Slide!",
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
      return(p("Check e-mail address"))
    }
  })
  
processNormalization <- observeEvent(
  input$normalization,
  priority=10,
  {
    #cat("Inside observeEvent for Linetypes\n")
    X <- setLinetypes(input$normalization)
    prior$plasmaLinetype <<- X$plasmaLinetype
    prior$effectsiteLinetype <<- X$effectsiteLinetype
    output$Linetype <- renderUI({
    column(
      width = 1,
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
      # output$EmailButton <- renderUI({
      #       p("Processing slide")
      # })
      cat("input$sendSlide",input$sendSlide,'\n')
      img <- sendSlide(
        recipient = input$recipient,
        title = input$title, 
        plotObject = plotObject, 
        DT <- current$DT,
        allResults = allResults, 
        plotResults = plotResults,
        isShinyLocal = isShinyLocal,
        url = prior$url, 
        slide = as.numeric(input$sendSlide)
        )
      output$sentPlot <- renderPlot(
        plotObject + labs(title=paste("Image sent to",input$recipient))
      )
    }
  )

# Get reference time from client
output$getReferenceTime <- renderUI({
  time <- input$client_time
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
  prior$reference.time <<- start
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
      title = "Select starting time. Doses given before the starting time are handled as doses on the next day.",
      placement = "right", 
      options = list(container = "body")
    )
  )
}
)

# Hover control ############################################################
output$plotInfo <- renderText({""})
observeEvent(
  input$plot_hover,
  output$plotInfo <- renderText({
    xy_str(input$plot_hover)
  })
)

# Display Time, CE, or total opioid
xy_str <- function(e) {
  if(is.null(e)) return(clickNote)
  if(is.null(e$panelvar1)) return(clickNote)
  # cat("e$panelvar1 = ", e$panelvar1, "\n")
  yaxis <- gsub("\n"," ", e$panelvar1)
  if (yaxis == "% MEAC")
  {
    TO <- plotResults$Drug == "total opioid"
    if (sum(TO) == 0)
    {
      TO <- plotResults$Wrap == "% MEAC"
      cat("Elements found in search of plotResults$Wrap", sum(TO), "\n")
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
  i <- which(x[1] == drugList)
  j <- which.min(abs(e$x - drugs[[i]]$equiSpace$Time))
  x[2] <- substr(x[2],2,10) 
  x[2] <- substr(x[2],1,nchar(x[2])-1)
  # cat("in xy_str()\n")
  # cat("i = ", i, "\n")
  # cat("j = ",j,"\n")
  # cat(str(drugs[[i]]$equiSpace$Time), "\n")
  time <- round(drugs[[i]]$equiSpace$Time[j], 1)
  if (input$referenceTime != "none") 
  {
    time <- deltaToClockTime(input$referenceTime, time)
  } else {
    time = paste(time, "minutes")
  }
  returnText <- paste0("Time: ", time, ", ",x[1], " Ce: ", signif(drugs[[i]]$equiSpace$Ce[j], 2), " ", x[2])
  if (prior$plotRecovery)
  {
    returnText <- paste0(returnText,", Recovery in ",round(drugs[[i]]$equiSpace$Recovery[j], 1), " minutes")
  }
  return(returnText)
}

# Click and Double Click Control ##########################################################
# get date and time from image

# Response to single click 
observeEvent(
  input$plot_click,
  {
    # cat("in click routine\n")
    x <- imgDrugTime(input$plot_click)
    prior$DrugTimeUnits <<- x
    
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
    # cat("in double click routine\n")
    x <- imgDrugTime(input$plot_dblclick)
    prior$DrugTimeUnits <<- x
    
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
  # cat("in imgDrugTime\n")
  plottedDrugs <- unique(allResults$Drug)
  plottedAll   <- unique(as.character(plotResults$Drug))
#  cat("plottedDrugs", plottedDrugs,"\n")
#  cat("plottedAll", plottedAll,"\n")

  # Get Time
  if (is.null(e$x) | is.null(e) | length(plottedDrugs) == 0)
  {
    if (e$coords_img$x < 300)
    {
      time <- 0
    } else {
      time <- prior$plotMaximum
#      cat("time is plotMaximum: ", time, "\n")
    }
  } else {
    i <- which(plottedDrugs[1] == drugList)
    j <- which.min(abs(e$x - drugs[[i]]$equiSpace$Time))
    time <- round(drugs[[i]]$equiSpace$Time[j], 1)
#    cat("Time from x axis = ",time,"\n")
  }
  if (input$referenceTime == "none") 
  {
    time <- as.character(time)
  } else {
    time <- deltaToClockTime(input$referenceTime, time)
  }

  if(is.null(e) | length(plottedDrugs) == 0) 
  {
#    cat("Retuning because length(plottedDrugs = 0\n")
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
    str(drug)
#    cat("drug from panelvar1",drug, "\n")
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
      inline = TRUE
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
observeEvent(
  input$clickOKDrug, 
  {
    # Check that data object exists and is data frame.
    modelOK <- TRUE
    clickTime <- validateTime(input$clickTimeDrug)
    clickDose <- validateDose(input$clickDose)
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
      if (clickTime != "" & clickDose != "")
      {
        doseTable <- current$DT
        i <- which(doseTable$Drug == "")[1]
        doseTable$Drug[i] <- prior$DrugTimeUnits$drug
        doseTable$Time[i] <- clickTime
        doseTable$Dose[i] <- clickDose
        doseTable$Units[i] <- input$clickUnits
        if (doseTable$Drug[nrow(doseTable)] != "" )
        {
          doseTable <- rbind(doseTable, doseTableNewRow)
        }
        output$doseTableHTML <- renderRHandsontable({createHOT(doseTable, drugDefaults)})
        current$DT <<- doseTable
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
    DT <- hot_to_r(input$tempTableHTML)
    DT$Drug <- prior$DrugTimeUnits$drug
    doseTable <- rbind(
      DT[!DT$Delete,c("Drug","Time","Dose","Units")],
      current$DT[current$DT$Drug != prior$DrugTimeUnits$drug,]
    )
    for (i in 1:nrow(doseTable))
    {
      doseTable$Time[i] <- validateTime(doseTable$Time[i])
      doseTable$Dose[i] <- validateDose(doseTable$Dose[i]) # should work for target too
    }
    current$DT <<- doseTable
    output$doseTableHTML <- renderRHandsontable({createHOT(doseTable, drugDefaults)})
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


############################### Double Click Response ################################
dblclickPopupDrug <- function(
  failed = "", 
  x
) 
{
  drug <- x$drug
  time <- x$time
  units <- unique(c(drugDefaults$Bolus.Units, drugDefaults$Infusion.Units))
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
      if (clickTime != "" & clickDose != "")
      {
        doseTable <- current$DT
        i <- which(doseTable$Drug == "")[1]
        doseTable$Drug[i] <- input$dblclickDrug
        doseTable$Time[i] <- clickTime
        doseTable$Dose[i] <- clickDose
        doseTable$Units[i] <- input$dblclickUnits
        if (doseTable$Drug[nrow(doseTable)] != "" )
        {
          doseTable <- rbind(doseTable, doseTableNewRow)
        }
        output$doseTableHTML <- renderRHandsontable({createHOT(doseTable, drugDefaults)})
        current$DT <<- doseTable
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
    doseTable <- current$DT
    doseTable <- doseTable[doseTable$Drug != prior$DrugTimeUnits$drug,]
    output$doseTableHTML <- renderRHandsontable({createHOT(doseTable, drugDefaults)})
    current$DT <<- doseTable
  }
)

################################################### Target Drug Dosing (TCI Like) ###########################################
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
       hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) %>%
       hot_rows(rowHeights = 10) %>%
       hot_cols(colWidths = c(70,70))
     output$targetTableHTML <- renderRHandsontable(targetHOT)
     showModal(
       modalDialog(
         title = paste("Enter Target Effect Site Concentrations"),
         tags$div(
           HTML(paste(tags$span(style="color:red; font-weight:bold ", 
           "Enter time and target concentration below. Decreasing targets are not yet supported, and will be removed. Doses are found with non-linear regression, which takes a moment to calculate. The suggestion will be close to ideal, but better algorithms may exist."), sep = ""))
         ),
         selectInput(
           inputId = "targetDrug",
           label = "Drug",
           choices = drugList
         ),
         rHandsontableOutput(outputId = "targetTableHTML"),
         textInput(
           inputId = "targetEndTime",
           label = "End Time",
           value = ""
         ),
       actionButton(
         inputId = "targetOK", 
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


# Evaluate target concentration
observeEvent(
  input$targetOK, 
  {
    removeModal()
    endTime <- validateTime(input$targetEndTime)
    if ((endTime) == "")
    {
      cat("No endtime\n")
      return()
    }
    targetTable <- hot_to_r(input$targetTableHTML)
    targetTable$Time <- as.character(targetTable$Time)
    targetTable$Target <- as.character(targetTable$Target)
    print(str(targetTable))
    # Process and clean up targetTable
    for (i in 1:nrow(targetTable))
    {
      targetTable$Time[i] <- validateTime(targetTable$Time[i])
      targetTable$Target[i] <- validateDose(targetTable$Target[i]) # should work for target too
    }
    # cat("After validating time and dose\n")
    # print(str(targetTable))
    targetTable$Target    <- as.numeric(targetTable$Target)
    targetTable$Time    <- as.character(targetTable$Time)  # Stored as factors... Arrgh.....
    targetTable <- targetTable[!is.na(targetTable$Target)  & targetTable$Time != "",]
    cat("structure of targetTable\n")
    # print(str(targetTable))
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
    # cat("End Time = ", endTime, "\n")
    # cat("structure of targetTable after processing time\n")
    # print(str(targetTable))
    targetTable <- targetTable[
        !is.na(targetTable$Target) & 
        !is.na(targetTable$Time), ]
    targetTable <- targetTable[targetTable$Time < endTime,]
    if (nrow(targetTable) == 0)
    {
      cat("Target table is empty\n")
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
    # cat("Ready to search for the target dose\n")
    drug <- which(input$targetDrug == drugList)

    infusionT1 <- round(c(targetTable$Time + drugs[[drug]]$tPeak, endTime), 0)
    infusionT2 <- round(infusionT1[1:(length(infusionT1)-1)] +
                        (infusionT1[2:(length(infusionT1))] - infusionT1[1:(length(infusionT1)-1)]) / 5  
                    ,0)
    
    testTable <- data.frame(
      Time = c(targetTable$Time[],infusionT1, infusionT2),
      Dose = 1,
      Units = c(rep(drugs[[drug]]$Bolus.Units, nrow(targetTable)),
                rep(drugs[[drug]]$Infusion.Units,nrow(targetTable)*2+1)),
      stringsAsFactors = FALSE)
    testTable <- testTable[order(testTable$Time),]
    testTable$Dose[nrow(testTable)] <- 0
    
    # cat("Structure of testTable\n")
    # print(str(testTable))
    ET <- eventTable()
    cat("In Target, ET = \n")
    print(ET)
    results <- simCpCe(testTable, ET, drugs[[drug]], endTime)$equiSpace[,c("Time","Ce")]
    # plot <- ggplot(results,aes(x=Time, y=Ce)) +
    #   geom_line() +
    #   labs(title="First pass")
    # print(plot)
    
    
    # Now calcualte the infusion rate that would based on the end infusion concentrations
    USE <- 1:(nrow(testTable)-1)
    for (x in 1:10)
    {
      results <- simCpCe(testTable, ET, drugs[[drug]] ,endTime)$equiSpace[,c("Time","Ce")]
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
      ce <- simCpCe(DT, ET, PK, endTime)$equiSpace[,c("Time","Ce")]
      target <- sapply(results$Time, 
                       function(x) {
                         targetTable$Target[
                           which(targetTable$Time == max(
                             targetTable$Time[targetTable$Time <= x])
                           )]
                       })
      return(sum((ce-target)^2))
    }
    # cat("About to run nlm\n")
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
    results <- simCpCe(testTable,ET, drugs[[drug]],endTime)$equiSpace[,c("Time","Ce")]
    # plot <- ggplot(results,aes(x=Time, y=Ce)) +
    #   geom_line() +
    #   labs(title="After nlm")
    # print(plot)
    
    testTable$Drug <- input$targetDrug
    testTable$Time <- testTable$Time + offsetTime
    # print(str(testTable))
    
    doseTable <- current$DT
    doseTable <- doseTable[doseTable$Drug != input$targetDrug,]
    # print(str(doseTable))
    
    doseTable <- rbind(testTable[,c("Drug","Time","Dose","Units")], doseTable)
    output$doseTableHTML <- renderRHandsontable({createHOT(doseTable, drugDefaults)})
    current$DT <<- doseTable
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
      hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) %>%
      hot_rows(rowHeights = 10)
    output$editDrugsHTML <- renderRHandsontable(drugsHOT)
    showModal(
      modalDialog(
        title = paste("Edit Drug Defaults"),
        tags$div(
          HTML(paste(tags$span(style="color:red; font-weight:bold ",
                               "This is primarily intended for stanpumpR collaborators. ",
                               "If you believe some drug defaults are incorrect, please contact ",
                               "steven.shafer@stanford.edu. ",
                               "Also, you can easily break your session by entering crazy things. ",
                               "If so, just reload your session. "), sep = ""))
        ),
        rHandsontableOutput(outputId = "editDrugsHTML"),
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
    newDrugDefaults$Emerge               <- as.numeric(newDrugDefaults$Emerge)
    drugDefaults <<- newDrugDefaults
    originalUnits <<- drugDefaults$Units
    drugDefaults$Units <<- strsplit(drugDefaults$Units, ",")
    drugList <<- drugDefaults$Drug
    colorList <<- drugDefaults$Color
    for (i in 1:length(drugs))
    {
      drugs[[i]]$Color <<- drugDefaults$Color[i]
    } 
    newDrugDefaultsFlag(TRUE)
  }
  )
}

#shinyApp(ui, server)
