# stanpumpR  ------------------------

##################################################################################
# Note to Dean:                                                                  #
# I think the easiest way to explain the rhandsontable is to annotate the code.  #
# You will find all of the annotations beginning with "Note to Dean."            #
# There are three programs with such note: server.r, ui.r, and createHOT.r.      #
# createHOT is the code that creates the hands on table ("HOT").                 #
# Most of the code is in server.r (this program).                                #
# The handsontable  has the dose, hence the name "doseTable."                    #
# To limit interaction with the browser, for the most part the server            #
# code uses "prior$DT" and "current$DT" for the dose table. Obviously,           #
# "current$DT" should reflect the current contents of the dose table.            #
# "prior$DT" reflects the dose table when the graph was last created.            #
# I use it to figure out what drugs, if any, need to be updated.                 #
#                                                                                #
# You will find a "Note to Dean" everwhere that I referene or validate           #
# The handsontable in the code below.                                            #
##################################################################################

# server function ----------------------------------------------------------------------------------
function(input, output, session)
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

  # Write out logs to the log section
  initLogMsg <- "Comments Log"
  commentsLog <- reactiveVal(initLogMsg)
  output$logContent <- renderUI({
    shinyjs::delay(0, shinyjs::js$scrollLogger())
    HTML(commentsLog())
  })
  outputComments <- function(text, echo = TRUE) {
    if (echo) {
      cat(text, "\n")
    }
    isolate(commentsLog(paste0(commentsLog(), "<br>", text)))
  }

  #############################################################################
  #                           Initialization                                  #
  #############################################################################

  cat("**********************************************************************\n")
  cat("*                       Initializing                                 *\n")
  cat("**********************************************************************\n")

  output$PlotSimulation <- renderPlot({
   introductionPlot
  })
  outputComments("Initializing")
  # Make drugs and events local to session
  cat("Setting drugDefaults and eventDefaults\n")
  drugDefaults <- drugDefaults_global
  drugList <- drugDefaults$Drug
  colorList <- drugDefaults$Color

  cat("Initializing prior and current\n")

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

  prior$DT <- doseTableInit
  cat("prior$DT at the end of initialization\n")
  print(prior$DT)

  ##################################################################################
  # Note to Dean:                                                                  #
  # As you suggested, I am setting up doseTable as a reactive variable. The is     #
  # Where it is initialized.                                                       #
  ##################################################################################
  doseTable <- reactiveVal(doseTableInit)

  # Routine to output doseTableHTML from doseTable
  ##################################################################################
  # Note to Dean:                                                                  #
  # This is the initialization of the handsontable. I doubt there is any need to   #
  # try and move this to JavaScript. The cat() command is for debugging when I'm   #
  # in RStudio. The "outputComments()" function gives me a running commentary when #
  # running on the shiny server. The reason for this is that I found the log file  #
  # on the shiny server to be practically useless. Thus, while the program is      #
  # still in development, I have a running log on the bottom of the screen.        #
  # Few people even notice it, because it is below the visible screen so  you have #
  # to scroll to see the running log.                                              #
  #
  # October 17, 2019: As requested, this is where output$doseTableHTML is called.  #
  # Everyplace else I just set doseTable, which is now a reactive variable.        #
  ##################################################################################

  output$doseTableHTML <- renderRHandsontable({
    outputComments("Setting up doseTableHTML.")
    createHOT(doseTable(), drugDefaults)
  })

  # End Initialize current$DT

  # Initialize prior$ET
  outputComments("Initializing prior$ET")
  if (is.null(prior$ET))
  {
    # This creates a NULL Table
    eventTable <- data.frame(
      Time = 0,
      Event = "CPBStart",
      Fill = "blue",
      stringsAsFactors = FALSE
    )
    eventTable <- eventTable[FALSE,]
    prior$ET <- eventTable
  }
  eventTable <- reactiveVal(
    prior$ET
  )

  cat ("Done with prior$ET\n")


  # Finishing Setup
  outputComments("Completing setup")
  plotObject <- NULL
  allResults <- NULL
  plotResults <- NULL
  PK_set <- FALSE

  # Initialize drug table (PK for each drug)
  drugs <- vector("list", length = nrow(drugDefaults))
  names(drugs) <- drugDefaults$Drug
  for (i in 1:length(drugs))
  {
    drugs[[i]]$Color <- drugDefaults$Color[i]
  }


  newDrugDefaultsFlag <- reactiveVal(FALSE)
  updatedDoseTableFlag <- reactiveVal(FALSE) # Forces a new plot

  cat("Setup Complete \n")


  # Get reference time from client
  output$getReferenceTime <- renderUI({
    outputComments("Getting reference time from client.")
    time <- input$client_time
    outputComments(paste("Reference time from client:", time))

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
    prior$referenceTime <<- start
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
      "editDrugsHTML_select",
      "editDrugs",
      "newEmerge",
      "hoverInfo"
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
    cat("***************************************************************************\n")
    cat("*                       Restoring Session from URL                        *\n")
    cat("***************************************************************************\n")
    prior$DT   <<- as.data.frame(state$values$DT, stringsAsFactors=FALSE)
    doseTable(prior$DT)
    current$DT <- prior$DT
    current$DT$Dose <- "" # Force an update
    cat("prior$DT after restoring state\n")
    print(prior$DT)

    prior$ET <<- as.data.frame(base64_dec(state$values$ET), stringsAsFactors=FALSE)
    cat("prior$ET after restoring state\n")
    print(prior$ET)
    eventTable <<- reactiveVal(prior$ET)

    # This next line should not be necessaruy because
    # doseTable and eventTable are both reactive
  })

  ##################################################################################
  # Note to Dean:                                                                  #
  # The code below is the validation loop that I would like to move into           #
  # JavaScript in the browser. The commente "dose table loop" is a note to self    #
  # explaining why the need to check the time and not process a dose for 3 seconds #
  # after the last update. Hopefully, all of that will go away.                    #
  # doseTableHTML is the handsontable                                              #
  ##################################################################################

  #########################################################################
  # Dose Table Loop                                                       #
  # Logic has been a challenge, because I want to check each entry for    #
  # validity. Part of the intent is to be very forgiving of typographical #
  # errors. Also, need to format the units for each drug individually.    #
  # Finally, there can be significant "bounce" between the UI and the     #
  # server, because the table gets written to the UI, and then loaded     #
  # from the UI.                                                          #
  #########################################################################
  observeEvent(
    {
      input$doseTableHTML
    },
    {
      outputComments("Starting dose Table Loop")
      outputComments(paste0("Is input$doseTableHTML NULL? ", is.null(input$doseTableHTML), "."))
      req(input$doseTableHTML)
      outputComments("Requirements for doseTable Loop are met.")

      # Note to Dean: The req() statement above is probably not necessary. I recently
      # changed this from observe() to observeEvent(), since it should only
      # be called when the doseTableHTML changes.

      # Get time to prevent continuous looping
      time <- as.numeric(Sys.time())
      delta <- time - prior$timeDT
      prior$timeDT <<- time
      # Note to Dean: The above code is for the timing, to prevent the endless loop
      # from occurring. This should not be necessary when it is all running within
      # the browser.

      # Set updateFlag to FALSE. Only update if the table has changed
      updateFlag <- FALSE

      # Note to Dean: I use updateFlag to determine if I need to rewrite the table
      # in the browser. If someone enters "25" as the dose, the validation will return
      # "25", so the browser is identical to the contents of current$DT. There is no
      # reason to update the browser with a new doseTableHTML. However, if someone
      # enters "25x" as the dose, the validation routine will strip off the x and return
      # "25". If that happens, then I want to update the browser to reflect that
      # the dose is 25. Thus, I set updateFlag to TRUE, and a new handsontable is
      # generated and sent to the browser.


      # Note to Dean: because of the issues with communication to/from the browser, I
      # created a "refresh" button to force a refresh to get things back in sync. My
      # hope is that by putting the handsontable validation entirely in JavaScript, I
      # can eliminate the refresh button entirely.

      # If refresh button has been pushed, erase prior dose table to refresh entirely from UI
      if (!is.null(input$Refresh) & input$Refresh != prior$Refresh)
      {
        outputComments("Forced Refresh of DoseTable")
        current$DT$Dose <- "" # Just set it to blank. It will be updated shortly
        updatedDoseTableFlag(TRUE)     # Gets set back to FALSE in simulationPlot, not here
        # Note to Dean: updatedDoseTableFlag is a reactive variable which will force
        # recalculation of the plots, based on an updated dosetable.
      } else {
        # Check for looping. Return if less than than 3
        if (delta < 3) # Note to Dean: Set this to 0 to let it enter the infinite update loop
        {
          outputComments(paste0("Exiting doseTable Loop to break bounce, delta = ",round(delta,2)))
          return()
        }
      }

      # This is the function that sometimes gets a doseTable that doesn't match the UI
      # Unclear how to fix this.
      # Note to Dean: If the doseTable is validated in JavaScript, then this command
      # should always return the current DT The problem has been that this
      # next command does not return the current DT if I've just pushed a new
      # table to the UI.

      DT <- hot_to_r(input$doseTableHTML)

      # Note to Dean: I want the dose table to always have a blank line at the end,
      # making it easy to add new doses. This next code checks to see if the last line
      # is blank. If it isn't blank, then a blank line is appended to the bottom of the
      # doseTable
      # Add a blank line if necessary
      if (nrow(DT) == 0 || DT$Drug[nrow(DT)] != "")
      {
        cat("Adding a line to doseTable\n")
        DT <- rbind(DT, doseTableNewRow)
        updateFlag <- TRUE
      }

      # Note to Dean: This is more debugging code.
      cat("Dose Table extracted from hot_to_r(input$doseTableHTML)\n")
      print(DT)

      cat("Current DT\n")
      print(current$DT)

      # Note to Dean:
      # October 17, 2019: sameTable replaced with isTRUE(all_equal())
      # Return if nothing has changed
      if (isTRUE(all_equal(DT, current$DT)))
      {
        outputComments("No change in doseTable. Exiting.")
        return()
      }

      # Note to Dean: I originally allowed the doseTable to shrink, based on the built in
      # "delete row" selection in handsontable. However, there is a problem in this routine.
      # If you delete several rows, it generates an error related to the row number. This
      # has been reported several times. It isn't clear if this is an error in handontable.js, or
      # in rHandsontable. However, because it isn't reliable, I turned off the abilityh to
      # remove rows from the handsontable. I would like to turn it back on if possible.
      # However, I didn't feel like tracking down the error.
      # If the doseTable shrinks, then that should be the only issue.

      # Step 1: Did doseTable shrink?
      if (nrow(DT) < nrow(current$DT))
      {
        outputComments("DoseTable shrunk. Nothing to do other than update DT")
        doseTable(DT)
        current$DT <- DT
        return()
      }

      # Note to Dean: I originally used sameTable to determine what had actually changed
      # However, it proved to be easier just to loop through everything and validate
      # every entry in the doseTable. These tables are never very big. In most cases it
      # is less than a dozen rows, with just four fields in each row. Thus, validating
      # every entry is easier than trying to figure out which entry changed and only
      # validating that entry.
      # October 17, 2019: sameTable replaced with isTRUE(all_equal())

      # Step 2: Dose table has changed. Loop through everything
      for (row in seq_len(nrow(DT)))
      {
        # Note to Dean: DT$Drug[row] is the name of the drug. If the name is
        # blank, then the rest of the row needs to be blank also.
        if (DT$Drug[row] == "")
        {
          if (DT$Time[row] != "" || DT$Dose[row] != "" || DT$Units[row] != "")
          {
            updateFlag <- TRUE
            DT$Time[row]  <- ""
            DT$Dose[row]  <- ""
            DT$Units[row] <- ""
          }
        } else {
          # Note to Dean: If the name is not blank, then I identify which row of the
          # drugDefaults corresponds to this drug. That information will be used
          # to assign the dose unit choices. Also, the select table for the dose
          # is assigned in createHOT. I've noted the code for you.

          thisDrug <- which (drugDefaults$Drug == DT$Drug[row])
          # Column 1: Drug
          # Note to Dean: if someone changes the drug, then I zero out everything
          # in the row. The reason is that every drug has unique dose units.
          # The dose of, say, propofol is completely different units  from the dose
          # of fentany.
          # The code below looks for a change in the drug name, and zeros out
          # the dose and changes the units if the name changes.
          if (DT$Drug[row] != current$DT$Drug[row])
          {
            cat("Row is: ", row, "\n")
            cat("DT\n")
            print(DT[row,])
            cat("current$DT\n")
            print(current$DT[row,])

            cat("Setting row ", row, "to initial zeros and default units\n")
            updateFlag <- TRUE
            DT$Time[row]  <- "0"
            DT$Dose[row]  <- "0"
            DT$Units[row] <- drugDefaults$Default.Units[thisDrug]
          }

          # Note to Dean: this is the time validation. The routine validateTime()
          # will need to be implemented in JavaScript, as well as in the Server.
          # Hopefully the results will be the same. You can find the routine in
          # validateTime.r. Note that I try to make sense of whatever the user
          # enters, rather than insist on a specific format.

          # Column 2: Time
          if (is.na(DT$Time[row])) DT$Time[row] <- ""
          x <- as.character(validateTime(DT$Time[row]))
          if (x != DT$Time[row])
          {
            updateFlag <- TRUE
            DT$Time[row] <- x
          }

          # Note to Dean: this is the Dose validation. The routine validateDose()
          # will need to be implemented in JavaScript, as well as in the Server.
          # Hopefully the results will be the same. You can find the routine in
          # validateTime.r. Note that I try to make sense of whatever the user
          # enters, rather than insist on a specific format.

          # Column 3: Dose
          if (is.na(DT$Dose[row])) DT$Dose[row] <- ""
          x <- as.character(validateDose(DT$Dose[row]))
          if (x != DT$Dose[row])
          {
            updateFlag <- TRUE
            DT$Dose[row] <- x
          }
        }
      }
      if (!isTRUE(all_equal(DT, current$DT))) updateFlag <- TRUE
      cat("\udpdateFlag at end of the loop: ", updateFlag, "\n")
      # Don't update table if nothing has changed
      # Note to Dean: As mentioned, I only update the table is something visible has
      # changed, such as the person entering "25x" and my changing that to just "25". If
      # there is no visible change, then I don't update the table.
      if (updateFlag)
      {
        doseTable(DT)
        current$DT <- DT
      }
      outputComments("Existing doseTable Loop")
    }
  )


  ####################################################################################
  ## Main Observation Loop
  observe({
    outputComments("Starting Main Observation Loop")
    outputComments(paste0("input$sex: ", input$sex,"."), echo = FALSE)
    outputComments(paste0("input$age: ", input$age,"."), echo = FALSE)
    outputComments(paste0("input$ageUnit: ", input$ageUnit,"."), echo = FALSE)
    outputComments(paste0("input$weight: ", input$weight,"."), echo = FALSE)
    outputComments(paste0("input$weightUnit: ", input$weightUnit,"."), echo = FALSE)
    outputComments(paste0("input$height: ", input$height,"."), echo = FALSE)
    outputComments(paste0("input$heightUnit: ", input$heightUnit,"."), echo = FALSE)
    outputComments(paste0("input$maximum: ", input$maximum,"."), echo = FALSE)
    outputComments(paste0("Is doseTableHTML NULL? ", is.null(input$doseTableHTML), "."), echo = FALSE)
    outputComments(paste0("input$referenceTime: ", input$referenceTime,"."), echo = FALSE)
    outputComments(paste0("Is input$referenceTime NULL? ", is.null(input$referenceTime), "."), echo = FALSE)

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
    outputComments("Main loop requirements are met.")
    if (
      is.numeric(input$age)             &&
      is.numeric(input$weight)          &&
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
        # write.xlsx(plotTable[,c("Drug","Lower","Upper","Typical","MEAC","tPeak")],file="data/PlotTable.xlsx")

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
    #######################################################################
    DT <- doseTable()   # Here is where the reactivity is forced
    print("doseTable() in simulation plot\n")
    print(doseTable())
    DT$Drug    <- as.character(DT$Drug)
    DT$Units   <- as.character(DT$Units)
    DT$Dose    <- as.numeric(DT$Dose)
    DT$Time    <- as.character(DT$Time)  # Stored as factors... Arrgh.....
    DT <- DT[DT$Drug != "" & !is.na(DT$Dose) & DT$Time != "" & DT$Units != "",]
    cat("Contents of DT in simulationPlot()\n")
    print(DT)
    cat("Contents of prior$DT in simulationPlot()\n")
    print(prior$DT)

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
      outputComments("Dose table is empty in main loop.")
      output$optionFlag <- renderText("")
      output$PlotSimulation <- renderPlot(nothingtoPlot)
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
      outputComments("Plotting because RefreshFlag requested")
      prior$RefreshFlag <<- FALSE
      replotFlag <- TRUE
    }
    if (recalculatePKFlag)
    {
      cat("plotting because recalculatePKFlag is TRUE\n")
      replotFlag <- TRUE
    }
    if (!isTRUE(all_equal(DT, prior$DT))) {
      outputComments("plotting becasue doseTable changed.")
      replotFlag <- TRUE
    } else {
      cat("doseTable remains unchanged in simulationPlot()\n")
    }
    if (!isTRUE(all_equal(ET, prior$ET))) {
      outputComments("plotting because eventTable changed.")
      replotFlag <- TRUE
    }

    if (plotMaximum != prior$plotMaximum) {
      cat("plotting because plotMaximum changed to ", plotMaximum, "\n")
      replotFlag <- TRUE
    }
    plotRecovery           <- "Time to Emergence"    %in% input$addedPlots
    if (plotRecovery != prior$plotRecovery)
    {
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

      # Setting prior$DT
      cat("Setting prior$DT in simulationPlot()\n")
      print(prior$DT)
      prior$DT  <<- DT
      prior$ET  <<- ET
      prior$plotMaximum <<- plotMaximum
    }
    if (length(input$plasmaLinetype) == 0 ||
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
      if (updatedDoseTableFlag() ) cat("New plot triggered new Dose Table\n")

      cat("\n")

      if (normalization != prior$normalization)
       {
        X <- setLinetypes(normalization)
        plasmaLinetype <- X$plasmaLinetype
        effectsiteLinetype <- X$effectsiteLinetype
      }
      if (caption == "")
      {
        prior$caption <<- ""
        caption <- paste0(
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
          updatedDoseTableFlag(FALSE)
        })

          if (is.null(plotObject))
          {
            output$optionFlag <- renderText("")
            cat("Null plot after calling simulation Plot()\n")
            return(nothingtoPlot)
          } else {
            output$optionFlag <- renderText("Graph Options")
            return(plotObject)
          }
        })
    } else {
        cat("Nothing flagged to make new plot\n\n")
      }
    } else {
      cat("Test for numeric covariates failed\n")
      cat("input$sex = ",input$sex,"\n")
      cat("input$ageUnit = ",input$ageUnit, is.numeric(input$ageUnit),typeof(input$ageUnit), "\n")
      cat("input$weightUnit = ",input$weightUnit,is.numeric(input$weightUnit),"\n")
      cat("input$heightUnit = ",input$heightUnit,is.numeric(input$heightUnit),"\n")
      return(NULL)
    }
    outputComments("Exiting main observation loop.")
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
    prior$plasmaLinetype <<- X$plasmaLinetype
    prior$effectsiteLinetype <<- X$effectsiteLinetype
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
      # output$EmailButton <- renderUI({
      #       p("Processing slide")
      # })
      cat("input$sendSlide",input$sendSlide,'\n')
      img <- sendSlide(
        prior = prior,
        recipient = input$recipient,
        plotObject = plotObject,
        allResults = allResults,
        plotResults = plotResults,
        isShinyLocal = isShinyLocal,
        slide = as.numeric(input$sendSlide),
        drugs
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
    cat("hovering\n")
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
  if(is.null(e)) return(NULL)
  if(is.null(e$panelvar1)) return(NULL)
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
    returnText <- paste0(returnText,", Emergence in ",round(drugs[[i]]$equiSpace$Recovery[j], 1), " minutes")
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
  if (is.null(e$x) || is.null(e) || length(plottedDrugs) == 0)
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

  if(is.null(e) || length(plottedDrugs) == 0)
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
  emerge <- drugDefaults$Emerge[thisDrug]
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
    numericInput(
      inputId = "newEmerge",
      label = "Set emergence concentration",
      value = emerge,
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
      if (drugDefaults$Emerge[thisDrug] != input$newEmerge)
      {
        drugDefaults$Emerge[thisDrug] <<- input$newEmerge
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

# Note to Dean: The event below allows users to edit previous doses. This is yet another way
# in which the table can be changed. I don't think this needs to be
# edited, but it explains why there will always be some updating of the doseTable from the
# server.

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
    TT$Drug <- prior$DrugTimeUnits$drug
    current$DT <- rbind(
      current$DT[!TT$Delete,c("Drug","Time","Dose","Units")],
      current$DT[current$DT$Drug != prior$DrugTimeUnits$drug,]
    )
    for (i in seq_len(nrow(doseTable())))
    {
      current$DT$Time[i] <- validateTime(current$DT$Time[i])
      current$DT$Dose[i] <- validateDose(current$DT$Dose[i]) # should work for target too
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


############################### Double Click Response ################################
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

# Note to Dean: The program can also calculate the dose necessary to reach a given
# target concentration. This is yet another way the server may update the dose table.
# This does not need to change.

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
      ce <- simCpCe(DT, ET, PK, endTime, plotRecovery = FALSE)$equiSpace[,c("Time","Ce")]
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
    results <- simCpCe(testTable,ET, drugs[[drug]],endTime, plotRecovery = FALSE)$equiSpace[,c("Time","Ce")]
    # plot <- ggplot(results,aes(x=Time, y=Ce)) +
    #   geom_line() +
    #   labs(title="After nlm")
    # print(plot)

    testTable$Drug <- input$targetDrug
    testTable$Time <- testTable$Time + offsetTime
    # print(str(testTable))

    current$DT <- current$DT[current$DT$Drug != input$targetDrug,]
    # print(str(doseTable))

    current$DT <- rbind(testTable[,c("Drug","Time","Dose","Units")], current$DT)
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
  })


  outputComments("Reached the end of server()")
}
