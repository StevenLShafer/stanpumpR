remove(list=ls())
setwd("c:/google drive/Projects/stanpumpR")
library(rhandsontable)
library(shiny)
library(DT)
library(openxlsx)
drugDefaults <- read.xlsx("Drug Defaults.xlsx")
drugList <- drugDefaults$Drug

colorList <-  c("black","red","green","blue", "orange","brown","grey", "white")

doseTable <- data.frame(
  Drug = "",
  Color = "",
  Minute = rep(0,6),
  Bolus = FALSE,
  Dose = 0,
  Units = ""
)
doseTableNewRow <- doseTable[1,]

bolusUnits <- c("mg","mcg")
infusionUnits <- c("mg/min","mg/hr","mg/kg/min","mg/kg/hr","mcg/min","mcg/hr","mcg/kg/min","mcg/kg/hr")

units <- c(bolusUnits, infusionUnits)


ui <- shinyUI(
  fluidPage(
      mainPanel(
        rHandsontableOutput(outputId = "doseTableHTML") # doseTableHTML is the name of the outputId and the inputId
      )
    )
  )

server <- shinyServer(
  function(input, output)
  {

    values <- reactiveValues()

    #############################################
    # source("processdoseTable.R")
    ## Dose Table
    observe({
      if (!is.null(input$doseTableHTML)) {
        doseTable <<- hot_to_r(input$doseTableHTML)
      }

      doseTable$Drug <- as.character(doseTable$Drug)
      doseTable$Color <- as.character(doseTable$Color)
      doseTable$Units <- as.character(doseTable$Units)
      lastRow <- nrow(doseTable)
      if (doseTable$Drug[lastRow]!="")
        doseTable <- rbind(doseTable,doseTableNewRow)


      # Just reset bad units to blanks
      doseTable$Units[doseTable$Drug == ""] <- ""
      doseTable$Units[doseTable$Drug != "" &
                        !doseTable$Bolus &
                        !grepl("/",doseTable$Units)] <- ""
      doseTable$Units[doseTable$Drug != "" &
                        doseTable$Bolus &
                        grepl("/",doseTable$Units)] <- ""
      # Now fix all the blanks, using the drug defaults
      FIX <- doseTable$Drug != "" & doseTable$Units == ""
      CROWS <- match(doseTable$Drug,drugDefaults$Drug)
      cat("CROWS",CROWS,"\n")
      doseTable$Units[FIX & doseTable$Bolus] <-
        drugDefaults$Bolus.Units[CROWS[FIX & doseTable$Bolus]]
      cat("Bolus Column", !doseTable$Bolus,"\n")
      doseTable$Units[FIX & !doseTable$Bolus] <-
        drugDefaults$Infusion.Units[CROWS[FIX & !doseTable$Bolus]]

      for (i in 1:nrow(drugDefaults))
      {
        USE <- doseTable$Drug == drugDefaults$Drug[i]
        if (sum(USE) > 0)
        {
          #         cat("procesing",drugDefaults$Drug[i],"\n")
          # Process color column
          colors <- doseTable$Color[USE]
          if (length(colors) == 1 & colors[1] == "")
            colors <- colorList[which(!colorList %in% doseTable$Color)[1]]
          colors <- colors[colors != ""]
          colorTable <- as.data.frame(ftable(colors),stringsAsFactors = FALSE)
          colorTable$Freq <- as.numeric(colorTable$Freq)
          colorTable$colors <- as.character(colorTable$colors)
          replacementColor <-
            colorTable$colors[which(colorTable$Freq == min(colorTable$Freq))]
          if (length(replacementColor) > 1)
            replacementColor <-
            doseTable$Color[tail(which(doseTable$Color %in% replacementColor),1)]
          doseTable$Color[USE] <- replacementColor
        }
      }

      # Now sort the the drugs by drug, by The last line needs to remain the
      #CROWS <- match(doseTable$Drug, c(drugList,""))
      #doseTable <- doseTable[order(CROWS, doseTable$Minute,doseTable$Bolus),]

      # cat("Selected", str(input$doseTableHTML_select),"\n") # returns the entire table


      HOT <- rhandsontable(
        doseTable,
        rowHeaders = NULL,
        width = 550,
        height = 300,
        selectCallback = TRUE
      ) %>%
        hot_col(
          col = "Drug",
          type = "dropdown",
          source = drugList,
          strict = TRUE,
          halign = "htLeft",
          valign = "vtMiddle",
          allowInvalid=FALSE
        ) %>%
        hot_col(
          col = "Color",
          type = "dropdown",
          source = colorList,
          strict = TRUE,
          halign = "htLeft",
          valign = "htMiddle",
          allowInvalid = FALSE
        ) %>%
        hot_col(
          col = "Minute",
          type="numeric",
          format="0",
          halign = "htRight",
          #          valign = "htTop",
          allowInvalid = FALSE
        ) %>%
        hot_col(
          col = "Bolus",
          type = "checkbox",
          halign = "htRight",
          valign = "htMiddle",
          allowInvalid = FALSE
        ) %>%
        hot_col(
          col = "Dose",
          type = "numeric",
          format = "0.0",
          halign = "htRight",
          # valign = "htTop",
          allowInvalid = FALSE
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
        #        hot_validate_numeric(cols = c("Minute"), min = 0, max = 1440) %>%
        hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) %>%
        hot_rows(rowHeights = 10) %>%
        hot_cols(colWidths = c(140,70,60,50,50,100))
      HOT$sizingPolicy$viewer$padding <- 20 # no effect
      HOT$sizingPolicy$browser$padding <- 20 # no effect
      HOT$sizingPolicy$viewer$defaultHeight <- 80 # no effect

      #            cat(str(HOT))

      output$doseTableHTML <- renderRHandsontable({HOT})

      values[["doseTable"]] <- doseTable
    })

    #########################################################
  }
)
  ## run app
  runApp(list(ui=ui, server=server))
