remove(list=ls())

library(rhandsontable)
library(shiny)
library(DT)

doseTable = data.frame(val = 1:10, bool = TRUE, big = LETTERS[1:10],
                small = letters[1:10],
                dt = seq(from = Sys.Date(), by = "days", length.out = 10),
                stringsAsFactors = FALSE)


rhandsontable(DF, rowHeaders = NULL, width = 550, height = 300) %>%
  hot_col(col = "big", type = "dropdown", source = LETTERS) %>%
  hot_col(col = "small", type = "autocomplete", source = letters,
          strict = FALSE)

# try updating big to a value not in the dropdown


doseTable <- vector("list",3)
for (i in 1:3)
{
  doseTable[[i]] <- data.frame(Minutes = rep(i,3), Bolus = FALSE, Dose=0, Units = "mcg/kg/min",
                               stringsAsFactors = FALSE)
}

ui <- shinyUI(
  fluidPage(
      mainPanel(
        uiOutput("doseTables")
        # rHandsontableOutput(outputId = paste0("doseTableHTML",1)) # doseTableHTML is the name of the outputId and the inputId
      )
    )
  )

server <- shinyServer(
  function(input, output)
  {

    values <- reactiveValues()

    ## Dose Table
    observe({
      if (!is.null(input$doseTableHTML1)) {
        doseTable[[1]] <<- hot_to_r(input$doseTableHTML1)
      }
    output$doseTables <- list()
    output$doseTable[[1]] <- renderRHandsontable({
      rhandsontable(doseTable[[1]], useTypes = TRUE)
    })

    output$doseTable[[2]] <- renderRHandsontable({
        rhandsontable(doseTable[[2]], useTypes = TRUE)
    })

    output$doseTable[[3]] <- renderRHandsontable({
        rhandsontable(doseTable[[3]], useTypes = TRUE)
    })
    cat("first Dose",doseTable[[i]]$Dose[i],"\n")
    })
  }
)

  ## run app
  runApp(list(ui=ui, server=server))
