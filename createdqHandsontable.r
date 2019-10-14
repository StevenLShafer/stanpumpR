# Create the handsontable that has the dosing information
library(shiny)
library(openxlsx)
library(dqshiny)
library(shinyjs)
library(shinydashboard)
library(shinyBS)


setwd("g:/projects/stanpumpr")

drugDefaults <-read.xlsx("Drug Defaults.xlsx")
drugDefaults$Units <- strsplit(drugDefaults$Units, ",")

units <- c("1","2","3","4","5")
doseTable <- data.frame(
  Drug = c("propofol","fentanyl","remifentanil","rocuronium",""),
  Time = c(as.character(rep(0,4)), ""),
  Dose = c(as.character(rep(0,4)), ""),
  Units = c("mg","mcg","mcg/kg/min","mg",""),
  stringsAsFactors = FALSE
)


createdqHandsontable <- function(doseTable,drugDefaults)
{
  dq_render_handsontable(
    "doseTableHTML", 
    doseTable,
    # filters = c(
    #   "Auto", 
    #   "Text", 
    #   "Text", 
    #   "Auto"
    # ), 
    sorting = FALSE,
    horizontal_scroll = FALSE,
    page_size = NULL,
    # c(
    #   17L, 
    #   5L, 
    #   500L, 
    #   1000L
    # ),
    table_param(
#      overflow = 'visible',
      rRowHeaders = NULL,
      selectCallback = FALSE
    ),
    col_param = list(
       list(
         col = "Drug",
         type = "dropdown",
         source = drugDefaults$Drug,
         strict = TRUE,
         halign = "htLeft",
         valign = "vtMiddle",
         allowInvalid=FALSE
       ),
       list(
         col = "Time",
         type="autocomplete",
         halign = "htRight",
         allowInvalid = TRUE,
         strict = FALSE
       ),
       list(
         col = "Dose",
         type = "autocomplete",
         halign = "htRight",
         allowInvalid = TRUE,
         strict = FALSE
         ),
       list(
         col = "Units",
         type = "dropdown",
         source = units,
         strict = TRUE,
         halign = "htLeft",
         valign = "vtMiddle",
         allowInvalid=FALSE
       )   
    )
  )
}


shinyApp(
  ui = fluidPage(dq_handsontable_output("doseTableHTML", 9L)),
  server = function(input, output, session) {
    createdqHandsontable(doseTable,drugDefaults)
  }
)




