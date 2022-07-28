library(shiny)
library(ggplot2)

options(warn = 2)

isShinyLocal <- Sys.getenv('SHINY_PORT') == ""

config <- config::get()

# Load other files
#CANCEL <- readPNG("www/cancel.png", native=TRUE)
enableBookmarking(store = "url")

# Setup Theme
theme_update(
  panel.background = element_rect(fill = "white", color = "white"),
  legend.box.background = element_rect(fill = "white", color = "white"),
  panel.grid.major.y = element_line(color = "lightgrey"),
  panel.grid.major.x = element_line(color = "lightgrey"),
  axis.ticks = element_line(color = "lightgrey"),
  axis.ticks.length = unit(.25, "cm"),
  axis.title = element_text(size = rel(1.5)),
  axis.text = element_text(size = rel(1.2)),
  axis.line = element_line(size = 1, color = "black"),
  aspect.ratio = 0.6,
  plot.title = element_text(size = rel(1.5)),
  legend.text = element_text(size = rel(0.9)),
  legend.position = "right",
  legend.key = element_blank()
)

blanks <- rep("", 6)
doseTableInit <- data.frame(
  Drug = c("propofol","fentanyl","remifentanil","rocuronium", blanks),
  Time = c(as.character(rep(0,4)), blanks),
  Dose = c(as.character(rep(0,4)), blanks),
  Units = c("mg","mcg","mcg/kg/min","mg", blanks),
  stringsAsFactors = FALSE
)
doseTableNewRow <-  doseTableInit[5, ]

eventTableInit <- data.frame(
  Time = 0,
  Event = "",
  Fill = "",
  stringsAsFactors = FALSE
)[FALSE, ]

outputComments <- function(
  ...,
  echo = getOption("ECHO_OUTPUT_COMMENTS", TRUE),
  sep = " ")
{
  isolate({
    argslist <- list(...)
    if (length(argslist) == 1) {
      text <- argslist[[1]]
    } else {
      text <- paste(argslist, collapse = sep)
    }

    # If this is called within a shiny app, try to get the active session
    # and write to the session's logger
    commentsLog <- function(x) invisible(NULL)
    session <- getDefaultReactiveDomain()
    if (!is.null(session) &&
        is.environment(session$userData) &&
        is.reactive(session$userData$commentsLog))
    {
      commentsLog <- session$userData$commentsLog
    }

    if (is.na(echo)) return()
    if (is.data.frame((text)))
    {
      con <- textConnection("outputString","w",local=TRUE)
      capture.output(print(text, digits = 3), file = con, type="output", split = FALSE)
      close(con)
      if (echo)
      {
        for (line in outputString) cat(line, "\n")
      }
      for (line in outputString) commentsLog(paste0(commentsLog(), "<br>", line))
    } else {
      if (echo)
      {
        cat(text, "\n")
      }
      commentsLog(paste0(commentsLog(), "<br>", text))
    }
  })
}

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
  "editPriorDosesTable_select",
  "editPriorDosesTable",
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
