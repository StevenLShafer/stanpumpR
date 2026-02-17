blanks <- rep("", 6)
doseTableInit <- data.frame(
  Drug = c("propofol","propofol","fentanyl","remifentanil","remifentanil","rocuronium", blanks),
  Time = c(as.character(rep(0,6)), blanks),
  Dose = c(as.character(rep(0,6)), blanks),
  Units = c("mg","mcg/kg/min","mcg","mcg","mcg/kg/min","mg", blanks)
)
doseTableNewRow <-  doseTableInit[7, ]

eventTableInit <- data.frame(
  Time = numeric(0),
  Event = character(0),
  Fill = character(0)
)

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
  "hoverInfo",
  "debug_level",
  "show_intro_modal",
  "client_time"
)

outputComments <- function(
    ...,
    level = DEBUG_LEVEL_NORMAL,
    echo = getOption("ECHO_OUTPUT_COMMENTS", TRUE),
    sep = " ")
{
  isolate({
    session <- getDefaultReactiveDomain()

    if (!is.null(session) &&
        is.environment(session$userData) &&
        is.reactive(session$userData$debug) &&
        session$userData$debug() < level) {
      return()
    }

    argslist <- list(...)
    if (length(argslist) == 1) {
      text <- argslist[[1]]
    } else {
      text <- paste(argslist, collapse = sep)
    }

    # If this is called within a shiny app, try to get the active session
    # and write to the session's logger
    commentsLog <- function(x) invisible(NULL)
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
      for (line in outputString) commentsLog(paste0(commentsLog(), "\n", line))
    } else {
      if (echo)
      {
        cat(text, "\n")
      }
      commentsLog(paste0(commentsLog(), "\n", text))
    }
  })
}
