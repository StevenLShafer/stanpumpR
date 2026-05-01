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
  "editThresholds",
  "plotContainer_full_screen",
  "thresholdEditsOK",
  "editThresholdsTable",
  "editEventsTableHTML",
  "doseTableHTML",
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
  "editPriorDosesTable",
  "addDoseDrug",
  "addDoseAmount",
  "clickEvent",
  "clickEvent-selectized",
  "addDoseBtn",
  "clickOKEvent",
  "addDoseTime",
  "clickTimeEvent",
  "addDoseUnits",
  "deleteAllDosesBtn",
  "confirmDeleteAllDoses",
  "editDosesOK",
  "editEvents",
  "editEventsOK",
  "sendSlide",
  "recipient",
  "drugEditsOK",
  "editDrugsHTML",
  "editDrugs",
  "hoverInfo",
  "debug_level",
  "profiler_threshold",
  "plotWidth",
  "show_intro_modal",
  "client_time",
  "dosetable_apply",
  "dosetable_undo",
  "dosetable_redo",
  "debug_area",
  "HandsontableCopyPaste",
  "shinyalert"
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
