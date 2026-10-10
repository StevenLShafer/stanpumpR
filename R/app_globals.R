blanks <- rep("", 6)
# The dose table the app opened with before the startup drug menu, and what
# the menu still gives when Start is pressed with its defaults ticked
# (startupDoseTable(STARTUP_DRUGS_DEFAULT); test-startup-drugs.R holds them
# equal).
doseTableInit <- data.frame(
  Drug = c("propofol","propofol","fentanyl","remifentanil","remifentanil","rocuronium", blanks),
  Time = c(as.character(rep(0,6)), blanks),
  Dose = c(as.character(rep(0,6)), blanks),
  Units = c("mg","mcg/kg/min","mcg","mcg","mcg/kg/min","mg", blanks)
)
doseTableNewRow <-  doseTableInit[7, ]
# The dose table a session holds until the startup menu fills it
doseTableBlank <- doseTableInit[7:12, ]
rownames(doseTableBlank) <- NULL

eventTableInit <- data.frame(
  Time = numeric(0),
  Event = character(0)
)

bookmarksToExclude <- c(
  "editThresholds",
  "plotContainer_full_screen",
  "thresholdEditsOK",
  "editThresholdsTable",
  "showThresholdModal",
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
  "maximum-selectized",
  "timeUnits-selectized",
  "showLongTermTime",
  "targetTableHTML",
  "editPriorDosesTable",
  "addDoseDrug",
  "addDoseAmount",
  "clickEvent",
  "clickEvent-selectized",
  "addDoseBtn",
  "addEventBtn",
  "addDoseTime",
  "clickTimeEvent",
  "addDoseUnits",
  "deleteAllDosesBtn",
  "confirmDeleteAllDoses",
  "deleteAllEventsBtn",
  "confirmDeleteAllEvents",
  "confirmRemoveMEAC",
  "confirmRemoveInteraction",
  "editDosesOK",
  "editEvents",
  "editEventsOK",
  "sendSlide",
  "recipient",
  "emailComments",
  "commentSafe",
  "drugEditsOK",
  "editDrugsHTML",
  "editDrugs",
  "hoverInfo",
  "debug_level",
  "profiler_threshold",
  "plotWidth",
  "show_intro_modal",
  # The startup drug menu (R/startup-drugs.R): what it chose is in the dose
  # table.  One checkbox group per entry in DRUG_CATEGORIES, which loads after
  # this file; test-startup-drugs.R checks that every one is listed here.
  "startup_ok",
  "startup_tour",
  "startupDrugs_1", "startupDrugs_2", "startupDrugs_3", "startupDrugs_4",
  "startupDrugs_5", "startupDrugs_6", "startupDrugs_7", "startupDrugs_8",
  "startupDrugs_9", "startupDrugs_10", "startupDrugs_11",
  "startupDrugs_12",
  "client_time",
  "dosetable_apply",
  "dosetable_undo",
  "dosetable_redo",
  "debug_area",
  "HandsontableCopyPaste",
  "shinyalert",
  # The Help tab (R/help-server.R): which tab and page are open is not part of
  # a simulation
  "mainNav",
  "help_goto",
  "help_search",
  "help_scenario_load"
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
      utils::capture.output(print(text, digits = 3), file = con, type="output", split = FALSE)
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
