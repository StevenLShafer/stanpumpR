# Load Libraries
source(file.path("R", "global", "packages.R"), local = TRUE)
source(file.path("R", "globalVariables.R"), local = TRUE)

options(warn = 2)

isShinyLocal <- Sys.getenv('SHINY_PORT') == ""
# cat("isShinyLocal",isShinyLocal,"\n")

source("R/havingIP.R", local = TRUE)

if (!isShinyLocal) {
  Sys.setenv(R_CONFIG_ACTIVE = "production")  # Running on Shinyapps
  internetConnected <- TRUE
} else {
  Sys.unsetenv("R_CONFIG_ACTIVE") # Running on laptop
  internetConnected <- FALSE
  x <- system.time({ internetConnected <- checkConnection() })
  cat("Time to determine if it has an internet connection\n")
  print(x)
  cat("\n")

  # tell shiny to log all reactivity
  library(reactlog)
  options(shiny.reactlog = TRUE)

  appFiles <- dir()
  appFiles <- appFiles[grepl("\\.", appFiles)]
  appFiles <- c(appFiles, "R","data","www", "misc")
  source("scripts/deployActive.R")
  source("scripts/deployTest.R")
}
config <- config::get()

# Load stanpumpR routines
for (file in list.files("R", pattern = "\\.R$")) {
  source(file.path("R", file), local = TRUE)
}

# Load other files
#CANCEL <- readPNG("www/cancel.png", native=TRUE)
enableBookmarking(store = "url")

eventDefaults <- read.csv("data/Event Defaults.csv", stringsAsFactors = FALSE)

drugDefaults_global <- read.csv("data/Drug Defaults.csv", stringsAsFactors = FALSE, na.strings = "")

# Load individual drug routines
for (drug in drugDefaults_global$Drug) {
  source(file.path("data", "drugs", paste0(drug, ".R")), local = TRUE)
}

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


introductionPlot <- staticPlot("Initializing your session.")

nothingtoPlot <- staticPlot(
  paste(
    "Welcome to stanpumpR.",
    "",
    "Please enter the drugs in the table to the left.",
    "Use the pull down menu to select each drug.",
    "Drugs and doses can be entered in any order",
    "Set the units in the last column.",
    "",
    "After plots appear here, you can enter new doses",
    "by clicking on the plot. You can enter new drugs",
    "by double clicking on any plot.",
    sep = "\n"
  )
)

drugUnitsExpand <- function(units) {
  strsplit(units, ",")
}
drugUnitsSimplify <- function(units) {
  unlist(lapply(units, paste, collapse = ","))
}
drugDefaults_global$Units <- drugUnitsExpand(drugDefaults_global$Units)

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

`%then%` <- shiny:::`%OR%`

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
