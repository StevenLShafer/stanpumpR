# -----------------------------------------------------------------------------
# The Help tab: server side
# -----------------------------------------------------------------------------
# Provenance
# ----------
# Drafted by Claude Code (Claude Fable 5.1), 2026-10-06, at the request of
# Steven L. Shafer.
#
# Called once from app_server().  Three inputs arrive from the browser, all set
# by inst/www/app.js:
#
#   input$help_goto           a page id; clicked on any [data-help-page] link
#   input$help_scenario_load  a scenario id; clicked on any [data-help-scenario]
#   input$help_search         the search box
#   input$investigator_show   an investigator key; clicked on any
#                             [data-investigator] link, in the help or in the
#                             References panel (R/help-investigators.R)
#
# The page ids come from the browser, so they are checked against the
# registry before anything is read from disk.
# -----------------------------------------------------------------------------

#' Wire up the Help tab
#'
#' @param input,output,session the Shiny server arguments
#' @param doseTable,eventTable the app's reactiveVal()s, written to when a
#'   scenario is loaded
#' @param drugDefaults the reactiveVal() holding the (editable) drug library;
#'   the drug pages describe the library as the session currently has it
#' @param navId id of the page_navbar(), to switch tabs
#' @param timeApi the app_server()'s time-units functions (setDoseTable,
#'   showTimeSettings), through which a scenario sets its time unit and writes
#'   its dose table; NULL writes the tables directly
#' @param onLoad NULL, or a function called as a scenario is loaded, before
#'   it is put in, for what else belongs to the patient it replaces
#'   (app_server() clears the measured concentrations of the Bayesian update)
#' @param restoreMeasurements function(measurements, format) that puts a
#'   scenario's own measured concentrations into the Bayesian Updating table
#'   (applyHelpScenario()), or NULL
#' @returns a list: `page`, the reactiveVal() holding the current page id, and
#'   `loadScenario(s)`, which loads a scenario into the simulator (used for a
#'   ?scenario= link, R/deep-links.R)
#' @noRd
helpServer <- function(input, output, session, doseTable, eventTable, drugDefaults,
                       navId = "mainNav", timeApi = NULL, onLoad = NULL,
                       restoreMeasurements = NULL) {
  helpPage <- reactiveVal("home")

  registry <- reactive({
    helpPageRegistry(drugDefaults())
  })

  observeEvent(input$help_goto, {
    page <- input$help_goto
    if (!is.character(page) || length(page) != 1 || is.na(page)) return()
    if (nchar(page) > 200) return()
    if (!helpPageExists(page, registry())) page <- "not-found"
    helpPage(page)
    bslib::nav_select(navId, "Help", session = session)
  })

  output$help_nav <- renderUI({
    helpSidebarNav(registry(), helpPage())
  })

  output$help_content <- renderUI({
    outputComments("Rendering help page", helpPage(), level = DEBUG_LEVEL_VERBOSE)
    helpPageUI(helpPage(), drugDefaults())
  })

  # One string of at most the box's maxlength: a vector sent for it put a
  # condition of length 2 in the if() below, which printed R's error in the
  # results, and a megabyte string was searched for across every page each
  # time it changed (Carbon's third UI audit, 2026-11-15, M5)
  searchTerm <- debounce(reactive({
    term <- trimws(asScalarString(input$help_search))
    substr(term, 1L, HELP_SEARCH_MAX_CHARS)
  }), 300)

  output$help_search_results <- renderUI({
    term <- trimws(searchTerm())
    if (nchar(term) < 2) return(NULL)
    helpSearchResultsUI(term, helpSearch(term))
  })

  # Load a scenario into the simulator: from the Load buttons, and from a
  # ?scenario= link as the session starts (R/deep-links.R).  Returns TRUE when
  # the scenario was loaded.
  loadScenario <- function(s) {
    if (is.null(s)) {
      showNotification("That scenario does not exist.", type = "error")
      return(FALSE)
    }
    problems <- helpScenarioCheck(s, drugDefaults())
    if (length(problems) > 0) {
      # A scenario the tests accept can still clash with a drug library the
      # user has edited in this session, e.g. a unit removed.
      outputComments("Scenario", s$id, "cannot be loaded:", paste(problems, collapse = "; "))
      showNotification(
        paste("This scenario cannot be loaded with the current drug library:",
              problems[1]),
        type = "error", duration = 10
      )
      return(FALSE)
    }
    s <- helpScenarioInSessionUnits(s, drugDefaults())
    if (is.null(s)) {
      showNotification(
        "This scenario cannot be loaded with the current drug library: a target or measured concentration in its units would pass 1,000,000,000.",
        type = "error", duration = 10)
      return(FALSE)
    }
    outputComments("Loading help scenario", s$id)
    # What else belonged to the patient being replaced goes first, so that the
    # scenario's own measured concentrations and variabilities come after it
    if (is.function(onLoad)) onLoad()
    applyHelpScenario(session, s, doseTable, eventTable, timeApi, restoreMeasurements)
    bslib::nav_select(navId, "Simulator", session = session)
    converted <- if (length(s$converted) > 0) tagList(tags$br(), tags$span(class = "small", paste0(
      "Targets and measured concentrations converted to the units your Drug Library shows (",
      paste(s$converted, collapse = ", "), ").")))
    showNotification(
      tagList(tags$strong("Loaded: "), s$title, tags$br(),
              tags$span(class = "small", "Press Apply Changes after editing the dose table to see your own variations."),
              converted),
      type = "message", duration = 8
    )
    TRUE
  }

  observeEvent(input$help_scenario_load, {
    loadScenario(helpScenarioById(input$help_scenario_load))
  })

  observeEvent(input$investigator_show, {
    modal <- helpInvestigatorModal(input$investigator_show)
    if (is.null(modal)) return()
    outputComments("Showing investigator", input$investigator_show, level = DEBUG_LEVEL_VERBOSE)
    showModal(modal)
  })

  invisible(list(page = helpPage, loadScenario = loadScenario))
}
