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
#' @noRd
helpServer <- function(input, output, session, doseTable, eventTable, drugDefaults,
                       navId = "mainNav", timeApi = NULL) {
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

  searchTerm <- debounce(reactive(input$help_search), 300)

  output$help_search_results <- renderUI({
    term <- searchTerm()
    if (is.null(term) || nchar(trimws(term)) < 2) return(NULL)
    helpSearchResultsUI(trimws(term), helpSearch(trimws(term)))
  })

  observeEvent(input$help_scenario_load, {
    s <- helpScenarioById(input$help_scenario_load)
    if (is.null(s)) {
      showNotification("That scenario does not exist.", type = "error")
      return()
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
      return()
    }
    outputComments("Loading help scenario", s$id)
    applyHelpScenario(session, s, doseTable, eventTable, timeApi)
    bslib::nav_select(navId, "Simulator", session = session)
    showNotification(
      tagList(tags$strong("Loaded: "), s$title, tags$br(),
              tags$span(class = "small", "Press Apply Changes after editing the dose table to see your own variations.")),
      type = "message", duration = 8
    )
  })

  invisible(helpPage)
}
