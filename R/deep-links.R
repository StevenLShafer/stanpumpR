# -----------------------------------------------------------------------------
# Links that open the app on a teaching scenario or a help page
# -----------------------------------------------------------------------------
# Provenance
# ----------
# Drafted by Claude Code (Claude Fable 5.1), 2026-10-14, at the request of
# Steven L. Shafer, for a lecture that steps between slides and live
# simulations.  Verified by tests/testthat/test-deep-links.R.
#
# The address bar is the simulation (inst/help/sharing.md): every change is
# bookmarked into a long URL that restores the whole state.  The scenarios and
# the help pages had no address of their own, so a slide, a syllabus or an
# email could not point at one.  Two query parameters give them one:
#
#   <app>/?scenario=<id>       load that teaching scenario (R/help-scenarios.R)
#                              into the simulator, skipping the startup drug
#                              menu; the plot opens on the scenario
#   <app>/?help=<page-id>      open the Help tab on that page, with the default
#                              drugs in the simulator behind it (as "Take the
#                              tour" does); ?help=scenarios/<id> is a
#                              scenario's own page, with its Load button
#   both together              load the scenario and open on the page
#
# The link is read once, as the session starts, and the first change rewrites
# the address bar into an ordinary bookmark, so a reload keeps what was loaded.
# A bookmark (a URL carrying a dose table) is the saved state of a session, so
# it wins over ?scenario=: the parameter is dropped.  ?debug= is unaffected.
#
# The ids come from the browser, so they are checked against the scenario list
# and the page registry before anything is loaded; an unknown scenario is a
# warning and an ordinary start, an unknown page opens the help's not-found page.
# -----------------------------------------------------------------------------

DEEP_LINK_MAX_LENGTH <- 200

#' One query parameter, if it is a usable single string
#' @noRd
deepLinkParam <- function(query, name) {
  value <- query[[name]]
  if (!is.character(value) || length(value) != 1 || is.na(value)) return(NULL)
  value <- trimws(value)
  if (!nzchar(value) || nchar(value) > DEEP_LINK_MAX_LENGTH) return(NULL)
  value
}

#' What a URL asks the app to open on
#'
#' @param urlSearch the query string, `session$clientData$url_search`
#'   ("?scenario=propofol-bolus"), or NULL
#' @param drugDefaults the drug library the scenario is checked against
#' @param registry the help page registry
#' @returns a list: `scenario`, the scenario (as `helpScenarioById()` gives it)
#'   or NULL; `help`, a page id or NULL ("not-found" for a page that does not
#'   exist); `problems`, messages to show the person
#' @noRd
deepLinkFromQuery <- function(urlSearch, drugDefaults = getDrugDefaultsGlobal(),
                              registry = helpPageRegistry(drugDefaults)) {
  link <- list(scenario = NULL, help = NULL, problems = character(0))
  if (is.null(urlSearch) || !is.character(urlSearch) || length(urlSearch) != 1 ||
      is.na(urlSearch) || !nzchar(urlSearch)) {
    return(link)
  }
  query <- parseQueryString(urlSearch)

  id <- deepLinkParam(query, "scenario")
  if (!is.null(id)) {
    s <- helpScenarioById(id)
    if (is.null(s)) {
      link$problems <- c(link$problems,
                         sprintf("There is no teaching scenario called \"%s\".", id))
    } else {
      problems <- helpScenarioCheck(s, drugDefaults)
      if (length(problems) > 0) {
        link$problems <- c(link$problems,
                           sprintf("The scenario \"%s\" cannot be loaded with this drug library: %s",
                                   id, problems[1]))
      } else {
        link$scenario <- s
      }
    }
  }

  page <- deepLinkParam(query, "help")
  if (!is.null(page)) {
    link$help <- if (helpPageExists(page, registry)) page else "not-found"
  }
  link
}

#' Does the link open the app on something, rather than on the startup menu?
#' @noRd
deepLinkOpens <- function(link) {
  !is.null(link$scenario) || !is.null(link$help)
}

#' The relative link that opens the app on a scenario
#'
#' Resolved by the browser against the app's own address, so the same text
#' works wherever the app is served; right-click, copy link gives the full URL.
#' @noRd
deepLinkToScenario <- function(id) {
  paste0("?scenario=", utils::URLencode(id, reserved = TRUE))
}
