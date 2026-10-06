# -----------------------------------------------------------------------------
# The Help tab
# -----------------------------------------------------------------------------
# Provenance
# ----------
# Drafted by Claude Code (Claude Fable 5.1), 2026-10-06, at the request of
# Steven L. Shafer.  See R/help-content.R for how the pages are built and
# R/help-server.R for the reactive side.
#
# Layout: a sidebar with a search box and the table of contents, and the page.
# Every link in the sidebar and in the pages is a plain <a> with a
# data-help-page attribute; inst/www/app.js forwards clicks on those to
# input$help_goto with a single delegated handler, so the help adds two inputs
# to the app however many pages it has.
# -----------------------------------------------------------------------------

#' The Help nav panel for bslib::page_navbar()
#' @noRd
helpNavPanel <- function() {
  bslib::nav_panel(
    title = "Help",
    value = "Help",
    icon = icon("circle-question"),
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 320,
        class = "help-sidebar",
        textInput("help_search", NULL, "", placeholder = "Search the help") |>
          addInputAttributes(maxlength = 100, autocomplete = "off"),
        uiOutput("help_search_results"),
        uiOutput("help_nav")
      ),
      div(class = "help-main", uiOutput("help_content"))
    )
  )
}

#' The sidebar table of contents, with the current page marked
#'
#' One <details> per section; the current page's section is open, and so is
#' "Getting started".  The drug and scenario sections are long, so the rest
#' start closed.
#' @noRd
helpSidebarNav <- function(registry, current = "home") {
  currentSection <- registry$section[match(current, registry$id)]
  sections <- lapply(names(HELP_SECTIONS), function(sec) {
    rows <- registry[registry$section == sec, ]
    if (nrow(rows) == 0) return(NULL)
    open <- identical(sec, currentSection) || sec == "start"
    items <- lapply(seq_len(nrow(rows)), function(i) {
      isCurrent <- identical(rows$id[i], current)
      tags$li(
        class = if (isCurrent) "active",
        tags$a(
          href = "#",
          `data-help-page` = rows$id[i],
          `aria-current` = if (isCurrent) "page",
          rows$title[i]
        )
      )
    })
    tags$details(
      class = "help-nav-section",
      open = if (open) NA,
      tags$summary(HELP_SECTIONS[[sec]],
                   tags$span(class = "help-nav-count", nrow(rows))),
      tags$ul(class = "help-nav-list", items)
    )
  })
  tags$nav(class = "help-nav", `aria-label` = "Help contents", sections)
}
