# -----------------------------------------------------------------------------
# The help system: page registry, Markdown rendering and search
# -----------------------------------------------------------------------------
# Provenance
# ----------
# Drafted by Claude Code (Claude Fable 5.1), 2026-10-06, at the request of
# Steven L. Shafer.  Verified by tests/testthat/test-help-content.R.
#
# How it fits together
# --------------------
# The help is a "Help" tab in the app (R/help-ui.R, R/help-server.R).  Its
# pages come from two places:
#
#   * Hand-written Markdown in inst/help/, one file per page, named by page id
#     (inst/help/dose-table.md is the page "dose-table"; inst/help/models/
#     effect-site.md is "models/effect-site").  Files start at "##": the page
#     title comes from the registry below and is added as the <h1>.
#
#   * Pages generated from the code so they cannot drift from it: one page per
#     drug in drugDefaults_global.csv (R/help-drugs.R), one per teaching
#     scenario (R/help-scenarios.R), the drug index, the scenario index and the
#     bibliography.  A generated drug or scenario page also appends the
#     narrative Markdown of the same id, if there is one, so a drug page is
#     "what the code computes" followed by "what a person wants to say about
#     it".
#
#   * Blocks generated from the code inside a hand-written page: a line
#     "<!-- generated: NAME -->" in the Markdown is replaced by the block
#     R/help-generated.R builds (the route table on models/absorption).
#
# Links between pages are written in Markdown as [text](help:page-id); a link
# that loads a teaching scenario into the simulator is [text](scenario:id).
# helpRewriteLinks() turns these into data attributes that inst/www/app.js
# forwards to Shiny as input$help_goto and input$help_scenario_load.
#
# Adding a page: add a row to helpStaticPages() and create the .md file.
# Adding a drug: the page exists as soon as the drug is in the defaults CSV;
# add inst/help/drugs/<drug>.md for the narrative, which the tests require.
# -----------------------------------------------------------------------------

HELP_SECTIONS <- c(
  start     = "Getting started",
  simulator = "Using the simulator",
  drugs     = "Drug library",
  models    = "Models and methods",
  scenarios = "Teaching scenarios",
  people    = "People and history",
  project   = "The project",
  reference = "Reference"
)

# The hand-written pages, in the order they appear in the sidebar.  Generated
# pages (drugs, scenarios) are inserted after the index page of their section.
helpStaticPages <- function() {
  spec <- c(
    # id, title, section
    "home",                     "Welcome to stanpumpR",            "start",
    "quick-start",              "The five-minute tour",            "start",
    "cautions",                 "Cautions: what the curves mean",  "start",
    "faq",                      "Frequently asked questions",      "start",

    "patient-profile",          "Patient profile",                 "simulator",
    "dose-table",               "The dose table",                  "simulator",
    "reading-the-plot",         "Reading the plot",                "simulator",
    "graph-options",            "Graph options",                   "simulator",
    "additional-plots",         "Additional plots",                "simulator",
    "inhaled-agents",           "Inhaled anesthetics",             "simulator",
    "tci",                      "Target-controlled infusion",      "simulator",
    "suggest-dosing",           "Suggest Dosing",                  "simulator",
    "drug-library",             "Drug Library and Drug Thresholds","simulator",
    "time-display",             "Time display",                    "simulator",
    "sharing",                  "Sharing a simulation by URL",     "simulator",
    "email-slide",              "Email a slide",                   "simulator",
    "debug-mode",               "Debug mode",                      "simulator",

    "drugs/index",              "All drugs",                       "drugs",

    "models/overview",          "How stanpumpR computes",          "models",
    "models/three-compartment", "The three-compartment model",     "models",
    "models/effect-site",       "The effect site and ke0",         "models",
    "models/covariates",        "Covariates and body size",        "models",
    "models/fat-free-mass",     "Scaling to fat-free mass",        "models",
    "models/absorption",        "Oral, intramuscular, intranasal and regional anesthesia doses", "models",
    "models/metabolites",       "Active metabolites",              "models",
    "models/antidepressants",   "Antidepressant models and their limits", "models",
    "models/pk-events",         "Events that change the kinetics", "models",
    "models/meac",              "MEAC: comparing opioids",         "models",
    "models/interaction",       "Propofol-opioid interaction",     "models",
    "models/recovery",          "Time until threshold",            "models",
    "models/normalization",     "Normalization",                   "models",
    "models/suggest-algorithm", "How Suggest Dosing searches",     "models",
    "models/gas-engine",        "The inhaled-gas engine",          "models",
    "models/gas-differences",   "Where the gas engine differs from Gas Man", "models",
    "models/opioid-mac",        "Opioid reduction of MAC",         "models",

    "scenarios/index",          "All scenarios",                   "scenarios",

    "investigators",            "The investigators",               "people",
    "history",                  "From CATIA to stanpumpR",         "people",

    "repository",               "What is in the repository",       "project",
    "contributing",             "Contributing a drug or a model",  "project",
    "scripting",                "Driving the engine from R",       "project",
    "validation",               "Testing and validation",          "project",
    "in-development",           "In development",                  "project",

    "glossary",                 "Glossary",                        "reference",
    "units",                    "Units and conversions",           "reference",
    "references",               "Bibliography",                    "reference"
  )
  m <- matrix(spec, ncol = 3, byrow = TRUE)
  data.frame(id = m[, 1], title = m[, 2], section = m[, 3],
             stringsAsFactors = FALSE)
}

#' Display title for a drug or gas name
#'
#' "nitrousOxide" becomes "Nitrous Oxide"; "propofol" becomes "Propofol".
#' @noRd
helpDrugTitle <- function(drug) {
  spaced <- gsub("([a-z])([A-Z])", "\\1 \\2", drug)
  vapply(spaced, tools::toTitleCase, character(1), USE.NAMES = FALSE)
}

#' Every page the help knows about
#'
#' @param drugDefaults the drug defaults table; one page per row
#' @returns a data frame with id, title, section and sectionTitle, in sidebar
#'   order
#' @noRd
helpPageRegistry <- function(drugDefaults = getDrugDefaultsGlobal()) {
  static <- helpStaticPages()
  drugs <- data.frame(
    id = paste0("drugs/", drugDefaults$Drug),
    title = helpDrugTitle(drugDefaults$Drug),
    section = "drugs",
    stringsAsFactors = FALSE
  )
  scen <- helpScenarios()
  scenarios <- data.frame(
    id = paste0("scenarios/", vapply(scen, `[[`, character(1), "id")),
    title = vapply(scen, `[[`, character(1), "title"),
    section = "scenarios",
    stringsAsFactors = FALSE
  )
  parts <- lapply(names(HELP_SECTIONS), function(sec) {
    rows <- static[static$section == sec, ]
    if (sec == "drugs") rows <- rbind(rows, drugs)
    if (sec == "scenarios") rows <- rbind(rows, scenarios)
    rows
  })
  reg <- do.call(rbind, parts)
  reg$sectionTitle <- unname(HELP_SECTIONS[reg$section])
  rownames(reg) <- NULL
  reg
}

helpPageExists <- function(id, registry = helpPageRegistry()) {
  is.character(id) && length(id) == 1 && !is.na(id) && id %in% registry$id
}

# --- Markdown files ----------------------------------------------------------

helpContentDir <- function() {
  dir <- system.file("help", package = "stanpumpR")
  if (!nzchar(dir)) stop("The stanpumpR help content (inst/help) was not found")
  dir
}

helpMarkdownPath <- function(id) {
  file.path(helpContentDir(), paste0(id, ".md"))
}

helpHasMarkdown <- function(id) {
  file.exists(helpMarkdownPath(id))
}

#' Read a page's Markdown, or "" if there is none
#' @noRd
helpReadMarkdown <- function(id) {
  path <- helpMarkdownPath(id)
  if (!file.exists(path)) return("")
  paste(readLines(path, encoding = "UTF-8", warn = FALSE), collapse = "\n")
}

#' All Markdown files under inst/help, as page ids
#' @noRd
helpMarkdownIds <- function() {
  files <- list.files(helpContentDir(), pattern = "\\.md$", recursive = TRUE)
  files <- files[basename(files) != "README.md"]
  sub("\\.md$", "", files)
}

# --- Rendering ---------------------------------------------------------------

#' Render Markdown to an HTML string, with help links rewritten
#'
#' Uses shiny::markdown(), i.e. commonmark with the GitHub extensions (tables,
#' strikethrough, autolinks), which the app already depends on.  No pandoc.
#' @noRd
helpMarkdownToHTML <- function(text) {
  if (is.null(text) || !nzchar(trimws(text))) return("")
  html <- as.character(shiny::markdown(text, extensions = TRUE))
  helpRewriteLinks(html)
}

#' Turn help: and scenario: links into the data attributes app.js listens for,
#' and open external links in a new tab
#' @noRd
helpRewriteLinks <- function(html) {
  html <- gsub('href="help:([^"]*)"', 'href="#" data-help-page="\\1"', html)
  html <- gsub('href="scenario:([^"]*)"',
               'href="#" class="btn btn-primary btn-sm help-scenario-btn" data-help-scenario="\\1"',
               html)
  html <- gsub('<a href="(https?://[^"]*)"',
               '<a href="\\1" target="_blank" rel="noreferrer"', html)
  html
}

#' The help: and scenario: link targets in a Markdown text
#' @returns list(pages = character, scenarios = character)
#' @noRd
helpLinkTargets <- function(text) {
  pages <- regmatches(text, gregexpr("\\]\\(help:[^)]+\\)", text))[[1]]
  scenarios <- regmatches(text, gregexpr("\\]\\(scenario:[^)]+\\)", text))[[1]]
  list(
    pages = sub("^\\]\\(help:", "", sub("\\)$", "", pages)),
    scenarios = sub("^\\]\\(scenario:", "", sub("\\)$", "", scenarios))
  )
}

helpSlug <- function(text) {
  slug <- tolower(helpPlainText(text))
  slug <- gsub("[^a-z0-9]+", "-", slug)
  slug <- gsub("^-+|-+$", "", slug)
  if (!nzchar(slug)) slug <- "section"
  slug
}

#' Give every h2 and h3 an id, and return a table of contents
#'
#' @returns list(html = the html with ids, toc = data.frame(level, text, id))
#' @noRd
helpHeadingIds <- function(html) {
  pattern <- "<h([23])>(.*?)</h[23]>"
  matches <- regmatches(html, gregexpr(pattern, html, perl = TRUE))[[1]]
  toc <- data.frame(level = integer(0), text = character(0), id = character(0),
                    stringsAsFactors = FALSE)
  if (length(matches) == 0) return(list(html = html, toc = toc))
  used <- character(0)
  for (m in matches) {
    level <- as.integer(sub(pattern, "\\1", m, perl = TRUE))
    inner <- sub(pattern, "\\2", m, perl = TRUE)
    text <- helpPlainText(inner)
    id <- helpSlug(text)
    n <- 1
    while (id %in% used) {
      n <- n + 1
      id <- paste0(helpSlug(text), "-", n)
    }
    used <- c(used, id)
    replacement <- sprintf('<h%d id="%s">%s</h%d>', level, id, inner, level)
    html <- sub(m, replacement, html, fixed = TRUE)
    toc <- rbind(toc, data.frame(level = level, text = text, id = id,
                                 stringsAsFactors = FALSE))
  }
  list(html = html, toc = toc)
}

#' Strip tags and decode the common entities, for search and slugs
#' @noRd
helpPlainText <- function(html) {
  text <- gsub("<[^>]+>", " ", html)
  text <- gsub("&nbsp;", " ", text, fixed = TRUE)
  text <- gsub("&amp;", "&", text, fixed = TRUE)
  text <- gsub("&lt;", "<", text, fixed = TRUE)
  text <- gsub("&gt;", ">", text, fixed = TRUE)
  text <- gsub("&quot;", "\"", text, fixed = TRUE)
  text <- gsub("&#39;", "'", text, fixed = TRUE)
  text <- gsub("\\s+", " ", text)
  trimws(text)
}

#' The HTML body of a page, as a string, before the page shell is added
#' @noRd
helpPageHTML <- function(id, drugDefaults = getDrugDefaultsGlobal()) {
  if (id == "drugs/index") return(helpDrugIndexHTML(drugDefaults))
  if (id == "scenarios/index") return(helpScenarioIndexHTML())
  if (id == "references") return(helpReferencesHTML(drugDefaults))
  if (startsWith(id, "drugs/")) {
    return(helpDrugPageHTML(sub("^drugs/", "", id), drugDefaults))
  }
  if (startsWith(id, "scenarios/")) {
    return(helpScenarioPageHTML(sub("^scenarios/", "", id)))
  }
  # A hand-written page may carry blocks built from the code (R/help-generated.R)
  helpMarkdownToHTML(helpExpandGenerated(helpReadMarkdown(id), drugDefaults))
}

#' A complete help page: breadcrumb, title, table of contents, body
#'
#' @param id page id.  An unknown id gives a "not found" page rather than an
#'   error, since the id comes from the browser.
#' @returns a shiny tag
#' @noRd
helpPageUI <- function(id, drugDefaults = getDrugDefaultsGlobal()) {
  registry <- helpPageRegistry(drugDefaults)
  if (!helpPageExists(id, registry)) return(helpNotFoundUI(id))
  row <- registry[registry$id == id, ][1, ]

  rendered <- helpHeadingIds(helpPageHTML(id, drugDefaults))
  toc <- rendered$toc[rendered$toc$level == 2, ]
  tocUI <- NULL
  if (nrow(toc) >= 3) {
    tocUI <- tags$nav(
      class = "help-toc",
      tags$span(class = "help-toc-title", "On this page"),
      tags$ul(lapply(seq_len(nrow(toc)), function(i) {
        tags$li(tags$a(href = paste0("#", toc$id[i]), toc$text[i]))
      }))
    )
  }

  tags$article(
    class = "help-page",
    `data-help-id` = id,
    tags$div(
      class = "help-breadcrumb small text-muted",
      tags$a(href = "#", `data-help-page` = "home", "Help"),
      " › ",
      row$sectionTitle
    ),
    tags$h1(class = "help-title", row$title),
    tocUI,
    tags$div(class = "help-body", HTML(rendered$html))
  )
}

helpNotFoundUI <- function(id) {
  tags$article(
    class = "help-page",
    tags$h1(class = "help-title", "Page not found"),
    tags$p("There is no help page called ",
           tags$code(htmltools::htmlEscape(as.character(id)[1])), "."),
    tags$p(tags$a(href = "#", `data-help-page` = "home", "Back to the help contents"))
  )
}

# --- Search ------------------------------------------------------------------

#' Plain text of every page, for searching
#'
#' Built once per R session and kept in .sprglobals, since rendering every
#' drug page means running every drug model at six reference patients.
#' @noRd
helpSearchIndex <- function(drugDefaults = getDrugDefaultsGlobal()) {
  registry <- helpPageRegistry(drugDefaults)
  registry$text <- vapply(registry$id, function(id) {
    helpPlainText(helpPageHTML(id, drugDefaults))
  }, character(1), USE.NAMES = FALSE)
  registry
}

helpSearchIndexCached <- function() {
  if (is.null(.sprglobals$helpSearchIndex)) {
    .sprglobals$helpSearchIndex <- helpSearchIndex()
  }
  .sprglobals$helpSearchIndex
}

helpEscapeRegex <- function(x) {
  gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", x)
}

#' Search the help
#'
#' A plain, case-insensitive text search.  Pages whose title matches rank
#' first, then pages by number of occurrences.
#'
#' @param term what to look for; fewer than two characters finds nothing
#' @param index the search index, by default the cached one
#' @param maxHits at most this many pages
#' @returns a data frame with id, title, sectionTitle, hits and snippet
#' @noRd
helpSearch <- function(term, index = helpSearchIndexCached(), maxHits = 25) {
  empty <- data.frame(id = character(0), title = character(0),
                      sectionTitle = character(0), hits = integer(0),
                      snippet = character(0), stringsAsFactors = FALSE)
  term <- trimws(as.character(term)[1])
  if (is.na(term) || nchar(term) < 2) return(empty)
  pattern <- helpEscapeRegex(term)

  positions <- gregexpr(pattern, index$text, ignore.case = TRUE)
  count <- vapply(positions, function(p) if (p[1] == -1) 0L else length(p), integer(1))
  inTitle <- grepl(pattern, index$title, ignore.case = TRUE)
  exact <- tolower(index$title) == tolower(term) |
    tolower(basename(index$id)) == tolower(term)
  keep <- which(count > 0 | inTitle)
  if (length(keep) == 0) return(empty)

  snippet <- vapply(keep, function(i) {
    text <- index$text[i]
    p <- positions[[i]]
    if (p[1] == -1) return("")
    start <- max(1, p[1] - 70)
    end <- min(nchar(text), p[1] + attr(p, "match.length")[1] + 70)
    s <- substr(text, start, end)
    paste0(if (start > 1) "…", s, if (end < nchar(text)) "…")
  }, character(1))

  res <- data.frame(
    id = index$id[keep],
    title = index$title[keep],
    sectionTitle = index$sectionTitle[keep],
    hits = count[keep] + 100L * inTitle[keep] + 1000L * exact[keep],
    snippet = snippet,
    stringsAsFactors = FALSE
  )
  res <- res[order(-res$hits, res$title), ]
  rownames(res) <- NULL
  utils::head(res, maxHits)
}

#' Escape a snippet for HTML with the search term wrapped in <mark>
#'
#' The matches are found on the plain text and each piece is escaped on its
#' own, so a term such as "amp" cannot land inside an entity like &amp;.
#' @noRd
helpHighlight <- function(text, term) {
  esc <- htmltools::htmlEscape
  m <- gregexpr(helpEscapeRegex(term), text, ignore.case = TRUE)[[1]]
  if (m[1] == -1) return(esc(text))
  starts <- as.integer(m)
  lengths <- attr(m, "match.length")
  pieces <- character(0)
  pos <- 1
  for (k in seq_along(starts)) {
    pieces <- c(pieces,
                esc(substr(text, pos, starts[k] - 1)),
                "<mark>", esc(substr(text, starts[k], starts[k] + lengths[k] - 1)), "</mark>")
    pos <- starts[k] + lengths[k]
  }
  paste(c(pieces, esc(substr(text, pos, nchar(text)))), collapse = "")
}

#' Search results for the sidebar
#' @noRd
helpSearchResultsUI <- function(term, hits) {
  if (nrow(hits) == 0) {
    return(tags$div(class = "help-search-results small text-muted",
                    "Nothing found for “", htmltools::htmlEscape(term), "”."))
  }
  items <- lapply(seq_len(nrow(hits)), function(i) {
    snippet <- helpHighlight(hits$snippet[i], term)
    tags$li(
      tags$a(href = "#", `data-help-page` = hits$id[i], hits$title[i]),
      tags$span(class = "help-search-section", hits$sectionTitle[i]),
      if (nzchar(hits$snippet[i])) tags$div(class = "help-search-snippet", HTML(snippet))
    )
  })
  tags$div(
    class = "help-search-results",
    tags$div(class = "small text-muted",
             sprintf("%d page%s match", nrow(hits), if (nrow(hits) == 1) "" else "s")),
    tags$ul(items)
  )
}

# --- Shared HTML helpers -----------------------------------------------------

#' Format a number for a help table
#' @noRd
helpFormatNumber <- function(x, digits = 3) {
  vapply(x, function(v) {
    if (is.null(v) || length(v) == 0 || is.na(v)) return("—")
    if (!is.finite(v)) return("—")
    if (v == 0) return("0")
    format(signif(v, digits), big.mark = ",", scientific = FALSE, trim = TRUE)
  }, character(1), USE.NAMES = FALSE)
}

#' An HTML table from a data frame, for the help pages
#' @noRd
helpTableHTML <- function(df, caption = NULL, class = "table table-sm table-striped help-table") {
  if (is.null(df) || nrow(df) == 0) return("")
  header <- tags$tr(lapply(names(df), function(n) tags$th(n)))
  body <- lapply(seq_len(nrow(df)), function(i) {
    tags$tr(lapply(seq_along(df), function(j) {
      v <- df[i, j]
      if (inherits(v, "html") || inherits(v, "shiny.tag")) tags$td(v) else tags$td(as.character(v))
    }))
  })
  as.character(tags$table(
    class = class,
    if (!is.null(caption)) tags$caption(caption),
    tags$thead(header),
    tags$tbody(body)
  ))
}

helpColorSwatch <- function(color) {
  if (is.null(color) || is.na(color) || !nzchar(color)) return(NULL)
  tags$span(class = "help-swatch", style = paste0("background:", color, ";"),
            title = color)
}

#' A link to another help page, as an HTML string
#' @noRd
helpPageLink <- function(id, text = NULL) {
  if (is.null(text)) {
    registry <- helpStaticPages()
    text <- if (id %in% registry$id) registry$title[registry$id == id] else id
  }
  sprintf('<a href="#" data-help-page="%s">%s</a>',
          htmltools::htmlEscape(id, attribute = TRUE), htmltools::htmlEscape(text))
}
