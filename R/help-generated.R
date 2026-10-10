# -----------------------------------------------------------------------------
# Generated blocks inside hand-written help pages
# -----------------------------------------------------------------------------
# Provenance
# ----------
# Claude Code, 2026-10-09, at the request of Steven L.
# Shafer, after an audit found the hand-written route table on the absorption
# page listing fewer drugs than the dose table offers by mouth, by injection
# into muscle or by nasal spray.  Verified by tests/testthat/test-help-content.R.
#
# A hand-written page (inst/help/<id>.md) may hold a line that is nothing but
#
#     <!-- generated: NAME -->
#
# and helpPageHTML() replaces it, before rendering, with the Markdown that
# HELP_GENERATED_BLOCKS[[NAME]] builds from the code.  A fact the code owns --
# which drugs offer which routes -- is then stated from the code and cannot
# drift from it, while the prose around it stays hand-written.  Viewed as raw
# Markdown (on GitHub, say) the marker is an invisible comment.
#
# Adding a block: write a function of `drugDefaults` that returns Markdown, add
# it to HELP_GENERATED_BLOCKS, and put the marker in the page.  An unknown name
# is an error, so the "every page renders" test catches a misspelt marker.
# -----------------------------------------------------------------------------

HELP_GENERATED_MARKER <-
  "^[[:space:]]*<!--[[:space:]]*generated:[[:space:]]*([A-Za-z0-9_-]+)[[:space:]]*-->[[:space:]]*$"

#' Replace each generated-block marker in a page's Markdown with its block
#'
#' @param text the page's Markdown, one string
#' @param drugDefaults the drug library, passed to each block's builder
#' @returns `text` with every marker line replaced
#' @noRd
helpExpandGenerated <- function(text, drugDefaults = getDrugDefaultsGlobal()) {
  if (is.null(text) || !nzchar(text)) return(text)
  lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
  hit <- grepl(HELP_GENERATED_MARKER, lines)
  if (!any(hit)) return(text)
  for (i in which(hit)) {
    name <- sub(HELP_GENERATED_MARKER, "\\1", lines[i])
    build <- HELP_GENERATED_BLOCKS[[name]]
    if (is.null(build)) stop("Unknown generated help block: ", name)
    lines[i] <- build(drugDefaults)
  }
  paste(lines, collapse = "\n")
}

#' The extravascular routes each drug offers, from its units
#'
#' One entry per drug with a PO, SL, IM, IN or RA unit in `drugDefaults_global.csv`,
#' the same Units the dose table's selector offers.  Each route lists that
#' route's units without their repeating forms (`mg PO bid` is `mg PO` given
#' on a schedule), with a note when a unit is a rate (`mg/day PO`, a constant
#' daily input with no depot) or when the drug's model declares saturable oral
#' absorption (an `oralSaturation` or `sublingualSaturation` block).
#'
#' @param drugDefaults the drug library
#' @returns a list with one element per such drug, in library order: a list of
#'   `drug`, `iv` (TRUE when the drug is also given intravenously) and, named
#'   PO, SL, IM, IN and RA, a list of `units` (empty when the route is not offered) and
#'   `notes`
#' @noRd
helpRouteInventory <- function(drugDefaults = getDrugDefaultsGlobal()) {
  adult <- helpReferencePatients()[1, ]
  extravascular <- c(ROUTE_PO, ROUTE_SL, ROUTE_IM, ROUTE_IN, ROUTE_RA)
  rows <- lapply(seq_len(nrow(drugDefaults)), function(i) {
    row <- drugDefaults[i, ]
    if (isGasDrug(row$Drug)) return(NULL)
    units <- unique(scheduleBaseUnit(helpDrugUnits(row)))
    route <- doseRoute(units)
    if (!any(route %in% extravascular)) return(NULL)
    model <- helpDrugModelOutput(row$Drug, adult)
    saturable <- c(PO = !is.null(model$oralSaturation), SL = !is.null(model$sublingualSaturation))
    byRoute <- lapply(stats::setNames(extravascular, extravascular), function(r) {
      u <- units[route == r]
      list(units = u,
           notes = c(if (any(isRateUnit(u))) "constant daily rate, no depot",
                     if (length(u) > 0 && isTRUE(saturable[r])) "saturable absorption"))
    })
    c(list(drug = row$Drug, iv = any(route == ROUTE_IV)), byRoute)
  })
  rows[!vapply(rows, is.null, logical(1))]
}

#' The route table on the absorption page, as Markdown
#' @noRd
helpRouteTableMarkdown <- function(drugDefaults = getDrugDefaultsGlobal()) {
  cell <- function(x) {
    if (length(x$units) == 0) return("—")
    paste0(paste0("`", x$units, "`", collapse = ", "),
           if (length(x$notes) > 0) paste0(" (", paste(x$notes, collapse = "; "), ")") else "")
  }
  body <- vapply(helpRouteInventory(drugDefaults), function(d) {
    sprintf("| [%s](help:drugs/%s) | %s | %s | %s | %s | %s | %s |", helpDrugTitle(d$drug), d$drug,
            cell(d[[ROUTE_PO]]), cell(d[[ROUTE_SL]]), cell(d[[ROUTE_IM]]), cell(d[[ROUTE_IN]]),
            cell(d[[ROUTE_RA]]), if (d$iv) "yes" else "no")
  }, character(1))
  paste(c(paste("| Drug | Oral (PO) | Sublingual (SL) | Intramuscular (IM) | Intranasal (IN) |",
                "Regional anesthesia (RA) | Also intravenous |"),
          "|---|---|---|---|---|---|---|",
          body), collapse = "\n")
}

HELP_GENERATED_BLOCKS <- list(
  "route-table" = helpRouteTableMarkdown
)
