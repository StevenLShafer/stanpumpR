# -----------------------------------------------------------------------------
# Therapeutic reference ranges
# -----------------------------------------------------------------------------
# Drafted by Claude Code, 2026-10-19, at the request of Steven L. Shafer.
#
# The shaded band on a drug's panel (the Lower, Upper and Typical columns of
# drugDefaults_global.csv) is the drug's therapeutic reference range wherever
# one is established for the analyte the model plots.  The range itself, and
# where it came from, are recorded in inst/extdata/therapeuticRanges.csv: one
# row per drug, giving the analyte the range is for (a parent, or a parent
# plus metabolite), the matrix and sampling time, the limits, the laboratory
# alert level and level of recommendation for TDM, the source and
# how far it was read, the range it replaced, what the FDA or EMA product
# information says, and a band_status (THERAPEUTIC_RANGE_STATUSES) saying
# whether the band draws it and, if not, why.  Every drug in
# THERAPEUTIC_RANGE_CATEGORIES has a row; test-therapeutic-ranges.R holds the
# band columns to the registry.  The CSV is edited by hand.
# -----------------------------------------------------------------------------

#' The therapeutic reference range registry
#'
#' @returns a data frame read from \code{inst/extdata/therapeuticRanges.csv}
#' @keywords internal
therapeuticRanges <- function()
  utils::read.csv(
    system.file("extdata", "therapeuticRanges.csv", package = "stanpumpR"),
    stringsAsFactors = FALSE, na.strings = "NA")

#' One drug's therapeutic reference range
#'
#' @param drug a drug name
#' @returns the drug's registry row as a list, or NULL if it has none
#' @keywords internal
therapeuticRange <- function(drug)
{
  x <- therapeuticRanges()
  i <- match(drug, x$drug)
  if (is.na(i)) return(NULL)
  as.list(x[i, ])
}

#' One line describing a drug's therapeutic range, for its help page
#'
#' @param r a registry row from \code{therapeuticRange()}
#' @returns a character string
#' @keywords internal
therapeuticRangeSummary <- function(r)
{
  limits <- sprintf("%s to %s %s", helpFormatNumber(r$lower), helpFormatNumber(r$upper), r$units)
  what <- paste0(r$analyte, if (nzchar(r$sampling)) paste0(", ", r$sampling) else "",
                 if (!is.na(r$alert_level))
                   sprintf("; laboratory alert level %s %s", helpFormatNumber(r$alert_level), r$units)
                 else "")
  switch(r$band_status,
    drawn = sprintf("%s (%s); %s", limits, what, r$source),
    drawn_on_metabolite = sprintf("%s (%s), drawn on the %s row; %s",
                                  limits, what, r$band_drug, r$source),
    sum_not_drawn = sprintf("%s for %s, so not drawn on either row; %s", limits, what, r$source),
    pending_verification = sprintf("Not drawn: not yet verified in a primary source (%s)", r$source),
    not_applicable = sprintf("Not drawn: %s", r$notes),
    band_differs = sprintf("%s (%s; %s), not the band drawn: %s", limits, what, r$source, r$notes),
    none_established = sprintf("None established (%s)", r$source)
  )
}
