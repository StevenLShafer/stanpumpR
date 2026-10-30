# -----------------------------------------------------------------------------
# The drugs used to treat Parkinson's disease
# -----------------------------------------------------------------------------
# Drafted by Claude Code, 2026-10-19, at the request of Steven L. Shafer, who
# asked for the drugs of Parkinson's disease with their times of peak effect,
# therapeutic ranges, covariates (genotype included) and the literature and
# product monographs behind them.
#
# Every product is summarised in inst/extdata/parkinsonsProducts.csv (time to
# peak, half-life, onset and peak effect, range, covariates, genotype, label),
# every candidate source model is a row of parkinsonsRegistry.csv, and the
# numbers of every implemented model are in parkinsonsParameters.csv; all
# three are rebuilt by data-raw/parkinsonsRegistry.R and checked by
# tests/testthat/test-parkinsons-registry.R.  The implemented drugs are in the
# startup menu's PARKINSONS_DRUG_CATEGORY.  Levodopa (tablets and intestinal
# gel) is the one with an effect site, from Mao 2013's motor response; every
# other model is plasma only.  Help page: inst/help/models/parkinsons.md.
# -----------------------------------------------------------------------------

#' The Parkinson's disease product summary
#'
#' One row per product: its time to peak concentration, half-life, onset and
#' time of peak effect, therapeutic range, covariates and genotype, from its
#' US label and the literature, with the label's address.
#'
#' @returns a data frame read from \code{inst/extdata/parkinsonsProducts.csv}
#' @keywords internal
parkinsonsProducts <- function()
  utils::read.csv(
    system.file("extdata", "parkinsonsProducts.csv", package = "stanpumpR"),
    stringsAsFactors = FALSE, na.strings = "NA")

#' The Parkinson's disease model registry
#'
#' One row per drug and candidate source model, with its readiness, the
#' library drug that implements it (if any), what blocks it (if anything) and
#' the paper that would unblock it.
#'
#' @returns a data frame read from \code{inst/extdata/parkinsonsRegistry.csv}
#' @keywords internal
parkinsonsRegistry <- function()
  utils::read.csv(
    system.file("extdata", "parkinsonsRegistry.csv", package = "stanpumpR"),
    stringsAsFactors = FALSE, na.strings = "NA")

#' The parameter audit of the implemented Parkinson's disease models
#'
#' @returns a data frame read from \code{inst/extdata/parkinsonsParameters.csv}
#' @keywords internal
parkinsonsParameters <- function()
  utils::read.csv(
    system.file("extdata", "parkinsonsParameters.csv", package = "stanpumpR"),
    stringsAsFactors = FALSE, na.strings = "NA")
