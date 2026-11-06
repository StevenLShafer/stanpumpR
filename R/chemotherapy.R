# -----------------------------------------------------------------------------
# Chemotherapy: the source bibliography and model registry
# -----------------------------------------------------------------------------
# Drafted by Claude Code, 2026-11-05, at the request of Steven L. Shafer, who
# supplied a ChatGPT/Gemini handoff ("Chemotherapy pharmacokinetics for
# stanpumpR: final recommendations after audit", 2026-11-04) and asked that it
# be audited and incorporated.  The audit of that handoff is
# docs/chemotherapy.md.
#
# No chemotherapy drug is in the library yet.  What is here is the evidence
# plan the handoff asked for, in three tables rebuilt by
# data-raw/chemotherapyRegistry.R and checked by
# tests/testthat/test-chemotherapy-registry.R:
#
#   chemotherapySources.csv    one row per publication, guideline, regulatory
#                              guidance or label, with its PMID, PMCID and DOI
#                              as PubMed records them, and how far it was read
#   chemotherapyRegistry.csv   one row per drug, product and candidate source
#                              model: population, analyte, structure (as far
#                              as it is known), readiness, what the engine
#                              would need, what blocks it and what to obtain
#   chemotherapyRelations.csv  which source supports which drug or model, and
#                              in what role (a structural model, a covariate,
#                              a guideline target, a label, an outcome trial),
#                              with "global" for guidance that applies to all
#
# Help page: inst/help/models/chemotherapy.md.
# -----------------------------------------------------------------------------

#' Read one of the chemotherapy registry tables
#' @noRd
readChemotherapyCsv <- function(file)
  utils::read.csv(
    system.file("extdata", file, package = "stanpumpR"),
    stringsAsFactors = FALSE, na.strings = "NA", colClasses = "character",
    encoding = "UTF-8")

#' The chemotherapy source bibliography
#'
#' One row per document: a primary study, a consensus guideline, a regulatory
#' guidance or a product label, with its identifiers and how far it was read.
#'
#' @returns a data frame read from \code{inst/extdata/chemotherapySources.csv}
#' @keywords internal
chemotherapySources <- function() readChemotherapyCsv("chemotherapySources.csv")

#' The chemotherapy model registry
#'
#' One row per drug, product and candidate source model, with its readiness,
#' what the engine would need, what blocks it and the document that would
#' unblock it.  No row is implemented: no chemotherapy drug is in the library.
#'
#' @returns a data frame read from \code{inst/extdata/chemotherapyRegistry.csv}
#' @keywords internal
chemotherapyRegistry <- function() readChemotherapyCsv("chemotherapyRegistry.csv")

#' The chemotherapy source relations
#'
#' One row per source and drug (or \code{"global"}), with the model it
#' supports, if any, and its role.
#'
#' @returns a data frame read from \code{inst/extdata/chemotherapyRelations.csv}
#' @keywords internal
chemotherapyRelations <- function() readChemotherapyCsv("chemotherapyRelations.csv")
