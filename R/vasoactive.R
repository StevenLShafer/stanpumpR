# -----------------------------------------------------------------------------
# The vasoactive and resuscitation drugs of intensive care
# -----------------------------------------------------------------------------
# Drafted by Claude Code, 2026-10-18, at the request of Steven L. Shafer.
#
# The intravenous vasopressors, inotropes, vasodilators, antiarrhythmics and
# resuscitation drugs he asked for are catalogued, implemented or not, in
# inst/extdata/vasoactiveRegistry.csv (one row per drug and source), with the
# numbers of every implemented model in vasoactiveParameters.csv; both are
# rebuilt by data-raw/vasoactiveRegistry.R and checked by
# tests/testthat/test-vasoactive-registry.R.  The implemented drugs are in the
# startup menu's VASOACTIVE_DRUG_CATEGORY and are concentration models only:
# no blood pressure, heart rate or rhythm is predicted, and none has an effect
# site or a band.  Help page: inst/help/models/vasoactive.md.
# -----------------------------------------------------------------------------

#' The vasoactive and resuscitation drug registry
#'
#' One row per drug and candidate source, with its readiness, the library drug
#' that implements it (if any), what blocks it (if anything) and the papers
#' that would unblock it.
#'
#' @returns a data frame read from \code{inst/extdata/vasoactiveRegistry.csv}
#' @keywords internal
vasoactiveRegistry <- function()
  utils::read.csv(
    system.file("extdata", "vasoactiveRegistry.csv", package = "stanpumpR"),
    stringsAsFactors = FALSE, na.strings = "NA")

#' The parameter audit of the implemented vasoactive models
#'
#' @returns a data frame read from \code{inst/extdata/vasoactiveParameters.csv}
#' @keywords internal
vasoactiveParameters <- function()
  utils::read.csv(
    system.file("extdata", "vasoactiveParameters.csv", package = "stanpumpR"),
    stringsAsFactors = FALSE, na.strings = "NA")

#' Two-compartment parameters from clearance, central volume and two rates
#'
#' The micro-rate constants that give exponential half-lives \code{thalf1}
#' and \code{thalf2} with clearance \code{CL} out of a central volume
#' \code{V1}:
#' \deqn{k_{10} = CL/V_1,\; k_{21} = \lambda_1\lambda_2/k_{10},\;
#'       k_{12} = \lambda_1 + \lambda_2 - k_{10} - k_{21}}
#' and then \eqn{V_2 = k_{12} V_1 / k_{21}} and \eqn{Q = k_{12} V_1}.  This is
#' the stable decomposition when a source reports its central volume; when it
#' reports only the steady-state volume, see
#' \code{twoCompartmentFromHalfLives()} (\code{R/drugs_clevidipine.R}).  Used
#' by labetalol and diltiazem, whose sources print a steady-state volume as
#' well, which their tests use as the check.  Refuses a set of numbers with
#' no admissible mammillary model.
#'
#' @param CL clearance
#' @param V1 central volume, in the volume units of \code{CL}
#' @param thalf1,thalf2 the two exponential half-lives, in the time units of
#'   \code{CL}, in either order
#' @returns a list of \code{v1}, \code{v2}, \code{cl1} and \code{cl2}
#' @keywords internal
twoCompartmentFromCentralVolume <- function(CL, V1, thalf1, thalf2)
{
  l1 <- log(2) / min(thalf1, thalf2)
  l2 <- log(2) / max(thalf1, thalf2)
  k10 <- CL / V1
  k21 <- l1 * l2 / k10
  k12 <- l1 + l2 - k10 - k21
  if (!(k12 > 0 && k21 > l2 && k21 < l1))
    stop("These numbers have no physically admissible two-compartment model")
  list(v1 = V1, v2 = k12 * V1 / k21, cl1 = CL, cl2 = k12 * V1)
}
