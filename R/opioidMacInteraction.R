# -----------------------------------------------------------------------------
# Opioid reduction of MAC
# -----------------------------------------------------------------------------
# Provenance
# ----------
# Drafted by Claude Code (Claude Fable 5.1), 2026-10-05, at the request of
# Steven L. Shafer.  The model form is from a research summary Shafer
# commissioned from Gemini ("Pharmacodynamic Modeling of
# Opioid-Induced Reduction in Minimum Alveolar Concentration: A MEAC-Normalized
# Mathematical Framework for stanpumpR", supplied 2026-10-05).  Run and verified
# on R 4.6.1 by tests/testthat/test-opioid-mac.R -- which verifies that the code
# does what the equations say, NOT that the parameters are right.
#
# The model
# ---------
# Opioids lower the alveolar concentration needed to prevent movement, steeply
# at first and then to a ceiling: they cannot replace the anaesthetic entirely.
# The opioids in play are put on one scale by dividing each effect-site
# concentration by that opioid's MEAC, and the scaled values are added (Shafer:
# "the algebraic sum of the MEAC(t) of the individual opioids"):
#
#     U(t) = sum_i  Ce_i(t) / MEAC_i
#
# stanpumpR already computes each term -- it is the "% MEAC" series, here as a
# fraction rather than a percentage -- and uses its own MEAC values from
# drugDefaults_global.csv, not the ones tabulated in the Gemini summary.
#
# The fractional reduction in MAC is a sigmoid in U:
#
#     R(U) = Emax * U^gamma / (U50^gamma + U^gamma)
#
# so the MAC in force is MAC0 * (1 - R), and an alveolar concentration that was
# worth M multiples of MAC without opioid is worth
#
#     M / (1 - R(U))
#
# with it.  That is what is plotted: the MAC series RISES when an opioid is on
# board, because the same end-tidal concentration is now a larger multiple of a
# smaller MAC.  Nitrous oxide needs no separate term; it is already summed into
# the MAC series as a fraction of its own MAC, which is the same thing as the
# summary's carrier-gas adjustment.
#
# PARAMETERS: AN APPROXIMATE MODEL
# --------------------------------
# The published studies of opioid MAC reduction do not agree well with one
# another (Shafer, 2026-10-05), and this is offered as an approximate model, not
# a fitted one.  Shafer intends to supply his own parameters; until then the
# values below are a rough fit by Claude to nine published points, adopted at
# his direction on 2026-10-05.
#
# They replace the Gemini summary's "unified default parameter set" (Emax 0.68,
# U50 2.20, gamma 1.75), which did not survive a check against the abstracts of
# the papers it cites (PubMed, 2026-10-05):
#
#   * The concentrations it lists as each study's "EC50" are the concentrations
#     those papers report for a 50% REDUCTION IN MAC.  In this model U50 is where
#     the reduction is half of Emax, which with Emax 0.68 is 34%, not 50%.
#   * Its ceiling of 0.68 is below what the same papers report: remifentanil
#     32 ng/mL reduced isoflurane MAC by 91% (Lang 1996).
#   * Those studies fitted logistic regressions, not Hill curves, so the
#     per-study Emax and slope values in its table do not appear to come from
#     them.
#
# The values here were chosen so that the reduction is about 50% near
# U = 2.2 x MEAC, where the fentanyl and sufentanil studies put it, with a
# ceiling of 0.9.  Observed reduction in MAC, percent, against the two sets,
# using stanpumpR's MEAC values:
#
#   study                 opioid, plasma conc.      U     observed  these  Gemini
#   Brunner 1994 (cited)  fentanyl      1.67 ng/mL   2.78    50       55     41
#   Katoh 1999            fentanyl      3    ng/mL   5.00    61       67     55
#   Katoh 1999            fentanyl      6    ng/mL  10.00    74       77     64
#   Brunner 1994          sufentanil    0.145 ng/mL  2.59    50       54     39
#   Lang 1996             remifentanil  1.37 ng/mL   1.37    50       39     21
#   Lang 1996             remifentanil 32    ng/mL  32.00    91       85     67
#   Westmoreland 1994     alfentanil   28.8  ng/mL   0.74    50       27      9
#   Sebel 1992            fentanyl      0.78 ng/mL   1.30    59       38     19
#   Sebel 1992            fentanyl      1.72 ng/mL   2.87    67       56     42
#
# Closer than the set it replaces at every point, and still well short at low
# opioid levels and for alfentanil and remifentanil -- part of which is the MEAC
# each is scaled by rather than the curve.  DOIs: Lang
# 10.1097/00000542-199610000-00006; Katoh 10.1097/00000542-199902000-00012;
# Brunner 10.1093/bja/72.1.42; Westmoreland 10.1213/00000539-199401000-00006;
# Sebel 10.1097/00000542-199201000-00008.
#
# The parameters are isolated here so that they can be replaced without touching
# anything else.
# -----------------------------------------------------------------------------

OPIOID_MAC_EMAX  <- 0.90   # ceiling on the fractional reduction in MAC
OPIOID_MAC_U50   <- 1.76   # opioid level, in multiples of MEAC, at half of Emax
OPIOID_MAC_GAMMA <- 1.00   # steepness


#' Fractional reduction in MAC produced by opioids
#'
#' @param U total opioid effect-site concentration in multiples of MEAC, summed
#'   over opioids.  Negative or missing values are treated as zero.
#' @param Emax,U50,gamma parameters of the sigmoid; see the file header for
#'   their provenance.  This is an approximate model.
#' @returns the fraction by which MAC is reduced, between 0 and \code{Emax}
#' @export
opioidMacReduction <- function(U, Emax = OPIOID_MAC_EMAX, U50 = OPIOID_MAC_U50,
                               gamma = OPIOID_MAC_GAMMA)
{
  U[is.na(U) | U < 0] <- 0
  Ug <- U^gamma
  Emax * Ug / (U50^gamma + Ug)
}


#' Total opioid effect over time, in multiples of MEAC
#'
#' The sum over every simulated drug of its effect-site concentration divided by
#' its MEAC.  Only the opioids have an MEAC, so only they contribute.
#'
#' @param drugs the intravenous entries of the \code{drugs} list, each with an
#'   \code{equiSpace} data frame carrying \code{Time} and \code{MEAC} (the
#'   latter as a percentage of MEAC, as \code{simCpCe()} writes it)
#' @returns a data frame of \code{Time} and \code{U}, or NULL if no drug with an
#'   MEAC has been simulated
#' @export
totalOpioidMEAC <- function(drugs)
{
  es <- lapply(drugs, function(d) d$equiSpace)
  es <- es[!vapply(es, is.null, logical(1))]
  es <- es[vapply(es, function(e) any(e$MEAC > 0, na.rm = TRUE), logical(1))]
  if (length(es) == 0) return(NULL)

  # Every drug is simulated onto the same equispaced grid, so the series add
  # directly.  Interpolate onto the first one's grid anyway, so that a future
  # change to that does not silently misalign them.
  Time <- es[[1]]$Time
  U <- Reduce(`+`, lapply(es, function(e)
    stats::approx(e$Time, e$MEAC, Time, rule = 2)$y / 100))
  data.frame(Time = Time, U = U)
}


#' Apply the opioid interaction to the MAC series
#'
#' Rescales the synthetic "MAC" entry produced by \code{gasDrugEntries()} so
#' that it reports multiples of the opioid-reduced MAC.  The gases themselves
#' are untouched: their concentrations do not depend on the opioid, only what
#' those concentrations are worth.
#'
#' @param gasEntries output of \code{gasDrugEntries()}
#' @param drugs the intravenous entries of the \code{drugs} list
#' @returns \code{gasEntries}, with its "MAC" entry rescaled; unchanged if there
#'   is no MAC entry or no opioid
#' @export
applyOpioidMacInteraction <- function(gasEntries, drugs)
{
  mac <- gasEntries[["MAC"]]
  if (is.null(mac)) return(gasEntries)
  opioid <- totalOpioidMEAC(drugs)
  if (is.null(opioid)) return(gasEntries)

  series <- mac$results[mac$results$Site == "Plasma", c("Time", "Y")]
  U <- stats::approx(opioid$Time, opioid$U, series$Time, rule = 2)$y
  series$Y <- series$Y / (1 - opioidMacReduction(U))

  # Rebuilt through gasEntry() so that the normalisation series, the equispaced
  # values behind the hover readout, and the maxima all follow.
  gasEntries[["MAC"]] <- gasEntry(
    drug         = "MAC",
    alveolar     = series,
    brain        = series,
    xout         = mac$equiSpace$Time,
    drugDefaults = NULL,
    unitLabel    = "opioid-adjusted",
    typical      = c(lower = mac$lowerTypical, typical = mac$typical,
                     upper = mac$upperTypical),
    color        = mac$Color
  )
  gasEntries
}
