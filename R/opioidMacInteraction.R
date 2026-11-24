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
#   McEwan 1993 (cited)   fentanyl      1.67 ng/mL   2.78    50       55     41
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
#' its MEAC.  Only the opioids have an MEAC, so only they contribute.  Each
#' drug is read from its own simulated series (\code{derivedEffectSite()}),
#' which is dense where its curve bends, as the MEAC and Interaction panels
#' read it (R/derivedSeries.R).  Until 2026-11-24 it was read from the drug's
#' 100 equispaced points, which on a 24-hour plot are 14.5 minutes apart: a
#' remifentanil bolus at 5 minutes read 0.64 x MEAC at 6.2 minutes where its
#' effect site was at 7.8 ng/mL, and its effect began before it was given, on
#' a plot of any length (Gemini's gas-engine audit of 2026-11-23, D-01).
#'
#' @param drugs the intravenous entries of the \code{drugs} list, each with its
#'   \code{results} (long form, with an "Effect Site" series) and \code{MEAC}.
#'   An entry with no \code{results} is read from its \code{equiSpace}
#'   \code{MEAC} column, a percentage of MEAC, as \code{simCpCe()} writes it.
#' @param times the times to evaluate at; by default the drugs' own simulated
#'   times together with the equispaced grid (\code{derivedSeriesTimes()})
#' @returns a data frame of \code{Time} and \code{U}, or NULL if no drug with an
#'   MEAC has been simulated
#' @export
totalOpioidMEAC <- function(drugs, times = NULL)
{
  meac <- function(d) {
    m <- suppressWarnings(as.numeric(d$MEAC))
    if (length(m) == 1 && !is.na(m) && m > 0) m else 0
  }
  fromSeries <- function(d) !is.null(d$results) && meac(d) > 0
  fromGrid <- function(d) is.null(d$results) && !is.null(d$equiSpace$MEAC) &&
    any(d$equiSpace$MEAC > 0, na.rm = TRUE)
  # A companion entry (the Minto model drawn beside the library's
  # remifentanil; R/alternativeModels.R) is a second line of the same drug,
  # not a second opioid, so it is left out of the sum.
  opioids <- Filter(function(d) is.list(d) && !isAlternativeModelEntry(d) &&
                      (fromSeries(d) || fromGrid(d)), drugs)
  if (length(opioids) == 0) return(NULL)

  if (is.null(times)) {
    full <- Filter(fromSeries, opioids)
    times <- if (length(full) == 0) opioids[[1]]$equiSpace$Time else
      derivedSeriesTimes(full, max(unlist(lapply(full, function(d) d$results$Time))))
  }
  U <- Reduce(`+`, lapply(opioids, function(d)
    if (fromSeries(d)) derivedEffectSite(d, times) / meac(d) else
      stats::approx(d$equiSpace$Time, d$equiSpace$MEAC, times, rule = 2)$y / 100))
  data.frame(Time = times, U = U)
}


# The simulated times of the opioids in `drugs` (those read from their own
# series) within `limits`, for applyOpioidMacInteraction()
opioidTimes <- function(drugs, limits)
{
  t <- unlist(lapply(drugs, function(d) {
    if (!is.list(d) || isAlternativeModelEntry(d) || is.null(d$results)) return(NULL)
    m <- suppressWarnings(as.numeric(d$MEAC))
    if (length(m) != 1 || is.na(m) || m <= 0) return(NULL)
    d$results$Time[d$results$Site == "Effect Site"]
  }))
  t <- unique(t)
  t[t > limits[1] & t < limits[2]]
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
  series <- mac$results[mac$results$Site == "Plasma", c("Time", "Y")]
  opioid <- totalOpioidMEAC(drugs, series$Time)
  if (is.null(opioid)) return(gasEntries)
  R <- opioidMacReduction(opioid$U)

  # Time until threshold, if it is being shown.  The opioid-adjusted MAC is the
  # unadjusted MAC divided by (1 - R), so it falls to the threshold when the
  # unadjusted MAC falls to threshold * (1 - R).
  #
  # APPROXIMATION: R is held at its value at the moment delivery stops, though
  # the opioid's effect-site concentration goes on changing.  The error has no
  # fixed direction: where the opioid is waning the adjusted MAC falls FASTER
  # than this says and the time reads long, and where its effect-site
  # concentration is still rising (a bolus just given) it falls slower and the
  # time reads short, as inst/app-help/models/opioid-mac.md says.  Until
  # 2026-11-23 this comment said it always erred long (ChatGPT's gas-engine
  # audit of 2026-11-23, D-01).  Doing it properly would follow each opioid's
  # own washout after delivery stops, which the intravenous engines keep as
  # recoveryStatesOwn (effect-site amplitudes and rates, advanceStatesOnto())
  # when the time until threshold is drawn, but not for every engine (not the
  # Michaelis-Menten or transit-chain engines, nor through a pending lag),
  # and the coupled gas washout would need a threshold that changes with the
  # time since the stop: a proposal recorded in
  # docs/gas-engine-audit/gemini-2026-11-23-assessment.md (D-03).  Until
  # 2026-11-24 this comment said the engines did not expose them.
  #
  # The threshold changes from row to row wherever the opioid does, so
  # gasCoupledRecovery() computes every row rather than a sample of them.
  recovery <- NULL
  if (!is.null(mac$washout))
    recovery <- macRecoveryTime(mac$washout, mac$age, mac$endCe * (1 - R))

  # The opioid's own times inside the plot join the MAC series, so a bolus's
  # peak effect is drawn where it is, not only at the gas engine's points
  # (2.4 minutes apart late in a 24-hour plot); the unadjusted MAC, a smooth
  # curve, is interpolated there, and so is the time until threshold.
  series$Y <- series$Y / (1 - R)
  extra <- setdiff(opioidTimes(drugs, range(series$Time)), series$Time)
  if (length(extra) > 0) {
    unadjusted <- seriesStepAt(series$Time, series$Y * (1 - R), extra)
    added <- data.frame(Time = extra, Y = unadjusted /
                          (1 - opioidMacReduction(totalOpioidMEAC(drugs, extra)$U)))
    if (!is.null(recovery))
      recovery <- c(recovery, seriesStepAt(series$Time, recovery, extra))
    series <- rbind(series, added)
    o <- order(series$Time)
    series <- series[o, , drop = FALSE]
    if (!is.null(recovery)) recovery <- recovery[o]
    rownames(series) <- NULL
  }

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
    color        = mac$Color,
    recovery     = recovery,
    endCe        = mac$endCe
  )
  gasEntries[["MAC"]]$washout <- mac$washout
  gasEntries[["MAC"]]$age     <- mac$age
  gasEntries
}
