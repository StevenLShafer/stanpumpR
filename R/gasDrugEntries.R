# -----------------------------------------------------------------------------
# Provenance
# ----------
# Drafted by Claude Code (Claude Opus 5), 2026-09-02, at the request of
# Steven L. Shafer, who pointed out that the inhaled gases should appear as
# entries in the `drugs` list rather than being partitioned out of it.
#
# Rationale: the constraint on the gases is only that they must not go through
# getDrugPK() and simCpCe(), which assume a three-compartment mammillary model
# and a mass dose.  Nothing stops the gas engine from populating `drugs`
# entries directly, and doing so means the whole of simulationPlot() -- facets,
# colours, linetypes, normalisation, the hover readout -- works on them
# unchanged, and a dose table containing only inhaled agents plots correctly
# instead of hitting the nrow(allResults) == 0 early return.
#
# STATUS: run and verified on R 4.6.1 by tests/testthat/test-gas-entries.R.
# -----------------------------------------------------------------------------
#
# The mapping onto the existing plotting vocabulary:
#
#   engine output          drugs entry          meaning
#   -------------          -----------          -------
#   Site "Alveolar"    ->  Site "Plasma"        end-tidal, the measured value
#   Site "Brain"       ->  Site "Effect Site"   vessel-rich group tension
#
# That is not a fudge.  End-tidal gas is the directly monitored quantity, as
# plasma concentration is for an intravenous drug, and the vessel-rich group is
# the effect site.  Reusing the two names means Site stays inside the factor
# levels set at simulationPlot.R:365 and the solid/dashed linetype mapping at
# :388 applies with no change.
# -----------------------------------------------------------------------------


#' Which gases should be reported as their own series
#'
#' Air, nitrogen and the ventilatory rate are inputs or internal state, not
#' results: air is a carrier whose oxygen and nitrogen contributions are already
#' reported, nitrogen is tracked only so the fractions close and denitrogenation
#' is right, and ventilation is a setting.  Oxygen is always reported when any
#' gas is running, because the inspired oxygen fraction matters whatever else is
#' being given.
#'
#' @param gasDose the gas rows of the dose table
#' @returns character vector of gas names to report
#' @keywords internal
reportableGases <- function(gasDose)
{
  dosed <- unique(gasDose$Drug[!is.na(gasDose$Dose) & gasDose$Dose > 0])
  agents <- intersect(potentAgents(), dosed)

  anyFlow <- any(c("air", "oxygen", "nitrousOxide") %in% dosed)
  c(if (anyFlow) "oxygen", agents)
}


#' Turn inhaled-gas engine output into `drugs`-shaped entries
#'
#' Produces, for each reported gas, the same structure that \code{simCpCe()}
#' returns for an intravenous drug -- \code{results}, \code{equiSpace} and
#' \code{max} -- plus the metadata fields \code{simulationPlot()} reads in its
#' Step D1.  A synthetic "MAC" entry is added when any potent agent is running.
#'
#' @param sim output of \code{simulateGases()}, or NULL
#' @param gasDose the gas rows of the dose table, used to decide what to report
#' @param drugDefaults the drug defaults table, for colours and units
#' @param maximum simulation length in minutes
#' @param washout optional output of \code{gasWashout()}.  When supplied, each
#'   entry also carries the "time until threshold": for an agent, the time for
#'   its vessel-rich group tension to fall to the \code{endCe} threshold in
#'   \code{drugDefaults}, adjusted for age; for MAC, the time for the summed
#'   MAC to fall to \code{macThreshold}.  See R/gasRecovery.R.
#' @param age patient age in years, for the age-adjusted thresholds
#' @param macThreshold threshold for the MAC series, in multiples of MAC
#' @returns a named list of drug-shaped entries, empty if there is nothing to
#'   report
#' @export
gasDrugEntries <- function(sim, gasDose, drugDefaults = NULL, maximum = 60,
                           washout = NULL, age = 50,
                           macThreshold = GAS_MAC_THRESHOLD)
{
  if (is.null(sim)) return(list())
  if (is.null(drugDefaults)) drugDefaults <- getDrugDefaultsGlobal()

  report <- reportableGases(gasDose)
  if (length(report) == 0) return(list())

  props <- getGasProperties()
  xout  <- seq(from = 0, to = maximum, length.out = RESOLUTION)
  out   <- list()

  for (g in report)
  {
    alv <- sim$results[sim$results$Drug == g & sim$results$Site == "Alveolar", ]
    brn <- sim$results[sim$results$Drug == g & sim$results$Site == "Brain", ]
    if (nrow(alv) == 0) next

    # Oxygen has no tissue compartment, so its effect-site series is its
    # alveolar series: the end-tidal value is the whole of what we model.
    if (nrow(brn) == 0) brn <- alv

    # Time until threshold.  Only the soluble agents wash out by the model in
    # gasRecovery.R; oxygen has no threshold and no washout to speak of.
    endCe <- drugDefaults$endCe[match(g, drugDefaults$Drug)]
    if (length(endCe) != 1 || is.na(endCe)) endCe <- 0
    # A volatile agent's threshold is stored at age 40 and follows MAC with age.
    endCe <- gasThresholdForAge(g, endCe, age)
    recovery <- NULL
    if (!is.null(washout) && g %in% names(washout$rate) && endCe > 0)
      recovery <- gasRecoveryTime(washout, g, endCe)

    out[[g]] <- gasEntry(
      drug         = g,
      alveolar     = alv,
      brain        = brn,
      xout         = xout,
      drugDefaults = drugDefaults,
      unitLabel    = "%",
      typical      = gasTypicalBand(g, props),
      recovery     = recovery,
      endCe        = if (is.null(recovery)) 0 else endCe
    )
  }

  # MAC gets its own series when any potent agent is running.
  potent <- intersect(potentAgents(), report)
  if (length(potent) > 0)
  {
    mac <- sim$results[sim$results$Drug == "MAC", ]
    if (nrow(mac) > 0)
    {
      out[["MAC"]] <- gasEntry(
        drug         = "MAC",
        alveolar     = mac,
        brain        = mac,
        xout         = xout,
        drugDefaults = drugDefaults,
        unitLabel    = "age-adjusted",
        typical      = c(lower = 0.8, typical = 1.0, upper = 1.3),
        color        = "#000000",
        recovery     = if (is.null(washout)) NULL else
          macRecoveryTime(washout, age, macThreshold),
        endCe        = if (is.null(washout)) 0 else macThreshold
      )
      # Kept so that applyOpioidMacInteraction() can redo the time until
      # threshold against the opioid-reduced MAC without re-running anything.
      out[["MAC"]]$washout <- washout
      out[["MAC"]]$age     <- age
    }
  }

  out
}


#' Typical-range band for a gas facet
#'
#' The shaded band on each facet marks the usual clinical range.  For the potent
#' agents that is roughly 0.7 to 1.3 MAC; for oxygen it is a sensible inspired
#' range rather than a therapeutic window.
#'
#' @param drug gas name
#' @param props output of \code{getGasProperties()}
#' @returns named numeric vector: lower, typical, upper
#' @keywords internal
gasTypicalBand <- function(drug, props)
{
  # Oxygen's band is an acceptable inspired range, not a therapeutic window.
  if (drug == "oxygen") return(c(lower = 25, typical = 40, upper = 90))

  # Nitrous oxide is the exception to the MAC rule.  Its MAC is 104%, so a
  # 0.7-1.3 MAC band would be drawn at 73-135% -- a range that cannot be
  # delivered at one atmosphere without a hypoxic mixture.  Use the range it is
  # actually given in instead.
  if (drug == "nitrousOxide") return(c(lower = 50, typical = 60, upper = 70))

  MAC40 <- props$MAC40[props$gas == drug]
  if (length(MAC40) == 0 || is.na(MAC40)) return(c(lower = 0, typical = 0, upper = 0))
  c(lower = 0.7 * MAC40, typical = 1.0 * MAC40, upper = 1.3 * MAC40)
}


#' Build one `drugs`-shaped entry from a pair of gas series
#'
#' Mirrors the structure \code{simCpCe()} returns, including the four
#' normalisation series, so that the "Peak plasma" and "Peak effect site"
#' normalisation modes work on gases as they do on intravenous drugs.
#'
#' @param drug series name
#' @param alveolar,brain data frames of Time and Y
#' @param xout equispaced output times
#' @param drugDefaults drug defaults table
#' @param unitLabel axis-label units for the facet, e.g. "\%" for a gas or
#'   "age-adjusted" for the MAC-equivalents series
#' @param typical named vector of lower, typical, upper for the shaded band
#' @param color optional colour override
#' @param recovery optional "time until threshold" in minutes, one value per
#'   row of \code{alveolar}; NULL for none
#' @param endCe the threshold \code{recovery} refers to, in the units plotted
#' @returns a list shaped like an entry of the `drugs` list
#' @keywords internal
gasEntry <- function(drug, alveolar, brain, xout, drugDefaults,
                     unitLabel, typical, color = NULL,
                     recovery = NULL, endCe = 0)
{
  if (is.null(color))
  {
    idx   <- which(drugDefaults$Drug == drug)
    color <- if (length(idx) == 1) drugDefaults$Color[idx] else "#666666"
  }

  cp <- alveolar$Y
  ce <- brain$Y
  maxCp <- max(cp)
  maxCe <- max(ce)

  results <- data.frame(
    Drug = drug,
    Time = c(alveolar$Time, brain$Time),
    Site = rep(c("Plasma", "Effect Site"), c(nrow(alveolar), nrow(brain))),
    Y    = c(cp, ce),
    stringsAsFactors = FALSE
  )

  # The normalisation series, computed exactly as simCpCe() does.
  addNorm <- function(site, y, denom)
  {
    data.frame(Drug = drug, Time = alveolar$Time, Site = site,
               Y = if (denom == 0) rep(0, length(y)) else y / denom * 100,
               stringsAsFactors = FALSE)
  }
  results <- rbind(
    results,
    addNorm("CpNormCp", cp, maxCp), addNorm("CeNormCp", ce, maxCp),
    addNorm("CpNormCe", cp, maxCe), addNorm("CeNormCe", ce, maxCe)
  )

  equiSpace <- data.frame(
    Drug     = drug,
    Time     = xout,
    Ce       = stats::approx(brain$Time, ce, xout, rule = 2)$y,
    Recovery = if (is.null(recovery)) 0 else
      stats::approx(alveolar$Time, recovery, xout, rule = 2)$y,
    MEAC     = 0,
    stringsAsFactors = FALSE
  )

  list(
    drug                = drug,
    Drug                = drug,
    Color               = color,
    Concentration.Units = unitLabel,
    unitLabel           = unitLabel,
    typical             = unname(typical[["typical"]]),
    lowerTypical        = unname(typical[["lower"]]),
    upperTypical        = unname(typical[["upper"]]),
    MEAC                = 0,
    endCe               = endCe,
    isGas               = TRUE,
    results             = results,
    equiSpace           = equiSpace,
    max                 = data.frame(Drug = drug,
                                     Recovery = if (is.null(recovery)) 0 else max(recovery),
                                     Cp = maxCp, Ce = maxCe,
                                     stringsAsFactors = FALSE)
  )
}


#' Is this plotted series produced by the inhaled-gas engine?
#'
#' Covers the gases themselves plus the synthetic MAC series.  Used by
#' \code{simulationPlot()} to label their facets in \% or MAC rather than
#' appending the "/ml" that an intravenous concentration carries.
#'
#' @param drug one or more series names
#' @returns logical vector
#' @export
isGasSeries <- function(drug)
{
  drug %in% c(gasDrugNames(), "MAC")
}
