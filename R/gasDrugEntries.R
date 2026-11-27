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

  # At the body's temperature (gasPropertiesAt()) and the patient's age
  # (gasTypicalBand()), so the band follows MAC.  A band is one range for the
  # whole plot, at the temperature in force at time 0; with physiology off
  # normal the plot replaces it with the physiology ribbon
  # (R/physiologyRibbon.R), and the threshold line steps (endCeSteps).
  props <- gasPropertiesAt(sim$body$temperature)
  # (sim$body is the body in force at time 0, an event at 0 included,
  # advanceClosedFormGas(); ChatGPT's review of PR #796, finding 9)
  xout  <- seq(from = 0, to = maximum, length.out = RESOLUTION)
  out   <- list()

  for (g in report)
  {
    alv <- sim$results[sim$results$Drug == g & sim$results$Site == "Alveolar", ]
    brn <- sim$results[sim$results$Drug == g & sim$results$Site == "Brain", ]
    if (nrow(alv) == 0) next

    # Oxygen has no tissue compartment.  Its second line is the ARTERIAL
    # oxygen tension (R/arterialOxygen.R; Shafer, 2026-11-19): the end-tidal
    # value itself without a shunt, below it with an endobronchial tube.
    arterialO2 <- NULL
    if (g == "oxygen") {
      arterialO2 <- gasArterialOxygen(sim)
      if (!is.null(arterialO2))
        brn <- data.frame(Drug = g, Time = arterialO2$Time, Site = "Brain",
                          Y = arterialO2$arterial, stringsAsFactors = FALSE)
    }
    if (nrow(brn) == 0) brn <- alv

    # Time until threshold.  Only the soluble agents wash out by the model in
    # gasRecovery.R; oxygen has no threshold and no washout to speak of.
    endCe <- drugDefaults$endCe[match(g, drugDefaults$Drug)]
    if (length(endCe) != 1 || is.na(endCe)) endCe <- 0
    # A volatile agent's threshold is stored at age 40 and follows MAC with age.
    # Every potent gas's is defined at 37 C and follows MAC with temperature
    # too (gasThresholdForTemperature(); Shafer, 2026-11-24): the time until
    # threshold moves it to the temperature in force when the agent is turned
    # off.  The value here is at the temperature in force at time 0; the
    # panel's line steps with each later temperature (endCeSteps below,
    # thresholdSteps(); ChatGPT's round-2 gas audit, D-02).
    endCe <- gasThresholdForAge(g, endCe, age)
    recovery <- NULL
    if (!is.null(washout) && g %in% names(washout$rate) && endCe > 0)
      recovery <- gasRecoveryTime(washout, g, endCe)
    endCeAtAge <- endCe
    endCe <- gasThresholdForTemperature(g, endCe, sim$body$temperature)

    out[[g]] <- gasEntry(
      drug         = g,
      alveolar     = alv,
      brain        = brn,
      xout         = xout,
      drugDefaults = drugDefaults,
      unitLabel    = "%",
      typical      = gasTypicalBand(g, props, age),
      recovery     = recovery,
      endCe        = if (is.null(recovery)) 0 else endCe
    )
    # PaO2 and SaO2 for the oxygen panel's hover
    if (!is.null(arterialO2)) out[[g]]$arterialOxygen <- arterialO2
    # After a physiology event that changes the temperature, the threshold
    # line steps with it, as the time until threshold does (thresholdSteps();
    # Shafer, 2026-11-25, Gemini's gas-engine audit, C-01)
    if (!is.null(recovery) && !is.null(sim$bodySteps)) {
      temps <- vapply(sim$bodySteps$body, function(b)
        if (is.null(b$temperature)) PHYSIOLOGY_TEMPERATURE_DEFAULT else b$temperature, numeric(1))
      if (length(unique(temps)) > 1)
        out[[g]]$endCeSteps <- data.frame(
          Start = sim$bodySteps$time,
          endCe = vapply(temps, function(t) gasThresholdForTemperature(g, endCeAtAge, t), numeric(1)))
    }
    # The muscle group (state 4) and the fat group (state 5), the gas's
    # counterparts of V2 and V3, as tensions in % of an atmosphere, for the
    # compartment panels (R/compartments.R).  Oxygen has no tissue states.
    state <- sim$state[[g]]
    if (!is.null(state) && ncol(state) >= 5 && !is.null(sim$timeLine)) {
      out[[g]]$compartments <- data.frame(Time = sim$timeLine,
                                          MRG = state[, 4], FG = state[, 5])
      # Every state of the agent (circuit, lungs, VRG, muscle, fat), the body
      # and circuit they were solved with, the cardiac output and dead space
      # they ran on, and the gas rows, for the Visualizer's gas run
      # (viewerGasRun() in R/viewerRun.R).  Read by nothing else.
      if (!is.null(sim$body))
        out[[g]]$gasRun <- list(time = sim$timeLine, state = state, body = sim$body,
                                circuit = sim$circuit, weight = sim$weight, age = sim$age,
                                Qco = sim$Qco, deadSpace = sim$deadSpace,
                                gasDose = gasDose,
                                # Physiology events: the bodies, the body of
                                # each row, and the events themselves
                                bodySteps = sim$bodySteps, bodyRow = sim$bodyRow,
                                cardiacOutput = sim$cardiacOutput,
                                physiologyEvents = sim$physiologyEvents,
                                # The arterial oxygen where a tube shunts
                                # blood (R/arterialOxygen.R), for the
                                # Visualizer's SaO2
                                arterialOxygen = gasRunArterialOxygen(sim))
    }
  }

  # MAC gets its own series when any potent agent is running: the summed
  # end-tidal MAC as its "plasma" line, which the time until threshold
  # reads, and the summed brain (vessel-rich group) MAC as its "effect site"
  # line (gasBrainMac(); Shafer, 2026-11-25, Gemini's gas-engine audit of
  # 2026-11-23, C-04 and D-05: "I think Ted Eger would have approved").
  # Until then the effect-site line was the end-tidal MAC again.
  potent <- intersect(potentAgents(), report)
  if (length(potent) > 0)
  {
    mac <- sim$results[sim$results$Drug == "MAC", ]
    if (nrow(mac) > 0)
    {
      out[["MAC"]] <- gasEntry(
        drug         = "MAC",
        alveolar     = mac,
        brain        = gasBrainMac(sim),
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
#' agents that is roughly 0.7 to 1.3 MAC, the patient's: MAC at the patient's
#' age, as the MAC series and the time until threshold use it.  Until
#' 2026-11-23 the band was drawn at the MAC of a 40-year-old whatever the age,
#' 2.1% sevoflurane where an 80-year-old's MAC is 1.64% (ChatGPT's gas-engine
#' audit of 2026-11-23, C-02).  For oxygen it is a sensible inspired range
#' rather than a therapeutic window.
#'
#' @param drug gas name
#' @param props output of \code{getGasProperties()}
#' @param age patient age in years; 40, the reference age, gives MAC40
#' @returns named numeric vector: lower, typical, upper
#' @keywords internal
gasTypicalBand <- function(drug, props, age = 40)
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
  MAC <- macForAge(MAC40, age)
  c(lower = 0.7 * MAC, typical = 1.0 * MAC, upper = 1.3 * MAC)
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
#' @param unitLabel axis-label units for the facet, e.g. "%" for a gas or
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

  # A physiology event (R/physiologyEvents.R) repeats its time, before and
  # after the step: a point at the step reads after it, one before it reads
  # towards the value before it (seriesStepAt())
  equiSpace <- data.frame(
    Drug     = drug,
    Time     = xout,
    Ce       = seriesStepAt(brain$Time, ce, xout),
    Recovery = if (is.null(recovery)) 0 else
      seriesStepAt(alveolar$Time, recovery, xout),
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
#' \code{simulationPlot()} to label their facets in % or MAC rather than
#' appending the "/ml" that an intravenous concentration carries.
#'
#' @param drug one or more series names
#' @returns logical vector
#' @export
isGasSeries <- function(drug)
{
  drug %in% c(gasDrugNames(), "MAC")
}
