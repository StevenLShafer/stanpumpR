# -----------------------------------------------------------------------------
# "Time until threshold" for the inhaled agents and MAC
# -----------------------------------------------------------------------------
# Provenance
# ----------
# Drafted by Claude Code (Claude Fable 5.1), 2026-10-05, at the request of
# Steven L. Shafer, extending the intravenous "time until threshold" feature to
# the inhaled gases; revised the same day to his specification of what
# "delivery stops" means and what the thresholds are.  Run and verified on
# R 4.6.1 by tests/testthat/test-gas-recovery.R, which checks it against doing
# the same thing in the full engine and simulating on.
#
# What is computed
# ----------------
# For every moment in the simulation: if this agent were turned off NOW, how
# long until it falls to its threshold?  As specified by Shafer, 2026-10-05:
#
#   * Each agent is its own decision.  Turning the vaporiser off and turning the
#     nitrous oxide off are separate adjustments, so each agent's time is the
#     time for THAT agent to wash out, whatever is done with the others.
#   * The fresh gas flow is turned up so that there is NO REBREATHING.  "If one
#     wants to awaken the patient, the fresh gas flow is increased so that no
#     rebreathing occurs.  That is the clinically important number."
#   * Ventilation stays what it was.
#
# No rebreathing means the patient inspires none of the agent: the circuit is
# flushed and stays clean.  That is the limit of high fresh gas flow, and it is
# modelled as that limit rather than as some particular large flow.
#
# The alternative -- leave the fresh gas flow where it was and let the circuit
# wash out with the patient -- is still available as gasWashout(rebreathing =
# TRUE).  It is slower, much slower at low flow, and is not what the app shows.
#
# How
# ---
# With none of the agent inspired, each gas obeys dy/dt = A y over the alveolar,
# vessel-rich, muscle and fat compartments, where A is the engine's own system
# matrix with the circuit removed.  A does not depend on the state, so the
# washout from any starting state y0 is
#
#     y(tau) = V exp(Lambda tau) V^-1 y0
#
# and any one compartment is a sum of four decaying exponentials (five, with
# the circuit, when rebreathing).  That is the form the intravenous code already
# solves, so the same root-finder, recoveryCalc(), serves both.  One
# eigen-decomposition per gas per interval of constant settings covers every
# time point in the interval.
#
# The approximation
# -----------------
# The engine couples the gases through their summed uptake (the concentration
# and second gas effect), which makes the true washout very slightly nonlinear.
# That coupling is LEFT OUT here, because including it means a forward
# simulation from every time point, which is far too slow to run on every edit.
# Leaving it out is also what makes each agent's answer independent of what is
# done with the others.  Measured against the full engine with 70% nitrous
# oxide (see the test file), the time reported is:
#
#   volatile agent, no nitrous oxide in use            within 0.2 min
#   volatile agent off, nitrous oxide left running     within 2%
#   nitrous oxide off, volatile agent left running     about 9% long
#   both off together: the volatile agent              about 5% long
#   both off together: MAC                             about 16% long
#
# It errs long because nitrous oxide leaving the blood adds to the gas leaving
# the alveoli and carries everything else out faster.  The MAC figure is the one
# that matters, and an exact version -- a forward simulation, for the MAC series
# only, when nitrous oxide is in use -- would be the way to remove it.
#
# Which concentration
# -------------------
#   * For each agent: the VESSEL-RICH GROUP (brain) tension, which is the gas
#     counterpart of the effect site, as in gasDrugEntries().
#   * For MAC: the ALVEOLAR tensions, summed over the potent agents as fractions
#     of their age-adjusted MAC, because that is how the plotted MAC series is
#     defined.  Every potent agent, nitrous oxide included, is turned off for
#     this one: MAC cannot fall to a tenth with nitrous oxide still running.
#
# Thresholds (Shafer, 2026-10-05, "personal choice, reflecting my clinical
# experience")
# -------------------------------------------------------------------------
#   * MAC: 0.1.
#   * Each volatile agent: 0.1 x its age-adjusted MAC.  Stored in
#     drugDefaults_global.csv as endCe at the reference age of 40 -- sevoflurane
#     0.21, isoflurane 0.11, desflurane 0.6 -- and adjusted for the patient's
#     age when used, by gasThresholdForAge().
#   * Nitrous oxide: 10%, not age-adjusted.  It comes off fast enough that the
#     number is not clinically important.
# All are editable in the Drug Thresholds dialog.
# -----------------------------------------------------------------------------

# Default threshold for the MAC series, in multiples of MAC.
GAS_MAC_THRESHOLD <- 0.1

# Fraction of age-adjusted MAC used as each volatile agent's default threshold.
# The endCe values in drugDefaults_global.csv are this times MAC40.
GAS_AGENT_THRESHOLD_FRACTION <- 0.1


#' Which gases have an age-adjusted threshold
#'
#' The volatile agents: potent, and delivered by a vaporiser.  Their threshold
#' is a fraction of MAC, and MAC falls with age.  Nitrous oxide's is a plain
#' percentage, and the carrier gases have none.
#'
#' @returns character vector of gas names
#' @keywords internal
ageAdjustedThresholdGases <- function()
{
  setdiff(potentAgents(), "nitrousOxide")
}


#' Threshold for a gas at the patient's age
#'
#' Thresholds for the volatile agents are stored at the reference age of 40 and
#' scale with age exactly as MAC does, so that a threshold of a tenth of MAC
#' stays a tenth of MAC.  Everything else is returned unchanged.
#'
#' @param drug one or more drug names
#' @param endCe the stored thresholds, same length as \code{drug}
#' @param age patient age in years
#' @param inverse if TRUE, convert a threshold at the patient's age back to the
#'   stored, age-40 value
#' @returns numeric vector of thresholds
#' @export
gasThresholdForAge <- function(drug, endCe, age, inverse = FALSE)
{
  f <- macForAge(1, age)
  adjust <- drug %in% ageAdjustedThresholdGases()
  endCe[adjust] <- if (inverse) endCe[adjust] / f else endCe[adjust] * f
  endCe
}


#' The thresholds as shown in the Drug Thresholds dialog
#'
#' One row per drug that can have a threshold, in the units it is plotted in,
#' AT THE PATIENT'S AGE for the volatile agents -- so the number on screen is
#' the number the plot uses -- plus a row for the MAC series.  The carrier gases
#' and the ventilation setting have no threshold and are left out.
#'
#' @param drugDefaults the drug defaults table
#' @param age patient age in years
#' @param macThreshold threshold for the MAC series
#' @returns data frame of \code{Drug} and \code{Threshold}
#' @export
thresholdTableForDisplay <- function(drugDefaults, age, macThreshold = GAS_MAC_THRESHOLD)
{
  x <- drugDefaults[, c("Drug", "endCe")]
  names(x)[2] <- "Threshold"
  x <- x[!x$Drug %in% c("air", "oxygen", "ventilation"), ]
  x$Threshold <- signif(gasThresholdForAge(x$Drug, x$Threshold, age), 3)
  if (any(isGasDrug(x$Drug)))
    x <- rbind(x, data.frame(Drug = "MAC", Threshold = macThreshold))
  rownames(x) <- NULL
  x
}


#' Take the edited Drug Thresholds table back into the defaults
#'
#' The inverse of \code{thresholdTableForDisplay()}.  Rows not in the table keep
#' the threshold they had; a blank or negative entry is treated as no threshold.
#'
#' @param edited the edited table: \code{Drug} and \code{Threshold}
#' @param drugDefaults the drug defaults table to update
#' @param age patient age in years
#' @param macThreshold the current MAC threshold, kept if the table has no MAC row
#' @returns a list with the updated \code{drugDefaults} and \code{macThreshold}
#' @export
thresholdTableToDefaults <- function(edited, drugDefaults, age,
                                     macThreshold = GAS_MAC_THRESHOLD)
{
  value <- suppressWarnings(as.numeric(edited$Threshold))
  value[is.na(value) | value < 0] <- 0
  drug <- as.character(edited$Drug)

  isMac <- drug == "MAC"
  if (any(isMac)) macThreshold <- value[isMac][1]

  stored <- gasThresholdForAge(drug[!isMac], value[!isMac], age, inverse = TRUE)
  at <- match(drugDefaults$Drug, drug[!isMac])
  drugDefaults$endCe[!is.na(at)] <- stored[at[!is.na(at)]]

  list(drugDefaults = drugDefaults, macThreshold = macThreshold)
}


#' Washout of every soluble gas, as sums of exponentials
#'
#' For each time point of a gas simulation, and each soluble gas, expresses the
#' washout that would follow if that gas were turned off at that moment as
#' decaying exponentials per compartment.  See the file header.
#'
#' @param sim output of \code{simulateGases()}
#' @param gasDose the gas rows of the dose table: Time, Drug, Dose
#' @param weight patient weight in kg
#' @param cardiacOutput cardiac output in L/min; defaults to the body's
#' @param rebreathing FALSE, the default and what the app shows: the fresh gas
#'   flow is turned up so that none of the agent is inspired.  TRUE: the fresh
#'   gas flow is left as it is and the circuit washes out with the patient.
#' @returns a list with \code{Time}, \code{rate} (per gas, an nT x K matrix of
#'   positive decay rates per minute) and \code{amplitude} (per gas, an
#'   nT x 5 x K array: time point, compartment, exponential), where K is 4
#'   without rebreathing and 5 with; or NULL if there is no simulation.
#'   Compartments are numbered as in the engine: 1 circuit, 2 alveolar,
#'   3 vessel-rich group, 4 muscle, 5 fat.
#' @export
gasWashout <- function(sim, gasDose, weight = 70, cardiacOutput = NULL,
                       rebreathing = FALSE)
{
  if (is.null(sim)) return(NULL)
  body <- getGasBody(weight)
  Qco  <- if (is.null(cardiacOutput)) body$Q_cardiac else cardiacOutput
  props <- getGasProperties()
  gases <- props$gas[props$soluble]

  gasDose <- gasDose[order(gasDose$Time), , drop = FALSE]
  bySetting <- split(gasDose, gasDose$Drug)
  Time <- sim$timeLine
  nT <- length(Time)

  # Without rebreathing the circuit is out of the picture: the patient inspires
  # none of the agent, so only compartments 2 to 5 take part.
  keep <- if (rebreathing) 1:5 else 2:5
  K <- length(keep)

  # Settings are constant between change points, so A is too.  Index each time
  # point by the interval it falls in; a point exactly on a change takes the
  # NEW settings, which are the ones that would be left running.
  changes  <- sort(unique(c(0, gasDose$Time)))
  interval <- findInterval(Time + 1e-9, changes)

  rate <- list(); amplitude <- list()
  for (g in gases)
  {
    rate[[g]]      <- matrix(0, nT, K)
    amplitude[[g]] <- array(0, c(nT, 5, K))
    y <- sim$state[[g]]
    for (iv in unique(interval))
    {
      s <- gasSettingsAt(bySetting, changes[iv])
      # Agent inflow off, no coupling.  The rows and columns for compartments
      # 2 to 5 do not involve the fresh gas flow at all, which is why dropping
      # the circuit IS the no-rebreathing limit.
      A <- gasSystemSoluble(props[props$gas == g, ], body, s$Q, s$VA, Qco,
                            Ffgf = 0, totUptake = 0)$A[keep, keep, drop = FALSE]
      e <- eigen(A)
      # A is similar to a symmetric matrix, so its eigenvalues are real; any
      # imaginary part is rounding.
      V  <- Re(e$vectors)
      lam <- pmax(-Re(e$values), 0)
      Vinv <- solve(V)

      use <- which(interval == iv)
      cf  <- y[use, keep, drop = FALSE] %*% t(Vinv)        # nUse x K: V^-1 y0
      rate[[g]][use, ] <- matrix(lam, length(use), K, byrow = TRUE)
      for (j in seq_len(K))
        amplitude[[g]][use, keep[j], ] <- sweep(cf, 2, V[j, ], `*`)
    }
  }
  list(Time = Time, rate = rate, amplitude = amplitude, rebreathing = rebreathing)
}


#' Time until one gas falls to its threshold
#'
#' @param washout output of \code{gasWashout()}
#' @param gas gas name
#' @param threshold target tension, percent of one atmosphere
#' @param compartment 3 for the vessel-rich group (the default, the gas
#'   counterpart of the effect site), 2 for alveolar
#' @returns numeric vector of minutes, one per time point; zero wherever the
#'   tension is already at or below the threshold, or if there is no threshold
#' @export
gasRecoveryTime <- function(washout, gas, threshold, compartment = 3)
{
  nT <- length(washout$Time)
  if (is.null(threshold) || is.na(threshold) || threshold <= 0) return(rep(0, nT))
  amp <- washout$amplitude[[gas]]; lam <- washout$rate[[gas]]
  vapply(seq_len(nT), function(i)
    recoveryCalc(amp[i, compartment, ], lam[i, ], threshold), numeric(1))
}


#' Time until the summed MAC falls to a threshold
#'
#' Alveolar tensions of the potent agents, each as a fraction of its
#' age-adjusted MAC, summed -- exactly as the MAC series is built.  Every potent
#' agent is taken to be turned off.
#'
#' @param washout output of \code{gasWashout()}
#' @param age patient age in years
#' @param threshold target, in multiples of MAC.  May be a vector with one value
#'   per time point, which is how the opioid interaction enters: an opioid that
#'   lowers MAC by a fraction R makes a given end-tidal concentration worth
#'   1 / (1 - R) times as much, so the threshold on the unadjusted MAC is
#'   \code{threshold * (1 - R)}.
#' @returns numeric vector of minutes, one per time point
#' @export
macRecoveryTime <- function(washout, age, threshold = GAS_MAC_THRESHOLD)
{
  nT <- length(washout$Time)
  threshold <- rep_len(threshold, nT)
  props  <- getGasProperties()
  potent <- props$gas[props$potent]
  MAC <- vapply(potent, function(g)
    macForAge(props$MAC40[props$gas == g], age), numeric(1))

  vapply(seq_len(nT), function(i) {
    if (is.na(threshold[i]) || threshold[i] <= 0) return(0)
    amp <- unlist(lapply(potent, function(g) washout$amplitude[[g]][i, 2, ] / MAC[[g]]))
    lam <- unlist(lapply(potent, function(g) washout$rate[[g]][i, ]))
    recoveryCalc(amp, lam, threshold[i])
  }, numeric(1))
}
