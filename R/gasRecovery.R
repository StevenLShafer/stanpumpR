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
# In the ideal circuit, the engine's default, no rebreathing is what any fresh
# gas flow at or above the minute ventilation gives.
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
# By simulating it.  For each line on the plot, the agent in question is turned
# off at every time point in turn and the washout is integrated forward, with
# the engine's own equations in the limit of no rebreathing, until the
# concentration comes down through the threshold for the last time -- until,
# that is, a rise back above it is shown to be impossible, which below the
# threshold and falling does not show (see "When a row of the washout is
# finished", below gasCoupledRecovery()).  The gases stay coupled as
# they are in the engine: nitrous oxide leaving the blood swells the gas leaving
# the alveoli and carries the other agents out with it, and the agents left
# running go on being inspired at their fresh-gas concentrations.
#
# A forward simulation from every time point sounds expensive and is not,
# because they are all done at once: the states of all the time points form one
# matrix, and each step of the integration is a handful of matrix products on
# it.  A line costs a few hundredths of a second.  See gasCoupledRecovery().
#
# Measured against doing the same thing in the full engine (see the test file),
# every line agrees to within a few hundredths of a minute.
#
# The shortcut that came first
# ----------------------------
# The first version left the coupling out.  Each gas then obeys dy/dt = A y, the
# washout is a sum of four decaying exponentials, and recoveryCalc() -- the
# root-finder the intravenous drugs use -- gives the time directly.  It is exact
# for a volatile agent on its own, but reads long when nitrous oxide is washing
# out as well: with 70% nitrous oxide after two hours, about 10% for the
# volatile agent, 18% for the nitrous oxide's own line and 10% for MAC.  It is
# kept as `exact = FALSE`, and is what is used for the stable-flow
# (`rebreathing = TRUE`) variant, which has no coupled counterpart here.
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
#' The inverse of \code{thresholdTableForDisplay()}.  Rows not in the table, and
#' rows left as the dialog showed them, keep the threshold they had (a stored
#' value past the ceiling, shown at another age or converted from mcg to ng,
#' therefore no longer refuses every other edit).  Zero, entered as 0, is no threshold.  An entry that
#' is not a number from 0 to \code{MAX_DOSE_VALUE} (blank, ".", text, negative, too large) is refused rather
#' than read as 0: nothing is changed, and \code{invalid} names the rows, so
#' the dialog can say which to correct.
#'
#' @param edited the edited table: \code{Drug} and \code{Threshold}
#' @param drugDefaults the drug defaults table to update
#' @param age patient age in years
#' @param macThreshold the current MAC threshold, kept if the table has no MAC row
#' @returns a list with the updated \code{drugDefaults} and \code{macThreshold},
#'   and \code{invalid}, the drugs whose entries were refused (empty when all
#'   were accepted; otherwise \code{drugDefaults} and \code{macThreshold} are
#'   returned unchanged)
#' @export
thresholdTableToDefaults <- function(edited, drugDefaults, age,
                                     macThreshold = GAS_MAC_THRESHOLD)
{
  drug <- as.character(edited$Drug)
  value <- vapply(edited$Threshold, function(x) {
    if (is.null(x) || length(x) != 1) return(NA_real_)
    suppressWarnings(as.numeric(as.character(x)))
  }, numeric(1), USE.NAMES = FALSE)
  # Only the library's drugs and MAC: a row naming anything else is not one
  # the dialog drew, and its name was quoted back in the refusal (Gemini's
  # production audit, 2026-11-08, finding 6)
  known <- drug %in% c(as.character(drugDefaults$Drug), "MAC")
  drug <- drug[known]
  value <- value[known]
  # Only an entry the edit changed is read: one left as the dialog showed it
  # keeps its stored value, unrounded.  A stored value past the grid's
  # ceiling (a volatile agent's threshold shown at another age, a range
  # converted from mcg to ng) made every later OK fail, whatever drug it
  # edited (Carbon's second UI audit, 2026-11-15, F5)
  shown <- thresholdTableForDisplay(drugDefaults, age, macThreshold)
  before <- shown$Threshold[match(drug, shown$Drug)]
  changed <- is.na(before) | is.na(value) | abs(value - before) > 1e-12 * abs(before)
  drug <- drug[changed]
  value <- value[changed]
  # The grid's ceiling, validateDose()'s, held here too (fourth review, F05)
  bad <- !is.finite(value) | value < 0 | value > MAX_DOSE_VALUE
  if (any(bad)) {
    return(list(drugDefaults = drugDefaults, macThreshold = macThreshold,
                invalid = drug[bad]))
  }

  isMac <- drug == "MAC"
  if (any(isMac)) macThreshold <- value[isMac][1]

  stored <- gasThresholdForAge(drug[!isMac], value[!isMac], age, inverse = TRUE)
  at <- match(drugDefaults$Drug, drug[!isMac])
  drugDefaults$endCe[!is.na(at)] <- stored[at[!is.na(at)]]

  list(drugDefaults = drugDefaults, macThreshold = macThreshold,
       invalid = character(0))
}


# The thresholds a bookmark carries: those edited away from the library's
# defaults, keyed by drug, or NULL when none has been.  The whole column, one
# entry per drug, was half of every bookmark's length (3,700 characters before
# encoding) and helped the longest past the server's limit
# (MAX_BOOKMARK_URL_CHARS); restoreThresholds() reads a subset, and an old
# bookmark's full column, alike.  A drug left at its default therefore follows
# the library's default when the bookmark is reopened, where the full column
# froze it at the value of the day.
changedThresholds <- function(drugDefaults, defaults = getDrugDefaultsGlobal())
{
  value <- suppressWarnings(as.numeric(drugDefaults$endCe))
  default <- suppressWarnings(as.numeric(defaults$endCe[match(drugDefaults$Drug, defaults$Drug)]))
  changed <- !is.na(value) & (is.na(default) | abs(value - default) > 1e-9)
  if (!any(changed)) return(NULL)
  stats::setNames(value[changed], drugDefaults$Drug[changed])
}

#' Thresholds from a restored bookmark
#'
#' A bookmark saves the thresholds edited away from the library's defaults
#' (\code{changedThresholds()}; before 2026-10-31 the stored \code{endCe} of
#' every drug), named by drug, and the MAC threshold.  Both are optional:
#' bookmarks made before thresholds were saved, and any entry that is not a
#' usable number, leave the drug's current value in place and the MAC
#' threshold at its default.  Drugs named in the bookmark but absent from the
#' defaults are ignored.
#'
#' @param drugDefaults the drug defaults table to update
#' @param endCe named numeric vector of stored thresholds, or NULL
#' @param macThreshold a single threshold for the MAC series, or NULL
#' @returns a list with the updated \code{drugDefaults} and \code{macThreshold}
#' @export
restoreThresholds <- function(drugDefaults, endCe = NULL, macThreshold = NULL)
{
  usable <- function(x) suppressWarnings(as.numeric(x))

  if (length(endCe) > 0 && !is.null(names(endCe))) {
    value <- usable(endCe)
    at <- match(names(endCe), drugDefaults$Drug)
    # held to the dialog's ceiling, as a link is untrusted (Carbon's second
    # UI audit, 2026-11-15, F5).  A volatile agent's threshold is stored at
    # age 40 and shown at the patient's, so the most the dialog stores is its
    # ceiling at the oldest age, where MAC is least (gasThresholdForAge());
    # held to MAX_DOSE_VALUE, a threshold accepted at age 70 was dropped from
    # the link that saved it (CodeRabbit on PR #730).
    limit <- ifelse(names(endCe) %in% ageAdjustedThresholdGases(),
                    MAX_DOSE_VALUE / macForAge(1, MAX_AGE), MAX_DOSE_VALUE)
    ok <- !is.na(at) & is.finite(value) & value >= 0 & value <= limit
    drugDefaults$endCe[at[ok]] <- value[ok]
  }

  mac <- usable(macThreshold)
  macThreshold <- if (length(mac) == 1 && is.finite(mac) && mac >= 0 &&
                      mac <= MAX_DOSE_VALUE) mac else GAS_MAC_THRESHOLD

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
#'   gas flow is left as it is, and whatever rebreathing the circuit allows at
#'   that flow goes on.
#' @param circuit the circuit model, as in \code{advanceClosedFormGas()}.  Only
#'   matters when \code{rebreathing = TRUE}: without rebreathing there is no
#'   circuit to model.
#' @param deadSpace dead space as a fraction of minute ventilation, as in
#'   \code{advanceClosedFormGas()}; must be the value the simulation used.
#' @param oxygenUptake whether oxygen consumed shrinks the gas volume, as in
#'   \code{advanceClosedFormGas()}; must match the simulation.
#' @returns a list with \code{Time}, \code{rate} (per gas, an nT x K matrix of
#'   positive decay rates per minute) and \code{amplitude} (per gas, an
#'   nT x 5 x K array: time point, compartment, exponential), where K is 5 for
#'   rebreathing in a semi-closed circuit and 4 otherwise; or NULL if there is
#'   no simulation.
#'   Compartments are numbered as in the engine: 1 circuit, 2 alveolar,
#'   3 vessel-rich group, 4 muscle, 5 fat.
#' @export
gasWashout <- function(sim, gasDose, weight = 70, cardiacOutput = NULL,
                       rebreathing = FALSE,
                       circuit = c("ideal", "semi-closed"),
                       deadSpace = GAS_DEAD_SPACE_FRACTION,
                       oxygenUptake = TRUE)
{
  circuit <- match.arg(circuit)
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
  # none of the agent, so only compartments 2 to 5 take part.  The same is true
  # of the ideal circuit even WITH rebreathing, because its circuit tension is
  # a function of the alveolar tension and not a state of its own.
  washoutCircuit <- if (!rebreathing) "open" else circuit
  keep <- if (washoutCircuit == "semi-closed") 1:5 else 2:5
  K <- length(keep)

  # Settings are constant between change points, so A is too.  Index each time
  # point by the interval it falls in; a point exactly on a change takes the
  # NEW settings, which are the ones that would be left running.  A row that
  # repeats the setting in force changes nothing and is not a change point:
  # every interval is integrated on its own to the end of a day by
  # gasCoupledRecovery(), so 500 such rows held the worker 33 s where one
  # takes half a second (CodeRabbit on PR #730).
  changes  <- gasSettingChanges(bySetting, sort(unique(c(0, gasDose$Time))))
  interval <- findInterval(Time + 1e-9, changes)

  rate <- list(); amplitude <- list()
  for (g in gases)
  {
    rate[[g]]      <- matrix(0, nT, K)
    amplitude[[g]] <- array(0, c(nT, 5, K))
    y <- sim$state[[g]]
    for (iv in unique(interval))
    {
      s <- gasSettingsAt(bySetting, changes[iv], deadSpace)
      # Agent inflow off, no coupling.  "open" is the no-rebreathing limit:
      # the patient inspires fresh gas only, whatever the flow.
      #
      # With rebreathing, the volume lost to oxygen consumption still has to be
      # allowed for: at low flows it is a large part of the fresh gas, and what
      # is vented -- the only way the agent leaves -- is the fresh gas less the
      # oxygen consumed.  It is a constant, so the washout stays linear.
      uO2 <- 0; cE <- 0
      if (rebreathing && oxygenUptake) {
        uO2 <- body$VO2 * (1 - GAS_RESPIRATORY_QUOTIENT)
        if (s$MV > 0) cE <- min(GAS_RESPIRATORY_QUOTIENT * body$VO2 / s$MV,
                                GAS_MAX_EXHALED_CO2)
      }
      A <- gasSystemSoluble(props[props$gas == g, ], body, s$Q, s$VA, Qco,
                            Ffgf = 0, totUptake = uO2, circuit = washoutCircuit,
                            MV = s$MV, cE = cE)$A[keep, keep, drop = FALSE]
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
  list(Time = Time, rate = rate, amplitude = amplitude, rebreathing = rebreathing,
       # What macRecoveryTimeExact() needs to run the coupled washout itself.
       state = sim$state[gases], bySetting = bySetting, changes = changes,
       interval = interval, body = body, Qco = Qco, deadSpace = deadSpace,
       oxygenUptake = oxygenUptake)
}


# The times at which a gas setting changes: of `times` (sorted, the first
# kept), those at which the setting in force of some gas row (a flow, a
# vaporiser, the ventilation) differs from what it was at the last time kept.
# `bySetting` is the gas rows split by Drug, as gasWashout() holds them, and
# the settings are read as settingAt() reads them, so a row that repeats the
# setting in force, or one that a later row at the same time overrides with
# the same value, changes nothing.
gasSettingChanges <- function(bySetting, times)
{
  # The settings gasSettingsAt() and gasCoupledRecovery() read
  read <- intersect(names(bySetting), c("air", "oxygen", "nitrousOxide", "ventilation",
                                        setdiff(potentAgents(), "nitrousOxide")))
  if (length(times) < 2 || length(read) == 0) return(times)
  at <- function(t) vapply(read, function(g) as.numeric(settingAt(bySetting[[g]], t)), numeric(1))
  keep <- rep(TRUE, length(times))
  last <- at(times[1])
  for (i in seq_along(times)[-1]) {
    now <- at(times[i])
    if (identical(now, last)) keep[i] <- FALSE else last <- now
  }
  times[keep]
}


#' Time until one gas falls to its threshold
#'
#' That gas alone is turned off, with no rebreathing; anything else in use goes
#' on being given.
#'
#' @param washout output of \code{gasWashout()}
#' @param gas gas name
#' @param threshold target tension, percent of one atmosphere
#' @param compartment 3 for the vessel-rich group (the default, the gas
#'   counterpart of the effect site), 2 for alveolar
#' @param exact TRUE, the default: the coupled forward simulation of
#'   \code{gasCoupledRecovery()}.  FALSE: the sum-of-exponentials shortcut,
#'   which leaves the coupling between gases out.  The shortcut is always used
#'   for a washout made with \code{rebreathing = TRUE}.
#' @returns numeric vector of minutes, one per time point; zero wherever the
#'   tension is at or below the threshold and not on its way above it, or if
#'   there is no threshold
#' @export
gasRecoveryTime <- function(washout, gas, threshold, compartment = 3, exact = TRUE)
{
  nT <- length(washout$Time)
  if (is.null(threshold) || is.na(threshold) || threshold <= 0) return(rep(0, nT))

  if (exact && !isTRUE(washout$rebreathing) && !is.null(washout$state))
  {
    gases  <- names(washout$state)
    target <- numeric(4 * length(gases))
    target[(match(gas, gases) - 1) * 4 + (compartment - 1)] <- 1
    return(gasCoupledRecovery(washout, off = gas, target = target,
                              threshold = threshold))
  }

  amp <- washout$amplitude[[gas]]; lam <- washout$rate[[gas]]
  vapply(seq_len(nT), function(i)
    recoveryCalc(amp[i, compartment, ], lam[i, ], threshold), numeric(1))
}


#' Time until the summed MAC falls to a threshold
#'
#' Alveolar tensions of the potent agents, each as a fraction of its
#' age-adjusted MAC, summed -- exactly as the MAC series is built.  Every potent
#' agent is taken to be turned off, with no rebreathing.
#'
#' @param washout output of \code{gasWashout()}
#' @param age patient age in years
#' @param threshold target, in multiples of MAC.  May be a vector with one value
#'   per time point, which is how the opioid interaction enters: an opioid that
#'   lowers MAC by a fraction R makes a given end-tidal concentration worth
#'   1 / (1 - R) times as much, so the threshold on the unadjusted MAC is
#'   \code{threshold * (1 - R)}.
#' @param exact TRUE, the default: the coupled forward simulation of
#'   \code{gasCoupledRecovery()}.  FALSE: the sum-of-exponentials shortcut,
#'   which leaves the coupling between gases out and reads long when nitrous
#'   oxide is in use.  The shortcut is always used for a washout made with
#'   \code{rebreathing = TRUE}.
#' @returns numeric vector of minutes, one per time point
#' @export
macRecoveryTime <- function(washout, age, threshold = GAS_MAC_THRESHOLD,
                            exact = TRUE)
{
  if (exact && !isTRUE(washout$rebreathing) && !is.null(washout$state))
    return(macRecoveryTimeExact(washout, age, threshold))

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


#' Time until the summed MAC falls to a threshold, with the gases coupled
#'
#' Every potent agent is turned off, with no rebreathing, and the washout is
#' simulated forward by \code{gasCoupledRecovery()}.
#'
#' @param washout output of \code{gasWashout()} with \code{rebreathing = FALSE}
#' @param age patient age in years
#' @param threshold target in multiples of MAC; one value, or one per time point
#' @returns numeric vector of minutes, one per time point: zero where MAC is
#'   already at or below the threshold or there is no threshold, and
#'   \code{MINS_PER_DAY} if it has not been reached in a day
#' @export
macRecoveryTimeExact <- function(washout, age, threshold = GAS_MAC_THRESHOLD)
{
  props  <- getGasProperties()
  gases  <- names(washout$state)
  potent <- intersect(props$gas[props$potent], gases)

  # MAC as a linear function of the state: each potent agent's alveolar tension
  # over its age-adjusted MAC.
  target <- numeric(4 * length(gases))
  for (g in potent)
    target[(match(g, gases) - 1) * 4 + 1] <-
      1 / macForAge(props$MAC40[props$gas == g], age)

  gasCoupledRecovery(washout, off = potent, target = target, threshold = threshold)
}


# The coupled washout's step (gasCoupledRecovery()), in minutes: 0.1 for the
# first hour of washout, where the crossing nearly always is, and 0.25 after,
# as since 2026-10-05.
GAS_RECOVERY_STEP_FIRST_HOUR <- 0.1
GAS_RECOVERY_STEP_LATER <- 0.25

# The most a step may be, in time constants of the fastest rate a method
# samples rather than solves.  In the first hour of washout, one: that hour
# carries the crossings that ride on the fast alveolar washout (MAC's nearly
# always), so there the step is the accuracy, and over one time constant the
# classical Runge-Kutta factor, 0.375, is within 2% of the exact exp(-1).
# After it, two: only the slow tissues are left and the step need only be
# stable.  Each gas's system is similar to a symmetric matrix, so its
# eigenvalues are real and negative, and the classical Runge-Kutta method is
# stable on the negative real axis to 2.785 time constants; 2 leaves a margin
# for the coupling between the gases, which the rate allows for without
# bounding it exactly.  At 70 kg and a ventilation of 4.6 to 12 L/min, with
# or without nitrous oxide, the fastest rate is 4.2 to 7.5 per minute (it is
# isoflurane's, which every washout carries), so the two steps are 0.42 to
# 0.75 and 1.05 to 1.9 time constants, and both stand (gasCoupledRecovery()).
GAS_RECOVERY_ACCURATE_STEP <- 1
GAS_RECOVERY_STABLE_STEP <- 2

# The least a step may be, minutes, whatever the rate: 1.5 seconds, so that a
# washout runs at most 57,600 steps to the end of its day.  Without it the
# exponential step followed the coupling's rate down without limit: a cardiac
# output of 5,000 L/min (gasWashout(cardiacOutput =)) made it 0.0007 minute
# and the search ran for longer than a minute, and the step had no floor at
# all (Carbon's fourth UI audit, 2026-11-15, H9).  Nothing the app can enter
# reaches it: the coupling's rate is largest in the lightest patient the app
# takes (0.1 kg, whose cardiac output is 10.3 times its lung volume a
# minute), where even a vaporiser at 100% after nitrous oxide gives under 39
# per minute, a step above 0.0258 minute.  Why the exponential step stays
# stable at the floor, see gasCoupledRecovery().
GAS_RECOVERY_MIN_STEP <- 0.025

#' The coupled washout's step
#'
#' \code{GAS_RECOVERY_STEP_FIRST_HOUR} for the first hour of washout and
#' \code{GAS_RECOVERY_STEP_LATER} after, each held to
#' \code{GAS_RECOVERY_ACCURATE_STEP} (first hour) or
#' \code{GAS_RECOVERY_STABLE_STEP} (after) time constants of \code{rate}, and
#' never less than \code{GAS_RECOVERY_MIN_STEP}.
#'
#' @param tau minutes of washout so far
#' @param rate the fastest rate the method samples, per minute
#' @returns the step, minutes
#' @keywords internal
gasRecoveryStep <- function(tau, rate)
{
  h <- if (tau < 60) {
    min(GAS_RECOVERY_STEP_FIRST_HOUR, GAS_RECOVERY_ACCURATE_STEP / rate)
  } else {
    min(GAS_RECOVERY_STEP_LATER, GAS_RECOVERY_STABLE_STEP / rate)
  }
  max(h, GAS_RECOVERY_MIN_STEP)
}


#' Coefficients of the exponential Runge-Kutta step of the coupled washout
#'
#' Cox and Matthews' fourth-order exponential time differencing (ETDRK4): for
#' \code{dy/dt = B y + g(y)} the linear part is propagated exactly and only
#' \code{g} is sampled, at four stages, so the step's stability does not
#' depend on \code{B}.  \code{B} is block diagonal, one block per gas; each
#' block's phi functions come from one matrix exponential of the augmented
#' matrix \code{[[hA, I, 0, 0], [0, 0, I, 0], [0, 0, 0, I], [0, 0, 0, 0]]},
#' whose top row of blocks is \code{exp(hA)}, \code{phi1}, \code{phi2} and
#' \code{phi3} (\code{expmPade()}, correct when \code{A} is singular, as it is
#' with no ventilation).
#'
#' Cox SM, Matthews PC.  Exponential time differencing for stiff systems.
#' \emph{J Comput Phys} 2002;176:430-455.
#'
#' @param blocks the per-gas system matrices, for column-vector states
#' @param h the step, minutes
#' @returns the step's matrices, transposed for row-vector states: \code{E}
#'   and \code{E2}, the propagators over the step and the half step;
#'   \code{P2}, the half step's \code{(h/2) phi1}; and \code{F1}, \code{F2},
#'   \code{F3}, the full step's weights on its stages
#' @keywords internal
gasEtdCoefficients <- function(blocks, h)
{
  n <- nrow(blocks[[1]])
  m <- n * length(blocks)
  out <- lapply(c(E = 0, E2 = 0, P2 = 0, F1 = 0, F2 = 0, F3 = 0),
                function(x) matrix(0, m, m))
  I <- diag(n)
  Z <- matrix(0, n, n)
  for (b in seq_along(blocks)) {
    A <- blocks[[b]]
    full <- expmPade(rbind(cbind(h * A, I, Z, Z), cbind(Z, Z, I, Z),
                           cbind(Z, Z, Z, I), cbind(Z, Z, Z, Z)))
    half <- expmPade(rbind(cbind(h / 2 * A, I), cbind(Z, Z)))
    p1 <- full[1:n, n + 1:n]
    p2 <- full[1:n, 2 * n + 1:n]
    p3 <- full[1:n, 3 * n + 1:n]
    j <- (b - 1) * n + 1:n
    out$E[j, j]  <- t(full[1:n, 1:n])
    out$E2[j, j] <- t(half[1:n, 1:n])
    out$P2[j, j] <- t(h / 2 * half[1:n, n + 1:n])
    out$F1[j, j] <- t(h * (p1 - 3 * p2 + 4 * p3))
    out$F2[j, j] <- t(h * (p2 - 2 * p3))
    out$F3[j, j] <- t(h * (4 * p3 - p2))
  }
  out
}


#' Time until a concentration falls to a threshold, by coupled forward simulation
#'
#' Turns the gases in \code{off} off at each time point in turn, with no
#' rebreathing, and integrates the washout forward until \code{target} -- any
#' linear combination of the gas tensions -- comes down through the threshold
#' for the last time.
#'
#' The equations are the engine's own (R/advanceClosedFormGas.R) with the
#' patient inspiring fresh gas and nothing else.  For each gas, over the
#' alveolar, vessel-rich, muscle and fat compartments,
#'
#'   dy/dt = A y + (VA / Va) F e1 + coupling,
#'
#' with F the fresh-gas tension of that gas -- zero for the gases turned off,
#' unchanged for the agents left running, and whatever the air flow carries for
#' nitrogen -- and the coupling term the engine's: with u the summed uptake of
#' all the gases, the alveolar tension gains (u / Va) F when u is positive and
#' (u / Va) y_alv when it is negative.  u is linear in the state, so the
#' right-hand side is a few matrix products.
#'
#' Every time point is integrated at once, as rows of one matrix, by classical
#' fourth-order Runge-Kutta.  The step is 0.1 min for the first hour of washout,
#' where the crossing nearly always is, and 0.25 min after.  That serves while
#' each step is within a time constant (first hour) or two (after) of the
#' system's fastest rate (\code{GAS_RECOVERY_ACCURATE_STEP},
#' \code{GAS_RECOVERY_STABLE_STEP}), as it is at 70 kg and any ordinary
#' ventilation.  A faster system is advanced instead by exponential time
#' differencing (\code{gasEtdCoefficients()}), which solves the linear part
#' exactly and samples only the coupling, with the same steps: a 3 kg patient
#' ventilated at 4.6 L/min has a rate of 36 per minute, and the fixed
#' Runge-Kutta step, outside its region of stability, grew without bound and
#' failed ("NAs are not allowed in subscripted assignments"; Carbon's third
#' UI audit, 2026-11-15, H2).  Shortening the Runge-Kutta step instead would
#' cost a step per time constant, without limit: the dose table takes a
#' ventilation of up to \code{MAX_DOSE_VALUE} L/min.  Neither step is ever
#' less than \code{GAS_RECOVERY_MIN_STEP}, which bounds the work of a row at
#' 57,600 steps (Carbon's fourth UI audit, 2026-11-15, H9).  Where the floor
#' holds the exponential step at more than the coupling's time constant (a
#' cardiac output hundreds of times the body's, which only a caller of
#' \code{gasWashout(cardiacOutput =)} can give), it stays stable.  The
#' coupling is driven by the summed uptake, and the exchange between lungs and
#' blood, which the step solves exactly, holds the uptake to what the
#' ventilation brings and takes away, whatever the cardiac output; the part of
#' the coupling that does grow with the cardiac output, its response to the
#' alveolar tensions, is a fraction (the alveolar tensions' sum over 100) of
#' the exchange that damps those tensions.  On a single mode the exponential
#' Runge-Kutta step contracts at every step length when the sampled rate is
#' below the solved one (checked to steps of 10^7 time constants).  Checked
#' against the full engine to a cardiac output of 5,000,000 L/min
#' (tests/testthat/test-ui-audit-carbon4-gas.R).
#'
#' A row is finished only when it is proved that it cannot rise above the
#' threshold again within the day: the washout of each gas \code{target}
#' weighs, with the coupling left out, is an upper bound on its coupled
#' washout, and the row is finished when that bound, a sum of exponentials, is
#' at or below the threshold on a grid of times to a day and, rigorously,
#' between its points (\code{gasWashoutBound()}; the argument is in the
#' comments above it).  Until 2026-11-15 a row was finished once it was below
#' the threshold and falling, which the muscle and fat behind the brain can
#' contradict (H9).  A tissue still filling from the alveoli when the agent is
#' turned off -- below the threshold but about to rise through it -- is
#' followed over the top and back down, as before.  A row still running at
#' the end of the day reads \code{MINS_PER_DAY} if it is above the threshold
#' and its last downward crossing otherwise, as \code{recoveryCalc()} reads
#' its horizon.
#'
#' The plot shows a hundred points, so a simulation with more time points than
#' \code{maxPoints} is sampled and the result interpolated.  The sample
#' includes the first and last time point of every interval of constant
#' settings, and the interpolation stays within each interval, because the time
#' until threshold jumps where a setting changes; until 2026-11-15 it was
#' interpolated across the change (H9).
#'
#' Provenance: Claude Code (Claude Fable 5.1), 2026-10-05, at the request of
#' Steven L. Shafer.  Verified on R 4.6.1 against the same manoeuvre in the full
#' engine by tests/testthat/test-gas-recovery.R.
#'
#' @param washout output of \code{gasWashout()} with \code{rebreathing = FALSE}
#' @param off names of the gases turned off
#' @param target numeric weights on the state, four per soluble gas in the
#'   order of \code{names(washout$state)}: alveolar, vessel-rich, muscle, fat
#' @param threshold one value, or one per time point
#' @param maxPoints the most time points to integrate from
#' @returns numeric vector of minutes, one per time point
#' @keywords internal
gasCoupledRecovery <- function(washout, off, target, threshold, maxPoints = 120)
{
  nT <- length(washout$Time)
  threshold <- rep_len(threshold, nT)
  props  <- getGasProperties()
  gases  <- names(washout$state)
  body   <- washout$body
  Qco    <- washout$Qco
  Va     <- body$V_alveolar
  nG     <- length(gases)
  alvCol <- (seq_len(nG) - 1) * 4 + 1          # alveolar column of each gas
  volatiles <- setdiff(potentAgents(), "nitrousOxide")

  # Summed uptake as a linear function of the state:
  #   u = sum_g lambda_g Qco (alveolar - mixed venous) / 100
  uWeight <- numeric(4 * nG)
  for (k in seq_len(nG)) {
    lb <- props$lambda_blood[props$gas == gases[k]]
    uWeight[alvCol[k] + 0:3] <- lb * Qco / 100 *
      c(1, -body$f_brain, -body$f_muscle, -body$f_fat)
  }

  # The time points to start from, and their states side by side: alveolar and
  # the three tissues of each gas.
  idx <- gasRecoverySample(washout$interval, maxPoints)
  Y0 <- do.call(cbind, lapply(gases, function(g) washout$state[[g]][idx, 2:5, drop = FALSE]))
  thr0 <- threshold[idx]
  out <- rep(NA_real_, length(idx))
  out[is.na(thr0) | thr0 <= 0] <- 0

  for (iv in unique(washout$interval[idx]))
  {
    rows <- which(washout$interval[idx] == iv & is.na(out))
    if (length(rows) == 0) next
    t0 <- washout$changes[iv]
    s  <- gasSettingsAt(washout$bySetting, t0, washout$deadSpace)

    # Fresh gas once `off` is off.  The carrier flows that remain keep their
    # proportions (the total is turned up, not the mixture changed), the
    # vaporisers left on still displace their share of it, and if nothing is
    # left flowing it is oxygen.
    flow <- vapply(c(air = "air", oxygen = "oxygen", nitrousOxide = "nitrousOxide"),
                   function(g) if (g %in% off) 0 else settingAt(washout$bySetting[[g]], t0),
                   numeric(1))
    if (sum(flow) == 0) flow[["oxygen"]] <- 1
    vap <- vapply(volatiles, function(g)
      if (g %in% off) 0 else settingAt(washout$bySetting[[g]], t0), numeric(1))
    carrier <- max(0, 1 - sum(vap) / 100)
    Fin <- stats::setNames(numeric(nG), gases)
    for (g in intersect(volatiles, gases)) Fin[[g]] <- vap[[g]]
    if ("nitrousOxide" %in% gases)
      Fin[["nitrousOxide"]] <- 100 * carrier * flow[["nitrousOxide"]] / sum(flow)
    if ("nitrogen" %in% gases)
      Fin[["nitrogen"]] <- 100 * carrier * AIR_FRACTION_N2 * flow[["air"]] / sum(flow)

    # Block-diagonal system matrix, transposed for row-vector states, and the
    # constant inflow.
    Bt <- matrix(0, 4 * nG, 4 * nG)
    inflow <- numeric(4 * nG)
    blocks <- vector("list", nG)
    fastest <- 0
    for (k in seq_len(nG)) {
      A <- gasSystemSoluble(props[props$gas == gases[k], ], body, s$Q, s$VA, Qco,
                            Ffgf = 0, totUptake = 0, circuit = "open")$A[2:5, 2:5]
      j <- alvCol[k] + 0:3
      Bt[j, j] <- t(A)
      blocks[[k]] <- A
      inflow[alvCol[k]] <- s$VA / Va * Fin[[k]]
      # A is similar to a symmetric matrix, so its eigenvalues are real.
      fastest <- max(fastest, abs(eigen(A, only.values = TRUE)$values))
    }
    FinAlv <- numeric(4 * nG); FinAlv[alvCol] <- Fin

    # What proves a row finished: the uncoupled washout of the gases the
    # target weighs, an upper bound on the coupled one (gasWashoutBound()).
    bound <- gasWashoutBound(blocks, target, Fin)

    # Oxygen consumed shrinks the alveolar gas too, net of the carbon dioxide
    # that replaces it; a constant, added to the uptake of the other gases.
    uOxygen <- if (isTRUE(washout$oxygenUptake))
      body$VO2 * (1 - GAS_RESPIRATORY_QUOTIENT) / Va else 0

    # The fastest rate of the uptake coupling.  Its derivative is the make-up
    # (or displaced) gas, a gas's tension times the summed uptake per alveolar
    # volume, whose own rate is at most Qco / Va times the sum of
    # lambda_blood x tension / 100 over the gases; the uptake itself, which
    # displaces alveolar gas at |u| / Va, is bounded by the same sum.  The
    # tensions are taken at their largest, over the rows' starting states and
    # the gas being breathed: a washout only lowers the gases turned off and
    # takes the others towards what is inspired.
    tension <- vapply(seq_len(nG), function(k)
      max(abs(Y0[rows, alvCol[k] + 0:3]), Fin[[k]]), numeric(1))
    lambdaBlood <- props$lambda_blood[match(gases, props$gas)]
    coupling <- 2 * Qco / Va * sum(lambdaBlood * tension) / 100 + uOxygen

    # Each step is the classical Runge-Kutta method's, as since 2026-10-05,
    # while it is within GAS_RECOVERY_ACCURATE_STEP (first hour) or
    # GAS_RECOVERY_STABLE_STEP (after) time constants of the whole system's
    # fastest rate.  Otherwise it is exponential time differencing's, which
    # solves the linear part exactly and samples only the coupling, so that
    # only the coupling's rate holds its step.  The fixed Runge-Kutta step was
    # unstable from a rate of 28 per minute, and 11 after the first hour: a
    # 3 kg patient ventilated at 4.6 L/min (36), 70 kg at 100 L/min (31), or
    # 0.5 kg at its own default ventilation (14), whose time until threshold
    # grew without bound and failed (Carbon's third UI audit, 2026-11-15,
    # H2).  Shortening that step instead would cost a step per time constant,
    # without limit: the dose table takes a ventilation of up to
    # MAX_DOSE_VALUE L/min.
    rate <- fastest + coupling
    etd <- list()

    # The coupling and the inflow: the derivative less the linear part.
    nonlinear <- function(Y) {
      u <- as.vector(Y %*% uWeight) / Va + uOxygen
      D <- matrix(inflow, nrow(Y), length(inflow), byrow = TRUE)
      D <- D + outer(pmax(u, 0), FinAlv)
      D[, alvCol] <- D[, alvCol] + pmin(u, 0) * Y[, alvCol, drop = FALSE]
      D
    }

    deriv <- function(Y) {
      u <- as.vector(Y %*% uWeight) / Va + uOxygen
      D <- Y %*% Bt
      D <- sweep(D, 2, inflow, `+`)
      # Uptake positive: make-up gas is drawn in at the fresh-gas tension.
      # Negative: alveolar gas is pushed out at the alveolar tension.
      D <- D + outer(pmax(u, 0), FinAlv)
      D[, alvCol] <- D[, alvCol] + pmin(u, 0) * Y[, alvCol, drop = FALSE]
      D
    }

    Y <- Y0[rows, , drop = FALSE]
    thr <- thr0[rows]
    val <- as.vector(Y %*% target)
    cross <- rep(0, length(rows))      # last downward crossing so far; 0 = none
    tau <- 0
    while (length(rows) > 0 && tau < MINS_PER_DAY)
    {
      # Fine while the alveoli are emptying; a longer step later, when only the
      # slow tissues are left, does not trouble the integrator.
      h <- if (tau < 60) GAS_RECOVERY_STEP_FIRST_HOUR else GAS_RECOVERY_STEP_LATER
      if (gasRecoveryStep(tau, rate) >= h) {
        k1 <- deriv(Y)
        k2 <- deriv(Y + h / 2 * k1)
        k3 <- deriv(Y + h / 2 * k2)
        k4 <- deriv(Y + h * k3)
        Y <- Y + h / 6 * (k1 + 2 * k2 + 2 * k3 + k4)
      } else {
        # Cox and Matthews' ETDRK4, in row form (gasEtdCoefficients()).
        h <- gasRecoveryStep(tau, coupling)
        key <- format(h, digits = 17)
        if (is.null(etd[[key]])) etd[[key]] <- gasEtdCoefficients(blocks, h)
        cf <- etd[[key]]
        g0 <- nonlinear(Y)
        a  <- Y %*% cf$E2 + g0 %*% cf$P2
        ga <- nonlinear(a)
        b  <- Y %*% cf$E2 + ga %*% cf$P2
        gb <- nonlinear(b)
        c4 <- a %*% cf$E2 + (2 * gb - g0) %*% cf$P2
        gc <- nonlinear(c4)
        Y <- Y %*% cf$E + g0 %*% cf$F1 + 2 * (ga + gb) %*% cf$F2 + gc %*% cf$F3
      }
      valNew <- as.vector(Y %*% target)

      # Came down through the threshold in this step: a straight line across it.
      down <- val > thr & valNew <= thr
      cross[down] <- tau + h * (val[down] - thr[down]) / (val[down] - valNew[down])

      # Finished: below the threshold, and proved unable to rise above it
      # again within the day.  Below the threshold and falling is not enough:
      # the muscle and fat behind the brain can bring it back (H9).
      done <- valNew <= thr
      if (any(done)) {
        done[done] <- if (is.null(bound)) FALSE else
          gasWashoutBelow(bound, Y[done, , drop = FALSE], thr[done])
      }
      if (any(done)) {
        out[rows[done]] <- cross[done]
        keep <- !done
        rows <- rows[keep]; Y <- Y[keep, , drop = FALSE]
        thr <- thr[keep]; valNew <- valNew[keep]; cross <- cross[keep]
      }
      val <- valNew
      tau <- tau + h
    }
    # Still running at the end of the day: a day if above the threshold, and
    # otherwise the last downward crossing, as recoveryCalc() reads its
    # horizon.
    out[rows] <- ifelse(!is.na(val) & val <= thr, cross, MINS_PER_DAY)
  }

  if (length(idx) == nT) return(out)
  # Interpolated within each interval of constant settings, never across a
  # change, where the time until threshold jumps (H9).  The sample holds every
  # interval's first and last point (gasRecoverySample()).
  res <- numeric(nT)
  for (iv in unique(washout$interval)) {
    at <- which(washout$interval == iv)
    from <- which(washout$interval[idx] == iv)
    res[at] <- if (length(from) == 1) out[from] else
      stats::approx(washout$Time[idx[from]], out[from], washout$Time[at], rule = 2)$y
  }
  res
}


# The time points gasCoupledRecovery() starts from: every point when there
# are at most maxPoints, and otherwise maxPoints spread evenly together with
# the first and last point of every interval of constant settings, so that
# no time is ever interpolated across a change of setting (Carbon's fourth UI
# audit, 2026-11-15, H9).  With no change of setting the sample is the one
# used since 2026-10-05.
gasRecoverySample <- function(interval, maxPoints)
{
  nT <- length(interval)
  if (nT <= maxPoints) return(seq_len(nT))
  first <- which(c(TRUE, interval[-1] != interval[-nT]))
  last  <- c(first[-1] - 1L, nT)
  sort(unique(c(round(seq(1, nT, length.out = maxPoints)), first, last)))
}


# -----------------------------------------------------------------------------
# When a row of the washout is finished (Carbon's fourth UI audit, 2026-11-15,
# H9)
# -----------------------------------------------------------------------------
# Until this date a row was finished as soon as its concentration was below
# the threshold and falling, on the reasoning that it would not be back.  That
# holds for one compartment, not for four.  Isoflurane 2.5% for three hours,
# then the vaporiser off and the lungs flushed at 25 L/min for five minutes,
# then a minute ventilation of 0.4 L/min: at the end of the flush the brain is
# at 0.55%, under a threshold of 0.6% and still falling, so the time read 0;
# but the barely ventilated alveoli fill from the mixed venous blood, which
# the loaded muscle keeps high, and the brain rises to 0.90% and is not back
# under 0.6% for 215 minutes.
#
# A row is now finished only when a rise above the threshold is shown to be
# impossible.  For a gas that is turned off (fresh-gas tension F = 0) the
# engine's coupling adds (u / Va) y_alv to its alveolar tension when the
# summed uptake u is negative, gas pushed out, and nothing when u is
# positive, the make-up gas being fresh gas, which carries none of it.  So,
# whatever u and the other gases do,
#
#     dy/dt = (A + min(u(t), 0) / Va e1 e1') y,
#
# with A the gas's own washout matrix.  A is compartmental: its off-diagonal
# entries are flows and never negative (it is Metzler), and a non-positive
# term on the diagonal keeps it so.  Then y(t + s) = Phi(s) y(t), where the
# transition matrix Phi is entrywise non-negative and entrywise no larger
# than exp(A s): z = exp(A s) e_i - Phi(s) e_i starts at 0 and obeys
# dz/ds = A z - min(u, 0) / Va e1 e1' Phi e_i, a Metzler system with a
# non-negative input, so z stays non-negative.  For non-negative weights c --
# a tension, or tensions over MAC summed --
#
#     c' y(t + s) <= c' Phi(s) y+(t) <= c' exp(A s) y+(t),   every s >= 0,
#
# y+ being the state with any negative rounding set to zero.  The right side
# is the gas's washout with the coupling left out, a sum of four exponentials
# per gas (summed over the gases c weighs).  When it stays at or below the
# threshold for a day from now, the row cannot rise above the threshold again
# within the day, and is finished.  The bound holds for the equations
# integrated here, whatever the coupling does, and it is close: for a
# volatile agent on its own the coupling is small and the bound nearly the
# washout itself.
#
# It is checked on a grid of times and, rigorously, between the grid's
# points: on [s_k, s_k+1] a term with a positive amplitude is largest at s_k
# and one with a negative amplitude at s_k+1 (gasWashoutBelow()).  The grid
# starts at a thousandth of the fastest time constant and grows by 10% a
# point to a day, so between two points the bound gives away at most
# (1.1 - 1) / e, 3.7%, of the negative amplitudes.  A row it does not clear
# at once it clears a few steps later, further below the threshold; the
# answer, the last downward crossing, is the same either way.
# -----------------------------------------------------------------------------

# The washout bound's grid: its first point after zero, in time constants of
# the fastest rate, and the ratio of neighbouring points.
GAS_BOUND_FIRST <- 1e-3
GAS_BOUND_RATIO <- 1.1

# The bound, prepared once per interval of constant settings: for each gas
# the target weighs, the modes of its own washout matrix, and the grid's
# exponentials.  NULL where no bound holds -- a negative weight, or a weighed
# gas still being breathed, whose make-up gas then carries it in -- and the
# rows then run to the end of the day.  `blocks` are the per-gas matrices,
# for column-vector states, and `Fin` the fresh-gas tensions.
gasWashoutBound <- function(blocks, target, Fin)
{
  if (any(target < 0)) return(NULL)
  weighed <- which(vapply(seq_along(blocks), function(k)
    any(target[(k - 1) * 4 + 1:4] != 0), logical(1)))
  if (length(weighed) == 0 || any(Fin[weighed] != 0)) return(NULL)

  modes <- lapply(weighed, function(k) {
    A <- blocks[[k]]
    # A = C^-1 G, with C the compartments' capacities and G symmetric, so
    # C^1/2 A C^-1/2 is symmetric: its eigenvectors are orthonormal, and no
    # ill-conditioned matrix is inverted.  The capacities, relative to the
    # alveoli's, are A[1, j] / A[j, 1].
    C <- rep(1, 4)
    linked <- c(FALSE, A[2:4, 1] > 0)
    C[linked] <- A[1, linked] / A[linked, 1]
    S <- diag(sqrt(C)) %*% A %*% diag(1 / sqrt(C))
    e <- eigen((S + t(S)) / 2, symmetric = TRUE)
    j <- (k - 1) * 4 + 1:4
    list(cols    = j,
         rate    = pmax(-e$values, 0),
         toModes = diag(sqrt(C)) %*% e$vectors,             # row states to modes
         weight  = as.vector(target[j] %*% diag(1 / sqrt(C)) %*% e$vectors))
  })
  rate <- unlist(lapply(modes, `[[`, "rate"))
  first <- if (max(rate) > 0) GAS_BOUND_FIRST / max(rate) else Inf
  s <- if (first < MINS_PER_DAY) {
    n <- ceiling(log(MINS_PER_DAY / first) / log(GAS_BOUND_RATIO))
    c(0, exp(seq(log(first), log(MINS_PER_DAY), length.out = n + 1)))
  } else {
    c(0, MINS_PER_DAY)
  }
  list(modes = modes,
       left  = exp(-outer(rate, s[-length(s)])),
       right = exp(-outer(rate, s[-1])))
}

# The rows of Y (row states, four per gas) whose bound stays at or below
# their threshold thr for a day: those that cannot rise above it again.
gasWashoutBelow <- function(bound, Y, thr)
{
  alpha <- do.call(cbind, lapply(bound$modes, function(m)
    sweep(pmax(Y[, m$cols, drop = FALSE], 0) %*% m$toModes, 2, m$weight, `*`)))
  U <- pmax(alpha, 0) %*% bound$left + pmin(alpha, 0) %*% bound$right
  rowSums(U > thr | is.na(U)) == 0
}
