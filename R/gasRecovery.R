# -----------------------------------------------------------------------------
# "Time until threshold" for the inhaled agents and MAC
# -----------------------------------------------------------------------------
# Provenance
# ----------
# Drafted by Claude Code (Claude Fable 5.1), 2026-10-05, at the request of
# Steven L. Shafer, extending the intravenous "time until threshold" feature to
# the inhaled gases.  Run and verified on R 4.6.1 by
# tests/testthat/test-gas-recovery.R, which checks it against stopping delivery
# in the full engine and simulating on.
#
# What is computed
# ----------------
# For every moment in the simulation: if delivery stopped NOW, how long until
# the concentration falls to the threshold?  For an intravenous drug "delivery
# stops" means no more drug is given.  For a gas it is taken to mean:
#
#   * every vaporiser is turned off and nitrous oxide is turned off;
#   * the TOTAL fresh gas flow stays what it was, now as oxygen;
#   * ventilation stays what it was.
#
# That is, the anaesthetist closes the vaporiser and changes nothing else.  It
# is the neutral assumption, not the clinical one -- at emergence the flow is
# usually turned up, which is faster -- and it is what makes the answer a
# property of the current state rather than of a guess about what happens next.
#
# How
# ---
# With the agent's inflow zero, each gas obeys dy/dt = A y, where A is the same
# five-compartment system matrix the engine uses (circuit, alveolar, vessel-rich
# group, muscle, fat).  A does not depend on the state, so the washout from any
# starting state y0 is
#
#     y(tau) = V exp(Lambda tau) V^-1 y0
#
# and any one compartment is a sum of five decaying exponentials.  That is the
# form the intravenous code already solves, so the same root-finder,
# recoveryCalc(), serves both.  One eigen-decomposition per gas per interval of
# constant settings covers every time point in the interval.
#
# The approximation
# -----------------
# The engine couples the gases through their summed uptake (the concentration
# and second gas effect), which makes the true washout very slightly nonlinear.
# That coupling is LEFT OUT here, because including it means a forward
# simulation from every time point, which is far too slow to run on every edit.
# Measured against stopping delivery in the full engine (see the test file), the
# difference is within a few percent of the time reported; it is largest when
# nitrous oxide is washing out alongside a volatile agent.
#
# Which concentration
# -------------------
#   * For each agent: the VESSEL-RICH GROUP (brain) tension, which is the gas
#     counterpart of the effect site, as in gasDrugEntries().
#   * For MAC: the ALVEOLAR tensions, summed over the potent agents as fractions
#     of their age-adjusted MAC, because that is how the plotted MAC series is
#     defined.  Alveolar gas falls faster than brain, so this is the earlier of
#     the two possible answers.
# -----------------------------------------------------------------------------

# Default threshold for the MAC series, in multiples of MAC.  About one third of
# MAC is where patients respond to command on emergence ("MAC-awake" is 0.33 to
# 0.4 MAC for the volatile agents).  The per-agent thresholds are the endCe
# column of drugDefaults_global.csv, as for every other drug, and are editable
# in the Drug Thresholds dialog; this one has no row there yet.
#
# NOT YET CONFIRMED BY SHAFER (Claude Code, 2026-10-05).
GAS_MAC_THRESHOLD <- 0.33


#' Washout of every soluble gas, as sums of exponentials
#'
#' For each time point of a gas simulation, and each soluble gas, expresses the
#' washout that would follow if delivery stopped at that moment as five decaying
#' exponentials per compartment.  See the file header for the assumptions.
#'
#' @param sim output of \code{simulateGases()}
#' @param gasDose the gas rows of the dose table: Time, Drug, Dose
#' @param weight patient weight in kg
#' @param cardiacOutput cardiac output in L/min; defaults to the body's
#' @returns a list with \code{Time}, \code{rate} (per gas, an nT x 5 matrix of
#'   positive decay rates per minute) and \code{amplitude} (per gas, an
#'   nT x 5 x 5 array: time point, compartment, exponential), or NULL if there
#'   is no simulation
#' @export
gasWashout <- function(sim, gasDose, weight = 70, cardiacOutput = NULL)
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

  # Settings are constant between change points, so A is too.  Index each time
  # point by the interval it falls in; a point exactly on a change takes the
  # NEW settings, which are the ones that would be left running.
  changes  <- sort(unique(c(0, gasDose$Time)))
  interval <- findInterval(Time + 1e-9, changes)

  rate <- list(); amplitude <- list()
  for (g in gases)
  {
    rate[[g]]      <- matrix(0, nT, 5)
    amplitude[[g]] <- array(0, c(nT, 5, 5))
    y <- sim$state[[g]]
    for (iv in unique(interval))
    {
      s <- gasSettingsAt(bySetting, changes[iv])
      # Agent inflow off, total flow and ventilation as they are, no coupling.
      A <- gasSystemSoluble(props[props$gas == g, ], body, s$Q, s$VA, Qco,
                            Ffgf = 0, totUptake = 0)$A
      e <- eigen(A)
      # A is similar to a symmetric matrix, so its eigenvalues are real; any
      # imaginary part is rounding.
      V  <- Re(e$vectors)
      lam <- pmax(-Re(e$values), 0)
      Vinv <- solve(V)

      use <- which(interval == iv)
      cf  <- y[use, , drop = FALSE] %*% t(Vinv)            # nUse x 5: V^-1 y0
      rate[[g]][use, ] <- matrix(lam, length(use), 5, byrow = TRUE)
      for (j in 1:5)
        amplitude[[g]][use, j, ] <- sweep(cf, 2, V[j, ], `*`)
    }
  }
  list(Time = Time, rate = rate, amplitude = amplitude)
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
#' age-adjusted MAC, summed -- exactly as the MAC series is built.
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
