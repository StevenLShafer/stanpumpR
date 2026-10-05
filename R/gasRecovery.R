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
# concentration comes down through the threshold.  The gases stay coupled as
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
#'   gas flow is left as it is, and whatever rebreathing the circuit allows at
#'   that flow goes on.
#' @param circuit the circuit model, as in \code{advanceClosedFormGas()}.  Only
#'   matters when \code{rebreathing = TRUE}: without rebreathing there is no
#'   circuit to model.
#' @param deadSpace dead space as a fraction of minute ventilation, as in
#'   \code{advanceClosedFormGas()}; must be the value the simulation used.
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
                       deadSpace = GAS_DEAD_SPACE_FRACTION)
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
      s <- gasSettingsAt(bySetting, changes[iv], deadSpace)
      # Agent inflow off, no coupling.  "open" is the no-rebreathing limit:
      # the patient inspires fresh gas only, whatever the flow.
      A <- gasSystemSoluble(props[props$gas == g, ], body, s$Q, s$VA, Qco,
                            Ffgf = 0, totUptake = 0, circuit = washoutCircuit,
                            MV = s$MV)$A[keep, keep, drop = FALSE]
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
       interval = interval, body = body, Qco = Qco, deadSpace = deadSpace)
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
#' where the crossing nearly always is, and 0.25 min after; the fastest rate in
#' the system is under 4 per minute, so both are comfortably fine.  A row is
#' finished once it is below the threshold and falling, so that a tissue still
#' filling from the alveoli when the agent is turned off -- below the threshold
#' but about to rise through it -- is followed over the top and back down.
#'
#' The plot shows a hundred points, so a simulation with more time points than
#' \code{maxPoints} is sampled and the result interpolated.
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
  idx <- if (nT <= maxPoints) seq_len(nT) else unique(round(seq(1, nT, length.out = maxPoints)))
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
    for (k in seq_len(nG)) {
      A <- gasSystemSoluble(props[props$gas == gases[k], ], body, s$Q, s$VA, Qco,
                            Ffgf = 0, totUptake = 0, circuit = "open")$A[2:5, 2:5]
      j <- alvCol[k] + 0:3
      Bt[j, j] <- t(A)
      inflow[alvCol[k]] <- s$VA / Va * Fin[[k]]
    }
    FinAlv <- numeric(4 * nG); FinAlv[alvCol] <- Fin

    deriv <- function(Y) {
      u <- as.vector(Y %*% uWeight) / Va
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
      h <- if (tau < 60) 0.1 else 0.25
      k1 <- deriv(Y)
      k2 <- deriv(Y + h / 2 * k1)
      k3 <- deriv(Y + h / 2 * k2)
      k4 <- deriv(Y + h * k3)
      Y <- Y + h / 6 * (k1 + 2 * k2 + 2 * k3 + k4)
      valNew <- as.vector(Y %*% target)

      # Came down through the threshold in this step: a straight line across it.
      down <- val > thr & valNew <= thr
      cross[down] <- tau + h * (val[down] - thr[down]) / (val[down] - valNew[down])

      # Finished: below the threshold and falling, so it will not be back.
      done <- valNew <= thr & valNew <= val
      if (any(done)) {
        out[rows[done]] <- cross[done]
        keep <- !done
        rows <- rows[keep]; Y <- Y[keep, , drop = FALSE]
        thr <- thr[keep]; valNew <- valNew[keep]; cross <- cross[keep]
      }
      val <- valNew
      tau <- tau + h
    }
    out[rows] <- MINS_PER_DAY
  }

  if (length(idx) == nT) return(out)
  stats::approx(washout$Time[idx], out, washout$Time, rule = 2)$y
}
