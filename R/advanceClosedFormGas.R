# -----------------------------------------------------------------------------
# Provenance
# ----------
# Drafted by Claude Code (Claude Opus 5), 2026-09-02, at the request of
# Steven L. Shafer.  Implements the Gas Man(R)-style inhaled-gas model described
# in R/gasProperties.R.  Gas Man is closed source; this is written from the
# published model structure, not from its code.
#
# STATUS: run and verified on R 4.6.1 by tests/testthat/test-gas-engine.R --
# the closed-form advance agrees with an independent RK4 integration of the same
# ODEs, single-compartment analytic limits are reproduced, and steady states
# match hand calculation.  NOT YET validated against Gas Man 4.2 output; that
# requires the fixture grid (see the TODO(fixture) markers in gasProperties.R).
# -----------------------------------------------------------------------------
#
# THE EQUATIONS
# =============
#
# State, per soluble gas i (nitrous oxide, sevoflurane, isoflurane, nitrogen),
# all as gas tensions in % of 1 atmosphere:
#
#     y_i = [ F_circ , F_alv , F_brain , F_muscle , F_fat ]
#
# Segment inputs, held constant between dose-table change points:
#
#     Q    = Q_air + Q_O2 + Q_N2O           total fresh gas flow, L/min
#     VA                                    alveolar ventilation, L/min
#     Qco                                   cardiac output, L/min
#     F_fgf,i                               fresh-gas fraction of gas i, %
#
# Fresh gas composition.  The vaporisers add vapour to the fresh gas stream, so
# the carrier gases are diluted by the vapour they displace:
#
#     carrier    = 1 - (F_vap,sevo + F_vap,iso)/100
#     F_fgf,O2   = 100 * carrier * (Q_O2 + 0.2093 Q_air) / Q
#     F_fgf,N2   = 100 * carrier * (0.7807 Q_air)        / Q
#     F_fgf,N2O  = 100 * carrier * (Q_N2O)               / Q
#     F_fgf,sevo = F_vap,sevo         (already a % of 1 atm)
#
# (1) CIRCUIT.  Fresh gas enters at the flowmeter/vaporiser composition and
#     leaves through the pop-off at circuit composition; exhaled gas returns at
#     alveolar composition.  The VA(F_alv - F_circ) term IS the rebreathing:
#     at high Q the circuit is flushed and F_circ -> F_fgf, at low Q it is
#     pulled toward F_alv.  At steady state
#         F_circ = (Q F_fgf + VA F_alv) / (Q + VA),
#     a smooth weighted average, with no threshold at Q = VA.
#
#         V_circ dF_circ/dt = Q (F_fgf - F_circ) + VA (F_alv - F_circ)
#
# (2) ALVEOLAR, soluble gases.  Ventilation exchanges with the circuit; blood
#     removes gas in proportion to the alveolar-to-mixed-venous gradient.
#     Mixed venous tension is the blood-flow-weighted mean of tissue tensions:
#
#         F_v = f_brain F_brain + f_muscle F_muscle + f_fat F_fat
#
#         V_alv dF_alv/dt = VA (F_circ - F_alv) - lambda_b Qco (F_alv - F_v)
#
# (3) TISSUES, t in {brain, muscle, fat}, flow-limited (perfusion-limited).
#     Tissue capacity is V_t * lambda_t/gas; delivery is Q_t * lambda_b/gas
#     times the arterial-to-tissue gradient, with Q_t = f_t * Qco:
#
#         V_t lambda_t/gas dF_t/dt = Q_t lambda_b/gas (F_alv - F_t)
#
# (4) OXYGEN is gas phase only -- it is consumed metabolically and binds
#     haemoglobin nonlinearly, so it has no partition coefficient and no tissue
#     compartment.  A constant sink VO2 (L/min) removes volume from the
#     alveolar gas.  This sink exists whether or not nitrous oxide is present.
#
#         V_circ dF_circ/dt = Q (F_fgf - F_circ) + VA (F_alv - F_circ)
#         V_alv  dF_alv/dt  = VA (F_circ - F_alv) - 100 VO2
#
#     At steady state this gives F_alv,O2 = F_circ,O2 - 100 VO2 / VA, about
#     14-15% on room air at 4 L/min -- the correct alveolar oxygen tension falls
#     out of the mass balance rather than being asserted.
#
# (5) MAC, summed over the potent agents, using the BRAIN (vessel-rich group)
#     tension and age-adjusted MAC:
#
#         MAC_total = sum_i F_brain,i / MAC_i(age)
#
# LINEARITY AND THE METHOD OF SOLUTION
# ====================================
# For fixed (Q, VA, Qco) every equation above is linear in the state, so each
# segment is
#         dy/dt = A y + b
# with A and b constant.  This has the exact solution
#         y(t+dt) = e^{A dt} y(t) + A^{-1}(e^{A dt} - I) b
# obtained here in one step from the augmented matrix
#         M = [[A, b], [0, 0]]
#         expm(M dt) = [[ e^{A dt} , A^{-1}(e^{A dt}-I) b ],
#                       [ 0        , 1                    ]]
# a form that stays correct when A is singular.  No numerical ODE solver is
# used, preserving the closed-form invariant of the rest of the package.
#
# Note what is and is not linear.  The VAPORISER SETTING enters only b, so
# doubling it doubles the whole trajectory, brain compartment included.  FRESH
# GAS FLOW AND VENTILATION enter A: they change the eigenvalues, hence the
# shape of the curve and not merely its scale.  That is why no coefficient can
# be precomputed and then scaled by a rate, the way p_coef_infusion_l1 is for
# intravenous drugs -- A must be re-exponentiated whenever those settings
# change.  Since they are piecewise-constant values the user types into the
# dose table, the segments are known in advance and no iteration is required.
#
# The gases are independent of one another GIVEN Q and VA, so the full system is
# block diagonal and each gas is advanced as its own 5x5 (2x2 for oxygen).  They
# are still coupled through the dose table, because total fresh gas flow is the
# sum of the air, oxygen and nitrous oxide rows -- which is why they must be
# simulated as one group and cannot be diffed drug by drug the way
# processdoseTable() diffs the intravenous drugs.
#
# The one term that would break linearity is the volume loss from bulk nitrous
# oxide uptake (the concentration effect).  It is deliberately not implemented:
# whether Gas Man models it at all is unresolved.  The switch is carried through
# the call signature so that turning it on later is a flag, not a rewrite.
# -----------------------------------------------------------------------------


#' Matrix exponential by scaling and squaring with a [6/6] Pade approximant
#'
#' Written out rather than taken from a package, to keep the engine
#' dependency-free in the same spirit as \code{cube.R}.
#'
#' @param A a square numeric matrix
#' @returns the matrix exponential of \code{A}
#' @keywords internal
expmPade <- function(A)
{
  n <- nrow(A)
  normA <- max(rowSums(abs(A)))
  if (normA == 0) return(diag(n))

  # Scale so that the norm of A/2^s is comfortably below 1, where the Pade
  # approximant is accurate, then undo the scaling by repeated squaring.
  s  <- max(0, ceiling(log2(normA)) + 1)
  As <- A / 2^s

  I  <- diag(n)
  N  <- I
  D  <- I
  Ak <- I
  ck <- 1
  q  <- 6
  for (k in 1:q)
  {
    ck <- ck * (q - k + 1) / ((2 * q - k + 1) * k)
    Ak <- Ak %*% As
    N  <- N + ck * Ak
    D  <- D + (-1)^k * ck * Ak
  }
  E <- solve(D, N)

  for (i in seq_len(s)) E <- E %*% E
  E
}


#' System matrix and forcing vector for one soluble gas
#'
#' Implements equations (1), (2) and (3) of the header for the state ordering
#' \code{c(F_circ, F_alv, F_brain, F_muscle, F_fat)}.
#'
#' @param props one row of \code{getGasProperties()}
#' @param body output of \code{getGasBody()}
#' @param Q total fresh gas flow, L/min
#' @param VA alveolar ventilation, L/min
#' @param Qco cardiac output, L/min
#' @param Ffgf fresh-gas fraction of this gas, percent of 1 atm
#' @returns a list with \code{A} (5x5) and \code{b} (length 5)
#' @keywords internal
gasSystemSoluble <- function(props, body, Q, VA, Qco, Ffgf)
{
  lb  <- props$lambda_blood
  ltg <- gasPartitionTissueGas(props)

  Vc <- body$V_circuit
  Va <- body$V_alveolar

  # Blood flow to each tissue group
  Qb <- body$f_brain  * Qco
  Qm <- body$f_muscle * Qco
  Qf <- body$f_fat    * Qco

  # Tissue rate constants: Q_t * lambda_b / (V_t * lambda_t/gas)
  kb <- Qb * lb / (body$V_brain  * ltg[["brain"]])
  km <- Qm * lb / (body$V_muscle * ltg[["muscle"]])
  kf <- Qf * lb / (body$V_fat    * ltg[["fat"]])

  A <- matrix(0, 5, 5)

  # (1) circuit
  A[1, 1] <- -(Q + VA) / Vc
  A[1, 2] <-  VA / Vc

  # (2) alveolar.  The mixed-venous term redistributes lambda_b * Qco across the
  # three tissue states in proportion to their share of cardiac output.
  A[2, 1] <-  VA / Va
  A[2, 2] <- -(VA + lb * Qco) / Va
  A[2, 3] <-  lb * Qco * body$f_brain  / Va
  A[2, 4] <-  lb * Qco * body$f_muscle / Va
  A[2, 5] <-  lb * Qco * body$f_fat    / Va

  # (3) tissues
  A[3, 2] <-  kb;  A[3, 3] <- -kb
  A[4, 2] <-  km;  A[4, 4] <- -km
  A[5, 2] <-  kf;  A[5, 5] <- -kf

  b <- c(Q * Ffgf / Vc, 0, 0, 0, 0)

  list(A = A, b = b)
}


#' System matrix and forcing vector for oxygen
#'
#' Implements equation (4) for the state ordering \code{c(F_circ, F_alv)}.
#'
#' @param body output of \code{getGasBody()}
#' @param Q total fresh gas flow, L/min
#' @param VA alveolar ventilation, L/min
#' @param Ffgf fresh-gas oxygen fraction, percent of 1 atm
#' @returns a list with \code{A} (2x2) and \code{b} (length 2)
#' @keywords internal
gasSystemOxygen <- function(body, Q, VA, Ffgf)
{
  Vc <- body$V_circuit
  Va <- body$V_alveolar

  A <- matrix(0, 2, 2)
  A[1, 1] <- -(Q + VA) / Vc
  A[1, 2] <-  VA / Vc
  A[2, 1] <-  VA / Va
  A[2, 2] <- -VA / Va

  # Metabolic consumption is a constant volume sink, so it enters b, not A.
  # The factor of 100 converts L/min of oxygen into percent of alveolar volume.
  b <- c(Q * Ffgf / Vc, -100 * body$VO2 / Va)

  list(A = A, b = b)
}


#' Advance one linear segment exactly
#'
#' Solves \code{dy/dt = A y + b} over \code{dt} by the augmented-matrix form
#' described in the header, which stays correct when \code{A} is singular.
#'
#' @param y state vector at the start of the segment
#' @param A system matrix
#' @param b forcing vector
#' @param dt segment length in minutes
#' @returns the state vector at the end of the segment
#' @keywords internal
advanceGasSegment <- function(y, A, b, dt)
{
  if (dt <= 0) return(y)
  n <- length(y)
  M <- matrix(0, n + 1, n + 1)
  M[1:n, 1:n]   <- A
  M[1:n, n + 1] <- b
  E <- expmPade(M * dt)
  as.vector(E[1:n, 1:n] %*% y + E[1:n, n + 1])
}


#' Build a reusable propagator for a linear segment
#'
#' Returns the pair \code{(P, q)} such that \code{y(t+dt) = P y(t) + q} for
#' \code{dy/dt = A y + b}.  Because \code{P} and \code{q} depend only on
#' \code{(A, b, dt)} and not on the state, one propagator can be built per
#' interval and applied repeatedly across a uniform sub-grid -- which is what
#' keeps the engine fast enough to re-run on every dose-table edit.
#'
#' @param A system matrix
#' @param b forcing vector
#' @param dt step length in minutes
#' @returns list with \code{P} (matrix) and \code{q} (vector)
#' @keywords internal
gasPropagator <- function(A, b, dt)
{
  n <- length(b)
  M <- matrix(0, n + 1, n + 1)
  M[1:n, 1:n]   <- A
  M[1:n, n + 1] <- b
  E <- expmPade(M * dt)
  list(P = E[1:n, 1:n, drop = FALSE], q = E[1:n, n + 1])
}


#' Value of a step-function dose-table setting at a given time
#'
#' Fresh gas flows, vaporiser settings and ventilation all persist until
#' changed, exactly as an infusion rate does.  Before the first entry the
#' setting is zero.
#'
#' @param doseRows data frame with \code{Time} and \code{Dose} for one setting,
#'   or NULL
#' @param t time in minutes
#' @returns the value in force at \code{t}
#' @keywords internal
settingAt <- function(doseRows, t)
{
  if (is.null(doseRows) || nrow(doseRows) == 0) return(0)
  use <- which(doseRows$Time <= t + 1e-9)
  if (length(use) == 0) return(0)
  doseRows$Dose[use[length(use)]]
}


#' Settings in force at a given time, and the derived fresh-gas composition
#'
#' Collects the six dose-table settings and turns the three flowmeter rows plus
#' the two vaporiser rows into the fresh-gas fractions of equation set (0).
#'
#' @param split list of per-setting data frames, from \code{split()} on Drug
#' @param t time in minutes
#' @returns a list with the raw settings and the fresh-gas fractions
#' @keywords internal
gasSettingsAt <- function(split, t)
{
  Q_air  <- settingAt(split[["air"]],          t)
  Q_O2   <- settingAt(split[["oxygen"]],       t)
  Q_N2O  <- settingAt(split[["nitrousOxide"]], t)
  F_sevo <- settingAt(split[["sevoflurane"]],  t)
  F_iso  <- settingAt(split[["isoflurane"]],   t)
  VA     <- settingAt(split[["ventilation"]],  t)

  Q <- Q_air + Q_O2 + Q_N2O

  # Vapour displaces carrier gas.  At 2% sevoflurane this is a 2% correction;
  # small, but free to include and it keeps the fractions summing to 100.
  carrier <- 1 - (F_sevo + F_iso) / 100
  if (carrier < 0) carrier <- 0

  if (Q > 0)
  {
    Ffgf <- c(
      oxygen       = 100 * carrier * (Q_O2 + AIR_FRACTION_O2 * Q_air) / Q,
      nitrogen     = 100 * carrier * (AIR_FRACTION_N2 * Q_air) / Q,
      nitrousOxide = 100 * carrier * Q_N2O / Q
    )
  } else {
    Ffgf <- c(oxygen = 0, nitrogen = 0, nitrousOxide = 0)
  }
  Ffgf[["sevoflurane"]] <- F_sevo
  Ffgf[["isoflurane"]]  <- F_iso

  list(Q = Q, VA = VA, Ffgf = Ffgf)
}


#' Simulate the inhaled gases in a circle breathing system
#'
#' Closed-form solution of the Gas Man(R)-style model documented at the top of
#' this file.  All the gases are simulated together, because they share one
#' breathing circuit and one alveolar ventilation: the total fresh gas flow is
#' the sum of the air, oxygen and nitrous oxide rows, so changing any one of
#' them changes every gas trajectory.
#'
#' @param gasDose data frame of dose-table rows for the gases, with columns
#'   \code{Time} (minutes), \code{Drug} (one of air, oxygen, nitrousOxide,
#'   sevoflurane, isoflurane, ventilation) and \code{Dose} (L/min for the
#'   flows and ventilation, percent of 1 atm for the vaporisers).  Each row is
#'   a setting that persists until the next row for the same drug.
#' @param weight patient weight in kg
#' @param age patient age in years, used for the age adjustment of MAC
#' @param maximum length of the simulation in minutes
#' @param body geometry and flows, from \code{getGasBody()}.  Defaults to the
#'   weight-scaled standard.
#' @param cardiacOutput cardiac output in L/min.  Defaults to the covariate
#'   value in \code{body} (75 mL/kg).  Accepted per call so that a
#'   time-varying cardiac output can be added later without changing the
#'   engine; note that letting it vary would logically require the intravenous
#'   pharmacokinetics to respond to it as well, which stanpumpR does not model.
#' @param resolution number of output time points
#' @param concentrationEffect if TRUE, apply the volume-loss correction from
#'   bulk nitrous oxide uptake.  NOT IMPLEMENTED -- whether Gas Man models the
#'   concentration effect is unresolved, and setting this to TRUE is an error
#'   rather than a silent no-op.
#'
#' @returns a list with \code{results}, a tidy data frame of
#'   \code{Drug, Time, Site, Y} matching the shape returned by
#'   \code{simCpCe()}, where \code{Site} is "Alveolar" or "Brain" and MAC
#'   appears as \code{Drug == "MAC"}; and \code{state}, the full state
#'   trajectory as a matrix, for testing.
#'
#' @export
advanceClosedFormGas <- function(
  gasDose,
  weight = 70,
  age = 50,
  maximum = 60,
  body = NULL,
  cardiacOutput = NULL,
  resolution = 601,
  concentrationEffect = FALSE
)
{
  if (isTRUE(concentrationEffect))
    stop("The concentration effect (nitrous oxide volume loss) is not yet ",
         "implemented.  See the header of advanceClosedFormGas.R.")

  if (is.null(body)) body <- getGasBody(weight)
  Qco <- if (is.null(cardiacOutput)) body$Q_cardiac else cardiacOutput

  props <- getGasProperties()

  # Split the dose table by setting.  Rows for a setting must be time-ordered
  # for settingAt() to pick the last one in force.
  if (is.null(gasDose) || nrow(gasDose) == 0)
  {
    gasDose <- data.frame(Time = numeric(0), Drug = character(0),
                          Dose = numeric(0), stringsAsFactors = FALSE)
  }
  gasDose <- gasDose[order(gasDose$Time), , drop = FALSE]
  bySetting <- split(gasDose, gasDose$Drug)

  # Change points partition the simulation into intervals over which the
  # settings, and therefore A and b, are constant.  Within an interval the
  # output grid is made UNIFORM, so a single propagator can be built once and
  # then applied repeatedly.  That is the difference between one matrix
  # exponential per interval per gas and one per plotted point -- roughly a
  # hundredfold, which matters because this runs on every dose-table keystroke.
  changes <- sort(unique(c(0, gasDose$Time, maximum)))
  changes <- changes[changes >= 0 & changes <= maximum]
  if (length(changes) < 2) changes <- c(0, maximum)

  # Initial conditions: patient and circuit equilibrated with room air.  The
  # alveolar oxygen tension relaxes from the inspired value to its steady state
  # within the first minute or so, on the FRC/VA time constant.
  solubleGases <- props$gas[props$soluble]
  state <- list()
  for (g in solubleGases)
  {
    init <- if (g == "nitrogen") AIR_FRACTION_N2 * 100 else 0
    state[[g]] <- rep(init, 5)
  }
  state[["oxygen"]] <- rep(AIR_FRACTION_O2 * 100, 2)

  times  <- 0
  record <- list()
  for (g in props$gas) record[[g]] <- matrix(state[[g]], nrow = 1)

  for (iv in seq_len(length(changes) - 1))
  {
    t0  <- changes[iv]
    t1  <- changes[iv + 1]
    len <- t1 - t0
    if (len <= 0) next

    # Enough sub-steps to draw a smooth curve, proportional to the share of the
    # simulation this interval occupies.  Accuracy does not depend on this:
    # the advance is exact at every step size.
    nSub <- max(1, round(resolution * len / maximum))
    dt   <- len / nSub

    # Settings are read at the START of the interval and held across it, which
    # is what makes the interval linear and the advance exact.
    s <- gasSettingsAt(bySetting, t0)

    # Build one propagator per gas for this interval: y <- P y + q.
    prop <- list()
    for (g in props$gas)
    {
      if (g == "oxygen")
      {
        sys <- gasSystemOxygen(body, s$Q, s$VA, s$Ffgf[["oxygen"]])
      } else {
        sys <- gasSystemSoluble(props[props$gas == g, ], body,
                                s$Q, s$VA, Qco, s$Ffgf[[g]])
      }
      prop[[g]] <- gasPropagator(sys$A, sys$b, dt)
    }

    stepStates <- list()
    for (g in props$gas) stepStates[[g]] <- matrix(NA_real_, nSub, length(state[[g]]))

    for (k in seq_len(nSub))
    {
      for (g in props$gas)
      {
        state[[g]] <- as.vector(prop[[g]]$P %*% state[[g]] + prop[[g]]$q)
        stepStates[[g]][k, ] <- state[[g]]
      }
    }

    times <- c(times, t0 + dt * seq_len(nSub))
    for (g in props$gas) record[[g]] <- rbind(record[[g]], stepStates[[g]])
  }

  timeLine <- times
  nT  <- length(timeLine)
  out <- record

  # Assemble the reported series.  Alveolar tension is state 2 for every gas;
  # brain (vessel-rich group) tension is state 3 for the soluble gases.
  results <- data.frame()
  for (g in props$gas)
  {
    results <- rbind(results, data.frame(
      Drug = g, Time = timeLine, Site = "Alveolar", Y = out[[g]][, 2],
      stringsAsFactors = FALSE))
    if (g != "oxygen")
      results <- rbind(results, data.frame(
        Drug = g, Time = timeLine, Site = "Brain", Y = out[[g]][, 3],
        stringsAsFactors = FALSE))
  }

  # (5) MAC: the age-adjusted sum of brain tensions over the potent agents.
  macTotal <- rep(0, nT)
  for (g in props$gas[props$potent])
  {
    MAC <- macForAge(props$MAC40[props$gas == g], age)
    macTotal <- macTotal + out[[g]][, 3] / MAC
  }
  results <- rbind(results, data.frame(
    Drug = "MAC", Time = timeLine, Site = "MAC", Y = macTotal,
    stringsAsFactors = FALSE))

  list(results = results, state = out, timeLine = timeLine)
}


#' Simulate every inhaled gas in a dose table as one coupled group
#'
#' The gases share one breathing circuit and one alveolar ventilation, so they
#' cannot be simulated -- or cached -- drug by drug the way the intravenous
#' drugs are.  Total fresh gas flow is the sum of the air, oxygen and nitrous
#' oxide rows, so changing any one of them changes every gas trajectory.  This
#' function therefore takes the whole dose table and simulates all of the gas
#' rows together in a single call.
#'
#' The gas rows must also be kept away from \code{recalculatePK()} and
#' \code{simCpCe()}: the former would call \code{eval(call("air", ...))} and
#' fail, since the gases have no \code{drugs_*.R} covariate function, and the
#' latter converts every dose to a mass, which a gas tension is not.
#'
#' @param doseTable a cleaned dose table with \code{Drug}, \code{Time} in
#'   minutes, and \code{Dose}.  Non-gas rows are ignored.
#' @param weight patient weight in kg
#' @param age patient age in years
#' @param maximum simulation length in minutes
#' @param cardiacOutput optional override in L/min; defaults to 75 mL/kg
#'
#' @returns \code{NULL} if the dose table contains no gases, otherwise the list
#'   returned by \code{advanceClosedFormGas()}
#' @export
simulateGases <- function(doseTable, weight = 70, age = 50, maximum = 60,
                          cardiacOutput = NULL)
{
  if (is.null(doseTable) || nrow(doseTable) == 0) return(NULL)

  gasRows <- doseTable[isGasDrug(doseTable$Drug), , drop = FALSE]
  if (nrow(gasRows) == 0) return(NULL)

  gasDose <- data.frame(
    Time = as.numeric(gasRows$Time),
    Drug = as.character(gasRows$Drug),
    Dose = as.numeric(gasRows$Dose),
    stringsAsFactors = FALSE
  )
  gasDose <- gasDose[!is.na(gasDose$Time) & !is.na(gasDose$Dose), , drop = FALSE]
  if (nrow(gasDose) == 0) return(NULL)

  advanceClosedFormGas(
    gasDose,
    weight        = weight,
    age           = age,
    maximum       = maximum,
    cardiacOutput = cardiacOutput
  )
}
