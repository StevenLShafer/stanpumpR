# -----------------------------------------------------------------------------
# Provenance
# ----------
# Drafted by Claude Code (Claude Opus 5), 2026-09-03, at the request of
# Steven L. Shafer.
#
# This is a deliberate REIMPLEMENTATION OF GAS MAN'S OWN INTEGRATION SCHEME,
# transcribed from GasDoc.cpp::Calc and GasDoc.cpp::CalcUptake in the Gas Man
# source released under GPL-3.0 at github.com/rasman/gasmanonline.  No code is
# copied; the algorithm is restated in R from the published source.
#
# WHY THIS EXISTS ALONGSIDE advanceClosedFormGas()
# ------------------------------------------------
# Shafer's plan, 2026-09-03: start from an implementation as close to identical
# with Gas Man as we can make it, so that agreement with Gas Man output
# validates the whole chain -- parameters, structure and arithmetic together.
# Only then move away from that baseline, in documented steps: parameters
# corrected against the peer-reviewed literature, and the per-compartment
# splitting below replaced by the fully coupled matrix exponential in
# advanceClosedFormGas().  Each step auditable in git.
#
# So this file is not meant to be the better routine.  It is meant to be the
# SAME routine.  Where Gas Man does something questionable it is reproduced
# here, with the oddity noted rather than corrected.
#
# STATUS: run and verified on R 4.6.1 by tests/testthat/test-gasman-baseline.R.
# NOT YET compared against Gas Man's own CSV export, which is the point of it.
# -----------------------------------------------------------------------------
#
# THE ALGORITHM, as Gas Man states it
# ===================================
#
# GasDoc.cpp:692 --
#   "Each compartment uses the exact solution to its first-order linear ODE:
#      P(t+dt) = P_target * (1 - exp(-k*dt)) + P(t) * exp(-k*dt)
#    where k = effective_flow / (volume * solubility).
#    This is sometimes called differential extrapolation -- it is NOT Euler
#    forward-difference and NOT Runge-Kutta."
#
# Compartment order is Gas Man's: CKT, ALV, VRG, MUS, FAT, VEN.
#
# Effective flows, all carrying the compartment's solubility:
#
#     effFlow[CKT]    = FGF * lambda[CKT]          lambda[CKT] = 1 (gas phase)
#     effFlow[ALV]    = VA  * lambda[ALV]          lambda[ALV] = 1 (gas phase)
#     bloodFlow       = lambdaBloodGas * CO
#     effFlow[tissue] = lambdaBloodGas * CO * ratio[tissue]
#
# Targets, ALL computed from the state at the start of the sub-step, so the
# update is simultaneous rather than Gauss-Seidel:
#
#     target[CKT] = (effFlow[CKT]*DEL + effFlow[ALV]*ALV) / (effFlow[CKT]+effFlow[ALV])
#     target[ALV] = (effFlow[ALV]*CKT + bloodFlow*VEN)    / (effFlow[ALV]+bloodFlow)
#     target[VRG] = target[MUS] = target[FAT] = ALV
#
# THE UPTAKE (CONCENTRATION AND SECOND GAS) CORRECTION
# ----------------------------------------------------
# Added to the alveolar numerator before dividing, and to the circuit numerator
# on emergence.  totUptake is summed across ALL gases and handed to each one, so
# nitrous oxide's uptake augments a volatile's alveolar tension.  This is the
# second gas effect; Gas Man calls it "Correct for constant lung capacity".
#
#     if uptakeEffect:
#         if totUptake > 0:  alvNumerator <- alvNumerator + CKT * totUptake
#         else:              alvNumerator <- alvNumerator + ALV * totUptake
#     if uptakeEffect and totUptake < 0:
#         cktNumerator <- cktNumerator - totUptake * ALV
#
# VENOUS
# ------
# VEN is not a differential compartment.  After the update it is set to the
# blood-flow-weighted mean of the NEW tissue tensions, so the alveolar target
# above uses the PREVIOUS sub-step's value -- a one-sub-step lag.  With
# recirculation disabled it is zero instead.
#
# ADAPTIVE SUB-STEPPING ("vernier")
# ---------------------------------
# Each base tick is advanced 2^nVernier times.  A sub-step is rejected, and the
# whole tick retried at twice the resolution, if any tension goes negative, or
# if more than 90% of the circuit or alveolar value decays in one sub-step
# within VERNIER_TICKS ticks of a settings change.  MAX_VERNIER caps it at 2^4.
#
# ODDITIES REPRODUCED RATHER THAN FIXED
# -------------------------------------
#   * Compartment volumes are NOT scaled by weight.  Weight enters only as
#     fWtFactor = weight/70 multiplying the uptake rate.  A 140 kg patient
#     therefore gets the same FRC and the same tissue volumes as a 70 kg one,
#     but twice the uptake.  Faithfully reproduced; flagged for the phase that
#     corrects parameters.
#   * Gas Man's uptake sum runs over VRG, MUS and FAT only, and when
#     recirculation is disabled it adds back CO * lambdaBloodGas * mixedVenous.
# -----------------------------------------------------------------------------


GASMAN_MAX_VERNIER   <- 5     # MAX_VERNIER in GasGlobal.h
GASMAN_VERNIER_TICKS <- 3     # VERNIER_TICKS
GASMAN_STD_WEIGHT    <- 70    # STD_WEIGHT

# Blood flow fractions, [Ratio] in gasman.ini: VRG 76, MUS 18, FAT 6 percent.
GASMAN_RATIO <- c(VRG = 0.76, MUS = 0.18, FAT = 0.06)

# Compartment volumes in litres, [Volumes] in gasman.ini.
GASMAN_VOLUME <- c(CKT = 8, ALV = 2.5, VRG = 6, MUS = 33, FAT = 14.5, VEN = 1)


#' Per-compartment solubilities for one gas, in Gas Man's ordering
#'
#' The circuit and alveolus are gas phase, so their solubility is 1.  The tissue
#' values are the tissue:gas coefficients from the properties table, and the
#' venous entry is the blood:gas coefficient, exactly as GasAnes.cpp sets them.
#'
#' @param props one row of \code{getGasProperties()}
#' @returns named numeric vector over CKT, ALV, VRG, MUS, FAT, VEN
#' @keywords internal
gasManSolubility <- function(props)
{
  c(CKT = 1, ALV = 1,
    VRG = props$tg_brain, MUS = props$tg_muscle, FAT = props$tg_fat,
    VEN = props$lambda_blood)
}


#' Uptake rate for one gas, after GasDoc.cpp::CalcUptake
#'
#' @param state named tension vector for this gas
#' @param sol per-compartment solubilities
#' @param lambdaBlood blood:gas coefficient
#' @param fExpTissue precomputed tissue exponentials for this sub-step
#' @param subdt sub-step length in minutes
#' @param weight patient weight in kg
#' @param CO cardiac output in L/min
#' @param recirculation whether venous return is modelled
#' @returns uptake in litres of agent per minute
#' @keywords internal
gasManUptake <- function(state, sol, lambdaBlood, fExpTissue, subdt,
                         weight, CO, recirculation)
{
  tissues <- c("VRG", "MUS", "FAT")

  # Amount moved into each tissue over this sub-step, divided by its length to
  # give a rate.  Gas Man divides by 100 because tensions are percentages.
  moved <- sum(GASMAN_VOLUME[tissues] * sol[tissues] *
                 (state[["ALV"]] - state[tissues]) * (1 - fExpTissue[tissues]))
  uptake <- moved * (weight / GASMAN_STD_WEIGHT) / subdt / 100

  if (!recirculation)
  {
    # With no venous return, agent carried away in blood never comes back, so
    # it counts as uptake.
    mixedVenous <- sum(state[tissues] * GASMAN_RATIO[tissues])
    uptake <- uptake + CO * lambdaBlood * mixedVenous / 100
  }

  unname(uptake)
}


#' Advance one gas by one sub-step, after GasDoc.cpp::Calc
#'
#' @param state named tension vector for this gas
#' @param sol per-compartment solubilities
#' @param lambdaBlood blood:gas coefficient
#' @param DEL delivered tension for this gas, percent of 1 atm
#' @param FGF total fresh gas flow, L/min
#' @param VA alveolar ventilation, L/min
#' @param CO cardiac output, L/min
#' @param fExpTissue precomputed tissue exponentials for this sub-step
#' @param subdt sub-step length in minutes
#' @param totUptake uptake summed over every gas, L/min
#' @param opts list of circuit, uptakeEffect, recirculation
#' @returns list of \code{state} and \code{ok}, where ok is FALSE when the
#'   sub-step should be rejected and retried at finer resolution
#' @keywords internal
gasManCalc <- function(state, sol, lambdaBlood, DEL, FGF, VA, CO,
                       fExpTissue, subdt, totUptake, opts)
{
  effCKT    <- FGF * sol[["CKT"]]
  effALV    <- VA  * sol[["ALV"]]
  bloodFlow <- lambdaBlood * CO

  target <- state
  fixedCKT <- FALSE

  if (opts$circuit == "open")
  {
    # Delivered gas determines the circuit outright.
    state[["CKT"]] <- DEL
    fixedCKT <- TRUE
  } else if (opts$circuit == "ideal")
  {
    # New mixture displaces exhaled gas with no mixing.  Note the explicit
    # threshold at FGF = VA: above it the circuit simply is the delivered gas.
    if (effCKT < effALV) {
      f <- effCKT / effALV
      state[["CKT"]] <- f * DEL + (1 - f) * state[["ALV"]]
    } else {
      state[["CKT"]] <- DEL
    }
    fixedCKT <- TRUE
  } else {
    g <- effCKT + effALV
    if (g != 0) {
      f <- effCKT * DEL + effALV * state[["ALV"]]
      if (opts$uptakeEffect && totUptake < 0)
        f <- f - totUptake * state[["ALV"]]
      target[["CKT"]] <- f / g
    } else {
      target[["CKT"]] <- state[["CKT"]]
    }
  }

  g <- effALV + bloodFlow
  if (g != 0) {
    f <- effALV * state[["CKT"]] + bloodFlow * state[["VEN"]]
    if (opts$uptakeEffect) {
      # Constant lung capacity: uptake of gas draws replacement in from the
      # circuit on induction, and pushes alveolar gas out on emergence.  This
      # is the concentration and second gas effect, because totUptake is the
      # sum over every gas.
      if (totUptake > 0) f <- f + state[["CKT"]] * totUptake
      else               f <- f + state[["ALV"]] * totUptake
    }
    target[["ALV"]] <- f / g
  } else {
    target[["ALV"]] <- state[["ALV"]]
  }

  target[["VRG"]] <- target[["MUS"]] <- target[["FAT"]] <- state[["ALV"]]

  fCKT <- exp(-subdt / (GASMAN_VOLUME[["CKT"]] * sol[["CKT"]]) * (effCKT + effALV))
  fALV <- exp(-subdt / (GASMAN_VOLUME[["ALV"]] * sol[["ALV"]]) * (effALV + bloodFlow))
  decay <- c(CKT = fCKT, ALV = fALV,
             VRG = fExpTissue[["VRG"]], MUS = fExpTissue[["MUS"]],
             FAT = fExpTissue[["FAT"]])

  ok <- TRUE
  for (cmp in c("CKT", "ALV", "VRG", "MUS", "FAT"))
  {
    if (cmp == "CKT" && fixedCKT) next
    state[[cmp]] <- target[[cmp]] * (1 - decay[[cmp]]) + state[[cmp]] * decay[[cmp]]
    if (state[[cmp]] < 0) ok <- FALSE
  }

  # More than 90% of the circuit or alveolar value gone in one sub-step means
  # the step is too coarse to trust.
  if (fCKT < 0.1 || fALV < 0.1) ok <- FALSE

  state[["VEN"]] <- if (opts$recirculation) {
    sum(state[c("VRG", "MUS", "FAT")] * GASMAN_RATIO)
  } else 0

  list(state = state, ok = ok)
}


#' Simulate the inhaled gases using Gas Man's own integration scheme
#'
#' A faithful restatement of Gas Man's stepping, for validating this package
#' against Gas Man output.  \code{advanceClosedFormGas()} remains the routine
#' the application uses: it solves the fully coupled system exactly and is far
#' faster.  This one exists to establish that we started from the same place.
#'
#' OXYGEN IS NOT SIMULATED HERE.  Gas Man's agent list is Desflurane, Ether,
#' Enflurane, Halothane, Isoflurane, Nitrogen, Nitrous Oxide, Sevoflurane and
#' Xenon; oxygen is not among them.  Modelling oxygen with a metabolic sink is
#' stanpumpR's addition, so it is left out of the baseline to keep the
#' comparison clean.
#'
#' @param gasDose dose-table rows for the gases, with Time, Drug and Dose
#' @param weight patient weight in kg
#' @param maximum simulation length in minutes
#' @param cardiacOutput cardiac output in L/min; defaults to Gas Man's 5
#' @param dt base tick in minutes.  Gas Man's m_fdt is m_cMSec_dx/60000, and
#'   m_cMSec_dx is 6000 ms -- its breath period, 10 breaths per minute -- so the
#'   default here is 0.1.  dt does not multiply ventilation; it only sets how
#'   stale the targets are between sub-steps.
#' @param circuit one of "semi-closed", "open" or "ideal"
#' @param uptakeEffect apply the constant-lung-capacity correction, which is the
#'   concentration and second gas effect.  Gas Man defaults it on.
#' @param recirculation model venous return.  Gas Man defaults it on.
#' @param resolution number of output time points
#'
#' @returns a list with \code{results} (tidy Drug/Time/Site/Y over the Gas Man
#'   compartments), \code{state}, \code{timeLine} and \code{maxVernier}
#' @export
advanceGasManBaseline <- function(gasDose, weight = 70, maximum = 60,
                                  cardiacOutput = 5, dt = 0.1,
                                  circuit = c("semi-closed", "open", "ideal"),
                                  uptakeEffect = TRUE, recirculation = TRUE,
                                  resolution = 601)
{
  circuit <- match.arg(circuit)
  opts <- list(circuit = circuit, uptakeEffect = uptakeEffect,
               recirculation = recirculation)

  props <- getGasProperties()
  agents <- props$gas[props$soluble]          # oxygen excluded: not a Gas Man agent

  sol <- lapply(agents, function(g) gasManSolubility(props[props$gas == g, ]))
  names(sol) <- agents
  lambdaBlood <- setNames(props$lambda_blood[match(agents, props$gas)], agents)

  if (is.null(gasDose) || nrow(gasDose) == 0)
    gasDose <- data.frame(Time = numeric(0), Drug = character(0), Dose = numeric(0))
  gasDose <- gasDose[order(gasDose$Time), , drop = FALSE]
  bySetting <- split(gasDose, gasDose$Drug)

  # Initial tensions.  Nitrogen starts at gasman.ini's Ambient of 80 for that
  # agent; everything else starts at zero.
  cmps <- c("CKT", "ALV", "VRG", "MUS", "FAT", "VEN")
  state <- lapply(agents, function(g)
    setNames(rep(if (g == "nitrogen") 80 else 0, length(cmps)), cmps))
  names(state) <- agents

  nTicks   <- max(1, ceiling(maximum / dt))
  timeLine <- seq(0, maximum, length.out = resolution)
  record   <- lapply(agents, function(g) matrix(NA_real_, length(timeLine), length(cmps),
                                                dimnames = list(NULL, cmps)))
  names(record) <- agents
  for (g in agents) record[[g]][1, ] <- state[[g]]

  maxVernierUsed <- 0
  nextOut <- 2

  for (tick in seq_len(nTicks))
  {
    t0 <- (tick - 1) * dt
    s  <- gasSettingsAt(bySetting, t0)

    nVernier <- 0
    repeat {
      saved <- state
      nSub  <- 2^nVernier
      subdt <- dt / nSub

      # Tissue exponentials are constant within a sub-step size.
      fExpTissue <- lapply(agents, function(g) {
        eff <- lambdaBlood[[g]] * cardiacOutput * GASMAN_RATIO
        exp(-eff * subdt / (GASMAN_VOLUME[c("VRG", "MUS", "FAT")] *
                              sol[[g]][c("VRG", "MUS", "FAT")]))
      })
      names(fExpTissue) <- agents

      ok <- TRUE
      for (nv in seq_len(nSub))
      {
        totUptake <- sum(vapply(agents, function(g)
          gasManUptake(state[[g]], sol[[g]], lambdaBlood[[g]], fExpTissue[[g]],
                       subdt, weight, cardiacOutput, recirculation),
          numeric(1)))

        for (g in agents)
        {
          r <- gasManCalc(state[[g]], sol[[g]], lambdaBlood[[g]],
                          DEL = s$Ffgf[[g]], FGF = s$Q, VA = s$VA,
                          CO = cardiacOutput, fExpTissue = fExpTissue[[g]],
                          subdt = subdt, totUptake = totUptake, opts = opts)
          state[[g]] <- r$state
          if (!r$ok) ok <- FALSE
        }
        if (!ok) break
      }

      if (ok || nVernier + 1 >= GASMAN_MAX_VERNIER) break
      state <- saved                      # reject the tick and retry finer
      nVernier <- nVernier + 1
    }
    maxVernierUsed <- max(maxVernierUsed, nVernier)

    t1 <- tick * dt
    while (nextOut <= length(timeLine) && timeLine[nextOut] <= t1 + 1e-12)
    {
      for (g in agents) record[[g]][nextOut, ] <- state[[g]]
      nextOut <- nextOut + 1
    }
  }
  # Any output points past the last tick hold the final state.
  for (g in agents) {
    miss <- is.na(record[[g]][, 1])
    if (any(miss))
      record[[g]][miss, ] <- matrix(state[[g]], sum(miss), length(cmps),
                                    byrow = TRUE)
  }

  results <- do.call(rbind, lapply(agents, function(g)
    do.call(rbind, lapply(cmps, function(cmp) data.frame(
      Drug = g, Time = timeLine, Site = cmp, Y = record[[g]][, cmp],
      stringsAsFactors = FALSE)))))

  list(results = results, state = record, timeLine = timeLine,
       maxVernier = maxVernierUsed)
}
