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
#     MV                                    minute ventilation, L/min: what the
#                                           user enters as "ventilation"
#     VA   = MV (1 - d)                     alveolar ventilation, L/min, with
#                                           d the dead-space fraction, 0.3
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
# 0.2093 + 0.7807 is 0.99, not 1 (AIR_FRACTION_O2 and AIR_FRACTION_N2 in
# gasProperties.R).  The remaining 1% of air, mostly argon, is not carried:
# it is neither lumped into nitrogen nor normalised away.  An air flow counts
# in full in Q but puts only 99% of itself into the carried fractions, and the
# initial state (room air) is 78.07% nitrogen and 20.93% oxygen, summing to 99.
#
# The vapour displaces carrier gas within Q; it does not add to Q.  Gas Man has
# an option (off by default) that instead adds the vapour's own volume, raising
# the effective fresh gas flow by 1 / (1 - F_vap/100).  That option has no
# counterpart here.  (Both notes: Claude Code, Claude Opus 5.5, 2026-10-09,
# from audit findings F08 and F13.)
#
# (1) CIRCUIT.  Two models, chosen by the `circuit` argument.
#
#     "ideal" -- THE DEFAULT since 2026-10-05, at Shafer's direction: "the ideal
#     circuit is real life".  A circle system's valves keep exhaled gas out of
#     the inspiratory limb, so every litre of fresh gas is inspired and exhaled
#     gas only makes up the shortfall:
#
#         Q >= MV:   F_circ = F_fgf                           no rebreathing
#         Q <  MV:   F_circ = f F_fgf + (1 - f) F_alv,        f = Q / (VA + Q d)
#
#     There is a threshold at Q = MV, the MINUTE ventilation, above which the
#     patient inspires fresh gas and nothing else, and there is no circuit
#     volume and so no lag.  (With the uptake coupling and oxygen consumption
#     on, as they are by default, (4a) below moves the threshold to
#     Q = MV + max(u, 0), u the summed uptake; gasCircuitBlend() is the rule
#     the engine runs.  Q = MV is the u = 0 case and the usual rule of thumb.)
#
#     Where f comes from.  Below the threshold the patient inspires all the
#     fresh gas and makes up the rest with exhaled gas:
#         MV F_circ = Q F_fgf + (MV - Q) F_exhaled.
#     Exhaled gas is not alveolar gas.  A fraction d of every breath only
#     reaches the dead space and comes back unchanged, so
#         MV F_exhaled = VA F_alv + d MV F_circ.
#     Eliminating F_exhaled gives the line above; f rises from 0 at Q = 0 to
#     exactly 1 at Q = MV.
#
#     With d = 0 this is Gas Man's "Ideal" circuit (GasDoc.cpp, IDEAL_CKT),
#     whose threshold is at Q = VA because Gas Man has no dead space: its
#     ventilation IS alveolar ventilation.  The dead space, and with it the
#     threshold at minute ventilation, is stanpumpR's (Shafer, 2026-10-05: "the
#     user sets minute ventilation, not alveolar ventilation ... set dead space
#     at 30% of minute ventilation").  Pass deadSpace = 0 to compare with Gas
#     Man.
#
#     Reference supplied by Shafer: Feldman JM, Lampotang S, Hendrickx J.  Is
#     rebreathing prevented when FGF equals MV?  APSF, 20 October 2022.
#     https://www.apsf.org/article/is-rebreathing-prevented-when-fgf-equals-mv/
#     It supports the threshold as a rule of thumb, and adds two cautions that
#     this model does not capture: the real threshold sits slightly above or
#     below minute ventilation depending on the workstation, and after a change
#     of setting the gas already in the circuit takes a little time to mix out.
#     The article's threshold is MINUTE ventilation, which is where this model
#     puts it.
#
#     F_circ is then not a
#     state but a function of F_alv, and substituting it into (2) leaves a
#     four-state system per gas; the circuit entry of the state vector is filled
#     in afterwards so that everything downstream still finds it.
#
#     "semi-closed" -- Gas Man's own default, and this engine's until that date.
#     The whole circuit is ONE WELL-MIXED VOLUME: fresh gas enters at the
#     flowmeter/vaporiser composition and leaves through the pop-off at circuit
#     composition; exhaled gas returns at alveolar composition.
#
#         V_circ dF_circ/dt = Q (F_fgf - F_circ) + VA (F_alv - F_circ)
#
#     At steady state F_circ = (Q F_fgf + VA F_alv) / (Q + VA), a smooth
#     weighted average with no threshold at Q = VA: because fresh and exhaled
#     gas are stirred together before any is vented, some fresh gas is always
#     thrown away and some exhaled gas always rebreathed, at any flow.  With
#     Q = VA the patient still inspires half exhaled gas.  The two models agree
#     as Q -> 0 and differ most at Q = VA.  Kept for comparison with Gas Man.
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
# (4a) OXYGEN AND THE GAS VOLUME (2026-10-05; `oxygenUptake`, on by default).
#     The sink above removes oxygen from the oxygen fraction but not from the
#     VOLUME, so with nothing else done the fractions stop adding up: 0.3 L/min
#     of oxygen with 1 L/min of nitrous oxide settled at 5% oxygen, 74% nitrous
#     oxide and no nitrogen.  Oxygen consumed is volume lost, exactly as agent
#     taken up is, and it enters the same coupling term, with carbon dioxide
#     accounted for where it actually goes:
#
#       * In the ALVEOLI, carbon dioxide replaces most of the oxygen taken up.
#         Alveolar gas therefore shrinks by VO2 - VCO2 = VO2 (1 - RQ), and that
#         is what is added to the summed uptake u of the other gases.
#       * Carbon dioxide is not carried as a gas.  Its alveolar fraction is
#         taken at its steady value, 100 VCO2 / VA (about 5%), so the fractions
#         that ARE carried sum to 100 less that in the alveoli.
#       * In the CIRCUIT, the absorber strips the carbon dioxide from the
#         exhaled gas that is rebreathed.  That gas shrinks by the carbon
#         dioxide it held, c_E = VCO2 / MV of its volume, and the fractions of
#         everything else in it rise to match; inspired gas sums to 100.
#
#     The ideal-circuit blend becomes, with u+ the uptake when positive,
#         Q >= MV + u+ :  F_circ = F_fgf
#         otherwise    :  F_circ = f F_fgf + g F_alv
#             k = (MV + u+ - Q) / (MV (1 - c_E))      rebreathed, per unit exhaled
#             f = Q / D,   g = k VA / D,   D = (MV + u+) - k (MV - VA)
#     which is the earlier f = Q / (VA + Q d), g = 1 - f when u and c_E are zero.
#     The patient now inspires MV + u+ and exhales MV, so the fresh gas flow
#     that stops rebreathing is the minute ventilation plus what is taken up.
#
#     In the steady state the gas vented is the fresh gas less the oxygen
#     consumed: with 0.3 + 1.0 L/min in and 0.21 L/min consumed, 1.09 L/min
#     leaves as 92% nitrous oxide and 8% oxygen (dry, carbon dioxide aside).
#
#     Gas Man does none of this: it has no oxygen.  Pass oxygenUptake = FALSE to
#     compare with it.
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
# The one term that would break linearity is the volume change from bulk gas
# uptake -- the concentration and second gas effect.  It is NOT implemented
# here yet.
#
# UPDATE (2026-10-09, Claude Code, Claude Opus 5.5, from audit finding F07):
# that sentence is historical.  The term is implemented (`uptakeEffect`, on by
# default), and it does break linearity: the summed uptake depends on the
# state.  The engine keeps each SUB-STEP linear by holding the summed uptake
# (and the ideal-circuit blend that depends on it) at its value at the start of
# the sub-step, advances the sub-step exactly by matrix exponential, and
# recomputes the uptake for the next one.  Propagation within a sub-step is
# exact; the coupling between sub-steps is first order in dt.  The result is
# therefore NOT independent of the step size, which is set by `resolution`
# (about maximum / 600 by default).  Measured against the same engine at a
# 40-fold finer step: under 0.003 percentage points in the scenarios without
# nitrous oxide; with 4 L/min nitrous oxide in 6 L/min, the first plotted
# alveolar values run low by about 1.5% of their value on a 60-minute plot,
# 6% on 240 minutes and 13% on 1440, fading below 1% within 1, 2 and 12
# minutes.  In the audit's matched ideal-circuit sevoflurane case the
# 30-minute alveolar value moves from 1.71176014% with 31 steps to
# 1.71178091% with 2401, against 1.71178112% from an independent ODE solver.
# See the Integration section of inst/help/models/gas-engine.md.
#
# CORRECTION (2026-09-03): an earlier version of this comment said it was
# unresolved whether Gas Man models it, and a later one said Gas Man does not.
# Both were wrong.  Gas Man does model it, in GasDoc.cpp::Calc, under the names
# "uptake effect" and "Correct for constant lung capacity" rather than
# "concentration effect" -- which is why a grep for the usual term found
# nothing and produced the wrong conclusion:
#
#     if (samp.m_bUptEnb) {
#         if (fTotUptake > 0.0F)  f += fResults[CKT] * fTotUptake;   // inducing
#         else                    f += fResults[ALV] * fTotUptake;   // emerging
#     }
#     fTarget[ALV] = f / g;
#
# and fTotUptake is summed across ALL gases before being handed to each one, so
# nitrous oxide's uptake augments a volatile's alveolar tension.  That is the
# second gas effect, and it is on by default (m_bRtnEnb = m_bUptEnb = true).
#
# It must therefore be implemented for the Gas Man baseline to be a baseline.
# -----------------------------------------------------------------------------


#' Matrix exponential by scaling and squaring with a \[6/6\] Pade approximant
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


# -----------------------------------------------------------------------------
# The uptake coupling: the concentration and second gas effect
# -----------------------------------------------------------------------------
# Gas taken up by blood leaves a volume deficit in the alveolus.  On induction
# that deficit draws replacement gas in from the circuit, concentrating whatever
# else is there; on emergence the flow reverses.  Because the deficit is summed
# over ALL gases, nitrous oxide's uptake augments a volatile's alveolar tension.
# That is the second gas effect, and Gas Man models it (m_bUptEnb, defaulting
# true) -- see the note at the head of this file.
#
# Uptake of gas i into tissue t, in L/min, is the volume the tissue gains:
#
#     V_t * lambda_t/gas * dF_t/dt / 100
#       = V_t * lambda_t/gas * k_t * (F_alv - F_t) / 100
#       = Q_t * lambda_blood * (F_alv - F_t) / 100        since k_t = Q_t*lb/(V_t*ltg)
#
# Summing over the three tissue groups collapses to the classical Fick form
#
#     uptake_i = lambda_blood,i * Qco * (F_alv,i - F_ven,i) / 100
#
# with F_ven the blood-flow-weighted mean tissue tension.  The /100 is because
# tensions are carried as percent of one atmosphere.
#
# NOTE ON OXYGEN.  Oxygen is carried here as a two-state gas with no tissue
# compartments, so it contributes nothing to the sum, and it does not RECEIVE
# the term either.  Gas Man does not model oxygen at all, so there is no
# reference for what it should do, and applying the correction to it would be an
# untestable deviation.  Physically the volume loss must concentrate alveolar
# oxygen as well, so this is a known simplification rather than a settled
# question -- revisit it once the soluble gases are validated.

#' Blood-flow-weighted mixed venous tension for one soluble gas
#'
#' @param y state vector for one soluble gas, \code{[circ, alv, brain, muscle, fat]}
#' @param body geometry from \code{getGasBody()}
#' @returns mixed venous tension, percent of one atmosphere
gasMixedVenous <- function(y, body)
{
  y[[3]] * body$f_brain + y[[4]] * body$f_muscle + y[[5]] * body$f_fat
}


#' Total uptake summed over every soluble gas
#'
#' Gas Man's \code{fTotUptake}: positive while gas is being taken up, negative
#' during washout.  Oxygen is excluded -- see the note above.
#'
#' @param state named list of per-gas state vectors
#' @param props gas property table from \code{getGasProperties()}
#' @param body geometry from \code{getGasBody()}
#' @param Qco cardiac output, L/min
#' @returns total uptake in L/min
gasTotalUptake <- function(state, props, body, Qco)
{
  total <- 0
  for (g in props$gas)
  {
    if (g == "oxygen") next
    y  <- state[[g]]
    lb <- props$lambda_blood[props$gas == g]
    total <- total + lb * Qco * (y[[2]] - gasMixedVenous(y, body)) / 100
  }
  total
}


#' Fraction of inspired gas that is fresh gas, for the ideal circuit
#'
#' One when fresh gas flow meets or exceeds MINUTE ventilation -- no rebreathing
#' -- and \code{Q / (VA + Q d)} below that, with \code{d = 1 - VA / MV} the
#' dead-space fraction; see the file header for the derivation.  With no dead
#' space this is \code{Q / VA}, Gas Man's ideal circuit.  Always one for an open
#' circuit.
#'
#' @param Q total fresh gas flow, L/min
#' @param VA alveolar ventilation, L/min
#' @param circuit "ideal" or "open"
#' @param MV minute ventilation, L/min; defaults to \code{VA}, i.e. no dead space
#' @returns a number between 0 and 1
#' @keywords internal
gasFreshFraction <- function(Q, VA, circuit = "ideal", MV = VA)
{
  gasCircuitBlend(Q, VA, MV, circuit)$fresh
}


#' Inspired gas in the ideal circuit, as a blend of fresh and alveolar gas
#'
#' \code{F_circ = fresh * F_fgf + alveolar * F_alv}.  See (1) and (4a) in the
#' file header.  With no uptake and no carbon dioxide the two weights sum to
#' one; with carbon dioxide absorbed from the rebreathed gas they sum to a
#' little more, because the alveolar fractions they multiply sum to a little
#' less than 100.
#'
#' @param Q total fresh gas flow, L/min
#' @param VA alveolar ventilation, L/min
#' @param MV minute ventilation, L/min
#' @param circuit "ideal" or "open"
#' @param u summed uptake of gas from the alveoli, L/min; only a positive value
#'   adds to what is inspired
#' @param cE carbon dioxide as a fraction of exhaled gas, removed from whatever
#'   is rebreathed.  Capped at \code{GAS_MAX_EXHALED_CO2}, which keeps the blend
#'   physical at any ventilation.
#' @returns a list with \code{fresh} and \code{alveolar}
#' @keywords internal
gasCircuitBlend <- function(Q, VA, MV = VA, circuit = "ideal", u = 0, cE = 0)
{
  inspired <- MV + max(u, 0)
  if (circuit == "open" || MV <= 0 || Q >= inspired)
    return(list(fresh = 1, alveolar = 0))
  cE <- min(max(cE, 0), GAS_MAX_EXHALED_CO2)
  k <- (inspired - Q) / (MV * (1 - cE))
  D <- inspired - k * (MV - VA)
  # With cE capped and the dead space under half of the minute ventilation, D
  # is positive; this is a guard against a future change to either, not a
  # branch that runs.
  if (D <= 0) stop("ventilation and dead space do not permit a physical circuit blend")
  list(fresh = Q / D, alveolar = k * VA / D)
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
#' @param totUptake summed uptake of all the gases, L/min, for the coupling
#' @param circuit "ideal" (the default), "semi-closed" or "open"; see the
#'   header.  "open" is the ideal circuit with no rebreathing whatever the flow.
#' @param MV minute ventilation, L/min, which sets where rebreathing stops in
#'   the ideal circuit.  Defaults to \code{VA}: no dead space.
#' @param cE carbon dioxide as a fraction of exhaled gas, absorbed from whatever
#'   is rebreathed; see \code{gasCircuitBlend()}
#' @returns a list with \code{A} (5x5), \code{b} (length 5) and, for the ideal
#'   and open circuits, \code{fresh} and \code{alveolar}: the weights from
#'   which the circuit tension is \code{fresh * Ffgf + alveolar * F_alv}.  They
#'   are NULL for the semi-closed circuit, whose tension is a state.
#' @keywords internal
gasSystemSoluble <- function(props, body, Q, VA, Qco, Ffgf, totUptake = 0,
                             circuit = c("ideal", "semi-closed", "open"),
                             MV = VA, cE = 0)
{
  circuit <- match.arg(circuit)
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

  # (1) circuit.  On EMERGENCE (totUptake < 0) alveolar gas is displaced back
  # into the circuit, which Gas Man models by subtracting the term from the
  # circuit numerator:  if (tot_uptake < 0) f <- f - tot_uptake * ALV.  There is
  # no matching term on induction, when make-up gas flows the other way.
  A[1, 1] <- -(Q + VA) / Vc
  A[1, 2] <-  VA / Vc
  if (totUptake < 0) A[1, 2] <- A[1, 2] - totUptake / Vc

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

  # The uptake coupling.  Gas Man's Calc adds this to the alveolar numerator:
  #
  #     if (fTotUptake > 0) f += fResults[CKT] * fTotUptake;   // inducing
  #     else                f += fResults[ALV] * fTotUptake;   // emerging
  #
  # so on induction the make-up volume is drawn from the CIRCUIT and the term is
  # proportional to the circuit tension; on emergence alveolar gas is pushed out
  # and the term is proportional to the alveolar tension.  Either way it is
  # linear in the state, so it modifies A rather than b, and with totUptake held
  # fixed across a sub-step the system stays linear and the matrix exponential
  # stays exact over that step.
  if (totUptake > 0) {
    A[2, 1] <- A[2, 1] + totUptake / Va
  } else if (totUptake < 0) {
    A[2, 2] <- A[2, 2] + totUptake / Va
  }

  b <- c(Q * Ffgf / Vc, 0, 0, 0, 0)

  if (circuit == "semi-closed")
    return(list(A = A, b = b, fresh = NULL, alveolar = NULL))

  # IDEAL (or open) CIRCUIT.  The circuit tension is not a state but
  #     F_circ = f F_fgf + (1 - f) F_alv.
  # Everything the alveolus takes from the circuit -- ventilation, plus the
  # make-up gas drawn in when uptake is positive -- is A[2, 1], already
  # assembled above.  Substituting F_circ splits it: the fresh-gas share becomes
  # a constant inflow, and the rebreathed share comes straight back as alveolar
  # gas.  The circuit row and column are then empty, so the propagator leaves
  # that entry alone and the caller sets it from the line above.
  bl <- gasCircuitBlend(Q, VA, MV, circuit, totUptake, cE)
  fromCircuit <- A[2, 1]
  A[2, 2] <- A[2, 2] + fromCircuit * bl$alveolar
  A[1, ] <- 0
  A[, 1] <- 0
  b <- c(0, fromCircuit * bl$fresh * Ffgf, 0, 0, 0)

  list(A = A, b = b, fresh = bl$fresh, alveolar = bl$alveolar)
}


#' System matrix and forcing vector for oxygen
#'
#' Implements equation (4) for the state ordering \code{c(F_circ, F_alv)}.
#'
#' @param body output of \code{getGasBody()}
#' @param Q total fresh gas flow, L/min
#' @param VA alveolar ventilation, L/min
#' @param Ffgf fresh-gas oxygen fraction, percent of 1 atm
#' @param circuit "ideal" (the default), "semi-closed" or "open"
#' @param MV minute ventilation, L/min; defaults to \code{VA}, no dead space
#' @param totUptake summed uptake of gas from the alveoli, L/min.  Oxygen takes
#'   the same coupling as the soluble gases: make-up gas drawn in when it is
#'   positive, alveolar gas pushed out when it is negative.
#' @param cE carbon dioxide as a fraction of exhaled gas; see
#'   \code{gasCircuitBlend()}
#' @returns a list with \code{A} (2x2), \code{b} (length 2), \code{fresh} and
#'   \code{alveolar}, as for \code{gasSystemSoluble()}
#' @keywords internal
gasSystemOxygen <- function(body, Q, VA, Ffgf,
                            circuit = c("ideal", "semi-closed", "open"),
                            MV = VA, totUptake = 0, cE = 0)
{
  circuit <- match.arg(circuit)
  Vc <- body$V_circuit
  Va <- body$V_alveolar

  # What the alveolus draws from the circuit, and what uptake pushes back out.
  fromCircuit <- (VA + max(totUptake, 0)) / Va
  pushedOut   <- min(totUptake, 0) / Va

  if (circuit != "semi-closed")
  {
    # Ideal circuit: F_circ = f F_fgf + (1 - f) F_alv, so the alveolus sees
    #     V_alv dF_alv/dt = VA f (F_fgf - F_alv) - 100 VO2.
    # Below Q = MV the rebreathed share returns the patient's own oxygen-poor
    # gas.  With no dead space the steady state is F_fgf - 100 VO2 / Q: what is
    # delivered less what is consumed, as it must be.
    bl <- gasCircuitBlend(Q, VA, MV, circuit, totUptake, cE)
    A <- matrix(0, 2, 2)
    A[2, 2] <- -VA / Va + fromCircuit * bl$alveolar + pushedOut
    b <- c(0, fromCircuit * bl$fresh * Ffgf - 100 * body$VO2 / Va)
    return(list(A = A, b = b, fresh = bl$fresh, alveolar = bl$alveolar))
  }

  A <- matrix(0, 2, 2)
  A[1, 1] <- -(Q + VA) / Vc
  A[1, 2] <-  VA / Vc - min(totUptake, 0) / Vc
  A[2, 1] <-  fromCircuit
  A[2, 2] <- -VA / Va + pushedOut

  # Metabolic consumption is a constant volume sink, so it enters b, not A.
  # The factor of 100 converts L/min of oxygen into percent of alveolar volume.
  b <- c(Q * Ffgf / Vc, -100 * body$VO2 / Va)

  list(A = A, b = b, fresh = NULL, alveolar = NULL)
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
#' @param deadSpace dead space as a fraction of minute ventilation.  The
#'   "ventilation" row of the dose table is MINUTE ventilation, and alveolar
#'   ventilation is \code{MV * (1 - deadSpace)}.  Zero makes the row alveolar
#'   ventilation, as it is in Gas Man.
#' @returns a list with total fresh gas flow \code{Q}, minute ventilation
#'   \code{MV}, alveolar ventilation \code{VA}, and the fresh-gas fractions
#' @keywords internal
gasSettingsAt <- function(split, t, deadSpace = GAS_DEAD_SPACE_FRACTION)
{
  Q_air  <- settingAt(split[["air"]],          t)
  Q_O2   <- settingAt(split[["oxygen"]],       t)
  Q_N2O  <- settingAt(split[["nitrousOxide"]], t)
  MV     <- settingAt(split[["ventilation"]],  t)
  VA     <- MV * (1 - deadSpace)

  # Every volatile agent in the properties table has its vaporiser read here,
  # rather than from a hardcoded list.  Adding desflurane to the table
  # previously meant remembering to add it here too, and forgetting produced a
  # subscript error rather than a wrong number -- loud, but avoidable.  Nitrous
  # oxide is excluded because it arrives through a flowmeter, not a vaporiser.
  volatiles <- setdiff(potentAgents(), "nitrousOxide")
  F_vap <- vapply(volatiles, function(g) settingAt(split[[g]], t), numeric(1))

  Q <- Q_air + Q_O2 + Q_N2O

  # Vapour displaces carrier gas.  At 2% sevoflurane this is a 2% correction;
  # small, but free to include and it keeps the fractions summing to 100.
  carrier <- 1 - sum(F_vap) / 100
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
  for (g in volatiles) Ffgf[[g]] <- F_vap[[g]]

  list(Q = Q, VA = VA, MV = MV, Ffgf = Ffgf)
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
#'   value in \code{body} (Gas Man's 5 L/min at 70 kg, allometric).  Accepted per call so that a
#'   time-varying cardiac output can be added later without changing the
#'   engine; note that letting it vary would logically require the intravenous
#'   pharmacokinetics to respond to it as well, which stanpumpR does not model.
#' @param resolution number of output time points.  It also sets the sub-step
#'   over which the uptake coupling is held fixed, and so, when
#'   \code{uptakeEffect} is TRUE, the accuracy: the error is first order in the
#'   sub-step, about \code{maximum / resolution} (see the file header).
#' @param uptakeEffect if TRUE (the default, as in Gas Man, whose m_bUptEnb
#'   defaults true), couple the gases through their summed uptake, giving the
#'   concentration and second gas effect.  Set FALSE to isolate that term: with
#'   it off the gases do not influence one another at all, which is what makes
#'   the effect measurable as a difference rather than asserted.
#' @param circuit "ideal", the default: no rebreathing once fresh gas flow
#'   reaches minute ventilation, and no circuit volume.  "semi-closed": Gas
#'   Man's default, the circuit as one well-mixed volume.  See the file header.
#' @param oxygenUptake if TRUE (the default), oxygen consumed shrinks the gas
#'   volume: its net volume, after the carbon dioxide that replaces it in the
#'   alveoli, joins the summed uptake that couples the gases, oxygen itself
#'   takes that coupling, and the absorber removes carbon dioxide from
#'   rebreathed gas.  See (4a) in the file header.  FALSE restores the earlier
#'   behaviour, in which oxygen was a sink with no effect on volume; Gas Man has
#'   no oxygen, so comparisons with it pass FALSE.  Has no effect when
#'   \code{uptakeEffect} is FALSE, and is switched off for the semi-closed
#'   circuit, whose well-mixed-box model has no carbon dioxide absorber to put
#'   the volume balance in: that circuit exists only for comparison with Gas
#'   Man, which has no oxygen.
#' @param deadSpace dead space as a fraction of minute ventilation, 0.3 by
#'   default.  The "ventilation" rows are minute ventilation; alveolar
#'   ventilation is that times \code{1 - deadSpace}.  Pass 0 to treat the rows
#'   as alveolar ventilation, as Gas Man does.
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
  uptakeEffect = TRUE,
  circuit = c("ideal", "semi-closed"),
  deadSpace = GAS_DEAD_SPACE_FRACTION,
  oxygenUptake = TRUE
)
{
  circuit <- match.arg(circuit)
  # The semi-closed circuit is Gas Man's mixing box, kept for comparison with a
  # program that has no oxygen.  It has no absorber in its volume balance, so
  # the oxygen volume model is not applied to it rather than applied wrongly.
  if (circuit == "semi-closed") oxygenUptake <- FALSE
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
    # simulation this interval occupies.  With uptakeEffect = FALSE accuracy
    # does not depend on this: the advance is exact at every step size.  With
    # the uptake coupling on (the default) it does: the coupling is frozen per
    # sub-step (below), so the sub-step also sets the accuracy, first order in
    # dt.  See the UPDATE in the file header for measured sizes.
    nSub <- max(1, round(resolution * len / maximum))
    dt   <- len / nSub

    # Settings are read at the START of the interval and held across it, which
    # is what makes the interval linear -- apart from the uptake coupling,
    # which is linearised per sub-step below.
    s <- gasSettingsAt(bySetting, t0, deadSpace)

    # Build one propagator per gas for this interval: y <- P y + q.
    #
    # Without the uptake coupling the gases are independent and the system is
    # constant across the whole interval, so the propagators are built once.
    # With it they are coupled through totUptake, which depends on the state, so
    # the system is no longer linear over the interval and the propagators are
    # rebuilt every sub-step from the state at the START of that step.
    #
    # That is Gas Man's own treatment -- it freezes fTotUptake per tick too --
    # but the propagation WITHIN each step stays exact here where Gas Man
    # splits.  This removes Gas Man's splitting error but not the error of
    # freezing the coupling, which is first order in dt: the sub-step is exact
    # for the frozen coefficients, not for the coupled equations.  (An earlier
    # version of this comment called the result "strictly more accurate" and
    # step-independent; neither was shown, and the second is false -- audit
    # finding F07.)  The two engines converge as dt shrinks rather than
    # agreeing digit-for-digit at any fixed dt.
    buildProp <- function(totUptake, oxygenTotUptake = 0, cE = 0)
    {
      pr <- list()
      for (g in props$gas)
      {
        if (g == "oxygen")
        {
          sys <- gasSystemOxygen(body, s$Q, s$VA, s$Ffgf[["oxygen"]], circuit, s$MV,
                                 oxygenTotUptake, cE)
        } else {
          sys <- gasSystemSoluble(props[props$gas == g, ], body,
                                  s$Q, s$VA, Qco, s$Ffgf[[g]], totUptake, circuit,
                                  s$MV, cE)
        }
        pr[[g]] <- gasPropagator(sys$A, sys$b, dt)
        # Ideal circuit: remember how to fill in the circuit tension, which is
        # a function of the alveolar tension rather than a state.
        pr[[g]]$fresh    <- sys$fresh
        pr[[g]]$alveolar <- sys$alveolar
        pr[[g]]$Ffgf     <- s$Ffgf[[g]]
      }
      pr
    }

    # The propagators for one sub-step, from the state at its start.
    coupledProp <- function(state)
    {
      u <- gasTotalUptake(state, props, body, Qco)
      if (!oxygenUptake) return(buildProp(u))

      # Oxygen consumed is volume lost: VO2 from the system, of which carbon
      # dioxide gives VCO2 back to the alveoli and the absorber then takes it
      # from the rebreathed gas.  All of it stops if the oxygen has run out.
      consuming <- state[["oxygen"]][2] > 0
      VO2  <- if (consuming) body$VO2 else 0
      VCO2 <- GAS_RESPIRATORY_QUOTIENT * VO2
      u <- u + VO2 - VCO2
      buildProp(u, u, if (s$MV > 0) min(VCO2 / s$MV, GAS_MAX_EXHALED_CO2) else 0)
    }

    prop <- if (uptakeEffect) NULL else buildProp(0)

    stepStates <- list()
    for (g in props$gas) stepStates[[g]] <- matrix(NA_real_, nSub, length(state[[g]]))

    for (k in seq_len(nSub))
    {
      if (uptakeEffect) prop <- coupledProp(state)

      # Every gas advances from the state at the start of the sub-step, so the
      # update is simultaneous rather than sequential and no gas sees another's
      # new value within a step.
      newState <- state
      for (g in props$gas)
      {
        newState[[g]] <- as.vector(prop[[g]]$P %*% state[[g]] + prop[[g]]$q)

        # Ideal circuit: F_circ = f F_fgf + (1 - f) F_alv, set from the new
        # alveolar tension.
        if (!is.null(prop[[g]]$fresh))
          newState[[g]][1] <- prop[[g]]$fresh * prop[[g]]$Ffgf +
            prop[[g]]$alveolar * newState[[g]][2]

        # Oxygen cannot go negative (Shafer, 2026-10-05).  Metabolic consumption
        # is modelled as a constant sink, which is right while there is oxygen
        # to consume but, unchecked, drives the fraction below zero whenever
        # supply cannot meet demand (apnea, or a hypoxic mixture).  Flooring at
        # zero after each sub-step is the statement that consumption stops when
        # nothing is left.  The linear advance within the sub-step is unchanged,
        # so a run that never reaches zero is unaffected to the last digit.
        # (Claude Code, Claude Fable 5.1; verified by test-gas-engine.R.)
        if (g == "oxygen") newState[[g]] <- pmax(newState[[g]], 0)

        stepStates[[g]][k, ] <- newState[[g]]
      }
      state <- newState
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

  # (5) MAC, from the ALVEOLAR tension -- state 2, not the brain state 3.
  #
  # Minimum Alveolar Concentration is alveolar by definition, it is what is
  # clinically titrated to, and it is what Gas Man itself reports: its CSV
  # export writes GetALV(fMin, ng) / m_fMAC.  An earlier version of this file
  # used the brain tension, which lags alveolar and is not what MAC means.
  #
  # Two things here are stanpumpR's, with no Gas Man counterpart to be faithful
  # to.  Gas Man emits ONE ROW PER AGENT, each with its own MAC column, and
  # never sums them; additive MAC across agents is ours.  And Gas Man uses
  # m_fMAC raw, with no age term, so macForAge() is ours too -- a validation run
  # against Gas Man output must use the reference age.
  macTotal <- rep(0, nT)
  for (g in props$gas[props$potent])
  {
    MAC <- macForAge(props$MAC40[props$gas == g], age)
    macTotal <- macTotal + out[[g]][, 2] / MAC
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
#' @param cardiacOutput optional override in L/min; defaults to Gas Man's 5 L/min at 70 kg, scaled by (weight/70)^0.75
#' @param uptakeEffect,circuit,deadSpace,oxygenUptake passed to
#'   \code{advanceClosedFormGas()}
#'
#' @returns \code{NULL} if the dose table contains no gases, otherwise the list
#'   returned by \code{advanceClosedFormGas()}
#' @export
simulateGases <- function(doseTable, weight = 70, age = 50, maximum = 60,
                          cardiacOutput = NULL, uptakeEffect = TRUE,
                          circuit = c("ideal", "semi-closed"),
                          deadSpace = GAS_DEAD_SPACE_FRACTION,
                          oxygenUptake = TRUE)
{
  circuit <- match.arg(circuit)
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
    cardiacOutput = cardiacOutput,
    uptakeEffect  = uptakeEffect,
    circuit       = circuit,
    deadSpace     = deadSpace,
    oxygenUptake  = oxygenUptake
  )
}
