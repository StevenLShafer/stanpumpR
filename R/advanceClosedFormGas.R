# -----------------------------------------------------------------------------
# Provenance
# ----------
# Drafted by Claude Code (Claude Opus 5), 2026-09-02, at the request of
# Steven L. Shafer.  Implements the GasMan(R)-style inhaled-gas model described
# in R/gasProperties.R.  Nothing here comes from GasMan's binary or UI; some
# specific defaults (cardiac output and ventilation scaling,
# R/gasProperties.R) are read from the GasDoc.cpp/gasman_api SOURCE FILES
# GasMan's authors released under GPL-3.0 at github.com/rasman/gasmanonline,
# as noted where each is used.  That repository also holds the desktop and web
# application's source (src/gasman_main), so GasMan is not closed source, as
# this header said until 2026-11-23. The rest of
# the structure here follows the peer-reviewed literature, not any GasMan
# source.
#
# STATUS: run and verified on R 4.6.1 by tests/testthat/test-gas-engine.R --
# the closed-form advance agrees with an independent RK4 integration of the same
# ODEs, single-compartment analytic limits are reproduced, and steady states
# match hand calculation. Also run against GasMan's own C++ (built from the
# released gasmanonline/gasman_api source, commit d3a2dd3) on five scenarios:
# worst difference 2.9% of peak against GasMan C++ and well under 1% against
# the analytic limit (`inst/validation/VALIDATION.md`, "Run against GasMan
# itself"), for both the semi-closed and ideal circuits. Still untested: the
# open and closed circuits, liquid injection, flush, agents other than
# sevoflurane/isoflurane/desflurane/nitrous oxide, and the web/desktop editions
# (only the API build was run).
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
# The vapour displaces carrier gas within Q; it does not add to Q.  GasMan has
# an option (off by default) that instead adds the vapour's own volume, raising
# the effective fresh gas flow by 1 / (1 - F_vap/100).  That option has no
# counterpart here.  (Both notes: Claude Code, 2026-10-09,
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
#     With d = 0 this is GasMan's "Ideal" circuit (GasDoc.cpp, IDEAL_CKT),
#     whose threshold is at Q = VA because GasMan has no dead space: its
#     ventilation IS alveolar ventilation.  The dead space, and with it the
#     threshold at minute ventilation, is stanpumpR's (Shafer, 2026-10-05: "the
#     user sets minute ventilation, not alveolar ventilation ... set dead space
#     at 30% of minute ventilation").  Pass deadSpace = 0 to compare with
#     GasMan.
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
#     "semi-closed" -- GasMan's own default, and this engine's until that date.
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
#     as Q -> 0 and differ most at Q = VA.  Kept for comparison with GasMan.
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
#     THE CLOSED CIRCUIT (Carbon's third UI audit, 2026-11-15, H2).  k is the
#     share of the exhaled gas that is rebreathed, so it cannot exceed one.
#     It does when Q < u+ + VCO2 (= u+ + c_E MV): the fresh gas no longer
#     covers what the patient takes up, oxygen included.  Every litre exhaled
#     is then rebreathed, nothing is vented, and the circuit still falls short;
#     a real reservoir bag empties.  Until this date k was left above one, so
#     more gas was rebreathed than was exhaled, at the exhaled composition:
#     gas made from nothing.  With no oxygen flow (Q = 0) and 4.6 L/min of
#     ventilation at 70 kg it made 0.25 L/min, the oxygen consumed; nitrogen
#     rose past 100% of an atmosphere in the first minutes, the uptake
#     coupling then fed on it (make-up gas at more than 100% adds more than
#     it replaces), alveolar nitrogen reached 142% on a day's plot, and on a
#     three-day plot the states overflowed and the gas panels vanished
#     ("missing value where TRUE/FALSE needed").  Now k is held at one and the
#     alveoli receive only what the circuit has, the fresh gas and the
#     rebreathed gas less the dead space's share,
#         Q + MV (1 - c_E) - (MV - VA) = Q + VA - VCO2,
#     whose composition is F_circ = (Q F_fgf + VA F_alv) / (Q + VA - VCO2).
#     Each gas then gains only what the fresh gas brings and loses only what
#     the blood takes up: the circuit and the patient are one closed system,
#     and nothing is created.  The volume deficit is not modelled (the
#     alveolar volume is fixed), so it appears as tensions summing to less
#     than an atmosphere rather than as a bag emptying.  The same holds in
#     apnoea (MV = 0), where the alveoli draw the uptake from the fresh gas as
#     far as it goes and no further.  gasCircuitBlend() returns the share of
#     the alveoli's demand that the circuit meets as `supply`.
#
#     On emergence (u < 0) the alveoli push gas out, -u L/min at alveolar
#     composition, and that gas is in the circuit too.  Below the threshold
#     the circuit vents it first, rebreathing the exhaled gas, and vents more
#     than it in all (Q - VCO2 - u >= -u while k <= 1).  Once the circuit is
#     closed that is no longer so: everything exhaled is rebreathed, and the
#     pushed-out gas is rebreathed as far as the shortfall needs,
#         r = min(MV + u+ - Q - MV (1 - c_E), -u)     (u+ = 0 here),
#     only the rest, -u - r = max(0, Q - VCO2 - u), being vented, which is
#     the circuit's surplus and nothing while it is short.  The alveoli then
#     receive D = Q + VA - VCO2 + r, at
#         F_circ = (Q F_fgf + (VA + r) F_alv) / D,
#     all of VA again (supply 1) while the pushed-out gas covers the
#     shortfall.  Until 2026-11-15 r was zero, so all of the pushed-out gas
#     was vented, and a washout of nitrous oxide into a closed circuit lost
#     every gas at -u times its alveolar tension, though nothing left the
#     circuit (CodeRabbit on PR #730).  The change is confined to k > 1 with
#     u < 0, and is continuous at k = 1, where r is zero.
#
#     In the steady state the gas vented is the fresh gas less the oxygen
#     consumed: with 0.3 + 1.0 L/min in and 0.21 L/min consumed, 1.09 L/min
#     leaves as 92% nitrous oxide and 8% oxygen (dry, carbon dioxide aside).
#
#     GasMan does none of this: it has no oxygen.  Pass oxygenUptake = FALSE to
#     compare with it.
#
# (5) MAC, summed over the potent agents, using the ALVEOLAR tension and the
#     age-adjusted MAC (at the body temperature in force):
#
#         MAC_total = sum_i F_alveolar,i / MAC_i(age)
#
#     MAC is defined at the end-tidal concentration, so this is the summed
#     end-tidal MAC (see "(5) MAC" in the loop below, which says why).  Until
#     2026-11-24 this header said the brain (vessel-rich) tension, which an
#     earlier version used (Gemini's gas-engine audit of 2026-11-23, C-04).
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
# UPDATE (2026-10-09, Claude Code, from audit finding F07):
# that sentence is historical.  The term is implemented (`uptakeEffect`, on by
# default), and it does break linearity: the summed uptake depends on the
# state.  The engine keeps each SUB-STEP linear by holding the summed uptake
# (and the ideal-circuit blend that depends on it) at its value at the start of
# the sub-step, advances the sub-step exactly by matrix exponential, and
# recomputes the uptake for the next one.  Propagation within a sub-step is
# exact; the coupling between sub-steps is first order in dt.  The result is
# therefore NOT independent of the step size.  Until 2026-11-23 the step was
# set by `resolution` alone (about maximum / 600 by default), and measured
# against the same engine at a 40-fold finer step it gave under 0.003
# percentage points in the scenarios without nitrous oxide but, with 4 L/min
# nitrous oxide in 6 L/min, first plotted alveolar values low by about 1.5% of
# their value on a 60-minute plot, 6% on 240 minutes and 13% on 1440, fading
# below 1% within 1, 2 and 12 minutes.  Since 2026-11-23 (ChatGPT's gas-engine
# audit, A-03) the sub-steps start at GAS_STEP_FIRST (0.01 minute) at each
# change of setting and grow by GAS_STEP_GROWTH (1.1) until they reach
# maximum / resolution (gasSubSteps()), so each is about a tenth of the time
# since the change.  Measured against a reference starting at 0.0005 minute
# and growing by 1.02 to a twentieth of the plot's step, in that induction,
# its emergence, and the same in a 4 kg infant, on plots of an hour, a day
# and a week: the largest error at any plotted point was 0.36% of the reading
# (0.59% in the infant), against 1.6%, 13.4% and 6.3% before, for 15 to 130
# more steps than the 601.  In the audit's matched ideal-circuit sevoflurane case the
# 30-minute alveolar value moves from 1.71176014% with 31 steps to
# 1.71178091% with 2401, against 1.71178112% from an independent ODE solver.
# See the Integration section of inst/app-help/models/gas-engine.md.
#
# CORRECTION (2026-09-03): an earlier version of this comment said it was
# unresolved whether GasMan models it, and a later one said GasMan does not.
# Both were wrong.  GasMan does model it, in GasDoc.cpp::Calc, under the names
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
# It must therefore be implemented for the GasMan baseline to be a baseline.
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
# That is the second gas effect, and GasMan models it (m_bUptEnb, defaulting
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
# the term either.  GasMan does not model oxygen at all, so there is no
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
  sum(y[3:gasStateCount(body)] * gasTissueShares(body))
}


#' The tissue groups of the gas body, in state order
#'
#' States 3, 4 and 5 are the vessel-rich group, muscle and fat.  When the
#' cardiac-output slider has moved (\code{gasBodyPhysiology()}), the brain and
#' heart are state 3 -- still the effect site, reported as "Brain" -- and the
#' rest of the vessel-rich group is a sixth state, so that the flow the brain
#' keeps in a low output is not spread over the kidneys, liver and gut.  Its
#' tissue:gas partition coefficient is the vessel-rich group's.
#'
#' @param body output of \code{getGasBody()}, possibly through
#'   \code{gasBodyPhysiology()}
#' @returns \code{gasTissueShares()}: the share of cardiac output of each;
#'   \code{gasTissueVolumes()}: their volumes, L; \code{gasStateCount()}: the
#'   number of states of a soluble gas, 5 or 6
#' @keywords internal
gasTissueShares <- function(body)
{
  f <- c(brain = body$f_brain, muscle = body$f_muscle, fat = body$f_fat)
  if (!is.null(body$f_vrgRest)) f <- c(f, vrgRest = body$f_vrgRest)
  f
}

#' @rdname gasTissueShares
#' @keywords internal
gasTissueVolumes <- function(body)
{
  v <- c(brain = body$V_brain, muscle = body$V_muscle, fat = body$V_fat)
  if (!is.null(body$f_vrgRest)) v <- c(v, vrgRest = body$V_vrgRest)
  v
}

#' @rdname gasTissueShares
#' @keywords internal
gasStateCount <- function(body) 2L + length(gasTissueShares(body))


#' Total uptake summed over every soluble gas
#'
#' GasMan's \code{fTotUptake}: positive while gas is being taken up, negative
#' during washout.  Oxygen is excluded -- see the note above.
#'
#' @param state named list of per-gas state vectors
#' @param props gas property table from \code{getGasProperties()}
#' @param body geometry from \code{getGasBody()}
#' @param Qco cardiac output, L/min
#' @returns total uptake in L/min, at the ambient pressure of the body's
#'   altitude
gasTotalUptake <- function(state, props, body, Qco)
{
  total <- 0
  for (g in props$gas)
  {
    if (g == "oxygen") next
    y  <- state[[g]]
    lb <- props$lambda_blood[props$gas == g]
    total <- total + lb * (1 - gasShunt(body)) * Qco *
      (y[[2]] - gasMixedVenous(y, body)) / 100
  }
  # An ambient volume: the same partial pressures are 1/r the volume at sea
  # level (R/altitude.R)
  total / gasPressureRatio(body)
}


#' Fraction of inspired gas that is fresh gas, for the ideal circuit
#'
#' One when fresh gas flow meets or exceeds MINUTE ventilation -- no rebreathing
#' -- and \code{Q / (VA + Q d)} below that, with \code{d = 1 - VA / MV} the
#' dead-space fraction; see the file header for the derivation.  With no dead
#' space this is \code{Q / VA}, GasMan's ideal circuit.  Always one for an open
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
#'   adds to what is inspired.  A negative value is gas pushed out of the
#'   alveoli, which a closed circuit rebreathes as far as its shortfall needs.
#' @param cE carbon dioxide as a fraction of exhaled gas, removed from whatever
#'   is rebreathed.  Capped at \code{GAS_MAX_EXHALED_CO2}, which keeps the blend
#'   physical at any ventilation.
#' @returns a list with \code{fresh} and \code{alveolar}, the composition of
#'   what is inspired, and \code{supply}, the share of what the alveoli draw
#'   from the circuit (\code{VA + u+}) that it can give them: one, unless the
#'   fresh gas is less than the uptake and the carbon dioxide absorbed, when
#'   the circuit is closed and the gas pushed out of the alveoli (a negative
#'   \code{u}) does not make up the difference (see THE CLOSED CIRCUIT in the
#'   file header)
#' @keywords internal
gasCircuitBlend <- function(Q, VA, MV = VA, circuit = "ideal", u = 0, cE = 0)
{
  inspired <- MV + max(u, 0)
  if (circuit == "open" || Q >= inspired)
    return(list(fresh = 1, alveolar = 0, supply = 1))
  # Apnoea: nothing is exhaled, so the uptake is drawn from the fresh gas,
  # and only as far as it goes.  Until 2026-11-15 the whole uptake was drawn
  # at the fresh gas's composition whatever the flow, so with no flow a
  # vaporiser's setting was drawn into the lungs from nowhere (Carbon's third
  # UI audit, 2026-11-15, H2).
  if (MV <= 0)
    return(list(fresh = 1, alveolar = 0, supply = Q / inspired))
  cE <- min(max(cE, 0), GAS_MAX_EXHALED_CO2)
  k <- (inspired - Q) / (MV * (1 - cE))
  supply <- 1
  # The gas pushed out of the alveoli and rebreathed, L/min: none unless the
  # circuit is closed (below).
  pushedBack <- 0
  if (k <= 1) {
    D <- inspired - k * (MV - VA)
  } else {
    # k is the share of the exhaled gas rebreathed.  Above one the circuit is
    # closed: all of it is rebreathed (THE CLOSED CIRCUIT in the file header;
    # Carbon's third UI audit, 2026-11-15, H2).  When the summed uptake is
    # negative, the gas pushed out of the alveoli, -u, is in the circuit too,
    # and is rebreathed as far as the shortfall needs; only what is left of
    # it is vented.  Until 2026-11-15 all of it was vented, so a washout into
    # a closed circuit lost gas that went nowhere (CodeRabbit on PR #730).
    # What reaches the alveoli is what there is, Q + VA - VCO2 plus the gas
    # pushed back, at most the VA + u+ they draw.
    k <- 1
    pushedBack <- min(inspired - Q - MV * (1 - cE), max(-u, 0))
    D <- Q + MV * (1 - cE) - (MV - VA) + pushedBack
    supply <- D / (VA + max(u, 0))
  }
  # With cE capped and the dead space under half of the minute ventilation, D
  # is positive; this is a guard against a future change to either, not a
  # branch that runs.
  if (D <= 0) stop("ventilation and dead space do not permit a physical circuit blend")
  list(fresh = Q / D, alveolar = (k * VA + pushedBack) / D, supply = supply)
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
#' @returns a list with \code{A} (5x5, or 6x6 with the vessel-rich group
#'   split; see \code{gasTissueShares()}), \code{b} and, for the ideal
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

  # Blood flow to each tissue group, and the tissue rate constants
  # Q_t * lambda_b / (V_t * lambda_t/gas).  The rest of the vessel-rich group,
  # when it is split from the brain and heart, has the vessel-rich partition.
  shares <- gasTissueShares(body)
  n <- 2L + length(shares)
  partition <- c(brain = ltg[["brain"]], muscle = ltg[["muscle"]],
                 fat = ltg[["fat"]], vrgRest = ltg[["brain"]])[names(shares)]
  k <- shares * Qco * lb / (gasTissueVolumes(body) * partition)
  # An endobronchial tube (R/endobronchial.R): the share `sh` of cardiac
  # output passes the unventilated lung and takes up nothing
  sh <- gasShunt(body)
  lungFlow <- (1 - sh) * Qco

  A <- matrix(0, n, n)

  # (1) circuit.  On EMERGENCE (totUptake < 0) alveolar gas is displaced back
  # into the circuit, which GasMan models by subtracting the term from the
  # circuit numerator:  if (tot_uptake < 0) f <- f - tot_uptake * ALV.  There is
  # no matching term on induction, when make-up gas flows the other way.
  A[1, 1] <- -(Q + VA) / Vc
  A[1, 2] <-  VA / Vc
  if (totUptake < 0) A[1, 2] <- A[1, 2] - totUptake / Vc

  # (2) alveolar.  The mixed-venous term redistributes lambda_b * Qco across the
  # three tissue states in proportion to their share of cardiac output.
  A[2, 1] <-  VA / Va
  A[2, 2] <- -(VA + lb * lungFlow) / Va
  A[2, 3:n] <- lb * lungFlow * shares / Va

  # (3) tissues, fed by arterial blood: (1 - sh) alveolar and sh mixed venous
  for (j in seq_along(k)) {
    A[2 + j, 2] <- k[[j]] * (1 - sh)
    if (sh > 0) A[2 + j, 3:n] <- A[2 + j, 3:n] + k[[j]] * sh * shares
    A[2 + j, 2 + j] <- A[2 + j, 2 + j] - k[[j]]
  }

  # The uptake coupling.  GasMan's Calc adds this to the alveolar numerator:
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

  b <- c(Q * Ffgf / Vc, rep(0, n - 1))

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
  # When the fresh gas falls short of the uptake the circuit is closed, and the
  # alveoli receive only the share `supply` of what they draw: the rest does
  # not exist (THE CLOSED CIRCUIT in the file header).
  bl <- gasCircuitBlend(Q, VA, MV, circuit, totUptake, cE)
  fromCircuit <- A[2, 1] * bl$supply
  A[2, 2] <- A[2, 2] + fromCircuit * bl$alveolar
  A[1, ] <- 0
  A[, 1] <- 0
  # The fresh gas humidified in the airway at the body's altitude
  # (gasHumidityRatio(), R/altitude.R; exactly 1 at sea level)
  b <- c(0, fromCircuit * bl$fresh * Ffgf * gasHumidityRatio(body), rep(0, n - 2))

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
    # A closed circuit gives the alveoli only what it has (`supply`, THE
    # CLOSED CIRCUIT in the file header): oxygen then gains what the fresh
    # gas brings and loses what is consumed, and no more.
    fromCircuit <- fromCircuit * bl$supply
    A <- matrix(0, 2, 2)
    A[2, 2] <- -VA / Va + fromCircuit * bl$alveolar + pushedOut
    b <- c(0, fromCircuit * bl$fresh * Ffgf * gasHumidityRatio(body) -
             100 * body$VO2 / Va)
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
#'   ventilation, as it is in GasMan.
#' @param ratio the barometric pressure as a fraction of a sea-level
#'   atmosphere (\code{gasPressureRatio()}, R/altitude.R).  The fresh gas is
#'   returned as partial pressures in % of a sea-level atmosphere: a flowmeter
#'   gas at \code{ratio} times its fraction, a variable-bypass vaporizer at its
#'   dial, and the desflurane vaporizer, which delivers volume percent, at
#'   \code{ratio} times its dial.
#' @returns a list with total fresh gas flow \code{Q}, minute ventilation
#'   \code{MV}, alveolar ventilation \code{VA}, and the fresh-gas partial
#'   pressures
#' @keywords internal
gasSettingsAt <- function(split, t, deadSpace = GAS_DEAD_SPACE_FRACTION,
                          ratio = 1)
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
  # Vaporisers that together exceed 100% have no physical fresh gas: the
  # carrier would be negative, and clipping it to zero (as this did until
  # 2026-10-12) left the fractions summing past 100.  The dose table is
  # checked by gasVaporiserProblems() before it gets here; this is the
  # engine's own refusal.
  # The dial's partial pressure (R/altitude.R): a variable-bypass vaporizer
  # keeps it at any altitude, the desflurane vaporizer keeps its volume percent
  vpv <- intersect(names(F_vap), ALTITUDE_VOLUME_PERCENT_VAPORIZERS)
  F_vap[vpv] <- F_vap[vpv] * ratio
  if (sum(F_vap) > 100 * ratio + 1e-9) {
    stop(sprintf("the vaporisers together are set to %s%% at %s min; the fresh gas cannot be more than 100%% vapour",
                 format(sum(F_vap)), format(t)))
  }
  # The vapour's share of the fresh gas is its partial pressure over the
  # ambient pressure; the carrier gases share the rest of the ambient pressure.
  carrier <- 1 - sum(F_vap) / (100 * ratio)
  if (carrier < 0) carrier <- 0

  if (Q > 0)
  {
    Ffgf <- c(
      oxygen       = 100 * ratio * carrier * (Q_O2 + AIR_FRACTION_O2 * Q_air) / Q,
      nitrogen     = 100 * ratio * carrier * (AIR_FRACTION_N2 * Q_air) / Q,
      nitrousOxide = 100 * ratio * carrier * Q_N2O / Q
    )
  } else {
    Ffgf <- c(oxygen = 0, nitrogen = 0, nitrousOxide = 0)
  }
  for (g in volatiles) Ffgf[[g]] <- F_vap[[g]]

  list(Q = Q, VA = VA, MV = MV, Ffgf = Ffgf)
}


#' Vaporiser settings that together exceed 100%
#'
#' At every time a vaporiser setting changes, the settings then in force for
#' all the volatile agents are summed; a sum above 100% is a fresh gas that
#' cannot exist (there would be no carrier left), and the engine refuses it.
#' The app shows these as a notification, so that the gas curves do not simply
#' vanish.  Rows for a single agent at one time keep the last one, as the
#' engine does.
#'
#' @param doseTable a dose table with \code{Drug}, \code{Time} (minutes, or
#'   anything \code{as.numeric()} reads) and \code{Dose}; non-gas rows and
#'   blank rows are ignored
#' @returns a character vector of messages, one per offending time, empty when
#'   the settings are possible
#' @export
gasVaporiserProblems <- function(doseTable, altitude = ALTITUDE_DEFAULT)
{
  # At altitude the engine holds the vapour to the ambient pressure, a
  # desflurane vaporizer's dial scaled by it (gasSettingsAt()), so the check
  # does too: checked at sea level, settings the engine then refused at
  # altitude made the gas curves vanish without a word (Carbon's second
  # repository audit, 2026-11-19, A2-F23).
  ratio <- pressureRatio(altitude)
  if (is.null(doseTable) || nrow(doseTable) == 0) return(character(0))
  volatiles <- setdiff(potentAgents(), "nitrousOxide")
  drug <- as.character(doseTable$Drug)
  rows <- doseTable[drug %in% volatiles, , drop = FALSE]
  if (nrow(rows) == 0) return(character(0))
  rows <- data.frame(
    Time = suppressWarnings(as.numeric(as.character(rows$Time))),
    Drug = as.character(rows$Drug),
    Dose = suppressWarnings(as.numeric(as.character(rows$Dose))),
    stringsAsFactors = FALSE
  )
  rows <- rows[!is.na(rows$Time) & !is.na(rows$Dose), , drop = FALSE]
  if (nrow(rows) == 0) return(character(0))
  rows <- rows[order(rows$Time), , drop = FALSE]
  split <- split(rows, rows$Drug)
  problems <- character(0)
  for (t in sort(unique(rows$Time))) {
    dial <- vapply(split, function(d) settingAt(d, t), numeric(1))
    vpv <- names(dial) %in% ALTITUDE_VOLUME_PERCENT_VAPORIZERS
    total <- sum(dial[!vpv]) + sum(dial[vpv]) * ratio
    if (total > 100 * ratio + 1e-9) {
      problems <- c(problems, if (ratio == 1) sprintf(
        "At %s min the vaporisers together are set to %s%%; the fresh gas cannot be more than 100%% vapour, so the gases are not simulated until one of them is lowered.",
        format(t), format(sum(dial))) else sprintf(
        "At %s min the vaporisers together give a partial pressure of %s%% of a sea-level atmosphere, above the ambient %s%% at %s m; the fresh gas cannot be more than 100%% vapour, so the gases are not simulated until one of them is lowered.",
        format(t), format(signif(total, 3)), format(signif(100 * ratio, 3)), format(altitude)))
    }
  }
  problems
}


#' The first sub-step after a change of setting, minutes, and the factor by
#' which each sub-step after it is longer, until the plot's own step is
#' reached (gasSubSteps())
#' @noRd
GAS_STEP_FIRST <- 0.01
GAS_STEP_GROWTH <- 1.1


#' Sub-steps of one interval of constant settings
#'
#' The coupled advance holds the summed uptake at its value at the start of
#' each sub-step, which is first order in the sub-step's length, and the
#' uptake changes fastest just after a change of setting: a nitrous oxide
#' wash-in starts with none in the lungs, so a first step held at that uptake
#' takes up none at all.  So the sub-steps start at \code{first} and grow by
#' \code{growth} until they reach \code{dtNominal}, the plot's own step, which
#' takes the rest of the interval.  Each sub-step is then a fixed share of the
#' time since the change (a tenth, at the default growth), and the error a
#' fixed share of the reading, whatever the plot's length.  Until 2026-11-23
#' every sub-step was \code{dtNominal}, the plot's length over 600, so a 24-hour
#' plot stepped 2.4 minutes from the start of a nitrous oxide induction and
#' read 13 per cent low at its first point (ChatGPT's gas-engine audit of
#' 2026-11-23, A-03).
#'
#' @param len the interval's length, minutes
#' @param dtNominal the plot's step, \code{maximum / resolution}
#' @param graded FALSE for uniform steps, as the uncoupled advance needs (one
#'   propagator serves them all, and it is exact at any step)
#' @param first,growth the first sub-step and the factor between sub-steps;
#'   an interval that continues a ramp the one before it did not finish (a row
#'   that repeats the settings in force) starts at the step that ramp would
#'   have taken next
#' @returns the sub-steps' lengths, summing to \code{len}; where the interval
#'   ends before the ramp reaches \code{dtNominal}, the attribute
#'   \code{nextStep} is the step the ramp would take next
#' @keywords internal
gasSubSteps <- function(len, dtNominal, graded = TRUE,
                        first = GAS_STEP_FIRST, growth = GAS_STEP_GROWTH)
{
  if (!graded || dtNominal <= first) {
    n <- max(1, round(len / dtNominal))
    return(rep(len / n, n))
  }
  h <- numeric(0)
  elapsed <- 0
  step <- first
  while (step < dtNominal && elapsed + step < len) {
    h <- c(h, step)
    elapsed <- elapsed + step
    step <- step * growth
  }
  rest <- len - elapsed
  # ceiling(), not round(): no step after the ramp is longer than the plot's
  # (round() let one reach 1.5 times it, 0.1495 at an interval of 1.133
  # minutes on a 60-minute plot; CodeRabbit on PR #858)
  n <- max(1, ceiling(rest / dtNominal - 1e-9))
  out <- c(h, rep(rest / n, n))
  if (step < dtNominal) attr(out, "nextStep") <- step
  out
}


#' Simulate the inhaled gases in a circle breathing system
#'
#' Closed-form solution of the GasMan(R)-style model documented at the top of
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
#'   value in \code{body} (GasMan's 5 L/min at 70 kg, allometric).  Accepted per call so that a
#'   time-varying cardiac output can be added later without changing the
#'   engine; note that letting it vary would logically require the intravenous
#'   pharmacokinetics to respond to it as well, which stanpumpR does not model.
#' @param resolution about the number of output time points: the plot's
#'   sub-step is \code{maximum / resolution}.  With \code{uptakeEffect} TRUE
#'   the sub-steps start shorter at each change of setting and grow to it
#'   (\code{gasSubSteps()}), which adds a few dozen points per change; the
#'   error, first order in the sub-step, is then about the same on any plot
#'   length (see the file header).
#' @param uptakeEffect if TRUE (the default, as in GasMan, whose m_bUptEnb
#'   defaults true), couple the gases through their summed uptake, giving the
#'   concentration and second gas effect.  Set FALSE to isolate that term: with
#'   it off the gases do not influence one another at all, which is what makes
#'   the effect measurable as a difference rather than asserted.
#' @param circuit "ideal", the default: no rebreathing once fresh gas flow
#'   reaches minute ventilation, and no circuit volume.  "semi-closed":
#'   GasMan's default, the circuit as one well-mixed volume, kept for the
#'   GasMan validation and not offered by the app; it takes no humidity
#'   correction at altitude (R/altitude.R), so at 7000 m its end-tidal tension
#'   settles about 10% above the ideal circuit's (Gemini's gas-engine audit of
#'   2026-11-23, A-04).  See the file header.
#' @param bodySteps NULL, or \code{gasBodySteps()}'s list of bodies and the
#'   times from which each is in force (physiology events): the body, its gas
#'   properties, pressure, humidity and cardiac output are switched at each,
#'   the agent carried across (\code{gasStateAcrossBodies()},
#'   \code{gasStateAcrossPressure()}); the result
#'   then carries \code{bodySteps} and \code{bodyRow}, the body of each row
#'   (a step's time appears twice, before and after it)
#' @param oxygenUptake if TRUE (the default), oxygen consumed shrinks the gas
#'   volume: its net volume, after the carbon dioxide that replaces it in the
#'   alveoli, joins the summed uptake that couples the gases, oxygen itself
#'   takes that coupling, and the absorber removes carbon dioxide from
#'   rebreathed gas.  See (4a) in the file header.  FALSE restores the earlier
#'   behaviour, in which oxygen was a sink with no effect on volume; GasMan has
#'   no oxygen, so comparisons with it pass FALSE.  Has no effect when
#'   \code{uptakeEffect} is FALSE, and is switched off for the semi-closed
#'   circuit, whose well-mixed-box model has no carbon dioxide absorber to put
#'   the volume balance in: that circuit exists only for comparison with
#'   GasMan, which has no oxygen.
#' @param deadSpace dead space as a fraction of minute ventilation, 0.3 by
#'   default.  The "ventilation" rows are minute ventilation; alveolar
#'   ventilation is that times \code{1 - deadSpace}.  Pass 0 to treat the rows
#'   as alveolar ventilation, as GasMan does.
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
  oxygenUptake = TRUE,
  bodySteps = NULL
)
{
  circuit <- match.arg(circuit)
  # Physiology events (R/physiologyEvents.R): a body in force from each of
  # bodySteps$time, the first at 0 (gasBodySteps()).  The body, and with it
  # the gas properties, the pressure, the humidity and the cardiac output,
  # are switched at each step, the agent in each tissue carried across
  # (gasStateAcrossBodies()).
  if (!is.null(bodySteps)) body <- bodySteps$body[[1]]
  # The semi-closed circuit is GasMan's mixing box, kept for comparison with a
  # program that has no oxygen.  It has no absorber in its volume balance, so
  # the oxygen volume model is not applied to it rather than applied wrongly.
  if (circuit == "semi-closed") oxygenUptake <- FALSE
  if (is.null(body)) body <- getGasBody(weight)
  Qco <- if (is.null(cardiacOutput)) body$Q_cardiac else cardiacOutput

  # The partition coefficients and MAC at the body's temperature
  # (gasPropertiesAt(); 37 C unless the temperature slider has set one).
  props <- gasPropertiesAt(body$temperature)

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
  changes <- sort(unique(c(0, gasDose$Time, bodySteps$time, maximum)))
  changes <- changes[changes >= 0 & changes <= maximum]
  bodyIndex <- 1L
  if (length(changes) < 2) changes <- c(0, maximum)

  # Initial conditions: patient and circuit equilibrated with room air.  The
  # alveolar oxygen tension relaxes from the inspired value to its steady state
  # within the first minute or so, on the FRC/VA time constant.
  # Room air is a fraction of the ambient pressure, humidified in the airway
  # (R/altitude.R).
  ratio <- gasPressureRatio(body)
  humid <- gasHumidityRatio(body)
  solubleGases <- props$gas[props$soluble]
  state <- list()
  for (g in solubleGases)
  {
    init <- if (g == "nitrogen") AIR_FRACTION_N2 * 100 * ratio * humid else 0
    state[[g]] <- rep(init, gasStateCount(body))
  }
  state[["oxygen"]] <- rep(AIR_FRACTION_O2 * 100 * ratio * humid, 2)

  times  <- 0
  # The body in force at each recorded row (physiology events)
  bodyRow <- 1L
  record <- list()
  for (g in props$gas) record[[g]] <- matrix(state[[g]], nrow = 1)

  # The last change point is the end of the plot: no interval starts there,
  # but a physiology step at it is still taken, below, so the plot's last
  # point reads after it, as an intravenous drug's does.  Until 2026-11-23 the
  # loop stopped one point short and such a step was never taken (ChatGPT's
  # gas-engine audit of 2026-11-23, C-02).
  lastSettings <- NULL
  lastBody <- bodyIndex
  # The next step of a ramp an interval did not finish, NULL once finished
  rampStep <- NULL
  for (iv in seq_along(changes))
  {
    t0  <- changes[iv]

    # A physiology step at t0: the new body, the agent in each compartment
    # carried across
    if (!is.null(bodySteps)) {
      k <- findInterval(t0, bodySteps$time)
      if (k != bodyIndex) {
        newBody <- bodySteps$body[[k]]
        newProps <- gasPropertiesAt(newBody$temperature)
        state <- gasStateAcrossBodies(state, props, newProps)
        state <- gasStateAcrossPressure(
          state, body, newBody, circuit,
          fresh = gasSettingsAt(bySetting, t0, deadSpace, gasPressureRatio(newBody))$Ffgf)
        body <- newBody
        props <- newProps
        ratio <- gasPressureRatio(body)
        humid <- gasHumidityRatio(body)
        Qco <- if (is.null(cardiacOutput)) body$Q_cardiac else cardiacOutput
        bodyIndex <- k
        # The record shows the step: the same time, the carried state
        times <- c(times, t0)
        bodyRow <- c(bodyRow, k)
        for (g in props$gas) record[[g]] <- rbind(record[[g]], state[[g]])
      }
    }
    if (iv == length(changes)) break

    t1  <- changes[iv + 1]
    len <- t1 - t0
    if (len <= 0) next

    # Enough sub-steps to draw a smooth curve, proportional to the share of the
    # simulation this interval occupies.  With uptakeEffect = FALSE accuracy
    # does not depend on this: the advance is exact at every step size.  With
    # the uptake coupling on (the default) it does: the coupling is frozen per
    # sub-step (below), so the sub-step also sets the accuracy, first order in
    # dt, and the sub-steps start short at each change of setting and grow
    # (gasSubSteps()), so the accuracy no longer depends on the plot's length.
    # See the UPDATE in the file header for measured sizes.
    # Settings are read at the START of the interval and held across it, which
    # is what makes the interval linear -- apart from the uptake coupling,
    # which is linearised per sub-step below.
    s <- gasSettingsAt(bySetting, t0, deadSpace, ratio)

    # The sub-steps restart short only where something changed: a row that
    # repeats the settings in force carries on where the steps were, inside
    # the ramp or at the plot's step.  Until 2026-11-24 such a row inside the
    # ramp jumped to the plot's step (CodeRabbit on PR #858: a row repeating
    # nitrous oxide at 2 minutes of a day's plot took the first half hour's
    # error from 0.43 to 4.5 per cent).
    changed <- is.null(lastSettings) || !identical(s, lastSettings) ||
      bodyIndex != lastBody
    lastSettings <- s
    lastBody <- bodyIndex
    if (changed) rampStep <- GAS_STEP_FIRST
    steps <- gasSubSteps(len, maximum / resolution,
                         graded = uptakeEffect && !is.null(rampStep),
                         first = if (is.null(rampStep)) GAS_STEP_FIRST else rampStep)
    rampStep <- attr(steps, "nextStep")
    steps <- as.vector(steps)
    nSub <- length(steps)

    # Build one propagator per gas for this interval: y <- P y + q.
    #
    # Without the uptake coupling the gases are independent and the system is
    # constant across the whole interval, so the propagators are built once.
    # With it they are coupled through totUptake, which depends on the state, so
    # the system is no longer linear over the interval and the propagators are
    # rebuilt every sub-step from the state at the START of that step.
    #
    # That is GasMan's own treatment -- it freezes fTotUptake per tick too --
    # but the propagation WITHIN each step stays exact here where GasMan
    # splits.  This removes GasMan's splitting error but not the error of
    # freezing the coupling, which is first order in dt: the sub-step is exact
    # for the frozen coefficients, not for the coupled equations.  (An earlier
    # version of this comment called the result "strictly more accurate" and
    # step-independent; neither was shown, and the second is false -- audit
    # finding F07.)  The two engines converge as dt shrinks rather than
    # agreeing digit-for-digit at any fixed dt.
    buildProp <- function(totUptake, oxygenTotUptake = 0, cE = 0, dt)
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
    coupledProp <- function(state, dt)
    {
      u <- gasTotalUptake(state, props, body, Qco)
      if (!oxygenUptake) return(buildProp(u, dt = dt))

      # Oxygen consumed is volume lost: VO2 from the system, of which carbon
      # dioxide gives VCO2 back to the alveoli and the absorber then takes it
      # from the rebreathed gas.  All of it stops if the oxygen has run out.
      # KNOWN APPROXIMATION (ChatGPT's gas-engine audit of 2026-11-23, A-02):
      # this is decided at the start of the step, while the oxygen equation
      # keeps its sink and is floored at zero after it.  So once a supply
      # below VO2 has emptied the lungs, the oxygen that arrives is consumed
      # (the floor) but the volume it frees and the carbon dioxide it makes
      # are left out here: with 0.1 L/min of oxygen, 0.02 L/min of net
      # volume and 0.08 L/min of carbon dioxide, against an alveolar
      # ventilation of 4.2.  It arises only in a patient with no oxygen,
      # and is left as it is.
      consuming <- state[["oxygen"]][2] > 0
      # VO2 is a sea-level volume; at altitude the same oxygen is 1/r the
      # ambient volume, and the carbon dioxide 1/r the share of the exhaled
      # gas (R/altitude.R).
      VO2  <- if (consuming) body$VO2 / ratio else 0
      VCO2 <- GAS_RESPIRATORY_QUOTIENT * VO2
      u <- u + VO2 - VCO2
      buildProp(u, u, if (s$MV > 0) min(VCO2 / s$MV, GAS_MAX_EXHALED_CO2) else 0,
                dt = dt)
    }

    # Uncoupled, the steps are uniform (gasSubSteps()) and one propagator
    # serves them all
    prop <- if (uptakeEffect) NULL else buildProp(0, dt = steps[[1]])

    stepStates <- list()
    for (g in props$gas) stepStates[[g]] <- matrix(NA_real_, nSub, length(state[[g]]))

    for (k in seq_len(nSub))
    {
      if (uptakeEffect) prop <- coupledProp(state, steps[[k]])

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

    # The last sub-step's end is t1 itself, not a sum that rounding moves
    times <- c(times, t0 + cumsum(steps)[-nSub], t1)
    bodyRow <- c(bodyRow, rep(bodyIndex, nSub))
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
  # clinically titrated to, and it is what GasMan itself reports: its CSV
  # export writes GetALV(fMin, ng) / m_fMAC.  An earlier version of this file
  # used the brain tension, which lags alveolar and is not what MAC means.
  #
  # Two things here are stanpumpR's, with no GasMan counterpart to be faithful
  # to.  GasMan emits ONE ROW PER AGENT, each with its own MAC column, and
  # never sums them; additive MAC across agents is ours.  And GasMan uses
  # m_fMAC raw, with no age term, so macForAge() is ours too -- a validation run
  # against GasMan output must use the reference age.
  # MAC at each row's body temperature (physiology events step it)
  macTotal <- rep(0, nT)
  rowBodies <- if (is.null(bodySteps)) list(body) else bodySteps$body
  for (k in unique(bodyRow))
  {
    rows <- which(bodyRow == k)
    pk <- gasPropertiesAt(rowBodies[[k]]$temperature)
    for (g in pk$gas[pk$potent])
    {
      MAC <- macForAge(pk$MAC40[pk$gas == g], age)
      macTotal[rows] <- macTotal[rows] + out[[g]][rows, 2] / MAC
    }
  }
  results <- rbind(results, data.frame(
    Drug = "MAC", Time = timeLine, Site = "MAC", Y = macTotal,
    stringsAsFactors = FALSE))

  # The body, circuit and patient go with the states, so a reader of them (the
  # Visualizer's gas run, R/viewerRun.R) knows each compartment's capacity
  # without rebuilding them.
  # Qco is the cardiac output the simulation ran on, an explicit override
  # included, so the washout runs on the same flows (gasWashout()); deadSpace
  # the share of minute ventilation that did not reach the alveoli, so the
  # Visualizer can say the alveolar ventilation the engine ran on.
  # With physiology events, `body` and `Qco` are those in force at time 0,
  # and `bodySteps` the rest (gasWashout() works through them)
  if (!is.null(bodySteps)) {
    # The body in force at time 0: an event at time 0 is in force from it
    # (the gas typical band reads it; ChatGPT's review of PR #796, finding 9)
    body <- bodySteps$body[[max(1L, findInterval(0, bodySteps$time))]]
    Qco <- if (is.null(cardiacOutput)) body$Q_cardiac else cardiacOutput
  }
  list(results = results, state = out, timeLine = timeLine,
       body = body, circuit = circuit, weight = weight, age = age, Qco = Qco,
       deadSpace = deadSpace, bodySteps = bodySteps, bodyRow = bodyRow,
       cardiacOutput = cardiacOutput)
}

#' The agent carried across a change of body temperature
#'
#' A physiology event steps the body at a time (R/physiologyEvents.R).  What
#' is conserved is the amount of agent in each compartment.  In a gas phase
#' (the circuit and the alveoli) the amount is the partial pressure times
#' the volume, and neither the volume nor the partial pressure changes with
#' a step in cardiac output or temperature (a step in altitude moves gas in
#' or out of them, \code{gasStateAcrossPressure()}, which the engine applies
#' after this).  In a tissue the amount is the partial pressure times the
#' volume times the tissue:gas partition coefficient, which temperature
#' changes (gasPropertiesAt()), so the tension steps by the ratio of the old
#' coefficient to the new: a cooled tissue holds the same agent at a lower
#' tension.  A change of cardiac output changes only flows; the tissue
#' volumes are the same in every body of one run (gasBodySteps() splits the
#' vessel-rich group in all of them or none).  An endobronchial tube halves
#' the alveolar volume the engine ventilates: the gas of the lung it closes
#' is trapped and not followed, and the lung it reopens returns at the
#' ventilated lung's tension (R/endobronchial.R says the trapped gas is not
#' modelled).  Oxygen and nitrogen's tissue coefficients do not change with
#' temperature (gasPropertiesAt() changes the volatile agents' and nitrous
#' oxide's), so for them nothing moves.
#'
#' @param state named list of state vectors, one per gas
#' @param from,to gasPropertiesAt() tables of the two bodies
#' @returns the state in the new body
#' @keywords internal
gasStateAcrossBodies <- function(state, from, to)
{
  for (g in names(state)) {
    if (g == "oxygen") next
    a <- from[from$gas == g, ]
    b <- to[to$gas == g, ]
    if (nrow(a) != 1 || nrow(b) != 1) next
    la <- gasPartitionTissueGas(a)
    lb <- gasPartitionTissueGas(b)
    y <- state[[g]]
    n <- length(y)
    tissues <- c("brain", "muscle", "fat", "brain")[seq_len(n - 2L)]
    y[3:n] <- y[3:n] * unname(la[tissues] / lb[tissues])
    state[[g]] <- y
  }
  state
}

#' The gas phases carried across a change of barometric pressure
#'
#' The circuit's and the alveoli's tensions are partial pressures relative to
#' sea level (R/altitude.R): the circuit's dry gas totals \code{100 r}, r the
#' barometric pressure over a sea-level atmosphere (\code{gasPressureRatio()}),
#' and the alveoli's, humidified, \code{100 r h} (\code{gasHumidityRatio()}),
#' that is \code{100 (P - 47) / (760 - 47)}.  At a step in altitude the
#' lungs and the circuit keep their volume.  Where the pressure falls, gas
#' leaves each of them at its own composition, so every tension is scaled by
#' the new total over the old.  Where it rises, gas enters: the alveoli keep
#' what they hold and draw in gas to make up the difference.  In the
#' semi-closed circuit that gas is the circuit's own, compressed, and the
#' circuit, refilled from the fresh gas, which it nearly matches, is scaled.
#' The ideal and open circuits hold no gas, so the lungs draw in fresh gas at
#' the new pressure, and their entry, the inspired gas, set from the settings
#' at the next sub-step, is not scaled (scaled, a 2% sevoflurane vaporiser
#' read 3.77% for one row at a descent from 5000 m, and the lungs were filled
#' with the vapour's share of the thinner gas; CodeRabbit on PR #864).
#' Dissolved agent does not move at the step (tissue tensions are
#' unchanged).  Until 2026-11-24 the gas phases kept their tensions, so just
#' after a step from sea level to 5000 m the alveolar gases summed to 94% of
#' a sea-level atmosphere where at most 52% fits, and the alveolar oxygen read
#' twice what the new pressure allows (Gemini's gas-engine audit of
#' 2026-11-23, C-07).
#'
#' @param state named list of state vectors, one per gas (circuit first,
#'   alveoli second)
#' @param from,to the bodies before and after the step
#' @param circuit the breathing circuit ("ideal", "semi-closed" or "open")
#' @param fresh the fresh gas's tensions at the new pressure, by gas
#'   (\code{gasSettingsAt()}'s \code{Ffgf}): what the ideal and open circuits
#'   bring to lungs that are compressed
#' @returns the state after the step
#' @keywords internal
gasStateAcrossPressure <- function(state, from, to, circuit = "ideal", fresh = NULL)
{
  r1 <- gasPressureRatio(from)
  r2 <- gasPressureRatio(to)
  if (isTRUE(all.equal(r1, r2, tolerance = 1e-12))) return(state)
  a1 <- r1 * gasHumidityRatio(from)
  a2 <- r2 * gasHumidityRatio(to)
  for (g in names(state)) {
    y <- state[[g]]
    # The composition of the gas drawn in, as a fraction of its dry gas (x 100)
    drawn <- if (circuit == "semi-closed" || is.null(fresh) || is.na(fresh[g]))
      y[1] / r1 else unname(fresh[g]) / r2
    if (circuit == "semi-closed") y[1] <- y[1] * r2 / r1
    y[2] <- if (a2 < a1) y[2] * a2 / a1 else y[2] + drawn * (a2 - a1)
    state[[g]] <- y
  }
  state
}

#' The bodies of a gas run with physiology events
#'
#' @param weight patient weight, kg
#' @param events physiologyEventsFrom()'s data frame, at least one row
#' @returns list(time, body): normal physiology from 0 and each event's from
#'   its time.  If any body has cardiac output off normal, every body splits
#'   the vessel-rich group (gasBodyPhysiology(split = TRUE)), so the states
#'   are the same in all of them.  A body in force for no time (normal
#'   physiology when an event is at 0, or an event at the same time as the
#'   next) is left out: until 2026-11-24 the run started in the normal body
#'   whatever an event at 0 said, so its lungs started at sea-level room air
#'   at 5000 m (752 Torr where the ambient pressure is 420), its first time
#'   was recorded twice, and the Visualizer refused a run whose temperature
#'   was 30 C throughout as one whose temperature changed (Gemini's
#'   gas-engine audit of 2026-11-23, C-07).
#' @keywords internal
gasBodySteps <- function(weight, events)
{
  states <- physiologyEventStates(events)
  split <- any(vapply(states, function(x) x[["cardiac"]] != 1, TRUE))
  bodies <- lapply(states, function(x) {
    body <- gasBodyPhysiology(getGasBody(weight), x[["cardiac"]], split = split)
    if (x[["temperature"]] != 37) body$temperature <- unname(x[["temperature"]])
    if (x[["altitude"]] != ALTITUDE_DEFAULT) body$altitude <- unname(x[["altitude"]])
    body <- gasBodyEndobronchial(body, x[["endobronchial"]] == 1)
    if (x[["haemoglobin"]] != ARTERIAL_OXYGEN_HAEMOGLOBIN)
      body$haemoglobin <- unname(x[["haemoglobin"]])
    body
  })
  time <- c(0, events$Time)
  keep <- c(time[-1] > time[-length(time)], TRUE)
  list(time = time[keep], body = bodies[keep])
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
#' @param cardiacOutput optional override in L/min; defaults to GasMan's 5 L/min at 70 kg, scaled by (weight/70)^0.75
#' @param physiology NULL, or the physiology sliders' factors (see
#'   \code{getDrugPK()}); only \code{cardiac} acts on the gases, through
#'   \code{gasBodyPhysiology()}: cardiac output and its split among the tissue
#'   groups
#' @param temperature NULL (37 C), or the body temperature in C, which sets
#'   the agents' solubility and MAC (\code{gasPropertiesAt()}); kept in the
#'   body as \code{temperature}
#' @param altitude NULL (sea level), or metres above sea level (0 to 7000),
#'   kept in the body as \code{altitude}; the barometric pressure it sets
#'   (\code{barometricPressure()}, R/altitude.R) changes the fresh gas, room
#'   air and the volume taken up, not the potency
#' @param endobronchial TRUE for an endobronchial tube: one lung ventilated,
#'   the other perfused and shunting (\code{gasBodyEndobronchial()},
#'   R/endobronchial.R)
#' @param physiologyEvents NULL, or physiologyEventsFrom()'s data frame
#'   (R/physiologyEvents.R): with any row, physiology is normal from time 0
#'   and each event's from its time (cardiac output, temperature, altitude
#'   and intubation, \code{gasBodySteps()}), the agent in each compartment
#'   carried across each step, and \code{physiology}, \code{temperature},
#'   \code{altitude}, \code{endobronchial} and \code{haemoglobin} are not used
#' @param haemoglobin NULL (15 g/dL), or the haemoglobin, g/dL, of the
#'   arterial oxygen read off the run (\code{gasArterialOxygen()},
#'   R/arterialOxygen.R), kept in the body as \code{haemoglobin}; the engine
#'   does not read it
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
                          oxygenUptake = TRUE,
                          # Appended after the pre-existing arguments, so a
                          # positional call keeps its binding (exported).
                          physiology = NULL, temperature = NULL,
                          altitude = NULL, endobronchial = FALSE,
                          physiologyEvents = NULL, haemoglobin = NULL)
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

  body <- gasBodyPhysiology(getGasBody(weight),
                            checkPhysiology(physiology)[["cardiac"]])
  temperature <- checkBodyTemperature(temperature)
  if (temperature != 37) body$temperature <- temperature
  altitude <- checkAltitude(altitude)
  if (altitude != ALTITUDE_DEFAULT) body$altitude <- altitude
  body <- gasBodyEndobronchial(body, endobronchial)
  # The haemoglobin of the arterial oxygen (R/arterialOxygen.R); the engine
  # itself does not read it
  haemoglobin <- checkHaemoglobin(haemoglobin)
  if (haemoglobin != ARTERIAL_OXYGEN_HAEMOGLOBIN) body$haemoglobin <- haemoglobin
  # With physiology events, normal physiology from 0 and each event's from
  # its time; the other physiology arguments are not used
  # (R/physiologyEvents.R)
  bodySteps <- if (is.data.frame(physiologyEvents) && nrow(physiologyEvents) > 0)
    gasBodySteps(weight, physiologyEvents)

  sim <- advanceClosedFormGas(
    gasDose,
    weight        = weight,
    body          = body,
    age           = age,
    maximum       = maximum,
    cardiacOutput = cardiacOutput,
    uptakeEffect  = uptakeEffect,
    circuit       = circuit,
    deadSpace     = deadSpace,
    oxygenUptake  = oxygenUptake,
    bodySteps     = bodySteps
  )
  if (!is.null(bodySteps)) sim$physiologyEvents <- physiologyEvents
  sim
}
