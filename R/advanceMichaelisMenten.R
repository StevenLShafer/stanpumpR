# -----------------------------------------------------------------------------
# Numerical engine: Michaelis-Menten elimination (one or two compartments)
# -----------------------------------------------------------------------------
# Provenance
# ----------
# Drafted by Claude Code, 2026-10-10, at the request of Steven L. Shafer, for
# phenytoin, the one antiseizure drug whose systemic elimination saturates at
# therapeutic concentrations.  Verified by tests/testthat/test-michaelis-menten.R:
# against the closed-form engine when Km is so large that the elimination is
# linear, against the implicit analytical solution of an intravenous bolus,
# against the steady state Css = Km R / (Vmax - R) of a constant infusion, and
# by mass balance.
#
# WHY A SEPARATE ENGINE
# =====================
# Every other engine in the library is closed form: it adds exponentials, and
# superposition makes a dose table a sum of independent doses.  Michaelis-
# Menten elimination,
#
#     dA/dt = inputs(t) - Vmax C / (Km + C),        C = A / V,
#
# has no such solution for an arbitrary dose history, and superposition fails
# by design: the second dose is cleared more slowly because the first is still
# there, which is the clinically important behaviour (a modest dose increase
# gives a disproportionate rise at steady state).  So this engine integrates
# the whole dose table at once, with the classical fourth-order Runge-Kutta
# method, and nothing upstream may split the table into parts that are run
# separately and added (simCpCe() dispatches here before it splits oral
# formulations).
#
# THE PERIPHERAL COMPARTMENT (OPTIONAL)
# =====================================
# A model may declare first-order distribution to one peripheral compartment
# with the rate constants k23 and k32 (the source's own naming, central = 2
# and peripheral = 3), which adds
#
#     dA/dt  <- ... - k23 A + k32 Ap,     dAp/dt = k23 A - k32 Ap
#
# to the system above.  Saturable elimination still acts on the CENTRAL
# concentration only.  Both constants are optional and default to zero, which
# leaves the peripheral compartment permanently empty and every one-
# compartment drug's numbers bit-for-bit unchanged (test-michaelis-menten.R
# pins that).  The engine carries it on the same peripheral state as a PK set's
# cl2 and v2 (below): k23 A - k32 Ap is q (C - Cp) with q = k23 V and
# Vp = V k23 / k32.  A drug declaring one must declare both, and the peripheral
# volume is implied by k23/k32 x V rather than given: the source that needed
# this (GHB, Chen 2020) reports the rate constants and derives the volume the
# same way.
#
# WHY NOT A LINEAR TWO-COMPARTMENT FIT INSTEAD
# ============================================
# The peripheral compartment is linear, so it alone would not need this
# engine.  It is here because the ELIMINATION is saturable: a drug needing
# both (GHB) cannot use any closed-form engine, and could not be reduced to
# one compartment without losing the distribution phase its source fitted.
#
# THE INPUTS
# ==========
# All the absorption and conversion processes are linear, first order, and are
# integrated as states alongside the central amount:
#
#   - one depot per oral formulation (the drug's default oral form and each
#     entry of PK$oralFormulations), each with its own ka, bioavailability and
#     lag; a dose enters its depot at the time given plus the lag;
#   - an intravenous bolus enters the central compartment directly;
#   - an intravenous rate row ("mg/min", "mg/hr") is a running input to the
#     central compartment until the next such row;
#   - a prodrug given in drug equivalents ("mg PE", fosphenytoin) enters a
#     conversion compartment, which empties into the central compartment at
#     kConversion; given intramuscularly ("mg PE IM") it first enters an
#     intramuscular depot absorbed at ka_IM; given as a rate ("mg PE/min") it
#     is a running input to the conversion compartment until the next such row.
#
#   - a subcutaneous dose, for a drug whose block carries saturableSC
#     (exenatide, R/drugs_exenatide.R; Cirincione 2017): a share F (1 - fr)
#     of the dose enters the central compartment at a constant rate over the
#     next `duration` minutes, and the rest, D (1 - F (1 - fr)), then enters
#     a depot that empties into the central compartment at the saturable rate
#     F fr kaMax S / (kmKa + S).  The doses share the depot.
#
# Each dose is multiplied by its SALT FACTOR before it enters: the mass of the
# modelled moiety in a unit of what was given (phenytoin sodium is 0.92
# phenytoin acid by mass; see R/drugs_phenytoin.R).  The factors are in the
# model's michaelisMenten block, keyed "IV", "PE", "default" (the default oral
# form) and the names of the further oral formulations.
#
# DISPOSITION
# ===========
# The central compartment is the PK set's v1, eliminated by the saturable
# pathway and, when the block carries clLinear (volume per minute), by a
# parallel linear one.  A PK set with a positive cl2 adds a peripheral
# compartment of volume v2 (exenatide); phenytoin's cl2 is 0, which leaves
# it out.  The PK set's cl1 is not read: it holds the low-concentration
# limit for the help tables.  Elimination and distribution act on the
# UNBOUND concentrations, which are the amounts over the volumes unless the
# drug binds a target:
#
#   - rapidBinding (linagliptin, R/drugs_linagliptin.R; Retlich 2015): the
#     drug binds at equilibrium, with dissociation constant kd, to a
#     capacity bmaxCentral (a concentration) in the central compartment and
#     amaxPeripheral (an amount) in the peripheral one.  The states are the
#     total amounts; the unbound concentration is the root of the binding
#     equilibrium (unboundAtEquilibrium()), and the plasma concentration
#     plotted is the total, bound and unbound, as the assay measured it.
#   - targetBinding (vildagliptin, R/drugs_vildagliptin.R; Landersdorfer
#     2012): binding is a process, at a rate (rmax - DR) k2 C / (kd + C)
#     into a drug-target complex DR in each compartment, which dissociates
#     at koff and is hydrolysed at kdeg (freeing the target).  The central
#     and peripheral states are the free drug, and the plasma concentration
#     is the free drug, as the paper's output equation has it.
#
# rapidBinding may carry plotted = "unbound" (vincristinePediatric,
# R/drugs_vincristinePediatric.R; Nijstad 2022), for a drug whose bound pool
# is a tissue store fed from the central amount (vincristine on
# beta-tubulin) rather than drug bound in the plasma sample: the plasma
# concentration plotted is then the unbound central concentration, which is
# what that source observed.  The default, "total", is linagliptin's.  Nijstad
# fitted the binding as a process (kon 1300 /h, koff 11.5 /h), far too fast for
# this engine's step (kon is about 22 /min, beyond the Runge-Kutta stability
# limit of 2.78 / 2 min), so the drug declares its quasi-equilibrium, which is
# exactly this block: test-drugs-vincristinePediatric.R measures it against a
# fine-step integration of the full binding ODE.
#
# SATURABLE DISTRIBUTION (OPTIONAL)
# =================================
# A block saturableDistribution = list(vmax, km, k21) (paclitaxel,
# R/drugs_paclitaxelJoerger.R; Joerger 2006) adds a further compartment, held as an
# amount with no volume, filled from the central compartment at the saturable
# rate vmax Cu / (km + Cu) (vmax an amount per minute, km a concentration) and
# emptied back into it at the first-order rate k21 (/min) times its amount:
#
#     dA/dt  <- ... - vmax_T Cu / (km_T + Cu) + k21 As,
#     dAs/dt  =       vmax_T Cu / (km_T + Cu) - k21 As.
#
# With volume (litres) in place of k21, the return is saturable too, by the
# same carrier: its rate constant is vmax / (volume (km + Cu)), saturated by
# the CENTRAL concentration, applied to the store's amount (nab-paclitaxel,
# R/drugs_paclitaxelProteinBound.R; Chen 2014, Fig. 1, whose legend defines km
# by the central concentration):
#
#     dAs/dt  =  vmax_T (Cu - Cs) / (km_T + Cu),  Cs = As / volume.
#
# Below Km that is a linear compartment of that volume, entered and left at
# the clearance vmax / km.  Read it as sd[["volume"]]: `$` matches a prefix,
# and a name such as v would read vmax where it is absent.
#
# It sits beside the linear peripheral compartment of the PK set's cl2 and v2
# (which carries paclitaxel's own second, linear, peripheral compartment, its
# V3).  Having no volume it has no concentration, so the compartment panels
# (R/compartments.R) show only the linear one.
#
# An oral dose of a drug whose block carries kaTransit (vildagliptin's two
# sequential absorption compartments) passes through a transit compartment,
# emptied at kaTransit, before the default oral depot.
#
# AUTOINDUCTION (OPTIONAL)
# ========================
# A block autoinduction = list(cl, kEnz, emax, ec50, imax, ic50) adds a
# clearance cl (volume per minute) multiplied by a relative enzyme amount E,
# a state that starts at 1 (the uninduced enzyme) and turns over at kEnz
# (/min), formed faster by the drug (an Emax term) and degraded more slowly
# (an inhibition term), both driven by the unbound central concentration:
#
#     dA/dt  <- ... - cl E Cu,
#     dE/dt   = kEnz [(1 + emax Cu / (ec50 + Cu)) - E (1 - imax Cu / (ic50 + Cu))].
#
# The two published forms are special cases: ifosfamide's (Kerbusch 2000,
# R/drugs_ifosfamide.R), where the drug inhibits the enzyme's degradation
# (emax 0, imax 1, so E rises without bound toward 1 + Cu/ic50), and
# cyclophosphamide's in children (McCune 2009, R/drugs_cyclophosphamideMcCune.R),
# where it raises the enzyme's formation (imax 0, E toward 1 + emax).  The
# clearance that is not induced is clLinear, and vmax is then 0.  The enzyme
# state is carried through the whole dose table, so induction accumulates
# over a course of daily doses and fades after the last, which is why the
# drug needs this engine: the clearance depends on the dose history.  Tests:
# test-autoinduction.R (an independent fine-step integration, the steady
# state of a constant infusion, the fall back to the uninduced curve when kEnz
# is negligible, and mass balance).
#
# An optional hill makes the formation term sigmoid, emax Cu^hill /
# (ec50^hill + Cu^hill): suzetrigine's induction (FDA Integrated Review of NDA
# 219209, R/drugs_suzetrigine.R) has a Hill coefficient of 10, so the enzyme
# is a near switch around 430 ng/mL.  Without hill the term is the hyperbola,
# computed as before.
#
# METABOLITE (OPTIONAL)
# =====================
# A block metabolite = list(name, fraction, v1, cl, q, v2, induced, mwRatio)
# advances an active metabolite with the parent, as advanceTransitChain()
# advances enalaprilat, because the closed-form metabolite route convolves
# exponential amplitudes a nonlinear system does not have.  It is formed from
# a fraction of everything the parent loses by elimination (the saturable,
# linear and induced pathways together; distribution is not loss), and has a
# central compartment v1, cleared at cl (times the enzyme E when induced is
# TRUE: suzetrigine's M6-SUZ, whose clearance Figure 24 multiplies by the same
# AENZ), with an optional peripheral compartment (q, v2):
#
#     dAm/dt  = fraction mwRatio (eliminated) - cl [E] Am/v1 - q (Am/v1 - Amp/v2),
#     dAmp/dt = q (Am/v1 - Amp/v2).
#
# getDrugPK() names the metabolite in the PK set, as for a transit chain, and
# adds its unitScale; the engine returns CpMetabolite, CeMetabolite (NA) and
# C2Metabolite, which simCpCe() lifts into the metabolite drug's own series.
#
# ZERO-ORDER RELEASE INTO THE DEPOT (OPTIONAL)
# ============================================
# zeroOrderDepot (minutes): each oral dose, after its lag, enters its depot
# at a constant rate over that long rather than all at once, and is then
# absorbed at first order (suzetrigine's tablet, D1TAB 1.1 h).  The release's
# start and end are points of the time line, so no step straddles one.
#
# AN ABSORPTION RATE THAT DEPENDS ON THE DOSE (OPTIONAL)
# ======================================================
# kaDoseDependence = list(emax, d50, hill): an oral dose of D mg is absorbed
# at ka (1 - emax D^hill / (d50^hill + D^hill)) (suzetrigine, Table 149's KA
# dose effect).  Each distinct dose size of the default oral form gets a
# depot of its own, so that a 100 mg and a 50 mg dose absorbed at once keep
# their own rates.  simCpCe() passes the dose as given, in mg (DoseMg), before
# any scaling of its bioavailability.  Tests: test-drugs-suzetrigine.R (an
# independent fine-step integration of the published model with all three).
#
# ORAL PATHS (OPTIONAL)
# =====================
# oralPaths = list of list(share, ka, lag, zeroOrder, into): each plain oral
# dose is split among parallel paths, each its own depot emptied at its own
# ka, after its own lag and, where it has one, a zero-order release over
# zeroOrder minutes; a path into "metabolite" empties into the metabolite's
# central compartment, as a dose formed before it reaches the circulation
# (macitentan and aprocitentan, Bartolucci 2021, R/drugs_macitentan.R).  A
# path's share of a dose is a constant, times p or 1 - p where p =
# plogis(gamma - beta log(1 + D)) with D the dose in mg (mmPathShare()).  Not
# with kaTransit, kaDoseDependence or zeroOrderDepot.  Tests:
# test-drugs-macitentan.R (an independent integration of the published model).
#
# STEP SIZE
# =========
# The engine reports on the library's time line (simulationTimeGrid()), so the
# plot and the derived series behave as for any other drug.  Between its
# points it takes adaptive steps (mmDormandPrince(): Dormand and Prince's
# embedded 5(4) pair, error held to MM_ADAPTIVE_RTOL of each state plus
# MM_ADAPTIVE_ATOL of the largest dose), every drug's default since
# 2026-11-15 and phenytoin's since 2026-11-14: short steps while a dose is
# absorbed, hours between doses.  Calibrated on sixteen regimens of the seven
# drugs against a 0.1-minute fixed step: the largest error is 1.9e-6 of the
# level where it is above 5% of its peak, where the fixed step's was 3.2e-3
# (oral voriconazole), and over 52 weeks it is 2 to 14 times faster.
#
# A block's adaptiveStep = FALSE selects the classical Runge-Kutta step of at
# most MM_MAX_STEP minutes, the second method the tests compare with, held
# within that method's region of stability for the fastest process while
# drug is being absorbed (mmStateRates()): at 2 minutes alone, voriconazole's
# absorption (ka 100 /h) multiplied what was left in the depot by 2.2 a step.
# Vildagliptin's binding to tissue DPP-4 while the target is free (a rate
# constant near 0.7/min) holds its fixed step to about 1.4 minutes.  A jump
# (a bolus, a dose arriving in a depot) and a change of rate happen only at
# points of the time line, which always carries every dose time, so no step
# straddles one.  So does a switch of the antiseizure interactions' Vmax and
# Km multipliers, which is a point of the line but not a knot of it (no fill
# after it, no instant before it: it bends the curve and moves no amount).
#
# Where the adaptive step is held short between doses it is by accuracy as
# much as by stiffness: vildagliptin's binding to tissue DPP-4 is the fastest
# process (about 0.7/min), and with the binding taken out its 30-day run took
# 5,218 steps against 7,721 (2026-11-15), so an implicit step for the binding
# would buy at most a third of the steps, at several times the cost of each.
#
# NO EFFECT SITE AND NO TIME UNTIL THRESHOLD
# ==========================================
# Ce is NA throughout (the drug is plotted as plasma only), and the time until
# threshold is not computed: it would need a fresh nonlinear simulation from
# every point of the time line.  The Recovery column is NA.
# -----------------------------------------------------------------------------

#' Largest Runge-Kutta step of the Michaelis-Menten engine, minutes: the fixed
#' step's (adaptiveStep = FALSE), and the adaptive step's first after a dose
#' @keywords internal
MM_MAX_STEP <- 2

#' Relative tolerance of the engine's adaptive step (a drug whose
#' michaelisMenten block sets adaptiveStep, mmDormandPrince())
#' @keywords internal
# Calibrated 2026-11-14 on eleven phenytoin regimens (single, qd, bid and tid
# oral doses over up to 91 days, intravenous loads and infusions,
# fosphenytoin IV and IM) against a 0.25-minute fixed step: at 1e-6 the
# largest error is 4.5e-7 of the level where the level is above 5% of its
# peak (the fixed 2-minute step's was up to 2.3e-7), against the 1% the
# user set as the bound (Shafer: the assay is 1%, the model 30%).  Looser
# buys almost nothing, the step being held to the time line's points.
MM_ADAPTIVE_RTOL <- 1e-6

#' The fixed step's limit as a multiple of the reciprocal of the fastest
#' rate (mmStateRates())
#' @keywords internal
# The eigenvalues of a compartmental system lie in the discs centred on its
# diagonal, each of radius at most the centre's magnitude (a compartment
# loses no more than it holds); scaled by a step h with h |J_ii| <= 1.2,
# every such disc lies inside the classical Runge-Kutta method's region of
# stability (|R(z)| <= 1 on the disc of centre -1.2 and radius 1.2, checked
# numerically 2026-11-15).  1 keeps a margin.
MM_STABLE_STEP <- 1

#' What is left in an absorption state below this fraction of the largest
#' dose is taken to have been absorbed (advanceMichaelisMenten())
#' @keywords internal
MM_NEGLIGIBLE_AMOUNT <- 1e-12

#' The rate of each state's first-order loss in a Michaelis-Menten system
#'
#' The magnitudes of the Jacobian's diagonal at empty compartments, where a
#' saturable process is at its fastest, by a forward difference of the
#' derivative.
#'
#' @param f function(x) giving the derivative with no input
#' @param n the number of states
#' @returns a rate per state, per minute
#' @keywords internal
mmStateRates <- function(f, n)
{
  zero <- numeric(n)
  base <- f(zero)
  delta <- 1e-9
  rates <- vapply(seq_len(n), function(i) {
    x <- zero
    x[i] <- delta
    abs(f(x)[i] - base[i]) / delta
  }, numeric(1))
  rates[!is.finite(rates)] <- 0
  rates
}

#' Absolute tolerance of the engine's adaptive step, as a fraction of the
#' largest dose or of an hour of the largest rate given so far
#' (mmDormandPrince())
#' @keywords internal
# Calibrated 2026-11-15 on sixteen regimens of the seven drugs over two weeks
# against a fixed step of 0.1 minute: at 1e-6 of the largest dose (the
# relative tolerance), a drug whose plotted compartment holds a small share
# of the dose was held to too little of its level (cetagliptin 6e-4 at a
# tenth of its peak); at 1e-9 the largest error is 1.9e-6 of the level where
# the level is above 5% of its peak, at much the same cost (the fixed step's
# largest was 3.2e-3, oral voriconazole's).
MM_ADAPTIVE_ATOL <- 1e-9

#' The smallest adaptive step, as a fraction of its interval: a step that
#' still fails its error test there stops the simulation (mmDormandPrince())
#' @keywords internal
MM_ADAPTIVE_MIN_STEP <- 1e-9

#' Advance the Michaelis-Menten engine's states across one interval of its
#' time line with an adaptive step
#'
#' The embedded Runge-Kutta pair of Dormand and Prince (1980), fifth order
#' with a fourth-order error estimate, the first stage of each step the last
#' of the one before.  A step is taken when its estimated error is within
#' `atol + rtol * |x|` in every state, and the next is sized from it.  The
#' engine's fixed 2-minute step is set by its fastest processes (fosphenytoin's
#' conversion, absorption), which last only while a depot holds drug; for
#' phenytoin the rest of the time only saturable elimination is left, at
#' about 1e-5 per minute, and an adaptive step takes it in steps of hours
#' (2026-11-14).  Doses arrive at points of the time line, never inside an
#' interval, so each interval is smooth.
#'
#' Three guards (Carbon's second UI audit, 2026-11-15, F2).  A step whose
#' error cannot be estimated (a stage that left the states' domain gives NaN
#' or Inf) is refused like a step that fails its test, where it stopped the
#' simulation with "missing value where TRUE/FALSE needed".  A step that
#' still fails at `MM_ADAPTIVE_MIN_STEP` of the interval stops the
#' simulation with a message, where it was taken anyway and the next one
#' made five times smaller, until t + h == t and the loop never ended.
#' And a system left with every state within `atol` of zero and no input is
#' zero for the rest of the interval: the states are amounts, which with no
#' input can only fall, so the zero is within the tolerance already
#' accepted.  Without it, an explicit method on a system made stiff by the
#' Bayesian search's corner (20 times the clearance and a twentieth of the
#' volume in a 0.5 kg neonate) stepped noise below `atol` at its stability
#' limit for the rest of the plot: 240,000 derivatives over 52 weeks for a
#' drug gone within hours.  An accepted step's negative states (an amount
#' cannot be negative) are set to zero, as the fixed step's are.
#'
#' @param f function(t, x) giving the derivative
#' @param x the states at `t0`
#' @param t0,t1 the interval
#' @param h the first step to try
#' @param rtol,atol the tolerances
#' @param clean function(x) applied to the states after each step taken
#'   (advanceMichaelisMenten() empties a depot left with less than `atol`),
#'   or NULL
#' @returns a list: `x`, the states at `t1`, and `h`, the step to try next
#' @keywords internal
mmDormandPrince <- function(f, x, t0, t1, h, rtol, atol, clean = NULL)
{
  t <- t0
  span <- t1 - t0
  hMin <- max(MM_ADAPTIVE_MIN_STEP * span, 64 * .Machine$double.eps * max(1, abs(t1)))
  # Whether the system has an input in this interval: the derivative at zero
  # (computed once, when first needed)
  inputFree <- NULL
  settled <- function(x) {
    if (any(abs(x) > atol)) return(FALSE)
    if (is.null(inputFree)) inputFree <<- all(f(t, 0 * x) == 0)
    inputFree
  }
  if (settled(x)) return(list(x = 0 * x, h = h))
  k1 <- f(t, x)
  while (t < t1) {
    last <- h >= t1 - t
    if (last) h <- t1 - t
    repeat {
      k2 <- f(t + h / 5, x + h * (k1 / 5))
      k3 <- f(t + 3 * h / 10, x + h * (3 / 40 * k1 + 9 / 40 * k2))
      k4 <- f(t + 4 * h / 5, x + h * (44 / 45 * k1 - 56 / 15 * k2 + 32 / 9 * k3))
      k5 <- f(t + 8 * h / 9, x + h * (19372 / 6561 * k1 - 25360 / 2187 * k2 +
                                       64448 / 6561 * k3 - 212 / 729 * k4))
      k6 <- f(t + h, x + h * (9017 / 3168 * k1 - 355 / 33 * k2 + 46732 / 5247 * k3 +
                                49 / 176 * k4 - 5103 / 18656 * k5))
      xNew <- x + h * (35 / 384 * k1 + 500 / 1113 * k3 + 125 / 192 * k4 -
                         2187 / 6784 * k5 + 11 / 84 * k6)
      k7 <- f(t + h, xNew)
      e <- h * (71 / 57600 * k1 - 71 / 16695 * k3 + 71 / 1920 * k4 -
                  17253 / 339200 * k5 + 22 / 525 * k6 - 1 / 40 * k7)
      err <- max(abs(e) / (atol + rtol * pmax(abs(x), abs(xNew))))
      if (is.finite(err) && err <= 1) break
      if (h <= hMin)
        stop("The concentration could not be computed: the adaptive step ",
             "failed at ", signif(t, 6), " minutes.", call. = FALSE)
      h <- max(hMin, h * (if (is.finite(err)) max(0.2, 0.9 * err^(-1 / 5)) else 0.2))
      last <- FALSE
    }
    t <- if (last) t1 else t + h
    x <- pmax(xNew, 0)
    if (!is.null(clean)) x <- clean(x)
    k1 <- if (identical(x, xNew)) k7 else f(t, x)
    h <- max(hMin, h * min(5, max(0.2, 0.9 * max(err, 1e-10)^(-1 / 5))))
    if (t < t1 && settled(x)) return(list(x = 0 * x, h = h))
  }
  list(x = x, h = h)
}

#' The Michaelis-Menten engine's derivative
#'
#' The derivative of the states of \code{advanceMichaelisMenten()} (see its
#' header for the states and the processes), as a closure over the run's
#' constants.  Elimination and distribution act on the unbound
#' concentrations.  With targetBinding the central and peripheral amounts are
#' the free drug, and binding to the target (k2, saturable in the free target,
#' Rmax - DR), dissociation (koff) and hydrolysis of the complex (kdeg, which
#' frees the target) are states of their own (Landersdorfer 2012,
#' vildagliptin).
#'
#' Called six times a step (385,000 times for 249 days of vildagliptin), so
#' what does not change during a run is settled here: a term a drug does not
#' have is not computed, rather than computed as an exact zero (the results
#' are identical, bit for bit), and binding that is not there is not a
#' function call.  A Bayesian fit simulates the drug dozens of times
#' (2026-11-14).
#'
#' It was a closure in \code{advanceMichaelisMenten()} itself, whose frame of
#' some 110 variables, not hashed, was searched for every name it read: 45%
#' of vildagliptin's time (16.4 against 9.1 microseconds a call).  Built here,
#' the closure reads a small frame of its own and is byte-compiled with this
#' function.  (Moving the old closure with \code{environment<-} would not do:
#' R drops a closure's byte code when its environment is replaced, and the
#' JIT did not compile it again, so every simulation after the first in a
#' process ran twice as slow.)  The same expressions on the same values, so
#' the same doubles (Carbon's fourth UI audit, 2026-11-15, C3).
#'
#' @param V,V2 the central and peripheral volumes
#' @param ka the absorption rate constants of the oral depots and the
#'   intramuscular one
#' @param kT,kc the transit and prodrug-conversion rate constants (0 if none)
#' @param q the distribution clearance (0 if none)
#' @param clLinear the parallel linear clearance (0 if none)
#' @param nOral the number of oral depots
#' @param iIM,iConv,iC,iP,iS,iT,iDRC,iDRP,iSD,iE the states' indices
#' @param rb,sc,tb,sd,ai the block's rapidBinding, saturableSC,
#'   targetBinding, saturableDistribution and autoinduction, or NULL
#' @param met the block's metabolite, or NULL, with the indices of its
#'   central and peripheral states added as iC and iP
#' @param nState the number of states (mmStateCount())
#' @returns function(x, rIV, rPE, vmax, km, rD = 0), the derivative of the
#'   states, rD the zero-order release rates into the oral depots
#' @keywords internal
#' The engine's number of states
#'
#' The saturable-distribution and enzyme states exist only for a drug that
#' declares them: carried always, the enzyme's constant 1 kept the adaptive
#' step from ever finding the system empty (mmDormandPrince()), and a year of
#' phenytoin cost 1.9 s against 1.5.
#' @keywords internal
mmStateCount <- function(iDRP, iSD, iE, sd, ai, iMP = iE + 2L, met = NULL)
  if (!is.null(met)) iMP else if (!is.null(ai)) iE else if (!is.null(sd)) iSD else iDRP

mmDerivative <- function(V, V2, ka, kT, kc, q, clLinear, nOral, iIM, iConv, iC, iP,
                         iS, iT, iDRC, iDRP, rb, sc, tb, iSD = iDRP + 1L,
                         iE = iDRP + 2L, sd = NULL, ai = NULL, met = NULL,
                         nState = mmStateCount(iDRP, iSD, iE, sd, ai, met$iP, met))
{
  # Unbound concentrations from the amounts: the amount over the volume, or,
  # with rapid binding, the root of the binding equilibrium.
  unboundC <- if (is.null(rb)) function(a) a / V else
    function(a) unboundAtEquilibrium(a / V, rb$bmaxCentral, rb$kd)
  unboundP <- if (is.null(rb)) function(a) a / V2 else
    function(a) unboundAtEquilibrium(a / V2, rb$amaxPeripheral / V2, rb$kd)
  scAbsorption <- if (is.null(sc)) function(S) 0 else
    function(S) sc$bioavailability * sc$fraction * sc$kaMax * S / (sc$kmKa + S)
  depotIdx <- seq_len(iIM)
  # The oral depots that empty into the parent: all but those of an oral
  # path that delivers the metabolite itself (oralPaths)
  oralIdx <- setdiff(seq_len(nOral), met$oralIdx)
  plainBinding <- is.null(rb)
  hasSD <- !is.null(sd)
  hasAI <- !is.null(ai)
  if (hasAI) {
    aiEmax <- if (is.null(ai$emax)) 0 else ai$emax
    aiImax <- if (is.null(ai$imax)) 0 else ai$imax
    aiEc50 <- if (aiEmax > 0) ai$ec50 else 1
    aiIc50 <- if (aiImax > 0) ai$ic50 else 1
    # A sigmoid induction (suzetrigine's Hill coefficient of 10); the
    # hyperbola, the same doubles as before, when there is none
    aiHill <- if (is.null(ai$hill)) 1 else ai$hill
    aiStim <- if (aiHill == 1) function(C) C / (aiEc50 + C) else
      function(C) { Ch <- C^aiHill; Ch / (aiEc50^aiHill + Ch) }
  }
  # A metabolite formed from the parent's elimination and advanced here
  # (METABOLITE in the header)
  hasMet <- !is.null(met)
  if (hasMet) {
    iMC <- met$iC
    iMP <- met$iP
    metFraction <- met$fraction
    metV <- met$v1
    metCl <- met$cl
    metQ <- if (is.null(met$q)) 0 else met$q
    metV2 <- if (metQ > 0) met$v2 else 1
    metInduced <- isTRUE(met$induced)
    metYield <- if (is.null(met$mwRatio)) 1 else met$mwRatio
    metOral <- met$oralIdx
  }
  hasTransit <- kT != 0
  hasSC <- !is.null(sc)
  hasTarget <- !is.null(tb)
  hasPeripheral <- q != 0 || hasTarget
  # The binding constants as plain numbers, each the block's own value (a
  # list lookup at every call cost time and changed no number)
  if (hasTarget) {
    kOut <- tb$koff + tb$kdeg
    bindK <- tb$k2
    rmaxC <- tb$rmaxCentral
    rmaxP <- tb$rmaxPeripheral
    kdT <- tb$kd
    koffT <- tb$koff
  }
  function(x, rIV, rPE, vmax, km, rD = 0) {
    Cu <- if (plainBinding) x[iC] / V else unboundC(x[iC])
    dx <- numeric(nState)
    out <- ka * x[depotIdx]
    dx[depotIdx] <- -out
    # A zero-order release into the oral depots (zeroOrderDepot)
    dx[seq_along(rD)] <- dx[seq_along(rD)] + rD
    if (hasTransit) {
      xT <- x[iT]
      dx[iT] <- -kT * xT
      dx[1] <- dx[1] + kT * xT
    }
    xConv <- x[iConv]
    dx[iConv] <- out[iIM] + rPE - kc * xConv
    fromS <- 0
    if (hasSC) {
      fromS <- scAbsorption(x[iS])
      dx[iS] <- -fromS
    }
    toP <- 0
    if (hasPeripheral) {
      Cpu <- if (plainBinding) x[iP] / V2 else unboundP(x[iP])
      toP <- q * (Cu - Cpu)
      dx[iP] <- toP
    }
    saturable <- vmax * Cu / (km + Cu)
    dx[iC] <- sum(out[oralIdx]) + kc * xConv + rIV + fromS -
      saturable - clLinear * Cu - toP
    eliminated <- saturable + clLinear * Cu
    if (hasTarget) {
      xDRC <- x[iDRC]
      xDRP <- x[iDRP]
      bindC <- (rmaxC - xDRC) * bindK * Cu / (kdT + Cu)
      bindP <- (rmaxP - xDRP) * bindK * Cpu / (kdT + Cpu)
      dx[iDRC] <- bindC - kOut * xDRC
      dx[iDRP] <- bindP - kOut * xDRP
      dx[iC] <- dx[iC] - bindC + koffT * xDRC
      dx[iP] <- dx[iP] - bindP + koffT * xDRP
    }
    # Saturable distribution to a compartment held as an amount (paclitaxel)
    if (hasSD) {
      toS <- sd$vmax * Cu / (sd$km + Cu)
      # emptied at first order (Joerger), or saturably from the store's own
      # concentration when the block gives its volume v (nab-paclitaxel, Chen 2014)
      fromSD <- if (is.null(sd[["volume"]])) sd$k21 * x[iSD] else
        sd$vmax * (x[iSD] / sd[["volume"]]) / (sd$km + Cu)
      dx[iSD] <- toS - fromSD
      dx[iC] <- dx[iC] - toS + fromSD
    }
    # A clearance multiplied by a relative enzyme amount the drug induces
    # (AUTOINDUCTION in the header)
    if (hasAI) {
      E <- x[iE]
      induced <- ai$cl * E * Cu
      dx[iC] <- dx[iC] - induced
      eliminated <- eliminated + induced
      dx[iE] <- ai$kEnz * ((1 + aiEmax * aiStim(Cu)) -
                             E * (1 - aiImax * Cu / (aiIc50 + Cu)))
    }
    # The metabolite: a fraction of everything the parent loses by
    # elimination, cleared at a clearance the enzyme multiplies too where the
    # block says so (suzetrigine's M6-SUZ)
    if (hasMet) {
      Cm <- x[iMC] / metV
      clM <- if (metInduced) metCl * x[iE] else metCl
      toMP <- if (metQ > 0) metQ * (Cm - x[iMP] / metV2) else 0
      dx[iMC] <- metFraction * metYield * eliminated - clM * Cm - toMP
      if (length(metOral)) dx[iMC] <- dx[iMC] + metYield * sum(out[metOral])
      if (metQ > 0) dx[iMP] <- toMP
    }
    dx
  }
}

#' The derivative over one interval of the engine's time line
#'
#' What \code{mmDormandPrince()} integrates: the engine's derivative with the
#' input rates and the Vmax and Km in force over the interval, and Vmax's
#' decline with time where the drug has one.  Built here, in a frame holding
#' only those, for the reason \code{mmDerivative()} gives; the same call of
#' the derivative with the same values.
#'
#' @param deriv the engine's derivative, function(x, rIV, rPE, vmax, km)
#' @param rIV,rPE the intravenous and prodrug rates over the interval
#' @param vmax,km Vmax and Km over the interval
#' @param decline function(t) giving Vmax's fraction left at t, or NULL
#' @param rD the zero-order release rates into the oral depots
#' @returns function(t, y), the derivative
#' @keywords internal
mmIntervalDerivative <- function(deriv, rIV, rPE, vmax, km, decline = NULL, rD = 0)
{
  force(deriv); force(rIV); force(rPE); force(vmax); force(km); force(decline)
  force(rD)
  if (is.null(decline)) function(t, y) deriv(y, rIV, rPE, vmax, km, rD) else
    function(t, y) deriv(y, rIV, rPE, vmax * decline(t), km, rD)
}

#' Validate a model's michaelisMenten block
#'
#' @param mm the block a drug model returned, or NULL
#' @param drug the drug's name, for the error message
#' @returns `mm`, unchanged, if valid
#' @keywords internal
validateMichaelisMenten <- function(mm, drug)
{
  if (is.null(mm)) return(NULL)
  ok <- is.list(mm) &&
    is_valid_number(mm$vmax) && mm$vmax >= 0 &&
    is_valid_number(mm$km) && mm$km > 0 &&
    (mm$vmax > 0 || (is_valid_number(mm$clLinear) && mm$clLinear > 0) ||
       !is.null(mm$autoinduction))
  if (!ok)
    stop("Invalid michaelisMenten for ", drug, ": needs a vmax (mass per ",
         "minute) and a positive km (concentration), and a positive vmax ",
         "unless clLinear is positive or the block carries autoinduction.")
  for (s in names(mm$saltFactor))
    if (!is_valid_number(mm$saltFactor[[s]], 0, 1) || mm$saltFactor[[s]] <= 0)
      stop("Invalid michaelisMenten for ", drug, ": salt factor '", s,
           "' must lie in (0, 1].")
  if (!is.null(mm$kConversion) &&
      !(is_valid_number(mm$kConversion) && mm$kConversion > 0))
    stop("Invalid michaelisMenten for ", drug, ": kConversion must be positive.")
  if (!is.null(mm$clLinear) &&
      !(is_valid_number(mm$clLinear) && mm$clLinear >= 0))
    stop("Invalid michaelisMenten for ", drug, ": clLinear must be zero or positive.")
  vd <- mm$vmaxDecline
  if (!is.null(vd) &&
      !(is.list(vd) && is_valid_number(vd$fraction, 0, 1) &&
        is_valid_number(vd$t50) && is_valid_number(vd$onset) && vd$onset >= 0 &&
        vd$t50 > vd$onset))
    stop("Invalid michaelisMenten for ", drug, ": vmaxDecline needs a fraction in ",
         "[0, 1], an onset of zero or more and a t50 after the onset (minutes).")
  sc <- mm$saturableSC
  if (!is.null(sc)) {
    ok <- is.list(sc) &&
      is_valid_number(sc$fraction, 0, 1) &&
      is_valid_number(sc$duration) && sc$duration > 0 &&
      is_valid_number(sc$kaMax) && sc$kaMax > 0 &&
      is_valid_number(sc$kmKa) && sc$kmKa > 0 &&
      is_valid_number(sc$bioavailability, 0, 1) && sc$bioavailability > 0
    if (!ok)
      stop("Invalid michaelisMenten for ", drug, ": saturableSC needs a fraction ",
           "and a bioavailability in [0, 1] (bioavailability above 0), and a ",
           "positive duration (min), kaMax (mass/min) and kmKa (mass).")
  }
  positive <- function(v) is_valid_number(v) && v > 0
  rb <- mm$rapidBinding
  if (!is.null(rb) && !(is.list(rb) && positive(rb$kd) &&
                        is_valid_number(rb$bmaxCentral) && rb$bmaxCentral >= 0 &&
                        is_valid_number(rb$amaxPeripheral) && rb$amaxPeripheral >= 0))
    stop("Invalid michaelisMenten for ", drug, ": rapidBinding needs a positive ",
         "kd and a bmaxCentral (concentration) and amaxPeripheral (amount) of ",
         "zero or more.")
  tb <- mm$targetBinding
  if (!is.null(tb) && !(is.list(tb) && positive(tb$kd) && positive(tb$k2) &&
                        is_valid_number(tb$koff) && tb$koff >= 0 &&
                        is_valid_number(tb$kdeg) && tb$kdeg >= 0 &&
                        is_valid_number(tb$rmaxCentral) && tb$rmaxCentral >= 0 &&
                        is_valid_number(tb$rmaxPeripheral) && tb$rmaxPeripheral >= 0))
    stop("Invalid michaelisMenten for ", drug, ": targetBinding needs a positive ",
         "kd and k2, and koff, kdeg, rmaxCentral and rmaxPeripheral of zero or more.")
  if (!is.null(rb) && !is.null(rb$plotted) &&
      !(is.character(rb$plotted) && length(rb$plotted) == 1L &&
        rb$plotted %in% c("total", "unbound")))
    stop("Invalid michaelisMenten for ", drug, ": rapidBinding's plotted must be ",
         "\"total\" or \"unbound\".")
  sd <- mm$saturableDistribution
  if (!is.null(sd) && !(is.list(sd) && positive(sd$vmax) && positive(sd$km) &&
                        xor(positive(sd[["k21"]]), positive(sd[["volume"]])) &&
                        (is.null(sd[["k21"]]) || is.null(sd[["volume"]]))))
    stop("Invalid michaelisMenten for ", drug, ": saturableDistribution needs a ",
         "positive vmax (amount per minute), km (concentration) and either k21 ",
         "(/min, a first-order return) or volume (the store's volume, a saturable return).")
  if (!is.null(rb) && !is.null(tb))
    stop("Invalid michaelisMenten for ", drug, ": rapidBinding and targetBinding ",
         "are alternatives.")
  ai <- mm$autoinduction
  if (!is.null(ai)) {
    nonneg <- function(v) is.null(v) || (is_valid_number(v) && v >= 0)
    ok <- is.list(ai) && positive(ai$cl) && positive(ai$kEnz) &&
      nonneg(ai$emax) && nonneg(ai$imax) &&
      (is.null(ai$imax) || ai$imax <= 1) &&
      (is.null(ai$emax) || ai$emax == 0 || positive(ai$ec50)) &&
      (is.null(ai$imax) || ai$imax == 0 || positive(ai$ic50))
    if (!ok)
      stop("Invalid michaelisMenten for ", drug, ": autoinduction needs a positive ",
           "cl (volume/min) and kEnz (/min), an emax of zero or more (with a ",
           "positive ec50 when above zero) and an imax in [0, 1] (with a positive ",
           "ic50 when above zero).")
    if (!is.null(ai$hill) && !positive(ai$hill))
      stop("Invalid michaelisMenten for ", drug, ": autoinduction's hill must be ",
           "positive.")
  }
  met <- mm$metabolite
  if (!is.null(met)) {
    ok <- is.list(met) && is.character(met$name) && length(met$name) == 1L &&
      nzchar(met$name) && is_valid_number(met$fraction, 0, 1) &&
      positive(met$v1) && positive(met$cl) &&
      (is.null(met$q) || (is_valid_number(met$q) && met$q >= 0)) &&
      (is.null(met$q) || met$q == 0 || positive(met$v2)) &&
      (is.null(met$mwRatio) || positive(met$mwRatio)) &&
      (is.null(met$induced) || isTRUE(met$induced) || isFALSE(met$induced))
    if (!ok)
      stop("Invalid michaelisMenten for ", drug, ": metabolite needs a name, a ",
           "fraction in [0, 1], a positive v1 (L) and cl (L/min), a q of zero ",
           "or more (with a positive v2 when above zero), and optionally a ",
           "positive mwRatio and a logical induced.")
    if (isTRUE(met$induced) && is.null(ai))
      stop("Invalid michaelisMenten for ", drug, ": an induced metabolite ",
           "clearance needs the block's autoinduction.")
  }
  if (!is.null(mm$zeroOrderDepot) && !positive(mm$zeroOrderDepot))
    stop("Invalid michaelisMenten for ", drug, ": zeroOrderDepot must be a ",
         "positive duration (minutes).")
  kd <- mm$kaDoseDependence
  if (!is.null(kd)) {
    if (!(is.list(kd) && is_valid_number(kd$emax, 0, 1) && positive(kd$d50) &&
          (is.null(kd$hill) || positive(kd$hill))))
      stop("Invalid michaelisMenten for ", drug, ": kaDoseDependence needs an ",
           "emax in [0, 1], a positive d50 (mg) and optionally a positive hill.")
    if (!is.null(mm$kaTransit))
      stop("Invalid michaelisMenten for ", drug, ": kaDoseDependence is not ",
           "available with kaTransit.")
  }
  op <- mm$oralPaths
  if (!is.null(op)) {
    pathOk <- function(p) {
      sh <- p$share
      is.list(p) && positive(p$ka) &&
        (is.null(p$lag) || (is_valid_number(p$lag) && p$lag >= 0)) &&
        (is.null(p$zeroOrder) || positive(p$zeroOrder)) &&
        (is.null(p$into) || isTRUE(p$into %in% c("central", "metabolite"))) &&
        is.list(sh) && is_valid_number(sh$constant, 0, 1) &&
        (is.null(sh$logit) || (is.numeric(sh$logit) && length(sh$logit) == 2L &&
                                 all(is.finite(sh$logit)))) &&
        (is.null(sh$complement) || isTRUE(sh$complement) || isFALSE(sh$complement))
    }
    if (!is.list(op) || length(op) == 0 || !all(vapply(op, pathOk, logical(1))))
      stop("Invalid michaelisMenten for ", drug, ": each of oralPaths needs a ",
           "positive ka (/min), optionally a lag (min) of zero or more, a positive ",
           "zeroOrder (min) and into \"central\" or \"metabolite\", and a share ",
           "with a constant in [0, 1] and optionally a logit c(gamma, beta) and a ",
           "logical complement.")
    if (any(vapply(op, function(p) identical(p$into, "metabolite"), logical(1))) &&
        is.null(mm$metabolite))
      stop("Invalid michaelisMenten for ", drug, ": an oral path into the ",
           "metabolite needs the block's metabolite.")
    if (!is.null(mm$kaTransit) || !is.null(mm$kaDoseDependence) ||
        !is.null(mm$zeroOrderDepot))
      stop("Invalid michaelisMenten for ", drug, ": oralPaths are not available ",
           "with kaTransit, kaDoseDependence or zeroOrderDepot.")
  }
  if (!is.null(mm$kaTransit) && !positive(mm$kaTransit))
    stop("Invalid michaelisMenten for ", drug, ": kaTransit must be positive.")
  # The optional peripheral compartment: both rate constants or neither, since
  # one alone is a one-way compartment that silently swallows the dose.
  peripheral <- c("k23", "k32")
  given <- peripheral[vapply(peripheral, function(k) !is.null(mm[[k]]), logical(1))]
  if (length(given) == 1L)
    stop("Invalid michaelisMenten for ", drug, ": a peripheral compartment ",
         "needs both k23 and k32; only ", given, " was given.")
  for (k in given)
    if (!(is_valid_number(mm[[k]]) && mm[[k]] > 0))
      stop("Invalid michaelisMenten for ", drug, ": ", k, " must be positive.")
  mm
}

#' Unbound concentration at rapid binding equilibrium
#'
#' Solves total = Cu + B Cu / (kd + Cu) for Cu, the positive root of
#' Cu^2 + (B + kd - total) Cu - total kd = 0, in the form that does not
#' cancel when total is small.
#'
#' @param total total concentration (amount over volume)
#' @param B binding capacity as a concentration
#' @param kd dissociation constant
#' @returns the unbound concentration
#' @keywords internal
unboundAtEquilibrium <- function(total, B, kd)
{
  if (total <= 0) return(0)
  b <- B + kd - total
  root <- sqrt(b * b + 4 * total * kd)
  if (b > 0) 2 * total * kd / (b + root) else (root - b) / 2
}

#' Is a dose unit given in prodrug equivalents ("mg PE", fosphenytoin)?
#' @param units character vector of dose units
#' @returns logical vector
#' @keywords internal
isEquivalentUnit <- function(units)
{
  grepl(" PE( |/|$)", as.character(units))
}

#' The share of an oral dose an oral path takes
#'
#' \code{constant} x p, or x (1 - p) for the \code{complement}, where p is
#' \code{plogis(gamma - beta log(1 + D))} for a path with a \code{logit}
#' term (macitentan's FMAC, Bartolucci 2021) and 1 for one without.
#'
#' @param share the path's share block
#' @param doseMg the doses as given, in mg
#' @returns the share of each dose
#' @keywords internal
mmPathShare <- function(share, doseMg)
{
  out <- rep(share$constant, length(doseMg))
  if (!is.null(share$logit)) {
    p <- stats::plogis(share$logit[1] - share$logit[2] * log(1 + doseMg))
    out <- out * (if (isTRUE(share$complement)) 1 - p else p)
  }
  out
}

#' Simulate a one-compartment Michaelis-Menten drug
#'
#' See the header of R/advanceMichaelisMenten.R.
#'
#' @param dose the drug's dose table, in base units, with the route flags
#'   \code{simCpCe()} sets (\code{Bolus}, \code{PO}, \code{IM}, ...)
#' @param PK the drug's PK from \code{getDrugPK()}, with a
#'   \code{michaelisMenten} block
#' @param maximum end of the simulation, minutes
#' @param plotRecovery ignored: see the header
#' @param emerge ignored: see the header
#'
#' @returns a data frame of \code{Time}, \code{Cp}, \code{Ce} (NA) and
#'   \code{Recovery} (NA), as the closed-form engines return
#' @keywords internal
advanceMichaelisMenten <- function(dose, PK, maximum, plotRecovery = FALSE,
                                   emerge = 0)
{
  mm <- PK$michaelisMenten
  pkSet <- PK$PK[[PK_EVENT_DEFAULT]]
  V <- pkSet$v1
  vmax <- mm$vmax
  km <- mm$km
  clLinear <- if (is.null(mm$clLinear)) 0 else mm$clLinear
  q <- if (is.null(pkSet$cl2)) 0 else pkSet$cl2
  sc <- mm$saturableSC
  rb <- mm$rapidBinding
  tb <- mm$targetBinding
  sd <- mm$saturableDistribution
  # Rapid binding may plot the unbound central concentration (vincristine
  # in children), the root of the binding equilibrium
  plotUnbound <- !is.null(rb) && identical(rb$plotted, "unbound")
  # Unbound concentrations from the amounts (mmDerivative())
  V2 <- if (is.null(pkSet$v2)) 1 else pkSet$v2
  salt <- function(key) {
    s <- mm$saltFactor[[key]]
    if (is.null(s)) 1 else s
  }

  units <- as.character(dose$Units)
  pe <- isEquivalentUnit(units)
  rate <- isRateUnit(units)
  route <- doseRoute(units)
  formulation <- doseFormulation(units)

  # The oral depots: the default form, then each further formulation.
  forms <- c("default", names(PK$oralFormulations))
  absorption <- lapply(forms, function(f) {
    set <- if (f == "default") pkSet else PK$oralFormulations[[f]][[PK_EVENT_DEFAULT]]
    # A drug with no oral route (exenatide) has no oral depot to fill.
    orZero <- function(v, zero = 0) if (is.null(v)) zero else v
    list(ka = orZero(set$ka_PO), F = orZero(set$bioavailability_PO, 1),
         tlag = orZero(set$tlag_PO), salt = salt(f))
  })
  names(absorption) <- forms
  oralForm <- ifelse(!is.na(formulation) & formulation %in% forms, formulation,
                     "default")
  # An absorption rate constant that falls with the size of each oral dose
  # (suzetrigine, kaDoseDependence in the header): each distinct dose size of
  # the default form gets a depot of its own, at its own ka, so that doses
  # of different sizes absorbed at once do not share one rate.
  kd <- mm$kaDoseDependence
  if (!is.null(kd)) {
    plainOral <- route == ROUTE_PO & !rate & oralForm == "default" &
      as.numeric(dose$Dose) != 0
    if (any(plainOral)) {
      if (is.null(dose$DoseMg))
        stop("Internal error: ", PK$drug, "'s absorption depends on the dose, ",
             "and the dose in mg was not passed to the engine.")
      h <- if (is.null(kd$hill)) 1 else kd$hill
      d <- as.numeric(dose$DoseMg)[plainOral]
      fac <- 1 - kd$emax * d^h / (kd$d50^h + d^h)
      levels <- unique(fac)
      sized <- paste0("default@", seq_along(levels))
      for (k in seq_along(levels))
        absorption[[sized[k]]] <- utils::modifyList(
          absorption[["default"]], list(ka = absorption[["default"]]$ka * levels[k]))
      forms <- c(forms, sized)
      oralForm[plainOral] <- sized[match(fac, levels)]
    }
  }

  # Oral paths (macitentan, Bartolucci 2021; ORAL PATHS in the header): each
  # plain oral dose is split among parallel paths, each its own depot with
  # its own ka, lag and optional zero-order release, one of which may empty
  # into the metabolite.  The default depot is then unused.
  paths <- mm$oralPaths
  pathForms <- character(0)
  if (!is.null(paths)) {
    pathForms <- paste0("path", seq_along(paths))
    for (k in seq_along(paths))
      absorption[[pathForms[k]]] <- list(
        ka = paths[[k]]$ka, F = absorption[["default"]]$F,
        tlag = if (is.null(paths[[k]]$lag)) 0 else paths[[k]]$lag,
        salt = absorption[["default"]]$salt)
    forms <- c(forms, pathForms)
  }

  # States: the oral depots, the intramuscular depot of the prodrug, the
  # prodrug's conversion compartment, the central amount, the peripheral
  # amount and the saturable subcutaneous depot.
  nOral <- length(forms)
  iIM <- nOral + 1
  iConv <- nOral + 2
  iC <- nOral + 3
  iP <- nOral + 4
  iS <- nOral + 5
  iT <- nOral + 6      # transit compartment before the default oral depot
  iDRC <- nOral + 7    # drug-target complex, central (targetBinding)
  iDRP <- nOral + 8    # drug-target complex, peripheral
  iSD <- nOral + 9     # saturable distribution compartment (an amount)
  iE <- nOral + 10     # relative enzyme amount (autoinduction), starting at 1
  iMC <- nOral + 11    # the metabolite's central amount (METABOLITE)
  iMP <- nOral + 12    # and its peripheral amount
  ai <- mm$autoinduction
  met <- mm$metabolite
  kT <- if (is.null(mm$kaTransit)) 0 else mm$kaTransit
  # A peripheral compartment declared as rate constants on the amounts (GHB,
  # Chen 2020: k23 and k32; see the header) is the same linear distribution
  # as a PK set's cl2 and v2: q = k23 V and V2 = V k23 / k32.
  if (!is.null(mm$k23)) {
    if (q > 0)
      stop("Invalid michaelisMenten for ", PK$drug, ": a peripheral compartment ",
           "from both cl2 and k23/k32.")
    q <- mm$k23 * V
    V2 <- V * mm$k23 / mm$k32
  }
  ka <- c(vapply(absorption, function(a) a$ka, numeric(1)),
          if (is.null(mm$ka_IM)) 0 else mm$ka_IM)
  kc <- if (is.null(mm$kConversion)) 0 else mm$kConversion

  # Every input as a jump: state index, time it enters, amount.
  jumps <- data.frame(time = numeric(0), state = integer(0), amount = numeric(0))
  addJump <- function(use, time, state, amount) {
    if (!any(use)) return(invisible())
    jumps <<- rbind(jumps, data.frame(time = time[use], state = state[use],
                                      amount = amount[use]))
  }
  t0 <- as.numeric(dose$Time)
  D  <- as.numeric(dose$Dose)

  isOral <- route == ROUTE_PO & !rate
  # A zero-order release into the oral depot (zeroOrderDepot, suzetrigine's
  # tablet): each oral dose enters its depot at a constant rate over that
  # many minutes after its lag, rather than all at once.
  zod <- mm$zeroOrderDepot
  boxes <- data.frame(start = numeric(0), end = numeric(0), state = integer(0),
                      rate = numeric(0), amount = numeric(0))
  addBoxes <- function(use, start, duration, state, amount) {
    if (!any(use)) return(invisible())
    boxes <<- rbind(boxes, data.frame(start = start[use], end = start[use] + duration,
                                      state = state[use], rate = amount[use] / duration,
                                      amount = amount[use]))
  }
  if (any(isOral)) {
    j <- match(oralForm, forms)
    lag <- vapply(absorption, function(a) a$tlag, numeric(1))[j]
    Fo  <- vapply(absorption, function(a) a$F * a$salt, numeric(1))[j]
    viaPaths <- isOral & oralForm == "default" & length(pathForms) > 0
    direct <- isOral & !viaPaths
    if (is.null(zod)) {
      # With a transit compartment the default form's doses enter it first.
      addJump(direct, t0 + lag, ifelse(j == 1 & kT > 0, iT, j), D * Fo)
    } else {
      addBoxes(direct & D != 0, t0 + lag, zod, j, D * Fo)
    }
    # Each plain oral dose split among the oral paths, by shares that may
    # depend on its size in mg (mmPathShare())
    if (any(viaPaths)) {
      if (is.null(dose$DoseMg))
        stop("Internal error: ", PK$drug, "'s oral paths need the dose in mg.")
      dmg <- as.numeric(dose$DoseMg)
      for (k in seq_along(paths)) {
        jk <- match(pathForms[k], forms)
        amount <- D * Fo * mmPathShare(paths[[k]]$share, dmg)
        start <- t0 + absorption[[jk]]$tlag
        use <- viaPaths & amount != 0
        if (is.null(paths[[k]]$zeroOrder))
          addJump(use, start, rep(jk, length(t0)), amount)
        else
          addBoxes(use, start, paths[[k]]$zeroOrder, rep(jk, length(t0)), amount)
      }
    }
  }
  ivBolus <- route == ROUTE_IV & !rate & !pe
  addJump(ivBolus, t0, rep(iC, length(t0)), D * salt("IV"))
  peBolus <- route == ROUTE_IV & !rate & pe
  addJump(peBolus, t0, rep(iConv, length(t0)), D * salt("PE"))
  peIM <- route == ROUTE_IM & !rate & pe
  if (any(peIM)) {
    lagIM <- if (is.null(mm$tlag_IM)) 0 else mm$tlag_IM
    fIM <- if (is.null(mm$bioavailability_IM)) 1 else mm$bioavailability_IM
    addJump(peIM, t0 + lagIM, rep(iIM, length(t0)), D * fIM * salt("PE"))
  }
  # A subcutaneous dose: its zero-order share is a box of rate (scRateAt()
  # below), the rest arrives in the saturable depot when the box ends.
  isSC <- route == ROUTE_SC & !rate & !is.null(sc)
  if (any(isSC)) {
    tau <- sc$duration
    addJump(isSC, t0 + tau, rep(iS, length(t0)),
            D * salt("SC") * (1 - sc$bioavailability * (1 - sc$fraction)))
  }
  other <- !(isOral | ivBolus | peBolus | peIM | isSC | rate) |
    (rate & route != ROUTE_IV)
  if (any(other & D != 0))
    stop("The Michaelis-Menten engine cannot simulate units: ",
         paste(unique(units[other & D != 0]), collapse = ", "))

  # Two running rates, each set by its own rows until the next: the drug
  # intravenously (into the central compartment) and the prodrug (into the
  # conversion compartment).
  ivRate <- rate & !pe & route == ROUTE_IV
  peRate <- rate & pe & route == ROUTE_IV

  # Antiseizure interactions (R/antiseizureInteractions.R): piecewise-constant
  # multipliers of Vmax and Km, each segment starting at interactionMM$time.
  # Their switching times join the time line, so a step never straddles one.
  ddi <- PK$interactionMM
  # The multiplier in force at each time, for a vector of times at once: one
  # findInterval() over the time line rather than one per point, each of which
  # checked the whole segment vector for order and NAs (thousands of segments
  # over 52 weeks of interacting antiseizure drugs; Carbon's fourth UI audit,
  # 2026-11-15, C3).  The same segment, so the same doubles.
  ddiAt <- function(field, t) {
    if (is.null(ddi)) return(rep(1, length(t)))
    ddi[[field]][pmax(1L, findInterval(t, ddi$time))]
  }
  ddiMax <- function(field) if (is.null(ddi)) 1 else max(ddi[[field]])
  ddiMin <- function(field) if (is.null(ddi)) 1 else min(ddi[[field]])

  # A Vmax that declines with time since the first dose (voriconazole's
  # time-dependent autoinhibition, R/drugs_voriconazole.R): the fraction of
  # Vmax lost reaches half its maximum t50 minutes after the first dose, and
  # Vmax is the block's own value until `onset`.  Evaluated at the middle of
  # each integration step, which is at most MM_MAX_STEP minutes.
  #
  # The first dose is the first above 0, as the Bayesian update reads it
  # (bayesianUpdateDrugs()): a dose of 0 is no dose, and the startup menu
  # writes one at time 0 for every drug ticked.  Counted, it started the
  # decline from 0, so a first real dose a day later met a Vmax already
  # inhibited (Carbon's fourth UI audit, 2026-11-15, M6).
  vd <- mm$vmaxDecline
  given <- suppressWarnings(as.numeric(dose$Dose)) > 0 &
    !is.na(suppressWarnings(as.numeric(dose$Time)))
  given[is.na(given)] <- FALSE
  firstDose <- if (any(given)) min(as.numeric(dose$Time[given])) else 0
  vmaxDeclineAt <- function(t) {
    if (is.null(vd)) return(1)
    s <- max(t - firstDose - vd$onset, 0)
    1 - vd$fraction * s / (s + vd$t50 - vd$onset)
  }

  # The time line: every dose time and arrival, and the instant before each;
  # a zero-order release starts and ends at points of it.
  knots <- unique(c(t0, jumps$time, boxes$start, boxes$end))
  timeLine <- simulationTimeGrid(c(knots, knots - PRE_DOSE_OFFSET), maximum,
                                 gridStart(0))
  # The interaction multipliers' switches are points of the line, so that no
  # step straddles one, but not knots: a switch changes Vmax or Km, and so
  # the slope, never the concentration, so it needs neither the instant
  # before it, which shows a jump, nor the fine fill after it, which resolves
  # an absorption.  As knots they were most of the line: phenytoin among
  # seven interacting drugs over 52 weeks had 36,469 points for 2,565
  # switches, against 9,100 on its own, every one an integration step
  # (Carbon's fourth UI audit, 2026-11-15, C3).  The times at which the
  # interactions read this drug's concentration (`sample`, the multipliers'
  # grid) are points too, so that a perpetrator's concentration is read
  # where it was computed rather than between points: without them, read
  # off the sparser line, phenytoin's was up to 1e-3 of its peak out.
  if (!is.null(ddi)) {
    extra <- c(ddi$time, ddi$sample)
    timeLine <- sort(unique(c(timeLine, extra[extra > 0 & extra <= maximum])))
  }
  timeLine <- timeLine[timeLine >= 0]
  L <- length(timeLine)

  ddiVmaxLine <- ddiAt("vmax", timeLine)
  ddiKmLine <- ddiAt("km", timeLine)

  rateAt <- function(use, times) {
    if (!any(use)) return(rep(0, length(times)))
    o <- order(t0[use])
    tt <- t0[use][o]
    rr <- D[use][o]
    # The last row at or before each time; zero before the first.
    c(0, rr)[findInterval(times, tt) + 1]
  }
  ivRateLine <- rateAt(ivRate, timeLine) * salt("IV")
  peRateLine <- rateAt(peRate, timeLine) * salt("PE")
  # The subcutaneous boxes add: each runs over [t0, t0 + duration).  Every
  # box edge is on the time line (the depot arrivals are jumps), so the rate
  # at the start of a step holds through it.
  scRateLine <- rep(0, L)
  for (k in which(isSC & D != 0)) {
    inBox <- timeLine >= t0[k] & timeLine < t0[k] + sc$duration
    scRateLine[inBox] <- scRateLine[inBox] + D[k] * salt("SC") *
      sc$bioavailability * (1 - sc$fraction) / sc$duration
  }

  # The zero-order releases into the oral depots, one column per depot: each
  # box's start and end are points of the line, so the rate at the start of a
  # step holds through it.
  depotRateLine <- matrix(0, L, nOral)
  for (k in seq_len(nrow(boxes))) {
    inBox <- timeLine >= boxes$start[k] & timeLine < boxes$end[k]
    depotRateLine[inBox, boxes$state[k]] <- depotRateLine[inBox, boxes$state[k]] +
      boxes$rate[k]
  }
  # What a box has still to release, for the Depot series
  boxLeft <- function(t) {
    if (!nrow(boxes)) return(0)
    on <- boxes$start <= t & t < boxes$end
    sum(boxes$rate[on] * (boxes$end[on] - t))
  }

  jumpAt <- match(jumps$time, timeLine)
  # The jumps at each point, found once (a which() at every point cost time in
  # the product of the points and the doses: 6 of the 57 seconds of six
  # interacting antiseizure drugs over 52 weeks)
  jumpsAt <- split(seq_along(jumpAt), factor(jumpAt, levels = seq_along(timeLine)))
  if (anyNA(jumpAt[jumps$time <= maximum]))
    stop("Internal error: a dose time is missing from the time line.")

  # The derivative (mmDerivative()), built in a frame of its own
  nState <- mmStateCount(iDRP, iSD, iE, sd, ai, iMP, met)
  if (!is.null(met)) {
    met$iC <- iMC
    met$iP <- iMP
    # the depots of the oral paths that deliver the metabolite itself
    toMet <- vapply(paths, function(p) identical(p$into, "metabolite"), logical(1))
    met$oralIdx <- match(pathForms[toMet], forms)
  }
  deriv <- mmDerivative(V, V2, ka, kT, kc, q, clLinear, nOral, iIM, iConv, iC, iP,
                        iS, iT, iDRC, iDRP, rb, sc, tb, iSD, iE, sd, ai,
                        met, nState)

  x <- numeric(nState)
  if (!is.null(ai)) x[iE] <- 1   # the uninduced enzyme
  Cp <- numeric(L)
  # The peripheral concentration (on the same footing as Cp: the amount in the
  # compartment over its volume) and the amount still to be absorbed, in the
  # oral depots, the transit compartment before them, the intramuscular depot
  # of the prodrug and the saturable subcutaneous depot; see R/compartments.R.
  C2 <- numeric(L)
  depot <- numeric(L)
  # The metabolite's concentrations, in its own units (unitScale, from
  # getDrugPK())
  Cm <- C2m <- numeric(L)
  if (!is.null(met)) {
    mScale <- if (is.null(met$unitScale)) 1 else met$unitScale
    mQ <- if (is.null(met$q)) 0 else met$q
  }
  depotStates <- c(seq_len(iIM), iT, iS)
  # The adaptive step (mmDormandPrince()), for a drug whose block asks for
  # it.  Its absolute tolerance is on amounts, relative to the largest dose
  # or an hour of the largest rate.
  adaptive <- !isFALSE(mm$adaptiveStep)
  # The fixed step is held within the classical Runge-Kutta method's region
  # of stability for the system's fastest process (mmStateRates()).  At
  # MM_MAX_STEP alone, voriconazole's absorption (ka 100 /h, fixed in adults)
  # made each 2-minute step multiply what was left in the oral depot by 2.2:
  # a single 200 mg dose fell to zero two hours after it was given, and a
  # second dose reached 1e32 mg/L (found reproducing Carbon's second UI
  # audit, 2026-11-15; in the library since the drug was added).  The
  # absorption states (the depots, the transit and conversion compartments)
  # hold the step down only while they hold drug: what is left below
  # MM_NEGLIGIBLE_AMOUNT of the largest dose is taken as absorbed, so that a
  # fast absorption costs small steps for minutes after a dose, not for the
  # rest of the plot.
  absorbing <- unique(c(depotStates, iConv))
  if (!adaptive) {
    rates <- mmStateRates(function(y) deriv(y, 0, 0, vmax * ddiMax("vmax"),
                                            km * ddiMin("km")), nState)
    stableStep <- function(r) if (r > 0) min(MM_MAX_STEP, MM_STABLE_STEP / r) else MM_MAX_STEP
    stepCore <- stableStep(max(rates[-absorbing], 0))
    stepAll <- stableStep(max(rates))
  }
  # The tolerances' scale: the largest dose, or an hour of the largest rate,
  # given SO FAR.  Taken over the whole table, a dose not yet given set the
  # scale for the doses before it: 1e9 mg/kg at 1,400 minutes in a 500 kg
  # patient made the tolerance 460 mg, and a 300 mg dose at time 0 was
  # emptied as negligible, its curve 0 until the later dose (Carbon's third
  # UI audit, 2026-11-15, H5).
  pointScale <- 60 * pmax(abs(ivRateLine + scRateLine), abs(peRateLine))
  for (k in seq_len(nrow(boxes))) {
    at <- match(boxes$start[k], timeLine)
    if (!is.na(at))
      pointScale[at] <- max(pointScale[at], boxes$amount[k])
  }
  for (j in which(!is.na(jumpAt)))
    pointScale[jumpAt[j]] <- max(pointScale[jumpAt[j]], abs(jumps$amount[j]))
  scaleAt <- cummax(pointScale)
  if (adaptive) {
    hNext <- MM_MAX_STEP
    atol <- MM_ADAPTIVE_ATOL * .Machine$double.eps
    # An absorption state left with less than atol has been absorbed, within
    # the tolerance: emptied, it no longer holds the step to its own rate
    # (the noise below atol of a fast depot, voriconazole's, otherwise kept
    # the step at that rate's limit of stability for the rest of the plot).
    emptyDepots <- function(y) {
      y[absorbing][y[absorbing] <= atol] <- 0
      y
    }
  }
  for (i in seq_len(L)) {
    if (i > 1) {
      dt <- timeLine[i] - timeLine[i - 1]
      scale <- max(scaleAt[i - 1], .Machine$double.eps)
      if (!adaptive) {
        x[absorbing][abs(x[absorbing]) <= MM_NEGLIGIBLE_AMOUNT * scale] <- 0
        absorbingNow <- any(x[absorbing] != 0) || peRateLine[i - 1] != 0 ||
          any(depotRateLine[i - 1, ] != 0)
        n <- max(1L, ceiling(dt / (if (absorbingNow) stepAll else stepCore)))
      } else {
        n <- 1L
      }
      h <- dt / n
      rIV <- ivRateLine[i - 1] + scRateLine[i - 1]
      rPE <- peRateLine[i - 1]
      rD <- depotRateLine[i - 1, ]
      vmaxStep <- vmax * ddiVmaxLine[i - 1]
      kmStep <- km * ddiKmLine[i - 1]
      if (adaptive) {
        # An adaptive step for a drug that asks for one (mmDormandPrince())
        atol <- MM_ADAPTIVE_ATOL * scale
        f <- mmIntervalDerivative(deriv, rIV, rPE, vmaxStep, kmStep,
                                  if (!is.null(vd)) vmaxDeclineAt, rD)
        stepped <- mmDormandPrince(f, x, timeLine[i - 1], timeLine[i], hNext,
                                   MM_ADAPTIVE_RTOL, atol, emptyDepots)
        x <- stepped$x
        hNext <- stepped$h
      } else for (k in seq_len(n)) {
        vmaxK <- if (is.null(vd)) vmaxStep else
          vmaxStep * vmaxDeclineAt(timeLine[i - 1] + (k - 0.5) * h)
        k1 <- deriv(x, rIV, rPE, vmaxK, kmStep, rD)
        k2 <- deriv(x + h / 2 * k1, rIV, rPE, vmaxK, kmStep, rD)
        k3 <- deriv(x + h / 2 * k2, rIV, rPE, vmaxK, kmStep, rD)
        k4 <- deriv(x + h * k3, rIV, rPE, vmaxK, kmStep, rD)
        x <- x + h / 6 * (k1 + 2 * k2 + 2 * k3 + k4)
      }
      # The central amount cannot fall below zero; RK4 can overshoot by
      # rounding when the compartment is empty.
      x[x < 0] <- 0
    }
    arriving <- jumpsAt[[i]]
    for (j in arriving) x[jumps$state[j]] <- x[jumps$state[j]] + jumps$amount[j]
    # A dose starts fast processes again: the step starts small again
    if (adaptive && length(arriving)) hNext <- min(hNext, MM_MAX_STEP)
    Cp[i] <- if (plotUnbound)
      unboundAtEquilibrium(x[iC] / V, rb$bmaxCentral, rb$kd) else x[iC] / V
    if (q > 0) C2[i] <- x[iP] / V2
    depot[i] <- sum(x[depotStates]) + boxLeft(timeLine[i])
    if (!is.null(met)) {
      Cm[i] <- x[iMC] * mScale / met$v1
      if (mQ > 0) C2m[i] <- x[iMP] * mScale / met$v2
    }
  }

  out <- data.frame(
    Time = timeLine,
    Cp = Cp,
    Ce = rep(NA_real_, L),
    Recovery = rep(NA_real_, L),
    C2 = C2,
    C3 = rep(0, L),
    Depot = depot
  )
  # The metabolite's columns, which simCpCe() lifts into a series of its own
  # (as advanceTransitChain() returns enalaprilat's)
  if (!is.null(met)) {
    out$CpMetabolite <- Cm
    out$CeMetabolite <- rep(NA_real_, L)
    out$C2Metabolite <- C2m
    out$C3Metabolite <- rep(0, L)
  }
  out
}
