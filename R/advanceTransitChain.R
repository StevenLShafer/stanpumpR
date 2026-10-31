# -----------------------------------------------------------------------------
# Exact linear engine: transit-chain absorption, by matrix exponential
# -----------------------------------------------------------------------------
# Provenance
# ----------
# Drafted by Claude Code, 2026-10-20, at the request of Steven L. Shafer, for
# enalapril (R/drugs_enalapril.R; Faisal 2019), whose absorption is a chain of
# eight identical transit compartments and whose active metabolite,
# enalaprilat, is formed behind two more.  Verified by
# tests/testthat/test-transit-chain.R.
#
# WHY A SEPARATE ENGINE, AND WHY NOT A NUMERICAL ONE
# ==================================================
# This system is linear and superposable, so by the library's usual test
# (docs/antihypertensives.md section 6) it does not need a numerical solver at
# all.  What it needs is exact handling of a REPEATED eigenvalue.
#
# A chain of N identical compartments, each emptying at ktr, has the single
# eigenvalue -ktr repeated N times.  Every closed-form coefficient in this
# library divides by a difference of two rate constants, which is why
# separateEigenvalues() exists: it nudges ties apart by a relative 1e-8.  That
# is the right answer for an accidental tie between two unrelated constants.
# It is the wrong answer for a tie that is structural and N deep, because the
# partial-fraction coefficients then scale as 1 / nudge^(N-1) -- for N = 8,
# about 1e56 -- and the curve would be the difference of enormous nearly equal
# numbers.  That is precisely the catastrophic cancellation that
# docs/antihypertensives.md section 3.3 rejected esmolol's decomposition for,
# so the library's own standard forbids the nudge here.
#
# The exact solution of a linear system with a defective matrix is the matrix
# exponential, which has no such difficulty: the Jordan structure is never
# formed, and the t^k exp(-ktr t) terms the repeated eigenvalue produces come
# out of the series by themselves.  So this engine is exact in the same sense
# the closed-form engines are -- to rounding, not to a step size -- and
# unlike advanceMichaelisMenten() it is not an approximation of anything.
#
# THE STATES
# ==========
# In order, so that the indices below read as the picture:
#
#   1 .. n      the chain.  Compartments 1 to n - 1 empty at the block's ktr;
#               the LAST empties at the PK set's ka_PO, into the central
#               compartment.  With n = 1 that is an ordinary oral depot.
#   n + 1       the parent's central compartment, volume v1
#   then        the parent's peripheral compartments, where it has them
#   then, when the drug forms an active metabolite:
#   .. 1 .. m   the metabolite's formation transit compartments, every one of
#               them at the metabolite block's ktr
#   ..          the metabolite's central compartment, volume v
#   ..          the metabolite's peripheral compartment, where it has one
#
#   ..          the effect site, where the drug has a ke0 (a concentration,
#               not an amount: dCe/dt = ke0 (Cp - Ce))
#
# and two further states, each held at 1, which carry a rate: one into the
# central compartment (an intravenous infusion) and one into the head of the
# chain (an oral or enteral infusion, multiplied by bioavailability_PO); see
# below.  An oral dose enters chain compartment 1, multiplied by
# bioavailability_PO, at its time plus tlag_PO, and an oral rate takes the same
# lag.  An intravenous bolus enters the central compartment.
#
# The two chains differ in their last step, and that asymmetry is the source's
# rather than this engine's: Faisal 2019 names a separate rate constant of
# absorption out of the last transit of the parent's chain and names no such
# constant for the metabolite's, where the drug simply passes on at ktr.  So
# the parent's chain takes ka_PO for its last step and the metabolite's does
# not.  A model needing the other arrangement should say so explicitly rather
# than have one inferred from the number of compartments.
#
# THE PARENT'S ELIMINATION SPLITS
# ===============================
# The PK set's k10 is the parent's TOTAL elimination, as for any drug, so
# getDrugPK() and the help tables see an ordinary clearance.  The block's
# kMetabolic is the part of it that forms the metabolite rather than leaving
# the body; k10 - kMetabolic is the rest.  A drug whose whole elimination
# forms the metabolite declares kMetabolic equal to k10; one that forms none
# declares no metabolite at all.  validateTransitChain() refuses a kMetabolic
# above k10, which would be an elimination rate the clearance does not cover.
#
# There is deliberately no molecular-weight ratio between the two moieties.
# The flux into the metabolite is kMetabolic times the parent's AMOUNT, and
# the metabolite's volume was fitted against its own measured concentrations
# with that same flux, so whatever molar bookkeeping the source used is
# already inside the fitted volume.  Applying a ratio on top would count it
# twice.  What a parent and metabolite reported in DIFFERENT units do need is
# the unit scale, which getDrugPK() computes with metaboliteUnitScale(); see
# R/metaboliteCoefficients.R for why its absence would be wrong by a
# thousandfold in a plausible-looking direction.
#
# HOW THE ADVANCE IS DONE, AND WHY IT IS CHEAP
# ============================================
# The matrix is constant between rate changes, so over a step of length dt the
# exact advance is
#
#     x(t + dt) = E(dt) x(t) + R g(dt),      E = exp(A dt),
#
# with g(dt) the integral of exp(A s) over the step applied to a unit input,
# and R the rate holding over it -- one such pair for the intravenous rate
# into the central compartment and one for the oral rate into the head of the
# chain.  All of it comes from ONE matrix exponential: augmenting A with a
# state held at 1 for each that feeds its compartment at a unit rate makes E
# the leading block of exp(Ahat dt) and the g's its last two columns.  Because
# the system is linear in R, the same g serves every rate -- it is scaled, not
# recomputed -- so the propagator depends on dt alone and is memoised on it.
#
# That matters because the library's time line is not uniform
# (simulationTimeGrid()): it fills every gap between dose times with the same
# geometric offsets, so the distinct step lengths number in the dozens where
# the points number in the thousands.  A drug's whole simulation therefore
# costs a few dozen matrix exponentials of a matrix a dozen or so states
# wide.
#
# exp(A dt) is formed by scaling and squaring: A dt is halved until its
# largest entry is at most EXPM_SCALED_NORM, the Taylor series is summed
# there, and the result is squared back.  A has non-negative off-diagonal
# entries, so exp(A dt) has non-negative entries exactly; rounding can leave
# one a hair below zero, and those are clipped rather than carried into an
# amount.
#
# THE EFFECT SITE
# ===============
# A parent with a ke0 (the PK set's, as getDrugPK() resolved it) gets an
# effect-site state, which is one more linear equation and so exact like the
# rest; Ce is NA for a drug without one, and always for the metabolite.  Added
# 2026-10-30 so that the Parkinson disease drugs of PR #548 (levodopa, with
# Mao 2013's motor effect site, behind one transit compartment) can run here
# instead of on the 1e-5 two-depot approximation of R/transit.R.
#
# TIME UNTIL THRESHOLD, BY A ROOT-FIND ON THE PROPAGATOR
# ======================================================
# Built 2026-10-30 on Steven L. Shafer's decision (asked when R/transit.R was
# coalesced into this engine: levodopa has a threshold, endCe 0.81 mcg/mL,
# Mao 2013's EC50, and its time until threshold had to survive the move).
#
# The definition is the closed-form engines' (R/recoveryCalc.R): stop all
# delivery at time t -- every rate, intravenous or oral -- give nothing more,
# and find the LAST time the timed concentration comes down through the
# threshold within the horizon; the horizon itself if it is still above there,
# zero if it never rises above.  The effect site is timed where there is one,
# else the plasma, with the same horizons (a day; a week or the plot), and a
# dose given but still inside its lag reads NA, as pendingDoseTimes() makes it
# elsewhere.  Drug still in the chain is drug given, so it counts.
#
# The closed-form engines solve this from exponential amplitudes, which a
# defective system does not have.  What it does have is linearity: after
# delivery stops the timed concentration s minutes on is
#
#     y(s) = c' exp(A s) x(t) = r(s)' x(t),     r(s)' = c' exp(A s),
#
# and r(s) does not depend on t.  So r is computed ONCE, on a uniform grid of
# the horizon, by repeated multiplication by one propagator, and every plotted
# point's future curve on that grid is one matrix product, X R'.
#
# A grid alone would miss an excursion between two of its points (the defect
# finding F09 found in the old closed-form search), so the grid only screens,
# with a bound that is RIGOROUS.  A is a generator: its off-diagonal entries
# are non-negative, so with delta the largest |A_ii|, N = A + delta I is
# non-negative and exp(A h) = exp(-delta h) exp(N h).  Every entry of
# exp(N h) is convex in h (its second derivative is N^2 exp(N h) >= 0), so on
# an interval of width w it lies under its chord, and with c and x
# non-negative -- amounts and an effect-site concentration -- that gives, for
# u = h / w in [0, 1],
#
#     y(a + u w) <= exp(-beta u) ((1 - u) y(a) + u exp(beta) y(b)),
#     beta = delta w,
#
# whose maximum over u has a closed form (chordBound()).  It is tight to
# second order in beta, so near a crossing it closes in as fast as the
# interval shrinks.  An interval whose bound is at or below the threshold
# cannot contain a point above it.  The intervals left are searched from the
# right, each split in two by the same bound with the row's own covector
# r(a)' carried along by the memoised exp(A w / 2^k), until the last point
# above the threshold is pinned to RECOVERY_TOL (a crossing) or an interval
# with no point found above is narrower than RECOVERY_TOL_PEAK (an excursion,
# if any, smaller than the bound's slack there -- about delta^2 1e-18 of the
# threshold).  The same two tolerances as R/recoveryCalc.R.
#
# Cost.  The grid's step makes delta times it TRANSIT_RECOVERY_GRID_BETA (0.1),
# so the bound's slack between grid points is about beta^2 / 8 of the output,
# and the screen leaves each row one or two open intervals at its last
# crossing.  Rows whose crossing is clean -- at every split the right half
# either starts above the threshold or is cleared -- are bisected together,
# one matrix product per level for all of them; the rest take the full search.
# Measured 2026-10-30: two weeks of six-hourly oral doses (2016 points)
# 0.16 s, a three-day enteral infusion at ka 9.2 /h (645 points) 0.17 s,
# against about 0.2 s for the closed-form engines' own search on a run that
# size.  A coarser grid costs refinement, never accuracy.
#
# The metabolite's own time until threshold stays NA: its threshold is not
# this drug's, and foldMetabolites() already treats a missing set of
# metabolite recovery states as "not reported" rather than a partial answer.
# -----------------------------------------------------------------------------

#' Largest entry of a scaled matrix before the Taylor series is summed
#'
#' Scaling and squaring: `A` is halved until no entry exceeds this, which
#' keeps the series' terms falling fast enough that
#' `EXPM_TAYLOR_TERMS` reaches rounding.
#' @keywords internal
EXPM_SCALED_NORM <- 0.5

#' Terms of the Taylor series for the scaled matrix exponential
#'
#' At `EXPM_SCALED_NORM` the next term is below 0.5^18 / 18!, which is far
#' under double rounding.
#' @keywords internal
EXPM_TAYLOR_TERMS <- 18


#' Matrix exponential, by scaling and squaring a Taylor series
#'
#' Dependency-free, and accurate to rounding for the generator matrices this
#' engine builds (non-positive diagonal, non-negative off-diagonal).  It is
#' not offered as a general-purpose `expm()`: a matrix with a large departure
#' from normality needs a Pade approximant with a sharper error bound.
#'
#' @param M a square matrix
#' @returns `exp(M)`
#' @keywords internal
expmScaleSquare <- function(M)
{
  big <- max(abs(M))
  s <- if (big > EXPM_SCALED_NORM)
    as.integer(ceiling(log2(big / EXPM_SCALED_NORM))) else 0L
  A <- M / 2^s
  E <- term <- diag(nrow(M))
  for (k in seq_len(EXPM_TAYLOR_TERMS))
  {
    term <- term %*% A / k
    E <- E + term
  }
  for (i in seq_len(s)) E <- E %*% E
  E
}


#' Validate a model's transitChain block
#'
#' @param tc the block a drug model returned, or NULL
#' @param pkSet the drug's default PK set, for the elimination check
#' @param drug the drug's name, for the error message
#' @returns `tc`, unchanged, if valid
#' @keywords internal
validateTransitChain <- function(tc, pkSet, drug)
{
  if (is.null(tc)) return(NULL)
  whole <- function(v) is_valid_number(v) && v >= 0 && v == round(v)
  positive <- function(v) is_valid_number(v) && v > 0

  if (!is.list(tc) || !whole(tc$n) || tc$n < 1)
    stop("Invalid transitChain for ", drug, ": n must be a whole number of ",
         "chain compartments, one or more (the last is the depot).")
  if (tc$n > 1 && !positive(tc$ktr))
    stop("Invalid transitChain for ", drug, ": ktr must be positive (per ",
         "minute) when n is above one.")
  if (!positive(pkSet$ka_PO))
    stop("Invalid transitChain for ", drug, ": the chain's last compartment ",
         "empties at ka_PO, so the model must give a positive one.")

  kMet <- if (is.null(tc$kMetabolic)) 0 else tc$kMetabolic
  if (!(is_valid_number(kMet) && kMet >= 0))
    stop("Invalid transitChain for ", drug, ": kMetabolic must be zero or ",
         "positive (per minute).")
  # The total elimination the clearance pays for.  A rounding slack, because
  # k10 is cl1 / v1 and a model that means "all of it" writes the same
  # arithmetic twice.
  if (kMet > pkSet$k10 * (1 + 1e-9))
    stop("Invalid transitChain for ", drug, ": kMetabolic (", signif(kMet, 6),
         "/min) exceeds the parent's total elimination rate constant k10 (",
         signif(pkSet$k10, 6), "/min), so the clearance does not cover it.")

  met <- tc$metabolite
  if (!is.null(met))
  {
    if (!is.list(met) || !is.character(met$name) || !nzchar(met$name))
      stop("Invalid transitChain for ", drug,
           ": metabolite needs the name of the drug that receives it.")
    if (!whole(met$n)) stop("Invalid transitChain for ", drug,
                            ": metabolite n must be a whole number.")
    # Every compartment of the metabolite's chain empties at ktr, its last
    # included, so one is needed as soon as there is a compartment at all.
    if (met$n > 0 && !positive(met$ktr))
      stop("Invalid transitChain for ", drug, ": metabolite ktr must be ",
           "positive when its n is above zero.")
    if (!positive(met$v) || !positive(met$kel))
      stop("Invalid transitChain for ", drug,
           ": metabolite needs a positive v (L) and kel (per minute).")
    k12 <- if (is.null(met$k12)) 0 else met$k12
    k21 <- if (is.null(met$k21)) 0 else met$k21
    if (!(is_valid_number(k12) && k12 >= 0 && is_valid_number(k21) && k21 >= 0))
      stop("Invalid transitChain for ", drug,
           ": metabolite k12 and k21 must be zero or positive.")
    if ((k12 > 0) != (k21 > 0))
      stop("Invalid transitChain for ", drug, ": a metabolite peripheral ",
           "compartment needs both k12 and k21, or neither.")
    if (kMet <= 0)
      stop("Invalid transitChain for ", drug, ": a metabolite is declared but ",
           "kMetabolic is zero, so none would ever be formed.")
  }
  tc
}


#' Advance a transit-chain drug, exactly, by matrix exponential
#'
#' @param dose the classified dose table `simCpCe()` builds, in the engine's
#'   amount unit
#' @param PK the whole resolved PK, for `PK$transitChain` and the default set
#' @param maximum the end of the plot, minutes
#' @param plotRecovery should the time until threshold be computed?  Zeros
#'   otherwise, as the closed-form engines give
#' @param emerge the threshold: on the effect site, or on the plasma for a
#'   drug without one
#' @returns a data frame of `Time`, `Cp`, `Ce` (NA without a ke0),
#'   `Recovery` (minutes; NA while a dose is inside its lag), `C2`,
#'   `C3` and `Depot`, and, for a drug that forms an active metabolite,
#'   `CpMetabolite`, `CeMetabolite` (NA), `C2Metabolite` and `C3Metabolite`,
#'   as the closed-form engines return
#' @keywords internal
advanceTransitChain <- function(dose, PK, maximum, plotRecovery = FALSE,
                                emerge = 0)
{
  tc <- PK$transitChain
  pkSet <- PK$PK[[PK_EVENT_DEFAULT]]
  met <- tc$metabolite

  orZero <- function(v, zero = 0) if (is.null(v) || is.na(v)) zero else v
  n    <- as.integer(tc$n)
  ktr  <- orZero(tc$ktr)
  ka   <- pkSet$ka_PO
  V    <- pkSet$v1
  kMet <- orZero(tc$kMetabolic)
  # The parent's loss that does NOT form the metabolite.  max() only against
  # the rounding slack validateTransitChain() allows.
  kOut <- max(0, pkSet$k10 - kMet)
  k12  <- orZero(pkSet$k12); k21 <- orZero(pkSet$k21)
  k13  <- orZero(pkSet$k13); k31 <- orZero(pkSet$k31)
  V2   <- orZero(pkSet$v2, 1); V3 <- orZero(pkSet$v3, 1)

  # ---- State layout ----
  # Only the compartments the drug actually has, so that the matrix is the
  # system and nothing more: a state nothing enters or leaves would stay zero
  # and cost an exponential anyway.
  iC <- n + 1L
  i2 <- if (k12 > 0) iC + 1L else NA_integer_
  i3 <- if (k13 > 0) iC + (k12 > 0) + 1L else NA_integer_
  nParent <- iC + (k12 > 0) + (k13 > 0)
  m <- if (is.null(met)) 0L else as.integer(met$n)
  iM1  <- nParent + 1L            # the first formation transit, if any
  iMC  <- nParent + m + 1L
  iMP  <- iMC + 1L
  nS   <- if (is.null(met)) nParent else
    if (orZero(met$k12) > 0) iMP else iMC
  ke0  <- orZero(pkSet$ke0)
  iE   <- if (ke0 > 0) nS + 1L else NA_integer_
  if (ke0 > 0) nS <- nS + 1L
  iOne <- nS + 1L                 # held at 1; carries the intravenous rate
  iOneOral <- nS + 2L             # held at 1; carries the oral rate

  mKel <- if (is.null(met)) 0 else met$kel
  mK12 <- if (is.null(met)) 0 else orZero(met$k12)
  mK21 <- if (is.null(met)) 0 else orZero(met$k21)
  mKtr <- if (is.null(met)) 0 else orZero(met$ktr)
  mV   <- if (is.null(met)) 1 else met$v
  # The metabolite's peripheral volume is implied by its rate constants, the
  # same relation advanceMichaelisMenten() uses for GHB's k23/k32.
  mVP  <- if (mK21 > 0) mV * mK12 / mK21 else 1
  mScale <- if (is.null(met)) 1 else orZero(met$unitScale, 1)

  # ---- The system matrix ----
  A <- matrix(0, iOneOral, iOneOral)
  # The chain: each compartment empties into the next, the last into the
  # central one.  All but the last at ktr; the last at ka_PO.
  chain <- c(rep(ktr, max(0L, n - 1L)), ka)
  for (j in seq_len(n))
  {
    A[j, j] <- -chain[j]
    A[j + 1L, j] <- chain[j]      # the last chain state feeds the central one
  }
  A[iC, iC] <- -(kOut + kMet + k12 + k13)
  if (k12 > 0) { A[iC, i2] <- k21; A[i2, iC] <- k12; A[i2, i2] <- -k21 }
  if (k13 > 0) { A[iC, i3] <- k31; A[i3, iC] <- k13; A[i3, i3] <- -k31 }
  # A unit intravenous rate into the central compartment, scaled by the rate
  # in force when the propagator is applied.
  A[iC, iOne] <- 1
  # And a unit oral rate into the head of the chain, bioavailability included.
  A[1L, iOneOral] <- orZero(pkSet$bioavailability_PO, 1)
  if (ke0 > 0) { A[iE, iC] <- ke0 / V; A[iE, iE] <- -ke0 }
  if (!is.null(met))
  {
    # The parent's metabolic loss enters the formation chain, or the
    # metabolite's central compartment directly when there is no chain.
    A[if (m > 0) iM1 else iMC, iC] <- kMet
    for (j in seq_len(m))
    {
      A[nParent + j, nParent + j] <- -mKtr
      A[nParent + j + 1L, nParent + j] <- mKtr
    }
    A[iMC, iMC] <- -(mKel + mK12)
    if (mK12 > 0)
    {
      A[iMC, iMP] <- mK21; A[iMP, iMC] <- mK12; A[iMP, iMP] <- -mK21
    }
  }

  # ---- The inputs ----
  units <- as.character(dose$Units)
  isRate <- isRateUnit(units)
  route <- doseRoute(units)
  t0 <- as.numeric(dose$Time)
  D  <- as.numeric(dose$Dose)

  jumps <- data.frame(time = numeric(0), state = integer(0), amount = numeric(0))
  addJump <- function(use, time, state, amount) {
    if (!any(use)) return(invisible())
    jumps <<- rbind(jumps, data.frame(time = time[use], state = state[use],
                                      amount = amount[use]))
  }
  isOral <- route == ROUTE_PO & !isRate
  addJump(isOral, t0 + orZero(pkSet$tlag_PO), rep(1L, length(t0)),
          D * orZero(pkSet$bioavailability_PO, 1))
  ivBolus <- route == ROUTE_IV & !isRate
  addJump(ivBolus, t0, rep(iC, length(t0)), D)
  ivRate <- route == ROUTE_IV & isRate
  # An oral or enteral infusion (levodopa intestinal gel) runs through the
  # whole chain, lagged as an oral dose is.
  oralRate <- route == ROUTE_PO & isRate
  tOralRate <- t0 + orZero(pkSet$tlag_PO)

  other <- !(isOral | ivBolus | ivRate | oralRate)
  if (any(other & D != 0))
    stop("The transit-chain engine cannot simulate units: ",
         paste(unique(units[other & D != 0]), collapse = ", "))

  # ---- The time line ----
  # Every dose time and every arrival, and the instant before each, so that a
  # jump never lands inside a step.  Plasma only, so the grid starts at 0.
  knots <- unique(c(t0, jumps$time, tOralRate[oralRate]))
  timeLine <- simulationTimeGrid(c(knots, knots - PRE_DOSE_OFFSET), maximum,
                                 gridStart(0))
  timeLine <- timeLine[timeLine >= 0]
  L <- length(timeLine)

  # The intravenous rate in force at each point: the last such row at or
  # before it, zero before the first.
  rateIn <- function(use, at) {
    if (!any(use)) return(rep(0, L))
    o <- order(at[use])
    c(0, D[use][o])[findInterval(timeLine, at[use][o]) + 1]
  }
  rateLine <- rateIn(ivRate, t0)
  oralRateLine <- rateIn(oralRate, tOralRate)
  if (any(oralRate & tOralRate <= maximum & is.na(match(tOralRate, timeLine))))
    stop("Internal error: an oral rate change is missing from the time line.")

  jumpAt <- match(jumps$time, timeLine)
  if (anyNA(jumpAt[jumps$time <= maximum]))
    stop("Internal error: a dose time is missing from the time line.")

  # ---- The advance ----
  # One propagator per distinct step length, memoised: E is the leading block
  # of exp(Ahat dt) and g its last column, the response to a unit rate.
  cache <- list()
  propagator <- function(dt) {
    key <- sprintf("%.14e", dt)
    got <- cache[[key]]
    if (is.null(got))
    {
      E <- expmScaleSquare(A * dt)
      E[E < 0] <- 0     # exact for a generator; rounding can dip below zero
      got <- list(E = E[seq_len(nS), seq_len(nS), drop = FALSE],
                  g = E[seq_len(nS), iOne], gOral = E[seq_len(nS), iOneOral])
      cache[[key]] <<- got
    }
    got
  }

  x <- numeric(nS)
  X <- matrix(0, L, nS)     # every point's state, for the time until threshold
  Cp <- C2 <- C3 <- depot <- numeric(L)
  Cm <- C2m <- numeric(L)
  Ce <- rep(NA_real_, L)
  # The amount still to be absorbed is the whole chain; the metabolite's
  # formation transits are not a depot -- that drug is already absorbed.
  depotStates <- seq_len(n)
  for (i in seq_len(L))
  {
    if (i > 1)
    {
      p <- propagator(timeLine[i] - timeLine[i - 1])
      x <- as.vector(p$E %*% x) + p$g * rateLine[i - 1] +
        p$gOral * oralRateLine[i - 1]
    }
    for (j in which(jumpAt == i)) x[jumps$state[j]] <- x[jumps$state[j]] + jumps$amount[j]
    X[i, ] <- x
    Cp[i] <- x[iC] / V
    if (k12 > 0) C2[i] <- x[i2] / V2
    if (k13 > 0) C3[i] <- x[i3] / V3
    if (ke0 > 0) Ce[i] <- x[iE]
    depot[i] <- sum(x[depotStates])
    if (!is.null(met))
    {
      Cm[i] <- x[iMC] * mScale / mV
      if (mK12 > 0) C2m[i] <- x[iMP] * mScale / mVP
    }
  }

  # ---- Time until threshold ----
  # Timed on the effect site where there is one, else on the plasma; see the
  # header.  Everything after delivery stops is the homogeneous system, the
  # leading block of A without the two unit-rate states.
  recovery <- rep(0, L)
  if (plotRecovery)
  {
    cvec <- numeric(nS)
    if (ke0 > 0) cvec[iE] <- 1 else cvec[iC] <- 1 / V
    horizon <- if (ke0 > 0) RECOVERY_HORIZON_EFFECT else
      recoveryHorizonPlasma(maximum)
    recovery <- transitChainRecovery(X, A[seq_len(nS), seq_len(nS), drop = FALSE],
                                     cvec, emerge, horizon)
    oralIn <- isOral | oralRate
    pending <- pendingDoseTimes(t0[oralIn], t0[oralIn] + orZero(pkSet$tlag_PO),
                                D[oralIn], timeLine)
    if (!is.null(pending)) recovery[pending] <- NA_real_
  }

  out <- data.frame(
    Time = timeLine,
    Cp = Cp,
    Ce = Ce,
    Recovery = recovery,
    C2 = C2,
    C3 = C3,
    Depot = depot
  )
  if (!is.null(met))
  {
    out$CpMetabolite <- Cm
    out$CeMetabolite <- rep(NA_real_, L)
    out$C2Metabolite <- C2m
    out$C3Metabolite <- rep(0, L)
  }
  out
}


#' Steps of the uniform screening grid, at most
#'
#' The grid's step is set so that `delta` times it is
#' `TRANSIT_RECOVERY_GRID_BETA`, but never finer than the horizon over this
#' many steps; a coarser grid costs more refinement, never accuracy.
#' @keywords internal
TRANSIT_RECOVERY_MAX_STEPS <- 20000L

#' The screening grid's step, as a multiple of the fastest rate's time constant
#' @keywords internal
TRANSIT_RECOVERY_GRID_BETA <- 0.1

#' Rows of the screening product computed at once, as a number of cells
#' @keywords internal
TRANSIT_RECOVERY_CHUNK_CELLS <- 2e6


#' A rigorous upper bound on a generator system's output over an interval
#'
#' For `y(a + u w) = c' exp(A u w) z`, with `A` a generator, `c` and `z`
#' non-negative and `beta = delta w` (`delta` the largest |A_ii|), the
#' chord bound on the convex `exp(N h)` gives
#' `y <= exp(-beta u) ((1 - u) ya + u exp(beta) yb)`; this is its maximum over
#' `u` in `[0, 1]`.  See the header of R/advanceTransitChain.R.
#'
#' @param ya,yb the output at the two ends (vectors or matrices alike)
#' @param beta delta times the width
#' @returns the bound, the shape of `ya`
#' @keywords internal
chordBound <- function(ya, yb, beta)
{
  q <- exp(beta) * yb - ya
  out <- pmax(ya, yb)
  # The interior stationary point, a maximum where q > 0: u* = 1/beta - ya/q,
  # at which ya + q u* = q / beta.
  u <- 1 / beta - ya / q
  inside <- q > 0 & u > 0 & u < 1
  inside[is.na(inside)] <- FALSE
  out[inside] <- pmax(out[inside], exp(-beta * u[inside]) * q[inside] / beta)
  out
}


#' chordBound() for one interval, without the vector overhead
#' @param ya,yb the two endpoint values of one interval, each a single number
#'   (the function branches on `ya > yb`, so vectors are an error)
#' @inheritParams chordBound
#' @keywords internal
chordBoundScalar <- function(ya, yb, beta)
{
  q <- exp(beta) * yb - ya
  out <- if (ya > yb) ya else yb
  if (q > 0)
  {
    u <- 1 / beta - ya / q
    if (u > 0 && u < 1)
    {
      v <- exp(-beta * u) * q / beta
      if (v > out) out <- v
    }
  }
  out
}


#' Time until threshold for a transit-chain drug, exactly
#'
#' @param X the state at every point of the time line, one row each
#' @param A the homogeneous system matrix, a generator
#' @param cvec the timed output: `y = cvec' x`
#' @param target the threshold
#' @param horizon how far ahead to look, minutes
#' @returns minutes, one per row of `X`: zeros when there is no threshold or
#'   the output never rises above it, the horizon when it is still above it
#'   there
#' @keywords internal
transitChainRecovery <- function(X, A, cvec, target, horizon)
{
  L <- nrow(X)
  if (is.null(target) || length(target) != 1 || is.na(target) || target <= 0)
    return(rep(0, L))
  X[X < 0] <- 0               # rounding only; the bound needs x >= 0
  delta <- max(-diag(A))
  nF <- as.integer(min(TRANSIT_RECOVERY_MAX_STEPS,
                       max(1, ceiling(horizon * delta / TRANSIT_RECOVERY_GRID_BETA))))
  dt <- horizon / nF
  beta <- delta * dt

  # E(dt / 2^k), memoised by k: the grid step and every bisection of it.
  halves <- list()
  stepE <- function(k) {
    got <- if (k + 1 <= length(halves)) halves[[k + 1]]
    if (is.null(got))
    {
      got <- expmScaleSquare(A * (dt / 2^k))
      got[got < 0] <- 0
      halves[[k + 1]] <<- got
    }
    got
  }

  # r(s)' = c' exp(A s) on the grid, one row per grid point.
  R <- matrix(0, nF + 1L, length(cvec))
  r <- cvec
  E1 <- stepE(0)
  for (k in seq_len(nF + 1L)) { R[k, ] <- r; r <- as.vector(r %*% E1) }

  # The last point above the threshold in one grid interval, to tolerance, or
  # NULL if none is found: a right-first search in which a piece is kept only
  # while its chord bound exceeds the threshold.  Each piece carries the
  # covectors r' at both its ends and the outputs there, so splitting one costs
  # a single vector-matrix product.  The stack is preallocated: a depth-first
  # search holds at most two pieces per level.
  maxLevel <- ceiling(log2(dt / RECOVERY_TOL_PEAK)) + 2L
  size <- 2L * maxLevel + 2L
  nS <- length(cvec)
  refine <- function(k, x)
  {
    sOff <- numeric(size); sLev <- integer(size)
    sYl <- numeric(size); sYr <- numeric(size)
    sRl <- matrix(0, size, nS); sRr <- matrix(0, size, nS)
    top <- 1L
    sRl[1, ] <- R[k, ];     sYl[1] <- sum(R[k, ] * x)
    sRr[1, ] <- R[k + 1, ]; sYr[1] <- sum(R[k + 1, ] * x)
    while (top > 0L)
    {
      off <- sOff[top]; lev <- sLev[top]; yl <- sYl[top]; yr <- sYr[top]
      rl <- sRl[top, ]; rr <- sRr[top, ]
      top <- top - 1L
      w <- dt / 2^lev
      if (chordBoundScalar(yl, yr, delta * w) <= target) next
      confirmed <- yl > target
      if (confirmed && w <= RECOVERY_TOL) return((k - 1) * dt + off + w / 2)
      if (!confirmed && w <= RECOVERY_TOL_PEAK) next
      rm <- as.vector(rl %*% stepE(lev + 1L))
      ym <- sum(rm * x)
      # Left half pushed first, so the right half is searched first.
      top <- top + 1L
      sOff[top] <- off; sLev[top] <- lev + 1L
      sRl[top, ] <- rl; sYl[top] <- yl; sRr[top, ] <- rm; sYr[top] <- ym
      top <- top + 1L
      sOff[top] <- off + w / 2; sLev[top] <- lev + 1L
      sRl[top, ] <- rm; sYl[top] <- ym; sRr[top, ] <- rr; sYr[top] <- yr
    }
    NULL
  }

  # Most rows have a clean crossing: the last interval the screen leaves open
  # starts above the threshold, and at every split the right half either
  # starts above it too or is cleared by its bound.  Those rows are bisected
  # together, one matrix product per level for all of them; a row where a
  # right half is neither (it may hide a brief excursion) drops out to the
  # full search, refine(), which also takes every row whose last open interval
  # does not start above the threshold.
  lockstep <- function(rows, k, Xs)
  {
    m <- length(rows)
    res <- rep(NA_real_, m)
    Rl <- R[k, , drop = FALSE];     yl <- rowSums(Rl * Xs)
    Rr <- R[k + 1, , drop = FALSE]; yr <- rowSums(Rr * Xs)
    off <- numeric(m)
    live <- seq_len(m)
    lev <- 0L
    while (length(live))
    {
      w <- dt / 2^lev
      if (w <= RECOVERY_TOL)
      {
        res[live] <- (k[live] - 1) * dt + off[live] + w / 2
        break
      }
      Rm <- Rl[live, , drop = FALSE] %*% stepE(lev + 1L)
      ym <- rowSums(Rm * Xs[live, , drop = FALSE])
      right <- ym > target
      left <- !right & chordBound(ym, yr[live], delta * w / 2) <= target
      goR <- live[right]; goL <- live[left]
      Rl[goR, ] <- Rm[right, ]; yl[goR] <- ym[right]
      off[goR] <- off[goR] + w / 2
      Rr[goL, ] <- Rm[left, ]; yr[goL] <- ym[left]
      live <- live[right | left]     # the rest fall back to refine()
      lev <- lev + 1L
    }
    res
  }

  out <- numeric(L)
  chunk <- max(1L, floor(TRANSIT_RECOVERY_CHUNK_CELLS / (nF + 1)))
  for (rows in split(seq_len(L), (seq_len(L) - 1L) %/% chunk))
  {
    Y <- X[rows, , drop = FALSE] %*% t(R)
    above <- Y[, nF + 1L] > target
    out[rows[above]] <- horizon
    # Each row's last open interval.  The chord bound is at most
    # max(ya, exp(beta) yb), so no interval after the last grid point above
    # threshold / exp(beta) can be open: the search starts at that point and
    # walks left, with the exact bound, only until an interval stays open.
    nR <- length(rows)
    hi <- Y > target * exp(-beta)
    lastHi <- max.col(hi, ties.method = "last")
    lastHi[!hi[cbind(seq_len(nR), lastHi)]] <- 0L
    last <- integer(nR)
    for (j in which(!above & lastHi > 0L))
    {
      for (k in min(lastHi[j], nF):1)
      {
        if (chordBoundScalar(Y[j, k], Y[j, k + 1L], beta) > target) { last[j] <- k; break }
      }
    }
    has <- last > 0L
    clean <- has
    clean[has] <- Y[cbind(which(has), last[has])] > target
    done <- rep(FALSE, length(rows))
    if (any(clean))
    {
      j <- which(clean)
      got <- lockstep(rows[j], last[j], X[rows[j], , drop = FALSE])
      out[rows[j]] <- ifelse(is.na(got), 0, got)
      done[j] <- !is.na(got)
    }
    for (j in which(has & !done))
    {
      i <- rows[j]
      out[i] <- 0
      for (k in last[j]:1)
      {
        if (chordBoundScalar(Y[j, k], Y[j, k + 1L], beta) <= target) next
        t <- refine(k, X[i, ])
        if (!is.null(t)) { out[i] <- t; break }
      }
    }
  }
  out
}
