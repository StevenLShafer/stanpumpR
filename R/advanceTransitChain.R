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
# and one further state, held at 1, which carries an intravenous rate; see
# below.  An oral dose enters chain compartment 1, multiplied by
# bioavailability_PO, at its time plus tlag_PO.  An intravenous bolus enters
# the central compartment.
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
# with g(dt) the integral of exp(A s) over the step applied to a unit input
# into the central compartment, and R the intravenous rate holding over it.
# Both come from ONE matrix exponential: augmenting A with a state held at 1
# that feeds the central compartment at a unit rate makes E the leading block
# of exp(Ahat dt) and g its last column.  Because the system is linear in R,
# the same g serves every rate -- it is scaled, not recomputed -- so the
# propagator depends on dt alone and is memoised on it.
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
# NO EFFECT SITE, NO TIME UNTIL THRESHOLD
# =======================================
# Ce is NA and the Recovery column is NA, for both moieties, as for any drug
# the library plots as plasma only -- which enalapril and enalaprilat are, as
# is every antihypertensive here.  The reason it is not merely "not yet" is
# structural: a time until threshold is solved from the exponential amplitudes
# the closed-form engines leave in hand (R/recoveryStates.R), and a defective
# system has no such amplitudes to leave.  foldMetabolites() is written for
# that case already -- a NULL set of metabolite recovery states makes
# foldedRecovery() return NULL rather than report a time for part of the
# picture.
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
#' @param plotRecovery unused: this engine reports no time until threshold
#' @param emerge unused, for the same reason
#' @returns a data frame of `Time`, `Cp`, `Ce` (NA), `Recovery` (NA), `C2`,
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
  iOne <- nS + 1L                 # held at 1; carries the intravenous rate

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
  A <- matrix(0, iOne, iOne)
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

  other <- !(isOral | ivBolus | ivRate)
  if (any(other & D != 0))
    stop("The transit-chain engine cannot simulate units: ",
         paste(unique(units[other & D != 0]), collapse = ", "))

  # ---- The time line ----
  # Every dose time and every arrival, and the instant before each, so that a
  # jump never lands inside a step.  Plasma only, so the grid starts at 0.
  knots <- unique(c(t0, jumps$time))
  timeLine <- simulationTimeGrid(c(knots, knots - PRE_DOSE_OFFSET), maximum,
                                 gridStart(0))
  timeLine <- timeLine[timeLine >= 0]
  L <- length(timeLine)

  # The intravenous rate in force at each point: the last such row at or
  # before it, zero before the first.
  rateLine <- rep(0, L)
  if (any(ivRate))
  {
    o <- order(t0[ivRate])
    rateLine <- c(0, D[ivRate][o])[findInterval(timeLine, t0[ivRate][o]) + 1]
  }

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
                  g = E[seq_len(nS), iOne])
      cache[[key]] <<- got
    }
    got
  }

  x <- numeric(nS)
  Cp <- C2 <- C3 <- depot <- numeric(L)
  Cm <- C2m <- numeric(L)
  # The amount still to be absorbed is the whole chain; the metabolite's
  # formation transits are not a depot -- that drug is already absorbed.
  depotStates <- seq_len(n)
  for (i in seq_len(L))
  {
    if (i > 1)
    {
      p <- propagator(timeLine[i] - timeLine[i - 1])
      x <- as.vector(p$E %*% x) + p$g * rateLine[i - 1]
    }
    for (j in which(jumpAt == i)) x[jumps$state[j]] <- x[jumps$state[j]] + jumps$amount[j]
    Cp[i] <- x[iC] / V
    if (k12 > 0) C2[i] <- x[i2] / V2
    if (k13 > 0) C3[i] <- x[i3] / V3
    depot[i] <- sum(x[depotStates])
    if (!is.null(met))
    {
      Cm[i] <- x[iMC] * mScale / mV
      if (mK12 > 0) C2m[i] <- x[iMP] * mScale / mVP
    }
  }

  out <- data.frame(
    Time = timeLine,
    Cp = Cp,
    Ce = rep(NA_real_, L),
    Recovery = rep(NA_real_, L),
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
