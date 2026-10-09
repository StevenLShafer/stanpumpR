# Time until a concentration falls to a target, once delivery stops.
#
# The concentration after delivery stops is a sum of exponentials,
#
#     C(t) = sum(state * exp(-lambda * t)),
#
# and this returns the time at which it comes down through `target` for the
# LAST time, searched over the next 24 hours (or `horizon`).  Zero means it is
# at or below the target now and stays there; the horizon itself means it is
# still above the target at the end of the search -- which includes a target
# of zero, since a decaying exponential never gets there.
#
# The amplitudes need not all be positive.  The effect site lags the plasma, so
# after a bolus its ke0 term is negative and C(t) RISES before it falls.  That
# is why the last downward crossing is the right one: straight after a bolus the
# effect site may still be below the target, but it is about to go above it, and
# "time until threshold" must be the time until it comes back down.
#
# Rewritten by Claude Code (Claude Fable 5.1), 2026-10-05, at the request of
# Steven L. Shafer; verified on R 4.6.1 by tests/testthat/test-recoveryCalc.R
# and, against stopping delivery in the simulation itself, by
# tests/testthat/test-recovery-engines.R.  The earlier version minimised
# (target - C(t))^2 with optimize(), which had two defects:
#
#   1. It returned 0 whenever C(0) was at or below the target, even when C was
#      still rising towards a peak above it -- so in the first minute or two
#      after every bolus, and for the whole absorption phase of an oral dose, it
#      reported no time at all.
#   2. Its error surface is flat wherever C has decayed to nothing, which is
#      most of a 24-hour search for a short-acting drug, and a golden-section
#      search on a flat surface can settle on the far end of the interval
#      instead of the crossing.
#
# EVERY CROSSING, NOT A SAMPLE OF THEM
# ====================================
# From 2026-10-05 to 2026-10-09 the search bracketed the crossing on a fixed
# grid (3 seconds, then about 12% longer a step) and refined it with uniroot().
# A rise above the target that began and ended between two grid points was
# missed, and an external audit found one (finding F09): 16.9985840174 mg of
# propofol in a 70 kg, 170 cm, 40-year-old man takes the effect site to a peak
# of 1.0000050 mcg/mL at 1.60 minutes, above a 1 mcg/mL threshold for 0.62
# seconds and below it at every grid point, so the time read zero where the
# effect site in fact comes down through the threshold at 1.6052 minutes.
#
# The search now finds every crossing, however brief the excursion.  C(t) -
# target is itself a sum of exponentials, the target being a term with rate
# zero, and such a sum
#
#     h(t) = sum(a_k exp(-mu_k t)),   mu_1 < mu_2 < ... (equal rates merged)
#
# has no more real zeros than there are sign changes in a_1, a_2, ... (Descartes'
# rule of signs, which Laguerre extended to sums of exponentials).  Between two
# neighbouring zeros of its derivative h is monotone, so it crosses zero at
# most once there, and whether it does is read off the signs at the two ends.
# The zeros of the derivative are found the same way, one level down:
# exp(mu_1 t) h(t) has the same sign as h, and its derivative,
#
#     sum over k > 1 of -a_k (mu_k - mu_1) exp(-(mu_k - mu_1) t),
#
# is a sum of one exponential fewer.  The recursion stops at a sum whose
# coefficients change sign once or not at all.  Not at all: every term has the
# same sign, so there is no zero.  Once, between rates mu_j and mu_j+1: then
# exp(m t) h(t), for any m between the two, has a derivative whose terms all
# share one sign, so it is strictly monotone and has one zero at most, which
# the signs at the ends of the interval bracket.  Shifting the rates so the
# slowest is zero is also what keeps the sign readable a week out, where
# every term of the unshifted sum would have underflowed to zero.
# expSumZeros() below does this.  It costs one root-finding per stationary
# point in the search interval and one for the crossing, and none for the
# stationary points of the commonest sum, a plasma that only decays.
#
# This runs at every point of every curve, so the cost matters.  The roots are
# found by expSumRoot(), a safeguarded Newton iteration, rather than by
# uniroot(), whose fixed overhead per call was most of the time; and
# recoveryRows() in R/recoveryStates.R prepares a whole state set's terms
# once and clears, without solving, every row that a rigorous bound shows
# never reaches the target.  Measured on R 4.3.3, 2026-10-09, against the grid
# search it replaced: oxycodone 10 mg by mouth every 6 hours for two weeks,
# 2016 points, about 0.2 s against 0.17 s for oxycodone's own row and 0.02 s
# against 0.05 s for the oxymorphone it forms; 59 points of a propofol
# infusion, about 5 ms against 4 ms.
#
# WHAT IS LEFT OF A TOLERANCE
# ===========================
# Each crossing is located to within RECOVERY_TOL minutes (60 microseconds),
# and each stationary point the search divides the interval at to within
# RECOVERY_TOL_PEAK (60 nanoseconds), by expSumRoot(); neither is asked to be
# closer than the doubles allow, which on a year's horizon is about 1e-7
# minutes.  A peak located that closely is misjudged by about
# |C''| RECOVERY_TOL_PEAK^2 / 2: 2e-19 mcg/mL for the propofol case above,
# whose |C''| at the peak is 0.38 mcg/mL/min^2.  That is far below the
# rounding error of the sum itself, 2.2e-16 of the sum of the absolute values
# of its terms, which there is 27.6 mcg/mL: 6e-15 mcg/mL.  So the one
# excursion that can still be missed is one whose height above the target is
# lost in rounding -- for the propofol case, a peak within about 1e-14 mcg/mL
# of a 1 mcg/mL threshold -- and the time reported is within RECOVERY_TOL of
# the last crossing of the sum as evaluated in double precision.
# (Claude Code, 2026-10-09, at the request of Steven L. Shafer.)

# How closely each crossing is located, in minutes.  The grid search this
# replaced solved to 0.01 minutes.
RECOVERY_TOL <- 1e-6

# How closely each stationary point -- a peak or a trough of the concentration,
# or of one of its derivatives -- is located, in minutes.  Tighter than the
# crossing, because whether a brief excursion is found at all turns on the
# concentration AT the peak.
RECOVERY_TOL_PEAK <- 1e-9

# How far ahead to look.  A day for a drug timed on its effect site: beyond
# that "more than a day" is the answer that matters for recovery.  A week for
# a drug timed on its plasma -- the antibiotics -- whose time above the MIC
# commonly runs past a day (free vancomycin after 1.5 g, for one).  Chosen by
# Steven L. Shafer, 2026-10-07.  See "Which concentration is timed" in
# R/recoveryStates.R.
RECOVERY_HORIZON_EFFECT <- MINS_PER_DAY
RECOVERY_HORIZON_PLASMA <- MINS_PER_WEEK

# The plasma horizon a run of length `maximum` uses: a week, or the length of
# the plot if that is longer.  A drug timed on its plasma may be one whose
# half-life is measured in weeks -- amiodarone's is 55 days -- and on a plot
# of months a week's horizon would report "a week" at almost every point,
# which is the cap, not an answer.  Looking as far ahead as the plot runs
# lets it report any time up to the length of the plot itself.  The search
# above costs the same however far ahead it looks, and the answer is still
# to RECOVERY_TOL.  A plot of a week or less keeps the week, and with it the
# same answers.  The effect-site horizon stays a day: beyond that "more than a
# day" is the answer that matters for recovery.
# (Claude Code, 2026-10-07, at the request of Steven L. Shafer.)
recoveryHorizonPlasma <- function(maximum)
{
  max(RECOVERY_HORIZON_PLASMA, maximum)
}

# The coarse grid recoveryRows() screens rows on before solving any: zero,
# then 0.05 minutes to the horizon in equal ratios, about 12% a step -- the
# grid the search itself bracketed on until 2026-10-09.  It now decides only
# how much work is skipped, never the answer; see recoveryRows() in
# R/recoveryStates.R.
recoveryGrid <- function(horizon)
{
  n <- ceiling(90 * log(horizon / 0.05) / log(MINS_PER_DAY / 0.05))
  c(0, exp(seq(log(0.05), log(horizon), length.out = n)))
}

recoveryCalc <-   function(
  state,
  lambda,
  target,
  horizon = MINS_PER_DAY
  )
{
  # C(t) - target as one sum, the target being the term with rate zero.
  h <- expSumTerms(c(state, -target), c(lambda, 0))
  if (is.null(h)) return(NA_real_)          # a non-finite amplitude or rate
  recoveryFromTerms(h$a, h$mu, horizon)
}


#' Time until a sum of exponentials last comes down through zero
#'
#' The body of \code{recoveryCalc()}, for a sum already in the form
#' \code{expSumTerms()} returns, with the target folded in as the term at rate
#' zero.  \code{recoveryRows()} prepares a whole state set's columns at once
#' and calls this row by row.
#'
#' @param a,mu the terms of C(t) - target, as \code{expSumTerms()} returns them
#' @param horizon how far ahead to look, minutes
#'
#' @returns minutes
#' @keywords internal
recoveryFromTerms <- function(a, mu, horizon)
{
  # Still above the target at the end of the search: "at least the horizon".
  if (sum(a * exp(-mu * horizon)) > 0) return(horizon)

  # No term that could carry it above the target: for t >= 0 the sum is at
  # most the sum of its positive amplitudes.  Most of a long run, once the
  # drug has fallen well below its threshold, ends here.
  if (sum(a[a > 0]) <= 0) return(0)

  # At or below the target at the horizon, so the last zero at which the sum
  # changes sign is a downward crossing; none means it never rises above.
  crossings <- expSumZeros(a, mu, 0, horizon, RECOVERY_TOL, RECOVERY_TOL_PEAK)
  if (length(crossings) == 0) 0 else crossings[length(crossings)]
}


#' The terms of a sum of exponentials, ready for expSumZeros()
#'
#' Drops terms with a zero amplitude (an unused route's absorption state, say,
#' whose rate may be zero or missing), merges terms with exactly equal rates --
#' a metabolite fold concatenates two state sets that share the receiving
#' drug's eigenvalues -- orders the rest by rate, and shifts the rates so the
#' slowest is zero.  The shift multiplies the sum by exp(mu_1 t), which changes
#' no sign and no zero.
#'
#' @param a amplitudes
#' @param mu rates, one per amplitude
#'
#' @returns a list of \code{a} and \code{mu}, ordered by \code{mu}, with
#'   \code{mu[1] == 0} and no zero amplitude (a single zero term when nothing
#'   is left); NULL when an amplitude or rate is not finite
#' @keywords internal
expSumTerms <- function(a, mu)
{
  keep <- a != 0 | is.na(a)
  a  <- a[keep]
  mu <- mu[keep]
  if (!all(is.finite(a)) || !all(is.finite(mu))) return(NULL)

  if (anyDuplicated(mu))
  {
    a  <- as.vector(rowsum(a, mu))        # grouped in increasing order of mu
    mu <- sort(unique(mu))
    keep <- a != 0
    a  <- a[keep]
    mu <- mu[keep]
  } else if (is.unsorted(mu)) {
    o  <- sort.list(mu)
    a  <- a[o]
    mu <- mu[o]
  }
  if (length(a) == 0) return(list(a = 0, mu = 0))
  list(a = a, mu = mu - mu[1])
}


#' Every zero at which a sum of exponentials changes sign
#'
#' The zeros of \code{sum(a * exp(-mu * t))} inside \code{(lo, hi)} at which it
#' changes sign, found exactly rather than by sampling: the stationary points
#' are found first, by the same function applied to the derivative, and the sum
#' is monotone between them, so each piece holds at most one zero and the signs
#' at its ends say whether it does.  A zero at which the sum only touches zero
#' is not a crossing and is not returned.  See the header of
#' \code{R/recoveryCalc.R}.
#'
#' @param a,mu the terms, as \code{expSumTerms()} returns them: ordered by
#'   rate, the rates distinct with the first zero, no amplitude zero
#' @param lo,hi the interval, \code{lo < hi}
#' @param tol how closely to locate each zero, minutes
#' @param tolPeak how closely to locate the stationary points the search
#'   divides the interval at -- the zeros of the derivative, and of its
#'   derivatives in turn
#'
#' @returns the zeros, in increasing order
#' @keywords internal
expSumZeros <- function(a, mu, lo, hi, tol = RECOVERY_TOL,
                        tolPeak = RECOVERY_TOL_PEAK)
{
  n <- length(a)
  if (n < 2) return(numeric(0))

  # Descartes: no more zeros on the whole line than sign changes.
  changes <- sum(a[-1] * a[-n] < 0)
  if (changes == 0) return(numeric(0))

  # The pieces over which the sum is monotone.  With one sign change there is
  # at most one zero anywhere, so the interval is a single piece.  Otherwise
  # its ends are the zeros of the derivative of the sum: the first term is
  # constant (mu[1] is zero) and drops out, and the rest are already ordered,
  # distinct and nonzero, so only the shift is needed.
  if (changes == 1) {
    breaks <- c(lo, hi)
  } else {
    m <- mu[-1]
    breaks <- c(lo, expSumZeros(-a[-1] * m, m - m[1], lo, hi, tolPeak, tolPeak),
                hi)
  }

  value <- vapply(breaks, function(t) sum(a * exp(-mu * t)), numeric(1))
  roots <- numeric(0)
  for (j in which(value[-length(value)] * value[-1] < 0))
    roots <- c(roots, expSumRoot(a, mu, breaks[j], breaks[j + 1],
                                 value[j], value[j + 1], tol))
  roots
}


#' The zero of a sum of exponentials inside a bracket
#'
#' Safeguarded Newton (Numerical Recipes' rtsafe), worked in log time,
#' \code{s = log(1 + t)}: a sum of exponentials is far closer to straight in s
#' than in t over a bracket that runs from seconds to days, and the derivative
#' comes almost free with the value, from the same exponentials.  A Newton step
#' that would leave the bracket, or that is not halving it fast enough, is
#' replaced by bisection in s.  It stops when the bracket is narrower than
#' \code{tol} in t, or when a Newton step is shorter than \code{tol / 2} and
#' the sum is shown to change sign within \code{tol / 2} of where it lands.
#' This rather than \code{uniroot()}, whose fixed cost per call was most of
#' the time until threshold on a long plot: it is called once or twice at
#' every point of every curve.
#'
#' @param a,mu the terms of the sum
#' @param lo,hi the bracket, \code{0 <= lo < hi}
#' @param flo,fhi the sum at \code{lo} and \code{hi}, of opposite signs
#' @param tol how closely to locate the zero, minutes
#'
#' @returns the zero, to within \code{tol}
#' @keywords internal
expSumRoot <- function(a, mu, lo, hi, flo, fhi, tol)
{
  # No closer than the doubles allow: in log time one unit in the last place
  # is about (1 + t) log(1 + t) 2.2e-16 minutes, and the bracket is not asked
  # to be narrower than 64 of those -- 1.5e-10 minutes at a day, 1e-7 at a
  # year.
  tol <- max(tol, 64 * .Machine$double.eps * (1 + hi) * max(1, log1p(hi)))
  am  <- a * mu
  sLo <- log1p(lo)
  sHi <- log1p(hi)
  # Oriented so that the sum is negative at sLo and positive at sHi.
  if (flo > 0) { x <- sLo; sLo <- sHi; sHi <- x }

  s <- (sLo + sHi) / 2
  stepOld <- abs(sHi - sLo)
  step    <- stepOld
  for (iteration in 1:200)
  {
    t <- expm1(s)
    e <- exp(-mu * t)
    f <- sum(a * e)
    if (f == 0) return(t)
    if (f < 0) sLo <- s else sHi <- s
    if (abs(expm1(sHi) - expm1(sLo)) <= tol) return(expm1((sLo + sHi) / 2))

    # d sum / ds = d sum / dt * (1 + t)
    df <- -sum(am * e) * (1 + t)
    newton <- s - f / df
    if (!is.finite(newton) || (newton - sLo) * (newton - sHi) >= 0 ||
        abs(2 * f) > abs(stepOld * df))
    {
      stepOld <- step
      step    <- (sHi - sLo) / 2
      s       <- sLo + step
    } else {
      stepOld <- step
      step    <- newton - s
      s       <- newton
      tNew    <- expm1(s)
      if (abs(tNew - t) < tol / 2)
      {
        l <- max(min(expm1(sLo), expm1(sHi)), tNew - tol / 2)
        r <- min(max(expm1(sLo), expm1(sHi)), tNew + tol / 2)
        if (sum(a * exp(-mu * l)) * sum(a * exp(-mu * r)) <= 0) return(tNew)
      }
    }
  }
  expm1((sLo + sHi) / 2)
}
