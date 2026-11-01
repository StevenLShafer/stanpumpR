# -----------------------------------------------------------------------------
# Provenance
# ----------
# Drafted by Claude Chat (claude.ai), 2026-08-07.  Not authored by Claude Code.
# The mathematics was verified numerically by the authoring model against
# adaptive quadrature; the R transcription was not executed at that time.
#
# 2026-08-23, Claude Code (Fable 5): the draft's complex branch called a CRAN
# package named 'Faddeeva' that does not exist, and 'RcppFaddeeva' (the name it
# was corrected to in #367) was archived from CRAN on 2022-06-13.  Rather than
# depend on either, the base-R faddeevaW() below was vendored in (Weideman
# 1994) and the verification re-run.  The file has been dependency-free since.
#
# 2026-10-31, #641 ("Remove dead R code, cleanup F") DELETED this file, on the
# correct observation that the version then on master was 221 lines of comments
# with no live code and no caller.  The working version survived only in a git
# stash.  The cause was structural: the only verification was an `if (FALSE)`
# block, which no test suite runs, so the file could not defend itself.  That
# is why the checks now live in tests/testthat/test-ig-absorption.R and this
# file holds production code only.  Do not move them back.
#
# 2026-11-01, Claude Code (Opus 5): restored, and verified INDEPENDENTLY rather
# than on the note above.  Worst relative error 1.6e-14 across 84 realistic
# parameter sets x ~80 time points, at concentrations above 1e-10 of peak, with
# the complex branch exercised in 57 of the 84.  Two apparent failures in the
# earlier check were traced to the REFERENCE, not to this code: plain
# stats::integrate() has an absolute accuracy floor that makes it wrong by
# 6e-8 at 1e-40 magnitudes and 1.7e-2 at 1e-318, where this closed form is
# exact to 1e-15.  See the test file for the two references that establish it.
# Functions renamed to the codebase's camelCase at the same time.
#
# NOT WIRED INTO THE SIMULATION ENGINE.  It is a validated kernel with no
# caller yet; see "Wiring" below for the open design question, which is
# deliberately not decided here.
# -----------------------------------------------------------------------------
# Closed-form convolution of an Inverse Gaussian absorption density with a
# sum-of-exponentials disposition function.
#
#   C(t) = F * Dose * integral_0^t  f(s; mu, lambda) * sum_i A_i exp(-k_i (t-s)) ds
#
# where f is the Inverse Gaussian (Wald) density with mean mu and shape lambda.
#
# Why this shape
# --------------
# A first-order depot starts absorbing at its maximum rate the instant the lag
# ends; a zero-order release steps from nothing to its full rate and back.
# Neither is a plausible account of a tablet dissolving.  The Inverse Gaussian
# density rises from zero, peaks, and decays, is differentiable throughout, and
# is the first-passage-time density of Brownian motion with drift -- very
# nearly a mechanistic story about gastric transit.  It reaches a similar time
# course to a lagged zero-order input without the discontinuities, at the cost
# of this file.
#
# Derivation sketch
# -----------------
# Writing the IG density with the exponent expanded,
#
#   f(t) = sqrt(lambda / (2 pi t^3)) * exp(lambda/mu) *
#            exp(-lambda t / (2 mu^2) - lambda / (2 t))
#
# the 1/t term carries lambda only, not mu.  Multiplying by exp(alpha t)
# therefore shifts only the coefficient of t: an exponentially tilted Inverse
# Gaussian is another Inverse Gaussian with the same lambda.  The required
# integral has the standard closed form
#
#   integral_0^t s^(-3/2) exp(-c/s - b s) ds
#     = (1/2) sqrt(pi/c) [ exp(-2 sqrt(bc)) erfc(sqrt(c/t) - sqrt(bt))
#                        + exp( 2 sqrt(bc)) erfc(sqrt(c/t) + sqrt(bt)) ]
#
# with b = lambda/(2 mu^2) - k_i  and  c = lambda/2.
#
# Folding exp(-k_i t) into the same exponential cancels k_i from the prefactor,
# which is what keeps the computation in range for every term.
#
# Validity
# --------
# b > 0 requires k_i < lambda / (2 mu^2).  Since CV^2 = mu/lambda for the IG,
# that threshold is 1/(2 mu CV^2) -- roughly 2/h for a one-hour mean absorption
# time at CV 0.5.  Fast distribution exponents routinely exceed it, so the
# b < 0 branch is the common case, not an edge case: it was taken by at least
# one exponent in 57 of the 84 parameter sets of the verification sweep
# recorded above (the test file runs a 12-set subset of it).  There sqrt(b) is
# imaginary, the two erfcx arguments become complex conjugates, and their sum
# is real: erfcx(z) = w(i z), the Faddeeva function.
#
# Accuracy
# --------
# The error is ~1e-16 RELATIVE TO THE LARGEST INTERMEDIATE TERM, not to the
# answer, because both branches end in a difference (real branch) or a real
# part (complex branch) that can be far below the terms composing it.
# Relative accuracy in the answer therefore degrades in the extreme tails,
# where the concentration is 1e-50 of its peak and no one cares.  Above 1e-10
# of peak it is 1.6e-14.  Measure any future change the same way -- against a
# reference that has no cancellation, and at concentrations that matter.
#
# Wiring -- OPEN, for Steven L. Shafer to decide
# ----------------------------------------------
# [decision] How should a drug declare IG absorption?  The precedents for a
# non-first-order input are `oralZeroOrder` (carvedilol, R/oral-zero-order.R),
# `oralPulses` (R/oral-pulses.R), `zeroOrderPO` (canagliflozin) and
# `zeroOrderSC` (the insulins), all of which REWRITE THE DOSE ROWS in
# simCpCe() and then run an existing engine.  IG cannot work that way: it is a
# genuinely different input kernel, not a re-expression of first-order or
# zero-order input, so it needs a branch in the absorption dispatch
# (R/advanceClosedFormPO_IM_IN.R) reached from routeKaField()/doseRoute() in
# R/routes.R, with a block giving `mu` (mean absorption time, minutes) and
# either `lambda` or `cv2` (lambda = mu / cv2).  Superposition over doses
# still holds, the system being linear, so the per-dose structure of the oral
# engine carries over unchanged.  Not implemented here: a mechanism with no
# consumer is the dead code that got this file deleted once already.
#
# [decision] Which drug, if any, converts first?  NO EXISTING DRUG MAY BE
# CONVERTED SILENTLY: every current model was fitted with first-order
# absorption or a documented zero-order/pulse approximation of a published
# shape, and replacing a published fit's input model changes that drug's curve.
# The candidates where a smooth differentiable kernel would earn its place are
# the two drugs that currently DISCRETISE a smooth published shape --
# hydrochlorothiazide's Weibull (shape 2.84, stood in for by eight oral pulses
# to 0.53% over the curve and 0.01% at the peak) and MDMA's zero-order depot
# filling (two lagged half doses).  An IG is not automatically the right shape
# for a Weibull fit: either substitution is a FITTING question with its own
# error budget, measured against the exact published shape at the pulse and
# kink times (a uniform grid flatters these approximations -- see
# R/drugs_hydrochlorothiazide.R), and held to the standard those drugs already
# meet.  Until that is done and read, this kernel is for new models only.
#
# HYDROCHLOROTHIAZIDE HAS NOW BEEN MEASURED, and the answer is no (Claude
# Code, 2026-11-01, at Shafer's request).  An IG fitted minimax to the plasma
# curve, pooled over the paper's four renal functions, reaches 6.6x the
# documented standard on the binding limit; however the two limits are traded
# it stays several-fold outside (curve alone 1.05%, 2.0x its limit, but then
# the peak is out by 0.92%, 92x its limit; the peak zeroed, the curve is
# 2.10%, 4.0x), against the eight pulses' 64% of both.  The whole deficit is
# the UPSTROKE: from 1.5 h on -- the peak, its time, and the entire
# elimination phase -- the IG is within 0.5% relative, and before that it runs
# up to +3.5% of peak high.  The reason is not the moments, which very nearly
# agree (the published input has CV 0.629 and skew 1.843, and an IG with that
# CV is forced to skew 3 CV = 1.886, a 2% mismatch): it is that Van Wart's
# depot is not an input DENSITY at all but a time-varying rate constant,
# 1 - exp(-(ka t)^gamma), still ramping up from zero while the IG is already
# absorbing.  A drug whose SOURCE fitted an IG input is the consumer this
# kernel wants; a drug whose source fitted something else is a refit, and this
# one does not survive it.  Do not re-derive this -- the working is in the
# session of 2026-11-01.
# -----------------------------------------------------------------------------


#' Scaled complementary error function for real, non-negative x
#'
#' erfcx(x) = exp(x^2) * erfc(x), computed in log space so that large x
#' neither overflows nor underflows.  Base R only -- no dependency.
#'
#' @param x numeric vector, non-negative
#' @return numeric vector, erfcx(x)
#' @noRd
erfcxReal <- function(x) {
  # erfc(x) = 2 * pnorm(-x * sqrt(2))
  exp(x^2 + log(2) + pnorm(-x * sqrt(2), log.p = TRUE))
}


# -----------------------------------------------------------------------------
# Vendored Faddeeva function -- provenance
# ----------------------------------------
# Written by Claude Code (Fable 5), 2026-08-23, to remove the dependency on a
# 'Faddeeva' CRAN package that turned out not to exist.  Implements
#
#   w(z) = exp(-z^2) * erfc(-i z)
#
# by the rational approximation of J.A.C. Weideman, "Computation of the
# complex error function", SIAM J. Numer. Anal. 31(4):1497-1518 (1994),
# transcribed from the cef.m MATLAB reference code in that paper.  With N = 64
# terms it is accurate to about 1e-14 throughout the upper half-plane
# Im(z) >= 0, which is the only region this file evaluates (z = q + i p with
# q, p > 0).  Do NOT call it with Im(z) < 0; that region needs the reflection
# formula, which is deliberately not implemented here.
#
# The 1e-14 is ABSOLUTE, or equivalently relative to |w(z)|.  On the real axis
# Re w(x) = exp(-x^2) falls below that floor near x = 6 and is meaningless
# beyond it; this is a property of the approximation, not a transcription
# error, and it does not reach igAbsorptionConc(), whose imaginary part
# p = sqrt(lambda / (2 t)) is strictly positive.  Verified on both axes
# against the exact identities w(0) = 1, w(iy) = exp(y^2) erfc(y) and
# Re w(x) = exp(-x^2) in tests/testthat/test-ig-absorption.R.
# -----------------------------------------------------------------------------

# The N expansion coefficients depend only on N, so compute them once at load
# time rather than on every call.  `a` holds the polynomial coefficients,
# highest degree first; `L` is Weideman's optimal scale parameter.
.faddeevaSetup <- local({
  N  <- 64L
  M  <- 2L * N
  M2 <- 2L * M
  L  <- sqrt(N / sqrt(2))

  # Sample the generating function exp(-t^2) (L^2 + t^2) on the tangent-mapped
  # grid, then read off the expansion coefficients with a single FFT.
  k     <- seq.int(-M + 1L, M - 1L)
  theta <- k * pi / M
  t     <- L * tan(theta / 2)
  f     <- c(0, exp(-t^2) * (L^2 + t^2))

  # fftshift: rotate the sample vector so the zero-frequency entry leads.
  n    <- length(f)                       # n = M2, always even
  half <- n %/% 2L
  fsh  <- c(f[(half + 1L):n], f[1L:half])
  a    <- Re(fft(fsh)) / M2
  a    <- rev(a[2L:(N + 1L)])             # highest-degree coefficient first

  list(a = a, L = L)
})

#' Faddeeva function w(z) for Im(z) >= 0
#'
#' Base R only -- no dependency.  See the provenance block above for the
#' approximation, its accuracy and the half-plane restriction.
#'
#' @param z complex vector with Im(z) >= 0
#' @return complex vector, w(z) = exp(-z^2) erfc(-i z)
#' @noRd
faddeevaW <- function(z) {
  L <- .faddeevaSetup$L

  # Moebius map onto the unit disk, then Horner evaluation of the polynomial
  # part of Weideman's rational approximation.
  Z <- (L + 1i * z) / (L - 1i * z)
  p <- 0
  for (coef in .faddeevaSetup$a) p <- p * Z + coef

  2 * p / (L - 1i * z)^2 + (1 / sqrt(pi)) / (L - 1i * z)
}


#' Plasma concentration with Inverse Gaussian absorption
#'
#' Exact closed form, to rounding: the system is linear and nothing here is
#' approximated except the Faddeeva evaluation on the complex branch.
#'
#' @param t       numeric vector of times (same units as mu and the exponents)
#' @param A       coefficients of the unit disposition function
#' @param k       exponents of the unit disposition function (same length as A)
#' @param mu      mean absorption time
#' @param lambda  IG shape parameter (CV^2 of absorption time = mu / lambda)
#' @param dose    administered dose
#' @param bioavailability fraction of the dose absorbed
#'
#' @return numeric vector of concentrations, same length as t
#' @noRd
igAbsorptionConc <- function(t, A, k, mu, lambda, dose = 1,
                             bioavailability = 1) {

  stopifnot(length(A) == length(k), mu > 0, lambda > 0, all(is.finite(t)))

  out <- numeric(length(t))
  pos <- t > 0
  if (!any(pos)) return(out)

  tt <- t[pos]
  c0 <- lambda / 2

  # Common prefactor exponent, independent of k_i.  This cancellation is what
  # keeps every branch bounded.
  E <- lambda / mu - c0 / tt - lambda * tt / (2 * mu^2)
  p <- sqrt(c0 / tt)

  total <- numeric(length(tt))

  for (i in seq_along(A)) {

    b <- lambda / (2 * mu^2) - k[i]

    if (b > 0) {
      # ---- real branch --------------------------------------------------
      q <- sqrt(b * tt)
      v <- p - q
      u <- p + q

      # erfcx(u) is always bounded: u > 0.
      term_u <- 0.5 * exp(E) * erfcxReal(u)

      # For v < 0, erfcx(v) overflows.  Use the reflection
      #   erfcx(v) = 2 exp(v^2) - erfcx(-v)
      # and note E + v^2 = lambda/mu - 2 sqrt(bc) - k_i t, which decays.
      ex <- 0.5 * exp(E) * erfcxReal(abs(v))
      big <- exp(lambda / mu - 2 * sqrt(b * c0) - k[i] * tt)
      term_v <- ifelse(v >= 0, ex, big - ex)

      total <- total + A[i] * (term_u + term_v)

    } else if (b < 0) {
      # ---- complex branch -----------------------------------------------
      # v = p - i q, u = conj(v), so erfcx(v) + erfcx(u) = 2 Re[erfcx(v)].
      # erfcx(v) = w(i v), and i v = q + i p, safely in the upper half-plane
      # where the vendored faddeevaW() above is valid.
      q <- sqrt(-b * tt)
      z <- complex(real = q, imaginary = p)
      total <- total + A[i] * exp(E) * Re(faddeevaW(z))

    } else {
      # ---- b exactly zero: limiting (Levy) case --------------------------
      # sqrt(b t) = 0, so v = u = p and the bracket is 2 * erfcx(p).
      total <- total + A[i] * exp(E) * erfcxReal(p)
    }
  }

  out[pos] <- bioavailability * dose * total
  out
}
