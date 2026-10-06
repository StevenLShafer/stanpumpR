# -----------------------------------------------------------------------------
# Provenance
# ----------
# Drafted by Claude Code (Claude Opus 5), 2026-10-05, at the request of
# Steven L. Shafer, so that "time until threshold" can account for a drug that
# arrives as another drug's active metabolite.
#
# STATUS: run and verified on R 4.6.1 by tests/testthat/test-recovery-states.R
# and tests/testthat/test-recovery-engines.R.
# -----------------------------------------------------------------------------
#
# WHY THE STATES HAVE TO TRAVEL
# =============================
# recoveryCalc() does not read a concentration.  It reads the effect site as a
# sum of exponentials -- one amplitude per eigenvalue -- and solves for the time
# at which that sum last comes down through a threshold.  Recovery therefore
# cannot be summed after the fact: two drugs' recovery times do not add, and
# neither does the time belonging to a concentration curve that is itself a sum.
#
# What DOES add is the state.  The effect site of a sum of exponentials is
# itself a sum of exponentials over the same eigenvalues plus ke0, and
# superposition holds through the whole intravenous path because every step of
# it is linear.  So the right object to carry across a metabolite fold is the
# vector of effect-site amplitudes, one per eigenvalue.  Concatenate the
# receiving drug's own amplitudes with the formed contribution's, hand the
# combined vector to recoveryCalc(), and the answer is exact -- not an
# approximation, and not the jointly simulated washout the inhaled gases need
# (gasCoupledRecovery() in R/gasRecovery.R), because the gases' uptake is
# coupled through a shared alveolus and an intravenous drug's disposition is
# not.
#
# A state set is a list of
#   time   the engine's own time line, length L
#   state  an L x K matrix of effect-site amplitudes
#   lambda the K eigenvalues: a vector when the PK does not change, or an L x K
#          matrix when it does (advanceClosedForm1)
#
# Each engine builds one while it is computing recovery anyway, so carrying it
# out costs a cbind().
# -----------------------------------------------------------------------------


#' Assemble a set of effect-site exponential states
#'
#' @param time the engine's time line
#' @param state a list or matrix of effect-site amplitudes, one column per
#'   eigenvalue
#' @param lambda the eigenvalues: a vector of length \code{ncol(state)}, or a
#'   matrix the same shape as \code{state} when the PK changes with time
#'
#' @returns a state set: a list of \code{time}, \code{state} and \code{lambda}
#' @keywords internal
recoveryStateSet <- function(time, state, lambda)
{
  state <- if (is.matrix(state)) state else do.call(cbind, state)
  stopifnot(nrow(state) == length(time))
  if (is.matrix(lambda)) {
    stopifnot(dim(lambda) == dim(state))
  } else {
    stopifnot(length(lambda) == ncol(state))
  }
  list(time = time, state = state, lambda = lambda)
}


#' Carry a set of exponential states onto another time line
#'
#' Exact, not interpolated.  Between two neighbouring points of an engine's own
#' time line the input rate is constant -- every dose time is a point on that
#' line -- so each state obeys
#'
#'     s(t) = s0 exp(-lambda dt) + I (1 - exp(-lambda dt))
#'
#' for some constant \code{I}, and the two known endpoints determine \code{I}
#' without any of the dose information being carried along:
#'
#'     I = (s1 - s0 exp(-lambda D)) / (1 - exp(-lambda D))
#'
#' where \code{D} is the whole interval.  A state with a zero eigenvalue -- a
#' compartment the model does not use -- accumulates linearly instead, and is
#' interpolated linearly, which is equally exact.
#'
#' The one place this smears is across a bolus, which lands at the right-hand
#' end of an interval rather than being spread over it.  The engines insert the
#' instant 0.01 minutes before every bolus, so the smear is confined to that
#' 0.01 minutes and cannot reach any point outside it.
#'
#' @param set a state set from \code{recoveryStateSet()}
#' @param times the times to carry it onto
#'
#' @returns a state set on \code{times}
#' @keywords internal
advanceStatesOnto <- function(set, times)
{
  t0 <- set$time
  S  <- set$state
  L  <- length(t0)
  K  <- ncol(S)
  nT <- length(times)

  Lam <- if (is.matrix(set$lambda)) set$lambda else
    matrix(set$lambda, L, K, byrow = TRUE)

  if (identical(times, t0))
    return(list(time = times, state = S, lambda = Lam))

  # The interval each new time falls in.  lo is the last point at or before it;
  # hi the next one.  A time at or beyond the last point has lo == hi, and then
  # there is no further input and the states simply decay.
  lo <- pmin(pmax(findInterval(times, t0), 1L), L)
  hi <- pmin(lo + 1L, L)

  # The eigenvalues governing the step INTO hi, which is also the set in force
  # at hi -- the same convention advanceClosedForm1() uses when it reports
  # recovery from the PK in force at a point.
  lam <- Lam[hi, , drop = FALSE]
  Slo <- S[lo, , drop = FALSE]
  Shi <- S[hi, , drop = FALSE]

  # dFull and d are per row; a matrix times a vector of nrow() recycles down
  # the rows, which is one value per row in every column.
  dFull <- t0[hi] - t0[lo]
  d     <- times  - t0[lo]

  E <- exp(-lam * dFull)
  e <- exp(-lam * d)

  # Default: free decay from the left anchor.  Right for a time at or past the
  # end of the line, and for an exact hit, where d is zero and e is one.
  out <- Slo * e

  inside <- matrix(dFull > 0, nT, K)
  decays <- inside & (1 - E) > 1e-12

  I <- (Shi - Slo * E) / (1 - E)
  out[decays] <- (Slo * e + I * (1 - e))[decays]

  # A zero eigenvalue accumulates linearly rather than exponentially.
  flat <- inside & !decays
  if (any(flat))
  {
    lin <- Slo + (Shi - Slo) * (d / dFull)
    out[flat] <- lin[flat]
  }

  list(time = times, state = out, lambda = lam)
}


#' Time until threshold from a set of effect-site states
#'
#' @param set a state set from \code{recoveryStateSet()}
#' @param emerge the threshold the effect site has to fall to
#'
#' @returns minutes, one per row of \code{set$state}; zeros when there is no
#'   threshold to fall to
#' @keywords internal
recoveryFromStates <- function(set, emerge)
{
  nT <- nrow(set$state)
  if (is.null(emerge) || length(emerge) != 1 || is.na(emerge)) return(rep(0, nT))

  Lam <- if (is.matrix(set$lambda)) set$lambda else
    matrix(set$lambda, nT, ncol(set$state), byrow = TRUE)

  vapply(seq_len(nT),
         function(i) recoveryCalc(set$state[i, ], Lam[i, ], emerge),
         numeric(1))
}


#' Time until threshold for a drug receiving a metabolite contribution
#'
#' Carries every contributing state set onto one time line, concatenates the
#' amplitudes, and solves once.  That is exact: the effect site the patient has
#' is the sum of the effect sites each contribution produces, and the sum is
#' again a sum of exponentials over the union of the eigenvalue sets.
#'
#' @param times the time line to report on, normally the merged series'
#' @param sets a list of state sets -- the receiving drug's own, when it was
#'   given directly, followed by one per parent contribution
#' @param emerge the receiving drug's threshold
#'
#' @returns minutes, one per element of \code{times}
#' @keywords internal
combinedRecovery <- function(times, sets, emerge)
{
  # advanceStatesOnto() always returns its lambdas as a matrix on the new time
  # line, so the two cbinds line up whether or not a contributor's PK changed
  # with time.
  onto <- lapply(sets, advanceStatesOnto, times = times)
  recoveryFromStates(
    list(state  = do.call(cbind, lapply(onto, `[[`, "state")),
         lambda = do.call(cbind, lapply(onto, `[[`, "lambda"))),
    emerge
  )
}


#' Effect-site coefficients of a concentration given as a sum of exponentials
#'
#' The effect site is a one-compartment link driven by the plasma, so it is the
#' convolution of that plasma concentration with \code{ke0 exp(-ke0 t)}: the
#' same integral \code{convolveExponentials()} performs, with a single-term
#' disposition.  It adds one exponential, at \code{ke0}, whose coefficient is
#' minus the sum of the others -- which is what keeps the effect site at zero
#' when the plasma jumps.
#'
#' This is the generalisation of the four lines getDrugPK.R already has for a
#' three-compartment model (\code{e_coef_bolus_l1 <- p_coef_bolus_l1 / (ke0 -
#' lambda_1) * ke0} and so on) to an arbitrary eigenvalue set, which is what a
#' metabolite needs: its concentration is a sum over the union of its own and
#' its parent's eigenvalues.
#'
#' @param coefs a list of \code{lambda} and any of \code{bolus},
#'   \code{infusion} and \code{PO}, as \code{metaboliteCoefficients()} returns
#' @param ke0 the effect-site equilibration rate constant
#'
#' @returns the same shape, over \code{c(lambda, ke0)}
#' @keywords internal
effectSiteCoefficients <- function(coefs, ke0)
{
  stopifnot(ke0 > 0)
  lambda <- separateEigenvalues(coefs$lambda, ke0)
  retard <- ke0 / (ke0 - lambda)

  # An infusion coefficient is the bolus coefficient over its own eigenvalue,
  # because the response to a unit infusion is the integral of the response to
  # a unit bolus.  getDrugPK.R uses the same relation.
  link <- function(x) {
    if (is.null(x)) return(NULL)
    y <- x * retard
    c(y, -sum(y))
  }

  bolus <- link(coefs$bolus)
  out <- list(lambda = c(lambda, ke0), bolus = bolus, PO = link(coefs$PO))
  out$infusion <- if (is.null(bolus)) NULL else bolus / out$lambda
  out
}
