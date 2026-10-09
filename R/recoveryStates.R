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
#   time    the engine's own time line, length L
#   state   an L x K matrix of effect-site amplitudes
#   lambda  the K eigenvalues: a vector when the PK does not change, or an L x K
#           matrix when it does (advanceClosedForm1)
#   pending an optional length-L logical: the times at which these amplitudes
#           do NOT tell the whole story, because a dose has been given that has
#           not begun to be absorbed
#
# WHAT THE MASK IS FOR
# ====================
# An extravascular dose may carry an absorption lag, and the engines apply it
# by moving the dose later.  Between being given and starting, the drug is in
# the patient but has no state in the model at all, so recoveryCalc() finds
# nothing above the threshold and returns zero -- which reads as "recovered"
# when it means "not yet absorbed".  Those are opposite situations and the
# number cannot tell them apart.
#
# The mask makes the engine say so: NA, not zero, wherever a dose is pending.
# It is not only the zero case.  A pending dose makes the answer wrong even
# when other doses give a nonzero one, because the time reported leaves out
# drug that is certain to be absorbed.
#
# This is deliberately the small half of the problem.  Reporting the RIGHT time
# through a lag means recoveryCalc() accounting for input that has not arrived,
# and a dose starting in the future contributes a delayed term that is not a
# sum of exponentials from now -- the one representation recoveryCalc() takes.
# That is a change to its contract and is left alone here.
#
# As of 2026-10-08 three drugs carry a nonzero oral lag, each as published:
# gabapentin (Tran 2017, 0.311 h), pregabalin (Chan 2021, 0.32 h) and
# acetaminophen (Morse 2022, 5.3 min), so this guard is live for them.  Before
# gabapentin none did: hydromorphone's intramuscular and intranasal lags were
# the last, removed when its absorption was refitted.
#
# Each engine builds one while it is computing recovery anyway, so carrying it
# out costs a cbind().
#
# WHICH CONCENTRATION IS TIMED
# ============================
# A drug with an effect site is timed on its effect site, and the states are
# the effect-site amplitudes.  A drug with NO effect site (ke0 = 0) is timed on
# its PLASMA, and the states are the plasma amplitudes -- including, after an
# extravascular dose, the absorption term, because drug in the depot has been
# given and keeps arriving after delivery stops.  Until 2026-10-07 such a drug
# carried all-zero effect-site states and so always read zero.  The case that
# matters is the antibiotics, whose threshold is the plotted concentration at
# which FREE drug equals the MIC (R/antibioticThresholds.R).  Every other drug
# without an effect site has a threshold of zero, which recoveryFromStates()
# reads as "no threshold".  A metabolite fold adds like to like: a receiving
# drug with no effect site has plasma states of its own, and its parent hands
# over the formed PLASMA contribution (advanceClosedFormMetabolite()).
# A plasma state set looks a week ahead rather than a day (its `horizon`;
# RECOVERY_HORIZON_PLASMA in R/recoveryCalc.R), because an antibiotic's time
# above the MIC commonly runs past a day -- or as far ahead as the plot runs,
# on a plot longer than a week (recoveryHorizonPlasma()), so that a drug whose
# half-life is measured in weeks reports a time rather than the cap.
# (Claude Code, 2026-10-07, at the request of Steven L. Shafer.)
# -----------------------------------------------------------------------------


#' Assemble a set of effect-site exponential states
#'
#' @param time the engine's time line
#' @param state a list or matrix of effect-site amplitudes, one column per
#'   eigenvalue
#' @param lambda the eigenvalues: a vector of length \code{ncol(state)}, or a
#'   matrix the same shape as \code{state} when the PK changes with time
#' @param pending optional logical, one per time, TRUE where a dose has been
#'   given but has not begun to be absorbed and the amplitudes therefore do not
#'   describe the whole of what the patient has received.  NULL, the default,
#'   means nothing is ever pending, which is the case for every intravenous
#'   dose and for any extravascular one with no lag.
#' @param horizon how far ahead, in minutes, to look for the threshold:
#'   \code{RECOVERY_HORIZON_EFFECT} (a day) for effect-site states,
#'   \code{recoveryHorizonPlasma(maximum)} (a week, or the length of the plot
#'   if that is longer) for plasma states
#'
#' @returns a state set: a list of \code{time}, \code{state}, \code{lambda},
#'   \code{pending} and \code{horizon}
#' @keywords internal
recoveryStateSet <- function(time, state, lambda, pending = NULL,
                             horizon = RECOVERY_HORIZON_EFFECT)
{
  state <- if (is.matrix(state)) state else do.call(cbind, state)
  stopifnot(nrow(state) == length(time))
  if (is.matrix(lambda)) {
    stopifnot(dim(lambda) == dim(state))
  } else {
    stopifnot(length(lambda) == ncol(state))
  }
  if (!is.null(pending))
  {
    stopifnot(is.logical(pending), length(pending) == length(time))
    # Nothing pending anywhere is the same as no mask, and dropping it here
    # keeps the common case from carrying a vector of FALSE around.
    if (!any(pending)) pending <- NULL
  }
  list(time = time, state = state, lambda = lambda, pending = pending,
       horizon = horizon)
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
#' 0.01 minutes and cannot reach any point outside it.  A metabolite fold's
#' time line keeps no point inside such an interval (metaboliteTimeLine() in
#' R/mergeMetabolite.R), so the fold never asks for one.
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

  horizon <- if (is.null(set$horizon)) RECOVERY_HORIZON_EFFECT else set$horizon
  if (identical(times, t0))
    return(list(time = times, state = S, lambda = Lam, pending = set$pending,
                horizon = horizon))

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

  # The mask is a step function that turns on when a dose is given and off when
  # it starts absorbing, and both of those instants are points on the engine's
  # own line, so the LEFT anchor's value holds across the interval.  That is
  # the opposite end from the one lambda is taken at, because they are
  # different kinds of quantity: lambda describes the step INTO hi, while
  # pending describes the state AT lo and onwards.
  list(time = times, state = out, lambda = lam,
       pending = if (is.null(set$pending)) NULL else set$pending[lo],
       horizon = horizon)
}


#' Time until threshold from a set of effect-site states
#'
#' @param set a state set from \code{recoveryStateSet()}
#' @param emerge the threshold the effect site has to fall to
#'
#' @returns minutes, one per row of \code{set$state}; zeros when there is no
#'   threshold to fall to (missing, or zero), and NA wherever
#'   \code{set$pending} says a dose has been given that has not begun to be
#'   absorbed
#' @keywords internal
recoveryFromStates <- function(set, emerge)
{
  nT <- nrow(set$state)
  # A threshold of zero is no threshold: a sum of decaying exponentials never
  # reaches it, and every drug without one -- the steroids, the reversal agents,
  # a prodrug's own row -- would otherwise read a full day at every point.
  if (is.null(emerge) || length(emerge) != 1 || is.na(emerge) || emerge <= 0)
    return(rep(0, nT))

  horizon <- if (is.null(set$horizon)) RECOVERY_HORIZON_EFFECT else set$horizon
  out <- recoveryRows(set$state, set$lambda, emerge, horizon)

  # Not computable rather than zero; see the header.
  if (!is.null(set$pending)) out[set$pending] <- NA_real_
  out
}


#' recoveryCalc() for every row of a state set
#'
#' What \code{recoveryCalc()} does one row at a time, with the work that is the
#' same for every row done once: when the eigenvalues do not change from row
#' to row -- every engine but advanceClosedForm1() -- the target is appended
#' as a column at rate zero, columns that are zero throughout (an unused
#' route's absorption state) are dropped, columns at equal rates are merged,
#' and the rest are put in order of rate, before the rows are solved.  Rows
#' whose eigenvalues differ go through \code{recoveryCalc()} itself.
#'
#' @param S the amplitudes, one row per time
#' @param lambda the eigenvalues: a vector, one per column, or a matrix the
#'   shape of \code{S}
#' @param target the threshold
#' @param horizon how far ahead to look, minutes
#'
#' @returns minutes, one per row of \code{S}
#' @keywords internal
recoveryRows <- function(S, lambda, target, horizon)
{
  nT <- nrow(S)
  if (nT == 0) return(numeric(0))
  if (is.matrix(lambda))
  {
    first <- lambda[1, ]
    if (nT > 0 && isTRUE(all(lambda == rep(first, each = nT)))) {
      lambda <- first
    } else {
      return(vapply(seq_len(nT),
                    function(i) recoveryCalc(S[i, ], lambda[i, ], target, horizon),
                    numeric(1)))
    }
  }

  used <- colSums(S != 0 | is.na(S)) > 0
  A  <- cbind(S[, used, drop = FALSE], -target)
  mu <- c(lambda[used], 0)
  if (!all(is.finite(mu)))
    return(vapply(seq_len(nT),
                  function(i) recoveryCalc(S[i, ], lambda, target, horizon),
                  numeric(1)))
  if (anyDuplicated(mu)) {
    A  <- t(rowsum(t(A), mu))               # columns grouped by increasing mu
    mu <- sort(unique(mu))
  } else {
    o  <- order(mu)
    A  <- A[, o, drop = FALSE]
    mu <- mu[o]
  }

  # A screen, before any row is solved.  Between two neighbouring points of a
  # coarse grid every positive term is at most its value at the left point and
  # every negative term at most its value at the right, so their sum bounds
  # C(t) - target from above over the whole interval.  A row whose bound is at
  # or below zero on every interval never rises above the target, and its
  # answer is zero without solving anything -- which is most rows of a run
  # that stays below its threshold, among them every formed metabolite that
  # never reaches its own.  The bound is rigorous, so the screen never clears
  # a row that does cross; a row it cannot clear is solved exactly.
  grid <- recoveryGrid(horizon)
  G    <- length(grid)
  Elo  <- exp(-outer(grid[-G], mu))
  Ehi  <- exp(-outer(grid[-1], mu))
  clear <- logical(nT)
  for (rows in split(seq_len(nT), (seq_len(nT) - 1) %/% 2048))
  {
    Ar  <- A[rows, , drop = FALSE]
    up  <- tcrossprod(pmax(Ar, 0), Elo) + tcrossprod(pmin(Ar, 0), Ehi)
    clear[rows] <- rowSums(up > 0 | is.na(up)) == 0
  }

  out <- numeric(nT)
  for (i in which(!clear))
  {
    a <- A[i, ]
    if (!all(is.finite(a))) { out[i] <- NA_real_; next }
    keep <- a != 0
    a <- a[keep]
    m <- mu[keep]
    out[i] <- recoveryFromTerms(a, m - m[1], horizon)
  }
  out
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

  # Any one contribution with a dose still to start makes the combined answer
  # incomplete, so the masks are OR'd: a pending parent dose leaves the
  # metabolite drug's row unable to report a time just as a pending dose of its
  # own would.
  pending <- Reduce(`|`, lapply(onto, function(x)
    if (is.null(x$pending)) logical(length(times)) else x$pending))
  if (!any(pending)) pending <- NULL

  # The receiving drug and what is formed into it are timed on the same
  # concentration (see "Which concentration is timed" above), so they share a
  # horizon; the longer is taken should they ever differ.
  recoveryFromStates(
    list(state   = do.call(cbind, lapply(onto, `[[`, "state")),
         lambda  = do.call(cbind, lapply(onto, `[[`, "lambda")),
         pending = pending,
         horizon = max(vapply(onto, `[[`, numeric(1), "horizon"))),
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


#' Times at which a dose has been given but has not begun to be absorbed
#'
#' An extravascular dose may carry an absorption lag, and the engines apply it
#' by moving the dose later.  Over the interval between the two the drug is in
#' the patient and nothing in the model represents it, so recovery cannot be
#' computed and must not be reported as zero.  See R/recoveryStates.R.
#'
#' @param given the dose times as the user entered them
#' @param started the same times after their lags have been added
#' @param amount the doses, so that a zero-dose row -- which some callers add
#'   purely to put a point on the time line -- does not blank anything
#' @param times the time line to report on
#'
#' @returns logical, one per element of \code{times}; NULL when no dose carries
#'   a lag at all, which is every intravenous case and the normal
#'   extravascular one
#' @keywords internal
pendingDoseTimes <- function(given, started, amount, times)
{
  lagged <- which(started > given & amount != 0)
  if (length(lagged) == 0) return(NULL)

  # Half open: pending from the instant it is given up to, but not including,
  # the instant it starts.  At the start instant the state exists and recovery
  # is computable again.
  out <- logical(length(times))
  for (i in lagged)
    out <- out | (times >= given[i] & times < started[i])
  if (!any(out)) NULL else out
}
