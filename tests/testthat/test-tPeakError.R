# ---------------------------------------------------------------------------
# test-tPeakError.R
#
# WHAT THIS FILE TESTS
# --------------------
# R/tPeakError.R holds the one-dimensional objective function that R/getDrugPK.R
# hands to stats::optimize() in order to fit ke0 -- the plasma <-> effect-site
# equilibration rate constant -- from a drug's published time to peak effect
# (tPeak).  It has had no direct test.
#
# For a candidate ke0 (passed positionally as `lambda_4`), tPeakError():
#   1. turns the unit-bolus plasma coefficients p_i into effect-site
#      coefficients  e_i = p_i * ke0 / (ke0 - lambda_i)  for the disposition
#      eigenvalues, plus e_4 = -(e_1 + e_2 + e_3) for the ke0 exponential
#      (lambda_2 / lambda_3 of zero mark 2- and 1-compartment models and zero out
#      the corresponding term),
#   2. evaluates the resulting 4-exponential Ce(t) through CE() (R/CE.R),
#   3. locates the maximum of that curve with an inner stats::optimize() over
#      t in [0, 100] minutes,
#   4. returns (tPeak - predictedPeak)^2.
#
# Everything asserted below is derived from the link-model algebra rather than
# from numbers the code printed.  For a unit bolus into a mammillary model,
#
#   Cp(t) = sum_i p_i exp(-lambda_i t)
#   Ce(t) = ke0 * integral_0^t Cp(s) exp(-ke0 (t - s)) ds
#         = sum_i [p_i ke0 / (ke0 - lambda_i)] exp(-lambda_i t)
#           - [sum_i p_i ke0 / (ke0 - lambda_i)] exp(-ke0 t)
#
# and, because the effect site obeys dCe/dt = ke0 (Cp - Ce), the effect-site
# peak is exactly the instant at which Ce(t) crosses Cp(t).  That crossing is
# used throughout as an INDEPENDENT locator of the peak: it is found with
# uniroot() and never calls tPeakError() or its inner optimize().  The first
# test block anchors the whole chain by checking the closed-form Ce against
# numerical integration of the convolution integral above, so the coefficient
# algebra reused by the local helpers is itself verified rather than assumed.
#
# KNOWN LIMITATIONS (deliberately not asserted as though they were correct)
# ------------------------------------------------------------------------
# * 100-minute inner search window.  The inner optimize() searches only
#   t in [0, 100], so a requested tPeak above 100 min can never be matched: the
#   objective floors at (tPeak - 100)^2 and the returned ke0 is meaningless.
#   The block "requested tPeak beyond the inner search window" pins that as a
#   quirk -- widening the window in tPeakError.R should deliberately update it.
#   Every shipped drug is inside the window, but morphine (tPeak = 93.8 min) sits
#   only ~6 min short of the ceiling, so a guard assertion is included.
# * Pole at ke0 == lambda_i.  e_i = p_i ke0 / (ke0 - lambda_i) is infinite when
#   the candidate ke0 exactly equals a disposition eigenvalue; Inf - Inf then
#   makes CE() return NaN, R emits "NA/NaN replaced by maximum positive value"
#   warnings, and the objective jumps to its worst attainable value even though
#   the mathematical limit is perfectly well behaved (the coincident exponential
#   pair degenerates to a t * exp(-lambda_i t) term).  optimize() will
#   essentially never land on the pole exactly, so this file does not exercise
#   the broken path.  It asserts instead the true property: the objective stays
#   continuous, finite and small as the pole is approached, and it matches the
#   analytic limiting peak time.
#
# Provenance: drafted by Claude Code (Fable 5), 2026-08-14, for the
# pre-deployment test plan (ke0 fitting / tPeakError).  Expected values derived
# from the closed-form link-model algebra shown above, from numerical
# integration of the effect-site convolution integral, and from an independent
# uniroot() search for the Cp(t) = Ce(t) crossing.  Run and verified against the
# working tree; no expected value was pasted back from the code under test.
# ---------------------------------------------------------------------------


# --- local helpers ---------------------------------------------------------
# Prefixed tpe_ so they cannot collide with package objects or with helpers in
# sibling test files.

# Unit-bolus plasma concentration, sum of exponentials.  Terms whose eigenvalue
# is zero are absent from real models (getDrugPK passes p_i = 0 with them), so a
# plain sum is correct.
tpe_Cp <- function(t, p1, p2, p3, lambda_1, lambda_2, lambda_3) {
  p1 * exp(-lambda_1 * t) + p2 * exp(-lambda_2 * t) + p3 * exp(-lambda_3 * t)
}

# Effect-site coefficients from the convolution solution quoted in the header.
# This mirrors the algebra inside tPeakError(); the first test block proves the
# algebra is right by comparing the resulting curve with numerical integration.
tpe_eCoefs <- function(ke0, p1, p2, p3, lambda_1, lambda_2, lambda_3) {
  e1 <- p1 / (ke0 - lambda_1) * ke0
  e2 <- if (lambda_2 > 0) p2 / (ke0 - lambda_2) * ke0 else 0
  e3 <- if (lambda_3 > 0) p3 / (ke0 - lambda_3) * ke0 else 0
  c(e1, e2, e3, -e1 - e2 - e3)
}

# Independent peak locator.  dCe/dt = ke0 (Cp - Ce), so the peak is the first
# t > 0 where Ce catches up with Cp.  A log-spaced coarse scan brackets the sign
# change, then uniroot() polishes it to ~1e-13 min -- far tighter than the
# default-tolerance optimize() inside tPeakError(), which is the point.
tpe_peakTime <- function(ke0, p1, p2, p3, lambda_1, lambda_2, lambda_3,
                         tmax = 1000) {
  e <- tpe_eCoefs(ke0, p1, p2, p3, lambda_1, lambda_2, lambda_3)
  gap <- function(t) {
    tpe_Cp(t, p1, p2, p3, lambda_1, lambda_2, lambda_3) -
      CE(t, e[1], e[2], e[3], e[4], lambda_1, lambda_2, lambda_3, ke0)
  }
  grid <- exp(seq(log(1e-7), log(tmax), length.out = 8000))
  d <- gap(grid)
  # Cp starts above Ce (Ce is zero at t = 0) and ends below it, so we want the
  # first downward crossing of the gap.
  i <- which(d[-1] < 0 & d[-length(d)] > 0)[1]
  if (is.na(i)) return(NA_real_)
  stats::uniroot(gap, c(grid[i], grid[i + 1]), tol = 1e-13)$root
}

# Tolerance budget.  stats::optimize() defaults to tol = .Machine$double.eps^0.25
# (about 1.22e-4) and that tolerance is absolute in t, so tPeakError()'s inner
# peak search can be off by ~1e-4 min.  Squaring propagates that as
# |d objective| ~ 2 |tPeak - tstar| * dt, so comparisons use that propagated
# bound with a safety factor of roughly four, plus a small absolute floor for the
# case where tPeak and the peak coincide.
tpe_tol <- function(tPeak, peak) 1e-3 * abs(tPeak - peak) + 1e-8

# Standard reference patient used for every getDrugPK() call in this file.
tpe_wt  <- 70
tpe_ht  <- 170
tpe_age <- 50
tpe_sex <- "male"

# Disposition of the propofol model at that reference patient, reused as the
# workhorse 3-compartment coefficient set.  Only the disposition half of
# getDrugPK() feeds tPeakError(), so nothing here depends on the fitted ke0.
tpe_propofol <- getDrugPK(
  "propofol", tpe_wt, tpe_ht, tpe_age, tpe_sex, getDrugDefaults("propofol")
)$PK$default


# --- 1. anchor: the effect-site closed form really is the convolution --------
# Without this block every later expectation would lean on coefficient algebra
# that merely restates the code under test.  Here the closed-form Ce produced by
# CE() is compared against stats::integrate() of
#   Ce(t) = ke0 * int_0^t Cp(s) exp(-ke0 (t - s)) ds,
# which is the defining first-order effect-site equation and shares no code with
# the package.
test_that("CE() reproduces the effect-site convolution integral", {
  p <- tpe_propofol
  for (ke0 in c(0.2, p$ke0, 3)) {
    e <- tpe_eCoefs(
      ke0,
      p$p_coef_bolus_l1, p$p_coef_bolus_l2, p$p_coef_bolus_l3,
      p$lambda_1, p$lambda_2, p$lambda_3
    )
    for (t in c(0.25, 1, 1.6, 5, 30)) {
      closedForm <- CE(
        t, e[1], e[2], e[3], e[4],
        p$lambda_1, p$lambda_2, p$lambda_3, ke0
      )
      numeric <- ke0 * stats::integrate(
        function(s) {
          tpe_Cp(
            s,
            p$p_coef_bolus_l1, p$p_coef_bolus_l2, p$p_coef_bolus_l3,
            p$lambda_1, p$lambda_2, p$lambda_3
          ) * exp(-ke0 * (t - s))
        },
        lower = 0, upper = t, rel.tol = 1e-12
      )$value
      # Both sides are analytic-quality here, so 1e-9 relative is not generous.
      expect_equal(closedForm, numeric, tolerance = 1e-9)
    }
  }
})


# --- 2. the objective really is the squared miss on the peak time -----------
# This is the definitional property of tPeakError().  The "expected" side comes
# from the independent Cp = Ce crossing, so a regression in the coefficient
# algebra, in CE(), or in the inner search would all break it.
test_that("tPeakError equals (tPeak - true effect-site peak)^2", {
  p <- tpe_propofol
  for (tPeak in c(1.6, 5)) {
    for (ke0 in c(0.05, 0.1, 0.2, 0.4, p$ke0, 1, 2, 5, 10)) {
      observed <- tPeakError(
        ke0, tPeak,
        p$p_coef_bolus_l1, p$p_coef_bolus_l2, p$p_coef_bolus_l3,
        p$lambda_1, p$lambda_2, p$lambda_3
      )
      peak <- tpe_peakTime(
        ke0,
        p$p_coef_bolus_l1, p$p_coef_bolus_l2, p$p_coef_bolus_l3,
        p$lambda_1, p$lambda_2, p$lambda_3
      )
      expect_false(is.na(peak))
      expect_equal(
        observed, (tPeak - peak)^2,
        tolerance = tpe_tol(tPeak, peak),
        label = paste0("tPeakError(ke0=", ke0, ", tPeak=", tPeak, ")")
      )
    }
  }
})


# --- 3. the objective is (numerically) zero at the fitted ke0 ---------------
# getDrugPK() minimises this same function, so its answer must sit at the bottom
# of the bowl.  The floor is set by optimize()'s default tolerance in BOTH loops,
# not by the algebra: the inner search locates the peak to ~1e-4 min and squaring
# that gives ~1e-8.  A generous 1e-6 keeps the assertion honest without pinning
# platform-level floating point noise.
test_that("the objective vanishes at the ke0 that getDrugPK fitted", {
  for (drug in c("propofol", "fentanyl", "remifentanil")) {
    pk <- getDrugPK(
      drug, tpe_wt, tpe_ht, tpe_age, tpe_sex, getDrugDefaults(drug)
    )
    p <- pk$PK$default
    expect_gt(p$ke0, 0)
    residual <- tPeakError(
      p$ke0, pk$tPeak,
      p$p_coef_bolus_l1, p$p_coef_bolus_l2, p$p_coef_bolus_l3,
      p$lambda_1, p$lambda_2, p$lambda_3
    )
    expect_lt(residual, 1e-6)
    # Squared minutes -> the peak lands within 1 ms of the requested tPeak.
    expect_lt(sqrt(residual), 1e-3)
  }
})


# --- 4. non-negativity and finiteness over a wide sweep ---------------------
# It is a squared error, so it can never be negative; and getDrugPK() lets
# optimize() roam over ke0 in [0, 200], so the whole of that interval must return
# a usable number (no NA, NaN or Inf) or the fit could fail unpredictably.
test_that("the objective is finite and non-negative across the search interval", {
  p <- tpe_propofol
  candidates <- c(
    0,                                  # lower end of getDrugPK's interval
    10^seq(-6, -2, length.out = 5),
    seq(0.01, 2, length.out = 25),
    5, 20, 100, 200                     # upper end of getDrugPK's interval
  )
  values <- vapply(
    candidates,
    function(ke0) {
      tPeakError(
        ke0, 1.6,
        p$p_coef_bolus_l1, p$p_coef_bolus_l2, p$p_coef_bolus_l3,
        p$lambda_1, p$lambda_2, p$lambda_3
      )
    },
    numeric(1)
  )
  expect_true(all(is.finite(values)))
  expect_true(all(values >= 0))
  # A squared error with an exact solution somewhere inside the interval must
  # come close to zero on a grid this dense around the optimum.
  expect_lt(min(values), 1e-3)
})


# --- 5. bowl shape around the optimum ---------------------------------------
# The fit is a plain golden-section / parabolic search, which is only guaranteed
# to find the global minimum if the objective is unimodal.  It is: the peak time
# falls monotonically with ke0 (block 6), so (tPeak - peak)^2 falls until the
# peak crosses tPeak and rises afterwards.  Assert exactly that -- strict
# monotone descent on the left of the fitted ke0 and strict ascent on the right,
# with the grid minimum at the fitted value itself.
test_that("the objective is a strictly unimodal bowl around the fitted ke0", {
  p <- tpe_propofol
  multipliers <- c(0.25, 0.35, 0.5, 0.65, 0.8, 0.9, 0.96,
                   1,
                   1.04, 1.1, 1.25, 1.5, 2, 3, 5, 10)
  centre <- which(multipliers == 1)
  values <- vapply(
    p$ke0 * multipliers,
    function(ke0) {
      tPeakError(
        ke0, 1.6,
        p$p_coef_bolus_l1, p$p_coef_bolus_l2, p$p_coef_bolus_l3,
        p$lambda_1, p$lambda_2, p$lambda_3
      )
    },
    numeric(1)
  )
  expect_equal(which.min(values), centre)
  expect_true(all(diff(values[seq_len(centre)]) < 0))
  expect_true(all(diff(values[centre:length(values)]) > 0))
})


# --- 6. faster equilibration means an earlier peak --------------------------
# This is the physical monotonicity the whole fit depends on: tPeak identifies
# ke0 only because the map ke0 -> peak time is strictly decreasing.  Checked
# with the independent crossing locator, i.e. without going through tPeakError()
# at all.
test_that("a larger ke0 always produces an earlier effect-site peak", {
  p <- tpe_propofol
  ke0Grid <- c(0.02, 0.05, 0.1, 0.2, 0.4, 0.8, 1.6, 3.2, 6.4, 12.8)
  peaks <- vapply(
    ke0Grid,
    function(ke0) {
      tpe_peakTime(
        ke0,
        p$p_coef_bolus_l1, p$p_coef_bolus_l2, p$p_coef_bolus_l3,
        p$lambda_1, p$lambda_2, p$lambda_3
      )
    },
    numeric(1)
  )
  expect_true(all(is.finite(peaks)))
  expect_true(all(peaks > 0))
  expect_true(all(diff(peaks) < 0))
  # Sanity on the physics as well as the ordering: an instantaneous effect site
  # peaks with the plasma (t -> 0 after a bolus), a very slow one lags badly.
  expect_lt(peaks[length(peaks)], 0.5)
  expect_gt(peaks[1], 5)
})


# --- 7. round trip through the fit getDrugPK actually performs --------------
# Run the very same stats::optimize() call as R/getDrugPK.R (same objective,
# same [0, 200] interval, same default tolerance) for a range of requested
# tPeak values, then confirm with the independent locator that the fitted ke0
# really does put the peak where it was asked to go.
test_that("optimize() over tPeakError recovers the requested tPeak", {
  p <- tpe_propofol
  for (tPeak in c(1.0, 1.6, 3.0, 5.0, 10.0)) {
    ke0 <- stats::optimize(
      tPeakError, c(0, 200), tPeak,
      p$p_coef_bolus_l1, p$p_coef_bolus_l2, p$p_coef_bolus_l3,
      p$lambda_1, p$lambda_2, p$lambda_3
    )$minimum
    expect_gt(ke0, 0)
    peak <- tpe_peakTime(
      ke0,
      p$p_coef_bolus_l1, p$p_coef_bolus_l2, p$p_coef_bolus_l3,
      p$lambda_1, p$lambda_2, p$lambda_3
    )
    # 0.2% relative is the accuracy the doubly nested default-tolerance
    # optimize() calls can deliver; the observed worst case here is ~0.1%.
    expect_equal(
      peak, tPeak,
      tolerance = 2e-3,
      label = paste0("peak time fitted for tPeak = ", tPeak)
    )
  }
})


# --- 8. one-compartment degenerate branch (lambda_2 = lambda_3 = 0) ---------
# With a single disposition exponential the effect-site curve collapses to
#   Ce(t) = p1 ke0/(ke0 - lambda_1) * (exp(-lambda_1 t) - exp(-ke0 t))
# whose maximum has the textbook closed form  t* = ln(ke0 / lambda_1) / (ke0 - lambda_1),
# symmetric in ke0 and lambda_1.  That gives a completely independent expected
# value with no numerics at all.
test_that("the 1-compartment branch matches the closed-form peak time", {
  lambda_1 <- cube(0.05, 0, 0, 0, 0)[1]   # one-compartment model: lambda_1 = k10
  expect_equal(lambda_1, 0.05)
  p1 <- 1 / 10                            # unit bolus into V1 = 10 L
  for (tPeak in c(2, 8, 20)) {
    for (ke0 in c(0.01, 0.1, 0.5, 2, 10)) {
      observed <- tPeakError(ke0, tPeak, p1, 0, 0, lambda_1, 0, 0)
      analytic <- log(ke0 / lambda_1) / (ke0 - lambda_1)
      expect_equal(
        observed, (tPeak - analytic)^2,
        tolerance = tpe_tol(tPeak, analytic),
        label = paste0("1-cmt tPeakError(ke0=", ke0, ", tPeak=", tPeak, ")")
      )
    }
  }
  # ...and the fit inverts that closed form: solve t* = tPeak for ke0 with
  # uniroot and compare against what optimize() over tPeakError returns.
  for (tPeak in c(2, 8)) {
    fitted <- stats::optimize(
      tPeakError, c(0, 200), tPeak, p1, 0, 0, lambda_1, 0, 0
    )$minimum
    exact <- stats::uniroot(
      function(ke0) log(ke0 / lambda_1) / (ke0 - lambda_1) - tPeak,
      c(lambda_1 * 1.000001, 200), tol = 1e-12
    )$root
    expect_equal(
      fitted, exact,
      tolerance = 1e-3,
      label = paste0("1-cmt fitted ke0 for tPeak = ", tPeak)
    )
  }
})


# --- 8b. behaviour approaching the ke0 == lambda_1 pole ---------------------
# See KNOWN LIMITATIONS.  The coefficients blow up at the pole, but the curve
# does not: as ke0 -> lambda_1 the pair of exponentials degenerates to
#   Ce(t) -> p1 lambda_1 t exp(-lambda_1 t),
# which peaks at exactly t = 1 / lambda_1.  Evaluate just off the pole and check
# the objective agrees with that limit.  The exact pole is avoided on purpose:
# there the code returns NaN-driven garbage plus warnings.
test_that("the objective stays continuous approaching the ke0 == lambda_1 pole", {
  lambda_1 <- 0.05
  p1 <- 1 / 10
  limitPeak <- 1 / lambda_1              # = 20 min, from t exp(-lambda_1 t)
  nearPole <- lambda_1 * (1 + 1e-5)
  # Asking for the limiting peak time should be an essentially exact hit...
  expect_lt(tPeakError(nearPole, limitPeak, p1, 0, 0, lambda_1, 0, 0), 1e-6)
  # ...and asking for anything else should miss by exactly the algebraic amount.
  for (tPeak in c(15, 25)) {
    expect_equal(
      tPeakError(nearPole, tPeak, p1, 0, 0, lambda_1, 0, 0),
      (tPeak - limitPeak)^2,
      tolerance = 1e-2,
      label = paste0("near-pole objective at tPeak = ", tPeak)
    )
  }
})


# --- 9. two-compartment degenerate branch (lambda_3 = 0) --------------------
# Five shipped drugs (lidocaine, rocuronium, oxytocin, oxycodone, oliceridine)
# are 2-compartment, so this branch is live in production.  Coefficients are
# built here from the standard 2-compartment bolus solution
#   A = (k21 - lambda_1) / (lambda_2 - lambda_1) / V1,
#   B = (k21 - lambda_2) / (lambda_1 - lambda_2) / V1,
# whose sum must be 1/V1 (Cp(0) = dose / V1) -- a cheap independent check that
# the eigenvalues from cube() and the coefficients agree.
test_that("the 2-compartment branch produces a finite objective and a sane fit", {
  k10 <- 0.05
  k12 <- 0.10
  k21 <- 0.03
  v1  <- 10
  roots <- cube(k10, k12, 0, k21, 0)
  lambda_1 <- roots[1]
  lambda_2 <- roots[2]
  lambda_3 <- roots[3]
  expect_equal(lambda_3, 0)
  p1 <- (k21 - lambda_1) / (lambda_2 - lambda_1) / v1
  p2 <- (k21 - lambda_2) / (lambda_1 - lambda_2) / v1
  expect_equal(p1 + p2, 1 / v1)          # Cp(0) = 1 / V1 for a unit bolus

  for (tPeak in c(1.5, 4, 12)) {
    ke0 <- stats::optimize(
      tPeakError, c(0, 200), tPeak, p1, p2, 0, lambda_1, lambda_2, lambda_3
    )$minimum
    expect_gt(ke0, 0)
    residual <- tPeakError(ke0, tPeak, p1, p2, 0, lambda_1, lambda_2, lambda_3)
    expect_true(is.finite(residual))
    expect_lt(residual, 1e-4)
    peak <- tpe_peakTime(ke0, p1, p2, 0, lambda_1, lambda_2, lambda_3)
    expect_equal(
      peak, tPeak,
      tolerance = 2e-3,
      label = paste0("2-cmt peak time fitted for tPeak = ", tPeak)
    )
  }
})


# --- 10. every drug in the library --------------------------------------
# The end-to-end contract: for each drug, the ke0 that getDrugPK() fitted must
# put the effect-site peak at the tPeak that drug's model asked for.  The peak is
# located independently (Cp = Ce crossing), so this exercises cube(), the bolus
# coefficient algebra, tPeakError() and the optimize() wrapper together.
#
# tPeak == 0 would take getDrugPK's ke0 = 0 short circuit, which never calls
# tPeakError() and leaves the effect site undefined; no drug currently does that,
# and the loop below skips (and counts) any that appear so a future zero-tPeak
# drug does not silently produce a bogus expectation.
test_that("every drug's fitted ke0 reproduces its published tPeak", {
  drugs <- getDrugDefaultsGlobal()$Drug
  expect_gt(length(drugs), 0)
  skipped <- character(0)

  for (drug in drugs) {
    pk <- getDrugPK(
      drug, tpe_wt, tpe_ht, tpe_age, tpe_sex, getDrugDefaults(drug)
    )
    p <- pk$PK$default

    if (pk$tPeak == 0) {
      # Documented short circuit: no effect-site model is fitted at all.
      expect_equal(p$ke0, 0)
      skipped <- c(skipped, drug)
      next
    }

    expect_gt(p$ke0, 0)
    peak <- tpe_peakTime(
      p$ke0,
      p$p_coef_bolus_l1, p$p_coef_bolus_l2, p$p_coef_bolus_l3,
      p$lambda_1, p$lambda_2, p$lambda_3
    )
    expect_false(is.na(peak))
    # 0.2% relative.  The worst shipped case is morphine at ~0.12%, whose slow
    # ke0 (~0.005 /min) is comparable with optimize()'s absolute default
    # tolerance of 1.22e-4, so its fitted ke0 is the least precisely resolved.
    expect_equal(
      peak, pk$tPeak,
      tolerance = 2e-3,
      label = paste0(drug, ": effect-site peak time")
    )
  }

  # As of this writing no drug takes the ke0 = 0 branch; if one ever does, this
  # expectation fails loudly and the branch above needs a real test of its own.
  expect_equal(length(skipped), 0)
})


# --- 11. pinned quirk: the 100-minute inner search window -------------------
# PINNED QUIRK.  tPeakError() searches for the peak only over t in [0, 100] min,
# so a requested tPeak above 100 cannot be matched and optimize() converges to a
# meaningless ke0 with a large residual instead of signalling anything.  These
# expectations pin today's behaviour; if the window in R/tPeakError.R is ever
# widened (or the failure is made explicit), this block should be updated
# deliberately rather than patched to stay green.
test_that("a requested tPeak beyond the inner search window silently fails", {
  p <- tpe_propofol
  for (tPeak in c(120, 150)) {
    ke0 <- stats::optimize(
      tPeakError, c(0, 200), tPeak,
      p$p_coef_bolus_l1, p$p_coef_bolus_l2, p$p_coef_bolus_l3,
      p$lambda_1, p$lambda_2, p$lambda_3
    )$minimum
    residual <- tPeakError(
      ke0, tPeak,
      p$p_coef_bolus_l1, p$p_coef_bolus_l2, p$p_coef_bolus_l3,
      p$lambda_1, p$lambda_2, p$lambda_3
    )
    # The best the search can do is peak at the window edge, t = 100 min.
    expect_equal(
      sqrt(residual), tPeak - 100,
      tolerance = 1e-3,
      label = paste0("clipped residual at tPeak = ", tPeak)
    )
  }
  # Guard for the drug library: every shipped tPeak must stay inside the window
  # with room to spare.  Morphine is the closest at 93.8 min.
  drugs <- getDrugDefaultsGlobal()$Drug
  tPeaks <- vapply(
    drugs,
    function(drug) {
      getDrugPK(
        drug, tpe_wt, tpe_ht, tpe_age, tpe_sex, getDrugDefaults(drug)
      )$tPeak
    },
    numeric(1)
  )
  expect_true(all(tPeaks < 100))
})
