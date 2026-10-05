# Tests for recoveryCalc() (R/recoveryCalc.R).
#
# recoveryCalc(state, lambda, target) returns the time (minutes, searched over
# [0, 1440]) for the multi-exponential decay sum(state * exp(-lambda * t)) to
# fall to `target`, using stats::optimize on the squared error.
#
# KNOWN LIMITATION (documented, not asserted): when every exponential has
# decayed below floating-point resolution over most of the [0, 1440] search
# interval (e.g. state = 1, lambda = 0.1, target = 0.5, true crossing at
# t = ln(2)/0.1 = 6.93 min), the squared-error surface is numerically flat on
# the right and optimize() can converge to the right edge (~1440) instead of
# the true crossing. Realistic 3-exponential PK states keep a slow terminal
# lambda, which preserves slope and avoids this; tests below use such shapes.
# Reported on the PK/PD-engine test-plan issue.
#
# Provenance: drafted by Claude Code (Opus 4.8), 2026-08-13, for the
# pre-deployment test plan (PK/PD engine). All expected values verified
# against a direct closed-form / reconstruction computation.

test_that("returns 0 when the target is already at or above the current level", {
  # target >= sum(state) means recovery is complete now
  expect_equal(recoveryCalc(c(0.3), c(0.1), 0.5), 0)
  expect_equal(recoveryCalc(c(1, 1, 1), c(0.5, 0.1, 0.02), 3), 0)
  expect_equal(recoveryCalc(c(1), c(0.1), 1), 0)   # boundary: equal
})

test_that("single-exponential decay matches the closed form t = ln(C0/target)/lambda", {
  # Slow lambdas keep the error surface numerically discriminable across the
  # search interval (see KNOWN LIMITATION above). tol = 0.1 in optimize()
  # limits precision, so compare with a loose absolute tolerance.
  for (lam in c(0.005, 0.01, 0.02, 0.05)) {
    expected <- log(1 / 0.5) / lam       # C0 = 1 decaying to 0.5
    actual <- recoveryCalc(c(1), c(lam), 0.5)
    expect_equal(actual, expected, tolerance = 0.05)
  }
})

test_that("multi-exponential recovery time reproduces the target concentration", {
  # A realistic 3-exponential effect-site state: fast, intermediate, and slow
  # phases. Rather than pin a time, assert the defining property: evaluating
  # the decay at the returned time recovers the target.
  state <- c(2, 1, 0.5)
  lambda <- c(0.5, 0.1, 0.02)
  for (target in c(2.0, 1.0, 0.25)) {
    t_rec <- recoveryCalc(state, lambda, target)
    expect_gt(t_rec, 0)
    reconstructed <- sum(state * exp(-lambda * t_rec))
    expect_equal(reconstructed, target, tolerance = 0.01)
  }
})

test_that("recovery times are monotone: lower targets take longer", {
  state <- c(2, 1, 0.5)
  lambda <- c(0.5, 0.1, 0.02)
  t_high <- recoveryCalc(state, lambda, 2.0)
  t_mid  <- recoveryCalc(state, lambda, 1.0)
  t_low  <- recoveryCalc(state, lambda, 0.25)
  expect_true(t_high < t_mid && t_mid < t_low)
})

test_that("unreachable target (0) saturates at the 1440-minute search bound", {
  # sum(state * exp(-lambda*t)) never reaches exactly 0, so the minimizer
  # runs to the end of the search interval. Pin the saturation behavior.
  t_rec <- recoveryCalc(c(1), c(0.01), 0)
  expect_gt(t_rec, 1400)
  expect_lte(t_rec, 1440)
})
