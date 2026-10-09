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


# --- Added 2026-10-05 with the rewrite of recoveryCalc() (Claude Code, Claude
# Fable 5.1).  The limitation documented at the top of this file no longer
# applies: the crossing is bracketed on a grid and refined with uniroot(), so a
# short-acting single exponential is found correctly.

test_that("a fast single exponential is found, not lost at the far end of the search", {
  # The case the old optimize()-based search could return ~1440 for.
  expect_equal(recoveryCalc(1, 0.1, 0.5), log(2) / 0.1, tolerance = 1e-3)
  expect_equal(recoveryCalc(5, 2, 0.01), log(500) / 2, tolerance = 1e-3)
})

test_that("a concentration still rising towards a peak above the target is not reported as zero", {
  # Effect site just after a bolus: nothing there yet, but plasma will fill it.
  # C(t) = exp(-0.05 t) - exp(-0.5 t): zero now, peaks near 0.70 at t = 5.1.
  state <- c(1, -1); lambda <- c(0.05, 0.5)
  C <- function(t) sum(state * exp(-lambda * t))
  expect_equal(C(0), 0)
  t_rec <- recoveryCalc(state, lambda, 0.3)
  expect_gt(t_rec, 5)                       # after the peak, not before it
  expect_equal(C(t_rec), 0.3, tolerance = 1e-3)
  expect_lt(C(t_rec + 1), 0.3)              # and falling through it

  # If the peak never reaches the target, there is nothing to wait for.
  expect_equal(recoveryCalc(state, lambda, 0.9), 0)
})

test_that("the search is bounded at a day", {
  # Still above the target after a day.
  expect_equal(recoveryCalc(1, 1e-6, 0.5), MINS_PER_DAY)
  # A target of zero is never reached, so it saturates too.
  expect_equal(recoveryCalc(c(2, 1), c(0.1, 0.01), 0), MINS_PER_DAY)
})


# --- Added 2026-10-09 with the exact search (Claude Code, at the request of
# Steven L. Shafer; external audit finding F09).  Until then the crossing was
# bracketed on a fixed grid, and a rise above the target that began and ended
# between two grid points was missed.  The search now finds every stationary
# point first, so no excursion is missed however brief; see the header of
# R/recoveryCalc.R.

# The grid the search used until 2026-10-09: 0, then 0.05 min to a day in 89
# equal ratios of about 1.122.  Its points near a minute are 0.895, 1.0045 and
# 1.127.
oldRecoveryGrid <- c(0, exp(seq(log(0.05), log(MINS_PER_DAY), length.out = 90)))

# Within an absolute tolerance, in minutes: expect_equal()'s is relative.
expectWithin <- function(actual, expected, within, label = NULL)
  expect_lt(max(abs(actual - expected)), within, label = label)

test_that("a brief excursion above the threshold between grid points is found (F09)", {
  # The audit's case: 70 kg, 170 cm, 40-year-old man, propofol 16.9985840174
  # mg.  The effect site peaks at 1.0000050162 mcg/mL at 1.60 min and is above
  # a 1 mcg/mL threshold for 0.62 s, below it at every point of the old grid.
  # The audit's independent solution (amount-matrix propagation, bounded
  # maximum search and a Brent root) puts the downward crossing at
  # 1.6051526509 min; the old search returned 0.
  PK <- getDrugPK("propofol", 70, 170, 40, "male", getDrugDefaults("propofol"))
  PK$endCe <- 1
  dose <- data.frame(Drug = "propofol", Time = 0, Dose = 16.9985840174,
                     Units = "mg")
  noEvents <- data.frame(Time = numeric(0), Event = character(0))
  X <- simCpCe(dose, noEvents, PK, 60, TRUE)

  # Through the app's own path: the Recovery column at the bolus.
  expectWithin(X$wide$Recovery[X$wide$Time == 0], 1.6051526509, RECOVERY_TOL)

  # And directly, with the evidence that the old grid could not see it.
  s <- X$recoveryStates
  C <- function(t) sum(s$state[1, ] * exp(-s$lambda * t))
  peak <- optimize(C, c(1, 2), maximum = TRUE, tol = 1e-10)
  expect_gt(peak$objective, 1)
  expect_lt(peak$objective, 1 + 1e-5)
  expect_true(all(vapply(oldRecoveryGrid, C, numeric(1)) < 1))
  t1 <- recoveryCalc(s$state[1, ], s$lambda, 1)
  expectWithin(t1, 1.6051526509, RECOVERY_TOL)
  expect_gt(C(t1 - 1e-5), 1)
  expect_lt(C(t1 + 1e-5), 1)
})

test_that("an excursion wholly between two grid points is found exactly", {
  # With x = exp(-t), -(x - r1)(x - r2) is positive only for x between r2 and
  # r1.  Expanded it is a sum of exponentials, C(t) - target with
  #     C(t) = -exp(-2 t) + (r1 + r2) exp(-t),   target = r1 r2,
  # above the target only for t between 1.01 and 1.06 -- three seconds, both
  # ends between the old grid's 1.0045 and 1.127.
  r <- exp(-c(1.01, 1.06))
  state <- c(-1, sum(r)); lambda <- c(2, 1); target <- prod(r)
  C <- function(t) sum(state * exp(-lambda * t))
  expect_true(all(vapply(oldRecoveryGrid, C, numeric(1)) < target))
  expectWithin(recoveryCalc(state, lambda, target), 1.06, RECOVERY_TOL)

  # Every sign change is found, here two inside one old grid step and a
  # third: (x - r1)(x - r2)(x - r3) has zeros at t = 1.01, 1.06 and 3.
  r <- exp(-c(1.01, 1.06, 3))
  a <- c(1, -sum(r), r[1] * r[2] + r[1] * r[3] + r[2] * r[3], -prod(r))
  h <- expSumTerms(a, c(3, 2, 1, 0))
  zeros <- expSumZeros(h$a, h$mu, 0, 10)
  expect_length(zeros, 3)
  expectWithin(zeros, c(1.01, 1.06, 3), RECOVERY_TOL)
})

test_that("the search agrees with a dense brute-force reference on random sums", {
  # Random sums of two to six exponentials, effect-site shaped half the time
  # (amplitudes summing to zero), each against a threshold between 5% and 100%
  # of its own maximum.  The reference samples 2e5 log-spaced and 2e5 evenly
  # spaced times and refines its last sign change with uniroot().
  reference <- function(state, lambda, target, horizon = MINS_PER_DAY) {
    g <- sort(unique(c(seq(0, horizon, length.out = 2e5),
                       exp(seq(log(1e-4), log(horizon), length.out = 2e5)))))
    v <- as.vector(exp(-outer(g, lambda)) %*% state) - target
    if (v[length(v)] > 0) return(horizon)
    above <- which(v > 0)
    if (length(above) == 0) return(0)
    i <- above[length(above)]
    if (v[i + 1] == 0) return(g[i + 1])
    stats::uniroot(function(t) sum(state * exp(-lambda * t)) - target,
                   g[c(i, i + 1)], f.lower = v[i], f.upper = v[i + 1],
                   tol = 1e-10)$root
  }
  set.seed(20261009)
  for (case in 1:40) {
    K <- sample(2:6, 1)
    lambda <- exp(runif(K, log(0.001), log(3)))
    state  <- rnorm(K) * exp(runif(K, -2, 2))
    if (case %% 2 == 0) state[K] <- -sum(state[-K])
    peak <- max(as.vector(exp(-outer(seq(0, MINS_PER_DAY, length.out = 5000),
                                      lambda)) %*% state))
    if (peak <= 0) next
    target <- peak * runif(1, 0.05, 1)
    expectWithin(recoveryCalc(state, lambda, target),
                 reference(state, lambda, target), 1e-5,
                 label = paste("case", case))
  }
})

test_that("recoveryRows() screens rows without changing any answer", {
  # recoveryRows() clears rows whose upper bound never reaches the target
  # before solving the rest; the answers must be recoveryCalc()'s, row by
  # row, for rows the screen clears, rows it cannot, and rows that cross.
  set.seed(7)
  lambda <- c(0.9, 0.05, 0.004, 0.6, 0)        # an unused route at rate 0
  S <- cbind(matrix(rnorm(200 * 3), 200, 3) *
               rep(c(30, 0.2, 0.02), each = 200), 0, 0)
  S[, 4] <- -rowSums(S[, 1:3])                  # effect-site shaped
  S[1:20, ] <- 0                                 # nothing given yet
  for (target in c(0.01, 0.3, 3)) {
    expect_equal(recoveryRows(S, lambda, target, MINS_PER_DAY),
                 vapply(1:200, function(i) recoveryCalc(S[i, ], lambda, target),
                        numeric(1)))
  }
  # Repeated rates, as a metabolite fold concatenates them, are merged.
  S2 <- cbind(S[, 1:4], S[, 1:4] / 2)
  expect_equal(recoveryRows(S2, c(lambda[1:4], lambda[1:4]), 0.3, MINS_PER_DAY),
               recoveryRows(S[, 1:4] * 1.5, lambda[1:4], 0.3, MINS_PER_DAY),
               tolerance = 1e-9)
})
