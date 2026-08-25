# Tests for CE() (R/CE.R): the 4-exponential effect-site concentration
#   CE(t) = sum_i coef_i * exp(-lambda_i * t)
# used inside tPeakError() when fitting ke0 from tPeak.
#
# In a real effect-site model the four coefficients sum to zero (the ke0 term
# carries a negative coefficient equal to minus the sum of the three plasma
# terms), giving CE(0) = 0 and a single interior peak. The tests cover both
# the raw algebraic contract and that model-shaped behavior.
#
# Provenance: drafted by Claude Code (Opus 4.8), 2026-08-13, for the
# pre-deployment test plan (PK/PD engine). Expected values are hand-computed
# from the formula (see inline comments).

test_that("CE(0) equals the sum of the coefficients", {
  expect_equal(CE(0, 1, 2, 3, 4, 0.1, 0.2, 0.3, 0.4), 10)
  expect_equal(CE(0, 0.5, -0.25, 0.75, -1, 1, 2, 3, 4), 0)
})

test_that("CE matches a hand-computed value", {
  # coef = (2, 1, 0.5, -3.5), lambda = (0.5, 0.1, 0.02, 1), t = 2:
  #   2*exp(-1) + 1*exp(-0.2) + 0.5*exp(-0.04) - 3.5*exp(-2)
  expected <- 2 * exp(-1) + exp(-0.2) + 0.5 * exp(-0.04) - 3.5 * exp(-2)
  expect_equal(CE(2, 2, 1, 0.5, -3.5, 0.5, 0.1, 0.02, 1), expected)
})

test_that("CE decays to zero at large times (positive lambdas)", {
  expect_equal(CE(1e6, 2, 1, 0.5, -3.5, 0.5, 0.1, 0.02, 1), 0, tolerance = 1e-12)
})

test_that("CE is vectorized over time", {
  t <- c(0, 1, 5, 10)
  vec <- CE(t, 2, 1, 0.5, -3.5, 0.5, 0.1, 0.02, 1)
  ind <- sapply(t, CE, 2, 1, 0.5, -3.5, 0.5, 0.1, 0.02, 1)
  expect_equal(vec, ind)
})

test_that("a model-shaped coefficient set rises from zero to one interior peak", {
  # Effect-site shape: plasma coefficients (2, 1, 0.5) with lambdas
  # (0.5, 0.1, 0.02), ke0 = 1 carrying coefficient -(2+1+0.5) = -3.5.
  coef <- c(2, 1, 0.5, -3.5)
  lambda <- c(0.5, 0.1, 0.02, 1)
  ce <- function(t) CE(t, coef[1], coef[2], coef[3], coef[4],
                       lambda[1], lambda[2], lambda[3], lambda[4])
  expect_equal(ce(0), 0)                       # starts at zero
  t <- seq(0.01, 60, by = 0.01)
  y <- ce(t)
  expect_true(all(y > 0))                      # positive after onset
  ipeak <- which.max(y)
  expect_gt(ipeak, 1)                          # peak is interior,
  expect_lt(ipeak, length(y))                  # not at either boundary
  expect_true(all(diff(y[1:ipeak]) > 0))       # monotone rise to the peak
  expect_true(all(diff(y[ipeak:length(y)]) < 0))  # monotone fall after it
})
