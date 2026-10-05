# Invariant (property) tests for cube() (R/cube.R).
#
# The existing test-cube.R pins exact roots for three fixed inputs. This file
# complements it with model-independent invariants that must hold for ANY valid
# rate constants, which catch regressions the point tests can miss:
#
#   For the disposition polynomial x^3 + a2 x^2 + a1 x + a0, the three
#   eigenvalues (lambdas) returned by cube() must satisfy Vieta's relations
#     sum(lambda)              = a2 = k10+k12+k13+k21+k31   (trace)
#     sum of pairwise products = a1
#     prod(lambda)             = a0 = k10*k21*k31
#   and be real, non-negative, and sorted descending.
#
# Provenance: drafted by Claude Code (Opus 4.8), 2026-08-13, for the
# pre-deployment test plan (PK/PD engine). Verified against R/cube.R on master.

vieta <- function(k10, k12, k13, k21, k31) {
  list(
    a2 = k10 + k12 + k13 + k21 + k31,
    a1 = k10 * k31 + k21 * k31 + k21 * k13 + k10 * k21 + k31 * k12,
    a0 = k10 * k21 * k31
  )
}

test_that("3-compartment roots are real, positive, and sorted descending", {
  r <- cube(0.1, 0.4, 0.2, 0.1, 0.05)
  expect_length(r, 3)
  expect_false(any(is.na(r)))
  expect_true(all(r >= 0))
  expect_true(all(diff(r) <= 0))          # sorted descending
})

test_that("3-compartment roots satisfy Vieta's relations", {
  ks <- list(
    c(0.1, 0.4, 0.2, 0.1, 0.05),
    c(0.05, 0.3, 0.15, 0.08, 0.02),
    c(0.2, 0.1, 0.05, 0.3, 0.12)
  )
  for (k in ks) {
    r <- cube(k[1], k[2], k[3], k[4], k[5])
    v <- vieta(k[1], k[2], k[3], k[4], k[5])
    expect_equal(sum(r), v$a2, tolerance = 1e-8)
    expect_equal(r[1]*r[2] + r[1]*r[3] + r[2]*r[3], v$a1, tolerance = 1e-8)
    expect_equal(prod(r), v$a0, tolerance = 1e-8)
  }
})

test_that("2-compartment branch (k31 = 0) reduces to the quadratic", {
  r <- cube(0.1, 0.4, 0, 0.1, 0)
  expect_equal(r[3], 0)                    # third root is exactly 0
  expect_true(all(r >= 0))
  expect_true(all(diff(r) <= 0))
  # Vieta for x^2 - (k10+k12+k21) x + k10*k21
  expect_equal(r[1] + r[2], 0.1 + 0.4 + 0.1, tolerance = 1e-8)
  expect_equal(r[1] * r[2], 0.1 * 0.1, tolerance = 1e-8)
})

test_that("1-compartment branch (k21 = k31 = 0) returns k10, 0, 0", {
  expect_equal(cube(0.123, 0, 0, 0, 0), c(0.123, 0, 0))
})

test_that("near-degenerate rate constants do not produce NaN (phi clamping)", {
  # Equal micro-constants push acos()'s argument toward the +/-1 clamp; the
  # solver must still return three finite, ordered, non-negative roots.
  r <- cube(0.1, 0.1, 0.1, 0.1, 0.1)
  expect_false(any(is.na(r)))
  expect_true(all(is.finite(r)))
  expect_true(all(r >= 0))
  expect_true(all(diff(r) <= 0))
  v <- vieta(0.1, 0.1, 0.1, 0.1, 0.1)
  expect_equal(sum(r), v$a2, tolerance = 1e-8)
})
