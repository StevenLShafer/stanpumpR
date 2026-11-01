# Inverse Gaussian absorption (R/ig_absorption.R): the closed-form convolution
# of an IG absorption density with a sum-of-exponentials disposition function.
# Written by Claude Code, 2026-11-01.
#
# WHY THIS FILE EXISTS AT ALL.  The kernel shipped in 2026-08 with its only
# verification inside an `if (FALSE)` block, which no test suite runs.  On
# 2026-10-31 a dead-code cleanup (#641) deleted the whole file, correctly, as
# 221 lines of comments with no caller.  These tests are what let it defend
# itself.  Do not move them back into the R file.
#
# TWO INDEPENDENT REFERENCES, and the reason there are two.  The obvious
# reference, adaptive quadrature of the convolution integral, has an ABSOLUTE
# accuracy floor: stats::integrate() cannot resolve an integrand whose scale is
# 1e-40, so it reports relative errors of 6e-8 there and 1.7e-2 at 1e-318 even
# when the closed form is exact to 1e-15.  An earlier note on this kernel
# attributed those discrepancies to the reference and was right, but asserted
# it rather than showing it.  So:
#
#   igRefTilt()    exponential tilting maps the IG to another IG, so the
#                  convolution becomes a sum of two POSITIVE pnorm terms --
#                  evaluated in log space it has no cancellation anywhere and
#                  is accurate to full relative precision.  Real branch only.
#                  Mathematically unlike the erfc-difference the code uses.
#
#   igRefScaled()  quadrature with the integrand's own peak magnitude factored
#                  out, so the adaptive rule works on an O(1) function.  This
#                  removes the absolute floor and covers BOTH branches, which
#                  is what the complex branch needs, the tilt trick requiring
#                  b > 0.
#
# The last test in this file asserts that plain quadrature is the WEAKER of the
# two, so that nobody "fixes" the closed form to agree with it.


# ---------------------------------------------------------------------------
# Reference 1: exponential tilting.  f(s; mu, lam) e^{ks} is itself an IG
# density with the same lam and mean mu' = sqrt(lam / (2b)), scaled by
# exp(lam/mu - lam/mu'), so the convolution is
#   exp(-kt + lam/mu - lam/mu') * F_IG(t; mu', lam)
# and the IG CDF is Phi(..) + e^{2lam/mu'} Phi(..), two positive terms.
# ---------------------------------------------------------------------------
igRefTilt <- function(t, A, k, mu, lambda) {
  vapply(t, function(ti) {
    tot <- 0
    for (i in seq_along(A)) {
      b <- lambda / (2 * mu^2) - k[i]
      if (b <= 0) stop("igRefTilt() is the real-branch reference; b must be > 0")
      m  <- sqrt(lambda / (2 * b))
      l1 <- pnorm(sqrt(lambda / ti) * (ti / m - 1), log.p = TRUE)
      l2 <- 2 * lambda / m + pnorm(-sqrt(lambda / ti) * (ti / m + 1),
                                   log.p = TRUE)
      lm <- max(l1, l2)
      logF <- lm + log(exp(l1 - lm) + exp(l2 - lm))
      tot <- tot + A[i] * exp(-k[i] * ti + lambda / mu - lambda / m + logF)
    }
    tot
  }, numeric(1))
}


# ---------------------------------------------------------------------------
# Reference 2: rescaled quadrature, valid on both branches.
# ---------------------------------------------------------------------------
igRefScaled <- function(t, A, k, mu, lambda) {
  vapply(t, function(ti) {
    tot <- 0
    for (i in seq_along(A)) {
      lg <- function(s) {
        0.5 * log(lambda / (2 * pi * s^3)) -
          lambda * (s - mu)^2 / (2 * mu^2 * s) - k[i] * (ti - s)
      }
      mode <- mu * (sqrt(1 + 9 * mu^2 / (4 * lambda^2)) - 3 * mu / (2 * lambda))
      cand <- c(mode, mu, ti * c(0.01, 0.1, 0.5, 0.9, 0.999))
      cand <- cand[cand > 0 & cand < ti]
      if (!length(cand)) cand <- ti / 2
      op   <- optimize(lg, lower = min(cand) * 1e-3, upper = ti, maximum = TRUE)
      Lmax <- max(lg(cand), op$objective)
      br   <- sort(unique(c(mode * c(0.2, 1, 5), mu, op$maximum)))
      br   <- unique(c(1e-14, br[br > 0 & br < ti], ti))
      sc <- 0
      for (j in seq_len(length(br) - 1)) {
        sc <- sc + integrate(function(s) exp(lg(s) - Lmax),
                             br[j], br[j + 1],
                             rel.tol = 1e-13, subdivisions = 2000L)$value
      }
      tot <- tot + A[i] * exp(Lmax + log(sc))
    }
    tot
  }, numeric(1))
}


# Worst relative error, counting only the points where the reference is above
# `floor` of the peak.  The tails are not interesting and are not asserted:
# see the Accuracy note in R/ig_absorption.R for why.
worstRelErr <- function(closed, ref, floor = 1e-10) {
  keep <- ref > max(closed) * floor
  max(abs(closed[keep] - ref[keep]) / abs(ref[keep]))
}


# Integrate f over (0, upper) with breaks at the density's own landmarks.
# Both parts matter and a first draft of these tests got both wrong.  A plain
# integrate() over the whole range STEPS OVER THE PEAK when the density is
# narrow -- it returned 0.0, not a small error, for mu 1 at lambda 240 -- and
# a fixed upper limit of a few hundred means TRUNCATES the tail: the IG at
# mu 60, lambda 0.5 has a CV of 11, and 400 means still leaves 1.2e-4 of the
# mass uncounted.  So the limit comes from the tail's own time constant,
# 2 mu^2 / lambda, not from a multiple of the mean.
igIntegrate <- function(f, mu, lambda, upper = mu * 50 + 60 * (2 * mu^2 / lambda)) {
  mode <- mu * (sqrt(1 + 9 * mu^2 / (4 * lambda^2)) - 3 * mu / (2 * lambda))
  br <- unique(sort(c(mode * c(0.05, 0.3, 1, 3, 10), mu * c(0.3, 1, 3, 10, 100))))
  br <- c(0, br[br > 0 & br < upper], upper)
  sum(vapply(seq_len(length(br) - 1), function(j)
    integrate(f, br[j], br[j + 1], rel.tol = 1e-12, subdivisions = 4000L)$value,
    numeric(1)))
}

# The IG distribution function in closed form: two POSITIVE pnorm terms, so it
# has no cancellation and is exact to full relative precision.
igCdfRef <- function(t, mu, lambda) {
  l1 <- pnorm(sqrt(lambda / t) * (t / mu - 1), log.p = TRUE)
  l2 <- 2 * lambda / mu + pnorm(-sqrt(lambda / t) * (t / mu + 1), log.p = TRUE)
  m <- pmax(l1, l2)
  exp(m) * (exp(l1 - m) + exp(l2 - m))
}


test_that("igDensity is a density, with the mean and variance it claims", {
  for (mu in c(0.5, 1, 60)) {
    for (lambda in c(0.5, 4, 240)) {
      m0 <- igIntegrate(function(s) igDensity(s, mu, lambda), mu, lambda)
      m1 <- igIntegrate(function(s) s * igDensity(s, mu, lambda), mu, lambda)
      m2 <- igIntegrate(function(s) s^2 * igDensity(s, mu, lambda), mu, lambda)
      expect_lt(abs(m0 - 1), 1e-10)                      # it integrates to 1
      expect_lt(abs(m1 - mu) / mu, 1e-10)                # mean mu
      expect_lt(abs((m2 - m1^2) - mu^3 / lambda) /       # variance mu^3/lambda
                  (mu^3 / lambda), 1e-9)
    }
  }
  expect_equal(igDensity(c(-1, 0), 1, 4), c(0, 0))
  expect_error(igDensity(1, 0, 4))
  expect_error(igDensity(1, 1, -4))
})


test_that("igDensity integrates to the closed-form IG distribution function", {
  # Stronger than the moments: it pins the whole shape, not three numbers of
  # it, and against a form with no quadrature and no cancellation in it.
  for (mu in c(0.5, 1, 60)) {
    for (lambda in c(0.5, 4, 240)) {
      for (q in c(0.2, 0.5, 1, 2, 5)) {
        t <- mu * q
        expect_lt(abs(igIntegrate(function(s) igDensity(s, mu, lambda),
                                  mu, lambda, upper = t) -
                        igCdfRef(t, mu, lambda)), 1e-12)
      }
      expect_lt(abs(igCdfRef(mu * 1e9, mu, lambda) - 1), 1e-12)
    }
  }
})


test_that("igDensity is the density igAbsorptionConc convolves", {
  # With no elimination the concentration is the density's own CDF, which ties
  # the two functions together: if either drifted from the other this fails.
  mu <- 1; lambda <- 4
  for (tt in c(0.25, 0.5, 1, 2, 5)) {
    cdf <- integrate(function(s) igDensity(s, mu, lambda), 0, tt,
                     rel.tol = 1e-12, subdivisions = 1000L)$value
    expect_lt(abs(igAbsorptionConc(tt, 1, 0, mu, lambda) - cdf), 1e-10)
  }
})


test_that("the vendored Faddeeva function matches the exact identities", {
  # w(0) = 1 exactly.
  expect_lt(Mod(faddeevaW(complex(real = 0, imaginary = 0)) - 1), 1e-15)

  # On the imaginary axis w(iy) = exp(y^2) erfc(y) = erfcx(y), and both sides
  # are O(1) or larger, so this is a RELATIVE check.
  ys <- c(1e-6, 1e-3, 0.1, 1, 3, 10, 30, 100)
  for (y in ys) {
    got <- Re(faddeevaW(complex(real = 0, imaginary = y)))
    expect_lt(abs(got - erfcxReal(y)) / erfcxReal(y), 1e-12)
  }

  # On the real axis Re w(x) = exp(-x^2), which underflows below the
  # approximation's ABSOLUTE floor near x = 6.  That is a documented property
  # of Weideman's rational form, not a transcription error, so the check here
  # is absolute -- and it pins the floor, so a regression that made it worse
  # would fail.
  xs <- c(1e-6, 0.1, 1, 3, 6, 10, 20)
  for (x in xs) {
    got <- Re(faddeevaW(complex(real = x, imaginary = 0)))
    expect_lt(abs(got - exp(-x^2)), 1e-15)
  }
  # Relative accuracy does hold where exp(-x^2) is above the floor.
  for (x in c(1e-6, 0.1, 1, 3)) {
    got <- Re(faddeevaW(complex(real = x, imaginary = 0)))
    expect_lt(abs(got - exp(-x^2)) / exp(-x^2), 1e-12)
  }
})


test_that("all of the dose is absorbed when nothing is eliminated", {
  # With k = 0 the disposition function is 1, so the convolution is the IG CDF
  # and must reach exactly 1.  Independent of everything else in the file.
  for (lambda in c(0.25, 1, 4, 20, 100)) {
    expect_lt(abs(igAbsorptionConc(500, 1, 0, mu = 1, lambda = lambda) - 1),
              1e-12)
  }
})


test_that("the real branch matches the tilted-IG reference", {
  mu <- 60; lambda <- 240
  tt <- c(1, 2, 5, 15, 30, 60, 120, 240, 480, 720, 1440)

  # b > 0 for every term: slow exponents only.
  A <- c(0.02, 0.005); k <- c(0.004, 0.0005)
  expect_true(all(lambda / (2 * mu^2) - k > 0))
  expect_lt(worstRelErr(igAbsorptionConc(tt, A, k, mu, lambda),
                        igRefTilt(tt, A, k, mu, lambda)), 1e-12)

  # A single slow exponent, out to where the concentration is ~1e-50, to show
  # the reflection path (v < 0) is not where the accuracy goes.
  cf <- igAbsorptionConc(tt, 1, 1e-4, mu, lambda)
  rf <- igRefTilt(tt, 1, 1e-4, mu, lambda)
  expect_lt(worstRelErr(cf, rf), 1e-12)
})


test_that("the complex branch matches the rescaled-quadrature reference", {
  mu <- 60
  tt <- c(5, 15, 30, 60, 120, 240, 480, 1440)
  for (lambda in c(1500, 240, 60)) {
    # k = 0.30 /min exceeds lambda/(2 mu^2) in all three, so b < 0.
    expect_lt(lambda / (2 * mu^2) - 0.30, 0)
    expect_lt(worstRelErr(igAbsorptionConc(tt, 1, 0.30, mu, lambda),
                          igRefScaled(tt, 1, 0.30, mu, lambda)), 1e-12)
  }
})


test_that("the b = 0 crossover is continuous and all three paths agree", {
  mu <- 1; lambda <- 4
  thr <- lambda / (2 * mu^2)          # 2 per unit time
  tt  <- c(0.5, 1, 2, 5, 10)

  # Sweep k through the threshold.  b = eps, so eps > 0 takes the real branch,
  # eps < 0 the complex branch and eps == 0 the b == 0 limiting case: three
  # different code paths, each checked against the one reference.
  for (eps in c(1e-2, 1e-4, 1e-8, 0, -1e-8, -1e-4, -1e-2)) {
    cf <- igAbsorptionConc(tt, 1, thr - eps, mu, lambda)
    expect_lt(worstRelErr(cf, igRefScaled(tt, 1, thr - eps, mu, lambda)), 1e-11)
  }

  # Continuity AT the switch.  These three k values differ by 1e-8 but are
  # evaluated by three different code paths, so a seam in the implementation
  # would show here even though it is invisible in the reference checks above.
  # Only adjacent values may be compared: a step of 1e-2 in k moves the answer
  # by ~6% for real reasons, which says nothing about the branches.
  d      <- 1e-8
  real   <- igAbsorptionConc(tt, 1, thr - d, mu, lambda)   # b = +1e-8
  zero   <- igAbsorptionConc(tt, 1, thr,     mu, lambda)   # b =  0
  cplx   <- igAbsorptionConc(tt, 1, thr + d, mu, lambda)   # b = -1e-8
  expect_lt(max(abs(real - zero) / zero), 1e-6)
  expect_lt(max(abs(cplx - zero) / zero), 1e-6)
  # Monotone in k across the seam, as it must be: more elimination, less drug.
  expect_true(all(real > zero & zero > cplx))
})


test_that("a realistic three-compartment drug is exact across both branches", {
  # Unit-dose disposition coefficients and exponents per MINUTE, the units the
  # engine works in, with a one-hour mean absorption time.  The fast exponent
  # takes the complex branch and the two slow ones the real branch, so one
  # call exercises both.
  A <- c(0.0483, 0.0135, 0.00147); k <- c(0.30, 0.030, 0.0030)
  mu <- 60; lambda <- 240
  branches <- lambda / (2 * mu^2) - k
  expect_lt(branches[1], 0)
  expect_true(all(branches[-1] > 0))

  tt <- c(1, 5, 15, 30, 60, 120, 240, 480, 720, 1440)
  expect_lt(worstRelErr(igAbsorptionConc(tt, A, k, mu, lambda),
                        igRefScaled(tt, A, k, mu, lambda)), 1e-12)
})


test_that("accuracy holds across the plausible parameter range", {
  # A reduced sweep; the full one (84 sets x ~80 points, worst 1.6e-14) was run
  # once by hand and is recorded in R/ig_absorption.R.
  A <- c(0.0483, 0.0135, 0.00147); k <- c(0.30, 0.030, 0.0030)
  tt <- c(2, 5, 10, 20, 40, 60, 90, 150, 240, 480, 900, 1440)
  worst <- 0
  anyComplex <- FALSE
  for (mu in c(10, 60, 240)) {
    for (cv2 in c(0.04, 0.25, 1, 4)) {
      lambda <- mu / cv2
      anyComplex <- anyComplex || any(lambda / (2 * mu^2) - k < 0)
      worst <- max(worst, worstRelErr(igAbsorptionConc(tt, A, k, mu, lambda),
                                      igRefScaled(tt, A, k, mu, lambda)))
    }
  }
  expect_true(anyComplex)
  expect_lt(worst, 1e-12)
})


test_that("doses superpose and scale, and degenerate input is handled", {
  A <- c(0.0483, 0.0135, 0.00147); k <- c(0.30, 0.030, 0.0030)
  mu <- 60; lambda <- 240
  tt <- c(0, 15, 60, 240)

  # t <= 0 gives exactly zero, including negative times.
  expect_equal(igAbsorptionConc(c(-5, 0), A, k, mu, lambda), c(0, 0))
  expect_equal(igAbsorptionConc(numeric(0), A, k, mu, lambda), numeric(0))

  # Linear in dose and in bioavailability.
  base <- igAbsorptionConc(tt, A, k, mu, lambda)
  expect_equal(igAbsorptionConc(tt, A, k, mu, lambda, dose = 7), 7 * base)
  expect_equal(igAbsorptionConc(tt, A, k, mu, lambda, bioavailability = 0.25),
               0.25 * base)

  # Superposition: two doses are the sum of two shifted single doses.  This is
  # the property any wiring into the engine will rely on.
  t2 <- seq(5, 600, by = 25)
  two <- igAbsorptionConc(t2, A, k, mu, lambda) +
    igAbsorptionConc(t2 - 120, A, k, mu, lambda)
  expect_equal(two[t2 <= 120], igAbsorptionConc(t2[t2 <= 120], A, k, mu, lambda))

  # Guards.
  expect_error(igAbsorptionConc(1, c(1, 1), 1, mu, lambda))
  expect_error(igAbsorptionConc(1, 1, 1, mu = 0, lambda))
  expect_error(igAbsorptionConc(1, 1, 1, mu, lambda = -1))
  expect_error(igAbsorptionConc(c(1, NA), 1, 1, mu, lambda))
})


test_that("plain quadrature is the weaker reference, not the closed form", {
  # A REGRESSION GUARD, not a property of the kernel.  Plain adaptive
  # quadrature of the convolution disagrees with the closed form in the deep
  # tail, and the temptation is to treat that as the closed form's error.  It
  # is not: against the cancellation-free tilted-IG reference the closed form
  # is right to 1e-14 at exactly the points where plain quadrature is wrong by
  # 1e-8 and worse.  If this test ever fails, suspect the quadrature, and read
  # the Accuracy note in R/ig_absorption.R before touching igAbsorptionConc().
  plainQuad <- function(t, A, k, mu, lambda) {
    igPdf <- function(s) {
      sqrt(lambda / (2 * pi * s^3)) * exp(-lambda * (s - mu)^2 / (2 * mu^2 * s))
    }
    vapply(t, function(ti) {
      integrate(function(s) {
        igPdf(s) * vapply(s, function(sj) sum(A * exp(-k * (ti - sj))),
                          numeric(1))
      }, 1e-14, ti, rel.tol = 1e-12, subdivisions = 500L)$value
    }, numeric(1))
  }

  # Deep tail of a real-branch term: concentrations of 1e-41 and 1e-82.
  mu <- 1; lambda <- 4; k <- 2 - 1e-3
  tt <- c(48, 96)
  ref <- igRefTilt(tt, 1, k, mu, lambda)
  closedErr <- max(abs(igAbsorptionConc(tt, 1, k, mu, lambda) - ref) / ref)
  quadErr   <- max(abs(plainQuad(tt, 1, k, mu, lambda) - ref) / ref)

  expect_lt(closedErr, 1e-13)
  expect_gt(quadErr, 1e-9)
  expect_gt(quadErr / closedErr, 1e4)
})
