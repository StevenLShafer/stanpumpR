# ---------------------------------------------------------------------------
# test-calculateCe-branches.R
#
# WHAT THIS TESTS
#
# R/calculateCe.R advances the effect-site concentration one interval at a time
# by solving
#
#     dCe/dt = ke0 * (Cp(t) - Ce(t))
#
# in closed form over each [t(i-1), t(i)] interval.  Between the two sampled Cp
# values it has to assume *something* about the shape of Cp, and it picks one of
# two interpolations:
#
#   * linear   (`if` branch, taken when y0 <= y1, or either endpoint is 0):
#         Cp(s) = y0 + k s,  k = (y1 - y0)/dt
#         Ce(dt) = Ce0 e^-ke0 dt + k dt + (ke0 y0 - k)(1 - e^-ke0 dt)/ke0
#   * log-linear / mono-exponential (`else` branch, taken when y0 > y1 and both
#     endpoints are non-zero -- i.e. Cp is falling, the usual case after a bolus):
#         Cp(s) = y0 e^ks,  k = log(y1/y0)/dt  (< 0)
#         Ce(dt) = Ce0 e^-ke0 dt + ke0 y0 (e^k dt - e^-ke0 dt)/(k + ke0)
#
# Both expressions were re-derived by hand with the integrating factor
# e^(ke0 t) and agree with the source line-for-line, so each branch is *exact*
# whenever the true Cp really has the assumed shape.  The pre-existing
# test-calculateCe.R only exercises the linear branch (its Cp is 1:26, strictly
# rising).  This file covers the falling/log branch, the degenerate branches,
# and the per-interval ke0 vector used by advanceClosedForm1().
#
# Expected values here are never copied back out of calculateCe().  They come
# from (a) the analytic solution of the ODE for the specific Cp used, derived by
# hand, (b) a fine-grid RK4 integrator written in this file, and (c) base R's
# adaptive quadrature via stats::integrate().  Tolerances are stated per
# assertion and derived from the numerics rather than from the shared
# expect_equal_rounded() default, because several of these comparisons are exact
# to machine precision and one is deliberately limited by grid resolution.
#
# KNOWN LIMITATIONS / pinned quirks discovered while writing this file
#
#   1. L < 2 is unguarded.  `for (i in 2:L)` counts *down* when L == 1, so
#      calculateCe(5, 0.3, 0, 1) reads Cp[2] (NA) and dies inside the `if`.
#      Pinned below as an expected error.
#   2. ke0 == 0 yields NaN (0/0 in the linear branch), rather than the
#      mathematical limit Ce == 0.  Not reachable from the app -- getDrugPK()
#      always fits a strictly positive ke0 -- but pinned so the behaviour is at
#      least documented.
#   3. The log branch has a removable singularity at k == -ke0 (Cp decaying at
#      exactly the ke0 rate): the code computes 0/0 = NaN there.  The correct
#      limit is ke0*y0*dt*e^(k dt).  Requires bit-exact equality so it is
#      vanishingly unlikely in production, but it is a real latent NaN.  Pinned.
#      (Near, but not at, the singularity the formula is fine: at
#      |k + ke0| = 1e-8 it still agrees with a series expansion to ~1e-8
#      relative, so no extra guard band is warranted.)
#   4. A negative Cp sample -- e.g. a tiny round-off undershoot -- sends the log
#      branch through log(negative), and the resulting NaN then poisons every
#      later Ce through the Ce[i-1] recursion.  Pinned.
#
# All four are pinned quirks: if any of them is ever fixed, the corresponding
# expectation below should be deliberately updated (that is the point of the
# pin), not deleted quietly.
#
# Provenance: drafted by Claude Code (Fable 5), 2026-08-14, for the
# pre-deployment test plan (effect-site / calculateCe branch coverage).
# Expected values derived from the hand-integrated analytic solution of
# dCe/dt = ke0(Cp - Ce), cross-checked with an RK4 integrator and
# stats::integrate() written independently in this file; every assertion was
# run against the working tree.
# ---------------------------------------------------------------------------


# ---------------------------------------------------------------------------
# Local helpers (independent of the code under test)
# ---------------------------------------------------------------------------

# Classical 4th-order Runge-Kutta for dCe/dt = ke0(t) * (Cp(t) - Ce), Ce(0) = 0.
# `sub` substeps are taken inside every reporting interval, so the reported
# values land exactly on `tgrid`.  ke0v[i] is the rate constant in force over
# the interval ENDING at tgrid[i] -- the same convention calculateCe() uses
# (see the ke0-vector test below, which is what establishes that convention).
rk4Ce <- function(tgrid, cpFun, ke0v, sub = 20)
{
  out <- numeric(length(tgrid))
  ce  <- 0
  for (i in 2:length(tgrid))
  {
    k0 <- ke0v[i]
    f  <- function(t, y) k0 * (cpFun(t) - y)
    hs <- (tgrid[i] - tgrid[i - 1]) / sub
    t  <- tgrid[i - 1]
    for (j in 1:sub)
    {
      k1 <- f(t,          ce)
      k2 <- f(t + hs / 2, ce + hs * k1 / 2)
      k3 <- f(t + hs / 2, ce + hs * k2 / 2)
      k4 <- f(t + hs,     ce + hs * k3)
      ce <- ce + hs * (k1 + 2 * k2 + 2 * k3 + k4) / 6
      t  <- t + hs
    }
    out[i] <- ce
  }
  out
}

# calculateCe() with the branch decision removed: ALWAYS linear interpolation.
# Used only to demonstrate that the falling-Cp tests really do land in the
# `else` branch -- if they did not, these two would agree.
ceLinearBranchOnly <- function(Cp, ke0, dt, L)
{
  Ce <- rep(0, L)
  for (i in 2:L)
  {
    y0 <- Cp[i - 1]
    y1 <- Cp[i]
    k  <- (y1 - y0) / dt[i]
    Ce[i] <- Ce[i - 1] * exp(-ke0[i] * dt[i]) +
      k * dt[i] + (ke0[i] * y0 - k) * (1 - exp(-ke0[i] * dt[i])) / ke0[i]
  }
  Ce
}


# ---------------------------------------------------------------------------
# 1. Strictly falling Cp -- the log-interpolation (else) branch
#
# Cp(t) = C0 e^-a t is EXACTLY the shape the else branch assumes, so the
# recursion should reproduce the analytic solution of
#     dCe/dt = ke0 (C0 e^-a t - Ce),  Ce(0) = 0
# which integrating-factor algebra gives as
#     Ce(t) = ke0 C0 (e^-a t - e^-ke0 t) / (ke0 - a)
# to machine precision, and -- the interesting part -- independently of how
# coarsely the decay is sampled.
# ---------------------------------------------------------------------------
test_that("falling Cp uses log interpolation and is exact for a mono-exponential decay", {
  C0  <- 10      # ug/mL at t = 0, bolus-like starting point
  a   <- 0.1     # /min elimination rate of the (single-exponential) Cp
  ke0 <- 0.4     # /min

  ceAnalytic <- function(t) ke0 * C0 * (exp(-a * t) - exp(-ke0 * t)) / (ke0 - a)

  # -- fine grid (1 min) ----------------------------------------------------
  times <- seq(0, 60, by = 1)
  L     <- length(times)
  Cp    <- C0 * exp(-a * times)
  dt    <- c(0, diff(times))            # dt[1] == 0, as advanceClosedForm0() builds it

  Ce <- calculateCe(Cp, rep(ke0, L), dt, L)

  # Every interval after the first must be strictly falling with non-zero
  # endpoints, i.e. every one of them takes the else branch.
  expect_true(all(Cp[-1] < Cp[-L]))
  expect_true(all(Cp > 0))

  # Machine-precision agreement: observed max |error| is ~3e-15 on values up to
  # ~6.3, so 1e-12 is a ~1000x margin over the floating point noise floor and
  # still ~1e6 tighter than the house 1.5e-6 default.
  expect_equal(Ce, ceAnalytic(times), tolerance = 1e-12)
  expect_equal(Ce[1], 0)                # Ce always starts from zero

  # -- coarse grid (5 min) --------------------------------------------------
  # Same exactness on a 5x coarser grid.  This is the signature of the log
  # branch: linear interpolation would incur an O(h^2) discretisation error
  # here, exponential interpolation incurs none.
  timesC <- seq(0, 60, by = 5)
  LC     <- length(timesC)
  CpC    <- C0 * exp(-a * timesC)
  CeC    <- calculateCe(CpC, rep(ke0, LC), c(0, diff(timesC)), LC)
  expect_equal(CeC, ceAnalytic(timesC), tolerance = 1e-12)

  # -- proof that the branch selection is what makes that work --------------
  # Force the linear branch on the same coarse data: it must be measurably
  # worse.  (Observed: max deviation 0.130 on a curve peaking near 6.3, i.e.
  # ~2% -- far above any plausible round-off.)
  CeLin <- ceLinearBranchOnly(CpC, rep(ke0, LC), c(0, diff(timesC)), LC)
  expect_true(max(abs(CeC - CeLin)) > 0.05)
  expect_true(max(abs(CeLin - ceAnalytic(timesC))) >
              max(abs(CeC   - ceAnalytic(timesC))))

  # -- second, wholly independent reference: fine-grid RK4 ------------------
  # RK4 knows nothing about closed forms; it just marches the ODE.  With 20
  # substeps per minute its own truncation error is ~1e-12, so agreement at
  # 1e-8 is a genuine cross-check of the closed form.
  rk <- rk4Ce(times, function(t) C0 * exp(-a * t), rep(ke0, L), sub = 20)
  expect_equal(Ce, rk, tolerance = 1e-8)
})


# ---------------------------------------------------------------------------
# 2. Bolus-like rise-then-fall: both branches in one pass
#
# Cp(t) = A(e^-a t - e^-b t) is the classic absorption/disposition shape (rise,
# peak, fall).  It is neither piecewise linear nor piecewise exponential, so
# calculateCe() now carries a genuine interpolation error and we can check that
# it converges at the expected second order.  Analytic solution, by
# superposition of the mono-exponential result used in test 1:
#     Ce(t) = ke0 A [ (e^-a t - e^-ke0 t)/(ke0 - a)
#                   - (e^-b t - e^-ke0 t)/(ke0 - b) ]
# ---------------------------------------------------------------------------
test_that("rise-then-fall Cp gives a lagging, damped Ce with the right qualitative shape", {
  A <- 10; a <- 0.1; b <- 1.0; ke0 <- 0.3

  cpFun      <- function(t) A * (exp(-a * t) - exp(-b * t))
  ceAnalytic <- function(t) ke0 * A * ((exp(-a * t) - exp(-ke0 * t)) / (ke0 - a) -
                                       (exp(-b * t) - exp(-ke0 * t)) / (ke0 - b))

  h     <- 0.05
  times <- seq(0, 120, by = h)
  L     <- length(times)
  Cp    <- cpFun(times)
  Ce    <- calculateCe(Cp, rep(ke0, L), c(0, diff(times)), L)

  # Both branches are genuinely exercised by this one profile.
  expect_true(sum(Cp[-1] >= Cp[-L]) > 5)    # rising leg  -> linear branch
  expect_true(sum(Cp[-1] <  Cp[-L]) > 5)    # falling leg -> log branch

  # Qualitative properties that follow directly from dCe/dt = ke0(Cp - Ce):
  expect_equal(Ce[1], 0)                    # starts at zero
  expect_true(all(Ce >= 0))                 # never negative for non-negative Cp
  expect_true(all(is.finite(Ce)))

  iCp <- which.max(Cp)
  iCe <- which.max(Ce)
  expect_true(iCe > iCp)                    # Ce peaks after Cp (hysteresis)
  expect_true(all(Ce[2:iCp] < Cp[2:iCp]))   # Ce lags below Cp while Cp rises
  expect_true(max(Ce) < max(Cp))            # Ce is a damped average of past Cp

  # Ce is unimodal here, and its extremum must coincide with the Cp/Ce
  # crossing, because dCe/dt = 0 exactly when Cp == Ce.  On a discrete grid the
  # crossing can only be resolved to within one step.
  expect_true(all(diff(Ce[1:iCe])  > 0))
  expect_true(all(diff(Ce[iCe:L])  < 0))
  iCross <- which(Cp < Ce)[1]
  expect_true(abs(iCross - iCe) <= 1)

  # Quantitative check against the analytic solution.  At h = 0.05 min the
  # interpolation error is ~3.7e-4 on a curve peaking near 5.1 (0.007%), so the
  # tolerance below is set by the grid, not by the algorithm.
  expect_true(max(abs(Ce - ceAnalytic(times))) < 1e-3)

  # ...and it really is a grid effect: halving h must quarter the error, i.e.
  # the interpolation is second-order accurate.  This is the honest way to show
  # the closed form is right without pretending the coarse answer is exact.
  errs <- sapply(
    c(0.4, 0.2, 0.1),
    function(hh)
    {
      ti <- seq(0, 120, by = hh)
      Li <- length(ti)
      max(abs(calculateCe(cpFun(ti), rep(ke0, Li), c(0, diff(ti)), Li) - ceAnalytic(ti)))
    }
  )
  expect_true(all(diff(errs) < 0))                       # error shrinks with h
  expect_true(all(errs[-3] / errs[-1] > 3.5))            # ratio ~ 4 = O(h^2)
  expect_true(all(errs[-3] / errs[-1] < 4.5))
})


# ---------------------------------------------------------------------------
# 3. Large ke0: Ce collapses onto Cp
#
# For Cp = C0 e^-a t the analytic ratio is
#     Ce(t)/Cp(t) = ke0/(ke0 - a) * (1 - e^-(ke0 - a) t)
# so once t >> 1/ke0 the fractional gap is exactly a/(ke0 - a) -- 0.2% for
# ke0 = 50/min, 2% for ke0 = 5/min.  That is the quantitative statement of
# "Ce tracks Cp".
# ---------------------------------------------------------------------------
test_that("large ke0 makes Ce track Cp, with the analytically predicted residual gap", {
  C0 <- 10; a <- 0.1
  times <- seq(0, 60, by = 0.5)
  L     <- length(times)
  Cp    <- C0 * exp(-a * times)
  dt    <- c(0, diff(times))
  sel   <- times >= 1                    # skip the first ~50 time constants

  for (ke0 in c(5, 50))
  {
    Ce  <- calculateCe(Cp, rep(ke0, L), dt, L)
    gap <- max(abs(Ce - Cp)[sel] / Cp[sel])
    # e^-(ke0 - a) * 1 is < 1e-2 (ke0 = 5) / 1e-21 (ke0 = 50) at t = 1, so the
    # plateau value a/(ke0 - a) is reached to well within this tolerance.
    expect_equal(gap, a / (ke0 - a), tolerance = 1e-6)
    # Sanity in absolute terms: the effect site stays within ~2% of plasma for
    # ke0 = 5/min and within ~0.2% for ke0 = 50/min.
    expect_true(gap < 0.021)
  }

  # And the limiting behaviour is monotone in ke0: faster equilibration, less lag.
  gapFor <- function(ke0)
  {
    Ce <- calculateCe(Cp, rep(ke0, L), dt, L)
    max(abs(Ce - Cp)[sel] / Cp[sel])
  }
  expect_true(gapFor(50) < gapFor(5))
  expect_true(gapFor(5)  < gapFor(0.5))
})


# ---------------------------------------------------------------------------
# 4. ke0 -> 0: the effect site never fills
#
# Ce(t) = ke0 * integral_0^t e^-ke0 (t-s) Cp(s) ds <= ke0 * AUC(0, inf), and for
# Cp = C0 e^-a t the AUC is C0/a, so Ce can never exceed ke0 * C0/a -- 1e-7 for
# ke0 = 1e-9.  ke0 exactly 0 is a different story; see the quirk section.
# ---------------------------------------------------------------------------
test_that("vanishing ke0 leaves Ce at essentially zero, bounded by ke0 * AUC", {
  C0 <- 10; a <- 0.1; ke0 <- 1e-9
  times <- seq(0, 60, by = 1)
  L     <- length(times)
  Cp    <- C0 * exp(-a * times)

  Ce <- calculateCe(Cp, rep(ke0, L), c(0, diff(times)), L)

  expect_true(all(is.finite(Ce)))
  expect_true(all(Ce >= 0))
  expect_true(max(Ce) <= ke0 * C0 / a)         # analytic upper bound = 1e-7
  expect_true(max(Ce) > 0)                     # ...but not identically zero
  expect_true(max(Ce) < 1e-6 * max(Cp))        # 9 orders below Cp

  # The mono-exponential closed form still holds exactly at this extreme, which
  # confirms there is no cancellation blow-up in ke0/(k + ke0) as ke0 -> 0
  # (here k = -a, so the denominator stays at ~ -0.1).
  expect_equal(Ce, ke0 * C0 * (exp(-a * times) - exp(-ke0 * times)) / (ke0 - a),
               tolerance = 1e-9)
})


# ---------------------------------------------------------------------------
# 5. Zero Cp everywhere
#
# With y0 == y1 == 0 the `y0 == 0` guard sends every interval to the linear
# branch with k = 0, so input == 0 and the recursion stays pinned at zero --
# importantly without ever evaluating log(0).
# ---------------------------------------------------------------------------
test_that("identically zero Cp gives identically zero Ce", {
  L  <- 8
  dt <- c(0, 1, 2, 3, 5, 8, 13, 21)
  Ce <- calculateCe(rep(0, L), rep(0.3, L), dt, L)

  expect_equal(Ce, rep(0, L))
  expect_false(any(is.nan(Ce)))

  # A zero endpoint anywhere in the profile is also routed to the linear
  # branch: a drug that washes out to exactly 0 and is then re-dosed must not
  # produce log(0) = -Inf.
  Cp2 <- c(0, 2, 1, 0, 0, 3, 1)
  Ce2 <- calculateCe(Cp2, rep(0.3, 7), c(0, 1, 1, 1, 1, 1, 1), 7)
  expect_true(all(is.finite(Ce2)))
  expect_true(all(Ce2 >= 0))
})


# ---------------------------------------------------------------------------
# 6. Constant Cp plateau -- the textbook 1 - e^-ke0 t wash-in
#
# y0 == y1 takes the linear branch with k = 0, leaving
#     Ce(i) = Ce(i-1) e^-ke0 dt + y0 (1 - e^-ke0 dt),
# whose telescoped solution from Ce(0) = 0 is Ce(t) = Cp (1 - e^-ke0 t).
# Deliberately uses ragged dt to prove the result depends only on elapsed time.
# ---------------------------------------------------------------------------
test_that("a constant Cp plateau washes the effect site in as Cp * (1 - exp(-ke0 t))", {
  Cp0 <- 4; ke0 <- 0.15
  dt  <- c(0, 0.5, 1, 2, 3, 5, 8, 13)
  t   <- cumsum(dt)
  L   <- length(dt)

  Ce <- calculateCe(rep(Cp0, L), rep(ke0, L), dt, L)

  # Exact to machine precision (observed max error 2.2e-16).
  expect_equal(Ce, Cp0 * (1 - exp(-ke0 * t)), tolerance = 1e-13)

  # Half-time check straight from the closed form: at t = log(2)/ke0 the effect
  # site is at half the plateau.
  tHalf <- log(2) / ke0
  Ce2   <- calculateCe(rep(Cp0, 2), rep(ke0, 2), c(0, tHalf), 2)
  expect_equal(Ce2[2], Cp0 / 2, tolerance = 1e-12)

  # Equal consecutive Cp values embedded inside a falling profile behave the
  # same way (y0 <= y1 is TRUE for y0 == y1, so the plateau segments stay on
  # the linear branch), and only the final falling step leaves the plateau law.
  CpMix <- c(5, 5, 5, 3)
  dtMix <- c(0, 2, 3, 1)
  keMix <- 0.25
  CeMix <- calculateCe(CpMix, rep(keMix, 4), dtMix, 4)
  expect_equal(CeMix[1:3], 5 * (1 - exp(-keMix * cumsum(dtMix)[1:3])), tolerance = 1e-13)

  # For the last, falling step verify with adaptive quadrature (stats::integrate)
  # rather than the closed form, so the reference is independent code:
  #   Ce(dt) = e^-ke0 dt [ Ce0 + ke0 * integral_0^dt e^ke0 s * Cp(s) ds ]
  # with Cp(s) = 5 e^ks, k = log(3/5)/1.
  k     <- log(3 / 5) / dtMix[4]
  quad  <- integrate(function(s) exp(keMix * s) * 5 * exp(k * s), 0, dtMix[4],
                     rel.tol = 1e-12)$value
  expect_equal(CeMix[4],
               exp(-keMix * dtMix[4]) * (CeMix[3] + keMix * quad),
               tolerance = 1e-9)
  # Ce keeps climbing through that step: it enters the interval below 5 and Cp
  # only falls to 3, so Cp - Ce stays positive over most of the minute.
  expect_true(CeMix[4] > CeMix[3])
})


# ---------------------------------------------------------------------------
# 7. ke0 supplied per interval (advanceClosedForm1's time-varying case)
#
# calculateCe() indexes ke0[i], so element i is the rate constant in force over
# the interval ENDING at time i.  advanceClosedForm0/PO pass rep(ke0, L) and
# cannot tell the difference; advanceClosedForm1 passes a genuine vector, so the
# convention matters.  RK4 with the same convention agrees to ~1e-11; RK4 with
# the ke0 vector shifted by one disagrees by 0.15 (~4% of the curve), so this
# assertion really does pin the indexing.
# ---------------------------------------------------------------------------
test_that("a per-interval ke0 vector is applied to the interval ending at that index", {
  C0 <- 8; a <- 0.08
  times <- seq(0, 40, by = 0.5)
  L     <- length(times)
  cpFun <- function(t) C0 * exp(-a * t)
  Cp    <- cpFun(times)
  ke0v  <- ifelse(times <= 20, 0.2, 0.6)     # deterministic step change at t = 20

  Ce <- calculateCe(Cp, ke0v, c(0, diff(times)), L)
  rk <- rk4Ce(times, cpFun, ke0v, sub = 40)

  expect_equal(Ce, rk, tolerance = 1e-8)

  # The shifted-convention reference must NOT match -- otherwise the test above
  # would be insensitive to an off-by-one in the ke0 index.
  rkShift <- rk4Ce(times, cpFun, c(ke0v[-1], ke0v[L]), sub = 40)
  expect_true(max(abs(Ce - rkShift)) > 0.05)

  # Faster ke0 in the second half pulls Ce back up toward Cp.
  gapEarly <- max(abs(Ce - Cp)[times > 10 & times <= 20] / Cp[times > 10 & times <= 20])
  gapLate  <- max(abs(Ce - Cp)[times > 30] / Cp[times > 30])
  expect_true(gapLate < gapEarly)
})


# ---------------------------------------------------------------------------
# 8. Edge cases and pinned quirks
#
# Everything in this block documents CURRENT behaviour that is arguably wrong.
# Each expectation is a deliberate pin: fixing calculateCe() should update these
# assertions on purpose.  See the KNOWN LIMITATIONS list in the header.
# ---------------------------------------------------------------------------
test_that("degenerate inputs behave as currently pinned (see KNOWN LIMITATIONS)", {

  # -- quirk 1: L == 1 is not guarded --------------------------------------
  # `2:L` becomes c(2, 1) when L == 1, so the loop runs backwards, reads
  # Cp[2] == NA and the `if` condition errors out.  A single-sample profile
  # should simply return 0.  No regexp is matched because R error messages are
  # translated in non-English locales.
  expect_error(calculateCe(5, 0.3, 0, 1))

  # L == 2 is the smallest input that works, and one linear step from Ce = 0
  # with Cp rising 1 -> 2 over 1 min at ke0 = 0.3 is
  #   k dt + (ke0 y0 - k)(1 - e^-ke0 dt)/ke0
  # = 1 + (0.3 - 1)(1 - e^-0.3)/0.3  (hand-evaluated below, not copied from output)
  two <- calculateCe(c(1, 2), c(0.3, 0.3), c(0, 1), 2)
  expect_equal(two[1], 0)
  expect_equal(two[2], 1 + (0.3 * 1 - 1) * (1 - exp(-0.3)) / 0.3, tolerance = 1e-12)

  # dt[1] is never read (the loop starts at i = 2), so a junk first dt cannot
  # change the answer.  advanceClosedForm0() sets dt[1] <- 0; this shows nothing
  # depends on that.
  expect_equal(calculateCe(c(1, 2), c(0.3, 0.3), c(99, 1), 2), two)

  # -- quirk 2: ke0 == 0 gives NaN, not the Ce == 0 limit -------------------
  # (ke0 y0 - k)(1 - e^0)/0 -> 0/0.  Not reachable from the app (getDrugPK()
  # always returns ke0 > 0) but pinned so a future guard is a conscious change.
  zeroKe0 <- calculateCe(c(1, 2, 3), rep(0, 3), c(0, 1, 1), 3)
  expect_equal(zeroKe0[1], 0)
  expect_true(all(is.nan(zeroKe0[2:3])))

  # -- quirk 3: removable singularity at k == -ke0 --------------------------
  # Cp halving every minute has k = log(0.5); with ke0 = -log(0.5) the log
  # branch computes y0 ke0/0 * 0 = NaN.  The limit of the expression as
  # k -> -ke0 is ke0 * y0 * dt * e^(k dt) = 0.3466 for the first step here.
  ke0Res  <- -log(0.5)
  resonant <- calculateCe(c(1, 0.5, 0.25), rep(ke0Res, 3), c(0, 1, 1), 3)
  expect_true(all(is.nan(resonant[2:3])))
  # Just off resonance the formula is well behaved and lands on that limit,
  # which is why this is a point singularity rather than a broken branch.
  offRes <- calculateCe(c(1, 0.5), rep(ke0Res * (1 + 1e-6), 2), c(0, 1), 2)
  expect_equal(offRes[2], ke0Res * 1 * 1 * exp(log(0.5) * 1), tolerance = 1e-5)

  # -- quirk 4: a negative Cp sample poisons the rest of the trace ----------
  # A round-off undershoot below zero after a positive sample takes the log
  # branch, log() of a negative number is NaN with a warning, and the NaN then
  # propagates through Ce[i-1] for the remainder of the simulation even though
  # every later Cp is perfectly sane.
  bad <- NULL
  expect_warning(bad <- calculateCe(c(1, -1e-12, 0.5, 0.4), rep(0.3, 4),
                                    c(0, 1, 1, 1), 4))
  expect_equal(bad[1], 0)
  expect_true(all(is.nan(bad[2:4])))
})
