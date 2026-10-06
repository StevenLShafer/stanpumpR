# Tests for convertState() (R/convertState.R)
#
# convertState(oldState, oldPK, newPK) is called from advanceClosedForm1() at the
# moment a PK event switches the active PK set mid-simulation. The "state" is the
# 3-vector of per-eigenvalue plasma concentration components (p_state_l1..l3 in
# advanceClosedForm1): Cp = state[1] + state[2] + state[3], each component decaying
# as exp(-lambda_j * t). Because the eigen-decomposition depends on the PK
# parameters, switching PK sets requires re-expressing the same *physical*
# compartment amounts (a1, a2, a3) in the new eigenbasis. convertState does this by
#   (1) reconstructing amounts from the old eigen-components:
#         a1 = v1 * sum(state)
#         a2 = sum_j state_j * v1*k12 / (k21 - lambda_j)   [written v2*k21 = v1*k12]
#         a3 = sum_j state_j * v1*k13 / (k31 - lambda_j)
#       (These follow from the compartment ODE da2/dt = k12*a1 - k21*a2: for a
#       plasma eigenmode c*exp(-lambda*t), the matching particular solution is
#       a2 = v1*k12*c/(k21 - lambda) * exp(-lambda*t); likewise for a3 with k13/k31.)
#   (2) solving the corresponding 3x3 linear system for the new eigen-components
#       by hand-coded elimination (the f1..f16 cascade).
# These tests verify both halves independently: step (1) against a brute-force RK4
# integration of the raw compartment ODEs, and step (2) against base R solve().
#
# NOTE on the a3 line in convertState: it multiplies by (v1 * k13 / k31) with v3
# commented out in the source. For every PK set built by getDrugPK, k13 = cl3/v1
# and k31 = cl3/v3, so v1*k13/k31 == v3 algebraically; probing the whole drug
# library showed at most a 1-ulp floating point difference. The two forms are
# interchangeable in production.
#
# KNOWN LIMITATION (documented here, deliberately NOT asserted): the reconstruction
# divides by (k21 - lambda_j) and (k31 - lambda_j). If a PK set ever had k21 (or
# k31) exactly equal to one of its eigenvalues, convertState would silently return
# a NaN state, e.g.:
#     pkBad <- pkA; pkBad$k21 <- pkA$lambda_2
#     convertState(state, pkBad, pkB)   # -> c(NaN, NaN, NaN), no warning
# This is unreachable for any PK set derived from a genuine mammillary model,
# because the eigenvalues strictly interlace the exchange rate constants
# (lambda_1 > k21 > lambda_2 > k31 > lambda_3; verified for propofol and fentanyl
# at multiple covariate sets while developing these tests), but there is no guard.
#
# Also documented, not asserted: the two-compartment branch (oldPK$lambda_3 == 0)
# ignores oldState[3] entirely and returns 0 in slot 3. That is harmless in
# production because a two-compartment set has p_coef_bolus_l3 = 0, so state[3] is
# identically zero when that branch runs.
#
# Provenance: drafted by Claude Code (Fable 5), 2026-08-14, for the pre-deployment
# test plan (PK/PD engine). Expected values derived from first principles: the
# amount-reconstruction formulas are re-derived by hand from the compartment ODEs
# and cross-validated against an independent RK4 numerical integration; the linear
# solve is cross-validated against base R solve(); conservation and round-trip
# properties are exact mathematical identities of the transformation. No expected
# value was copied from convertState output.

# ---------------------------------------------------------------------------
# Shared fixtures: real PK sets from getDrugPK so field names always match
# production. Two covariate sets per drug, mimicking the in-app scenario of a
# covariate-change PK event. getDrugPK sets satisfy k21 = cl2/v2 and k31 = cl3/v3,
# the same relations advanceClosedForm1 uses when it builds the lists it passes
# to convertState (v2 = v1*k12/k21, v3 = v1*k13/k31), so these fixtures carry
# production-faithful semantics.
pkA <- getDrugPK("propofol", 70, 170, 50, "male", getDrugDefaults("propofol"))$PK$default
pkB <- getDrugPK("propofol", 100, 180, 70, "female", getDrugDefaults("propofol"))$PK$default
pkC <- getDrugPK("fentanyl", 70, 170, 50, "male", getDrugDefaults("fentanyl"))$PK$default
pkD <- getDrugPK("fentanyl", 45, 155, 25, "female", getDrugDefaults("fentanyl"))$PK$default

# Realistic state: the eigen-components of plasma concentration t minutes after an
# IV bolus of `dose` units. This is exactly the state advanceClosedForm1 carries:
# component j after a bolus is dose * p_coef_bolus_lj * exp(-lambda_j * t).
bolusState <- function(pk, dose, t) {
  c(dose * pk$p_coef_bolus_l1 * exp(-pk$lambda_1 * t),
    dose * pk$p_coef_bolus_l2 * exp(-pk$lambda_2 * t),
    dose * pk$p_coef_bolus_l3 * exp(-pk$lambda_3 * t))
}

# Hand-derived compartment amounts (a1, a2, a3) from the eigen-components -- the
# derivation is in the header comment. Written differently from convertState
# (v1*k12 instead of v2*k21, v1*k13 instead of v3*k31 / the code's v1*k13/k31*k31)
# so agreement is not a copy-paste tautology.
compartmentAmounts <- function(state, pk) {
  lambda <- c(pk$lambda_1, pk$lambda_2, pk$lambda_3)
  c(a1 = sum(state) * pk$v1,
    a2 = sum(state / (pk$k21 - lambda)) * pk$v1 * pk$k12,
    a3 = sum(state / (pk$k31 - lambda)) * pk$v1 * pk$k13)
}

stateA <- bolusState(pkA, 100, 10)  # propofol, 10 min after 100 mg bolus
stateC <- bolusState(pkC, 250, 30)  # fentanyl, 30 min after 250 mcg bolus

# ---------------------------------------------------------------------------
test_that("amount-reconstruction formulas match independent RK4 ODE integration", {
  # Ground the algebra that both convertState and these tests rely on: integrate
  # the raw 3-compartment ODEs numerically (classic RK4, no ODE package) from a
  # bolus at t = 0, and compare the compartment amounts at t = 10 with the amounts
  # reconstructed from the closed-form eigen-state. If this passes, the
  # eigenmode reconstruction constants (k21/(k21 - lambda_j) etc.) are correct
  # against the physical model, not merely self-consistent.
  deriv <- function(a, pk) c(
    -(pk$k10 + pk$k12 + pk$k13) * a[1] + pk$k21 * a[2] + pk$k31 * a[3],
    pk$k12 * a[1] - pk$k21 * a[2],
    pk$k13 * a[1] - pk$k31 * a[3]
  )
  nStep <- 4000                      # h = 0.0025 min; RK4 global error ~ h^4
  h <- 10 / nStep
  a <- c(100, 0, 0)                  # 100-unit bolus into compartment 1
  for (i in seq_len(nStep)) {
    k1 <- deriv(a, pkA)
    k2 <- deriv(a + h / 2 * k1, pkA)
    k3 <- deriv(a + h / 2 * k2, pkA)
    k4 <- deriv(a + h * k3, pkA)
    a <- a + h / 6 * (k1 + 2 * k2 + 2 * k3 + k4)
  }
  # Observed agreement is ~2e-14 relative; 1e-10 leaves margin for the RK4
  # truncation error while still catching any wrong constant.
  expect_equal(unname(compartmentAmounts(stateA, pkA)), a, tolerance = 1e-10)
})

# ---------------------------------------------------------------------------
test_that("identity conversion returns the state to floating-point accuracy", {
  # oldPK == newPK: decompose to amounts and solve straight back. Not bitwise
  # identical (the round trip through the f1..f16 elimination reorders floating
  # point operations); probed max relative error ~8e-15, so 1e-12 is tight but
  # safe.
  expect_equal(convertState(stateA, pkA, pkA), stateA, tolerance = 1e-12)
  expect_equal(convertState(stateC, pkC, pkC), stateC, tolerance = 1e-12)
})

# ---------------------------------------------------------------------------
test_that("one-compartment branch (lambda_2 == 0) returns the state untouched", {
  # No drug in the current library is one-compartment, so modify a copy of a real
  # two-compartment set. The branch consults only oldPK$lambda_2 and returns the
  # input vector itself, so this must be exactly identical, not merely equal.
  pkLido <- getDrugPK("lidocaine", 70, 170, 50, "male",
                      getDrugDefaults("lidocaine"))$PK$default
  pk1cpt <- pkLido
  pk1cpt$lambda_2 <- 0
  pk1cpt$lambda_3 <- 0
  state <- c(1.23, 0, 0)
  expect_identical(convertState(state, pk1cpt, pkB), state)
})

# ---------------------------------------------------------------------------
test_that("central-compartment mass is conserved across 3-compartment conversions", {
  # Physical requirement: the switch changes the mathematical basis, not the drug
  # actually present. Plasma amount a1 = sum(state) * v1 must survive exactly.
  # (a2 and a3 conservation is covered in the solve() cross-check test below.)
  # By construction newState1 = a1/newv1 - newState2 - newState3, so this holds to
  # a couple of ulps (probed: exactly 0 for these cases); 1e-13 allows rounding.
  outAB <- convertState(stateA, pkA, pkB)
  expect_equal(sum(outAB) * pkB$v1, sum(stateA) * pkA$v1, tolerance = 1e-13)

  outCD <- convertState(stateC, pkC, pkD)
  expect_equal(sum(outCD) * pkD$v1, sum(stateC) * pkC$v1, tolerance = 1e-13)
})

# ---------------------------------------------------------------------------
test_that("conversion preserves all compartment amounts and matches solve()", {
  for (pair in list(list(state = stateA, old = pkA, new = pkB),
                    list(state = stateC, old = pkC, new = pkD))) {
    out <- convertState(pair$state, pair$old, pair$new)

    # (a) The full physical invariant: amounts in all three compartments,
    # reconstructed independently under each PK set, must agree.
    expect_equal(compartmentAmounts(out, pair$new),
                 compartmentAmounts(pair$state, pair$old),
                 tolerance = 1e-12)

    # (b) Cross-check the hand-coded f1..f16 elimination against base R solve()
    # on the explicit linear system:  rows are  sum(x) = a1/v1,
    # sum_j x_j*v1*k12/(k21-lambda_j) = a2,  sum_j x_j*v1*k13/(k31-lambda_j) = a3.
    am <- compartmentAmounts(pair$state, pair$old)
    lambdaNew <- c(pair$new$lambda_1, pair$new$lambda_2, pair$new$lambda_3)
    M <- rbind(rep(1, 3),
               pair$new$v1 * pair$new$k12 / (pair$new$k21 - lambdaNew),
               pair$new$v1 * pair$new$k13 / (pair$new$k31 - lambdaNew))
    expected <- solve(M, c(am[["a1"]] / pair$new$v1, am[["a2"]], am[["a3"]]))
    # Probed agreement ~5e-15 relative; 1e-12 is ample.
    expect_equal(out, expected, tolerance = 1e-12)
  }
})

# ---------------------------------------------------------------------------
test_that("two-compartment branch (lambda_3 == 0) conserves mass and matches solve()", {
  # Lidocaine is a genuine two-compartment model in the library (cl3 = 0, so
  # k13 = k31 = lambda_3 = 0), exercising the middle branch with production sets.
  pk2A <- getDrugPK("lidocaine", 70, 170, 50, "male",
                    getDrugDefaults("lidocaine"))$PK$default
  pk2B <- getDrugPK("lidocaine", 95, 180, 65, "female",
                    getDrugDefaults("lidocaine"))$PK$default
  expect_identical(pk2A$lambda_3, 0)  # guard: fixture really is two-compartment

  state2 <- bolusState(pk2A, 100, 10)  # third component is exactly 0
  out2 <- convertState(state2, pk2A, pk2B)

  # Slot 3 is hard-coded 0 in this branch
  expect_identical(out2[[3]], 0)

  # Central-compartment mass conservation, as in the 3-compartment case
  expect_equal(sum(out2) * pk2B$v1, sum(state2) * pk2A$v1, tolerance = 1e-13)

  # Cross-check against solve() on the 2x2 system:
  #   x1 + x2 = a1/v1
  #   k21/(k21-lambda_1)*x1 + k21/(k21-lambda_2)*x2 = a2/v2
  a1 <- sum(state2[1:2]) * pk2A$v1
  a2 <- sum(state2[1:2] / (pk2A$k21 - c(pk2A$lambda_1, pk2A$lambda_2))) *
    pk2A$v1 * pk2A$k12
  M2 <- rbind(c(1, 1),
              pk2B$k21 / (pk2B$k21 - c(pk2B$lambda_1, pk2B$lambda_2)))
  expected2 <- solve(M2, c(a1 / pk2B$v1, a2 / pk2B$v2))
  expect_equal(out2[1:2], expected2, tolerance = 1e-12)

  # Round trip. The fast component decays to ~1e-3 of the slow one by t = 10 and
  # its reconstruction loses a few digits to cancellation; probed round-trip
  # relative error ~2.5e-13 on that component, hence the looser 1e-10 here.
  expect_equal(convertState(out2, pk2B, pk2A), state2, tolerance = 1e-10)
})

# ---------------------------------------------------------------------------
test_that("round trip old -> new -> old recovers the original state", {
  # The conversion is a linear bijection (basis change), so converting back must
  # recover the original eigen-components up to floating point. Probed max
  # relative error ~8e-15; 1e-12 is tight but safe.
  rtA <- convertState(convertState(stateA, pkA, pkB), pkB, pkA)
  expect_equal(rtA, stateA, tolerance = 1e-12)

  rtC <- convertState(convertState(stateC, pkC, pkD), pkD, pkC)
  expect_equal(rtC, stateC, tolerance = 1e-12)
})
