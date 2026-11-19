# Tests for the buprenorphine mu-receptor interaction engine
# (R/opioidReceptorInteraction.R).  The occupancy advances are checked against
# independent dense-RK4 integration of the governing ODEs, and the P4
# backward-compatibility identities are checked exactly.

# -- independent references ----------------------------------------------------

# P1: dense RK4 of dx/dt = a(t)(1-x) - koff x, a(t) = (r + s t)/(p + q t).
p1_reference <- function(x0, r, s, p, q, koff, h, N = 200000) {
  dt <- h / N; x <- x0
  f <- function(t, x) ((r + s * t) / (p + q * t)) * (1 - x) - koff * x
  for (i in 0:(N - 1)) {
    t <- i * dt
    k1 <- f(t, x); k2 <- f(t + dt/2, x + dt/2 * k1)
    k3 <- f(t + dt/2, x + dt/2 * k2); k4 <- f(t + dt, x + dt * k3)
    x <- x + dt/6 * (k1 + 2*k2 + 2*k3 + k4)
  }
  x
}

# P2: dense RK4 of the (N+1)-state generator system.
p2_reference <- function(x0, kon, koff, Ce0, Ce1, P0, P1, h, N = 200000) {
  dt <- h / N; n <- length(x0); st <- c(x0, 1 - sum(x0))
  deriv <- function(st, tt) {
    Ce <- Ce0 + (Ce1 - Ce0) * tt / h; Phi <- P0 + (P1 - P0) * tt / h
    a <- kon * Ce / (1 + Phi); rho <- st[1:n]; r0 <- st[n + 1]
    c(a * r0 - koff * rho, sum(koff * rho) - sum(a) * r0)
  }
  for (i in 0:(N - 1)) {
    t <- i * dt
    k1 <- deriv(st, t); k2 <- deriv(st + dt/2 * k1, t + dt/2)
    k3 <- deriv(st + dt/2 * k2, t + dt/2); k4 <- deriv(st + dt * k3, t + dt)
    st <- st + dt/6 * (k1 + 2*k2 + 2*k3 + k4)
  }
  st[1:n]
}

# -- P1: single-slow-ligand advance -------------------------------------------

test_that("P1 advance matches independent integration across regimes", {
  kon <- 0.26; koff <- 0.019  # buprenorphine, naive
  cases <- list(          # x0, B0, B1, Phi0, Phi1, h
    c(0.0, 0.0, 2.0, 0.0, 0.0,  30),   # onset, no crowding
    c(0.5, 1.0, 1.0, 0.0, 5.0,  30),   # crowding rising
    c(0.7, 2.0, 0.5, 5.0, 0.5,  30),   # crowding falling, bup high
    c(0.3, 1.0, 1.0, 2.0, 2.0,  60),   # constant a
    c(0.2, 0.5, 3.0, 0.0, 0.0, 240),   # long stiff step -> sub-stepping
    c(0.9, 3.0, 0.1, 0.1, 8.0,  15)    # deep occupancy, fast change
  )
  for (cc in cases) {
    x0 <- cc[1]; B0 <- cc[2]; B1 <- cc[3]; P0 <- cc[4]; P1 <- cc[5]; h <- cc[6]
    got <- orSlowAdvance(x0, kon, koff, B0, B1, P0, P1, h)
    r <- kon * B0; s <- kon * (B1 - B0) / h; p <- 1 + P0; q <- (P1 - P0) / h
    ref <- p1_reference(x0, r, s, p, q, koff, h)
    expect_equal(got, ref, tolerance = 1e-7)
  }
})

test_that("P1 a == 0 is exact exponential decay", {
  expect_equal(orSlowAdvance(0.4, 0.26, 0.019, 0, 0, 1, 1, 60),
               0.4 * exp(-0.019 * 60), tolerance = 1e-13)
})

# -- P2: several slow ligands --------------------------------------------------

test_that("P2 multi-slow advance (CF4 Magnus) matches independent integration", {
  kon <- c(0.26, 0.20); koff <- c(0.019, 0.010)  # bup + slow antagonist
  x0 <- c(0.6, 0.1); Ce0 <- c(1.5, 0.0); Ce1 <- c(1.0, 2.0)
  got <- orSlowAdvanceMulti(x0, kon, koff, Ce0, Ce1, 3, 1, 30)
  ref <- p2_reference(x0, kon, koff, Ce0, Ce1, 3, 1, 30)
  expect_equal(got, ref, tolerance = 1e-5)
})

test_that("P2 CF4 Magnus converges at (near) 4th order", {
  kon <- c(0.26, 0.20); koff <- c(0.019, 0.010)
  x0 <- c(0.6, 0.1); Ce0 <- c(1.5, 0.0); Ce1 <- c(1.0, 2.0); P0 <- 3; P1 <- 1; h <- 30
  ref <- p2_reference(x0, kon, koff, Ce0, Ce1, P0, P1, h)
  # nsub manual CF4 steps over the interval, Ce/Phi linear within:
  cf4 <- function(nsub) {
    aAt <- function(u) kon * (Ce0 + (Ce1 - Ce0) * u) / (1 + (P0 + (P1 - P0) * u))
    a1 <- 0.25 + sqrt(3)/6; a2 <- 0.25 - sqrt(3)/6
    c1 <- 0.5 - sqrt(3)/6; c2 <- 0.5 + sqrt(3)/6
    st <- c(x0, 1 - sum(x0)); hs <- h / nsub
    for (i in seq_len(nsub)) {
      ua <- (i - 1)/nsub; ub <- i/nsub
      K1 <- orGenerator(aAt(ua + (ub - ua)*c1), koff)
      K2 <- orGenerator(aAt(ua + (ub - ua)*c2), koff)
      st <- as.numeric(expmPade(hs*(a2*K1 + a1*K2)) %*% (expmPade(hs*(a1*K1 + a2*K2)) %*% st))
    }
    st[1:2]
  }
  e2 <- max(abs(cf4(2) - ref)); e4 <- max(abs(cf4(4) - ref)); e8 <- max(abs(cf4(8) - ref))
  # in the asymptotic regime, halving the step cuts the error ~16x (4th order);
  # require well over 8x at each halving
  expect_gt(e2 / e4, 8)
  expect_gt(e4 / e8, 8)
})

# -- occupancy over a trajectory: simplex preservation -------------------------

test_that("occupancy stays on the simplex over a clinical trajectory", {
  times <- seq(0, 240, by = 1)
  ligs <- list(
    list(Ce = pmax(0, 2 - abs(times - 30) / 40), kon = 0.26, koff = 0.019,
         C50 = 0.019 / 0.26, alpha = 0.81, slow = TRUE),                         # bup
    list(Ce = ifelse(times >= 30, pmin(0.3 * (times - 30), 3), 0), kon = 1,
         koff = 1, C50 = 0.22, alpha = 0, slow = FALSE),                         # naloxone-like
    list(Ce = pmax(0, 2 - times / 120), kon = 2, koff = 2, C50 = 0.68,
         alpha = 1, slow = FALSE)                                               # fentanyl
  )
  oc <- orOccupancy(times, ligs)
  expect_equal(max(abs(rowSums(oc$rho) + oc$rho0 - 1)), 0, tolerance = 1e-11)
  expect_gte(min(c(oc$rho, oc$rho0)), -1e-12)
})

# -- P4: backward-compatibility identities -------------------------------------

test_that("P4: crowding equals the additive scale U with C50 = MEAC", {
  MEAC <- c(1.0, 0.6)
  Ce <- cbind(seq(0, 3, length = 50), seq(0, 2, length = 50))
  ligs <- list(
    list(Ce = Ce[, 1], kon = 2, koff = 2 * MEAC[1], C50 = MEAC[1], alpha = 1, slow = FALSE),
    list(Ce = Ce[, 2], kon = 2, koff = 2 * MEAC[2], C50 = MEAC[2], alpha = 1, slow = FALSE)
  )
  oc <- orOccupancy(seq_len(50), ligs)
  U <- Ce[, 1] / MEAC[1] + Ce[, 2] / MEAC[2]
  expect_equal(oc$Phi, U, tolerance = 1e-13)
})

test_that("P4: fast-ligand odds equal U even with buprenorphine present", {
  times <- seq(0, 240, by = 1)
  ligs <- list(
    list(Ce = pmax(0, 2 - abs(times - 30) / 40), kon = 0.26, koff = 0.019,
         C50 = 0.019 / 0.26, alpha = 0.81, slow = TRUE),
    list(Ce = ifelse(times >= 30, pmin(0.3 * (times - 30), 3), 0), kon = 1,
         koff = 1, C50 = 0.22, alpha = 0, slow = FALSE),
    list(Ce = pmax(0, 2 - times / 120), kon = 2, koff = 2, C50 = 0.68,
         alpha = 1, slow = FALSE)
  )
  oc <- orOccupancy(times, ligs)
  odds <- rowSums(oc$rho[, c(2, 3)]) / oc$rho0
  expect_equal(odds, oc$Phi, tolerance = 1e-11)
})

# -- transductions -------------------------------------------------------------

test_that("ventilation ceiling is alpha_B at full buprenorphine occupancy", {
  # one ligand fully occupying: depression -> alpha_B
  expect_equal(orVentilationDepression(matrix(1, 1, 1), 0.81), 0.81)
  expect_equal(orAvailableDrive(0.8, 2.0), 0.4)  # (1 - 0.8) * 2
})
