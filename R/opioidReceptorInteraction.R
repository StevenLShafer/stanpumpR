# -----------------------------------------------------------------------------
# Buprenorphine mu-opioid receptor interaction engine
# -----------------------------------------------------------------------------
# A pharmacodynamic post-process on the effect-site concentration curves the
# simulator already produces (never a PK engine), in the spirit of
# applyAntiseizureInteractions(): it runs after every opioid is simulated alone.
#
# The mathematics is docs/buprenorphine-interactions-math.md (typeset:
# docs/buprenorphine-interaction.tex).  The model is a competitive receptor-
# binding system: buprenorphine (and long-residence antagonists) bind slowly and
# carry a differential state; the full agonists and naloxone are fast and reach
# conditional equilibrium in the pool the slow ligands leave free.  Occupancy,
# not an adjusted MEAC, is the state.
#
# Normalise the receptor pool to 1.  For ligand i, rho_i is the bound fraction,
# rho_0 = 1 - sum(rho) the free fraction, and
#
#   drho_i/dt = kon_i Ce_i(t) rho_0 - koff_i rho_i                               (1)
#
# Slow ligands (koff below OR_SLOW_KOFF) keep a state; fast ligands collapse to
#
#   rho_j = (1 - sum_slow rho) phi_j / (1 + Phi),  phi_j = Ce_j/C50_j,  Phi = sum phi (2)
#
# C50_j = koff_j/kon_j.  Between grid points every Ce_i(t) is piecewise linear.
#
# This file holds the pure numerics (orSlow*, orOccupancy, the transductions).
# Verified standalone against independent integration; see
# tests/testthat/test-opioid-receptor-interaction.R.
# -----------------------------------------------------------------------------

# Below this koff (1/min) a ligand is "slow" and carries a differential state.
# Buprenorphine 0.019, naltrexone/nalmefene similar; fentanyl/naloxone are far
# faster.  0.1 /min (t1/2 ~7 min) sits well between the two groups.
OR_SLOW_KOFF <- 0.1

# Gauss-Legendre 8-point nodes/weights on [-1, 1] (symmetric pairs).
OR_GL8_NODES <- c(-0.9602898564975363, -0.7966664774136267, -0.5255324099163290,
                  -0.1834346424956498,  0.1834346424956498,  0.5255324099163290,
                   0.7966664774136267,  0.9602898564975363)
OR_GL8_WEIGHTS <- c(0.1012285362903763, 0.2223810344533745, 0.3137066458778873,
                    0.3626837833783620, 0.3626837833783620, 0.3137066458778873,
                    0.2223810344533745, 0.1012285362903763)

# How many "time constants" of decay we allow across one quadrature sub-step
# before splitting it, so Gauss-Legendre-8 stays at machine precision.
OR_SUBSTEP_TARGET <- 1.0
# Also cap the change in the crowding factor (1+Phi) across a quadrature sub-step.
# a(t) = (r+s t)/(p+q t) has a pole at t = -p/q; when 1+Phi surges across a step
# (e.g. a fast bolus drives Phi 0 -> 100 in one grid interval) that pole sits just
# off the interval and collapses Gauss-Legendre accuracy even though the step
# spans few time constants (Gemini's audit, 2026-11-19).  Keeping each sub-step's
# crowding ratio below this bound keeps the pole far (Bernstein ellipse intact).
OR_CROWD_RATIO <- 1.5
# |q| below this (per-minute slope of 1+Phi) is treated as constant crowding, to
# take the error-function limit via a series instead of the m*log form (P1, q->0).
OR_Q_TOL <- 1e-9

# -- P1: the integral of a(u) = (r + s u)/(p + q u) over [t1, t2] ---------------
# Exact; the q->0 branch is the constant-crowding (error-function) limit.
orIntegralA <- function(t1, t2, r, s, p, q)
{
  dt <- t2 - t1
  if (abs(q) <= OR_Q_TOL) {
    # 1 + Phi ~ p constant: a(u) = (r + s u)/p, integral is elementary.
    (r / p) * dt + (s / (2 * p)) * (t2 * t2 - t1 * t1)
  } else {
    bigA <- s / q
    w    <- r - s * p / q
    bigA * dt + (w / q) * log((p + q * t2) / (p + q * t1))
  }
}

orValueA <- function(t, r, s, p, q) (r + s * t) / (p + q * t)

# -- P1: advance x = rho_bup across one sub-step with the bounded kernel --------
# G(t, h) = exp(-(koff (h - t) + integral_a(t, h))) in (0, 1]; never forms e^{+Psi}.
orSlowSubstep <- function(x0, r, s, p, q, koff, h)
{
  if (h <= 0) return(x0)
  intA_0h <- orIntegralA(0, h, r, s, p, q)
  G0 <- exp(-(koff * h + intA_0h))               # G(0, h)
  # source term: integral_0^h G(t, h) a(t) dt by Gauss-Legendre-8
  half <- h / 2
  acc <- 0
  for (k in seq_along(OR_GL8_NODES)) {
    tk <- half + half * OR_GL8_NODES[k]
    Gk <- exp(-(koff * (h - tk) + orIntegralA(tk, h, r, s, p, q)))
    acc <- acc + OR_GL8_WEIGHTS[k] * Gk * orValueA(tk, r, s, p, q)
  }
  G0 * x0 + half * acc
}

# -- P1: advance one slow ligand across a grid interval, sub-stepping if stiff --
# B0/B1 effect-site conc of the slow ligand; Phi0/Phi1 crowding of the fast pool.
orSlowAdvance <- function(x0, kon, koff, B0, B1, Phi0, Phi1, h)
{
  if (h <= 0) return(x0)
  r <- kon * B0
  s <- kon * (B1 - B0) / h
  p <- 1 + Phi0
  q <- (Phi1 - Phi0) / h
  # number of decay time constants across the whole step
  span <- koff * h + abs(orIntegralA(0, h, r, s, p, q))
  # bound the LINEAR change in (1+Phi) per (equal-time) sub-step: the worst
  # sub-step is the one at the smaller 1+Phi, nearest a(t)'s pole at t=-p/q.
  crowdN <- abs(q) * h / ((OR_CROWD_RATIO - 1) * min(p, p + q * h))
  nsub <- max(1L,
              as.integer(ceiling(span / OR_SUBSTEP_TARGET)),
              as.integer(ceiling(crowdN)))
  if (nsub == 1L) return(orSlowSubstep(x0, r, s, p, q, koff, h))
  x <- x0
  for (i in seq_len(nsub)) {
    ta <- (i - 1) * h / nsub
    tb <- i * h / nsub
    # linear interpolation of B and Phi within the step
    Ba <- B0 + (B1 - B0) * ta / h; Bb <- B0 + (B1 - B0) * tb / h
    Pa <- Phi0 + (Phi1 - Phi0) * ta / h; Pb <- Phi0 + (Phi1 - Phi0) * tb / h
    rr <- kon * Ba; ss <- kon * (Bb - Ba) / (tb - ta)
    pp <- 1 + Pa;   qq <- (Pb - Pa) / (tb - ta)
    x <- orSlowSubstep(x, rr, ss, pp, qq, koff, tb - ta)
  }
  x
}

# -- P2: the (N+1)-state Markov generator for association rates a, dissociation koff
# Metzler (off-diagonals >= 0) with zero column sums, so exp(K dt) has column
# sums exactly 1: the state vector is conserved (sums to 1) to rounding.
# NOTE the (N+1)-th state is the AVAILABLE (non-slow) pool x_avail = 1 - sum_slow
# rho = (1+Phi) rho_0, NOT the free fraction rho_0 (reviewers' audit, 2026-11-19).
# a_i = kon_i Ce_i/(1+Phi) already carries the 1/(1+Phi), so a_i * x_avail =
# kon_i Ce_i rho_0 is the correct association term, and (x_1,...,x_N, x_avail) sums
# to 1 with no dPhi/dt term.  Recover rho_0 = x_avail/(1+Phi) and the fast
# occupancies rho_j = x_avail phi_j/(1+Phi) after the step (orOccupancy does this).
orGenerator <- function(a, koff)
{
  n <- length(a)
  K <- matrix(0, n + 1, n + 1)
  for (i in seq_len(n)) {
    K[i, i]     <- -koff[i]
    K[i, n + 1] <- a[i]
    K[n + 1, i] <- koff[i]
  }
  K[n + 1, n + 1] <- -sum(a)
  K
}

# 4th-order commutator-free Magnus (Blanes & Moan): two Gauss nodes, two
# exponentials.  Zero column sums are preserved in each factor, so sum rho = 1 is
# exact; it is 4th order in the within-step variation of the generator.
OR_CF4_C1 <- 0.5 - sqrt(3) / 6
OR_CF4_C2 <- 0.5 + sqrt(3) / 6
OR_CF4_A1 <- 0.25 + sqrt(3) / 6
OR_CF4_A2 <- 0.25 - sqrt(3) / 6

# -- P2: advance several slow ligands across one interval ----------------------
# Sub-step so the generator barely changes across a sub-step (as P1 does); each
# sub-step is a 4th-order CF-Magnus step.  Ce/Phi linear within the interval.
orSlowAdvanceMulti <- function(xslow, kon, koff, Ce0, Ce1, Phi0, Phi1, h)
{
  if (h <= 0) return(xslow)
  aAt <- function(u) {           # u in [0, 1], fraction of the interval
    Ce <- Ce0 + (Ce1 - Ce0) * u
    Phi <- Phi0 + (Phi1 - Phi0) * u
    kon * Ce / (1 + Phi)
  }
  a0 <- aAt(0); a1 <- aAt(1)
  span <- (max(koff) + max(a0, a1)) * h + max(abs(a1 - a0)) * h
  crowdN <- abs(Phi1 - Phi0) / ((OR_CROWD_RATIO - 1) * min(1 + Phi0, 1 + Phi1))
  nsub <- max(1L,
              as.integer(ceiling(span / OR_SUBSTEP_TARGET)),
              as.integer(ceiling(crowdN)))
  hs <- h / nsub
  state <- c(xslow, 1 - sum(xslow))
  n <- length(xslow)
  for (i in seq_len(nsub)) {
    ua <- (i - 1) / nsub; ub <- i / nsub
    K1 <- orGenerator(aAt(ua + (ub - ua) * OR_CF4_C1), koff)
    K2 <- orGenerator(aAt(ua + (ub - ua) * OR_CF4_C2), koff)
    E1 <- expmPade(hs * (OR_CF4_A1 * K1 + OR_CF4_A2 * K2))   # weights earlier node
    E2 <- expmPade(hs * (OR_CF4_A2 * K1 + OR_CF4_A1 * K2))   # weights later node
    state <- as.numeric(E2 %*% (E1 %*% state))               # exp(A2) exp(A1) state
  }
  state[seq_len(n)]
}

# -- Full occupancy over a shared time grid ------------------------------------
# ligands: list of lists each with Ce (numeric, length(times)), kon, koff, C50,
#   alpha, slow (logical).  Returns a list: rho (matrix times x ligands), rho0,
#   and Phi (the fast-pool crowding, == U when C50 == MEAC).
orOccupancy <- function(times, ligands)
{
  nt <- length(times)
  nl <- length(ligands)
  slow <- vapply(ligands, function(l) isTRUE(l$slow), logical(1))
  kon  <- vapply(ligands, function(l) l$kon,  numeric(1))
  koff <- vapply(ligands, function(l) l$koff, numeric(1))
  C50  <- vapply(ligands, function(l) l$C50,  numeric(1))
  Ce   <- vapply(ligands, function(l) l$Ce,   numeric(nt))   # nt x nl
  if (nl == 1L) Ce <- matrix(Ce, nrow = nt)

  rho  <- matrix(0, nt, nl)
  rho0 <- numeric(nt)
  Phi  <- numeric(nt)
  slowIdx <- which(slow)
  fastIdx <- which(!slow)

  # crowding of the fast pool, Phi(t) = sum_fast Ce/C50
  phiFast <- function(i) if (length(fastIdx)) sum(Ce[i, fastIdx] / C50[fastIdx]) else 0

  xslow <- if (length(slowIdx)) numeric(length(slowIdx)) else numeric(0)  # start naive (empty receptors)
  Phi[1] <- phiFast(1)
  if (length(slowIdx)) rho[1, slowIdx] <- xslow
  sumSlow <- sum(rho[1, slowIdx])
  rho0[1] <- (1 - sumSlow) / (1 + Phi[1])
  if (length(fastIdx)) rho[1, fastIdx] <- (1 - sumSlow) * (Ce[1, fastIdx] / C50[fastIdx]) / (1 + Phi[1])

  for (i in seq_len(nt - 1)) {
    h <- times[i + 1] - times[i]
    Phi[i + 1] <- phiFast(i + 1)
    if (length(slowIdx) == 1L) {
      xslow <- orSlowAdvance(xslow, kon[slowIdx], koff[slowIdx],
                             Ce[i, slowIdx], Ce[i + 1, slowIdx], Phi[i], Phi[i + 1], h)
    } else if (length(slowIdx) > 1L) {
      xslow <- orSlowAdvanceMulti(xslow, kon[slowIdx], koff[slowIdx],
                                  Ce[i, slowIdx], Ce[i + 1, slowIdx], Phi[i], Phi[i + 1], h)
    }
    if (length(slowIdx)) rho[i + 1, slowIdx] <- xslow
    sumSlow <- sum(rho[i + 1, slowIdx])
    rho0[i + 1] <- (1 - sumSlow) / (1 + Phi[i + 1])
    if (length(fastIdx))
      rho[i + 1, fastIdx] <- (1 - sumSlow) * (Ce[i + 1, fastIdx] / C50[fastIdx]) / (1 + Phi[i + 1])
  }
  list(rho = rho, rho0 = rho0, Phi = Phi)
}

# -- Transductions -------------------------------------------------------------
# Ventilation depression (fraction of baseline lost), Olofsen linear: sum alpha*rho.
orVentilationDepression <- function(rho, alpha)
  as.numeric(rho %*% alpha)

# Operational response (analgesia), Hill n: R = Theta^n/(1+Theta^n), Theta = gamma*sum(tau*rho).
orOperationalResponse <- function(rho, tau, gamma, n = 1)
{
  Theta <- gamma * as.numeric(rho %*% tau)
  Theta^n / (1 + Theta^n)
}

# Available (unblocked-equivalent) agonist drive: the drive U~ that, with no slow
# ligand and no antagonist, would give the SAME fast-agonist occupancy rhoAgonist
# now present.  Since fast occupancy saturates as U~/(1+U~), the exact equivalent
# drive is the fast-agonist OCCUPANCY ODDS (reviewers' audit, 2026-11-19):
#   U~ = rhoAgonist / (1 - rhoAgonist),   rhoAgonist = sum of fast-agonist rho.
# This is exact and accounts for buprenorphine AND antagonist blockade (both
# reduce rhoAgonist); the earlier (1 - rho_bup) * U held only for U << 1 and no
# antagonist.  rhoAgonist is on the occupancy (binding C50) scale; mapping U~ back
# to analgesic-MEAC units under the dual-C50 scale is a design question for Shafer.
orAvailableDrive <- function(rhoAgonist) rhoAgonist / (1 - rhoAgonist)
