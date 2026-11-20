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

# -----------------------------------------------------------------------------
# The unified "one physical occupancy" layer (Shafer, 2026-11-19)
# -----------------------------------------------------------------------------
# One PHYSICAL mu-receptor occupancy per ligand (true K_D); ventilation and
# analgesia are two transductions of it.  Calibration forced by the 2-5x MEAC
# analgesic ceiling band with physical efficacy e <= 1:
#   * analgesic threshold occupancy rho* = 0.2 (a full agonist reaches its
#     analgesic threshold at ~20% occupancy -- high opioid analgesic reserve);
#   * full-agonist true K_D = (1-rho*)/rho* * MEAC = 4*MEAC (larger than the
#     in-vitro binding K_D; the gap is receptor reserve, which reconciles a
#     full agonist's high ventilatory potency with a modest binding affinity);
#   * full-agonist ventilation reserve tau_V = 4.53 (= 1/rho at the ventilatory
#     C50 ~ 1.133*MEAC on the 4*MEAC binding scale; MEAC-independent), giving a
#     ventilation ceiling ~0.82.
# These are documented modelling constants; adjust with Shafer's calibration.
OR_RHO_STAR <- 0.2
OR_FULL_AGONIST_KD_FACTOR <- (1 - OR_RHO_STAR) / OR_RHO_STAR    # 4
OR_FULL_AGONIST_TAU_V     <- 4.53

# Grid B: the four Opioid-tolerance levels map to theta via the existing factors.
OR_TOLERANCE_FACTORS <- c(none = 1, mild = 1.5, somewhat = 2, high = 3)
orToleranceTheta <- function(level = "none")
  log(OR_TOLERANCE_FACTORS[[match.arg(level, names(OR_TOLERANCE_FACTORS))]]) / log(3)

# PROTOTYPE (Shafer, 2026-11-20).  The continuous Opioid-sensitivity control
# adds a theta OFFSET to the tolerance theta (R/constants.R): one physical
# occupancy, so a more-sensitive patient is handled on the same theta axis as
# a less-tolerant one (the source papers cannot separate the two).  Clamped
# and sanitised against a forged or blank input.
orSensitivityOffset <- function(sensitivity = 0) {
  s <- suppressWarnings(as.numeric(sensitivity))
  if (length(s) != 1 || is.na(s)) return(0)
  max(OPIOID_SENSITIVITY_MIN, min(OPIOID_SENSITIVITY_MAX, s))
}

# The Buprenorphine onset/offset control multiplies buprenorphine's biophase
# tPeak (ke0), the rate-limiting step for its slow onset and offset (biophase,
# not receptor dissociation: Yassen 2006 t1/2ke0 155 min vs t1/2koff 8.8 min).
# Clamped and sanitised; the buprenorphine model reads it through getDrugPK.
orOffsetScale <- function(offsetScale = 1) {
  s <- suppressWarnings(as.numeric(offsetScale))
  if (length(s) != 1 || is.na(s) || s <= 0) return(1)
  max(BUPRENORPHINE_OFFSET_MIN, min(BUPRENORPHINE_OFFSET_MAX, s))
}

# The explicit receptor constants (buprenorphine, naloxone); other opioids are
# defaulted from their MEAC.  Read once, cached.
orReceptorConstantsCache <- new.env(parent = emptyenv())
orReceptorConstants <- function()
{
  if (is.null(orReceptorConstantsCache$tab)) {
    path <- system.file("extdata", "opioidReceptorConstants.csv", package = "stanpumpR")
    if (!nzchar(path) || !file.exists(path)) {
      # dev / sourced-not-installed: try a few locations relative to cwd
      cand <- c("inst/extdata/opioidReceptorConstants.csv",
                "../../inst/extdata/opioidReceptorConstants.csv",
                file.path("..", "..", "inst", "extdata", "opioidReceptorConstants.csv"))
      hit <- cand[file.exists(cand)]
      path <- if (length(hit)) hit[1] else cand[1]
    }
    orReceptorConstantsCache$tab <- utils::read.csv(path, stringsAsFactors = FALSE)
  }
  orReceptorConstantsCache$tab
}

# The explicit partial-agonist / antagonist ligands (buprenorphine, naloxone):
# the opioid-receptor panels are offered only while one of these is drawn.
opioidReceptorSpecialDrugs <- function() orReceptorConstants()$drug

# logit / inverse-logit for the bounded ventilation-efficacy interpolation
orLogit    <- function(p) log(p / (1 - p))
orInvLogit <- function(x) 1 / (1 + exp(-x))

# Per-ligand receptor parameters at tolerance theta: a list with slow, kon, koff,
# C50 (= true K_D), tauV (ventilation reserve), eAnalg (analgesic efficacy).
# Tolerance interpolates the TRANSDUCTION (alpha_V -> tauV, eAnalg) and the full-
# agonist potency; every true K_D is fixed (binding does not change with tolerance).
orLigandParams <- function(drug, meac = 0, theta = 0)
{
  tab <- orReceptorConstants()
  row <- tab[tab$drug == drug, , drop = FALSE]
  if (nrow(row)) {
    row <- row[1, ]
    KD <- row$KD_true
    if (isTRUE(as.logical(row$slow))) {
      alphaV <- orInvLogit((1 - theta) * orLogit(row$alphaV_naive) +
                           theta * orLogit(row$alphaV_chronic))
      eAnalg <- exp((1 - theta) * log(row$eAnalg_naive) + theta * log(row$eAnalg_chronic))
      list(slow = TRUE, kon = row$koff / KD, koff = row$koff, C50 = KD,
           tauV = alphaV / (1 - alphaV), eAnalg = eAnalg)
    } else {
      # explicit fast ligand (naloxone): pure antagonist, no transduction
      list(slow = FALSE, kon = NA_real_, koff = NA_real_, C50 = KD,
           tauV = row$alphaV_naive, eAnalg = row$eAnalg_naive)
    }
  } else if (meac > 0) {
    # a full agonist, defaulted from its MEAC; tolerance raises its C50 potency
    KD <- OR_FULL_AGONIST_KD_FACTOR * meac * 3^theta       # Grid B potency shift
    list(slow = FALSE, kon = NA_real_, koff = NA_real_, C50 = KD,
         tauV = OR_FULL_AGONIST_TAU_V, eAnalg = 1)
  } else {
    NULL   # not a mu ligand
  }
}

# Ventilation depression (fraction of baseline), operational: Theta = sum tauV*rho,
# D = Theta/(1+Theta).  Bounded in [0,1); buprenorphine's ceiling is alpha_B, a
# full agonist's ~0.82 at full occupancy.
orVentilation <- function(rho, tauV)
{
  Theta <- as.numeric(rho %*% tauV)
  Theta / (1 + Theta)
}

# Analgesia in MEAC-equivalents: U_A = (sum e_i rho_i) / rho*, so one full agonist
# at its MEAC reads 1.0 and buprenorphine enters through its own partial analgesia.
orMeacEquivalent <- function(rho, eAnalg, rhoStar = OR_RHO_STAR)
  as.numeric(rho %*% eAnalg) / rhoStar

# -- The engine-to-plot bridge -------------------------------------------------
# Turn the already-simulated effect-site curves of the mu ligands into the
# occupancy and effect series the panels draw.  The app layer extracts ceList /
# meacList from the drug list and attaches the result; this function is the pure
# computation and is standalone-testable.
#   times       numeric, the shared time grid
#   ceList      named list of effect-site concentration series (one per opioid)
#   meacList    named numeric of MEACs (full agonists; 0/NA for bup/antagonists)
#   toleranceLevel  one of names(OR_TOLERANCE_FACTORS)
# Returns a list with: ligands (names), slow (logical), rho (times x ligands),
#   rho0, physical occupancy; ventilation (depression fraction), analgesia
#   (MEAC-equivalents), availableDrive (fast-agonist occupancy odds), and the
#   per-ligand eAnalg so a panel can split buprenorphine's own contribution.
orInteractionSeries <- function(times, ceList, meacList = NULL, toleranceLevel = "none",
                                sensitivity = 0)
{
  theta <- orToleranceTheta(toleranceLevel) + orSensitivityOffset(sensitivity)
  drugs <- names(ceList)
  meacOf <- function(d) { m <- if (!is.null(meacList)) meacList[[d]] else NULL
                          if (is.null(m) || is.na(m)) 0 else m }
  params <- lapply(drugs, function(d) orLigandParams(d, meac = meacOf(d), theta = theta))
  keep <- !vapply(params, is.null, logical(1))           # only mu ligands
  drugs <- drugs[keep]; params <- params[keep]
  if (!length(drugs)) return(NULL)

  ligands <- lapply(seq_along(drugs), function(i)
    list(Ce = ceList[[drugs[i]]], kon = params[[i]]$kon, koff = params[[i]]$koff,
         C50 = params[[i]]$C50, slow = params[[i]]$slow))
  oc <- orOccupancy(times, ligands)

  tauV   <- vapply(params, function(p) p$tauV,   numeric(1))
  eAnalg <- vapply(params, function(p) p$eAnalg, numeric(1))
  slow   <- vapply(params, function(p) p$slow,   logical(1))

  Theta <- as.numeric(oc$rho %*% tauV)
  ventilation <- Theta / (1 + Theta)
  analgesia   <- as.numeric(oc$rho %*% eAnalg) / OR_RHO_STAR
  agonist <- !slow & eAnalg > 0                          # fast full agonists
  rhoAgonist <- if (any(agonist)) rowSums(oc$rho[, agonist, drop = FALSE]) else numeric(length(times))
  availableDrive <- rhoAgonist / oc$rho0                 # exact unblocked-equiv drive (odds)

  list(ligands = drugs, slow = slow, eAnalg = eAnalg,
       rho = oc$rho, rho0 = oc$rho0,
       ventilation = ventilation, analgesia = analgesia, availableDrive = availableDrive)
}

# -- App layer: build the interaction series from the simulated drug list ------
# Reads each mu ligand's effect-site series (`$wide$"Effect Site"`) and MEAC
# (`$MEAC`) off the drug entries, puts them on a shared time base, and calls
# orInteractionSeries().  Companions (alternative-model entries) are excluded.
# Returns NULL unless at least one partial agonist or antagonist is present (so
# the panels appear only then).
opioidReceptorSeries <- function(drugs, opioidTolerance = "none", sensitivity = 0)
{
  tol <- if (opioidTolerance %in% names(OR_TOLERANCE_FACTORS)) opioidTolerance else "none"
  # theta here only gates which drugs are "special" (slow / antagonist), which
  # the sensitivity offset does not change; orInteractionSeries applies it.
  theta <- orToleranceTheta(tol)
  isCompanion <- function(d) isTRUE(tryCatch(isAlternativeModelEntry(d), error = function(e) FALSE))
  meacOf <- function(d) if (is.null(d$MEAC) || is.na(d$MEAC)) 0 else d$MEAC
  own <- Filter(function(d)
    !is.null(d$wide) && ("Effect Site" %in% names(d$wide)) && !is.null(d$drug) &&
    !isCompanion(d) && !is.null(orLigandParams(d$drug, meac = meacOf(d), theta = theta)),
    drugs)
  if (!length(own)) return(NULL)
  # require a partial agonist or antagonist (a slow ligand or an e = 0 antagonist)
  special <- any(vapply(own, function(d) {
    p <- orLigandParams(d$drug, meac = meacOf(d), theta = theta)
    isTRUE(p$slow) || isTRUE(p$eAnalg == 0)
  }, logical(1)))
  if (!special) return(NULL)

  base <- sort(unique(unlist(lapply(own, function(d) d$wide$Time))))
  ceList <- lapply(own, function(d)
    stats::approx(d$wide$Time, d$wide[["Effect Site"]], base, rule = 2)$y)
  names(ceList) <- vapply(own, function(d) d$drug, character(1))
  meacList <- as.list(vapply(own, meacOf, numeric(1)))
  names(meacList) <- names(ceList)

  res <- orInteractionSeries(base, ceList, meacList, tol, sensitivity)
  if (!is.null(res)) res$Time <- base
  res
}
