# -----------------------------------------------------------------------------
# The plasma decline after an infusion that held the plasma concentration
# constant (TCI), and the decrement times it gives
# -----------------------------------------------------------------------------
# One implementation for the library: the Duzitol help page
# (R/help-generated.R) and Single Paper Review (R/paper-review-checks.R) both
# call it, so the two cannot disagree (Steven L. Shafer, 2026-11-01: use
# stanpumpR's own code wherever possible).  It is a closed form, not a run of
# simCpCe(), because a figure check inverts it dozens of times per point;
# test-held-infusion.R holds it to simCpCe()'s own plasma-target TCI and to an
# independent matrix-exponential solution.
#
# Decrement times assume the concentration was held constant during the
# infusion, not that the rate was (Shafer, 2026-11-01).  With the plasma
# concentration held at 1 for T minutes from a drug-free start, each
# peripheral amount at the stop is V_p (1 - exp(-k_p1 T)), since V_p k_p1 =
# V1 k_1p; from that state the central amount is the sum of the residues of
#   [a1 prod_p (s + k_p1) + sum_p k_p1 a_p prod_(q != p) (s + k_q1)] / prod_j (s + lambda_j),
# and divided by V1 these are the shares of the maintained concentration each
# exponential carries; they sum to 1.  T = Inf is the steady state, T = 0 a
# bolus into V1.  The exponents are cube()'s, the simulator's own.
#
# (Claude Code, 2026-11-01, from R/duzitol.R's three-compartment version and
# the Single Paper Review's matrix-exponential one, now both replaced.)
# -----------------------------------------------------------------------------

#' The exponentials of the plasma decline after a held infusion
#'
#' @param model a PK set with volumes \code{v1}, \code{v2}, \code{v3} and
#'   clearances \code{cl1}, \code{cl2}, \code{cl3} (as getDrugPK() returns in
#'   \code{$PK$default}, or duzitolModels()); a peripheral compartment with
#'   no volume or no clearance is absent, so one to three compartments
#' @param duration how long the plasma concentration was held, min (Inf for
#'   steady state)
#' @returns a list: \code{coef}, the shares of the maintained concentration
#'   (summing to 1), and \code{lambda}, the exponents (1/min), fastest first
#' @keywords internal
heldInfusionExponentials <- function(model, duration)
{
  size <- function(x) if (length(x) == 1 && is.finite(x) && x > 0) x else 0
  v <- c(size(model$v1), size(model$v2), size(model$v3))
  cl <- c(size(model$cl1), size(model$cl2), size(model$cl3))
  present <- v[2:3] > 0 & cl[2:3] > 0
  k10 <- cl[1] / v[1]
  k1p <- ifelse(present, cl[2:3] / v[1], 0)
  kp1 <- ifelse(present, cl[2:3] / pmax(v[2:3], 1e-300), 0)
  # cube() takes a two-compartment model as compartments 1 and 2.
  if (!present[1] && present[2]) { k1p <- rev(k1p); kp1 <- rev(kp1); v[2:3] <- rev(v[2:3]); present <- rev(present) }
  n <- 1 + sum(present)
  lambda <- sort(cube(k10, k1p[1], k1p[2], kp1[1], kp1[2]), decreasing = TRUE)[seq_len(n)]
  periph <- which(present)
  a <- c(v[1], v[1 + periph] * (1 - exp(-kp1[periph] * duration)))
  k <- kp1[periph]
  coef <- vapply(seq_len(n), function(j) {
    s <- -lambda[j]
    numerator <- a[1] * prod(s + k) +
      sum(vapply(seq_along(k), function(p) k[p] * a[1 + p] * prod(s + k[-p]), numeric(1)))
    numerator / prod(lambda[-j] - lambda[j])
  }, numeric(1)) / v[1]
  list(coef = coef, lambda = lambda)
}

#' The shares of the maintained concentration in each exponential
#'
#' @inheritParams heldInfusionExponentials
#' @returns the coefficients, matching the exponents fastest first; they sum
#'   to 1
#' @keywords internal
heldInfusionCoefficients <- function(model, duration)
  heldInfusionExponentials(model, duration)$coef

#' Plasma concentration after a held infusion stops
#'
#' @inheritParams heldInfusionExponentials
#' @param t times after the stop, min
#' @returns the plasma concentration over the maintained concentration
#' @keywords internal
plasmaAfterHeldInfusion <- function(model, duration, t)
{
  e <- heldInfusionExponentials(model, duration)
  vapply(t, function(x) sum(e$coef * exp(-e$lambda * x)), numeric(1))
}

#' Decrement time after an infusion that held the plasma concentration
#'
#' The time for the plasma concentration to fall by \code{fraction} after an
#' infusion that held it constant for \code{duration} minutes: with fraction
#' 0.5, the context-sensitive half-time; with \code{duration} Inf, the
#' steady-state decrement time.  The search doubles its upper bound until the
#' concentration is below the target and then finds the root, the first
#' crossing because the concentration falls monotonically after such an
#' infusion.
#'
#' @inheritParams heldInfusionExponentials
#' @param fraction the decrease, 0 to 1
#' @returns minutes
#' @keywords internal
heldInfusionDecrementTime <- function(model, duration, fraction = 0.5)
{
  e <- heldInfusionExponentials(model, duration)
  target <- 1 - fraction
  g <- function(x) sum(e$coef * exp(-e$lambda * x)) - target
  hi <- 1
  while (g(hi) > 0 && hi < 1e12) hi <- hi * 2
  stats::uniroot(g, c(0, hi), tol = 1e-10)$root
}
