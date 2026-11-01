# -----------------------------------------------------------------------------
# Duzitol: why a table of pharmacokinetic parameters must be simulated
# -----------------------------------------------------------------------------
# Provenance
# ----------
# Claude Code, 2026-11-01, at the request of Steven L. Shafer, for the help
# page inst/help/models/why-simulate.md.  Verified by
# tests/testthat/test-duzitol.R.
#
# Shafer SL, Stanski DR. Improving the clinical utility of anesthetic drug
# pharmacokinetics. Anesthesiology 1992;76:327-330, the editorial beside
# Hughes, Glass and Jacobs's context-sensitive half-time.  Its Table 1 gives
# three models as the three half-lives, the metabolic clearance, V1 and Vdss:
# a fictional drug, Duzitol ("a drug that does it all"), in normal subjects
# and in hepatic failure, and propofol in normal subjects.  Read by eye, the
# hepatic failure column (every half-life four times longer, a smaller
# clearance, a larger Vdss) says recovery will be slowest; simulated, it is
# the fastest, with 50% decrement times after steady-state infusions of 41,
# 10 and 20 min.
#
# Duzitol is NOT a drug in the library, deliberately: a fictional drug cannot
# be offered in the dose table beside clinical models, so the page draws its
# curves from these functions (helpDuzitolFigure() and
# helpDuzitolModelTableMarkdown() in R/help-generated.R) instead of loading
# a scenario.
#
# threeCompartmentFromHalfLives() is Shafer's spreadsheet algebra (the "(3).
# V1, Vdss, Cl, half-lives" sheet he built the editorial's models with),
# copied from the Single Paper Review branch, where it reproduced those
# spreadsheets to every printed digit; the derivation is in the help page's
# appendix.  If that branch is merged, one of the two copies goes.
# -----------------------------------------------------------------------------

#' Table 1 of the Duzitol editorial
#'
#' One row per model, as printed: the three half-lives (min), the metabolic
#' clearance (L/min), V1 and Vdss (L), and the 50% decrement time after a
#' steady-state infusion that the editorial's text gives (min).
#' @noRd
DUZITOL_TABLE <- data.frame(
  model = c("Duzitol, normal subjects", "Duzitol, hepatic failure",
            "Propofol, normal subjects"),
  t1  = c(1.0, 4.0, 0.8),
  t2  = c(10.0, 40.0, 7.8),
  t3  = c(100, 400, 198),
  CL  = c(0.68, 0.51, 1.9),
  V1  = c(7.6, 5.0, 6.3),
  Vss = c(66, 93, 284),
  decrement50 = c(41, 10, 20),
  stringsAsFactors = FALSE
)

#' Three-compartment parameters from CL, V1, Vss and the three half-lives
#'
#' Steven L. Shafer's spreadsheet algebra.  With \eqn{k_{10} = CL/V_1} and
#' the three exponents \eqn{\lambda = \ln 2 / t_{1/2}}:
#' \eqn{k_{21} k_{31} = \lambda_1\lambda_2\lambda_3 / k_{10} = g_0};
#' \eqn{k_{12} k_{31} + k_{13} k_{21} = (V_{ss}/V_1 - 1) g_0 = g_1};
#' \eqn{k_{21} + k_{31} = (\sum_{i<j} \lambda_i\lambda_j - g_0 - g_1)/k_{10} = f},
#' so \eqn{k_{21}} and \eqn{k_{31}} are the roots of \eqn{x^2 - f x + g_0};
#' \eqn{k_{12} + k_{13} = \sum\lambda - k_{10} - k_{21} - k_{31} = i}; and
#' \eqn{k_{13} = (g_1 - i k_{31}) / (k_{21} - k_{31})}.  Refuses a set of
#' numbers with no admissible mammillary model.
#'
#' @param CL,V1,Vss clearance and volumes, in consistent units
#' @param t1,t2,t3 the three half-lives, in any order, in the time unit of CL
#' @returns a list of v1, v2, v3, cl1, cl2, cl3, compartment 2 the faster
#' @keywords internal
threeCompartmentFromHalfLives <- function(CL, V1, Vss, t1, t2, t3)
{
  l <- log(2) / sort(c(t1, t2, t3))
  k10 <- CL / V1
  g0 <- prod(l) / k10
  g1 <- (Vss - V1) / V1 * g0
  f <- (l[1] * l[2] + l[1] * l[3] + l[2] * l[3] - g1 - g0) / k10
  disc <- f^2 - 4 * g0
  if (!is.finite(disc) || disc <= 0)
    stop("These numbers have no physically admissible three-compartment model")
  k21 <- (f + sqrt(disc)) / 2
  k31 <- (f - sqrt(disc)) / 2
  i <- sum(l) - k10 - k21 - k31
  k13 <- (g1 - i * k31) / (k21 - k31)
  k12 <- i - k13
  if (!all(is.finite(c(k12, k13, k21, k31))) || !all(c(k12, k13, k21, k31) > 0))
    stop("These numbers have no physically admissible three-compartment model")
  list(v1 = V1, v2 = V1 * k12 / k21, v3 = V1 * k13 / k31,
       cl1 = CL, cl2 = V1 * k12, cl3 = V1 * k13)
}

#' The three models of Table 1, built
#'
#' @returns a list, one element per row of DUZITOL_TABLE and named by its
#'   model, of the volumes and clearances from threeCompartmentFromHalfLives()
#'   plus the micro rate constants and the exponents (1/min, fastest first)
#' @noRd
duzitolModels <- function()
{
  out <- lapply(seq_len(nrow(DUZITOL_TABLE)), function(r) {
    t <- DUZITOL_TABLE[r, ]
    m <- threeCompartmentFromHalfLives(t$CL, t$V1, t$Vss, t$t1, t$t2, t$t3)
    m$k10 <- m$cl1 / m$v1
    m$k12 <- m$cl2 / m$v1
    m$k13 <- m$cl3 / m$v1
    m$k21 <- m$cl2 / m$v2
    m$k31 <- m$cl3 / m$v3
    m$lambda <- cube(m$k10, m$k12, m$k13, m$k21, m$k31)
    m
  })
  stats::setNames(out, DUZITOL_TABLE$model)
}

#' The exponentials of the plasma decline after a TCI infusion
#'
#' After an infusion that held the plasma concentration at 1 for
#' \code{duration} minutes from a drug-free start (Hughes, Glass and Jacobs's
#' context), the peripheral amounts at the stop are
#' \eqn{V_2 (1 - e^{-k_{21} T})} and \eqn{V_3 (1 - e^{-k_{31} T})}, and from
#' that state the central amount is the sum of the residues of
#' \eqn{[a_1 (s + k_{21})(s + k_{31}) + k_{21} a_2 (s + k_{31}) + k_{31} a_3 (s + k_{21})] / \prod (s + \lambda_j)}.
#' Divided by V1 they are the share of the maintained concentration that
#' each exponential carries, and they sum to 1.  A \code{duration} of
#' \code{Inf} is the steady state.
#'
#' @param model one element of duzitolModels()
#' @param duration the length of the infusion, min (Inf for steady state)
#' @returns three coefficients, matching model$lambda (fastest first)
#' @noRd
heldInfusionCoefficients <- function(model, duration)
{
  m <- model
  a1 <- m$v1
  a2 <- m$v2 * (1 - exp(-m$k21 * duration))
  a3 <- m$v3 * (1 - exp(-m$k31 * duration))
  l <- m$lambda
  vapply(1:3, function(j) {
    s <- -l[j]
    (a1 * (s + m$k21) * (s + m$k31) + m$k21 * a2 * (s + m$k31) +
       m$k31 * a3 * (s + m$k21)) / prod(l[-j] - l[j])
  }, numeric(1)) / m$v1
}

#' Plasma concentration after a TCI infusion that held it constant
#'
#' @param model one element of duzitolModels()
#' @param duration the length of the infusion, min (Inf for steady state)
#' @param t times after the stop, min
#' @returns the plasma concentration over the maintained concentration
#' @noRd
plasmaAfterHeldInfusion <- function(model, duration, t)
{
  coef <- heldInfusionCoefficients(model, duration)
  vapply(t, function(x) sum(coef * exp(-model$lambda * x)), numeric(1))
}

#' Decrement time after a TCI infusion that held the plasma constant
#'
#' The time for the plasma concentration to fall by \code{fraction} after an
#' infusion that held it constant for \code{duration} minutes: with fraction
#' 0.5, the context-sensitive half-time.  The search doubles its upper bound
#' until the concentration is below the target and then finds the root, which
#' is the first crossing because the concentration falls monotonically after
#' such an infusion for these models (test-duzitol.R checks it on a dense grid).
#'
#' @param model one element of duzitolModels()
#' @param duration the length of the infusion, min (Inf for steady state)
#' @param fraction the decrease, 0 to 1
#' @returns minutes
#' @noRd
heldInfusionDecrementTime <- function(model, duration, fraction = 0.5)
{
  target <- 1 - fraction
  g <- function(x) plasmaAfterHeldInfusion(model, duration, x) - target
  hi <- 1
  while (g(hi) > 0) hi <- hi * 2
  stats::uniroot(g, c(0, hi), tol = 1e-10)$root
}
