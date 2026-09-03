# -----------------------------------------------------------------------------
# Provenance
# ----------
# Drafted by Claude Code (Claude Opus 5), 2026-09-02, at the request of
# Steven L. Shafer, to support drugs whose effect is mediated by an active
# metabolite -- codeine, whose analgesia is morphine's, being the clearest case.
#
# STATUS: run and verified on R 4.6.1 by tests/testthat/test-metabolite.R.  The
# closed form is checked against numerical convolution, against an independent
# ODE integration of the two-drug cascade, and against the effect-site
# coefficients already in getDrugPK.R, which are the one-compartment special
# case of the same integral.
# -----------------------------------------------------------------------------
#
# THE MATHEMATICS
# ===============
#
# The parent's unit-bolus plasma concentration is the sum of exponentials the
# rest of the package already works in:
#
#     Cp(t) = SUM_i  p_i exp(-lambda_i t)          p_i = p_coef_bolus_li
#
# Metabolite is formed by a FIRST ORDER TRANSFER FROM THE PLASMA COMPARTMENT,
# with its own rate constant kForm.  Writing A1 for the parent amount in the
# central compartment, R for the ratio of molecular weights (metabolite over
# parent, because formation is molar but concentrations are by mass) and U for a
# unit scale, the rate at which metabolite mass appears is
#
#     formation(t) = kForm * A1(t) * R * U
#                  = kForm * v1 * R * U * Cp(t)
#                  = K Cp(t)
#
# FORMATION DOES NOT ALTER THE PARENT (Shafer, 2026-09-03)
# -------------------------------------------------------
# The transfer is NOT subtracted from the parent's differential equations, and
# that is deliberate rather than an omission.  The parent's clearance was fitted
# to observed plasma concentrations, so it already subsumes whatever portion of
# the drug is eliminated as this metabolite.  Subtracting the pathway again would
# double-count the loss and would make the parent's own model disagree with the
# data it was fitted to.
#
# The consequence is that kForm is completely independent of the parent's PK
# model.  It is not a fraction of k10 and does not scale with the parent's
# clearance; it is its own parameter, added on top of a disposition model that
# stands unchanged.  An earlier version of this file expressed formation as a
# fraction of the parent's elimination clearance, which tied the two together
# and was wrong.
#
# U is needed because simCpCe() carries doses in an internal mass unit chosen by
# each drug's Concentration.Units: mg for a drug reported in mcg/mL, mcg for one
# reported in ng/mL.  A parent and its metabolite need not agree.  Codeine's
# metabolite morphine is reported in mcg/mL and so carried in mg, and a parent
# reported in ng/mL is carried in mcg -- a thousandfold difference that would
# otherwise pass silently into the metabolite curve.
#
# The metabolite then obeys its own disposition, whose unit-bolus response is
# likewise a sum of exponentials in its own right:
#
#     Cm_unit(tau) = SUM_j  m_j exp(-mu_j tau)     m_j = metabolite p_coef_bolus
#
# So the metabolite concentration is the convolution of the two:
#
#     Cm(t) = INT_0^t K Cp(s) Cm_unit(t - s) ds
#           = K SUM_i SUM_j p_i m_j INT_0^t exp(-lambda_i s) exp(-mu_j (t-s)) ds
#           = K SUM_i SUM_j p_i m_j (exp(-lambda_i t) - exp(-mu_j t))/(mu_j - lambda_i)
#
# Collecting terms by exponential leaves a sum over the UNION of the parent's
# and the metabolite's eigenvalues -- six exponentials for two three-compartment
# models -- which is exactly the representation advanceState() already advances:
#
#     coefficient on exp(-lambda_i t) =   K p_i SUM_j [ m_j / (mu_j - lambda_i) ]
#     coefficient on exp(-mu_j t)     = - K m_j SUM_i [ p_i / (mu_j - lambda_i) ]
#
# CONSISTENCY WITH THE EFFECT SITE
# --------------------------------
# The effect site is the one-compartment special case of this same integral, and
# getDrugPK.R already implements it:
#
#     e_coef_bolus_l1  <- p_coef_bolus_l1 / (ke0 - lambda_1) * ke0
#     e_coef_bolus_ke0 <- -e_coef_bolus_l1 - e_coef_bolus_l2 - e_coef_bolus_l3
#
# Put a single metabolite exponential with mu_1 = ke0 and K m_1 = ke0 into the
# formulae above and they reproduce those two lines exactly, including the sign
# and the fact that the mu coefficient is minus the sum of the lambda ones.
# That correspondence is asserted in the tests, so this derivation is pinned
# against code that has been in use for years.
# -----------------------------------------------------------------------------


#' Exponential terms of a disposition model
#'
#' Extracts the non-zero (coefficient, lambda) pairs from a pkSet, which is how
#' the package represents a unit-bolus response.  Compartments a model does not
#' use are carried as zero lambdas and are dropped here.
#'
#' @param pkSet a PK set carrying \code{p_coef_bolus_l1..l3} and
#'   \code{lambda_1..3}
#' @returns a list with numeric vectors \code{coef} and \code{lambda}
#' @keywords internal
dispositionTerms <- function(pkSet)
{
  coef   <- c(pkSet$p_coef_bolus_l1, pkSet$p_coef_bolus_l2, pkSet$p_coef_bolus_l3)
  lambda <- c(pkSet$lambda_1,        pkSet$lambda_2,        pkSet$lambda_3)

  use <- !is.na(lambda) & lambda > 0
  list(coef = coef[use], lambda = lambda[use])
}


#' Coefficients for an active metabolite formed from a parent drug
#'
#' Returns the metabolite's concentration as a sum of exponentials over the
#' union of the parent's and the metabolite's eigenvalues, in the same
#' bolus/infusion coefficient form the rest of the engine uses, so that the
#' result can be advanced by \code{advanceState()} without any new machinery.
#'
#' @param parent the parent drug's PK set
#' @param metabolite the metabolite's own disposition PK set
#' @param kForm first-order rate constant, per minute, for transfer from the
#'   parent's central compartment to the metabolite.  Its own parameter: it is
#'   not a fraction of the parent's elimination and does not scale with the
#'   parent's clearance.
#' @param mwRatio molecular weight of the metabolite divided by that of the
#'   parent.  Formation is molar but concentrations are reported by mass, so
#'   this converts between them.  Defaults to 1.
#' @param unitScale conversion from the parent's internal dose unit to the
#'   metabolite's, normally from \code{metaboliteUnitScale()}.  Defaults to 1,
#'   which is correct only when both drugs share a Concentration.Units.
#'
#' @returns a list with \code{lambda} (the union of eigenvalues),
#'   \code{bolus} and \code{infusion} (coefficients on each), and the scalar
#'   \code{K} used to form them
#' @export
metaboliteCoefficients <- function(parent, metabolite, kForm, mwRatio = 1,
                                   unitScale = 1)
{
  # No upper bound on kForm.  The old 'fraction' form was capped at 1 because it
  # was a share of elimination; a transfer rate constant has no such ceiling.
  stopifnot(kForm >= 0, mwRatio > 0, unitScale > 0)

  P <- dispositionTerms(parent)
  M <- dispositionTerms(metabolite)
  if (length(P$lambda) == 0 || length(M$lambda) == 0)
    stop("Both the parent and the metabolite need at least one exponential term")

  # Mass of metabolite formed per unit of plasma concentration per minute.  The
  # parent's v1 converts concentration to the amount the transfer acts on; the
  # parent's k10 does NOT appear, because formation is independent of it.
  K <- kForm * parent$v1 * mwRatio * unitScale

  # An exact shared eigenvalue would divide by zero; the convolution then takes
  # the t*exp(-lambda t) form instead.  Two independently fitted drugs never
  # share one exactly, so rather than carry that branch we separate the pair by
  # a relative hair and note it.  The perturbation is far below the precision of
  # any published parameter set.
  lamP <- P$lambda
  lamM <- M$lambda
  for (j in seq_along(lamM))
    for (i in seq_along(lamP))
      if (abs(lamM[j] - lamP[i]) < 1e-10 * max(lamM[j], lamP[i]))
        lamM[j] <- lamM[j] * (1 + 1e-8)

  # Coefficient on each parent eigenvalue: K p_i SUM_j m_j/(mu_j - lambda_i)
  coefP <- vapply(seq_along(lamP), function(i)
    K * P$coef[i] * sum(M$coef / (lamM - lamP[i])), numeric(1))

  # Coefficient on each metabolite eigenvalue: -K m_j SUM_i p_i/(mu_j - lambda_i)
  coefM <- vapply(seq_along(lamM), function(j)
    -K * M$coef[j] * sum(P$coef / (lamM[j] - lamP)), numeric(1))

  lambda <- c(lamP, lamM)
  bolus  <- c(coefP, coefM)

  # An infusion is the integral of the bolus response, so each exponential's
  # infusion coefficient is its bolus coefficient over its own eigenvalue --
  # the same relation the parent and effect-site coefficients already use.
  infusion <- bolus / lambda

  list(lambda = lambda, bolus = bolus, infusion = infusion, K = K)
}


#' Metabolite concentration at arbitrary times after a single parent bolus
#'
#' A direct evaluation of the closed form, used by the tests and useful for
#' checking a parameter set without running the full simulation.
#'
#' @param coefs output of \code{metaboliteCoefficients()}
#' @param dose parent bolus dose, in the parent's base mass units
#' @param times times in minutes
#' @returns numeric vector of metabolite concentrations
#' @export
metaboliteAfterBolus <- function(coefs, dose, times)
{
  vapply(times, function(t)
    dose * sum(coefs$bolus * exp(-coefs$lambda * t)), numeric(1))
}


#' Internal dose unit of a drug, in milligrams
#'
#' simCpCe() converts every dose into an internal mass unit implied by the
#' drug's \code{Concentration.Units}, so that dividing by a volume in litres
#' yields the reported concentration directly: a drug reported in mcg/mL carries
#' its doses in mg, one reported in ng/mL carries them in mcg.
#'
#' @param concentrationUnits the drug's \code{Concentration.Units}, "mcg" or "ng"
#' @returns the internal dose unit expressed in milligrams
#' @keywords internal
internalDoseScale <- function(concentrationUnits)
{
  switch(concentrationUnits,
         mcg = 1,      # doses carried in mg
         ng  = 1e-3,   # doses carried in mcg
         stop("Unsupported Concentration.Units: ", concentrationUnits))
}


#' Unit conversion from a parent's dose units to its metabolite's
#'
#' A parent and its metabolite need not report in the same units.  Morphine is
#' reported in mcg/mL and so carried internally in mg; a parent reported in
#' ng/mL is carried in mcg.  Without this factor the metabolite curve would be
#' wrong by a thousandfold, silently and in a plausible-looking direction.
#'
#' @param parentUnits parent drug's \code{Concentration.Units}
#' @param metaboliteUnits metabolite drug's \code{Concentration.Units}
#' @returns the multiplier to apply to metabolite formation
#' @export
metaboliteUnitScale <- function(parentUnits, metaboliteUnits)
{
  internalDoseScale(parentUnits) / internalDoseScale(metaboliteUnits)
}
