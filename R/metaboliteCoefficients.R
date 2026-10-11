# -----------------------------------------------------------------------------
# Provenance
# ----------
# Drafted by Claude Code (Claude Opus 5), 2026-09-02, at the request of
# Steven L. Shafer, to support drugs whose effect is mediated by an active
# metabolite -- codeine, whose analgesia is morphine's, being the clearest case.
#
# Extended 2026-10-05 (Claude Opus 5) with the oral route: the same convolution
# kernel now serves a bolus, an infusion and an oral dose, and a first-pass
# branch delivers metabolite formed before the parent reaches the systemic
# circulation.
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
# with its own rate constant kFormation.  Writing A1 for the parent amount in the
# central compartment, R for the ratio of molecular weights (metabolite over
# parent, because formation is molar but concentrations are by mass) and U for a
# unit scale, the rate at which metabolite mass appears is
#
#     formation(t) = kFormation * A1(t) * R * U
#                  = kFormation * v1 * R * U * Cp(t)
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
# The consequence is that kFormation is completely independent of the parent's PK
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
# THE ORAL ROUTE
# --------------
# Nothing above depends on the driving concentration being a bolus response.
# Any sum of exponentials will do, so the oral case reuses the same kernel with
# the parent's ORAL plasma response as the driving set:
#
#     Cp_PO(t) = SUM_i a_i exp(-lambda_i t) + a_ka exp(-ka t)
#     a_i      = p_i ka/(ka - lambda_i) F        a_ka = -SUM_i a_i
#
# which is exactly what getDrugPK writes into p_coef_PO_*.  The convolution then
# runs over the parent's eigenvalues, ka, and the metabolite's.
#
# Oral dosing adds a second, parallel route.  Some of the dose is converted
# before it ever reaches the systemic circulation, and that metabolite appears
# directly in the metabolite's own central compartment.  It is not a convolution
# through the parent at all: it is the metabolite's own oral response, scaled by
# the fraction converted.  The two routes are summed.
#
# They are separate parameters because they are separate processes with
# different time courses -- first-pass metabolite appears with the absorption
# kernel, systemic metabolite appears only after the parent has been absorbed
# and then converted.  Oral data alone constrain only their SUM; separating them
# needs intravenous parent data as well.
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


#' Separate eigenvalues that coincide
#'
#' The convolution divides by (mu - lambda), so an exact tie is a division by
#' zero.  Two independently fitted drugs never share an eigenvalue exactly, and
#' the degenerate case takes a t*exp(-lambda t) form that is not worth carrying,
#' so a tie is broken by a relative hair.  The perturbation is far below the
#' precision of any published parameter set.
#'
#' @param lamM eigenvalues to adjust
#' @param lamP eigenvalues to separate them from
#' @returns \code{lamM}, with any exact tie nudged
#' @keywords internal
separateEigenvalues <- function(lamM, lamP)
{
  for (j in seq_along(lamM))
    for (i in seq_along(lamP))
      if (abs(lamM[j] - lamP[i]) < 1e-10 * max(lamM[j], lamP[i]))
        lamM[j] <- lamM[j] * (1 + 1e-8)
  lamM
}


#' Convolve a driving concentration with a disposition response
#'
#' Both are sums of exponentials, so the convolution is again a sum of
#' exponentials over the union of the two eigenvalue sets.  This is the kernel
#' of the derivation at the top of this file, factored out so that the same
#' algebra serves a bolus, an infusion and an oral dose: only the driving term
#' set changes.
#'
#' @param P driving terms, a list of \code{coef} and \code{lambda}
#' @param M disposition terms of the formed species, same shape
#' @param K mass formed per unit driving concentration per minute
#'
#' @returns a list of \code{lambda} (driving eigenvalues, then disposition
#'   eigenvalues) and \code{coef}
#' @keywords internal
convolveExponentials <- function(P, M, K)
{
  lamP <- P$lambda
  lamM <- separateEigenvalues(M$lambda, lamP)

  # Coefficient on each driving eigenvalue: K p_i SUM_j m_j/(mu_j - lambda_i)
  coefP <- vapply(seq_along(lamP), function(i)
    K * P$coef[i] * sum(M$coef / (lamM - lamP[i])), numeric(1))

  # Coefficient on each disposition eigenvalue: -K m_j SUM_i p_i/(mu_j - lambda_i)
  coefM <- vapply(seq_along(lamM), function(j)
    -K * M$coef[j] * sum(P$coef / (lamM[j] - lamP)), numeric(1))

  list(lambda = c(lamP, lamM), coef = c(coefP, coefM))
}


#' Coefficients for an active metabolite formed from a parent drug
#'
#' Returns the metabolite's concentration as a sum of exponentials over the
#' union of the parent's and the metabolite's eigenvalues, in the same
#' bolus/infusion coefficient form the rest of the engine uses, so that the
#' result can be advanced by \code{advanceState()} without any new machinery.
#'
#' When the parent carries a first-order oral absorption constant, a third
#' coefficient vector is returned for an oral dose.  It is the sum of the
#' systemic route, in which absorbed parent is converted after reaching the
#' central compartment, and the first-pass route, in which a fraction of the
#' dose is converted before reaching the systemic circulation and appears
#' directly in the metabolite's central compartment.
#'
#' @param parent the parent drug's PK set
#' @param metabolite the metabolite's own disposition PK set
#' @param kFormation first-order rate constant, per minute, for transfer from the
#'   parent's central compartment to the metabolite.  Its own parameter: it is
#'   not a fraction of the parent's elimination and does not scale with the
#'   parent's clearance.
#' @param mwRatio molecular weight of the metabolite divided by that of the
#'   parent.  Formation is molar but concentrations are reported by mass, so
#'   this converts between them.  Defaults to 1.
#' @param unitScale conversion from the parent's internal dose unit to the
#'   metabolite's, normally from \code{metaboliteUnitScale()}.  Defaults to 1,
#'   which is correct only when both drugs share a Concentration.Units.
#' @param firstPassFraction fraction of an oral parent dose appearing directly
#'   as metabolite, having been converted before reaching the systemic
#'   circulation.  Defaults to 0.  Ignored when the parent has no oral
#'   absorption constant.
#'
#' @returns a list with \code{lambda} (the union of eigenvalues: parent, then
#'   metabolite, then the absorption constant when there is one),
#'   \code{bolus}, \code{infusion} and \code{PO} coefficients on each, and the
#'   scalar \code{K} used to form them.  A parent with a second oral depot
#'   (\code{ka_PO2}) adds its absorption constant after the first and a
#'   \code{PO2} vector for the doses that take that path.
#' @export
metaboliteCoefficients <- function(parent, metabolite, kFormation, mwRatio = 1,
                                   unitScale = 1, firstPassFraction = 0)
{
  # No upper bound on kFormation.  The old 'fraction' form was capped at 1 because it
  # was a share of elimination; a transfer rate constant has no such ceiling.
  stopifnot(kFormation >= 0, mwRatio > 0, unitScale > 0,
            firstPassFraction >= 0, firstPassFraction <= 1)

  P <- dispositionTerms(parent)
  M <- dispositionTerms(metabolite)
  if (length(P$lambda) == 0 || length(M$lambda) == 0)
    stop("Both the parent and the metabolite need at least one exponential term")

  # Mass of metabolite formed per unit of plasma concentration per minute.  The
  # parent's v1 converts concentration to the amount the transfer acts on; the
  # parent's k10 does NOT appear, because formation is independent of it.
  K <- kFormation * parent$v1 * mwRatio * unitScale

  iv     <- convolveExponentials(P, M, K)
  lambda <- iv$lambda
  bolus  <- iv$coef

  # An infusion is the integral of the bolus response, so each exponential's
  # infusion coefficient is its bolus coefficient over its own eigenvalue --
  # the same relation the parent and effect-site coefficients already use.
  infusion <- bolus / lambda

  nP <- length(P$lambda)
  nM <- length(M$lambda)

  # The oral response through one first-order depot, on the vector
  # (parent eigenvalues, metabolite eigenvalues, ka): the systemic route plus
  # the first-pass route (a fraction `fp` of the dose converted before
  # reaching the systemic circulation, entering the metabolite's central
  # compartment through the same absorption step).
  oralVector <- function(ka, bio, fp)
  {
    # The parent's own plasma response to a unit oral dose, which is what
    # getDrugPK writes into p_coef_PO_*: the bolus response retarded by
    # first-order absorption and scaled by bioavailability.
    lamPO <- separateEigenvalues(P$lambda, ka)
    aLam  <- P$coef * ka / (ka - lamPO) * bio
    drive <- list(coef = c(aLam, -sum(aLam)), lambda = c(lamPO, ka))

    # convolveExponentials returns driving eigenvalues (parent, then ka)
    # followed by the metabolite's.  Reorder onto the shared vector, which
    # carries ka last so that the bolus and infusion vectors keep the ordering
    # the intravenous case has always had.
    systemic <- convolveExponentials(drive, M, K)$coef
    out <- c(systemic[seq_len(nP)],
             systemic[nP + 1 + seq_len(nM)],
             systemic[nP + 1])

    if (fp > 0)
    {
      scale <- fp * mwRatio * unitScale
      muFP  <- separateEigenvalues(M$lambda, ka)
      fpMu  <- M$coef * ka / (ka - muFP) * scale
      out   <- out + c(rep(0, nP), fpMu, -sum(fpMu))
    }
    out
  }

  ka  <- if (is.null(parent$ka_PO)) 0 else parent$ka_PO
  ka2 <- if (is.null(parent$ka_PO2)) 0 else parent$ka_PO2
  if (ka > 0)
  {
    bio  <- if (is.null(parent$bioavailability_PO)) 1 else parent$bioavailability_PO
    bio2 <- if (ka2 > 0) parent$bioavailability_PO2 else 0
    # With a second oral depot (getDrugPK()), the dose is shared between the
    # depots in proportion to their bioavailabilities, and so is the
    # fraction converted on first pass.
    share <- if (bio + bio2 > 0) bio / (bio + bio2) else 1
    PO <- oralVector(ka, bio, firstPassFraction * share)

    lambda   <- c(lambda, ka)
    bolus    <- c(bolus, 0)
    infusion <- c(infusion, 0)

    if (ka2 > 0)
    {
      v2  <- oralVector(ka2, bio2, firstPassFraction * (1 - share))
      PO2 <- c(v2[seq_len(nP + nM)], 0, v2[nP + nM + 1])
      PO  <- c(PO, 0)
      lambda   <- c(lambda, ka2)
      bolus    <- c(bolus, 0)
      infusion <- c(infusion, 0)
    }
  } else {
    PO <- rep(0, length(lambda))
  }

  out <- list(lambda = lambda, bolus = bolus, infusion = infusion, PO = PO, K = K)
  # Only a parent with a second oral depot carries PO2, so that every other
  # drug's coefficients keep exactly the shape they always had.
  if (ka > 0 && ka2 > 0) out$PO2 <- PO2
  out
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


#' Metabolite concentration at arbitrary times after a single oral parent dose
#'
#' The oral counterpart of \code{metaboliteAfterBolus()}, carrying both the
#' systemic and the first-pass route.  Times are measured from the dose, after
#' any absorption lag has been applied by the caller.
#'
#' @param coefs output of \code{metaboliteCoefficients()}
#' @param dose oral parent dose, in the parent's base mass units
#' @param times times in minutes
#' @returns numeric vector of metabolite concentrations
#' @export
metaboliteAfterOral <- function(coefs, dose, times)
{
  vapply(times, function(t)
    dose * sum(coefs$PO * exp(-coefs$lambda * t)), numeric(1))
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
