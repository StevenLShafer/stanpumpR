# -----------------------------------------------------------------------------
# One transit compartment before an absorption depot, and a continuous
# enteral infusion through it
# -----------------------------------------------------------------------------
# Drafted by Claude Code, 2026-10-19, at the request of Steven L. Shafer, for
# levodopa (Othman & Dutta 2014: R/drugs_levodopa.R and
# R/drugs_levodopaIntestinalGel.R), whose oral tablet and intestinal gel are
# absorbed through a dosing compartment and one transit compartment, both
# emptied at the same rate constant Ktr, before the central compartment.
#
# THE ABSORPTION
# ==============
# Two equal first-order steps in sequence deliver a dose D with
# bioavailability F into the central compartment at the rate
#
#     I(t) = D F k^2 t exp(-k t),
#
# the gamma (Erlang-2) input, whose t exp(-k t) term is not a sum of
# exponentials and so is not carried by the closed-form engines.  Two steps
# with rates k1 and k2 that differ are, exactly,
#
#     I(t) = D F k1 k2 / (k2 - k1) (exp(-k1 t) - exp(-k2 t))
#          = D F [w1 k1 exp(-k1 t) + w2 k2 exp(-k2 t)],
#     w1 = k2 / (k2 - k1),  w2 = -k1 / (k2 - k1),  w1 + w2 = 1:
#
# two PARALLEL first-order depots fed by the same dose, the second with a
# negative share.  The engines already carry two parallel depots on each of
# the oral route (PO and PO2) and the tissue route (RA and RAslow), with the
# split of the dose in their bioavailabilities, so a PK set that declares
# transit_PO (or transit_RA) is given the second depot by getDrugPK() at
# k2 = k1 (1 + TRANSIT_RATE_SEPARATION) and the weights above, computed from
# the rate constants actually used (after separateEigenvalues()).  The
# equal-rate input is the limit of this one as k2 approaches k1, so the
# curve is the published model's to a relative 1e-5 (TRANSIT_RATE_SEPARATION;
# test-transit.R measures it against an independent numerical solution),
# and the cancellation of the two large opposite depots costs about 1e-11 in
# double precision.  This is the same device as the library's tie-breaking of
# equal rate constants (separateEigenvalues()), at a separation chosen to
# keep both errors far below plotting precision.
#
# The two depots are internal.  The PK set keeps what the model declared in
# `transit` (route, ka, bioavailability, lag) for the help, and the
# bioavailabilities of the two depots in the engine's fields are the weights
# times F, one large and positive, one large and negative.
#
# AN ENTERAL INFUSION
# ===================
# Levodopa-carbidopa intestinal gel is pumped continuously into the jejunum
# through a PEG-J tube, with a morning dose and extra doses through the same
# tube.  A drug that declares `enteralInfusion = TRUE` offers the rate unit
# "mg/hr PO" (enteralRateUnits) beside its "mg PO": getDrugPK() moves its
# oral absorption onto the tissue depot (oralReleaseToTissueDepot(),
# R/subcutaneous.R, as canagliflozin's oral release is moved), transit
# included, and simCpCe() flags its PO rows as tissue-depot doses and its
# "mg/hr PO" rows as a rate into the depot (RArate), which
# advanceClosedFormPO_IM_IN() carries exactly, as it carries a perineural
# infusion ("mg/hr RA").  "PO" is the route word because the gel is absorbed
# from the gut as an oral dose is; the drug's name says it is the gel.  Such
# a drug may have no tissue route of its own, and no other drug may list
# "mg/hr PO" (simCpCe() refuses the row).
# -----------------------------------------------------------------------------

#' Relative separation of the two depots that carry one transit compartment
#' @keywords internal
TRANSIT_RATE_SEPARATION <- 1e-5

#' The two parallel depots equivalent to a depot and one transit compartment
#'
#' @param k1 the depot's (and transit compartment's) rate constant, 1/min,
#'   already separated from the disposition's eigenvalues and ke0
#' @param avoid the rate constants the second depot must not tie with
#' @returns a list of the second depot's rate constant \code{k2} and the
#'   shares \code{w1} and \code{w2} of the dose the two depots carry
#' @keywords internal
transitChainDepots <- function(k1, avoid)
{
  k2 <- separateEigenvalues(k1 * (1 + TRANSIT_RATE_SEPARATION), c(avoid, k1))
  list(k2 = k2, w1 = k2 / (k2 - k1), w2 = -k1 / (k2 - k1))
}

#' Check a drug model's transit declarations
#'
#' A PK set may declare \code{transit_PO = TRUE} (one transit compartment
#' between the oral depot and the central compartment, emptied at
#' \code{ka_PO}) or, for the tissue depot, \code{transit_RA = TRUE}.  The
#' depot pair this makes is carried by the engines' second depot, so the set
#' may not declare one of its own, and the other oral machineries that read
#' or rewrite the oral rows are refused.
#'
#' @param X the whole model list
#' @param drug the drug's name, for the error message
#' @returns TRUE if any set declares a transit compartment, invisibly
#' @keywords internal
validateTransit <- function(X, drug)
{
  bad <- function(why) stop("Invalid transit compartment for ", drug, ": ", why)
  any <- FALSE
  for (s in X$PK)
  {
    if (isTRUE(s$transit_PO))
    {
      any <- TRUE
      if (!isTRUE(s$ka_PO > 0)) bad("transit_PO needs ka_PO.")
      if (!is.null(s$ka_PO2)) bad("transit_PO uses the second oral depot; declare no ka_PO2.")
    }
    if (isTRUE(s$transit_RA))
    {
      any <- TRUE
      if (!isTRUE(s$ka_RA > 0)) bad("transit_RA needs ka_RA (or ka_SC).")
      if (!is.null(s$ka_RA_slow)) bad("transit_RA uses the slow tissue depot; declare no ka_RA_slow.")
    }
  }
  # A dose-dependent oral bioavailability is allowed: simCpCe() scales each
  # oral dose before it copies the rows for the second depot, so both depots
  # carry the scaled dose (opicapone).  Not with an enteral infusion, whose
  # oral rows are tissue rows by then.
  if (isTRUE(X$enteralInfusion) && !is.null(X$oralSaturation))
    bad("an enteral infusion cannot have a dose-dependent oral bioavailability.")
  if (any && (!is.null(X$metabolite) || !is.null(X$oralFormulations) ||
              !is.null(X$oralPulses) ||
              !is.null(X$zeroOrderPO) || !is.null(X$oralZeroOrder) ||
              !is.null(X$zeroOrderSC) || !is.null(X$parallelSystems) ||
              !is.null(X$michaelisMenten) || isTRUE(X$doseDependent)))
    bad(paste0("not available with an active metabolite, several oral forms, ",
               "pulsed or zero-order oral input, zero-order SC input, ",
               "parallel systems, Michaelis-Menten elimination or dose-dependent PK."))
  invisible(any)
}

#' Check a drug model's enteral infusion declaration
#'
#' @param flag the model's \code{enteralInfusion}, or NULL
#' @param X the whole model list
#' @param drug the drug's name, for the error message
#' @returns TRUE when the drug is infused enterally, else FALSE
#' @keywords internal
validateEnteralInfusion <- function(flag, X, drug)
{
  if (is.null(flag)) return(FALSE)
  bad <- function(why) stop("Invalid enteralInfusion for ", drug, ": ", why)
  if (!isTRUE(flag)) bad("must be TRUE when present.")
  for (s in X$PK)
  {
    if (!isTRUE(s$ka_PO > 0)) bad("the drug must be absorbed from the gut (ka_PO).")
    if (any(grepl("_(RA|SC)(_slow)?$", names(s))))
      bad("the drug may have no tissue route: the infusion rides on the tissue depot.")
    if (!is.null(s$ka_PO2)) bad("the infusion cannot feed a second oral depot.")
  }
  TRUE
}

#' Carry a PK set's gut absorption, transit included, on the tissue depot
#'
#' @param pkSet one PK set of a drug declaring \code{enteralInfusion}
#' @returns the set with its oral absorption copied onto the tissue-depot
#'   fields and \code{transit_PO} moved to \code{transit_RA}
#' @keywords internal
enteralToTissueDepot <- function(pkSet)
{
  pkSet <- oralReleaseToTissueDepot(pkSet)
  pkSet$transit_RA <- isTRUE(pkSet$transit_PO)
  pkSet$transit_PO <- NULL
  pkSet
}
