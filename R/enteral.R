# -----------------------------------------------------------------------------
# A continuous enteral infusion
# -----------------------------------------------------------------------------
# Drafted by Claude Code, 2026-10-19, at the request of Steven L. Shafer, for
# the levodopa intestinal gels (R/drugs_levodopaIntestinalGel.R, Othman &
# Dutta 2014; R/drugs_levodopaEntacaponeIntestinalGel.R, Senek 2020), which
# are pumped continuously into the jejunum through a PEG-J tube, with a
# morning dose and extra doses through the same tube.
#
# A drug that declares `enteralInfusion = TRUE` offers the rate unit
# "mg/hr PO" (enteralRateUnits) beside its "mg PO".  "PO" is the route word
# because the gel is absorbed from the gut as an oral dose is; the drug's name
# says it is the gel.  No other drug may list "mg/hr PO" (simCpCe() refuses
# the row).  The rate reaches the gut's absorption by one of two paths:
#
# - a drug absorbed through a transit chain (levodopaIntestinalGel, one
#   transit compartment before its depot) takes the rate into the head of its
#   chain, lagged and scaled by bioavailability_PO as an oral dose is, in the
#   exact matrix-exponential engine (R/advanceTransitChain.R);
# - any other (Lecigon, one first-order depot) has its oral absorption moved
#   onto the tissue depot by getDrugPK() (oralReleaseToTissueDepot(),
#   R/subcutaneous.R, as canagliflozin's oral release is moved), and
#   simCpCe() flags its PO rows as tissue-depot doses and its "mg/hr PO" rows
#   as a rate into that depot (RArate), which advanceClosedFormPO_IM_IN()
#   carries exactly, as it carries a perineural infusion ("mg/hr RA").
#
# One transit compartment was carried until 2026-10-31 by R/transit.R, as the
# limit of two parallel closed-form depots separated by a relative 1e-5; the
# transit-chain engine, which solves it exactly, replaced it (PR #633 and the
# follow-up that moved levodopa, its gel and opicapone).
# -----------------------------------------------------------------------------

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
  # A dose-dependent oral bioavailability scales the oral doses but not the
  # rate, so the two would be absorbed on different terms.
  if (!is.null(X$oralSaturation))
    bad("an enteral infusion cannot have a dose-dependent oral bioavailability.")
  TRUE
}
