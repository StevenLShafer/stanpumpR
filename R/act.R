# -----------------------------------------------------------------------------
# The activated clotting time (ACT) from a heparin anti-factor Xa activity:
# a direct Emax link (Delavenne 2017)
# -----------------------------------------------------------------------------
# A drug that returns an `act` block (unfractionated heparin on cardiopulmonary
# bypass, R/drugs_heparinCPB.R) is simulated as an ordinary anti-factor Xa
# activity curve (IU/mL, the same two-compartment intravenous engine as every
# other activity drug) and then turned, point by point, into the activated
# clotting time it produces:
#
#     ACT = ACT0 + Emax x C / (C50 + C)
#
# with C the central anti-Xa activity in IU/mL.  This is the pharmacodynamic
# relationship Delavenne and colleagues fitted (Br J Anaesth 2017;118:705-712,
# Table 2): ACT0 the baseline clotting time, Emax the maximal prolongation and
# C50 the anti-Xa activity that gives half of it.  The link is INSTANTANEOUS --
# there is no effect compartment, so the ACT at each time is a static function
# of the anti-Xa activity at that time, unlike the delayed turnover of the INR
# (R/inr.R).
#
# What is plotted is the ACT, in seconds.  The anti-Xa activity that drives it
# is not plotted, and there is no effect site, no band, no MEAC and no time
# until threshold: the drug is a research and teaching model, not a validated
# clinical ACT or a safe anticoagulation target (see
# inst/help/drugs/heparinCPB.md).
#
# (Claude Code, 2026-10-20, at the request of Steven L. Shafer.)
# -----------------------------------------------------------------------------

#' The baseline ACT before any heparin, for normalising the rise
#'
#' The Emax link starts, at zero anti-Xa activity, at \code{ACT0}.
#' @param act the drug's ACT block
#' @keywords internal
actBaseline <- function(act) act$act0

#' Check a drug model's ACT block
#'
#' An ACT drug (\code{Concentration.Units} "ACT") returns an \code{act} block of
#' three positive numbers: \code{act0} (the baseline clotting time, s),
#' \code{emax} (the maximal prolongation, s) and \code{c50} (the anti-factor Xa
#' activity giving half of it, IU/mL).  The underlying anti-Xa activity is
#' simulated by the ordinary intravenous engine, so the model's PK is dosed in
#' international units like any activity drug.
#'
#' @param act the block a drug model returned, or NULL
#' @param drug the drug's name, for the error message
#' @param concentrationUnits the CSV's \code{Concentration.Units}, which must be
#'   "ACT" exactly when there is a block
#' @returns \code{act}, unchanged, if it is valid; otherwise an error
#' @keywords internal
validateACT <- function(act, drug, concentrationUnits)
{
  isACT <- identical(as.character(concentrationUnits), "ACT")
  if (is.null(act)) {
    if (isACT)
      stop("Invalid ACT model for ", drug, ": Concentration.Units \"ACT\" ",
           "needs an act block.")
    return(NULL)
  }
  if (!isACT)
    stop("Invalid ACT model for ", drug, ": only a drug with ",
         "Concentration.Units \"ACT\" may return an act block.")
  ok <- is.list(act) &&
    is_valid_number(act$act0) && act$act0 > 0 &&
    is_valid_number(act$emax) && act$emax > 0 &&
    is_valid_number(act$c50)  && act$c50  > 0
  if (!ok)
    stop("Invalid ACT model for ", drug, ": needs positive act0 (s), ",
         "emax (s) and c50 (IU/mL).")
  act
}

#' Turn an anti-factor Xa activity run into the plotted ACT
#'
#' @param base the engine output (its \code{Cp} is the central anti-Xa activity
#'   in IU/mL, the IU dose conversion already applied in \code{simCpCe()})
#' @param act the drug's ACT block
#' @returns one engine output: \code{Time}, \code{Cp} (the ACT, s), \code{Ce}
#'   (NA: no effect site) and \code{Recovery} (NA)
#' @keywords internal
actEngineResults <- function(base, act)
{
  c <- pmax(base$Cp, 0)
  data.frame(
    Time = base$Time,
    Cp = act$act0 + act$emax * c / (act$c50 + c),
    Ce = rep(NA_real_, nrow(base)),
    Recovery = rep(NA_real_, nrow(base))
  )
}
