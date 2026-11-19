# -----------------------------------------------------------------------------
# Insulin aspart (NovoLog, U-100), injection or pump
# -----------------------------------------------------------------------------
# Subcutaneous; doses in units (U), concentrations in mcU/mL of total insulin
# aspart.  The model is the population model of the FDA Clinical
# Pharmacology Review of Fiasp (NDA 208751, 2017), Table 5, NovoLog's
# transit rate and relative bioavailability: see R/insulinAspart.R for the
# source, the structure, the assumed reference weight and the covariates not
# applied.  Concentration only.  Drafted by Claude Code, 2026-11-18, at the
# request of Steven L. Shafer.
# -----------------------------------------------------------------------------

#' Insulin aspart (NovoLog), injection or pump: pharmacokinetics
#'
#' A depot, three transit compartments and one compartment, from the FDA
#' review of Fiasp (NDA 208751), with NovoLog's transit rate; see
#' R/insulinAspart.R.
#'
#' @inheritParams cefazolin
#' @param pregnant when \code{TRUE}, apply the rise in insulin clearance of
#'   late pregnancy, CL/F x 1.25 (see R/insulinAspart.R)
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
insulinAspart <- function(weight, height, age, sex, adjustToFFM = TRUE,
                          pregnant = FALSE)
{
  insulinAspartModel(weight, height, age, sex, adjustToFFM, "novolog",
                     pregnant = pregnant)
}
