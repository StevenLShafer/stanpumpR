# -----------------------------------------------------------------------------
# Faster insulin aspart (Fiasp, U-100), injection or pump
# -----------------------------------------------------------------------------
# Subcutaneous; doses in units (U), concentrations in mcU/mL of total insulin
# aspart.  The model is the population model of the FDA Clinical
# Pharmacology Review of Fiasp (NDA 208751, 2017), Table 5, Fiasp's transit
# rate (55.8/h against NovoLog's 16.1/h; the niacinamide excipient speeds the
# early absorption): see R/insulinAspart.R for the source, the structure, the
# assumed reference weight and the covariates not applied.  Concentration
# only.  Drafted by Claude Code, 2026-11-18, at the request of Steven L.
# Shafer.
# -----------------------------------------------------------------------------

#' Faster insulin aspart (Fiasp), injection or pump: pharmacokinetics
#'
#' A depot, three transit compartments and one compartment, from the FDA
#' review of Fiasp (NDA 208751), with Fiasp's transit rate; see
#' R/insulinAspart.R.
#'
#' @inheritParams cefazolin
#' @param pregnant when \code{TRUE}, apply the rise in insulin clearance of
#'   late pregnancy, CL/F x 1.25 (see R/insulinAspart.R)
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
insulinAspartFiasp <- function(weight, height, age, sex, adjustToFFM = TRUE,
                               pregnant = FALSE)
{
  insulinAspartModel(weight, height, age, sex, adjustToFFM, "fiasp",
                     pregnant = pregnant)
}
