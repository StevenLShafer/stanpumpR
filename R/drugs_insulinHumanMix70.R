# -----------------------------------------------------------------------------
# Human insulin 70/30 premix (Humulin 70/30, Novolin 70/30): 70% NPH, 30% regular
# -----------------------------------------------------------------------------
# Subcutaneous; doses in units (U), concentrations in mcU/mL.  The model is
# Tham et al. (J Clin Pharmacol 2017;57:1126-1137), Table 2: see
# R/insulinTham.R for the source, the parameters, the assumed structure of
# the zero-order input and the dose and weight effects not applied.
# Concentration only.  Drafted by Claude Code, 2026-10-17, at the request of
# Steven L. Shafer.
# -----------------------------------------------------------------------------

#' Human insulin 70/30 premix (Humulin 70/30, Novolin 70/30): 70% NPH, 30% regular: pharmacokinetics
#'
#' The Tham 2017 one-compartment model for human 70/30 premix: 70% NPH and 30% regular, each with its own absorption; see R/insulinTham.R.
#'
#' @inheritParams cefazolin
#' @param dose units of one injection, for Tham's dose covariates (CL/F
#'   and regular insulin's ka); NULL is the 10 U reference dose
#' @param pregnant when \code{TRUE}, apply the rise in insulin clearance of
#'   late pregnancy, CL/F x 1.25 (see R/insulinTham.R)
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
insulinHumanMix70 <- function(weight, height, age, sex, adjustToFFM = TRUE,
                              dose = NULL, pregnant = FALSE)
{
  thamInsulin(weight, height, age, sex, adjustToFFM, insulin = "human",
              components = c(nph = 0.7, regularU100 = 0.3), product = "human 70/30 (70% NPH, 30% regular)",
              dose = dose, pregnant = pregnant)
}
