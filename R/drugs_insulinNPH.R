# -----------------------------------------------------------------------------
# Human NPH (isophane) insulin (Humulin N, Novolin N)
# -----------------------------------------------------------------------------
# Subcutaneous; doses in units (U), concentrations in mcU/mL.  The model is
# Tham et al. (J Clin Pharmacol 2017;57:1126-1137), Table 2: see
# R/insulinTham.R for the source, the parameters, the assumed structure of
# the zero-order input and the dose and weight effects not applied.
# Concentration only.  Drafted by Claude Code, 2026-10-17, at the request of
# Steven L. Shafer.
# -----------------------------------------------------------------------------

#' Human NPH (isophane) insulin (Humulin N, Novolin N): pharmacokinetics
#'
#' The Tham 2017 one-compartment model for human NPH insulin, first-order absorption after a lag; see R/insulinTham.R.
#'
#' @inheritParams cefazolin
#' @param dose units of one injection, for Tham's dose covariates (CL/F
#'   and regular insulin's ka); NULL is the 10 U reference dose
#' @param pregnant when \code{TRUE}, apply the rise in insulin clearance of
#'   late pregnancy, CL/F x 1.25 (see R/insulinTham.R)
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
insulinNPH <- function(weight, height, age, sex, adjustToFFM = TRUE,
                       dose = NULL, pregnant = FALSE)
{
  thamInsulin(weight, height, age, sex, adjustToFFM, insulin = "human",
              components = c(nph = 1), product = "NPH",
              dose = dose, pregnant = pregnant)
}
