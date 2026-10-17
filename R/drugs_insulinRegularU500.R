# -----------------------------------------------------------------------------
# Concentrated human regular insulin U-500 (Humulin R U-500)
# -----------------------------------------------------------------------------
# Subcutaneous; doses in units (U), concentrations in mcU/mL.  The model is
# Tham et al. (J Clin Pharmacol 2017;57:1126-1137), Table 2: see
# R/insulinTham.R for the source, the parameters, the assumed structure of
# the zero-order input and the dose and weight effects not applied.
# Concentration only.  Drafted by Claude Code, 2026-10-17, at the request of
# Steven L. Shafer.
# -----------------------------------------------------------------------------

#' Concentrated human regular insulin U-500 (Humulin R U-500): pharmacokinetics
#'
#' The Tham 2017 one-compartment model for human insulin, regular U-500, absorbed first-order more slowly than U-100; see R/insulinTham.R.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
insulinRegularU500 <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  thamInsulin(weight, height, age, sex, adjustToFFM, insulin = "human",
              components = c(regularU500 = 1), product = "regular U500")
}
