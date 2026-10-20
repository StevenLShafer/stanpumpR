# -----------------------------------------------------------------------------
# Insulin lispro protamine and lispro 50/50 (Humalog Mix50/50)
# -----------------------------------------------------------------------------
# Subcutaneous; doses in units (U), concentrations in mcU/mL.  The model is
# Tham et al. (J Clin Pharmacol 2017;57:1126-1137), Table 2: see
# R/insulinTham.R for the source, the parameters, the assumed structure of
# the zero-order input and the dose and weight effects not applied.
# Concentration only.  Drafted by Claude Code, 2026-10-17, at the request of
# Steven L. Shafer.
# -----------------------------------------------------------------------------

#' Insulin lispro protamine and lispro 50/50 (Humalog Mix50/50): pharmacokinetics
#'
#' The Tham 2017 one-compartment model for lispro mix 50/50: 50% lispro protamine and 50% lispro; see R/insulinTham.R.
#'
#' @inheritParams cefazolin
#' @param pregnant when \code{TRUE}, apply the rise in insulin clearance of
#'   late pregnancy, CL/F x 1.19 (see R/insulinTham.R)
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
insulinLisproMix50 <- function(weight, height, age, sex, adjustToFFM = TRUE,
                               pregnant = FALSE)
{
  thamInsulin(weight, height, age, sex, adjustToFFM, insulin = "lispro",
              components = c(lisproProtamine = 0.5, lispro = 0.5), product = "lispro Mix 50/50 (50% lispro protamine)",
              pregnant = pregnant)
}
