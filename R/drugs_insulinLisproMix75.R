# -----------------------------------------------------------------------------
# Insulin lispro protamine and lispro 75/25 (Humalog Mix75/25)
# -----------------------------------------------------------------------------
# Subcutaneous; doses in units (U), concentrations in mcU/mL.  The model is
# Tham et al. (J Clin Pharmacol 2017;57:1126-1137), Table 2: see
# R/insulinTham.R for the source, the parameters, the assumed structure of
# the zero-order input and the dose and weight effects not applied.
# Concentration only.  Drafted by Claude Code, 2026-10-17, at the request of
# Steven L. Shafer.
# -----------------------------------------------------------------------------

#' Insulin lispro protamine and lispro 75/25 (Humalog Mix75/25): pharmacokinetics
#'
#' The Tham 2017 one-compartment model for lispro mix 75/25: 75% lispro protamine and 25% lispro; see R/insulinTham.R.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
insulinLisproMix75 <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  thamInsulin(weight, height, age, sex, adjustToFFM, insulin = "lispro",
              components = c(lisproProtamine = 0.75, lispro = 0.25), product = "lispro Mix 75/25 (75% lispro protamine)")
}
