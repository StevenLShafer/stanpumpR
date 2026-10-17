# -----------------------------------------------------------------------------
# Insulin glargine U-100, fitted to Basaglar
# -----------------------------------------------------------------------------
# Subcutaneous; doses in units (U), concentrations in mcU/mL.  The model is
# Tham et al. (J Clin Pharmacol 2017;57:1126-1137), Table 2: see
# R/insulinTham.R for the source, the parameters, the assumed structure of
# the zero-order input and the dose and weight effects not applied.
# Concentration only.  Drafted by Claude Code, 2026-10-17, at the request of
# Steven L. Shafer.
# -----------------------------------------------------------------------------

#' Insulin glargine U-100, fitted to Basaglar: pharmacokinetics
#'
#' The Tham 2017 one-compartment model for insulin glargine U-100 (Basaglar fit), parallel first- and zero-order absorption; see R/insulinTham.R.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
insulinGlargine <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  thamInsulin(weight, height, age, sex, adjustToFFM, insulin = "glargine",
              components = c(glargine = 1), product = "glargine U100 (Basaglar)")
}
