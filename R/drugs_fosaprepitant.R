# -----------------------------------------------------------------------------
# Fosaprepitant: plasma aprepitant after the intravenous prodrug
# -----------------------------------------------------------------------------
# The disposition, the dose conversion (mg fosaprepitant free acid x
# 534.44 / 614.40 = mg aprepitant, conversion taken as instantaneous and
# complete), what is plotted and what is left out are all in the header of
# R/drugs_aprepitant.R, which holds the shared model, aprepitantModel().
# -----------------------------------------------------------------------------

#' Fosaprepitant pharmacokinetics
#'
#' Plasma aprepitant after intravenous fosaprepitant (Nijstad 2023). Doses
#' are mg of fosaprepitant free acid, converted to aprepitant (0.8699 mg/mg)
#' on the assumption of instantaneous, complete conversion.
#'
#' @inheritParams aprepitant
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
fosaprepitant <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  aprepitantModel(
    weight, height, age, sex, adjustToFFM,
    doseFraction = FOSAPREPITANT_APREPITANT_FRACTION,
    reference = paste0(
      "Nijstad AL et al., J Oncol Pharm Pract 2023;29:899-904 (aprepitant ",
      "after intravenous fosaprepitant in children; dose converted to ",
      "aprepitant, 0.8699 mg/mg, with instantaneous complete conversion). ",
      "https://doi.org/10.1177/10781552221089243"
    )
  )
}
