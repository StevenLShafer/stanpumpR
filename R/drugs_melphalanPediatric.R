# -----------------------------------------------------------------------------
# Melphalan, high-dose intravenous, in children (Duong 2019): plasma melphalan
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min; amounts in
# mg and concentrations in mcg/mL (= mg/L).
#
# Drafted by Claude Code, 2026-11-06.  The source, the shared model and the
# reference weight (15.5 kg, Shafer's choice) are in the header of
# R/drugs_carboplatinPediatric.R.  Melphalan: two compartments, CL 8.9 L/h,
# VC 6.8 L, Q 4.1 L/h, VP 4.6 L at 15.5 kg, clearances x (WT/15.5)^0.75 and
# volumes x WT/15.5.  It is the best-sampled of the three (5-120 min after
# short infusions); half-lives 0.30 and 1.36 h at 14.5 kg.  Plasma (total)
# melphalan, HPLC-UV, LOQ 0.1 mcg/mL.  Children 1.7-8.3 years; the adults'
# model is melphalan (Nath 2010).  Pregnancy not applicable.  No band.
# -----------------------------------------------------------------------------

#' Melphalan, high-dose intravenous, in children (Duong 2019)
#'
#' Two compartments allometric in weight; see the header of
#' \code{R/drugs_carboplatinPediatric.R}.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
melphalanPediatric <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  duongModel("melphalan", weight, height, age, sex, adjustToFFM, paste0(
    "melphalan in two compartments, CL 8.9 L/h, VC 6.8 L, Q 4.1 L/h, VP 4.6 L at ",
    "15.5 kg (Shafer's choice), clearances x (WT/15.5)^0.75, volumes x WT/15.5. ",
    "Plasma melphalan."))
}
