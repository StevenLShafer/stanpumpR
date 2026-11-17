# -----------------------------------------------------------------------------
# Etoposide, high-dose intravenous, in children with neuroblastoma (Duong
# 2019): plasma etoposide
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min; amounts in
# mg and concentrations in mcg/mL (= mg/L).
#
# Drafted by Claude Code, 2026-11-06.  The source, the shared model and the
# reference weight (15.5 kg, Shafer's choice) are in the header of
# R/drugs_carboplatinPediatric.R.  Etoposide: two compartments, CL 0.59 L/h,
# VC 2.7 L, Q 0.07 L/h, VP 3.8 L at 15.5 kg, clearances x (WT/15.5)^0.75 and
# volumes x WT/15.5, fitted to high-dose 96-h infusions.  The peripheral
# compartment is unidentified (VP's RSE 234%): it gives a 42-h terminal
# half-life where 3-8 h is usual in children, so the late tail is an artefact
# to distrust.  The library's other children's etoposide is etoposidePediatric
# (Urien 2011, conventional 1-h infusions), the adults' etoposide
# (Krogh-Madsen 2012).  Pregnancy not applicable.  No band.
# -----------------------------------------------------------------------------

#' Etoposide, high-dose intravenous, in children (Duong 2019)
#'
#' Two compartments allometric in weight; see the header of
#' \code{R/drugs_carboplatinPediatric.R}.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
etoposideDuong <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  duongModel("etoposide", weight, height, age, sex, adjustToFFM, paste0(
    "etoposide in two compartments, CL 0.59 L/h, VC 2.7 L, Q 0.07 L/h, VP 3.8 L at ",
    "15.5 kg (Shafer's choice), clearances x (WT/15.5)^0.75, volumes x WT/15.5; the ",
    "peripheral compartment unidentified. Plasma etoposide."))
}
