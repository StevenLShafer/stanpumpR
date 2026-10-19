# -----------------------------------------------------------------------------
# Pramipexole, extended-release tablets (Mirapex ER): the disposition of
# R/drugs_pramipexole.R with a zero-order release into the gut, then
# first-order absorption
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in ng/mL of pramipexole base.
#
# Drafted by Claude Code, 2026-10-19, at the request of Steven L. Shafer.
#
# The source, the covariates, the dose basis (mg of the dihydrochloride
# monohydrate, divided by 1.431 for the base) and what is inferred are in the
# header of R/drugs_pramipexole.R: the FDA Clinical Pharmacology Review of
# NDA 22-421, Table 3.  The ER tablet's input is the review's "sequential
# zero and first order absorption": each tablet is released into the gut at
# a constant rate over D1 = 1.34 h and absorbed from there with ka 0.0873 /h,
# with no lag.  That is the zeroOrderPO block canagliflozin introduced
# (R/subcutaneous.R), which carries the release exactly.  It is a separate
# drug from the IR tablet because the engine allows one zero-order oral
# release per drug and no other oral form beside it.
#
# CHECK: 4.5 mg once daily at steady state peaks at 5.9 h (label: about 6 h)
# with Cmax 5.3 ng/mL and AUC0-24 108 ng.h/mL, against study 248.530's 4.9
# and 92 (healthy men); the review's own day-7 simulations at CrCl 80, 50 and
# 30 mL/min are reproduced to 0.3% (tests).  The label's food effect
# (Cmax +20%, Tmax +2 h, AUC unchanged) is not applied.
#
# References
# ----------
# FDA CDER, Clinical Pharmacology and Biopharmaceutics Review, NDA 22-421.
#   https://www.accessdata.fda.gov/drugsatfda_docs/nda/2010/022421s000ClinPharmR.pdf
# Mirapex ER label: https://www.accessdata.fda.gov/drugsatfda_docs/label/2024/022421s023lbl.pdf
# -----------------------------------------------------------------------------

#' Pramipexole, extended-release tablets
#'
#' The disposition of \code{pramipexole()} with the ER tablet's zero-order
#' release over 1.34 h and first-order absorption (FDA review of NDA 22-421);
#' doses in mg of pramipexole dihydrochloride monohydrate.  See the header of
#' \code{R/drugs_pramipexoleER.R}.
#'
#' @inheritParams pramipexoleDisposition
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
pramipexoleER <- function(weight, height, age, sex, adjustToFFM = TRUE,
                          creatinine = NULL)
{
  d <- pramipexoleDisposition(weight, height, age, sex, adjustToFFM, creatinine)
  default <- c(d$pk, list(
    ka_PO = PRAMIPEXOLE_KA_ER / 60,
    bioavailability_PO = 1,          # apparent parameters
    tlag_PO = 0
  ))
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  list(
    PK = PK, tPeak = 0, MEAC = 0,
    typical = 0, upperTypical = 0, lowerTypical = 0,
    zeroOrderPO = list(duration = PRAMIPEXOLE_D1_ER * 60),
    reference = paste0(
      "FDA Clinical Pharmacology Review of NDA 22-421 (Mirapex ER), Table 3: ",
      "the pramipexole disposition (CL/F 29.2 L/h with the Cockcroft-Gault ",
      "hockey stick at 121 mL/min, from the entered or an assumed normal ",
      "creatinine; V2/F 35.6 L, Q/F 115 L/h, V3/F 313 L with weight) and the ER ",
      "tablet's release into the gut over 1.34 h, then ka 0.0873/h; doses as the ",
      "dihydrochloride monohydrate (base = salt / 1.431); concentration only. ",
      "https://www.accessdata.fda.gov/drugsatfda_docs/nda/2010/022421s000ClinPharmR.pdf"
    )
  )
}
