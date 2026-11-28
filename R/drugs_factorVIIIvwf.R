# -----------------------------------------------------------------------------
# Factor VIII protected by von Willebrand factor: the panel of the factor VIII
# a VWF concentrate leaves in the patient
# -----------------------------------------------------------------------------
# Drafted by Claude Code, 2026-11-28, at the request of Steven L. Shafer.
#
# Not a drug that can be dosed (no Units, no Category, as enalaprilat and
# salicylate are): the curve is formed by a VWF concentrate, whose model
# returns a factorVIIIResponse block naming this drug, and is computed by
# vwfFactorVIIISeries() (R/vwfFactorVIII.R) from Bauer 2023's indirect-response
# model.  The factor VIII activity in IU/dL, total (the patient's own
# included, since the model's steady state on the patient's VWF is the
# patient's own factor VIII).  The PK set below is never simulated; it exists
# because every drug in the library has one.
#
# BAND: the label's factor VIII:C peak target for major surgery, 80-100
# IU/dL, typical 100 (VONVENDI prescribing information, 2.2, Table 3); its
# trough after major surgery is above 30 IU/dL (Table 4).
#
# References
# ----------
# Bauer A et al., J Blood Med 2023;14:399-411.
#   https://doi.org/10.2147/JBM.S395845
# VONVENDI prescribing information, BLA 125577, revised 7/2026.
# -----------------------------------------------------------------------------

#' Factor VIII protected by von Willebrand factor (Bauer 2023)
#'
#' The factor VIII activity a VWF concentrate leaves in the patient, drawn
#' from the concentrate's own model; not dosed. See the header of
#' \code{R/drugs_factorVIIIvwf.R}.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
factorVIIIvwf <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  # Never simulated (the drug cannot be dosed): a placeholder one-compartment
  # set at Bauer 2023's factor VIII volume for the pdVWF/FVIII model, 32.9 dL
  # at 75 kg, so that the library's rules on a PK set hold.
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)
  pkW  <- if (isTRUE(adjustToFFM)) size$pkWeight else weight
  default <- list(v1 = 3.29 * pkW / 75, v2 = 1, v3 = 1,
                  cl1 = 3.29 * pkW / 75 * VWF_FVIII_KOUT / 60 * (1 - VONICOG_FVIII_IMAX),
                  cl2 = 0, cl3 = 0)
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Bauer 2023: factor VIII activity driven by the VWF:RCo of a VWF concentrate, ",
    "an indirect response in which VWF inhibits factor VIII's elimination ",
    "(kout 15.9 /h, Imax 0.998, IC50 0.0658 IU/dL for Vonvendi; F0 0.5 IU/dL). ",
    "Band: the label's major-surgery peak target, 80-100 IU/dL. ",
    "https://doi.org/10.2147/JBM.S395845")

  list(PK = PK, tPeak = 0, MEAC = 0,
       typical = 100, upperTypical = 100, lowerTypical = 80,
       reference = reference)
}
