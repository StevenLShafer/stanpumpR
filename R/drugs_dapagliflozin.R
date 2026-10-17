# -----------------------------------------------------------------------------
# Dapagliflozin: two compartments, oral (Farxiga)
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in ng/mL.  Concentration only: no glucose, HbA1c or urinary
# glucose excretion is plotted.  Drafted by Claude Code, 2026-10-17, at the
# request of Steven L. Shafer; see the antidiabetic registry,
# inst/extdata/antidiabeticRegistry.csv.
#
# SOURCE
# ======
# Melin J, Tang W, Rekic D, et al.  Dapagliflozin pharmacokinetics is similar
# between patients with heart failure with reduced ejection fraction and
# patients with type 2 diabetes mellitus.  Br J Clin Pharmacol 2024;90:
# 606-612 (online 2023).  2744 people: 1869 with heart failure with reduced
# ejection fraction (DAPA-HF, troughs), 1269 adults with type 2 diabetes from
# four studies with serial sampling, 24 adolescents with type 2 diabetes and
# 30 healthy volunteers; 1 to 100 mg.  The sponsor's two-compartment model
# with first-order absorption and a fixed lag, refitted.  Supplementary
# Table 4 and the NONMEM control stream (Supporting Information 3, supplied
# by Steven L. Shafer):
#
#     ka   = 2.39 /h          lag = 0.248 h (fixed)
#     CL/F = 23.0 L/h x (eGFR / 83)^0.584 x exp(-0.174 if female)
#     Vc/F = 73.9 L x (weight / 89)^0.570
#     Q/F  = 8.92 L/h         Vp/F = 143 L
#
# "ref group: male, healthy"; exp(-0.174) = 0.840 in women.  A heart-failure
# effect on CL/F was tested and not retained (4%).
#
# RENAL FUNCTION: eGFR in mL/min/1.73 m^2 by CKD-EPI 2009 in adults
# (egfrCKDEPI2009(), at the patient's creatinine or an assumed normal one for
# age and sex) and by the bedside Schwartz equation, 0.413 x height / SCr, in
# adolescents, as the paper (egfrBedsideSchwartz(), at the child's own
# creatinine: patientCreatinine()); the data held no child under 10.
# The data covered eGFR 27 to 154; below 27 the power term extrapolates.
#
# BODY SIZE: the model's own weight term on Vc/F, at the pharmacokinetic
# weight with the switch on and at total weight with it off; the data
# covered 31 to 198 kg.  No other size scaling.
#
# KOBUCHI 2025, the earlier model of this drug in the library, fixed its V/F
# and ka to this paper's values and fitted CL/F to Japanese troughs; it is
# kept as dapagliflozinKobuchi.
#
# CHECKS: 10 mg daily at steady state in a 70 kg man with eGFR 83, AUC over
# a day 10000 / 23.0 = 435 ng.h/mL (the control stream's AUCS = DOSE x 1000 /
# CL).  See tests/testthat/test-drugs-dapagliflozin.R.
#
# References
# ----------
# Melin J et al., Br J Clin Pharmacol 2024;90:606-612.
#   https://doi.org/10.1111/bcp.15939
# -----------------------------------------------------------------------------

DAPAGLIFLOZIN_EGFR_EXPONENT <- 0.584
DAPAGLIFLOZIN_FEMALE_CL <- exp(-0.174)

#' Dapagliflozin pharmacokinetics (oral)
#'
#' Two compartments with a lagged first-order absorption (Melin 2023); see
#' the header of the file.
#'
#' @inheritParams cefazolin
#' @param creatinine serum creatinine, mg/dL, or NULL for an assumed normal
#'   value
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
dapagliflozin <- function(weight, height, age, sex, adjustToFFM = TRUE,
                          creatinine = NULL)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)
  pkW  <- if (isTRUE(adjustToFFM)) size$pkWeight else weight
  egfr <- if (age < 18)
    egfrBedsideSchwartz(height, patientCreatinine(creatinine, age, sex)) else
    egfrCKDEPI2009(age, sex, adultEquivalentCreatinine(creatinine, age, sex))

  cl <- 23.0 * (egfr / 83)^DAPAGLIFLOZIN_EGFR_EXPONENT *
    (if (sex == SEX_FEMALE) DAPAGLIFLOZIN_FEMALE_CL else 1)

  default <- list(
    v1  = 73.9 * (pkW / 89)^0.570, v2 = 143, v3 = 1,
    cl1 = cl / 60, cl2 = 8.92 / 60, cl3 = 0,
    ka_PO = 2.39 / 60,
    bioavailability_PO = 1,   # apparent parameters
    tlag_PO = 0.248 * 60
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  list(
    PK = PK, tPeak = 0, MEAC = 0,
    typical = 0, upperTypical = 0, lowerTypical = 0,
    reference = paste0(
      "Melin 2023 (2744 people: heart failure, type 2 diabetes, adolescents, ",
      "healthy): two compartments, CL/F 23.0 L/h x (eGFR/83)^0.584 (CKD-EPI 2009 ",
      "from the entered or an assumed normal creatinine; bedside Schwartz under 18) ",
      "x 0.840 in women, Vc/F 73.9 L x (weight/89)^0.570, Q/F 8.92 L/h, Vp/F 143 L, ",
      "ka 2.39/h, lag 0.248 h; concentration only. https://doi.org/10.1111/bcp.15939"
    )
  )
}
