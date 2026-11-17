# -----------------------------------------------------------------------------
# Vinorelbine, intravenous and oral (Variol 2002): whole-blood vinorelbine
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min; amounts in
# mcg and concentrations in ng/mL (doses entered in mg).
#
# Drafted by Claude Code, 2026-11-06, at the request of Steven L. Shafer
# (docs/chemotherapy.md, inst/extdata/chemotherapyRegistry.csv).  Every
# number below was read from the PDF, which Shafer supplied (2026-11-06):
# Tables 1 and 3 and Eqs. 1-4, on the page images.
#
# THE SOURCE
# ==========
# Variol P, Nguyen L, Tranchand B, Puozzo C.  A simultaneous oral/intravenous
# population pharmacokinetic model for vinorelbine.  Eur J Clin Pharmacol
# 2002;58:467-476 (doi 10.1007/s00228-002-0506-x).  175 patients (91 men),
# 57 +/- 11 years, BSA 1.74 +/- 0.17 m2; intravenous 20-45 mg/m2 and the oral
# soft capsule 60-100 mg/m2, as monotherapy; whole blood, HPLC-UV, LLOQ 2.5
# ng/mL, about 10 samples to 120 h after IV and 72 h after oral in phase I;
# NONMEM V.
#
# THE MODEL (Table 3)
# ===================
# Three compartments with a lagged first-order oral input, linear:
#
#     CL 41.9 L/h    Vc 22.2 L
#     k(central->fast) 2.46 /h, back 0.402 /h  (Variol's K24, K42)
#     k(central->deep) 1.39 /h, back 0.0337 /h (Variol's K23, K32)
#     oral: F 0.36, ka 0.481 /h, lag 0.15 h (fasting)
#
# Table 3's values are used, on Shafer's decision (2026-11-06), rather than
# the covariate equation (Eq. 1, CL = 30.5 x BSA x ...), which at the
# cohort's mean BSA gives 53.1 L/h, 27% above Table 3; the paper's Figure 1
# tail (about 7 ng/mL at 24 h, 5 at 36 h) fits 41.9.  Derived here: V2 136 L,
# V3 916 L, Vss 1074 L; half-lives 7.0 min, 3.0 h and 36.9 h; ln2/ka 1.44 h,
# the paper's "1.4 h".
#
# WHAT IS NOT REPRESENTED
# =======================
# Table 3 carries no covariates, so body surface area, creatinine clearance
# below 70 mL/min, raised transaminases and a platelet count above 400 (all on
# CL in Eq. 1), alkaline phosphatase (on F) and food (a longer lag) are not
# applied; the volumes and clearances scale by the library's factors with
# the switch on and are fixed with it off.  The IV infusion duration is not
# stated (infusions are entered as given).
#
# DOSE BASIS
# ==========
# Doses as labelled (Navelbine, mg of vinorelbine base as the tartrate); the
# paper does not state its basis.  The matrix is WHOLE BLOOD, not plasma.
#
# PREGNANCY (R/pregnancy.R): no comparison found; nothing applied.
# NO EFFECT SITE, NO BAND
# -----------------------------------------------------------------------------

# Variol 2002, Table 3; rate constants in /h
VINORELBINE_VARIOL <- list(
  cl = 41.9,        # L/h
  v1 = 22.2,        # L
  k12 = 2.46, k21 = 0.402,     # the fast peripheral (Variol's K24, K42)
  k13 = 1.39, k31 = 0.0337,    # the deep peripheral (Variol's K23, K32)
  f = 0.36, ka = 0.481, tlag = 0.15   # oral: fraction, /h, h
)

#' Vinorelbine, intravenous and oral (Variol 2002)
#'
#' Three compartments with a lagged first-order oral input; see the header of
#' \code{R/drugs_vinorelbine.R}.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
vinorelbine <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  p <- VINORELBINE_VARIOL
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume = 1, legacyClearance = 1)
  v1 <- p$v1 * size$volume

  default <- list(
    v1  = v1,
    v2  = p$v1 * p$k12 / p$k21 * size$volume,
    v3  = p$v1 * p$k13 / p$k31 * size$volume,
    cl1 = p$cl / 60 * size$clearance,
    cl2 = p$v1 * p$k12 / 60 * size$clearance,
    cl3 = p$v1 * p$k13 / 60 * size$clearance,
    ka_PO = p$ka / 60,
    bioavailability_PO = p$f,
    tlag_PO = p$tlag * 60
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Variol P, Nguyen L, Tranchand B, Puozzo C. A simultaneous oral/intravenous ",
    "population pharmacokinetic model for vinorelbine. Eur J Clin Pharmacol ",
    "2002;58:467-476. 175 patients: three compartments, CL 41.9 L/h, Vc 22.2 L, ",
    "k12 2.46, k21 0.402, k13 1.39, k31 0.0337 /h (Table 3, no covariates); oral ",
    "capsule F 0.36, ka 0.481 /h, lag 0.15 h. Whole-blood vinorelbine in ng/mL. ",
    "https://doi.org/10.1007/s00228-002-0506-x"
  )

  list(
    PK = PK,
    tPeak = 0,
    MEAC = 0,
    typical = 0,
    upperTypical = 0,
    lowerTypical = 0,
    reference = reference
  )
}
