# -----------------------------------------------------------------------------
# Apixaban: two-compartment apparent oral model in nonvalvular atrial
# fibrillation (Cirincione 2018, stage 2 final model)
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in ng/mL, total plasma apixaban.
#
# SOURCE
# ======
# Cirincione B, Kowalski K, Nielsen J, et al.  Population pharmacokinetics of
# apixaban in subjects with nonvalvular atrial fibrillation.  CPT
# Pharmacometrics Syst Pharmacol 2018;7:728-738.  4385 subjects, 11968
# concentrations from 12 studies: 270 healthy volunteers (phase I, 2.5 to 50
# mg single doses), 1044 with acute coronary syndrome and 3071 with
# nonvalvular atrial fibrillation (NVAF; an ARISTOTLE subset of 2932, one
# sample each).  Age 18-94 y, weight 32-198 kg, Cockcroft-Gault creatinine
# clearance 12-320 mL/min.  Table 3, "Stage 2 final model" column, read from
# the full text on 2026-10-17:
#
#     CL_R/F  = 1.57 x (CrCL / 80)                        L/h  (power 1 FIXED)
#     CL_NR/F = 2.02 x (age / 65)^-0.429 x (1 - 0.216 FEMALE)   L/h
#     CL/F    = (CL_R/F + CL_NR/F) x (1 - 0.119 ASIAN) x (1 - 0.139 NVAF)
#               x (1 - 0.215 ACS) x (1 - 0.146 CYP3A4/P-gp INHIBITOR)
#     Vc/F    = 30.0 x (WT / 70)^0.790 x (1 - 0.0405 NVAF) x (1 - 0.180 ACS)  L
#     Q/F     = 1.91 L/h          Vp/F = 27.0 L
#     ka      = 0.473 /h (morning or afternoon dose; x (1 - 0.433) evening)
#
# The thetas describe a healthy, non-Asian man of 65 y and 70 kg with a CrCL of
# 80 mL/min; the covariate effects on total CL/F are "compared with
# non-Asian subjects, healthy subjects, or subjects who did not receive" an
# inhibitor.  The equations above are rebuilt from the paper's definitions
# (the display equations are images not carried by the PubMed Central text)
# and checked against the paper's own numbers: its predicted AUC rises of 9%,
# 28% and 55% in NVAF with CrCL 65, 40 and 15 mL/min are reproduced (1.089,
# 1.280, 1.551), as are its 23.3% lower and 22% higher Vc/F at 50 and 90 kg.
# No absorption lag was fitted.
#
# WHAT IS FIXED HERE
# ==================
# The library has no input for indication, race, co-medication or the clock
# time of the dose, so the model is the paper's NVAF patient, the patient
# for whom apixaban is most often held before surgery: the NVAF factors are
# applied, and the patient is taken to be non-Asian, on no strong or moderate
# CYP3A4/P-gp inhibitor, and dosed in the morning or afternoon.  A patient
# outside those (an evening dose absorbs 43% more slowly; an inhibitor lowers
# CL/F 14.6%) is not represented.  The typical NVAF reference patient then
# has CL/F = 3.59 x 0.861 = 3.09 L/h, Vc/F 28.8 L.
#
# CREATININE CLEARANCE
# ====================
# Cockcroft-Gault (creatinineClearanceCG(), R/renalFunction.R) from the
# Patient Profile's serum creatinine, put on the adult scale by
# adultEquivalentCreatinine(); with the field blank it is the ASSUMED NORMAL
# creatinine for age and sex, so renal impairment is then not represented.
# The renal term is CL_R/F = 1.57 x (cCrCL / 80)^1, with the exponent fixed
# at 1 and NO cap: the supplement's equations (Cirincione 2018 Supplementary
# Material, Eq. 1b and Table S1/S2) show a single cCrCL exponent of 1 FIXED
# with no FLAG or truncation term, so clearance is linear and unbounded in
# creatinine clearance.  (An earlier version capped it at 150 mL/min on the
# strength of the handoff specification; the published model has no such cap,
# confirmed against the supplement on 2026-10-17.)  The model was fitted over
# cCrCL 12-320 mL/min; above that range it extrapolates linearly.
#
# DOSE-DEPENDENT RELATIVE BIOAVAILABILITY
# =======================================
# Stage 2 replaced the stage 1 Imax model with a power model: relative F is 1
# at 2.5 mg, and I50, the fall at 50 mg, is estimated on the logit scale
# (logit(I50) = -0.322, so I50 = 0.420); the shape gamma is 0.857.  As the
# oralSaturation form "reduction" (oralSaturationFraction(), R/routes.R):
#
#     F_rel(D) = 1 - 0.420 x ((D - 2.5) / 47.5)^0.857,   D in mg
#
# which is 0.966 at 5 mg, 0.914 at 10 mg and 0.580 at 50 mg.  This is the
# supplement's Eq. 1n exactly (E(F_rel) = 1 - I50 x ((DOSE - 2.5) / 47.5)^gamma,
# with gamma = 0.857 and I50 the logit of theta9 = -0.322), confirmed against
# Cirincione 2018 Supplementary Material (2026-10-17).  Doses above 50 mg were
# not studied.  The parameters are apparent
# (CL/F, V/F), so bioavailability_PO is 1: the label's absolute F of about
# 50% is already inside them and is not applied again.
#
# EFFECT SITE AND THRESHOLDS
# ==========================
# None: tPeak = 0 and the plotted concentration is plasma.  No band, no MEAC
# and no time until threshold.  Apixaban has no therapeutic range, and a
# simulated level is not a procedure-safe time: see inst/help/drugs/apixaban.md.
# Anti-factor Xa activity is not plotted; Byon 2017's slope (0.0159 IU/mL per
# ng/mL, LMWH-calibrated Rotachrom on STA-Compact) belongs to a different
# (VTE) fit and to one assay.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# The model carries its own weight covariate on Vc/F and, through
# Cockcroft-Gault, on CL_R/F.  Both are evaluated at the pharmacokinetic
# weight with the switch on (size$pkWeight) and at total body weight with it
# off, as for cefazolin.  CL_NR/F, Q/F and Vp/F have no size term in the
# source and take the library's fat-free-mass factors with the switch on;
# with it off they are as published (legacyVolume = 1).
#
# (Claude Code, 2026-10-17, at the request of Steven L. Shafer.)
# -----------------------------------------------------------------------------

APIXABAN_NVAF_CL  <- 1 - 0.139    # NVAF on total CL/F
APIXABAN_NVAF_VC  <- 1 - 0.0405   # NVAF on Vc/F

#' Apixaban pharmacokinetics (apparent oral, nonvalvular atrial fibrillation)
#'
#' Cirincione 2018 stage 2 model for a patient with NVAF; see the header of
#' \code{R/drugs_apixaban.R}.
#'
#' @inheritParams cefazolin
#' @param adjustToFFM when \code{TRUE}, evaluate the weight and
#'   creatinine-clearance terms at the pharmacokinetic weight and scale the
#'   size-free parameters to fat-free mass
#' @param creatinine serum creatinine in mg/dL, or NULL for the assumed normal
#'   value for age and sex
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
apixaban <- function(weight, height, age, sex, adjustToFFM = TRUE,
                     creatinine = NULL)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1)
  pkW  <- if (isTRUE(adjustToFFM)) size$pkWeight else weight
  female <- sex == SEX_FEMALE

  crcl <- creatinineClearanceCG(pkW, age, sex,                 # mL/min
                                adultEquivalentCreatinine(creatinine, age, sex))

  clR  <- 1.57 * (crcl / 80)                                   # L/h
  clNR <- 2.02 * (age / 65)^-0.429 * (1 - 0.216 * female) * size$clearance
  cl   <- (clR + clNR) * APIXABAN_NVAF_CL

  default <- list(
    v1  = 30.0 * (pkW / 70)^0.790 * APIXABAN_NVAF_VC,
    v2  = 27.0 * size$volume,
    v3  = 1,                                                   # two compartments
    cl1 = cl / 60,
    cl2 = 1.91 / 60 * size$clearance,
    cl3 = 0,
    ka_PO = 0.473 / 60,                                        # 1/min
    tlag_PO = 0,
    bioavailability_PO = 1                                     # apparent scale
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  list(
    PK = PK,
    tPeak = 0,
    MEAC = 0,
    typical = 0,
    upperTypical = 0,
    lowerTypical = 0,
    oralSaturation = list(
      form = ORAL_SATURATION_REDUCTION,
      I50 = stats::plogis(-0.322), D0 = 2.5, D50 = 50, exponent = 0.857,
      exampleDoses = c(2.5, 5, 10, 20, 50)
    ),
    reference = paste0(
      "Cirincione B et al., CPT Pharmacometrics Syst Pharmacol 2018;7:728-738 ",
      "(stage 2 apparent oral two-compartment model; a non-Asian patient with ",
      "nonvalvular atrial fibrillation on no CYP3A4/P-gp inhibitor, morning ",
      "dosing; Cockcroft-Gault creatinine clearance from the entered creatinine ",
      "or an assumed normal value for age and sex; renal term fixed exponent 1, ",
      "no cap). ",
      "https://doi.org/10.1002/psp4.12347"
    )
  )
}
