# -----------------------------------------------------------------------------
# Apixaban: two-compartment apparent oral model in venous thromboembolism
# treatment (Byon 2017, final population model)
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in ng/mL, total plasma apixaban.
#
# This is a SECOND, distinct apixaban model from the library's `apixaban`
# (Cirincione 2018, the nonvalvular-atrial-fibrillation model).  This one is the
# venous-thromboembolism (VTE) treatment model, fitted to the acute-DVT/PE and
# extended-prophylaxis trials (AMPLIFY, AMPLIFY-EXT) plus the phase I/II studies.
# The two are the apixaban analogue of dabigatran / dabigatranHip.
#
# SOURCE
# ======
# Byon W, Sweeney K, Frost C, Boyd RA.  Population Pharmacokinetics,
# Pharmacodynamics, and Exploratory Exposure-Response Analyses of Apixaban in
# Subjects Treated for Venous Thromboembolism.  CPT Pharmacometrics Syst
# Pharmacol 2017;6:340-349.  970 subjects, 8323 apixaban concentrations from
# eight phase I, one phase II (Botticelli DVT) and two phase III (AMPLIFY,
# AMPLIFY-EXT) studies; NONMEM.  Table 4 (final population PK model parameters),
# read from the full text on 2026-10-20.
#
# STRUCTURE
# =========
# Apparent (/F) two-compartment model with first-order oral input and no lag.
# Reference VTE-treatment subject: 60 y, 85 kg, non-Asian male, Cockcroft-Gault
# creatinine clearance 100 mL/min.  At that reference (Table 4):
#
#     CL_R/F  = 1.83 x min(cCrCL, 150) / 100                        L/h
#     CL_NR/F = 2.52 x (age / 60)^-0.267 x (1 - 0.223 FEMALE)       L/h
#     CL/F    = (CL_R/F + CL_NR/F) x (1 - 0.168 ASIAN)
#                                  x (1 - 0.203 CYP3A4/P-gp INHIBITOR)
#     Vc/F    = 32.1 x (WT / 85)^0.523                              L
#     Q/F     = 1.62 L/h          Vp/F = 19.8 L
#     ka      = 0.440 /h (morning or afternoon dose; 0.239 /h evening, -46%)
#
# At the reference patient CL/F = 1.83 + 2.52 = 4.35 L/h, the value the paper
# quotes.  The model reproduces the paper's own covariate predictions: severe
# renal impairment (cCrCL 15 mL/min) gives CL/F 2.79 L/h, a 36% fall and a 56%
# higher exposure; a 50- and 120-kg subject have Vc/F 24% lower and 20% higher;
# a 40- and 80-year-old have CL_NR/F 11% higher and 7% lower.
#
# The renal term uses Cockcroft-Gault with a breakpoint at 150 mL/min (the
# paper's FLAG): above 150 the renal clearance is held at its 150 mL/min value,
# so CL_R/F = 1.83 x min(cCrCL, 150) / 100.  Fitted over cCrCL 25-322 mL/min.
#
# WHAT IS FIXED HERE
# ==================
# The library has no input for race, co-medication or the clock time of the
# dose, so the patient is taken to be non-Asian (Asian lowers CL/F 16.8%), on no
# strong or moderate CYP3A4/P-gp inhibitor (an inhibitor lowers CL/F 20.3%), and
# dosed in the morning or afternoon (an evening dose absorbs 46% more slowly,
# ka 0.239 /h).  A patient outside those is not represented.
#
# DOSE-DEPENDENT RELATIVE BIOAVAILABILITY
# =======================================
# Apixaban absorption is dissolution-rate-limited above 10 mg (aqueous
# solubility 0.04 mg/mL), and the model carries an inhibitory-Emax relative-F
# term for that fall.  Its parameters are NOT in the paper's tables (they were
# fixed from the prior apixaban analyses, refs 25-26), so no dose-dependent F is
# applied here.  At the VTE-treatment doses (2.5, 5 and 10 mg; the simulated
# range) relative F is ~1, so this changes nothing in the therapeutic range;
# doses above 10 mg are outside the VTE indication and would be over-predicted.
# The parameters are apparent (CL/F, V/F), so bioavailability_PO is 1: the
# label's absolute F of about 50% is already inside them.
#
# EFFECT SITE AND THRESHOLDS
# ==========================
# None: tPeak = 0 and the plotted concentration is plasma.  No band, no MEAC and
# no time until threshold.  Apixaban has no therapeutic range, and a simulated
# level is not a procedure-safe time: see inst/help/drugs/apixabanVTE.md.  Byon's
# anti-factor Xa relationship (a linear 0.0159 IU/mL per ng/mL, LMWH-calibrated
# Rotachrom on a STA-Compact) is documentation only and is not plotted.  A
# research and teaching model, not validated clinical decision support.
#
# CREATININE CLEARANCE
# ====================
# Cockcroft-Gault (creatinineClearanceCG(), R/renalFunction.R) from the Patient
# Profile's serum creatinine, put on the adult scale by
# adultEquivalentCreatinine(); with the field blank it is the ASSUMED NORMAL
# creatinine for age and sex, so renal impairment is then not represented.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# The model carries its own weight covariate on Vc/F and, through
# Cockcroft-Gault, on CL_R/F.  Both are evaluated at the pharmacokinetic weight
# with the switch on (size$pkWeight) and at total body weight with it off, as
# for apixaban.  CL_NR/F, Q/F and Vp/F have no size term in the source and take
# the library's fat-free-mass factors with the switch on; with it off they are
# as published (legacyVolume = 1).
#
# (Claude Code, 2026-10-20, at the request of Steven L. Shafer.)
# -----------------------------------------------------------------------------

APIXABAN_VTE_CRCL_CAP <- 150     # mL/min, the renal-term breakpoint (FLAG)

#' Apixaban pharmacokinetics (apparent oral, venous thromboembolism treatment)
#'
#' Byon 2017 final population model for a VTE-treatment subject; see the header
#' of \code{R/drugs_apixabanVTE.R}.  A second, distinct apixaban model from the
#' library's \code{apixaban} (Cirincione 2018, atrial fibrillation).
#'
#' @inheritParams cefazolin
#' @param adjustToFFM when \code{TRUE}, evaluate the weight and
#'   creatinine-clearance terms at the pharmacokinetic weight and scale the
#'   size-free parameters to fat-free mass
#' @param creatinine serum creatinine in mg/dL, or NULL for the assumed normal
#'   value for age and sex
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
apixabanVTE <- function(weight, height, age, sex, adjustToFFM = TRUE,
                        creatinine = NULL)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1)
  pkW  <- if (isTRUE(adjustToFFM)) size$pkWeight else weight
  female <- sex == SEX_FEMALE

  crcl <- creatinineClearanceCG(pkW, age, sex,                 # mL/min
                                adultEquivalentCreatinine(creatinine, age, sex))

  # Renal clearance is linear in creatinine clearance up to a 150 mL/min
  # breakpoint (the paper's FLAG), above which it is held at its 150 value.
  clR  <- 1.83 * pmin(crcl, APIXABAN_VTE_CRCL_CAP) / 100       # L/h
  # The age term is a negative power, infinite at age 0 and fitted in adults:
  # it sees the age floored at ADULT_AGE_TERM_FLOOR (18), as for every such term.
  ageTerm <- max(age, ADULT_AGE_TERM_FLOOR)
  clNR <- 2.52 * (ageTerm / 60)^-0.267 * (1 - 0.223 * female) * size$clearance
  cl   <- clR + clNR                                           # non-Asian, no inhibitor

  default <- list(
    v1  = 32.1 * (pkW / 85)^0.523,
    v2  = 19.8 * size$volume,
    v3  = 1,                                                   # two compartments
    cl1 = cl / 60,
    cl2 = 1.62 / 60 * size$clearance,
    cl3 = 0,
    ka_PO = 0.440 / 60,                                        # 1/min (morning)
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
    reference = paste0(
      "Byon W et al., CPT Pharmacometrics Syst Pharmacol 2017;6:340-349 ",
      "(final apparent oral two-compartment model for venous thromboembolism ",
      "treatment; a non-Asian patient on no CYP3A4/P-gp inhibitor, morning ",
      "dosing; Cockcroft-Gault creatinine clearance from the entered creatinine ",
      "or an assumed normal value for age and sex, renal term capped at 150 ",
      "mL/min). ",
      "https://doi.org/10.1002/psp4.12184"
    )
  )
}
