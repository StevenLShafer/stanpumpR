# -----------------------------------------------------------------------------
# Rivaroxaban: one-compartment apparent oral model pooled across indications
# (Willmann 2018), for a patient with atrial fibrillation
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in ng/mL, total plasma rivaroxaban.
#
# SOURCE
# ======
# Willmann S, Zhang L, Frede M, et al.  Integrated population pharmacokinetic
# analysis of rivaroxaban across multiple patient populations.  CPT
# Pharmacometrics Syst Pharmacol 2018;7:309-320.  4918 patients, 22843
# concentrations, seven trials covering every approved indication; doses 2.5
# mg once daily to 30 mg twice daily.  Population means (Table 1): age 60.5 y,
# weight 82.5 kg, Tietz-truncated creatinine clearance 97.2 mL/min, 39%
# women.  Table 3, final model, read from the full text on 2026-10-17:
#
#     ka   = 0.821 /h          CL/F = 6.58 L/h          V/F = 62.5 L
#     CL/F x (CrCL / CrCL_ref)^0.406 x (WT / WT_ref)^-0.278 x indication
#     V/F  x (WT / WT_ref)^0.216 x (age / age_ref)^-0.189 x 0.889 (women)
#     indication on CL/F, relative to VTE treatment (= 1): atrial
#       fibrillation 0.849, ACS 1.14, VTE prevention 1.04 (<= 72 h after
#       surgery) and 1.29 (> 72 h)
#
# The PubMed Central text drops the table's subscripts, so the assignment of
# the four power estimates to covariates is from the order the final model
# lists them, their signs and the abstract ("CrCL ... on CL/F, age, weight,
# and gender on V/F").  No absorption lag.  Comedication factors on CL/F
# (strong CYP3A4 inhibitor 0.978, and others) are not applied: the library
# has no input for them.
#
# CENTRING
# ========
# The covariates enter as (X / X_ref)^theta, with X_ref the population MEDIAN
# of each distribution (the paper: "relative to the median of the respective
# distribution").  The medians are hard-coded in the published NONMEM control
# stream (Supplementary Material, suppinfo09): WT 81 kg, Tietz-truncated CrCL
# 93 mL/min, AGE 61 y.  (An earlier version used the Table 1 means,
# 82.48 / 97.17 / 60.53, as a placeholder; the control stream resolved it to
# the medians on 2026-10-17.)
#
# RELATIVE BIOAVAILABILITY
# ========================
# F falls with the dose (Fmin 0.590 and D50 14.4 mg estimated, Fmax 1.25
# fixed, F "fixed at 1.0 for a 10 mg dose").  The equation is an image in the
# paper; its form,
#
#     F(D) = 0.590 + (1.25 - 0.590) x exp(-ln 2 x D / 14.4),   D in mg
#
# is the published NONMEM control stream's F1 line exactly (Supplementary
# Material, suppinfo09: F1 = F1MIN + (F1MAX - F1MIN) * EXP(-LOG(2)/D50*DOSE)),
# confirmed on 2026-10-17.  It is 0.998 at 10 mg, 0.84 at 20 mg.
# As the oralSaturation form "exponential" (R/routes.R), each dose by its
# own size.  It is RELATIVE bioavailability on the apparent scale, and the
# trial doses were taken as labelled (15 and 20 mg with food), so it
# describes those conditions, not a fasting 20 mg dose; bioavailability_PO
# is 1.  The first day after hip or knee replacement, when absorption split
# into slow and fast, was excluded from the fit.
#
# WHAT IS FIXED HERE
# ==================
# The indication factor is atrial fibrillation's, 0.849: the patient for whom
# rivaroxaban is most often held before surgery.  The typical such patient
# (the medians: 81 kg, 61 y, CrCL 93 mL/min, a man) has CL/F 6.58 x 0.849 =
# 5.59 L/h, V/F 62.5 L and a half-life of 7.8 h.
#
# CREATININE CLEARANCE
# ====================
# Cockcroft-Gault, truncated at 140 mL/min x BSA / 1.73 m^2 (the "Tietz"
# truncation the paper used; BSA by Du Bois, the paper does not name its
# formula).  From the Patient Profile's serum creatinine on the adult scale
# (adultEquivalentCreatinine()); with the field blank, an ASSUMED NORMAL
# creatinine for age and sex, so renal impairment is then not represented.
#
# EFFECT SITE AND THRESHOLDS
# ==========================
# None: tPeak = 0 and the plotted concentration is plasma.  No band, no MEAC
# and no time until threshold: no therapeutic range is established, and a
# simulated level is not a procedure-safe time.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# Every size term is the model's own (weight on CL/F and V/F, and weight in
# Cockcroft-Gault), evaluated at the pharmacokinetic weight with the switch
# on (size$pkWeight) and at total body weight with it off, as for cefazolin.
#
# (Claude Code, 2026-10-17, at the request of Steven L. Shafer.)
# -----------------------------------------------------------------------------

RIVAROXABAN_WT_REF   <- 81    # kg, population median (NONMEM control stream)
RIVAROXABAN_CRCL_REF <- 93    # mL/min, population median Tietz-truncated CrCL
RIVAROXABAN_AGE_REF  <- 61    # y, population median
RIVAROXABAN_AF_CL    <- 0.849    # atrial fibrillation on CL/F

#' Rivaroxaban pharmacokinetics (apparent oral, atrial fibrillation)
#'
#' Willmann 2018; see the header of \code{R/drugs_rivaroxaban.R}.
#'
#' @inheritParams apixaban
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
rivaroxaban <- function(weight, height, age, sex, adjustToFFM = TRUE,
                        creatinine = NULL)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1)
  pkW  <- if (isTRUE(adjustToFFM)) size$pkWeight else weight

  crcl <- creatinineClearanceCG(pkW, age, sex,
                                adultEquivalentCreatinine(creatinine, age, sex))
  crcl <- min(crcl, 140 * bsaDuBois(pkW, height) / 1.73)      # Tietz truncation

  cl <- 6.58 * (crcl / RIVAROXABAN_CRCL_REF)^0.406 *
    (pkW / RIVAROXABAN_WT_REF)^-0.278 * RIVAROXABAN_AF_CL      # L/h
  # The age term is a negative power, infinite at age 0 and fitted in adults:
  # it sees the age floored at ADULT_AGE_TERM_FLOOR (18), as for every such term.
  ageTerm <- max(age, ADULT_AGE_TERM_FLOOR)
  v  <- 62.5 * (pkW / RIVAROXABAN_WT_REF)^0.216 *
    (ageTerm / RIVAROXABAN_AGE_REF)^-0.189 * (if (sex == SEX_FEMALE) 0.889 else 1)

  default <- list(
    v1  = v,
    v2  = 1,                                   # one compartment
    v3  = 1,
    cl1 = cl / 60,
    cl2 = 0,
    cl3 = 0,
    ka_PO = 0.821 / 60,                        # 1/min
    tlag_PO = 0,
    bioavailability_PO = 1                     # apparent scale
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
      form = ORAL_SATURATION_EXPONENTIAL, Fmin = 0.590, Fmax = 1.25, D50 = 14.4,
      exampleDoses = c(2.5, 10, 15, 20)
    ),
    reference = paste0(
      "Willmann S et al., CPT Pharmacometrics Syst Pharmacol 2018;7:309-320 ",
      "(apparent oral one-compartment model, atrial fibrillation; covariates ",
      "centred on the population medians, 81 kg, 93 mL/min and 61 y; ",
      "Cockcroft-Gault creatinine clearance, ",
      "Tietz-truncated, from the entered creatinine or an assumed normal value ",
      "for age and sex). https://doi.org/10.1002/psp4.12288"
    )
  )
}
