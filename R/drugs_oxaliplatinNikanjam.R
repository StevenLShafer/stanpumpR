# -----------------------------------------------------------------------------
# Oxaliplatin, intravenous, in children and adults (Nikanjam 2015):
# ultrafilterable plasma platinum
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min; amounts in
# mcg and concentrations in ng/mL (doses entered in mg of oxaliplatin; the
# curve is PLATINUM, as fitted: see DOSE AND CONCENTRATION BASIS).
#
# Drafted by Claude Code, 2026-11-06, at the request of Steven L. Shafer, as
# the library's second oxaliplatin model beside oxaliplatin (Bastian 2003).
# Every number and equation below was read from the PDF, which Shafer
# supplied (2026-11-06): Table 1, Table 2 and its equations (at 300 dpi) and
# Figure 1, on the page images.
#
# THE SOURCE
# ==========
# Nikanjam M, Stewart CF, Takimoto CH, Synold TW, Beaty O, Fouladi M,
# Capparelli EV.  Population pharmacokinetic analysis of oxaliplatin in
# adults and children identifies important covariates for dosing.  Cancer
# Chemother Pharmacol 2015;75:495-503 (doi 10.1007/s00280-014-2667-6).  186
# patients aged 0.6-86 years (7-165 kg, serum creatinine 0.2-3.4 mg/dL) from
# five trials: three paediatric (St Jude, COG, PBTC) and the NCI renal- and
# hepatic-impairment studies; 60-160 mg/m2 (4.3 mg/kg in infants) over 2 h;
# plasma ultrafiltrate platinum to 504 h by ICP-MS (LLOQ 1 ng/mL; atomic
# absorption, 10 ng/mL, in the hepatic study); NONMEM 7.2, FOCE-I.
#
# THE MODEL (Table 2 and its equations)
# =====================================
# Three compartments, linear, every parameter allometric in weight with no
# reference weight (per kg^0.75 and per kg):
#
#     CL = 0.67 x WT^0.75 x (0.6/SCR)^0.67     L/h   (SCR in mg/dL)
#     Q3 = 0.22 x WT^0.75,  Q2 = Q3 + 2.95 x WT^0.75     L/h
#     V1 = 0.39 x WT x (AGE/21)^0.16,  V2 = 6.22 x WT,  V3 = 16.9 x WT   L
#
# The printed CL equation omits theta1 (0.67), a misprint: every number the
# paper prints needs it.  The text calls the creatinine effect linear; the
# equation is a power, and the printed 6.7-fold range of clearance is
# (3.4/0.2)^0.67.  Age is a positive power on V1 (the 2.2-fold range is
# (86/0.6)^0.16).  No sex (female CL 86% was tested and dropped), hepatic,
# race or height term.  The Methods' "first-order absorption" is a misprint
# (ADVAN11, an intravenous model).
#
# Checks: at the cohort's medians (59 kg, creatinine 0.8 mg/dL, 20 years) the
# equations give CL 11.76 L/h, Q2 67.48, Q3 4.68 L/h, V1 22.83, V2 367.0, V3
# 997.1 L, Vss 1387 L, exactly the values the Deyme 2019 review tabulates for
# this model; half-lives 0.18, 19.3 and 214 h.  At 130 mg/m2 dose/CL gives an
# AUC of 17.7 (paper 17.3) for a 70-kg adult and 14.5 (14.2) for a 22-kg
# child; the simulated curve (1455 ng/mL at the end of a 2-h infusion, 12 at
# 168 h, 3.8 at 504 h) matches Figure 1; the typical clearance at each
# study's median creatinine is 0.553, 0.347 (Hepatic, Renal; the paper's
# medians 0.58, 0.34) and 0.604, 0.757 L/h/kg^0.75 (Peds1, Peds3; 0.78,
# 0.81), the children above typical as the paper's +13% paediatric bias says.
#
# DOSE AND CONCENTRATION BASIS
# ============================
# Doses are mg of oxaliplatin; concentrations are ng/mL of PLATINUM, with no
# molecular-weight conversion in the fit (the checks above reproduce only so),
# so the volumes and clearances are mixed quantities, about twice a true
# platinum volume (platinum is 0.491 of oxaliplatin's mass).  Ultrafilterable
# platinum is not intact oxaliplatin: Ho 2026 found intact drug 38% of it by
# 5 h, so the late curve overstates active drug.
#
# GUARDS ("No infinity")
# ======================
# Age: a positive power, so V1 would be 0 at age 0; age is floored at 0.6
# years, the youngest studied (OXALIPLATIN_NIKANJAM_MIN_AGE).  Creatinine:
# (0.6/SCR)^0.67 is infinite at 0; the creatinine is floored at 0.2 mg/dL,
# the lowest studied (OXALIPLATIN_NIKANJAM_MIN_SCR).  Values above 3.4 mg/dL
# are an extrapolation.
#
# RENAL FUNCTION
# ==============
# The patient's own serum creatinine through patientCreatinine(): the source
# used children's creatinine as measured (one paediatric trial's missing
# values were imputed).  A blank field is the assumed normal creatinine for
# age and sex.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# The model's own weight covariate, at the pharmacokinetic weight with the
# switch on and total weight with it off.
#
# PREGNANCY (R/pregnancy.R): no comparison found; nothing applied.
# NO EFFECT SITE, NO BAND
# -----------------------------------------------------------------------------

# Nikanjam 2015, Table 2 (per kg^0.75 and per kg)
OXALIPLATIN_NIKANJAM <- list(
  cl = 0.67, q2Increment = 2.95, q3 = 0.22,   # L/h per kg^0.75
  v1 = 0.39, v2 = 6.22, v3 = 16.9,            # L per kg
  ageReference = 21, ageExponent = 0.16,
  scrReference = 0.6, scrExponent = 0.67      # mg/dL
)
OXALIPLATIN_NIKANJAM_MIN_AGE <- 0.6           # years, the youngest studied
OXALIPLATIN_NIKANJAM_MIN_SCR <- 0.2           # mg/dL, the lowest studied

#' Oxaliplatin, intravenous, in children and adults (Nikanjam 2015)
#'
#' Three compartments allometric in weight, the serum creatinine on
#' clearance and age on V1; see the header of
#' \code{R/drugs_oxaliplatinNikanjam.R}.
#'
#' @inheritParams cefazolin
#' @param creatinine serum creatinine, mg/dL, or NULL for an assumed normal
#'   value
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
oxaliplatinNikanjam <- function(weight, height, age, sex, adjustToFFM = TRUE,
                                creatinine = NULL)
{
  p <- OXALIPLATIN_NIKANJAM
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume = 1, legacyClearance = 1)
  w <- if (isTRUE(adjustToFFM)) size$pkWeight else weight
  scr <- max(patientCreatinine(creatinine, age, sex), OXALIPLATIN_NIKANJAM_MIN_SCR)
  ageTerm <- max(age, OXALIPLATIN_NIKANJAM_MIN_AGE)
  w75 <- w^0.75

  cl <- p$cl * w75 * (p$scrReference / scr)^p$scrExponent             # L/h
  q3 <- p$q3 * w75
  q2 <- q3 + p$q2Increment * w75

  default <- list(
    v1  = p$v1 * w * (ageTerm / p$ageReference)^p$ageExponent,
    v2  = p$v2 * w,
    v3  = p$v3 * w,
    cl1 = cl / 60,
    cl2 = q2 / 60,
    cl3 = q3 / 60
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Nikanjam M, Stewart CF, Takimoto CH, et al. Population pharmacokinetic analysis ",
    "of oxaliplatin in adults and children identifies important covariates for dosing. ",
    "Cancer Chemother Pharmacol 2015;75:495-503. 186 patients 0.6-86 years: three ",
    "compartments, CL 0.67 x WT^0.75 x (0.6/SCR)^0.67 L/h (the patient's own or an ",
    "assumed normal creatinine), Q2 3.17 x WT^0.75, Q3 0.22 x WT^0.75 L/h, V1 0.39 x WT x ",
    "(age/21)^0.16, V2 6.22 x WT, V3 16.9 x WT L. Doses in mg oxaliplatin; ultrafilterable ",
    "platinum in ng/mL. https://doi.org/10.1007/s00280-014-2667-6"
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
