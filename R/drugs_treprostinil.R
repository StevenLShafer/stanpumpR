# -----------------------------------------------------------------------------
# Treprostinil (Remodulin): two compartments, ideal body weight, from the FDA's
# pharmacometric review of NDA 21-272
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min; amounts in
# mcg, so the plotted concentration is ng/mL (= mcg/L), total plasma.
#
# Drafted by Claude Code, 2026-11-15, at the request of Steven L. Shafer, who
# asked for the drugs of pulmonary arterial hypertension.  The numbers below
# were read on the page images of the FDA's Clinical Pharmacology and
# Biopharmaceutics Review of NDA 21-272 (Remodulin, treprostinil sodium for
# injection; Nhi Nguyen and Joga Gobburu, pharmacometrics appendix, review
# pages 97 and 99) and in the Remodulin label (United Therapeutics, DailyMed
# 2026-07-20, sec. 12.3), read in full.
#
# THE SOURCE
# ==========
# Healthy volunteers (study 09, rich sampling) and the two pivotal 12-week
# studies 04 and 05 of continuous subcutaneous infusion in pulmonary
# hypertension (233 treated patients, steady-state samples), NONMEM, two
# compartments; "Although the drug was given as a subcutaneous infusion, the
# absorption was rapid enough to be ignored."  Ideal body weight was the size
# covariate, by the review's equations (1) and (2):
#
#     CL = thetaCL (IBW/70)^beta        V = thetaV IBW/70
#
# "All the clearances and volumes of distribution were modeled using the
# equations 1 and 2", and Table 2 (studies 09, 04 and 05 combined):
#
#     CL 29.7 L/h/70 kg   Vc 13.6 L/70 kg   Q 25.3 L/h/70 kg   Vt 37.1 L/70 kg
#     beta 1.52
#
# (the clearance exponent applying to CL and Q).  The review does not name the
# ideal-body-weight formula; the label doses hepatic impairment by "ng/kg/min
# ideal body weight", and Devine's is used here (idealBodyWeightDevine()).
# Vc is poorly identified (inter-individual CV 809%: there were few early
# samples after a change in rate), so the distribution phase (half-life 9.5
# min) is uncertain; the terminal half-life is 2.0 h.
#
# CHECKS (not used in the fit)
# ============================
# The label: "Systemic clearance is approximately 30 L/hour for a 70 kg
# person"; "The volume of distribution of the drug in the central compartment
# is approximately 14 L/70 kg ideal body weight"; terminal half-life "approximately
# 4 hours" after subcutaneous administration (the subcutaneous depot, which the
# model ignores, lengthens the apparent terminal phase); "Concentrations in
# patients treated with an average dose of 9.3 ng/kg/min were approximately
# 2,000 ng/L".  This model at the reference man (170 cm, IBW 66 kg) at 9.3
# ng/kg/min of total weight (70 kg) gives 1.4 ng/mL at steady state.  Study
# P01:07 (the review, page 57; 15 healthy subjects, 15 ng/kg/min for 150 min
# intravenously): Cmax 1.57 ug/L and clearance 663 mL/kg/h (46 L/h at 70 kg);
# this model gives 1.8 ng/mL at 150 min for a 70 kg, 170 cm man, its
# clearance (27 L/h at that man's ideal body weight) being the patients' as
# much as the volunteers'.
#
# ROUTES
# ======
# Intravenous and subcutaneous Remodulin were bioequivalent at steady state
# ("absolute bioavailability approximating 100%"), and the source treated the
# subcutaneous infusion as intravenous, so one rate unit, ng/kg/min, serves
# both.  Inhaled (Tyvaso) and oral (Orenitram) treprostinil are not offered:
# the library has no pulmonary route, and the oral tablet is an osmotic
# extended release with F about 17%.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# The source's own size covariate, ideal body weight, which does not depend
# on total weight, so both switch positions give the same model.  Devine's
# formula is not defined below 5 feet, where the pharmacokinetic weight (or
# total weight, switch off) is used instead, as for linezolid.  The model was
# fitted in adults.
#
# DOSE
# ====
# Remodulin vials are labelled in mg of treprostinil ("2 mg (0.1 mg/mL) ... of
# treprostinil", as the sodium salt formed in situ) and dosed in ng/kg/min.
# Hepatic impairment (clearance reduced by up to 80%) and CYP2C8 inhibitors
# (gemfibrozil doubles exposure) are not modelled.
#
# PREGNANCY AND THE BAND
# ======================
# No pregnancy pharmacokinetics (registry row).  No therapeutic range: doses
# are titrated to symptoms and adverse effects.
# -----------------------------------------------------------------------------

# FDA review of NDA 21-272, pharmacometrics appendix, Table 2
TREPROSTINIL_CL   <- 29.7   # L/h at 70 kg ideal body weight
TREPROSTINIL_VC   <- 13.6   # L
TREPROSTINIL_Q    <- 25.3   # L/h
TREPROSTINIL_VT   <- 37.1   # L
TREPROSTINIL_BETA <- 1.52   # exponent of IBW/70 on the clearances
TREPROSTINIL_IBW_REF <- 70  # kg
TREPROSTINIL_DEVINE_MIN_HEIGHT <- 152.4   # cm (5 feet)

#' Treprostinil (FDA review of NDA 21-272)
#'
#' Two compartments scaled by ideal body weight; see the header of
#' \code{R/drugs_treprostinil.R}.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
treprostinil <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)
  ibw <- if (height >= TREPROSTINIL_DEVINE_MIN_HEIGHT) idealBodyWeightDevine(height, sex)
    else if (isTRUE(adjustToFFM)) size$pkWeight else weight
  f <- ibw / TREPROSTINIL_IBW_REF

  default <- list(
    v1  = TREPROSTINIL_VC * f,
    v2  = TREPROSTINIL_VT * f,
    v3  = 1,
    cl1 = TREPROSTINIL_CL * f^TREPROSTINIL_BETA / 60,
    cl2 = TREPROSTINIL_Q * f^TREPROSTINIL_BETA / 60,
    cl3 = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "FDA Clinical Pharmacology Review of NDA 21-272 (Remodulin, 2002), ",
    "pharmacometrics appendix: two compartments, CL 29.7 L/h, Vc 13.6 L, Q 25.3 L/h, ",
    "Vt 37.1 L at 70 kg ideal body weight (clearances x (IBW/70)^1.52, volumes x IBW/70); ",
    sprintf("ideal body weight %.1f kg. ", ibw),
    "https://www.accessdata.fda.gov/drugsatfda_docs/nda/2002/21-272_Remodulin_biopharmr_P2.pdf")

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
