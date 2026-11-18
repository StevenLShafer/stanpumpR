# -----------------------------------------------------------------------------
# Sildenafil (Revatio, Viagra): two compartments, intravenous and oral, from
# the FDA's Clinical Pharmacology Review of NDA 22-473 (Revatio injection)
# -----------------------------------------------------------------------------
# A CANDIDATE OF THE SILDENAFIL GROUP (2026-11-18): the drug a user doses is
# sildenafil, whose model the program chooses (R/modelSelection.R).  This
# model is chosen for adults and adolescents from about 16.4 years; children
# get Miura 2026 (R/drugs_sildenafilMiura.R), on Shafer's decision of
# 2026-11-18 ("Use Miura's model for children").
#
# Units: time in minutes, volumes in litres, clearances in L/min; amounts in
# mg, so the plotted concentration is ng/mL (= mcg/L), total plasma.
#
# Drafted by Claude Code, 2026-11-15, at the request of Steven L. Shafer, who
# asked for the drugs of pulmonary arterial hypertension.  The numbers below
# were read on the page images of the FDA's Clinical Pharmacology Review of
# NDA 22-473 (Revatio injection, 10/16/2009; Table 3 p.20, Table 9 p.31,
# Table 10 p.39, Table 5 p.24) and in the Revatio and Viagra labels (Viatris,
# DailyMed 2024 and 2023, sec. 12.3), read in full.
#
# THE SOURCE
# ==========
# Pfizer's population model, reproduced by the FDA reviewer (Table 10): 45
# healthy volunteers (the review, p. 31, read on the page images 2026-11-18:
# "the total number of subjects was 45", sex not given; its Table 8 lists 8,
# 12 and 24, 44 in all), 1479 concentrations, from studies 148-203 (20, 40 and 80 mg
# intravenously), 148-208 (50 mg intravenously over 50 min crossed with 50 mg
# orally; Nichols 2002) and 148-211 (oral multiple doses).  Two compartments
# with first-order absorption after a lag (Table 9, "Mean and 95%CI"):
#
#     CL 29.5 L/h   Vc 73.2 L   Vp 33.3 L   Q 5.32 L/h
#     ka 25.6 /h    lag 0.485 h
#     "Typical F1 (at 20 mg)" 0.355, "F1 dose dependency" 0.289
#
# These are absolute (the intravenous doses were fitted with them).  "F1
# shows a dose dependency (expressed by a power model)": F1 = 0.355 (D/20 mg)^
# 0.289, which the review itself checks: 46% at 50 mg against the 41%
# measured.  That is the library's oralSaturation block of the power form
# (oralSaturationFraction(), R/routes.R).  The intravenous bioavailability F2
# "is fixed at 1 when IV dosage is <= 20 mg but allowed dose dependency"
# whose exponent is not printed; the intravenous product is a 10 mg dose, so
# F2 is 1 here.  No covariate was tested (healthy, fasted men).
#
# The terminal half-life is 5.5 h and Vss 106.5 L (the label: "about 4
# hours", Vss 105 L); the distribution half-life is 1.4 h.
#
# The review warns that the model "cannot predict the AUC after PO
# administration for doses over 50 mg".  At 100 mg F1 is 0.565.
#
# PULMONARY ARTERIAL HYPERTENSION (the Patient Profile's field)
# ================================================================
# The model is the healthy volunteers', and is what a patient without PAH
# (Viagra's erectile dysfunction) gets.  With the Patient Profile's
# Pulmonary hypertension set (Steven L. Shafer, 2026-11-17: "very different
# PK in the two populations, as patients with PAH often have severe cardiac
# disease"), clearance is 30% lower (SILDENAFIL_PAH_CL, 0.70), the
# adjustment the review itself validated.  Its external validation (p.23-24)
# found the healthy model under-predicted patients with pulmonary arterial
# hypertension, and concluded that "the population model that accounts for
# 30% difference in clearance between healthy volunteer and PAH patients
# predicts exposures in PAH patients".  Its Table 5 gives the patients'
# clearance from two analyses: 24.7 L/h intravenously (study 1024, 62
# patients, a steady-state model with no volume), 19.4% below the healthy
# 29.5; and CL/F 55.1 L/h orally (studies 1140/1141, 337 patients), 22.1 L/h
# at a bioavailability of 40%, 33% below.  The 30% is between them.  The
# volumes, absorption and bioavailability are the healthy volunteers': the
# review measured none of them in patients (the oral analysis was one
# compartment with zero-order absorption, and the review says "the lower
# clearance and/or a higher oral bioavailability" without separating them).
# The Revatio label: steady-state concentrations in pulmonary hypertension
# "20 to 50% higher" than in volunteers.  Not applied, for either
# population: healthy volunteers aged 65 or more reached "approximately 84%"
# higher concentrations (45% higher unbound).
#
# CHECKS (not used in the fit)
# ============================
# Viagra label: 100 mg orally to healthy men, peak "approximately 440 ng/mL",
# about 2 ng/mL at 24 hours; this model gives 716 ng/mL at 39 min and 3.4
# ng/mL at 24 h, the over-prediction above 50 mg that the review warns of
# (ka 25.6 /h puts the whole absorption into the first minutes after the
# lag, and F1 is 0.565 at 100 mg against the label's 41% at 50 mg).  REVAKI-1 (Ring et al., Br J Clin Pharmacol 2017;83:
# 709-720, doi 10.1111/bcp.13162, Table 3): 10 mg intravenously in patients aged about
# 70 at cardiac surgery, geometric mean AUC 568 ng.h/mL and clearance 17.6
# L/h; this model's healthy volunteers give 339 ng.h/mL and 29.5 L/h, the
# lower clearance of age, illness and cardiopulmonary bypass (40% lower)
# that the help page states.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# Fixed values with no size term, for the source's healthy volunteers, scaled to
# fat-free mass with the switch on; with it off they are the published values
# for everyone (legacyVolume = 1).
#
# DOSE
# ====
# Revatio tablets: "sildenafil citrate equivalent to 20 mg of sildenafil";
# Revatio injection: "1.124 mg sildenafil citrate (equivalent to 0.8 mg
# sildenafil)" per mL; Viagra "equivalent to 25 mg, 50 mg and 100 mg of
# sildenafil".  Doses are therefore milligrams of sildenafil (base), as
# labelled.  The N-desmethyl metabolite (half the potency, about 40% of the
# parent's concentration after oral doses, 14% after intravenous ones) is not
# modelled: no adult parent-metabolite model was found.
#
# PREGNANCY AND THE BAND
# ======================
# No pregnancy pharmacokinetics (registry row).  No therapeutic range.
# -----------------------------------------------------------------------------

# FDA review of NDA 22-473, Tables 3, 9 and 10
#
# BODY SIZE ANCHOR (Steven L. Shafer, 2026-11-18; docs/weight-adjustment.md,
# Anchoring at the source's cohort): fixed published values belong to the
# source's cohort, not the reference man. The FDA review prints no central
# body size for the healthy subjects the model was fitted to (read
# 2026-11-18), so the fixed values stay anchored at the 70 kg reference man,
# as before the rule.
SILDENAFIL_CL    <- 29.5    # L/h
SILDENAFIL_PAH_CL <- 0.70   # clearance in PAH relative to healthy (p.23-24)
SILDENAFIL_V1    <- 73.2    # L
SILDENAFIL_V2    <- 33.3    # L
SILDENAFIL_Q     <- 5.32    # L/h
SILDENAFIL_KA    <- 25.6    # /h
SILDENAFIL_TLAG  <- 0.485   # h
SILDENAFIL_F_REF <- 0.355   # oral bioavailability at 20 mg
SILDENAFIL_F_DOSE_REF <- 20 # mg
SILDENAFIL_F_EXPONENT <- 0.289

#' Sildenafil: the FDA review of NDA 22-473 (adults; a candidate of the sildenafil group)
#'
#' Two compartments, intravenous and oral with dose-dependent bioavailability;
#' see the header of \code{R/drugs_sildenafil.R}.
#'
#' @inheritParams cefazolin
#' @param pulmonaryHypertension TRUE for a patient with pulmonary arterial
#'   hypertension (clearance 30% lower), FALSE (the default) for the healthy
#'   volunteers' model
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
sildenafilFDA <- function(weight, height, age, sex, adjustToFFM = TRUE,
                       pulmonaryHypertension = FALSE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1)
  pah <- isTRUE(pulmonaryHypertension)
  cl <- SILDENAFIL_CL * (if (pah) SILDENAFIL_PAH_CL else 1)

  default <- list(
    v1  = SILDENAFIL_V1 * size$volume,
    v2  = SILDENAFIL_V2 * size$volume,
    v3  = 1,
    cl1 = cl / 60 * size$clearance,
    cl2 = SILDENAFIL_Q / 60 * size$clearance,
    cl3 = 0,
    ka_PO = SILDENAFIL_KA / 60,
    bioavailability_PO = SILDENAFIL_F_REF,   # at 20 mg; scaled per dose below
    tlag_PO = SILDENAFIL_TLAG * 60
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
    oralSaturation = list(form = ORAL_SATURATION_POWER,
                          Dref = SILDENAFIL_F_DOSE_REF,
                          exponent = SILDENAFIL_F_EXPONENT),
    reference = paste0(
      "FDA Clinical Pharmacology Review of NDA 22-473 (Revatio injection, 2009), ",
      "Tables 3, 9 and 10: two compartments in 45 healthy volunteers, CL 29.5 L/h, Vc 73.2 L, ",
      "Vp 33.3 L, Q 5.32 L/h, ka 25.6 /h, lag 0.485 h, oral F 0.355 x (dose/20 mg)^0.289",
      if (pah) paste0("; pulmonary arterial hypertension: clearance x 0.70 (20.7 L/h), the ",
                      "review's validated adjustment (p.23-24)") else
        "; no pulmonary hypertension (the healthy volunteers')",
      ". https://www.accessdata.fda.gov/drugsatfda_docs/nda/2009/022473s000_ClinPharmR.pdf")
  )
}
