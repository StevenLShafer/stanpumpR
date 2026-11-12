# -----------------------------------------------------------------------------
# Flumazenil (Fiset 1995): three compartments, arterial, with an effect site
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min; amounts in
# mcg, so the plotted concentration is ng/mL.
#
# Drafted by Claude Code, 2026-11-12, at the request of Steven L. Shafer, who
# asked for the commonly used drugs the library lacked.  The numbers below
# were read from the full text of the source (the archive's copy, Tables I,
# III and IV checked on the page images) and from the Romazicon label (NDA
# 20-073/S-016, 2007, read in full).
#
# THE SOURCE
# ==========
# Fiset P, Lemmens HLM, Egan TD, Shafer SL, Stanski DR.  Pharmacodynamic
# modeling of the electroencephalographic effects of flumazenil in healthy
# volunteers sedated with midazolam.  Clin Pharmacol Ther 1995;58:567-582
# (doi 10.1016/0009-9236(95)90177-9).  Healthy men (Table I: mean 31.7 years,
# 79 kg, 177.2 cm; 21-47 years, 73-88 kg), seven with flumazenil
# pharmacokinetics in 13 sessions: midazolam held by a STANPUMP target-
# controlled infusion, flumazenil infused at 0.5 or 1 mg/min until the EEG
# effect was reversed.  Arterial samples from 30 s during the infusion to
# 240 min after it, venous to 480 min; GC/MS, detection limit 0.5 ng/mL.
# Three-compartment mammillary model, two-stage analysis.  Table IV (mean,
# SD):
#
#     V1  5.6 L (0.38)      CL1 1.12 L/min (0.16)
#     V2 15.93 L (8.24)     CL2 1.79 L/min (0.51)
#     V3 39.44 L (10.25)    CL3 0.86 L/min (0.38)
#
# with the table's derived half-lives 0.9, 11.4 and 63.2 min and Vss 61 L,
# which the six parameters reproduce (0.94, 11.4 and 63.4 min).  No
# covariates were tested.
#
# THE EFFECT SITE
# ===============
# Table III's equilibration half-lives (t1/2 ke0, the EEG reversal of
# midazolam, per subject and session, 1.1 to 7.4 min) average 4.96 min (SD
# 2.5) at light and 3.96 min (SD 1.5) at deep sedation.  The two levels'
# mean, 4.46 min, is used: ke0 = ln 2 / 4.46 = 0.155 /min, so on this
# model the effect peaks 3.3 min after a bolus.  Zhi 1994 (abstract, a
# psychomotor endpoint) gives 3.3 min, a peak at 2.8 min; the label says
# 80% of the response comes within 3 minutes.  ke0 is supplied directly, as
# the source estimated it.
#
# AGAINST THE LABEL AND A SECOND STUDY (neither used in the fit)
# ==============================================================
# The label, after 1 mg over 5 min: AUC 15 ng.h/mL, Cmax 24 ng/mL, clearance
# about 1 L/h/kg, terminal half-life 54 min.  This model gives AUC 14.9 and a
# clearance of 0.85 L/h/kg at 79 kg, but a Cmax 2.4 times the label's: the
# arterial early samples and a small V1, against the label's venous data
# (its initial volume is 0.5 L/kg), the artefact described in AGENTS.md
# ("Comparing sources: only clearance is real").  Roncari 1986 (Br J Clin
# Pharmacol 22:421, six men, scanned pages read): clearance 15.5 mL/min/kg
# against this model's 14.2.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# The source's men averaged 79 kg and 177 cm, not the library's reference,
# so the published values are written for a man of the source's mean size
# (fat-free mass 61.4 kg) and scaled from there by fat-free mass with the
# switch on, volumes in proportion and clearances to the 0.75 power.  With
# the switch off the published values are used unscaled, as the source has
# them.
#
# DOSE BASIS
# ==========
# Flumazenil is labelled and dosed as itself (Romazicon, 0.1 mg/mL), as in
# the source; there is no salt.
#
# NOT MODELLED
# ============
# The label: clearance falls to 40-60% of normal in moderate and to 25% in
# severe liver disease, and rises 50% with food.  No effect of age, sex or
# renal failure.  The benzodiazepine it opposes is not modelled: the effect
# site is flumazenil's own.
#
# THE BAND
# ========
# Not a therapeutic range (none is established; library_band): the label's
# "0.4 to 1 mg ... usually produce complete antagonism", peak plasma 12 to
# 28 ng/mL; 0.1 to 0.2 mg (3 to 6 ng/mL) produce partial antagonism.
# -----------------------------------------------------------------------------

# Fiset 1995, Table IV (two-stage means) and Table III (t1/2 ke0)
FLUMAZENIL_V1  <- 5.6      # L
FLUMAZENIL_V2  <- 15.93    # L
FLUMAZENIL_V3  <- 39.44    # L
FLUMAZENIL_CL1 <- 1.12     # L/min
FLUMAZENIL_CL2 <- 1.79     # L/min
FLUMAZENIL_CL3 <- 0.86     # L/min
FLUMAZENIL_T12KE0 <- (4.96 + 3.96) / 2   # min, light and deep sedation
# Table I: the source's mean man (his fat-free mass is computed in the
# function, ffmAlSallami() being defined in a file collated later)
FLUMAZENIL_SOURCE <- list(weight = 79, height = 177.2, age = 31.7)

#' Flumazenil (Fiset 1995)
#'
#' Three compartments, arterial, with an EEG effect site; see the header of
#' \code{R/drugs_flumazenil.R}.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
flumazenil <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume = 1, legacyClearance = 1)
  # The published values belong to the source's mean man; re-anchor them
  toRef <- if (isTRUE(adjustToFFM))
    FFM_REFERENCE / ffmAlSallami(FLUMAZENIL_SOURCE$weight, FLUMAZENIL_SOURCE$height,
                                 FLUMAZENIL_SOURCE$age, SEX_MALE) else 1
  vf  <- size$volume * toRef
  clf <- size$clearance * toRef^0.75

  default <- list(
    v1 = FLUMAZENIL_V1 * vf,
    v2 = FLUMAZENIL_V2 * vf,
    v3 = FLUMAZENIL_V3 * vf,
    cl1 = FLUMAZENIL_CL1 * clf,
    cl2 = FLUMAZENIL_CL2 * clf,
    cl3 = FLUMAZENIL_CL3 * clf
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Fiset P et al., Clin Pharmacol Ther 1995;58:567-582: three compartments, ",
    "arterial, healthy men (79 kg): V1 5.6, V2 15.93, V3 39.44 L, CL 1.12, ",
    "1.79, 0.86 L/min; t1/2 ke0 4.46 min (the EEG reversal of midazolam). ",
    "https://doi.org/10.1016/0009-9236(95)90177-9"
  )

  list(
    PK = PK,
    ke0 = log(2) / FLUMAZENIL_T12KE0,
    MEAC = 0,
    typical = 20,
    upperTypical = 28,
    lowerTypical = 12,
    reference = reference
  )
}
