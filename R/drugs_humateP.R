# -----------------------------------------------------------------------------
# Antihemophilic factor/von Willebrand factor complex (human), Humate-P: two
# compartments of VWF:RCo, intravenous, with the factor VIII it carries and
# protects drawn as a panel of its own
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min, amounts in
# units of 10 IU of VWF:RCo, activity in IU/dL (= % of normal).  Plotted as
# the patient's own VWF:RCo activity plus the infused (R/factorBaselines.R).
# Drafted by Claude Code, 2026-11-28, at the request of Steven L. Shafer
# (docs/library-expansion/plan.csv, Biologics (surgery): "build Vonvendi
# (rVWF) and Humate-P (pdVWF/FVIII) as two drugs, each plotting VWF:RCo and
# the factor VIII it carries"; "Go ahead with Humate-P after Vonvendi
# merges", 2026-11-28); digest card papers/digests/PMID_37332615.md.
#
# SOURCE
# ======
# Bauer A, Friberg-Hietala S, Smania G, Wolfsegger M.  J Blood Med
# 2023;14:399-411, with its Supplementary Methods and Results (supplied by
# Shafer on 2026-11-28); the same paper as Vonvendi's (R/drugs_vonicogAlfa.R).
# The pdVWF/FVIII models: 20 adults with type 3 von Willebrand disease in one
# cohort of the phase 1 study NCT00816660, each given one dose of Humate-P,
# 50 IU/kg VWF:RCo (VWF:RCo/FVIII:C 2.4:1), in a crossover with Vonvendi,
# sampled to 96 h; age 30 (18-60) years, weight 66.3 (43.8-132) kg,
# haematocrit 0.406, 9 men and 11 women (Table 3).
#
# Table 4, pdVWF/FVIII, VWF:RCo (the covariate equations and their reference,
# 75 kg and haematocrit 0.4, the rVWF model's, Supplementary Methods):
#
#     CL = 4.14 dL/h x (WT/75)^0.75        Q  = 4.47 dL/h x (WT/75)^0.75
#     Vc = 47.0 dL x (WT/75) x (Hct/0.4)^-0.334 (the exponent fixed)
#     Vp = 19.3 dL x (WT/75)
#
# EVWF fixed at 0.5 IU/dL, half the 1 IU/dL limit of quantification (18 of
# the 20 had pre-dose VWF:RCo below it).
#
# Factor VIII (Table 4 and the Supplementary Methods; R/vwfFactorVIII.R):
#
#     dF/dt = kout F0 + input/FVIII V - kout (1 - Imax V/(IC50 + V)) F
#     F0 = 0.500 IU/dL x (Hct/0.39)^-0.571, kout 15.9 /h (both fixed from
#     the rVWF model), Imax 0.994, IC50 0.0577 IU/dL, FVIII V 32.9 dL
#
# with V the total VWF:RCo.  The product's own factor VIII (1 IU for every
# 2.4 IU of VWF:RCo, the label's average ratio, 3 and 2.2) enters FVIII V and
# is cleared by the same VWF-dependent kout as the patient's own; the source
# could not estimate the two separately (Supplementary Methods).  The
# supplement supports that with the half-life at VWF:RCo 50 IU/dL, 11.5 h,
# "in the range of those observed for pdFVIII products"; its kout of 0.06 /h
# is the rVWF model's Imax (0.998).  With this model's Imax, 0.994, the same
# arithmetic gives 0.114 /h and 6.1 h, which is shorter: the two Imax values
# differ by 0.4% and their complements, which set the half-life at high
# VWF:RCo, by a factor of 3.  Recorded, not reconciled.  FVIII V agrees with
# a published 32.8 dL (the paper's reference 24).
#
# HAEMATOCRIT: held at 0.4, as for Vonvendi (the Patient Profile has none).
#
# THE BASELINE: the Patient Profile's VWF:RCo baseline, shared with Vonvendi;
# blank, 0.5 IU/dL, the source's type 3 value.
#
# WHAT IS DRAWN: VWF:RCo on this drug's panel, and factor VIII, the patient's
# own and the product's, on the panel of factorVIIIvwf, shared with
# Vonvendi.  A patient given Vonvendi and Humate-P together is refused
# (foldMetabolites()): the factor VIII model is not linear in the VWF:RCo,
# so the panel cannot be the sum of two integrations.
#
# DOSES: in IU of VWF:RCo, as the label doses von Willebrand disease (2.2,
# 2.3).  The label also doses haemophilia A in IU of factor VIII (2.1); that
# use is not offered, because this model's factor VIII production is the
# von Willebrand disease patient's (F0, kout): a patient with haemophilia A
# has normal von Willebrand factor and no factor VIII of their own, which the
# model cannot represent (it would draw their factor VIII near 80 IU/dL before
# any dose).  Kept out on Shafer's decision of 2026-11-28, an exception to the
# rule that a drug offers every unit its label uses: do not add the factor
# VIII units without reopening it.  Haemophilia A is the factor VIII models'
# (R/drugs_factorVIII.R).
#
# THE TWO HALF-LIVES: the factor VIII curve uses this model's own Imax and
# IC50, not the rVWF model's that the supplement's 11.5 h comes from
# (Shafer's decision of 2026-11-28; see below).
#
# CHECKS against the label (HUMATE-P prescribing information, BLA 103960,
# revised 6/2020, 12.3, Table 9: subjects with types 1, 2 and 3 before
# surgery; US study 60 IU/kg, n = 41; European 80 IU/kg, n = 28), at 75 kg:
# incremental recovery 75/47.0 = 1.60 IU/dL per IU/kg (label medians 2.4
# and 1.9; its dosing rule assumes 2.0); clearance 5.5 mL/h/kg (label
# medians 3.1 and 4.8); volume at steady state 88 mL/kg (53 and 59); terminal
# half-life 12.2 h (11 and 10 h).  Bauer's cohort is type 3 alone and
# sampled to 96 h, the label's mostly types 1 and 2 to 24 or 48 h; the
# volumes and recoveries are not compared further (only clearance is real),
# and the clearance is higher than the label's.  Against the source: the
# rVWF model's half-life is 1.74 times this one's (Results); this model gives
# 21.1/12.2 = 1.74.  Factor VIII after 50 IU/kg at 75 kg (3750 IU of VWF:RCo,
# 1563 IU of factor VIII): an immediate rise of 1563/32.9 = 47.5 IU/dL above
# the patient's own 4.2, where the label's 2.0 IU/dL per IU/kg of factor VIII
# (2.1) would give 41.7; then a peak of 61 IU/dL at 10 h, as the protected
# endogenous factor VIII rises, and 10 IU/dL at 72 h.
#
# BAND: the label's VWF:RCo peak target, 50-60 IU/dL for minor surgery and
# 100 for major (2.3, Table 3), drawn 50 to 100; the trough after major
# surgery above 50 IU/dL (Table 4), neither factor above 100.
#
# BODY SIZE: the source's own weight terms on CL, Q, Vc and Vp, kept as
# fitted (pharmacokinetic weight with the switch on, total weight with it
# off).  FVIII V has no size term in the source, and takes the fat-free-mass
# volume factor anchored at the cohort (docs/weight-adjustment.md): 66.3 kg,
# 30 years, 9 men and 11 women; heights not printed, so NCD-RisC's mean adult
# height for the 1979 birth cohort averaged without weights over the study's
# six countries (the United States, Canada, Austria, Germany, Italy and the
# United Kingdom; the number enrolled in each is not published), 177.8 cm for
# men and 164.3 cm for women, less 0.24 and 0.36 cm for the years from 18 to
# 30: 177.5 and 163.9 cm.
#
# AGE: adults (18-60) studied; the label approves Humate-P in children.  A
# child is drawn with the adult model scaled by weight, an extrapolation.
#
# PREGNANCY: none applied; Al Arashi 2025 (PMID 39760000) modelled factor
# VIII at delivery in women given Haemate P (inst/extdata/pregnancyRegistry.csv).
#
# References
# ----------
# Bauer A et al., J Blood Med 2023;14:399-411.
#   https://doi.org/10.2147/JBM.S395845
# HUMATE-P prescribing information, BLA 103960, revised 6/2020.
# -----------------------------------------------------------------------------

HUMATEP_CL       <- 4.14   # dL/h at 75 kg (Table 4)
HUMATEP_VC       <- 47.0   # dL at 75 kg, haematocrit 0.4
HUMATEP_Q        <- 4.47   # dL/h at 75 kg
HUMATEP_VP       <- 19.3   # dL at 75 kg
HUMATEP_FVIII_V  <- 32.9   # dL, the infused factor VIII's volume
HUMATEP_IMAX     <- 0.994
HUMATEP_IC50     <- 0.0577 # IU/dL
HUMATEP_RATIO    <- 2.4    # IU VWF:RCo per IU factor VIII (label 3)
# The pdVWF/FVIII cohort (Table 3), for FVIII V, which has no size term
HUMATEP_COHORT <- list(groups = list(
  list(weight = 66.3, height = 177.5, age = 30, men = 9, women = 0),
  list(weight = 66.3, height = 163.9, age = 30, men = 0, women = 11)))

#' Antihemophilic factor/von Willebrand factor complex (human), Humate-P
#' (Bauer 2023)
#'
#' Two compartments of VWF:RCo, plotted as the patient's own plus the
#' infused, in IU/dL, with the factor VIII it carries and protects drawn on
#' the panel of \code{factorVIIIvwf}. See the header of
#' \code{R/drugs_humateP.R}.
#'
#' @inheritParams vonicogAlfa
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
humateP <- function(weight, height, age, sex, adjustToFFM = TRUE,
                    vonWillebrandFactorBaseline = NULL)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume = 1, legacyClearance = 1,
                        cohort = do.call(cohortSizeFactors, HUMATEP_COHORT))
  pkW  <- if (isTRUE(adjustToFFM)) size$pkWeight else weight
  w <- pkW / VONICOG_REF_WEIGHT

  baseline <- if (is.null(vonWillebrandFactorBaseline)) VONICOG_BASELINE else
    vonWillebrandFactorBaseline

  default <- list(
    v1  = HUMATEP_VC / 10 * w * (VONICOG_HCT / 0.4)^VONICOG_HCT_VC,
    v2  = HUMATEP_VP / 10 * w,
    v3  = 1,
    cl1 = HUMATEP_CL / 10 / 60 * w^0.75,
    cl2 = HUMATEP_Q  / 10 / 60 * w^0.75,
    cl3 = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Bauer 2023 (20 adults, 18-60 years, with type 3 von Willebrand disease, given ",
    "Humate-P): two compartments, CL 4.14 dL/h, Vc 47.0 dL, Q 4.47 dL/h, Vp 19.3 dL ",
    "at 75 kg, weight^0.75 on the clearances and weight on the volumes, haematocrit ",
    "held at 0.4. Plotted as the patient's VWF:RCo plus the infused: baseline ",
    signif(baseline, 3), " IU/dL",
    if (is.null(vonWillebrandFactorBaseline)) " (blank: the source's type 3 value)" else "",
    ". Doses in IU of VWF:RCo; the factor VIII it carries (1 IU per 2.4) and ",
    "protects is drawn as factorVIIIvwf. Band: the label's peak targets, 50-60 IU/dL ",
    "(minor surgery) to 100 (major). https://doi.org/10.2147/JBM.S395845")

  list(
    PK = PK, tPeak = 0, MEAC = 0,
    typical = 100, upperTypical = 100, lowerTypical = 50,
    reference = reference,
    endogenousActivity = baseline,
    factorVIIIResponse = list(
      name        = "factorVIIIvwf",
      kout        = VWF_FVIII_KOUT / 60,
      imax        = HUMATEP_IMAX,
      ic50        = HUMATEP_IC50,
      baseline    = VWF_FVIII_F0 * (VONICOG_HCT / 0.39)^VWF_FVIII_HCT,
      vwfBaseline = baseline,
      ratio       = HUMATEP_RATIO,
      volume      = HUMATEP_FVIII_V / 10 * size$volume
    )
  )
}
