# -----------------------------------------------------------------------------
# Von Willebrand factor (recombinant), vonicog alfa (Vonvendi): two
# compartments, intravenous, with the factor VIII it protects drawn as a
# panel of its own
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min, amounts in
# units of 10 IU, activity in IU/dL (= % of normal).  Plotted as the patient's
# own VWF:RCo activity plus the infused (R/factorBaselines.R; Steven L.
# Shafer, 2026-11-25).  Drafted by Claude Code, 2026-11-28, at the request of
# Steven L. Shafer (docs/library-expansion/plan.csv, Biologics (surgery):
# "build Vonvendi (rVWF) and Humate-P (pdVWF/FVIII) as two drugs, each
# plotting VWF:RCo and the factor VIII it carries, every equation read rather
# than assumed"); digest card papers/digests/PMID_37332615.md.
#
# SOURCE
# ======
# Bauer A, Friberg-Hietala S, Smania G, Wolfsegger M.  Pharmacokinetic-
# pharmacodynamic comparison of recombinant and plasma-derived von Willebrand
# factor in patients with von Willebrand disease type 3.  J Blood Med
# 2023;14:399-411.  Read on the PDF, with its supplement (the Supplementary
# Methods' covariate equations, supplied by Shafer on 2026-11-28).  The rVWF
# models pooled four studies (phase 1 NCT00816660; phase 3 NCT01410227 and
# NCT02283268; EudraCT 2011-004314-42): 79 adults in the VWF:RCo model, age
# 35 (18-70) years, weight 74 (43.8-145) kg, haematocrit 0.417 (0.31-0.48),
# 46 men and 33 women, 57 with type 3 VWD, 12 with types 1 and 2 and 10 with
# severe haemophilia A; 41 of them (31 type 3) in the factor VIII model
# (Table 2).  VWF:RCo and FVIII:C one-stage, NONMEM.
#
# Table 4, rVWF, VWF:RCo:
#
#     CL = 2.10 dL/h x (WT/75)^0.75        Q  = 2.29 dL/h x (WT/75)^0.75
#     Vc = 43.5 dL x (WT/75) x (Hct/0.4)^-0.334
#     Vp = 15.8 dL x (WT/75)
#
# (the weight exponents fixed, the reference weight and haematocrit from the
# Supplementary Methods' equations; estimated exponents 0.54 and 0.33 were
# rejected by the authors as out of line with allometry).  The endogenous
# VWF:RCo was each patient's own, fixed at 0.5 IU/dL in type 3 (EVWF), and
# once it was accounted for the types did not differ.
#
# Factor VIII (Table 4 and the Supplementary Methods; R/vwfFactorVIII.R):
#
#     dF/dt = kout F0 - kout (1 - Imax V/(IC50 + V)) F
#     F0 = 0.500 IU/dL x (Hct/0.39)^-0.571, kout 15.9 /h, Imax 0.998,
#     IC50 0.0658 IU/dL
#
# with V the total VWF:RCo.  F0 is "the theoretical baseline FVIII:C assuming
# that no VWF is affecting kout" (note d).
#
# HAEMATOCRIT: the Patient Profile has none, so it is held at 0.4, the
# reference of Vc (Vc's factor 1) and near the cohort's median 0.417; F0 is
# then 0.500 x (0.4/0.39)^-0.571 = 0.493 IU/dL.
#
# THE BASELINE: the Patient Profile's VWF:RCo baseline (IU/dL), added to the
# infused activity and driving the factor VIII model with it.  Blank, 0.5
# IU/dL, the source's fixed type 3 value (its population's majority), half
# the 1 IU/dL limit of quantification, below which most type 3 patients'
# levels fell (Supplementary Results).  In the factor VIII model the VWF:RCo
# is the individual's predicted activity, the endogenous level included, and
# kin = F0 x kout from the steady state (Supplementary Methods).
#
# WHAT IS DRAWN: the VWF:RCo on this drug's panel and the factor VIII it
# leaves in the patient on the panel of factorVIIIvwf (R/drugs_factorVIIIvwf.R),
# which cannot be dosed.  Recombinant factor VIII given with Vonvendi (the
# label's advice when FVIII:C is below 40 IU/dL) is a drug of its own
# (factorVIII) and is not added to that panel: the source fitted factor VIII
# from rVWF alone (the rFVIII data entered only the pdVWF/FVIII model), and
# infused factor VIII is cleared by the same VWF-dependent process, which a
# sum of two independent curves would not carry.
#
# CHECKS against the source: at VWF:RCo 50 IU/dL the elimination rate of
# factor VIII is 15.9 x (1 - 0.998 x 50/50.0658) = 0.0527 /h, a half-life of
# 13.2 h; the supplement prints 0.06 /h and 11.5 h, the difference being the
# rounding of Imax to three figures (Imax 0.9976 gives 0.059 /h).  Against
# the label (VONVENDI prescribing information, BLA 125577, revised 7/2026,
# 12.2-12.3), at 75 kg after 50 IU/kg: incremental recovery 75/43.5 = 1.72
# IU/dL per IU/kg (label PK50 mean 1.9, SD 0.41; its dosing rule assumes 2.0);
# clearance 0.028 dL/kg/h (label 0.02, SD 0.005; Table 8's steady state
# 0.04); half-lives 3.3 h and 21.1 h (label terminal 22.6 h, SD 5.3).
# Factor VIII after that single dose in type 3 disease: from 4.2 IU/dL (the
# steady state on 0.5 IU/dL of VWF:RCo) to a peak of 98 IU/dL at 28 h; the
# label's peak is "approximately 24 hours post-infusion", and its Cmax after
# a year of 50 IU/kg twice weekly a median of 97 IU/dL (12.2).
# Against the source's own simulation: 50 IU/kg every 72 h in the typical
# patient keeps factor VIII "above 40 IU/dL ... for the full 72 h dosing
# interval" (Results, Figure 4A); the model's troughs are 41.4 IU/dL after the
# first interval and 45.2 at steady state, its steady-state peak 110.
#
# BAND: the label's VWF:RCo peak target for major surgery, 100 IU/dL, minor
# 50-60 (2.2, Table 3); drawn 50 to 100, typical 100.  Its trough targets
# after major surgery, above 50 IU/dL (Table 4), are the band's lower edge.
#
# BODY SIZE: the source's own weight terms on every parameter, kept as
# fitted, evaluated at the pharmacokinetic weight with the switch on and at
# total weight with it off; no cohort anchor is needed.
#
# AGE: the source studied adults (18-70 years); the label approves Vonvendi
# from 1 year (8.4).  A child is drawn with the adult model scaled by weight,
# which is an extrapolation the help page states.
#
# PREGNANCY: none studied; the label has no data (8.1).  Endogenous VWF and
# factor VIII rise in late pregnancy, which is the baseline field's to carry.
#
# References
# ----------
# Bauer A et al., J Blood Med 2023;14:399-411.
#   https://doi.org/10.2147/JBM.S395845
# VONVENDI prescribing information, BLA 125577, revised 7/2026.
# -----------------------------------------------------------------------------

VONICOG_CL         <- 2.10     # dL/h at 75 kg (Table 4)
VONICOG_VC         <- 43.5     # dL at 75 kg, haematocrit 0.4
VONICOG_Q          <- 2.29     # dL/h at 75 kg
VONICOG_VP         <- 15.8     # dL at 75 kg
VONICOG_REF_WEIGHT <- 75       # kg (Supplementary Methods)
VONICOG_HCT_VC     <- -0.334   # haematocrit exponent on Vc, reference 0.4
VONICOG_HCT        <- 0.4      # held: the Patient Profile has no haematocrit
# Blank: the source's fixed endogenous VWF:RCo in type 3 (EVWF, Table 4)
VONICOG_BASELINE   <- 0.5      # IU/dL

# Factor VIII (Table 4, rVWF model)
VWF_FVIII_KOUT     <- 15.9     # /h
VWF_FVIII_F0       <- 0.500    # IU/dL at haematocrit 0.39 and no VWF
VWF_FVIII_HCT      <- -0.571   # haematocrit exponent on F0, reference 0.39
VONICOG_FVIII_IMAX <- 0.998
VONICOG_FVIII_IC50 <- 0.0658   # IU/dL

#' Von Willebrand factor (recombinant), vonicog alfa (Bauer 2023)
#'
#' Two compartments, plotted as the patient's own VWF:RCo plus the infused,
#' in IU/dL, with the factor VIII it protects drawn on the panel of
#' \code{factorVIIIvwf}. See the header of \code{R/drugs_vonicogAlfa.R}.
#'
#' @inheritParams cefazolin
#' @param vonWillebrandFactorBaseline the patient's own VWF:RCo activity in
#'   IU/dL, or NULL for 0.5 IU/dL, the source's value in type 3 disease
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
vonicogAlfa <- function(weight, height, age, sex, adjustToFFM = TRUE,
                        vonWillebrandFactorBaseline = NULL)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)
  pkW  <- if (isTRUE(adjustToFFM)) size$pkWeight else weight
  w <- pkW / VONICOG_REF_WEIGHT

  baseline <- if (is.null(vonWillebrandFactorBaseline)) VONICOG_BASELINE else
    vonWillebrandFactorBaseline

  # dL to L, per hour to per minute
  default <- list(
    v1  = VONICOG_VC / 10 * w * (VONICOG_HCT / 0.4)^VONICOG_HCT_VC,
    v2  = VONICOG_VP / 10 * w,
    v3  = 1,
    cl1 = VONICOG_CL / 10 / 60 * w^0.75,
    cl2 = VONICOG_Q  / 10 / 60 * w^0.75,
    cl3 = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Bauer 2023 (79 adults, 18-70 years, with von Willebrand disease, mostly type 3, ",
    "or haemophilia A, given Vonvendi): two compartments, CL 2.10 dL/h, Vc 43.5 dL, ",
    "Q 2.29 dL/h, Vp 15.8 dL at 75 kg, weight^0.75 on the clearances and weight on ",
    "the volumes, haematocrit held at 0.4. Plotted as the patient's VWF:RCo plus the ",
    "infused: baseline ", signif(baseline, 3), " IU/dL",
    if (is.null(vonWillebrandFactorBaseline)) " (blank: the source's type 3 value)" else "",
    ". The factor VIII it protects is drawn as factorVIIIvwf. Band: the label's ",
    "peak targets, 50-60 IU/dL (minor surgery) to 100 (major). ",
    "https://doi.org/10.2147/JBM.S395845")

  list(
    PK = PK, tPeak = 0, MEAC = 0,
    typical = 100, upperTypical = 100, lowerTypical = 50,
    reference = reference,
    endogenousActivity = baseline,
    # Active itself, not a prodrug of the factor VIII drawn with it: read by
    # the help page (R/help-drugs.R)
    prodrug = FALSE,
    factorVIIIResponse = list(
      name        = "factorVIIIvwf",
      kout        = VWF_FVIII_KOUT / 60,
      imax        = VONICOG_FVIII_IMAX,
      ic50        = VONICOG_FVIII_IC50,
      baseline    = VWF_FVIII_F0 * (VONICOG_HCT / 0.39)^VWF_FVIII_HCT,
      vwfBaseline = baseline
    )
  )
}
