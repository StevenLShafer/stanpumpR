# -----------------------------------------------------------------------------
# Celecoxib: two-compartment apparent oral model FITTED TO MEAN DATA
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in ng/mL, total plasma celecoxib.
#
# WHERE THE PARAMETERS COME FROM
# ==============================
# No published population model of celecoxib with two systemic compartments
# could be reproduced: the FDA review of NDA 211759 (VYSCOXA oral suspension,
# 2025) says the applicant's models were two-compartment with linear
# elimination but publishes no coefficients, and Hannam 2023 is
# one-compartment.  These parameters are therefore Claude Code's own fit, made
# 2026-10-10 at the request of Steven L. Shafer, to the MEAN plasma curve of a
# single 200 mg Celebrex capsule taken fasting by 51 healthy adults (Study
# 915/22, Figure 1 of the FDA Office of Clinical Pharmacology review, signed
# 6/24/2025).  The 19 mean points were digitized from that figure, a 805 x 375
# pixel image (about 0.17 h and 3 ng/mL per pixel), with the overlapping early
# markers read by eye from enlarged crops:
#
#     t (h)  0.5  1.06 1.26 1.47 1.89 2.57 3.24 4.17 4.58 5.10 6.04
#     C       12  142  257  359  404  412  404  361  333  264  224
#     t (h)    8   10   12   14   24   36   48   72
#     C      167  138  127  114   82   45   23    5
#
# The digitized AUC(0-72 h), 5424 ng.h/mL, agrees with the review's mean
# AUC(0-t), 5379 (Table 1), which checks the digitizing.  A two-compartment
# model with first-order absorption and a lag was fitted by least squares on
# the square-root scale (R, optim, 400 random starts):
#
#     CL/F 36.9 L/h    V1/F 237 L    Q/F 72.0 L/h    V2/F 413 L
#     ka 0.985 /h      lag 0.853 h
#
# It is within 8% of the mean curve from 1 to 48 h; peaks at 445 ng/mL at
# 2.4 h (mean curve 412 at about 2.6 h; the mean of the individual peaks,
# Table 1, is 476, at a median 2.5 h); half-lives 1.2 and 15.0 h (Table 1,
# 14.4 h); AUC(0-inf) 5416 ng.h/mL (Table 1 arithmetic mean 6036, geometric
# least-squares mean 4784).  Independent check, not used in the fit:
# NCT04526197 (35 healthy adults, 200 mg fasting) reports a mean Cmax of
# 637 ng/mL (CV 49%) and AUC(0-inf) 6743 ng.h/mL (CV 38%), so CL/F 29.7 L/h,
# 20% below this fit and within its spread; a third of that cohort was
# Black, in whom the label reports about 40% higher AUC.  A second check:
# Itthipanichpong et al. (18 healthy Thai men, mean 63 kg, 200 mg fasting)
# report CL/F 35.9 L/h, which this fit gives at 63 kg (36.9 x (63/70)^0.265),
# and a mean individual Cmax of 687 ng/mL at 2.5 h.  Means of individual
# peaks always exceed the peak of a mean curve.  A third: Werner et al. (12
# healthy adults, 200 mg capsule) report a mean AUC(0-inf) of 6246 ug.h/L
# in the 11 extensive CYP2C9 metabolizers (CL/F 32.0 L/h), and 12561 in the
# one poor metabolizer.  A fourth, at steady state: Brenner et al. (200 mg
# twice daily for 15 days) report AUC over the dosing interval of 5.8 mg.h/L
# in young adults (71 kg; CL/F 34.5 L/h) and 5.6 in older ones (82 kg;
# 35.7 L/h), with no effect of age.  The label gives
# CL/F about 30 L/h and Vss/F about 400 L.
#
# WHAT A FIT TO MEAN DATA CANNOT GIVE
# ===================================
# No variability between patients.  A mean curve is smoother than any one
# patient's, so the absorption here is slower and the peak lower than a
# typical individual's.  The parameters are APPARENT (divided by an
# unmeasured bioavailability; no absolute bioavailability study exists), so
# celecoxib is offered by mouth only, with bioavailability_PO = 1.
#
# NOT MODELLED
# ============
# Food (a high-fat meal raises the suspension's Cmax 144% and AUC 35-50%);
# the oral suspension (22% lower Cmax at equal AUC); exposure less than
# proportional to dose above 200 mg, from poor solubility; CYP2C9 genotype
# (*3/*3 exposure 3-7 times higher, 8 subjects; no per-allele model); age
# (elderly AUC about 50% higher); hepatic impairment.  No CYP2D6 adjustment.
#
# EFFECT SITE
# ===========
# ke0 is supplied directly from Hannam et al. (2023), who re-analysed mean
# pain relief after celecoxib 200 and 400 mg for dental surgery in adults
# with an Emax model: equilibration half-time 1.12 h (SE 3.1%), Ce50 242
# ng/mL (total, plasma scale), Emax 3.25 on the 0-4 pain-relief scale.  They
# estimated it on their own one-compartment PK (CL/F 49 L/h/70 kg, V/F 346 L,
# absorption half-time 0.35 h, lag 0.62 h, 10 healthy adults), not on this
# fit, so pairing the two is an assumption, as it is for ibuprofen.  Their
# plasma-to-CSF equilibration half-time, 0.84 h, is shorter; CSF is not the
# effect site.  The time-until-threshold level (endCe in the CSV) is the
# Ce50, 242 ng/mL in the effect site.  No band; not on the MEAC panel.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# A mean curve carries no size information, so the weight effect is taken
# from the one celecoxib population model with a fitted weight covariate:
# Krishnaswami et al. (one-compartment, NONMEM; children 2-17 years with
# juvenile rheumatoid arthritis, median 41 kg, pooled with 36 adults with
# rheumatoid arthritis) found CL/F proportional to weight^0.265 and V/F to
# weight^0.499 (Table II, final model).  The label's statement that 10 kg
# and 25 kg patients have 40% and 24% lower CL/F than a 70 kg adult is
# (10/70)^0.265 = 0.60 and (25/70)^0.265 = 0.76.  Here the fitted values are
# taken as those of a 70 kg adult; CL/F and Q/F scale by (W/70)^0.265 and
# both volumes by (W/70)^0.499.  W is the pharmacokinetic weight with the
# switch on and total body weight with it off.  The model's own CL/F, 35.2
# L/h at 41 kg (about 40 L/h at 70 kg), agrees with this fit's 36.9.  Its
# 21% higher CL/F in males is not applied: the mean curve already averages
# the sexes of its cohort, whose make-up the review does not give.
#
# References
# ----------
# Hannam JA et al., Paediatr Anaesth 2023;33:291-302.
#   https://doi.org/10.1111/pan.14590
# Itthipanichpong C et al., J Med Assoc Thai 2005;88:632-638 (PMID 16149679).
# Brenner SS et al., Clin Pharmacokinet 2003;42:283-292.
# Werner U et al., Biomed Chromatogr 2002;16:56-60.
#   https://doi.org/10.1002/bmc.115
# Krishnaswami S et al., J Clin Pharmacol 2012;52:1134-1149.
#   https://doi.org/10.1177/0091270011412184
# FDA Office of Clinical Pharmacology Review, NDA 211759 (VYSCOXA), 2025.
# VYSCOXA prescribing information, revised 07/2025.
# NCT04526197 results, ClinicalTrials.gov (Alexion, ALXN1840 with celecoxib).
# -----------------------------------------------------------------------------

CELECOXIB_KE0   <- log(2) / (1.12 * 60)   # /min, Hannam 2023, T1/2keo 1.12 h
CELECOXIB_WT_CL <- 0.265   # Krishnaswami 2012, weight exponent on CL/F
CELECOXIB_WT_V  <- 0.499   # Krishnaswami 2012, weight exponent on V/F

#' Celecoxib pharmacokinetics (apparent oral, fitted to mean data)
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
celecoxib <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  # Own weight covariate (Krishnaswami): the pharmacokinetic weight with the
  # switch on, total body weight with it off.
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1)
  wt <- if (isTRUE(adjustToFFM)) size$pkWeight else weight
  fCl <- (wt / 70)^CELECOXIB_WT_CL
  fV  <- (wt / 70)^CELECOXIB_WT_V

  default <- list(
    v1  = 237   * fV,
    v2  = 413   * fV,
    v3  = 1,                                   # two compartments
    cl1 = 36.9 / 60 * fCl,
    cl2 = 72.0 / 60 * fCl,
    cl3 = 0,
    # Apparent parameters: the whole dose, F = 1
    ka_PO              = 0.985 / 60,           # 1/min
    bioavailability_PO = 1,
    tlag_PO            = 0.853 * 60            # min
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Two-compartment fit by Claude Code (2026) to the mean 200 mg Celebrex ",
    "capsule curve of the FDA Clinical Pharmacology Review, NDA 211759 ",
    "(VYSCOXA), 2025, Study 915/22; checked against NCT04526197. Not a ",
    "population model: no variability, apparent oral parameters. Weight ",
    "exponents from Krishnaswami S et al., J Clin Pharmacol 2012;52:1134-1149; ",
    "ke0 from Hannam JA et al., Paediatr Anaesth 2023;33:291-302."
  )

  list(
    PK = PK,
    # No tPeak: ke0 is supplied directly (Hannam 2023).  See the header.
    tPeak = 0,
    ke0 = CELECOXIB_KE0,
    MEAC = 0,
    typical = 0,
    upperTypical = 0,
    lowerTypical = 0,
    reference = reference
  )
}
