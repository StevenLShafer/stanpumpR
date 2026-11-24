# -----------------------------------------------------------------------------
# Paclitaxel protein-bound (nab-paclitaxel, Abraxane), intravenous (Chen
# 2014): total plasma paclitaxel, three compartments with saturable
# elimination and saturable distribution in both directions
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, amounts in mg, concentrations in
# mcg/mL (= mg/L).
#
# Drafted by Claude Code, 2026-11-16, at the request of Steven L. Shafer, who
# supplied the PDF and decided (2026-11-16) that the saturable return be
# carried by the engine and that nab-paclitaxel be a drug of its own, not a
# candidate of the solvent-based paclitaxel group (a different product).
# Every number below was read on the pages of the PDF.
#
# THE SOURCE
# ==========
# Chen N, Li Y, Ye Y, Palmisano M, Chopra R, Zhou S.  Pharmacokinetics and
# pharmacodynamics of nab-paclitaxel in patients with solid tumors:
# disposition kinetics and pharmacology distinct from solvent-based
# paclitaxel.  J Clin Pharmacol 2014;54:1097-1107 (doi 10.1002/jcph.304).
# 150 patients with solid tumours from eight studies (five phase I, one in
# hepatic impairment; one phase II; two phase III), nab-paclitaxel alone,
# 80-375 mg/m2, nearly all over 30 min; 1418 whole-blood or plasma
# concentrations (the whole-blood/plasma ratio estimated at 1.00); LC-MS/MS;
# NONMEM 7.2, FOCEI.
#
# THE MODEL (Fig. 1; Table 2)
# ===========================
#     dA1/dt = - VMEL C1/(KMEL + C1) - VMTR (C1 - C2)/(KMTR + C1) - Q2 (C1 - C3)
#                                                       (plus the infusion)
#     dA2/dt =   VMTR (C1 - C2)/(KMTR + C1)
#     dA3/dt =   Q2 (C1 - C3)
#
#     V1 15.8 L   V2 1650 L   V3 75.4 L   Q2 41.6 L/h
#     VMTR 325000 ug/h   KMTR 4260 ug/L   VMEL 8070 ug/h   KMEL 40.2 ug/L
#     VMEL = 8070 x (albumin / 3.9)^0.554     (albumin g/dL, centred on the median)
#
# Figure 1 labels the V1-to-V2 rate constant VMTR/(V1 (KMTR + C)) and the
# V2-to-V1 one VMTR/(V2 (KMTR + C)), with one C, and its legend defines KMTR
# as "paclitaxel concentration in the central compartment at 50% of VMTR"
# (and C in its PD part is the central concentration): so both rate
# constants are saturated by the central concentration C1, and the net
# transfer is VMTR (C1 - C2)/(KMTR + C1).  [decision] Claude's reading of
# 2026-11-16, for Shafer: the other reading, each direction saturated by its
# own side (return VMTR C2/(KMTR + C2)), differs by at most 3.5% (at 2 h after
# 300 mg/m2) and by under 1% in clearance.  It is carried by the
# saturable-distribution block of advanceMichaelisMenten() with the store's
# volume (a saturable return, added for this drug), and V3 by the PK set's
# linear compartment.
#
# UNITS.  Table 2 prints VMTR and VMEL in "mg/h" and KMTR and KMEL in "mg/L",
# which cannot be: 8070 mg/h would eliminate a 470-mg dose in 4 minutes.  They
# are ug/h and ug/L.  Table 3's solvent-based column (VMEL 31931, KMEL 452) is
# Joerger 2006's model (35.9 umol/h and 0.667 umol/L at 853.9 g/mol, i.e.
# 30650 ug/h and 570 ug/L) only in micrograms, and V1 + V2 + V3 = 1741 L is
# the label's "total volume of distribution ... approximately 1741 L".
#
# CHECKS (test-drugs-paclitaxelProteinBound.R): the label (ABRAXANE,
# 2026-04-06): total clearance 13 to 30 L/h/m2 over 80 to 300 mg/m2, terminal
# half-life 13 to 27 h, total volume 1741 L.
#
# NOT APPLIED: total bilirubin, CrCl, age and BSA had no significant effect.
# The label's 22-26% lower VMEL in moderate and severe hepatic impairment is
# a categorical finding (Chen's Fig. 2C), not a covariate of the model.
#
# ALBUMIN: the Patient Profile's Serum albumin, NULL (blank) meaning the
# median 3.9 g/dL; floored at MIN_ALBUMIN.
#
# DOSE BASIS: mg of paclitaxel, as labelled (paclitaxel has no salt).
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# No size covariate: weight and BSA were tested and not retained (as the
# label says, none across 40 to 143 kg and 1.3 to 2.4 m2).  Until 2026-11-24
# every parameter was fixed, after the precedent of Crombag's paclitaxel,
# which Shafer's first-principles rule of 2026-11-19 overturned ("tested and
# rejected" no longer exempts a parameter; docs/weight-adjustment.md, *Every
# parameter is scaled*; Gemini's chemotherapy audit of 2026-11-23, F4).  So,
# with the switch on, V1, V2 (the saturable store) and V3 take the
# fat-free-mass volume factor and VMEL, VMTR and Q the clearance factor,
# anchored at Chen's cohort (PACLITAXEL_CHEN_COHORT, below), so that a patient
# like Chen's receives the published values; KMEL and KMTR (concentrations)
# are not scaled.  None carries a sex term, so they are anchored at the
# cohort's pooled patient.  With the switch off they are unscaled, as before.
#
# PREGNANCY: see its registry row.  NO EFFECT SITE, NO BAND.
# -----------------------------------------------------------------------------

# Chen 2014, Table 2 (micrograms, see the header)
PACLITAXEL_CHEN <- list(
  v1 = 15.8, v2 = 1650, v3 = 75.4,      # L
  q2 = 41.6,                            # L/h, V1-V3
  vmtr = 325000, kmtr = 4260,           # ug/h, ug/L
  vmel = 8070, kmel = 40.2,             # ug/h, ug/L
  albuminExp = 0.554, albuminRef = 3.9  # g/dL
)

# Chen 2014, Table 1 (card PMID_24719309.md): medians 74 kg, BSA 1.9 m2, 57
# years; 60 men and 90 women.  No height is printed: 175.6 cm solves
# Mosteller's BSA back from 1.9 m2 at 74 kg (1.9^2 x 3600 / 74).
PACLITAXEL_CHEN_COHORT <- list(weight = 74, height = 175.6, age = 57, men = 60, women = 90)

#' Paclitaxel protein-bound (nab-paclitaxel), intravenous (Chen 2014)
#'
#' Three compartments with saturable elimination and saturable distribution
#' in both directions; serum albumin on the maximal elimination rate.  See
#' the header of \code{R/drugs_paclitaxelProteinBound.R}.
#'
#' @inheritParams cefazolin
#' @param albumin serum albumin in g/dL, or NULL for the median 3.9
#' @returns a list in the shape \code{getDrugPK()} expects, with a
#'   \code{michaelisMenten} block
#' @export
paclitaxelProteinBound <- function(weight, height, age, sex, adjustToFFM = TRUE,
                                   albumin = NULL)
{
  p <- PACLITAXEL_CHEN
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume = 1, legacyClearance = 1,
                        cohort = do.call(cohortSizeFactors, PACLITAXEL_CHEN_COHORT))
  alb <- if (is.null(albumin)) p$albuminRef else max(albumin, MIN_ALBUMIN)
  vmax <- p$vmel * (alb / p$albuminRef)^p$albuminExp / 1000 / 60 *
    size$clearance                                                 # mg/min
  km <- p$kmel / 1000                                             # mg/L

  default <- list(
    v1 = p$v1 * size$volume,
    v2 = p$v3 * size$volume,                 # the source's V3, linear
    v3 = 1,                                  # placeholder: the store is in the block
    # The low-concentration limit of the saturable elimination, Vmax / Km,
    # for the help tables only: the simulation uses the michaelisMenten block.
    cl1 = vmax / km,
    cl2 = p$q2 / 60 * size$clearance,
    cl3 = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Chen N, Li Y, Ye Y, Palmisano M, Chopra R, Zhou S. Pharmacokinetics and pharmacodynamics ",
    "of nab-paclitaxel in patients with solid tumors: disposition kinetics and pharmacology ",
    "distinct from solvent-based paclitaxel. J Clin Pharmacol 2014;54:1097-1107. 150 adults: ",
    "three compartments, saturable elimination (VMEL 8070 ug/h x (albumin/3.9)^0.554, KMEL ",
    "40.2 ug/L), saturable distribution both ways to V2 1650 L (VMTR 325000 ug/h, KMTR 4260 ",
    "ug/L), linear to V3 75.4 L (Q2 41.6 L/h), V1 15.8 L (Table 2's mg read as ug); ",
    "volumes and maximal rates scaled to fat-free mass, anchored at the cohort (74 kg, 57 y, 60 men, 90 women). ",
    "Nab-paclitaxel (Abraxane) only. Total plasma paclitaxel. https://doi.org/10.1002/jcph.304"
  )

  list(
    PK = PK,
    tPeak = 0,
    MEAC = 0,
    typical = 0,
    upperTypical = 0,
    lowerTypical = 0,
    reference = reference,
    michaelisMenten = list(
      vmax = vmax, km = km,
      saturableDistribution = list(
        vmax = p$vmtr / 1000 / 60 * size$clearance,  # mg/min
        km = p$kmtr / 1000,                      # mg/L
        volume = p$v2 * size$volume              # L: the return saturates too
      )
    )
  )
}
