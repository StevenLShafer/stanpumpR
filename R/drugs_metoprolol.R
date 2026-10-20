# -----------------------------------------------------------------------------
# Metoprolol, intravenous and oral (Lammers 2017): healthy men, by CYP2D6
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min.
#
# Drafted by Claude Code, 2026-10-19, at the request of Steven L. Shafer, as
# one of the vasoactive and resuscitation drugs used in intensive care
# (inst/help/models/vasoactive.md, inst/extdata/vasoactiveRegistry.csv), from
# the full text, which he supplied.
#
# THE SOURCE
# ==========
# Lammers LA, Achterbergh R, van Schaik RHN, Romijn JA, Mathot RAA.  Effect
# of short-term fasting on systemic cytochrome P450-mediated drug metabolism
# in healthy subjects: a randomized, controlled, crossover study using a
# cocktail approach.  Clin Pharmacokinet 2017;56:1231-1244
# (doi 10.1007/s40262-017-0515-7).  Twelve healthy white
# men (19-33 years, 68-103 kg, median 80) given a probe cocktail containing
# metoprolol 20 mg intravenously (1 mg/mL ampoules, AstraZeneca) and, on
# another occasion, a 100 mg tablet, each after an overnight fast (control)
# and after 36 hours of fasting; LC-MS/MS plasma, sampled for 9 hours after
# the intravenous dose and 10 after the oral.  NONMEM, two compartments with
# first-order absorption (Table 3):
#
#     CL 65.8 L/h     V1 187 L     Q 74.5 L/h     V2 97.6 L
#     ka 1.19 /h      F 0.45
#
# CYP2D6: one poor metaboliser (*4/*4) had 0.44 x the clearance and 2.28 x
# the bioavailability (so F 1.03, held at 1 here); one ultrarapid metaboliser
# (*1/*1xN) had 2.01 x the clearance and 0.43 x the bioavailability.  Both
# factors were estimated from those two men alone and then FIXED, so they
# are the right direction and roughly the right size, nothing more.  The
# typical subject is the rest of the group, five of whose nine genotyped men
# were intermediate metabolisers (*1/*3, *1/*4) and three normal, so the
# intermediate and normal phenotypes both take the typical values.
#
# The 36-hour fast raised clearance 13% and is not applied: these are the
# overnight-fasted control values.  The doses are the tartrate, as the
# products were labelled.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# Fixed values with no size term, for the source's young men, scaled to
# fat-free mass with the switch on; with it off they are the published values
# for everyone (legacyVolume = 1).
#
# EFFECT SITE: tPeak 3.3 MINUTES, FOR HEART RATE (Ritchie 1998); NO BAND
# ======================================================================
# Lammers measured no effect.  Ritchie RH, Morgan DJ, Horowitz JD, J Pharm
# Sci 1998;87:177-182 (doi 10.1021/js9702776, read in full) gave 4 mg
# intravenously to 12 patients with ischaemic heart disease at cardiac
# catheterisation and fitted an effect compartment to MYOCARDIAL metoprolol
# content (uptake 1.85 /min, efflux 0.061 /min), not plasma, so its ke0 is
# not carried over; but on its own model the reduction in heart rate (ke0
# 1.30 /min) peaks 3.3 minutes after the bolus, and that time to peak is
# used.  Its 95% interval is wide (ke0 -0.06 to 2.66 /min).  The paced PR
# interval and LV dP/dt max (ke0 0.30 and 0.29 /min) peak about 7.3
# minutes after the bolus, in line with the 5-10 minutes observed in the
# companion papers; heart rate is the endpoint used here.
#
# PREGNANCY
# =========
# Ryu and colleagues (2015) measured steady-state oral metoprolol across
# pregnancy, each woman paired with herself 3 months or more postpartum: the
# apparent oral clearance CL/F was 568 +/- 273 L/h in late pregnancy (34-38
# weeks, n = 8) against 192 +/- 98 L/h postpartum, x 2.96 (361 against 200,
# x 1.8, in mid-pregnancy, n = 5), with the metabolic ratio falling, as
# induction of CYP2D6 would make it; in the women on the same dose in both
# periods the ratio was 551/194 = 2.84.  Protein binding was low and
# unchanged (9% against 11%).  Hogstedt 1985 gave intravenous and oral doses
# to 5 women in late pregnancy and again 3 to 6 months postpartum: oral
# bioavailability was 1.3 to 3.7 times higher after pregnancy, while the
# systemic clearance after pregnancy, 26 to 97% of the pregnant value, did not
# differ significantly; Hogstedt 1983 (n = 5) found CL/F 4.4 times higher in
# late pregnancy.  The change is therefore mostly in hepatic first pass.
#
# Lammers' model has a systemic clearance and an oral bioavailability, so
# Ryu's 2.96 is split between them with a well-stirred liver at the hepatic
# extraction ratio Ryu gives, 0.5: a 2.96-fold intrinsic clearance raises the
# extraction to 2.96 / 3.96 = 0.75, so the systemic clearance rises by
# 0.75 / 0.5 = 1.5 (METOPROLOL_PREGNANCY_CL_FACTOR) and the bioavailability
# falls to 0.25 / 0.5 = 0.5 of its value (METOPROLOL_PREGNANCY_F_FACTOR), and
# CL/F rises by 1.5 / 0.5 = 3.0.  That agrees with Hogstedt's range of
# bioavailability and with his small, non-significant rise in systemic
# clearance.  Hogstedt 1985, read in full (2026-10-19), measured both halves
# of the split directly: bioavailability 0.21 +/- 0.06 in pregnancy against
# 0.42 +/- 0.05 after it (P < 0.01), exactly the factor 0.5, and systemic
# clearance 1.38 against 0.65 L/min (the postpartum value 55% +/- 13% of the
# pregnant one, not significant).  It also found the half-lives identical,
# 5.38 against 5.36 h intravenously and 3.42 against 3.42 h orally, with a
# larger volume in pregnancy (6.87 against 3.85 L/kg, not significant):
# clearance and volume rose together.  So the volumes and the distribution
# clearance take the clearance's factor as well
# (METOPROLOL_PREGNANCY_V_FACTOR = 1.5), which leaves every rate constant,
# and so the half-lives, unchanged, as Hogstedt measured.  With the Patient
# Profile's Pregnant field on, CL1, V1, V2 and Q take 1.5 and the oral
# bioavailability 0.5; ka is unchanged.  They
# are NOT applied to a CYP2D6 poor metaboliser, since pregnancy cannot induce
# an enzyme that is absent (Ryu's one poor metaboliser had a very low CL/F in
# pregnancy, 0.37-0.41 L/h/kg); they are applied to the intermediate, normal
# and ultrarapid phenotypes, on top of their own factors.  Ryu also reports a
# weight-adjusted ratio of 2.6 (which would give 1.45 and 0.55 by the same
# split); the model already scales clearance with size, but the difference
# is within the uncertainty and the unadjusted ratio is applied.  Late
# pregnancy is the reference state.  (Claude Code, 2026-10-19.)
#
# References
# ----------
# Lammers LA, Achterbergh R, van Schaik RHN, Romijn JA, Mathot RAA.  Clin
#   Pharmacokinet 2017;56:1231-1244.  https://doi.org/10.1007/s40262-017-0515-7
# Ryu RJ, Eyal S, Easterling TR, et al.  Pharmacokinetics of metoprolol
#   during pregnancy and lactation.  J Clin Pharmacol 2016;56:581-589.
#   https://doi.org/10.1002/jcph.631 (pregnancy)
# Hogstedt S, Lindberg B, Peng DR, Regardh CG, Rane A.  Pregnancy-induced
#   increase in metoprolol metabolism.  Clin Pharmacol Ther 1985;37:688-692.
#   https://doi.org/10.1038/clpt.1985.114 (pregnancy)
# Hogstedt S, Lindberg B, Rane A.  Increased oral clearance of metoprolol in
#   pregnancy.  Eur J Clin Pharmacol 1983;24:217-220.
#   https://doi.org/10.1007/BF00613820 (pregnancy)
# -----------------------------------------------------------------------------

# Lammers 2017, Table 3, control (overnight fast)
METOPROLOL_CL <- 65.8      # L/h
METOPROLOL_V1 <- 187       # L
METOPROLOL_Q  <- 74.5      # L/h
METOPROLOL_V2 <- 97.6      # L
METOPROLOL_KA <- 1.19      # /h
METOPROLOL_F  <- 0.45
METOPROLOL_TPEAK <- 3.3   # min, heart rate (Ritchie 1998)
# Fixed from one poor and one ultrarapid metaboliser
METOPROLOL_CYP2D6_CL <- c(poor = 0.44, intermediate = 1, normal = 1,
                          ultrarapid = 2.01)
METOPROLOL_CYP2D6_F  <- c(poor = 2.28, intermediate = 1, normal = 1,
                          ultrarapid = 0.43)

# Late pregnancy relative to the published values, not for a CYP2D6 poor
# metaboliser: Ryu 2015's CL/F ratio 2.96 split by a well-stirred liver at
# extraction 0.5 (extraction 2.96 / 3.96 = 0.75), systemic clearance
# 0.75 / 0.5 = 1.5 and bioavailability 0.25 / 0.5 = 0.5 (see the header).
METOPROLOL_PREGNANCY_CL_FACTOR <- 1.5
METOPROLOL_PREGNANCY_F_FACTOR  <- 0.5
# The volumes and Q with the clearance, so the half-lives are unchanged, as
# Hogstedt 1985 measured.
METOPROLOL_PREGNANCY_V_FACTOR  <- 1.5

#' Metoprolol, intravenous and oral (Lammers 2017)
#'
#' Two compartments with first-order oral absorption in healthy men, with
#' the CYP2D6 poor and ultrarapid metaboliser factors; see the header of
#' \code{R/drugs_metoprolol.R}.
#'
#' @inheritParams cefazolin
#' @param cyp2d6 CYP2D6 metaboliser phenotype, one of \code{CYP2D6_VALUES}.
#' @param pregnant is the patient pregnant?  \code{TRUE} multiplies the
#'   clearance, the volumes and Q by 1.5 and the oral bioavailability by 0.5,
#'   except in a CYP2D6
#'   poor metaboliser (Ryu 2015; see the file's header).
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
metoprolol <- function(weight, height, age, sex, adjustToFFM = TRUE,
                       cyp2d6 = CYP2D6_DEFAULT, pregnant = FALSE)
{
  if (length(cyp2d6) != 1 || !cyp2d6 %in% CYP2D6_VALUES) {
    stop("Invalid cyp2d6: ", paste(cyp2d6, collapse = ", "),
         ". Must be one of: ", paste(CYP2D6_VALUES, collapse = ", "))
  }
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1)
  # Pregnancy (see the header): induced CYP2D6, so not in a poor metaboliser.
  pregnancyApplied <- isTRUE(pregnant) && cyp2d6 != CYP2D6_POOR
  clPregnancy <- if (pregnancyApplied) METOPROLOL_PREGNANCY_CL_FACTOR else 1
  fPregnancy  <- if (pregnancyApplied) METOPROLOL_PREGNANCY_F_FACTOR else 1
  vPregnancy  <- if (pregnancyApplied) METOPROLOL_PREGNANCY_V_FACTOR else 1

  default <- list(
    v1  = METOPROLOL_V1 * size$volume * vPregnancy,
    v2  = METOPROLOL_V2 * size$volume * vPregnancy,
    v3  = 1,                          # placeholder: a two-compartment model
    cl1 = METOPROLOL_CL * METOPROLOL_CYP2D6_CL[[cyp2d6]] / 60 * size$clearance *
      clPregnancy,
    cl2 = METOPROLOL_Q / 60 * size$clearance * vPregnancy,
    cl3 = 0,
    ka_PO = METOPROLOL_KA / 60,
    bioavailability_PO = min(1, METOPROLOL_F * METOPROLOL_CYP2D6_F[[cyp2d6]]) *
      fPregnancy,
    tlag_PO = 0
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Lammers LA, Achterbergh R, van Schaik RHN, Romijn JA, Mathot RAA, Clin Pharmacokinet ",
    "2017;56:1231-1244.  Two compartments in 12 healthy men (20 mg IV, 100 mg ",
    "PO): CL 65.8 L/h, V1 187 L, Q 74.5 L/h, V2 97.6 L, ka 1.19 /h, F 0.45; ",
    "CYP2D6 poor metaboliser CL x 0.44 and F x 2.28, ultrarapid CL x 2.01 and ",
    "F x 0.43 (one subject each); CYP2D6 ", cyp2d6, ".  Time to peak ",
    "heart-rate effect 3.3 min from Ritchie RH et al., J Pharm Sci ",
    "1998;87:177-182. https://doi.org/10.1007/s40262-017-0515-7"
  )
  if (pregnancyApplied) {
    reference <- paste0(reference, "; ", pregnancyReference(
      paste0("clearance, volumes and Q x ", METOPROLOL_PREGNANCY_CL_FACTOR,
             ", oral bioavailability x ", METOPROLOL_PREGNANCY_F_FACTOR),
      paste0("Ryu RJ et al., J Clin Pharmacol 2016;56:581-589. ",
             "https://doi.org/10.1002/jcph.631")))
  }

  list(
    PK = PK,
    tPeak = METOPROLOL_TPEAK,
    MEAC = 0,
    typical = 0,
    upperTypical = 0,
    lowerTypical = 0,
    reference = reference
  )
}
