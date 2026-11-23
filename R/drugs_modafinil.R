# -----------------------------------------------------------------------------
# Modafinil: R- and S-modafinil as parallel systems (FDA review of NDA 20-717)
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in mcg/mL (= mg/L) of total modafinil, R + S.  Doses are
# modafinil as labelled (Provigil is the racemate itself, not a salt), by
# mouth only.
#
# SOURCE
# ======
# No population model of racemic modafinil could be read in full: Darwish 2012
# (J Clin Pharmacol 52:1328), Tao 2010 (Ther Drug Monit 32:189) and Seng 2011
# (Ther Drug Monit 33:719) are neither in the archive nor in PubMed Central,
# and Wu 2012's PubMed Central copy (PMC4011351) carries only its abstract.
# The model is therefore built from the FDA's Clinical Pharmacology and
# Biopharmaceutics Review of NDA 20-717 (Provigil; reviewer Rae Yuan, received
# 19 November 1997; Drugs@FDA, 020717A_Provigil_clinphrmr_P1-P7.pdf), a scan
# read on the page images (2026-11-19), which reports the sponsor's
# enantiomer-specific pharmacokinetics.
#
# Study 5, CEP-2101 (review Appendix A pp. 10-11 and the report's Tables I-IV):
# 32 healthy young men, 8 per dose group (6 active, 2 placebo), modafinil
# 200, 400, 600 or 800 mg once daily for 7 days (800 mg stopped after 3 days
# for severe hypertension in one subject), dense sampling on Days 1 and 7,
# troughs on Days 2-7, achiral and enantioselective assays.  The two
# enantiomers do not interconvert (label, 12.3); l-(-)-modafinil is
# R-modafinil (armodafinil), whose half-life the label gives as about three
# times S-modafinil's, which is the d-(+) enantiomer here.  Overall means
# (+/- SD) across 200-600 mg (n = 18; the review: linear and dose-independent
# over 200-600 mg/day):
#
#                         Day 1                      Day 7 (steady state)
#                 CL/F      V/F    ka         CL/F      V/F    ka
#   R, l-(-)     34+/-7    42+/-6  0.79+/-0.26  29+/-8   38+/-8  1.4+/-0.6
#   S, d-(+)    102+/-20   36+/-6      -       100+/-27  39+/-7     -
#                (mL/min)  (L)     (/h)        (mL/min)  (L)     (/h)
#
# (Table IV for R, Table III for S.)  R-modafinil "could fit to a
# one-compartment model with first-order absorption" (the review, p. 11),
# which gave ka; S-modafinil could not be fitted for want of measurable
# points, and the review notes that both were absorbed alike (Tmax about
# 2 h for both).  The half-lives these give, R 15.1 h and S 4.5 h at Day 7,
# match the tabulated 16 and 4.7 h.  The review's Comment 1 says the
# enantiomer CL/F and V/F were computed on the racemic dose; the tables
# themselves were not: each CL/F is the ENANTIOMER's dose over its AUC
# (R at 200 mg, Day 1: 100 mg / 50 ug.h/mL = 2.0 L/h = 33 mL/min, tabulated
# 35; S: 100 / 17 = 98 mL/min, tabulated 103), and the two enantiomers'
# AUCs sum to the racemate's (400 mg: 97 + 32 = 129, Table II's 129).
#
# WHICH DAY
# =========
# Both sets are published in the same tables.  This model uses the Day 7
# (steady-state) set, the state of once-daily use, and because it reproduces
# the study's own observations (checked 2026-11-19 against the closed-form
# sum of the two systems, every number read from the review):
#   - peak racemate, 200/400/600 mg: Day 1 -11%, -2%, -2% of the observed
#     mean Cmax (4.8, 8.7, 13 ug/mL); Day 7 -13%, +1%, -2% (6.4, 11, 17);
#   - troughs on Days 2-7 at all three doses (Table I): racemate -2% on
#     average (-21% to +14%), R-modafinil -1%.
# The Day 1 set does worse on its own study (Day 1 Cmax -22%, -14%, -13%;
# troughs -17% on average), most of it in accumulation: R-modafinil's CL/F
# was 15% lower at Day 7.  The label's description agrees: R-exposure at
# steady state about three times S (this model 100/29 = 3.4), the trough 90% R
# (this model 95%), effective half-life 15 h, Tmax 2-4 h (this model 2.1 h
# after one dose), V/F 0.9 L/kg.
#
# The absorption rate of S-modafinil is taken to be R-modafinil's (the
# review's observation above); bioavailability is apparent (the absolute
# value is unknown: modafinil is too insoluble to give intravenously, label
# 12.3), so the parameters are CL/F and V/F and only the oral route is
# offered.  No lag was reported.
#
# An external check: in the food study MOD-022 (Attachment A-7, Table 1),
# 12 healthy men fasted, 200 mg once, the mean racemate peaked at 3.84 ug/mL
# at 2 h and was 0.58 ug/mL at 24 h; this model gives 4.26 at 2.1 h (+11%)
# and 0.98 at 24 h (+69%).  MOD-022's men cleared modafinil faster than
# CEP-2101's (AUC 51.8 against 74 ug.h/mL at the same dose; this model's
# single-dose AUC is 74.1), and the six of them in whom the
# enantiomers were measured (Table 6.2-77) gave R CL/F 36.1 and S 119 mL/min
# (fasting), close to CEP-2101's Day 1; the difference is between cohorts.
#
# WHAT IS NOT MODELLED
# ====================
# Modafinil acid and modafinil sulfone (inactive, label 12.3); the slow
# auto-induction the label describes after weeks at 400 mg/day; the delay of
# about 1 h in Tmax with food (label 12.3); the halved clearance in severe
# hepatic impairment (label 12.3, 8.6) and the lower clearance of the elderly
# (label: about 20% at 63 years, and plasma levels about doubled at 82 years
# on many concomitant drugs) -- the Patient Profile has no hepatic field and
# the elderly data are not a covariate.  Renal failure (creatinine clearance
# <= 20 mL/min) did not change modafinil's own kinetics (label 12.3).  No
# effect site: tPeak 0, plasma only.
#
# BODY SIZE
# =========
# The published values carry no size covariate, so the model inherits the
# library's fat-free-mass scaling (docs/weight-adjustment.md), with the
# published values unscaled when the switch is off (legacyVolume = 1).
# BODY SIZE ANCHOR: the values are those of CEP-2101's men, whose mean weight
# the per-kilogram columns imply (V/F 38 L and 0.49 L/kg, CL/F 29 mL/min and
# 0.36 mL/min/kg: 78-81 kg), but the review prints neither their age nor their
# height, so the cohort's fat-free mass cannot be computed and the model is
# left at the reference anchor, a [decision] for Shafer
# (docs/weight-adjustment.md, Anchoring at the source's cohort).
#
# PREGNANCY
# =========
# No pharmacokinetic study of modafinil in pregnancy exists (the registry
# row); the flag is not read.
#
# THERAPEUTIC RANGE
# =================
# Hart 2026 (AGNP consensus, update 2026), Table 5 (p. 204, read on the PDF):
# 1,000-1,700 ng/mL 24 h after the dose of 200 mg/day, at steady state (3 d),
# alert level 3,400 ng/mL, level of recommendation 3; based on Xu 2008's
# steady state in healthy Chinese men.  Drawn as 1.0-1.7 mcg/mL.  On this
# model 200 mg once daily gives a steady-state trough of 1.43 mcg/mL.
#
# (Claude Code, 2026-11-19, at the request of Steven L. Shafer.)
# -----------------------------------------------------------------------------

MODAFINIL_ENANTIOMER_FRACTION <- 0.5          # 1:1 racemate, no salt
MODAFINIL_KA_PO <- 1.4 / 60                   # 1/min, R-modafinil, Day 7 (Table IV)
MODAFINIL_R_V   <- 38                         # L, V/F, Day 7 (Table IV)
MODAFINIL_R_CL  <- 29 / 1000                  # L/min, CL/F 29 mL/min, Day 7
MODAFINIL_S_V   <- 39                         # L, V/F, Day 7 (Table III)
MODAFINIL_S_CL  <- 100 / 1000                 # L/min, CL/F 100 mL/min, Day 7

#' Modafinil pharmacokinetics (oral, R and S enantiomers summed)
#'
#' Two one-compartment systems, R- and S-modafinil, each receiving half of
#' every dose; see the header of \code{R/drugs_modafinil.R}.
#'
#' @param weight weight in kg
#' @param height height in cm
#' @param age age in years
#' @param sex "male" or "female"
#' @param adjustToFFM scale to fat-free mass (TRUE) or use the published
#'   values unscaled (FALSE)
#' @return a stanpumpR drug list
#' @keywords internal
modafinil <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1)
  oral <- list(ka_PO = MODAFINIL_KA_PO, bioavailability_PO = 1, tlag_PO = 0)

  # R-modafinil (l-(-), armodafinil), one compartment
  default <- c(list(
    v1  = MODAFINIL_R_V * size$volume,
    v2  = 1,
    v3  = 1,
    cl1 = MODAFINIL_R_CL * size$clearance,
    cl2 = 0,
    cl3 = 0
  ), oral)

  # S-modafinil (d-(+)), one compartment
  sSet <- c(list(
    v1  = MODAFINIL_S_V * size$volume,
    v2  = 1,
    v3  = 1,
    cl1 = MODAFINIL_S_CL * size$clearance,
    cl2 = 0,
    cl3 = 0
  ), oral)

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "FDA Center for Drug Evaluation and Research. Clinical Pharmacology and ",
    "Biopharmaceutics Review, NDA 20-717 (Provigil, modafinil), R. Yuan, 1997: ",
    "study CEP-2101, R- and S-modafinil at steady state (Day 7 means), ",
    "one compartment each, summed; S absorbed at R's rate; apparent oral ",
    "parameters. ",
    "https://www.accessdata.fda.gov/drugsatfda_docs/nda/98/020717A_Provigil_clinphrmr_P2.pdf"
  )

  list(
    PK = PK,
    doseFraction = MODAFINIL_ENANTIOMER_FRACTION,
    parallelSystems = list(
      list(name = "S-modafinil", doseFraction = MODAFINIL_ENANTIOMER_FRACTION,
           PK = list(default = sSet))
    ),
    tPeak = 0,
    MEAC = 0,
    typical = 1.35,
    upperTypical = 1.7,
    lowerTypical = 1.0,
    reference = reference
  )
}
