# -----------------------------------------------------------------------------
# Idarucizumab (Praxbind): two compartments, intravenous, clearance linear in
# the Cockcroft-Gault creatinine clearance
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min, amounts in
# mg, concentrations in mcg/mL (= mg/L).  Drafted by Claude Code, 2026-11-28,
# at the request of Steven L. Shafer (docs/library-expansion/plan.csv,
# Biologics (surgery): dabigatran reversal), who chose Glund 2017 with a
# linear creatinine-clearance term on 2026-11-28.
#
# SOURCE
# ======
# Glund S, Stangier J, van Ryn J, et al.  Effect of age and renal function on
# idarucizumab pharmacokinetics and idarucizumab-mediated reversal of
# dabigatran anticoagulant activity in a randomized, double-blind, crossover
# phase Ib study.  Clin Pharmacokinet 2017;56:41-54.  Read on the PDF.  46
# adults (45-80 years) given dabigatran etexilate to steady state and then a
# 5-minute infusion of idarucizumab or placebo: middle-aged (2.5 g, 5 g),
# elderly (1 g, 5 g), mild renal impairment (Cockcroft-Gault 60-90 mL/min; 1
# g, 5 g) and moderate (30-60 mL/min; 2 x 2.5 g an hour apart), 6-8 a group.
# The paper is NON-COMPARTMENTAL: Table 2 gives each group's geometric means
# of CL, Vss and the initial and terminal half-lives, and Table 1 each group's
# mean Cockcroft-Gault creatinine clearance.  It prints no model and no
# covariate equation.
#
# WHAT IS DERIVED HERE (not printed by the source)
# ================================================
# 1. The distribution.  Each group's CL, Vss and two half-lives close a
#    two-compartment model exactly (twoCompartmentFromHalfLives(), as
#    clevidipine's companion check does).  The seven groups' V1 (2.99-3.72 L),
#    V2 (2.67-5.18 L) and Q (4.6-6.7 mL/min) show no trend with dose, age or
#    renal function, and their geometric means weighted by group size are
#    taken: V1 3.360 L, V2 3.689 L, Q 5.577 mL/min.
# 2. The clearance.  A least-squares line through the seven groups' (CrCl,
#    CL) means:
#
#        CL = 4.07 + 0.391 x CrCl   mL/min   (CrCl Cockcroft-Gault, mL/min)
#
#    THIS LINE IS NOT IN THE PAPER: it is Claude Code's fit through its group
#    means, labelled so on the help page.  Weighted by group size it is 4.34 +
#    0.391 x CrCl; through the 5 g groups only, 2.84 + 0.412 x CrCl.  The
#    studied CrCl ran 58.7-110.4 mL/min (group means); below that the line is
#    an extrapolation.  Glund 2018 (Clin Appl Thromb Hemost 24:724-733,
#    PMC6714879, read in full) gives the mechanism: idarucizumab (47.8 kDa) is
#    filtered and then taken up and broken down in the proximal tubule, its
#    clearance "largely controlled by the rate of glomerular filtration" and
#    "constant across the dose levels investigated" (the share excreted
#    unchanged rises with the dose as tubular uptake saturates, and the two
#    balance).  So the dose is not a covariate.
#
# CHECKS against the source: with the line and the shared distribution, the
# groups' initial half-lives are 0.73-1.16 h (measured 0.78-1.16 h, the
# moderately impaired group 1.16 and 1.16), and the terminal half-lives 8.6-9.5
# h (measured 5.8-10.8 h).  At the 5 g middle-aged group's 110.4 mL/min the
# line gives CL 47.2 mL/min (measured 47.1; the label's 47.0).
#
# EXTERNAL CHECK, against patients the line was not fitted to: Glund S,
# Coble K, Gansser D, et al. Pharmacokinetics of idarucizumab and its target
# dabigatran in patients requiring urgent reversal of the anticoagulant
# effect of dabigatran.  J Thromb Haemost 2019;17:1319-1328 (RE-VERSE AD,
# read on the PDF Shafer supplied 2026-11-28).  Also non-compartmental: its
# Table 1 gives the geometric mean AUC 0-24 h after 5 g in 347 bleeding or
# surgical patients (mean age 76.6 years) by Cockcroft-Gault category.  The
# population model it mentions (220 volunteers, 486 patients) is unpublished,
# so there is no patient model to wait for.  Against it, a 5 g bolus with the
# line and the shared distribution gives, at an assumed category CrCl of 99,
# 65, 40 and 21 mL/min (the label, 12.3, prints "median CrCl 21 to 99"; the
# middle two are this header's assumption):
#
#     category (CrCl)    measured AUC0-24    model    model/measured
#     normal (>= 80)          47 100         39 900        0.85
#     mild (50-80)            65 000         57 100        0.88
#     moderate (30-50)        89 700         83 200        0.93
#     severe (< 30)          116 000        125 400        1.08   (nmol h/L)
#
# The measured exposures imply clearances of 36.1, 25.7, 18.1 and 13.5
# mL/min, against the line's 42.8, 29.5, 19.7 and 12.3.  So the line, fitted
# to volunteers aged 45-80, is within 16% of the patients at every level of
# renal function: it underpredicts exposure by 15% with normal kidneys (these
# patients were older and ill, and cleared less than the volunteers) and
# overpredicts by 8% in severe impairment, beyond the studied range.  The
# label's percentages (+38%, +90%, +146% against normal) are these same
# AUCs as ratios, and look worse against the line (+43%, +108%, +214%) only
# because the patients' normal group cleared less.  Their Cmax (20 700
# nmol/L with normal kidneys) was sampled 10-30 min after the second of two
# infusions up to 15 min apart, so it is not compared with the model's peak.
# [decided] by Shafer, 2026-11-28: the line is kept, with this check on its
# help page, rather than refitted to these patients (which would make the
# volunteers' fit 15-20% too fast with normal kidneys).
#
# DOSE BASIS: grams of idarucizumab, as labelled (5 g, two 2.5 g vials).
# Glund reports concentrations in nmol/L; they are plotted here in mcg/mL
# (about 47,766 Da by the label, 11: 1 mcg/mL = 20.9 nmol/L).
#
# RENAL FUNCTION: the Patient Profile's serum creatinine through
# adultEquivalentCreatinine() (Cockcroft-Gault is an adult equation), or the
# library's assumed normal creatinine when blank, in Cockcroft-Gault with the
# weight below.
#
# BODY SIZE (docs/weight-adjustment.md): Glund tested no size covariate, and
# the label says weight "had no clinically important effect" in its
# population analysis; every parameter is scaled nonetheless (Shafer,
# 2026-11-19).  V1, V2, Q and the 4.07 mL/min intercept are anchored at
# Glund's 46 subjects (Table 1, seven groups, heights from each group's
# weight and BMI); the renal slope is not, its Cockcroft-Gault clearance
# carrying weight already.  The Cockcroft-Gault weight is the pharmacokinetic
# weight with the switch on and total weight with it off; with it off the
# volumes, Q and the intercept are fixed.
#
# AGE: adults 45-80 years; the label has no paediatric data (8.4).  A child is
# drawn with the adult model, an extrapolation the help page states.
#
# PREGNANCY (R/pregnancy.R): no pharmacokinetics in pregnancy (PubMed
# 2026-11-28, five records, none with any; the label 8.1 has no data).
# Clearance follows filtration (Glund 2018), so the Cockcroft-Gault term takes
# the rise in filtration of pregnancy (x 1.5, pregnancyGfrFactor(), only with
# the creatinine blank): physiological, an assumption, as for pemetrexed.
#
# NO EFFECT SITE, NO BAND: idarucizumab binds dabigatran "with higher affinity
# than the binding affinity of dabigatran to thrombin" (label 12.1); its
# effect is the dabigatran it binds, which this model does not carry, so there
# is no concentration to target.
#
# References
# ----------
# Glund S et al., Clin Pharmacokinet 2017;56:41-54.
#   https://doi.org/10.1007/s40262-016-0417-0
# Glund S et al., Clin Appl Thromb Hemost 2018;24:724-733.
#   https://doi.org/10.1177/1076029618755947
# PRAXBIND prescribing information, BLA 761025, revised 2/2024.
# -----------------------------------------------------------------------------

# Derived from Glund 2017 Table 2 (header, WHAT IS DERIVED HERE)
IDARUCIZUMAB_V1       <- 3.360     # L
IDARUCIZUMAB_V2       <- 3.689     # L
IDARUCIZUMAB_Q        <- 5.577     # mL/min
IDARUCIZUMAB_CL0      <- 4.07      # mL/min, intercept
IDARUCIZUMAB_CL_CRCL  <- 0.391     # mL/min per mL/min of CrCl

# Glund 2017 Table 1: the seven groups (weight kg, age y, men, women, BMI)
IDARUCIZUMAB_GLUND_COHORT <- list(groups = list(
  list(weight = 78.2, age = 53.2, men = 3, women = 3, bmi = 26.40),
  list(weight = 76.0, age = 50.0, men = 3, women = 3, bmi = 24.35),
  list(weight = 73.8, age = 70.0, men = 4, women = 4, bmi = 26.73),
  list(weight = 80.8, age = 67.5, men = 6, women = 2, bmi = 27.48),
  list(weight = 71.7, age = 62.0, men = 4, women = 2, bmi = 25.38),
  list(weight = 71.7, age = 68.7, men = 4, women = 2, bmi = 26.43),
  list(weight = 65.5, age = 72.0, men = 3, women = 3, bmi = 22.83)
))

#' Idarucizumab, intravenous (derived from Glund 2017)
#'
#' Two compartments derived from Glund 2017's group means, clearance linear in
#' the Cockcroft-Gault creatinine clearance; see the header of
#' \code{R/drugs_idarucizumab.R}.
#'
#' @inheritParams cefazolin
#' @param adjustToFFM \code{TRUE} (default) scales V1, V2, Q and the clearance's
#'   intercept by fat-free mass anchored at Glund 2017's 46 subjects and uses
#'   the pharmacokinetic weight in Cockcroft-Gault; \code{FALSE} fixes those
#'   at the derived values and uses total weight in Cockcroft-Gault
#' @param creatinine serum creatinine, mg/dL, or NULL for an assumed normal
#'   value
#' @param pregnant is the patient pregnant?  \code{TRUE}, with the creatinine
#'   blank, raises the creatinine clearance by the rise in filtration of
#'   pregnancy
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
idarucizumab <- function(weight, height, age, sex, adjustToFFM = TRUE,
                         creatinine = NULL, pregnant = FALSE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume = 1, legacyClearance = 1,
                        cohort = cohortSizeFactors(groups = IDARUCIZUMAB_GLUND_COHORT$groups))
  w <- if (isTRUE(adjustToFFM)) size$pkWeight else weight
  scr <- adultEquivalentCreatinine(creatinine, age, sex)
  gfrPregnancy <- pregnancyGfrFactor(pregnant, creatinine)
  crcl <- creatinineClearanceCG(w, age, sex, scr) * gfrPregnancy      # mL/min

  cl <- IDARUCIZUMAB_CL0 * size$clearance + IDARUCIZUMAB_CL_CRCL * crcl  # mL/min

  default <- list(
    v1  = IDARUCIZUMAB_V1 * size$volume,
    v2  = IDARUCIZUMAB_V2 * size$volume,
    v3  = 1,                                 # placeholder: two compartments
    cl1 = cl / 1000,
    cl2 = IDARUCIZUMAB_Q / 1000 * size$clearance,
    cl3 = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Derived from Glund 2017 (46 adults 45-80 years, middle-aged, elderly and ",
    "with mild or moderate renal impairment; non-compartmental group means): two ",
    "compartments, V1 3.36 L, V2 3.69 L, Q 5.58 mL/min, and CL 4.07 + 0.391 x ",
    "Cockcroft-Gault CrCl mL/min, a line fitted by stanpumpR through the groups' ",
    "means (not printed in the paper; studied CrCl 59-110 mL/min), from the ",
    "entered or an assumed normal creatinine",
    if (gfrPregnancy != 1) paste0("; ", pregnancyReference(
      "creatinine clearance x 1.5, the rise in filtration of pregnancy")) else "",
    ". https://doi.org/10.1007/s40262-016-0417-0")

  list(PK = PK, tPeak = 0, MEAC = 0,
       typical = 0, upperTypical = 0, lowerTypical = 0,
       reference = reference)
}
