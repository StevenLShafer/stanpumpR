# -----------------------------------------------------------------------------
# Ceftazidime, given as Avycaz (ceftazidime/avibactam; Franzese 2022): two
# compartments, the combined adult and pediatric model, with a normalised
# creatinine clearance (or, to two years, renal maturation by postmenstrual
# age), an Emax function of weight on clearance, weight on the volumes and Q,
# and the patient's infection
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min; amounts in
# mg, so the plotted concentration is mcg/mL (= mg/L), total plasma
# ceftazidime.
#
# Drafted by Claude Code, 2026-11-25, at the request of Steven L. Shafer
# ("Let's also move on with antibiotics", 2026-11-25), from the library
# expansion plan (docs/library-expansion/plan.csv, row Avycaz).  Every number
# below was read on the PDFs Shafer supplied on 2026-11-25: the paper, its
# supplement (the NONMEM control stream of the final ceftazidime model and
# Table S2), the 2024 corrigendum and the renal-dose paper.
#
# A COMPONENT OF AVYCAZ
# =====================
# Steven L. Shafer, 2026-11-18: the product is named Avycaz ("Use Zerbaxa and
# Avycaz too"), dosed in its label's units, and its curve is ceftazidime;
# avibactam, a beta-lactamase inhibitor, is not drawn (2026-11-24,
# docs/library-expansion/README.md, "Which moieties a combination product
# draws").  This model has no dose units of its own and no Category: an
# Avycaz dose reaches it as 0.8 of the product's weight, in the same unit
# (R/combination-products.R, COMBINATION_PRODUCTS$Avycaz).  The label's 2.5 g
# is "ceftazidime 2 grams and avibactam 0.5 grams"; its pediatric 62.5 mg/kg
# is "ceftazidime 50 mg/kg and avibactam 12.5 mg/kg".  The label's grams of
# ceftazidime are of the base (each vial "contains ceftazidime 2 grams
# (equivalent to 2.601 grams sterile ceftazidime pentahydrate/sodium
# carbonate)", label sec. 11), as the source's doses are, so no salt
# conversion is made.  Plain ceftazidime (Fortaz, Tazicef) is not
# offered: the plan row asks whether it is wanted once Avycaz is built.
#
# THE SOURCE
# ==========
# Franzese RC, McFadyen L, Watson KJ, Riccobene T, Carrothers TJ, Vourvahis
# M, Chan PLS, Raber S, Bradley JS, Lovern M.  Population pharmacokinetic
# modeling and probability of pharmacodynamic target attainment for
# ceftazidime-avibactam in pediatric patients aged 3 months and older.  Clin
# Pharmacol Ther 2022;111:635-645 (doi 10.1002/cpt.2460; PMID 34687548),
# with its Supplementary Methods (the control stream "Ceftazidime final
# model: NONMEM code") and Table S2.  Adult data from 18 phase I-III studies
# and 154 children 3 months to 17 years (Table 1) in one fit: 9,628
# ceftazidime observations from 2,130 subjects.  Two compartments, first-
# order elimination (ADVAN3 TRANS4).  Table S2 and the control stream:
#
#     CL = 7.75 L/h x f(WT) x CLCR x cIAI 1.33 x HAP/VAP 1.10
#     f(WT) = (WT/70) x (WT50 + 70)/(WT50 + WT),  WT50 53.5 kg
#     CLCR (age > 2 years) = 0.0103036 x NCrCL                 NCrCL < 100
#                          = 0.0103036 x 100 + 0.00125182 x (NCrCL - 100)
#     CLCR (age <= 2 years) = PMA^3.4 / (47.7^3.4 + PMA^3.4)
#     NCrCL in mL/min/1.73 m2, held at 150
#     Vc = 11.2 L x (WT/70) x cUTI 1.49 x cIAI or HAP/VAP 1.83
#          x ventilated (1 + 0.202)
#     Q  = 5.33 L/h x (WT/70)^0.67
#     Vp = 6.52 L x (WT/70)
#
# The control stream's $THETA gives 9.1259 for theta(1), CL, an initial
# value; Table S2's estimate is 7.75 and is used.  Its other initial values
# round to Table S2's.  The reference (POP 1) is the healthy participant.
# Race (non-Chinese, non-Japanese Asian CL x 0.864, Chinese CL x 0.916, and
# Asian Vc x 0.865) is not applied: the Patient Profile has no race field.
#
# NCrCL is "body surface area-normalized CrCL": in children over two years
# the updated bedside Schwartz equation, and in adults Cockcroft-Gault
# "adjusted for body surface area ... with units of mL/min/1.73 m2" (Methods).
# Neither paper names the BSA formula; Mosteller's is used, as
# colistimethate does.  The renal-dose paper (Franzese RC et al., Clin
# Pharmacol Ther 2023;113:182-195) reuses the same model, prints the same
# piecewise relation, and says the relation is flat above 150.
#
# WHY THIS SOURCE
# ===============
# The joint fit of the approved product in adults and children together, the
# model the label's pediatric dosing came from, with every indication the
# product is given for.  The adult-only fits it updated (Li J et al., 2019)
# are its predecessors.
#
# THE INFECTION (R/infection.R)
# =============================
# The control stream's POP codes the indication: 2 cUTI, 3 cIAI, 4 HAP/VAP;
# POP5 is "presence of ventilator", fitted in the phase III HAP/VAP trial.
# The Patient Profile's categories map onto them as follows [decision for
# Shafer, put to him 2026-11-25; docs/library-expansion/plan.csv, row
# Avycaz]:
#     none                 the healthy reference
#     cUTI                 Vc x 1.49
#     cIAI                 CL x 1.33, Vc x 1.83
#     pneumonia            CL x 1.10, Vc x 1.83 (HAP/VAP not ventilated)
#     pneumoniaVentilated  CL x 1.10, Vc x 1.83 x 1.202
#     other                the healthy reference: the source fitted no
#                          other infection
# The default, "none", gives the healthy participant's curve (Shafer,
# 2026-11-24); an infected patient's central volume is 1.49 to 2.20 times
# larger, so with the default the plot overstates the peak an infected
# patient reaches.  The help page says so.
#
# RENAL FUNCTION
# ==============
# From 18 years, Cockcroft-Gault on the patient's creatinine
# (adultEquivalentCreatinine(), a blank one being the assumed normal) times
# 1.73 / BSA (Mosteller).  From two years to 18, the bedside Schwartz
# equation, 0.413 x height / SCr, on the child's own creatinine
# (patientCreatinine(); blank, the assumed normal for age and sex), which is
# what the source applied to its children.  At two years and below, renal
# function is the maturation function alone, at the postmenstrual age (the
# Patient Profile's gestational age at birth, term when blank, plus the
# postnatal age; postmenstrualAge()), with no creatinine term, as fitted.
# The source studied children from three months; the maturation function is
# offered below that, an extrapolation the help page names.  The steps at two
# and at 18 years are the source's own.  At two a 12-kg child's clearance
# rises about 10% (the maturation function reaches 0.98, the Schwartz term
# about 1.04 with the assumed creatinine).  At 18 the change of equation moves
# a 60-kg, 170-cm young man from 6.6 to 7.8 L/h with the assumed creatinines,
# because the bedside Schwartz equation reads a lower filtration than
# Cockcroft-Gault normalised to 1.73 m2; the source fitted each equation in
# its own population, so the step is kept.  The source's renal term goes to zero with the
# creatinine clearance (its "non-renal ceftazidime CL ... was close to
# zero", renal-dose paper); a patient on dialysis is not described.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# The source carries its own weight term on every parameter (the Emax
# function on CL, linear on Vc and Vp, 0.67 on Q), so with the fat-free-mass
# switch on those terms read the pharmacokinetic weight, as do the Cockcroft-
# Gault weight and the BSA it is normalised by, and they take nothing more;
# with the switch off they read total body weight.
#
# PREGNANCY
# =========
# See the registry row.  Ceftazidime is cleared almost wholly by filtration
# (label sec. 12.3: "eliminated almost solely by the kidneys"), so the
# creatinine clearance takes the 50% rise in filtration of late pregnancy
# when the creatinine is blank (pregnancyGfrFactor()).
#
# CHECKS (not used in the fit)
# ============================
# The Avycaz label, sec. 12.3, Table 14, healthy adult men on 2.5 g every 8 h
# as 2-h infusions: ceftazidime CL 6.86 L/h (15% CV) and Vss 17 L at steady
# state, Cmax 90.4 mg/L.  The model's healthy reference man gives CL 8.06
# L/h, 17% higher, and Vss 17.7 L.  The corrected Table 2 (corrigendum, Clin
# Pharmacol Ther 2024;115:373) gives the geometric mean adult phase III
# ceftazidime exposures; tests/testthat/test-drugs-ceftazidime.R compares
# both.
#
# THE MIC LINE AND THE BAND
# =========================
# Free drug, read from R/antibioticThresholds.R and R/antibioticMbc.R.  The
# MIC line is free ceftazidime at 8 mg/L, the ceftazidime-avibactam
# susceptible breakpoint for Enterobacterales and Pseudomonas aeruginosa
# (avibactam fixed at 4 mg/L) and the source's own target, 50% fT > 8 mg/L.
# No band is drawn: no MBC50 or MBC90 of ceftazidime with avibactam has been
# found (antibioticNoBands()).
#
# References:
# Franzese RC et al., Clin Pharmacol Ther 2022;111:635-645, and its
#   Supplementary Materials.  https://doi.org/10.1002/cpt.2460
# Correction, Clin Pharmacol Ther 2024;115:373.
#   https://doi.org/10.1002/cpt.3143
# Franzese RC et al., Clin Pharmacol Ther 2023;113:182-195 (renal dose
#   adjustments).  https://doi.org/10.1002/cpt.2764
# Avycaz (ceftazidime and avibactam) label, NDA 206494, Allergan, DailyMed
#   2026-08-11 (papers/labels/NJ_label-avycaz_206494_2026-08-11.pdf).
# Ansari J, Carvalho B, Shafer SL, Flood P.  Anesth Analg 2016;122:786-804.
#   https://doi.org/10.1213/ANE.0000000000001143 (pregnancy)
# -----------------------------------------------------------------------------

# Franzese 2022, Table S2 and the control stream (final ceftazidime model)
CEFTAZIDIME_CL          <- 7.75       # L/h, theta(1)
CEFTAZIDIME_VC          <- 11.2       # L at 70 kg, theta(2)
CEFTAZIDIME_Q           <- 5.33       # L/h at 70 kg, theta(3)
CEFTAZIDIME_VP          <- 6.52       # L at 70 kg, theta(4)
CEFTAZIDIME_Q_WT_EXP    <- 0.67       # theta(12), fixed
CEFTAZIDIME_WT50        <- 53.5       # kg, theta(15)
CEFTAZIDIME_WT_REF      <- 70         # kg
CEFTAZIDIME_HILL        <- 3.4        # theta(6), fixed (Rhodin 2009)
CEFTAZIDIME_TM50        <- 47.7       # weeks, theta(7), fixed
CEFTAZIDIME_MATURATION_AGE <- 2       # years; the maturation function at and below
CEFTAZIDIME_ADULT_AGE   <- 18         # years; Cockcroft-Gault from here, Schwartz below
CEFTAZIDIME_SLOPE1      <- 0.0103036  # per mL/min/1.73 m2, NCrCL < 100
CEFTAZIDIME_SLOPE2      <- 0.00125182 # per mL/min/1.73 m2, NCrCL >= 100
CEFTAZIDIME_INFLECT     <- 100        # mL/min/1.73 m2
CEFTAZIDIME_NCRCL_MAX   <- 150        # mL/min/1.73 m2, the control stream's cap
# Infection factors (Table S2), by the Patient Profile's categories (see the
# header's THE INFECTION)
CEFTAZIDIME_VENTILATED  <- 1 + 0.202  # theta(22)
CEFTAZIDIME_INFECTION_CL <- c(none = 1, cUTI = 1, cIAI = 1.33,
                              pneumonia = 1.10, pneumoniaVentilated = 1.10, other = 1)
CEFTAZIDIME_INFECTION_VC <- c(none = 1, cUTI = 1.49, cIAI = 1.83,
                              pneumonia = 1.83,
                              pneumoniaVentilated = 1.83 * CEFTAZIDIME_VENTILATED,
                              other = 1)

#' Ceftazidime's renal term in Franzese 2022's model
#'
#' @param ncrcl body-surface-area-normalised creatinine clearance, mL/min/1.73 m2
#' @returns the multiplier of the typical clearance, 1.03 at 100 mL/min/1.73 m2
#' @keywords internal
ceftazidimeRenalTerm <- function(ncrcl)
{
  ncrcl <- min(ncrcl, CEFTAZIDIME_NCRCL_MAX)
  if (ncrcl < CEFTAZIDIME_INFLECT)
    CEFTAZIDIME_SLOPE1 * ncrcl
  else
    CEFTAZIDIME_SLOPE1 * CEFTAZIDIME_INFLECT +
      CEFTAZIDIME_SLOPE2 * (ncrcl - CEFTAZIDIME_INFLECT)
}

#' Ceftazidime (Franzese 2022), a component of Avycaz
#'
#' Two compartments from the combined adult and pediatric model: clearance
#' from an Emax function of weight, the normalised creatinine clearance (or
#' renal maturation at two years and below) and the infection; the volumes
#' and Q from weight; the central volume from the infection.  A component of
#' Avycaz: see R/combination-products.R and the header of
#' R/drugs_ceftazidime.R.
#'
#' @inheritParams cefazolin
#' @param creatinine serum creatinine in mg/dL, or NULL for the assumed normal
#'   value for age and sex
#' @param gestationalAge gestational age at birth in weeks, or NULL for a term
#'   birth; read at two years of age and below
#' @param infection the Patient Profile's infection, one of
#'   \code{INFECTION_VALUES}
#' @param pregnant is the patient pregnant?  \code{TRUE} raises the
#'   creatinine clearance by \code{PREGNANCY_GFR_FACTOR} when the creatinine
#'   is blank.
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
ceftazidime <- function(weight, height, age, sex, adjustToFFM = TRUE,
                        creatinine = NULL, gestationalAge = NULL,
                        infection = INFECTION_DEFAULT, pregnant = FALSE)
{
  infection <- checkInfection(infection)
  # Size (see the header's BODY SIZE): every weight term of the source, the
  # Cockcroft-Gault weight and its BSA read pkW.
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume = 1, legacyClearance = 1)
  pkW  <- if (isTRUE(adjustToFFM)) size$pkWeight else weight
  gfrPregnancy <- pregnancyGfrFactor(pregnant, creatinine)

  if (age <= CEFTAZIDIME_MATURATION_AGE) {
    pma <- postmenstrualAge(age, gestationalAge)
    renal <- pma^CEFTAZIDIME_HILL / (CEFTAZIDIME_TM50^CEFTAZIDIME_HILL + pma^CEFTAZIDIME_HILL)
    renalNote <- sprintf("renal maturation %.3f at postmenstrual age %.0f weeks", renal, pma)
  } else {
    if (age < CEFTAZIDIME_ADULT_AGE) {
      scr <- patientCreatinine(creatinine, age, sex)
      ncrcl <- egfrBedsideSchwartz(height, scr)
      method <- "bedside Schwartz"
    } else {
      scr <- adultEquivalentCreatinine(creatinine, age, sex)
      ncrcl <- creatinineClearanceCG(pkW, age, sex, scr) * 1.73 / bsaMosteller(pkW, height)
      method <- "Cockcroft-Gault x 1.73/BSA"
    }
    ncrcl <- ncrcl * gfrPregnancy
    renal <- ceftazidimeRenalTerm(ncrcl)
    renalNote <- paste0(
      method, " NCrCL ", sprintf("%.0f", ncrcl), " mL/min/1.73 m2 (",
      if (is.null(creatinine)) paste0("assumed normal serum creatinine ", format(signif(scr, 3)))
      else paste0("serum creatinine ", format(creatinine)),
      " mg/dL)")
  }

  wtTerm <- (pkW / CEFTAZIDIME_WT_REF) *
    (CEFTAZIDIME_WT50 + CEFTAZIDIME_WT_REF) / (CEFTAZIDIME_WT50 + pkW)
  cl <- CEFTAZIDIME_CL * wtTerm * renal * CEFTAZIDIME_INFECTION_CL[[infection]]  # L/h
  vc <- CEFTAZIDIME_VC * (pkW / CEFTAZIDIME_WT_REF) * CEFTAZIDIME_INFECTION_VC[[infection]]
  vp <- CEFTAZIDIME_VP * (pkW / CEFTAZIDIME_WT_REF)
  q  <- CEFTAZIDIME_Q * (pkW / CEFTAZIDIME_WT_REF)^CEFTAZIDIME_Q_WT_EXP

  default <- list(
    v1  = vc,
    v2  = vp,
    v3  = 1,
    cl1 = cl / 60,
    cl2 = q / 60,
    cl3 = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Franzese RC, McFadyen L, Watson KJ, et al., Clin Pharmacol Ther 2022;111:635-645, ",
    "Table S2 and the Supplementary Methods (adults and children): two compartments, ",
    "CL = 7.75 L/h x (WT/70)(53.5 + 70)/(53.5 + WT) x renal term, Vc 11.2 L x WT/70, ",
    "Q 5.33 L/h x (WT/70)^0.67, Vp 6.52 L x WT/70; ", renalNote, "; infection: ",
    infectionLabel(infection),
    if (infection != INFECTION_NONE)
      sprintf(" (CL x %s, Vc x %s)", format(CEFTAZIDIME_INFECTION_CL[[infection]]),
              format(signif(CEFTAZIDIME_INFECTION_VC[[infection]], 4))) else "",
    "; race not applied. Drawn for Avycaz (2.5 g = 2 g ",
    "ceftazidime + 0.5 g avibactam); avibactam not drawn. ",
    "https://doi.org/10.1002/cpt.2460")
  if (isTRUE(pregnant) && gfrPregnancy != 1 && age > CEFTAZIDIME_MATURATION_AGE)
    reference <- paste0(reference, "; ", pregnancyReference(
      paste0("creatinine clearance x ", PREGNANCY_GFR_FACTOR,
             " (physiological rise in filtration)")))

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
