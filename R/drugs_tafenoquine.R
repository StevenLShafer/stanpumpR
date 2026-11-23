# -----------------------------------------------------------------------------
# Tafenoquine (Krintafel, 150 mg tablets, radical cure of Plasmodium vivax;
# Arakoda, 100 mg tablets, malaria prophylaxis): oral, two compartments with
# a lagged first-order absorption, apparent (/F) parameters
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min; amounts in
# mg, so the plotted concentration is ng/mL (= mcg/L) of tafenoquine in
# plasma.
#
# Drafted by Claude Code, 2026-11-20, by the weekly library-expansion routine
# (docs/library-expansion/README.md; plan.csv, Antimalarials).  Every number
# was read on the PDF of the source.
#
# SOURCE: THAKKAR 2018
# ====================
# Thakkar N, Green JA, Koh GCKW, Duparc S, Tenero D, Goyal N.  Population
# pharmacokinetics of tafenoquine, a novel antimalarial.  Antimicrob Agents
# Chemother 2018;62(11):e00711-18.  doi 10.1128/AAC.00711-18 (PMID 30201820,
# PMC6201082).  5,286 plasma concentrations (LC-MS/MS, lower limit 2 ng/mL;
# 0.5 ng/mL in one study) from 675 subjects in five GSK studies, three in
# healthy volunteers (193) and two in patients with P. vivax malaria (482;
# DETECTIVE parts 1 and 2), single doses of 300-1200 mg as tablet or capsule;
# NONMEM 7.3, FOCE-I.  A sixth study (GATHER, 166 patients, tablets) was held
# out and the model predicted it (Fig. 2B).  Table 2: age 35.0 (15.0-79.0)
# years, weight 69.3 (37.2-138) kg, BMI 24.3 (15.5-47.0), 171 women (25.3%),
# medians (ranges).
#
# Table 3, final model (estimates; bootstrap medians beside them agree):
#
#     CL/F 2.96 L/h      V2/F 915 L      Q/F 5.09 L/h      V3/F 664 L
#     lag 0.908 h        ka 0.252 /h (tablet)
#     capsule: ka x 0.924, relative F 0.863 (tablet F1 = 1, the reference)
#     healthy volunteers / patients: V2/F x 1.35, V3/F x 0.347
#
# The reference is a PATIENT taking TABLETS: the abstract gives "2.96 liters/h
# for CL/F and 915 liters for V2/F in P. vivax-infected subjects", the table's
# footnote makes the tablet the reference, and both marketed products are
# tablets.  Weight acts on all four disposition parameters with the
# allometric exponents FIXED at 0.75 (CL/F, Q/F) and 1 (V2/F, V3/F) (Methods,
# "Covariate analysis").  The paper does not print the weight they are
# normalised to; this model takes the conventional 70 kg (the cohort's median
# is 69.3 kg, which would move clearances by 0.7% and volumes by 1.0%).  Age,
# sex and race were tested and had no relevant effect (Results).
#
# HEALTHY VOLUNTEERS ARE NOT APPLIED  [decision for Shafer, recorded in
# docs/library-expansion/plan.csv]
# The healthy volunteers' central volume was 1.35 times and their peripheral
# volume 0.347 times the patients'.  The authors offer two explanations:
# chloroquine, which the patients took alongside (and which raised the
# tafenoquine peak in an interaction study), and the dehydration and capillary
# leak of acute malaria.  stanpumpR has no field for either.  The curve is the
# PATIENT's, the population Krintafel is given to (with chloroquine, as in the
# source's patient studies).  An Arakoda traveller taking prophylaxis is a
# healthy volunteer: for 200 mg the healthy-volunteer volumes give a peak
# about a quarter lower (148 against 194 ng/mL), which is what Arakoda's
# label measured in healthy volunteers (147 ng/mL; CHECKS below).  Which state a prophylaxis curve should show is Shafer's to decide.
#
# CHECKS (not used in the fit; tests/testthat/test-drugs-tafenoquine.R)
# ====================================================================
# * Krintafel label (DailyMed set id 5cf989d5-36f5-4561-a30b-9fcb9deb6b6a,
#   section 12.3, read): apparent clearance "approximately 3 L/h" (this model
#   2.96 L/h at 70 kg), apparent volume "~1,600 L" (V2/F + V3/F = 1,579 L),
#   peak "12 to 15 hours" after a dose (this model 15.4 h after 300 mg, at
#   291 ng/mL), terminal half-life "approximately 15 days" (this model 17.2
#   days; Arakoda's label 16.5 days, range 10.8-27.3, in healthy
#   volunteers).  The distribution half-life is 1.95 days.
# * Arakoda label (set id 299e49d8-470f-4779-a010-4a1ee0e0c6cd, Table 5,
#   read): 200 mg in 65 healthy volunteers, fed, Cmax 147 ng/mL (CV 20.7%),
#   Tmax 14 h (median), AUCinf 70 h.mcg/mL.  This model gives an AUC of 67.6
#   h.mcg/mL at 70 kg (AUC = dose/CL whatever the volumes).  Its patient peak
#   is 194 ng/mL at 14.9 h; with the healthy volunteers' volumes, which the
#   curve does not use (see above), 148 ng/mL at 16.4 h, against the
#   measured 147 ng/mL at 14 h.
#
# FOOD
# ====
# Both labels say to take tafenoquine with food, which raised the AUC 41% and
# Cmax 31% for a capsule (labels, 12.3).  The source does not say how its
# studies were fed and fitted no food term, so the model is of the trials'
# own conditions, which the GATHER validation (tablets, the label's
# instructions) reproduced.
#
# DOSE
# ====
# "Each KRINTAFEL tablet contains 150 mg of tafenoquine (equivalent to 188.2
# mg tafenoquine succinate)" (label section 11), and Arakoda's 100 mg tablets
# are labelled as tafenoquine the same way.  The source dosed in mg of
# tafenoquine (300 mg = two tablets).  Doses are therefore entered as
# labelled, in mg of tafenoquine base, with no salt conversion (AGENTS.md,
# *Dose basis*).
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# The model carries its own size covariate (allometric weight on every
# volume and clearance), so it does not use pkSizeFactors()'s factors: with
# the switch on, weight is the pharmacokinetic weight (pkWeight); with it
# off, total body weight.  Studied weights 37.2-138 kg; age 15-79 years.
# Krintafel is approved from 16 years, Arakoda from 18.  The model is offered
# at every age, as the library's models are; below 15 years it is an
# extrapolation of the allometric weight terms.
#
# NOT MODELLED
# ============
# Whole blood concentrations, which the labels say are on average 67% higher
# than plasma.  The haemolytic risk in G6PD deficiency, which is the drug's
# defining hazard: a concentration curve says nothing about it.  No
# therapeutic range is established (inst/extdata/therapeuticRanges.csv).
# -----------------------------------------------------------------------------

TAFENOQUINE_CL      <- 2.96    # L/h, CL/F at 70 kg (patients, tablet)
TAFENOQUINE_V2      <- 915     # L, central V2/F at 70 kg (patients)
TAFENOQUINE_Q       <- 5.09    # L/h, Q/F at 70 kg
TAFENOQUINE_V3      <- 664     # L, peripheral V3/F at 70 kg (patients)
TAFENOQUINE_KA      <- 0.252   # /h, tablet
TAFENOQUINE_TLAG    <- 0.908   # h
TAFENOQUINE_WT_REF  <- 70      # kg, the allometric reference (not printed)
TAFENOQUINE_CL_EXP  <- 0.75    # fixed
TAFENOQUINE_V_EXP   <- 1       # fixed
# Recorded, not applied (healthy volunteers / patients; see the header)
TAFENOQUINE_HEALTHY_V2 <- 1.35
TAFENOQUINE_HEALTHY_V3 <- 0.347

#' Tafenoquine (Thakkar 2018)
#'
#' Two compartments with a lagged first-order absorption and allometric
#' weight on every volume and clearance, fitted to healthy volunteers and
#' patients with P. vivax malaria; the patients' volumes.  Plasma
#' tafenoquine.  See the header of \code{R/drugs_tafenoquine.R}.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
tafenoquine <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)
  w <- if (isTRUE(adjustToFFM)) size$pkWeight else weight
  wcl <- (w / TAFENOQUINE_WT_REF)^TAFENOQUINE_CL_EXP
  wv  <- (w / TAFENOQUINE_WT_REF)^TAFENOQUINE_V_EXP

  default <- list(
    v1  = TAFENOQUINE_V2 * wv,
    v2  = TAFENOQUINE_V3 * wv,
    v3  = 1,
    cl1 = TAFENOQUINE_CL * wcl / 60,
    cl2 = TAFENOQUINE_Q * wcl / 60,
    cl3 = 0,
    ka_PO = TAFENOQUINE_KA / 60,
    bioavailability_PO = 1,     # apparent parameters; tablet F1 = 1
    tlag_PO = TAFENOQUINE_TLAG * 60
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
    reference = paste0(
      "Thakkar N, Green JA, Koh GCKW, Duparc S, Tenero D, Goyal N, Antimicrob Agents ",
      "Chemother 2018;62:e00711-18, Table 3 (675 healthy volunteers and patients with ",
      "P. vivax malaria): two compartments, CL/F 2.96 L/h and Q/F 5.09 L/h x (WT/70)^0.75, ",
      "V2/F 915 L and V3/F 664 L x WT/70 (patients), ka 0.252 /h (tablet), lag 0.908 h; ",
      "the healthy volunteers' volumes (V2/F x 1.35, V3/F x 0.347) not applied; doses ",
      "in mg of tafenoquine as labelled. https://doi.org/10.1128/AAC.00711-18")
  )
}
