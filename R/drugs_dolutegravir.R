# -----------------------------------------------------------------------------
# Dolutegravir (Tivicay, 10, 25 and 50 mg film-coated tablets; an HIV-1
# integrase strand transfer inhibitor): oral, one compartment with a lagged
# first-order absorption, apparent (/F) parameters
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min; amounts in
# mg, so the plotted concentration is mcg/mL (= mg/L) of total dolutegravir in
# plasma.
#
# Drafted by Claude Code, 2026-11-27, by the weekly library-expansion routine
# (docs/library-expansion/README.md; plan.csv, Antivirals).  Every number was
# read on the PDF of the source (Table 3 and Table 4 on the page images).
# nlmixr2lib's entry Zhang_2015_dolutegravir led to the paper; none of its
# code was used (docs/nlmixr2lib/README.md).
#
# SOURCE: ZHANG 2015
# ==================
# Zhang J, Hayes S, Sadler BM, Minto I, Brandt J, Piscitelli S, Min S, Song
# IH.  Population pharmacokinetics of dolutegravir in HIV-infected
# treatment-naive patients.  Br J Clin Pharmacol 2015;80(3):502-514.
# doi 10.1111/bcp.12639 (PMID 25819132).  3,357 plasma concentrations
# (LC-MS/MS, lower limit 5 or 20 ng/mL) from 563 adults with HIV-1 who had
# never been treated, in three ViiV studies: ING111521 (10 days of
# monotherapy, 10 or 50 mg once daily, n = 19), SPRING-1 (phase 2b, 10, 25 or
# 50 mg once daily with two nucleosides, n = 141) and SPRING-2 (phase 3, 50
# mg once daily, n = 403).  NONMEM VII, FOCE-I.  Table 2, all studies, medians
# (ranges): age 37 (18-68) years, weight 74.5 (39.0-135) kg, BMI 24.2
# (14.7-47.9), 481 men and 82 women, 236 current smokers (42%).
#
# Table 3, final model, with its footnotes' equations:
#
#     CL/F = 0.901 L/h x 1.16^SMOK x 1.35^POC x (WT/70)^0.438
#                      x (AGE/40)^0.193 x (BILI/9)^-0.211
#     V/F  = 17.4 L x (WT/70)^0.768
#     F    = 1.21^GEND x 1.24^DOSE
#     ka   = 2.24 /h        lag = 0.263 h
#
# SMOK = 1 for a current smoker; POC = 1 for study ING111521; BILI is total
# bilirubin in umol/L; GEND = 1 for women; DOSE = 1 for the 10 mg dose.  The
# reference is a 40-year-old, 70 kg man who does not smoke, with a bilirubin
# of 9 umol/L, given more than 10 mg.
#
# WHAT IS APPLIED
# ===============
# * Weight, on CL/F and V/F, as fitted (see BODY SIZE).
# * Age on CL/F, as fitted, the age held at 18 years below it (AGENTS.md,
#   *Age terms*: the source studied 18 to 68 years).  The authors themselves
#   doubt this term: 17 of the 563 were over 55, one over 65, and a clearance
#   that RISES with age was unexpected (Discussion).  It is small: 0.86 at 18
#   years, 1.08 at 60, 1.17 at 90, which is an extrapolation.
# * Sex.  Women's bioavailability was 1.21 times men's.  The parameters being
#   apparent, that is entered as CL/F and V/F each divided by 1.21 for a
#   woman, and the bioavailability left at 1.
#
# WHAT IS HELD AT THE REFERENCE
# =============================
# * Smoking (CL/F x 1.16): the Patient Profile has no smoking field, so the
#   curve is a non-smoker's.  A smoker's exposure is 14% lower.  42% of the
#   source's patients smoked, which is why the cohort's mean exposure (Table
#   4) is a little below this model's reference patient's (CHECKS).
# * Bilirubin: 9 umol/L, the cohort's median.  Over the studied 3 to 38
#   umol/L the clearance runs from 26% higher to 26% lower.
# * Study ING111521's 35% higher clearance, which the authors cannot explain
#   (19 patients, monotherapy): not applied.
# * The 10 mg dose's 24% higher bioavailability, which the authors put down
#   to the smaller tablet dispersing better: not applied.  The adult dose is
#   one 50 mg tablet; 10 mg tablets are used for children.  A dose entered as
#   10 mg is therefore drawn about a fifth low against this source.
#
# CHECKS (tests/testthat/test-drugs-dolutegravir.R)
# =================================================
# * Table 4, SPRING-2, 50 mg once daily at steady state (n = 403, geometric
#   means of the individual estimates): AUC(0-tau) 53.6 mcg.h/mL, Cmax 3.69
#   mcg/mL, Ctau 1.10 mcg/mL, tmax 2.0 h.  This model's reference man, 70 kg,
#   40 years, gives AUC 55.5 (= 50/0.901), Cmax 3.72 mcg/mL at 1.8 h after
#   the dose and Ctau 1.21 mcg/mL: 4%, 1% and 10% above the cohort's means.
#   The cohort's smokers (42%, CL/F x 1.16) lower its means and its women
#   (15%, F x 1.21) raise them.
# * Tivicay label (DailyMed set id 63df5af3-b8ac-4e76-9830-2dbb340af922,
#   revised 8/2026, section 12.3, read): Table 9 repeats the SPRING values
#   (53.6, 3.67, 1.11); "apparent volume of distribution ... 17.4 L" and
#   "apparent clearance (CL/F) of 1.0 L/h" from the population analyses;
#   "terminal half-life of approximately 14 hours" (this model 13.4 h at the
#   reference); peak "1 to 3 hours postdose".  These are not independent of
#   the source: the label's values are this analysis's and its successors'.
# * CHILDREN, where the model is an extrapolation and reads HIGH: the label's
#   Table 10 gives children of 20 kg or more on 50 mg once daily (n = 49) an
#   AUC of 55.0 mcg.h/mL, the adults' value.  This model gives a 30 kg child
#   an AUC of about 94 mcg.h/mL with the switch off (weight^0.438 on CL/F,
#   and the age term held at 18 years' 0.86), so it over-predicts a child's
#   exposure by a half or more.  The source studied no one under 18.  A
#   paediatric model would be a second candidate for the model selector
#   (R/modelSelection.R), not a change to this one.
#
# FOOD
# ====
# The label: food raises the AUC by 33% to 66% and the peak by 46% to 67%
# with the fat in the meal, and delays the peak from 2 h to 3-5 h; Tivicay
# "may be taken with or without food".  The source fitted no food term and
# its phase 2b and 3 patients took the drug as outpatients, so the curve is
# of the trials' own mixed conditions.
#
# DOSE
# ====
# "Each tablet contains 50 mg of dolutegravir (as dolutegravir sodium)"; 52.6
# mg of the sodium salt is "equivalent to ... 50 mg dolutegravir free acid"
# (label, sections 3 and 11).  The source dosed in mg of dolutegravir.  Doses
# are entered as labelled, in mg of dolutegravir, with no salt conversion
# (AGENTS.md, *Dose basis*).  TIVICAY PD, the 5 mg tablets for oral
# suspension given to small children, has about 1.6 times the tablets'
# bioavailability (label) and is NOT this model's product.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# The model carries its own size covariate, estimated, on both of its
# disposition parameters, so it does not use pkSizeFactors()'s factors: with
# the switch on, weight is the pharmacokinetic weight (pkWeight); with it
# off, total body weight.  Studied weights 39-135 kg.  The absorption rate
# and lag take no size term.
#
# PREGNANCY
# =========
# Recorded, not applied (data-raw/pregnancyRegistry.R): two paired studies
# of total dolutegravir disagree on the size of the fall in late pregnancy,
# and unbound concentrations in the third trimester were like those
# postpartum.
#
# NOT MODELLED
# ============
# Unbound dolutegravir (it is at least 98.9% bound); UGT1A1 genotype (poor
# metabolisers' clearance 32% lower, label; no Patient Profile field);
# interactions (inducers such as rifampin and efavirenz, for which the label
# doubles the dose to 50 mg twice daily; cation-containing antacids).  No
# effect site, no band: no therapeutic range is established
# (inst/extdata/therapeuticRanges.csv).
# -----------------------------------------------------------------------------

DOLUTEGRAVIR_CL        <- 0.901   # L/h, CL/F at 70 kg, 40 years, a man
DOLUTEGRAVIR_V         <- 17.4    # L, V/F at 70 kg, a man
DOLUTEGRAVIR_KA        <- 2.24    # /h
DOLUTEGRAVIR_TLAG      <- 0.263   # h
DOLUTEGRAVIR_WT_REF    <- 70      # kg
DOLUTEGRAVIR_CL_WT_EXP <- 0.438
DOLUTEGRAVIR_V_WT_EXP  <- 0.768
DOLUTEGRAVIR_AGE_REF   <- 40      # years
DOLUTEGRAVIR_CL_AGE_EXP <- 0.193
DOLUTEGRAVIR_F_FEMALE  <- 1.21    # women's bioavailability over men's
# Recorded, not applied (see the header)
DOLUTEGRAVIR_CL_SMOKER    <- 1.16
DOLUTEGRAVIR_CL_ING111521 <- 1.35
DOLUTEGRAVIR_CL_BILI_EXP  <- -0.211   # (bilirubin / 9 umol/L)^-0.211
DOLUTEGRAVIR_F_10MG       <- 1.24

#' Dolutegravir (Zhang 2015)
#'
#' One compartment with a lagged first-order absorption, apparent oral
#' parameters, fitted to 563 previously untreated adults with HIV-1; weight
#' and age on clearance, weight on volume, sex on bioavailability.  Total
#' plasma dolutegravir.  See the header of \code{R/drugs_dolutegravir.R}.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
dolutegravir <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)
  w <- if (isTRUE(adjustToFFM)) size$pkWeight else weight
  ageTerm <- (max(age, ADULT_AGE_TERM_FLOOR) / DOLUTEGRAVIR_AGE_REF)^DOLUTEGRAVIR_CL_AGE_EXP
  # Apparent parameters: a bioavailability 1.21 times the reference's divides
  # both CL/F and V/F
  f <- if (sex == "female") DOLUTEGRAVIR_F_FEMALE else 1

  default <- list(
    v1  = DOLUTEGRAVIR_V * (w / DOLUTEGRAVIR_WT_REF)^DOLUTEGRAVIR_V_WT_EXP / f,
    v2  = 1,
    v3  = 1,
    cl1 = DOLUTEGRAVIR_CL * (w / DOLUTEGRAVIR_WT_REF)^DOLUTEGRAVIR_CL_WT_EXP * ageTerm / f / 60,
    cl2 = 0,
    cl3 = 0,
    ka_PO = DOLUTEGRAVIR_KA / 60,
    bioavailability_PO = 1,     # apparent parameters
    tlag_PO = DOLUTEGRAVIR_TLAG * 60
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
      "Zhang J, Hayes S, Sadler BM, Minto I, Brandt J, Piscitelli S, Min S, Song IH, ",
      "Br J Clin Pharmacol 2015;80:502-514, Table 3 (563 previously untreated adults ",
      "with HIV-1): one compartment, CL/F 0.901 L/h x (WT/70)^0.438 x (age/40)^0.193 ",
      "(age held at 18 years below it), V/F 17.4 L x (WT/70)^0.768, ka 2.24 /h, lag ",
      "0.263 h; women's bioavailability x 1.21 (CL/F and V/F divided by it); a ",
      "non-smoker (smokers' CL/F x 1.16 not applied) with a bilirubin of 9 umol/L; ",
      "doses in mg of dolutegravir as labelled. https://doi.org/10.1111/bcp.12639")
  )
}
