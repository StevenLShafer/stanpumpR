# -----------------------------------------------------------------------------
# Caspofungin (Cancidas): intravenous, two compartments, scaled by body-surface
# area (Xu 2026, critically ill children)
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min, amounts in mg
# of caspofungin, concentrations in mcg/mL (= mg/L) of total caspofungin in
# plasma.  Concentration only.  Drafted by Claude Code, 2026-11-28, on Steven L.
# Shafer's decision of that day ("For Caspofungin, switch to Xu 2026"), which
# settles the weekly run's [decision] of 2026-11-19 (Yang 2022's step
# covariates, Xu 2026, or a classic source); docs/library-expansion/plan.csv,
# Antifungals.
#
# SOURCE
# ======
# Xu N, Shi Y, Ju G, Liu X, Yan G, Zheng Y, Hou S, Xiang X, Lu G, Ouyang D, Zhu
# X, Wang Y.  Population pharmacokinetics of caspofungin in critically ill
# Chinese children: a prospective observational study.  Antimicrob Agents
# Chemother 2026;70(2):e01277-25 (PMID 41468548; card
# papers/digests/PMID_41468548.md; read on the PDF, Table 2 on the page).  29
# children in a paediatric ICU, 0.33-16 years, 4.9-74 kg, 0.29-1.89 m2 (Table
# 1), four on ECMO, given 70 mg/m2 then 50 mg/m2 (at most 70 mg) daily as a
# 1-hour infusion; 138 samples at steady state, total caspofungin by LC-MS/MS.
#
# Table 2 (final model), at a body-surface area of 0.79 m2, without ECMO:
#
#     CL = 0.196 L/h  x (BSA/0.79)^0.66   (exponent fixed)
#     V1 = 2.22 L     x  BSA/0.79         (exponent fixed)
#     Q  = 1.01 L/h
#     V2 = 1.63 L
#
# The paper fixes the exponents for "clearance" and "volume of distribution"
# and does not say whether Q and V2 take them; stanpumpR scales Q as a
# clearance and V2 as a volume, as allometric scaling does (an assumption,
# said on the help page).  Body-surface area is Mosteller's, as the paper's.
#
# POPULATION.  Critically ill children.  It is the only caspofungin model the
# library holds, so it is used for every patient by the nearest-population
# rule, adults included; the label's adults (Table 8) are the check on that
# extrapolation.  Yang 2022 (step covariates at 70 kg and 35 g/L albumin) was
# set aside on Shafer's decision.
#
# NOT APPLIED.  ECMO on V1 (18.2, a form the paper does not state; four
# patients; the card's [check]): the Patient Profile has no ECMO field, so the
# curve is a child's without ECMO.
#
# BODY SIZE.  The model carries its own body-surface-area covariate on every
# disposition parameter, from total weight and height whichever way the switch
# is set, as temozolomidePanetta's (the covariate was fitted against the
# children's actual size).  pkSizeFactors() is not called.
#
# DOSE BASIS.  Cancidas is labelled in mg of caspofungin (a 50 mg vial holds
# caspofungin acetate equivalent to 50 mg of caspofungin); the paper's doses
# are the same, so doses are entered as labelled.
#
# CHECKS.  Xu's own Bayesian estimates (non-ECMO median AUC 193.6 mg h/L on
# 50-70 mg/m2) are what the model gives: 204 mg h/L at 50 mg/m2 in a child of
# 0.82 m2.  The CANCIDAS label (Table 8, not fitted here; harmonic-mean
# clearances, 50 mg/m2 or 50 mg daily at steady state, 1-hour infusions) is
# NOT matched: clearance is about half the label's and exposure 1.3 to 2
# times it (the test pins these):
#
#                         label AUC24  model AUC24   label CL   model CL
#     12-17 y (1.49 m2)     124.9        250          12.6       5.0 mL/min
#     2-11 y (0.82 m2)      120.0        204           6.4       3.4
#     3-23 mo (0.46 m2)     131.2        167           3.2       2.3
#     adult 50 mg (70 kg)    87.3        147          10.6       5.7
#
# The label's children were neutropenic or had candidiasis but were not, as
# Xu's were, in intensive care; Xu itself finds its BSA-normalised clearance
# above three earlier paediatric models (0.248 against a median 0.208 L/h/m2),
# so the gap is to the label's studies, not to the other population models.
# An adult is an extrapolation of a paediatric ICU model.  Whether adults
# should keep it is a [decision] on the card (papers/digests/PMID_41468548.md).
#
# References
# ----------
# Xu N et al., Antimicrob Agents Chemother 2026;70:e01277-25.
#   https://doi.org/10.1128/aac.01277-25
# CANCIDAS (caspofungin acetate) for injection, prescribing information, Merck,
#   NDA 021227, revised 10/2024, DailyMed version of 2026-10-14 (setid 3bad23a6-09a6-4194-9182-093ed61bc71c).
# -----------------------------------------------------------------------------
CASPOFUNGIN_BSA_REF <- 0.79    # m2
CASPOFUNGIN_CL  <- 0.196       # L/h
CASPOFUNGIN_V1  <- 2.22        # L
CASPOFUNGIN_Q   <- 1.01        # L/h
CASPOFUNGIN_V2  <- 1.63        # L
CASPOFUNGIN_CL_EXP <- 0.66
CASPOFUNGIN_V_EXP  <- 1

#' Caspofungin pharmacokinetics (intravenous)
#'
#' Two compartments scaled by body-surface area (Xu 2026, critically ill
#' children); see the header of the file.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
caspofungin <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  # Body-surface area from TOTAL weight whichever way the switch is set: the
  # covariate was fitted against the children's actual size.
  b <- bsaMosteller(weight, height) / CASPOFUNGIN_BSA_REF
  cl <- b^CASPOFUNGIN_CL_EXP
  v  <- b^CASPOFUNGIN_V_EXP

  default <- list(
    v1  = CASPOFUNGIN_V1 * v,
    v2  = CASPOFUNGIN_V2 * v,
    v3  = 1,
    cl1 = CASPOFUNGIN_CL * cl / 60,
    cl2 = CASPOFUNGIN_Q * cl / 60,
    cl3 = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  list(
    PK = PK, tPeak = 0, MEAC = 0,
    typical = 0, upperTypical = 0, lowerTypical = 0,
    reference = paste0(
      "Xu N et al., Antimicrob Agents Chemother 2026;70:e01277-25, Table 2 (29 critically ",
      "ill children 0.33-16 years): two compartments, CL 0.196 L/h and Q 1.01 L/h x ",
      "(BSA/0.79)^0.66, V1 2.22 L and V2 1.63 L x BSA/0.79 (Mosteller BSA; Q and V2 scaled ",
      "as the paper's clearance and volume, an assumption); no ECMO; total plasma ",
      "caspofungin. https://doi.org/10.1128/aac.01277-25")
  )
}
