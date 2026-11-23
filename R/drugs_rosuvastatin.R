# -----------------------------------------------------------------------------
# Rosuvastatin: two compartments, sequential zero- and first-order oral
# absorption (Crestor), apparent oral parameters
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in ng/mL.  Concentration only: no LDL-cholesterol is plotted.
# Drafted by Claude Code, 2026-11-19, at the request of Steven L. Shafer.
#
# SOURCE
# ======
# Courlet P, Guidi M, Alves Saldanha S, Stader F, Traytel A, Cavassini M,
# Stoeckle M, Buclin T, Marzolini C, Decosterd LA, Csajka C, and the Swiss HIV
# Cohort Study.  Pharmacokinetic/pharmacodynamic modelling to describe the
# cholesterol lowering effect of rosuvastatin in people living with HIV.  Clin
# Pharmacokinet 2021;60:379-390.  PMID 33124006, PMC7932937,
# https://doi.org/10.1007/s40262-020-00946-3 (open access, CC BY-NC).  Read in
# full on 2026-11-19 from the publisher's JATS full text in Europe PMC (the
# PDF itself was not reachable from the session; the table cells are the
# publisher's own, not a rendering of the page).
#
# 154 concentrations from 65 adults living with HIV and taking rosuvastatin
# 5-20 mg once daily (median 10 mg) for hypercholesterolaemia in the Swiss HIV
# Cohort Study: 6 richly sampled (65 samples) and 62 sparsely sampled at
# routine visits (89), 0.2 to 38.5 h after the last dose.  Table 1: median age
# 55 years (IQR 49-64), 8 women (12%), weight 75 kg (66-85), creatinine
# clearance 87 mL/min/1.73 m^2 (79-119).  NONMEM ADVAN4 TRANS4.  Table 2, the
# final model:
#
#     ka   = 0.306 /h   (fixed from the rich data)
#     D1   = 0.461 h    (zero-order duration; fixed from the rich data)
#     CL/F = 122 L/h    (RSE 9%, BSV 51%)
#     Vc/F = 144 L      (RSE 47%, BSV 94%)
#     Vp/F = 1610 L     (RSE 33%)
#     Q/F  = 69 L/h     (RSE 19%)
#
# The absorption is sequential: each dose is released into the gut at a
# constant rate over D1 and then absorbed first-order (Fig. 1).  It runs
# through zeroOrderPO (R/subcutaneous.R) exactly, as canagliflozin's does.  No
# lag (adding one did not improve the fit).  No covariate was retained:
# sex, age, weight, creatinine clearance, ALT, AST and boosted protease
# inhibitors were tested on CL and none was significant; the study could rule
# out a fall in CL of more than 22% with boosted protease inhibitors.
#
# The paper's own check: its simulated steady-state AUC(0-24) at 20 mg a day,
# median 164 ng.h/mL, is 20000 / 122 (pinned in the test).
#
# WHY THIS SOURCE
# ===============
# The only adult population model of rosuvastatin whose full text could be
# read.  Tzeng 2008 (Curr Med Res Opin 24:2575-2585, PMID 18674408; healthy
# volunteers, dyslipidaemic patients and renal failure; AstraZeneca) is not in
# the archive or PubMed Central and was not read.  Macpherson 2016 (Eur J Clin
# Pharmacol 72:19-27, PMID 26387811, read on the PDF) is in children 6-17 years
# with heterozygous familial hypercholesterolaemia; its Figure 3 shows adult
# DYSLIPIDAEMIC PATIENTS clearing the drug far more slowly than adult healthy
# volunteers (median CL/F 78.8 against 302 L/h; Tzeng's own analysis put a
# 41% to 71% lower CL/F on dyslipidaemia, the restatements differ), so the
# healthy-volunteer models (An 2025, PMID 40491527, Korean men, with
# enterohepatic recirculation the engine cannot carry; the microdose and DDI
# studies) do not describe the patients who take the drug.  Courlet's
# patients are outpatients taking it as prescribed, which is the population
# the library prefers for a drug taken at home (AGENTS.md, *Adherence*), and
# its CL/F, 122 L/h, lies between Macpherson's adult patients (78.8) and
# volunteers (302).  Its weakness: the patients were living with HIV, 46% of
# the samples were taken on ritonavir-boosted darunavir, and the absorption
# was fitted to six richly sampled people.
#
# CHECKS AGAINST THE LABEL
# ========================
# Crestor (rosuvastatin calcium), AstraZeneca, NDA 021366, DailyMed setid
# 325a5d0e-9a72-4015-9fcd-1655fb504cee, read 2026-11-19, section 12.3: peak 3
# to 5 h after a dose; exposure proportional to the dose; absolute
# bioavailability about 20%; Vss about 134 L (after an intravenous dose);
# half-life about 19 h; no difference by sex or age (65 and over).  This
# model: terminal half-life 25.6 h (Courlet sampled to 38.5 h; a longer
# sampling lengthens a terminal half-life, AGENTS.md *Comparing sources*);
# apparent Vss 1754 L, about 350 L at F = 0.2, which is not comparable with
# the label's 134 L (a volume, so a construct; see AGENTS.md *Only clearance
# is real*).
#
# THE PEAK DISAGREES WITH THE LABEL.  After a single 20 mg dose this model
# peaks at 1.7 h with 20.7 ng/mL (pinned in the test, and reproduced by an
# independent Runge-Kutta integration of Table 2), earlier than the label's
# 3 to 5 h.  Macpherson 2016's model evaluated at a 75 kg man (CL/F 129 x
# (75/42)^0.352 x 1.41 = 223 L/h, Vc/F 303 L, Q/F 89.9 L/h, Vp/F 5153 L, ka
# 0.183 /h; Table 2) peaks at 2.1 h with 8.1 ng/mL: also early, and 2.5 times
# lower.  Courlet's absorption was fitted to six richly sampled people,
# its Vc/F is the least precise of its parameters (RSE 47%, between-subject
# variability 94%, which the authors say carried the unexplained variability
# of absorption), and 46% of its samples were taken on ritonavir-boosted
# darunavir, which raises rosuvastatin's Cmax.  So the AUC (CL/F) is the
# number to trust here, and the early, high peak is a [decision] for Shafer,
# recorded with the drug's pull request.
#
# DOSE BASIS
# ==========
# Crestor tablets are labelled as rosuvastatin (5, 10, 20, 40 mg, "equivalent
# to" 5.2, 10.4, 20.8, 41.6 mg of the calcium salt), and Courlet's doses are
# the prescribed 5-20 mg: the dose is entered as labelled, rosuvastatin, with
# no conversion, and the curve is rosuvastatin.
#
# APPARENT PARAMETERS
# ===================
# Fitted to oral data alone, so CL, V and Q are divided by the unmeasured
# bioavailability: oral units only, bioavailability_PO = 1.
#
# WHAT IS NOT REPRESENTED
# =======================
# * Asian ancestry: the label reports about twice the exposure (AUC and Cmax)
#   in Asian subjects, and Tzeng 2008 a 44% lower CL/F.  Courlet's cohort was
#   mostly white and did not estimate it; the Patient Profile has no ancestry
#   field.  Not applied.
# * SLCO1B1 (c.521T>C) and ABCG2 (c.421C>A) genotypes raise exposure; the
#   Patient Profile has no field for either.  Not applied.
# * Severe renal impairment (CLcr < 30 mL/min/1.73 m^2): the label reports
#   about three times the concentrations; mild to moderate impairment has no
#   effect.  Courlet tested creatinine clearance (IQR 79-119) and found none.
#   No renal term.
# * Interacting drugs (cyclosporine, gemfibrozil, the boosted protease
#   inhibitors) and the lactone.
#
# BODY SIZE ANCHOR
# ================
# No size covariate: the parameters are fixed values and should be anchored
# at Courlet's cohort (docs/weight-adjustment.md, *Anchoring at the source's
# cohort*): 57 men and 8 women, median 75 kg and 55 years (Table 1).  No
# height, BMI or BSA is printed, so the rule's step 3 needs NCD-RisC's mean
# height at 18 for Switzerland and the birth year 2018 - 55 = 1963, and the
# NCD-RisC file (ncdrisc.org) could not be reached from the session that wrote
# this model.  The model is therefore left at the reference anchor (the
# published values belong to the 70 kg, 170 cm, 35-year-old reference man)
# until that height is supplied.  The cost, computed with cohortSizeFactors()
# for men's heights of 172 to 178 cm (women 13.5 cm shorter): the cohort's
# patient would take factors of 1.016 to 1.047 on volume and 1.012 to 1.035
# on clearance, so this model's values are 1-5% higher than anchoring would
# give.  A [source] item, recorded with the drug's pull request.
#
# PREGNANCY
# =========
# Not applied (group unquantified; inst/extdata/pregnancyRegistry.csv).  Pego
# 2024 (J Clin Pharmacol, PMID 39189980; 11 healthy women, third trimester
# against 8-12 weeks postpartum, 5 mg) found AUC(0-24) 18.7 against 9.5
# ng.h/mL, and Moreira 2023 (J Clin Pharmacol 63:219-227, PMID 36087110;
# women living with HIV) 19.5 in the third trimester, 13.3 postpartum and 13.8
# in healthy nonpregnant women; Balhara 2025 (CPT Pharmacometrics Syst
# Pharmacol 14:2052-2064, PMC12706398, read in full) reports the terminal
# half-life unchanged, so the rise is in bioavailability, and notes that the
# postpartum values were lower than in nonpregnant adults, so the size of the
# change against a nonpregnant woman (1.4 to 2.0) is uncertain.  Only the
# abstracts of the two measuring studies were read.  The label advises
# stopping rosuvastatin when pregnancy is recognised.
# -----------------------------------------------------------------------------

ROSUVASTATIN_KA   <- 0.306   # /h, Courlet 2021 Table 2 (fixed)
ROSUVASTATIN_D1   <- 0.461   # h, zero-order duration, Table 2 (fixed)
ROSUVASTATIN_CL   <- 122     # L/h, CL/F, Table 2
ROSUVASTATIN_VC   <- 144     # L, Vc/F, Table 2
ROSUVASTATIN_VP   <- 1610    # L, Vp/F, Table 2
ROSUVASTATIN_Q    <- 69      # L/h, Q/F, Table 2

#' Rosuvastatin pharmacokinetics (oral)
#'
#' Courlet et al. (2021): two compartments with sequential zero- and
#' first-order absorption, apparent oral parameters, no covariates, fitted in
#' adults taking rosuvastatin 5-20 mg daily.  Concentration only.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
rosuvastatin <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  # Fixed values with no size covariate, left at the reference anchor until
  # the cohort's height can be read (see BODY SIZE ANCHOR above); the switch
  # off reproduces the published values.
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1)

  default <- list(
    v1  = ROSUVASTATIN_VC * size$volume,
    v2  = ROSUVASTATIN_VP * size$volume,
    v3  = 1,
    cl1 = ROSUVASTATIN_CL / 60 * size$clearance,
    cl2 = ROSUVASTATIN_Q / 60 * size$clearance,
    cl3 = 0,
    ka_PO = ROSUVASTATIN_KA / 60,
    bioavailability_PO = 1,   # apparent parameters
    tlag_PO = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  list(
    PK = PK, tPeak = 0, MEAC = 0,
    typical = 0, upperTypical = 0, lowerTypical = 0,
    # Each tablet is released into the gut over D1, then absorbed first-order
    zeroOrderPO = list(duration = ROSUVASTATIN_D1 * 60),
    reference = paste0(
      "Courlet 2021 (65 adults living with HIV taking rosuvastatin 5-20 mg daily, ",
      "Swiss HIV Cohort Study): two compartments, apparent oral parameters, ",
      "CL/F 122 L/h, Vc/F 144 L, Vp/F 1610 L, Q/F 69 L/h; release into the gut over ",
      "0.461 h, then ka 0.306/h; no covariates; dose as labelled (rosuvastatin); ",
      "Asian ancestry, SLCO1B1 and ABCG2 genotypes and severe renal impairment not ",
      "represented; concentration only. ",
      "https://doi.org/10.1007/s40262-020-00946-3"
    )
  )
}
