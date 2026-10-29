# -----------------------------------------------------------------------------
# Oxycodone: intravenous disposition pooled from three Finnish studies, with
# age and renal covariates, oral absorption and effect site from Lalovic
# 2006, and oxymorphone as its CYP2D6 metabolite
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in ng/mL.
#
# SOURCES
# =======
# None of the five papers fitted a population model.  Each reports
# noncompartmental or per-subject polyexponential summaries, and the model
# below is assembled from them as follows.
#
#   Poyhia 1991   9 young surgical adults (27 y, 71 kg), 0.07 mg/kg oxycodone
#                 chloride IV, which the paper calls "0.05 mg/kg free base":
#                 CL 0.78 L/min (11.0 mL/min/kg), Vss 2.60 L/kg, t1/2 3.7 h,
#                 computed on the free-base dose (see DOSE BASIS below).
#   Kirvela 1996  10 ASA I controls (36 y, 66 kg) and 10 uraemic patients
#                 (36 y, 67 kg) given 0.07 mg/kg oxycodone chloride IV
#                 ("0.05 mg/kg free base").  Medians: controls
#                 CL 1.10 L/min (16.7 mL/min/kg), Vc 0.22 and Vss 2.39 L/kg,
#                 t1/2 2.3 h; uraemic CL 0.84 L/min, Vss 3.99 L/kg, t1/2 3.9 h.
#                 Per-subject biexponential results in its Table 2.
#   Liukas 2011   41 orthopaedic patients, four age groups, 5 mg IV.  CL
#                 11.9, 8.6, 8.6 and 7.9 mL/min/kg at 27, 66, 77 and 84 y,
#                 with MDRD eGFR 108, 82, 76 and 64 mL/min/1.73 m^2; Vss
#                 3.2-3.7 L/kg, not different by age (p = 0.097).
#   Lalovic 2006  16 healthy adults (21-30 y, 73 kg), 15 mg oral: CL/F
#                 1.44 L/min (19.7 mL/min/kg), tmax 1.08 h, Cmax 38 ng/mL,
#                 t1/2 3.5 h; oxymorphone AUC 4% of oxycodone's; miosis
#                 PK-PD t1/2 ke0 11 min, EC50 30 ng/mL.
#   Balyan 2017   30 children (2-17 y, 46 kg), oral; CYP2D6 genotyped.
#                 Oxymorphone/oxycodone AUC ratio about 0.027 in extensive
#                 and 0.017 in poor/intermediate metabolisers; oxycodone
#                 exposure itself was not different by genotype.
#
# REFERENCE DISPOSITION (healthy adult, 70 kg, 30 years)
# ======================================================
# Clearance and Vss are the means of the three intravenous studies' young
# groups, each paper's own summary statistic (Kirvela reports medians), put
# on the hydrochloride dose basis (DOSE BASIS, below) and weighted by its
# number of subjects:
#
#     CL  = (9 x 14.5 + 10 x 23.4 + 11 x 11.9) / 30 = 16.5 mL/min/kg
#     Vss = (9 x 3.46 + 10 x 3.35 + 11 x 3.7)  / 30 = 3.51 L/kg
#
# Their mean age is 30 years, which is where the age effect below starts.
#
# DOSE BASIS
# ==========
# A dose is entered as the hydrochloride, as labelled, and the curve is
# oxycodone base, so CL and V must be the hydrochloride dose over the base
# concentrations.  All three studies gave the hydrochloride, but Poyhia and
# Kirvela did not compute on it.  Both full texts (read 2026-10-27, supplied
# by Steven L. Shafer) say the same: "Oxycodone chloride (Oxanest, 10 mg/ml,
# Leiras) was given in a dose of 0.07 mg/kg (= 0.05 mg/kg free base)"
# (Poyhia), "0.07 mg/kg (0.05 mg/kg free base)" (Kirvela).  Poyhia's Table 1
# lists each patient's dose in mg beside CL and Vss, and those doses are the
# free base: 3.1 mg at 57-62 kg, 4.7 mg at 90 kg, a mean of 0.0529 mg/kg, not
# 0.07.  So its CL (dose / AUC) and Vss were computed on the free base.
# Kirvela, the same group with the same product and wording, lists no dose
# and says only that the parameters "were calculated according to the
# standard formulae"; it is taken to follow the same convention (inferred,
# not stated).  Liukas gave "5 mg of intravenous oxycodone (OxyNorm)", the
# hydrochloride as labelled, with no conversion, so its CL is on the
# hydrochloride.
#
# The papers' free base is not oxycodone hydrochloride's: base over
# hydrochloride is 315.36 / 351.82 = 0.896, so 0.07 mg/kg of the
# hydrochloride holds 0.0627 mg/kg of base, where the papers state 0.05
# (0.714) and Poyhia's doses average 0.0529 (0.755).  Neither ratio matches
# any oxycodone salt checked (the trihydrate would be 0.777).  The rescaling
# therefore undoes each paper's own conversion rather than applying 0.896:
#
#   Poyhia   each patient's CL and Vss x 0.07 x weight / tabulated dose
#            (mean factor 1.33): CL 1.03 L/min (14.5 mL/min/kg), Vss 3.46 L/kg
#   Kirvela  x 0.07 / 0.05 = 1.4: CL 23.4 mL/min/kg, Vss 3.35 L/kg
#
# Until 2026-10-27 the published values were pooled unconverted (CL 13.2
# mL/min/kg, Vss 2.93 L/kg), and the note before that said the two papers
# gave 0.05 mg/kg of base as though that were the dose; both were wrong
# (opioid potency audit, docs/opioid-review-2026-10-27.md).  Rescaled, the
# pooled CL is 25% higher and Vss 20% higher, which lowers intravenous
# concentrations by about a fifth.  Oral exposure barely moves, because F is
# the pooled CL over Lalovic's CL/F (ORAL ABSORPTION, below).  Rescaled by decision of Steven L. Shafer,
# 2026-10-27.
#
# The three clearances now disagree more than they did: Liukas's 11.9
# against Poyhia's 14.5 and Kirvela's 23.4 mL/min/kg.  Clearance is the
# parameter to compare (AGENTS.md), and Liukas's is on the stated
# hydrochloride dose with a 2011 assay, so the model is 15-35% above
# Liukas's own four age groups (test-drugs-oxycodone.R pins the offset).
# Lalovic's oral CL/F, 19.7 mL/min/kg, gives F 0.84 at the pooled CL (ORAL
# ABSORPTION, below), against 0.60 at Liukas's.
#
# None of the papers reports a two-compartment model as such.  Kirvela gives
# Vc, Vss, CL and the terminal half-life for each subject, from which each
# subject's intercompartmental clearance follows exactly (the terminal
# eigenvalue fixes Q once the other three are known).  Subject 2 is
# internally inconsistent (a terminal half-life shorter than ln 2 x Vss/CL,
# which no biexponential allows) and is dropped.  For the remaining nine the
# medians are Vc/Vss = 0.131 and Q/CL = 2.33, and those ratios are carried
# onto the pooled CL and Vss:
#
#     V1 = 0.131 x 3.51 = 0.460 L/kg      V2 = 3.51 - 0.460 = 3.05 L/kg
#     Q  = 2.33 x 16.5  = 38.4 mL/min/kg
#
# The ratios are each subject's own and so do not depend on the dose basis.
# The terminal half-life is then 3.3 h, against Kirvela's 2.3 h, Poyhia's
# 3.7 h, Liukas's 4.0 h and Lalovic's 3.5 h after oral dosing.  The central volume is the least certain number
# here: early intravenous concentrations in these studies imply anything from
# 15 to over 100 L, depending on sampling site and time.
#
# AGE (Liukas 2011)
# =================
# Clearance fell with age, by 28-34% from the 20-40 group to the older ones.
# Part of that is renal function, which fell with age too.  Removing the
# renal factor below at each group's eGFR leaves age factors of 0.76, 0.77
# and 0.74 at 66, 77 and 84 years.  A linear decline from 30 years fitted to
# those by least squares through 1 at 30 is
#
#     ageFactor = 1 - 0.00527 x (age - 30),   age > 30
#
# (0.81, 0.75 and 0.72 at the three ages).  The age groups' Vss did not differ
# and is not adjusted.
#
# RENAL FUNCTION (Kirvela 1996)
# =============================
# In end-stage renal failure (creatinine 644 micromol/L, CKD-EPI eGFR about
# 8 mL/min/1.73 m^2) median clearance per kilogram was 0.75 of the controls'
# (eGFR about 100).  Oxycodone itself is under 10% renally excreted; the
# reduction is in hepatic metabolism.  Treating the reduction as linear in
# eGFR below a normal 100 mL/min/1.73 m^2:
#
#     renalFactor = 1 - 0.27 x (1 - min(eGFR, 100) / 100)
#
# which gives 0.75 at eGFR 8.  The eGFR is the CKD-EPI 2009 equation
# (R/renalFunction.R) rather than Liukas's MDRD; the two agree closely in
# this range.  It is computed from the Patient Profile's creatinine, read on
# the adult scale for a child.  A blank creatinine is an ASSUMED NORMAL
# creatinine for age and sex (assumedCreatinine()), so renal impairment is
# represented only when a creatinine is entered.  The assumed creatinine
# still lowers eGFR with age (78 mL/min/1.73 m^2 for a man of 84 at
# 0.9 mg/dL, against the 64 Liukas measured), and the age factor above was
# derived net of exactly that.
#
# Kirvela's uraemic patients also had a larger Vss (3.99 against 2.39 L/kg).
# That is not carried: they were volume loaded to a central venous pressure
# of 4 mmHg before sampling, and across the eGFR range of Liukas's
# non-dialysis patients Vss moved the other way, rising with eGFR.  The
# clearance effect alone takes the terminal half-life from 3.4 to 4.2 hours
# for Kirvela's patients (Kirvela: 2.3 to 3.9 h).
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# The pooled values are per kilogram of 70 kg adults.  With the switch on,
# volumes scale with fat-free mass relative to the reference man and
# clearances with that ratio ^ 0.75.  With it off, the per-kilogram values
# are used as published: volumes and clearances linear in total weight.
#
# CHILDREN: a check, not a fit
# ============================
# Balyan's children (11.9 y, 46 kg) had a dose-normalised AUC(0-24) of
# 10.9 ng.h/mL per mg, CL/F about 1.5 L/min.  This model with size scaling
# predicts CL/F about 1.0 L/min for a child of that size, so it is about a
# third low against the one paediatric oral dataset.  Their bioavailability was
# not measured, and nothing is adjusted for it (left so by decision of
# Steven L. Shafer, 2026-10-10).
#
# ORAL ABSORPTION (Lalovic 2006)
# ==============================
# Bioavailability is the reference clearance over Lalovic's per-kilogram
# CL/F: 16.5 / 19.7 = 0.84, above the 60-80% Lalovic quotes from the
# literature (it was 13.2 / 19.7 = 0.67 before the dose basis was corrected
# on 2026-10-27; see DOSE BASIS).  Taking F from the ratio keeps the oral
# exposure where Lalovic measured it whatever the intravenous clearance:
# the model's AUC after 15 mg is 10.5 ug.min/mL in a man of Lalovic's size
# (73 kg, 175 cm, 25 y) and 12.1 in a woman, against the 10.8 observed.
#
# ka (0.01 /min, no lag) puts the peak of the plasma curve at 30 ng/mL after
# 15 mg, the peak of the MEAN concentration curve in Lalovic's Figure 2, as
# the library matches oral peak heights (see R/drugs_diazepam.R; the 35-min
# peak kept so by decision of Steven L. Shafer, 2026-10-10).  The mean
# of the individual peaks, 38 ng/mL, is necessarily higher than the peak of
# the mean curve and is not the target.  The model then peaks at 35 min,
# earlier than Lalovic's mean tmax of 65 min, and gives 22.0, 12.5 and 3.6
# ng/mL at 3, 6 and 12 h, close to the mean curve.  (Re-solved for the
# 2026-10-27 disposition, ka is 0.0099 /min; 0.01 is kept, a mean peak of
# 30.3 ng/mL.)  Lalovic fitted most
# individual profiles with an absorption lag but did not report it; a lag
# that put the peak at 65 min could not also reach 30 ng/mL (it peaked at
# 28 with a 30-min lag), and during a lag the time until threshold cannot
# be shown, so none is used.  Balyan's children peaked later, at a median of
# about 2 h.
#
# EFFECT SITE (Lalovic 2006)
# ==========================
# Pupil constriction after oral oxycodone showed counter-clockwise hysteresis
# that the parent drug alone explained with an effect-site delay: the joint
# parent-noroxymorphone model failed, so the metabolites carry no effect.
# The mean t1/2 ke0 was 11 min (range 2-26; the mean ke0 of 0.1 /min is
# inflated by three subjects with almost no delay).  Lalovic drove the effect
# compartment with the observed plasma concentrations, so its ke0 does not
# depend on a pharmacokinetic model.  tPeak is the time of peak effect-site
# concentration after an intravenous bolus that ke0 = ln 2 / 11 gives with
# the reference patient's disposition.  This replaces the 60-minute tPeak
# of the previous model, which came from the time of peak CSF concentration.
#
# MEAC
# ====
# 12 ng/mL, set for consistency with morphine's 8 ng/mL.  None of the five
# papers behind the kinetics measured analgesia; Lalovic's EC50 of 30 ng/mL
# is for miosis.  (The earlier note that this was a compromise with "lower
# values suggested by Mandema" was wrong: Mandema 1996 has no
# pharmacodynamics.)
#
# The opioid MEAC and tPeak audit (2026-10-12) weighed two lines of evidence:
#   - Consistency with morphine.  At equal intravenous dose rates this model
#     reaches 1.4 times morphine's steady-state concentration (CL 0.86 against
#     1.23 L/min for the 70 kg reference), and intravenous oxycodone is 1.0 to
#     1.3 times as many mg as morphine for the same analgesia, so a MEAC
#     consistent with morphine's 8 is 8 x 1.4 x (1.0 to 1.3) = 11 to 15.
#     (With the 2026-10-27 dose basis, CL is 25% higher, so the ratio is
#     1.4 / 1.25 = 1.1 and the range 9 to 12; the MEAC of 12 is unchanged.)
#   - Direct measurement.  Kokki 2012 titrated intravenous oxycodone after
#     laparoscopic cholecystectomy and measured total venous plasma oxycodone
#     at the onset of pain (MEC, mean 21 ng/mL, 95% CI 13-29) and 10 min after
#     the dose that relieved it (MEAC, 55 and 47 ng/mL; authors' summary
#     45-50).  Kokki cites Pesonen's cardiac-surgery values of 6-12 (MEC) and
#     15-25 ng/mL (MEAC).  Kokki's MEC uses the same definition as Gourlay's
#     fentanyl MEC behind fentanyl's 0.6 ng/mL, and lies above 12.
# The direct value is higher in proportion to the way morphine's 8 ng/mL sits
# at the low end of morphine's own literature (Dahlstrom 1982, 16 ng/mL,
# range 6-33, as cited by Yue 1991 and Persson 1992), so the two are kept
# together.  Kept at 12 by decision of Steven L. Shafer, 2026-10-12, which
# resolves the TO BE CHECKED note of 2026-10-10.  With the 11-minute ke0,
# 10 mg by mouth peaks at about 19 ng/mL in the effect site, 1.6 MEAC, as
# morphine 30 mg by mouth does (1.6 MEAC).
#
# Kokki M et al., Basic Clin Pharmacol Toxicol 2012;110:469-475.
#   https://doi.org/10.1111/j.1742-7843.2011.00839.x
#
# OXYMORPHONE
# ===========
# Calibrated against observed plasma AUC ratios, by route:
#   * Intravenous: Poyhia 1991 (assay limit 0.5 ng/mL) and Kirvela 1996 did
#     not detect oxymorphone after IV oxycodone, and Liukas 2011 (limit
#     0.1 ng/mL) found it mostly unquantifiable, which puts the IV AUC ratio
#     at about 1% or less.  Systemic formation is set to give exactly 1% for a
#     normal metaboliser.
#   * Oral: 0.027 in Balyan's genotyped extensive metabolisers and 0.04 in
#     Lalovic's ungenotyped adults.  The midpoint, 0.0335, is the target for
#     a normal metaboliser.  The excess over the intravenous ratio is
#     presystemic formation (Liukas: "the oral route results in significantly
#     greater production of the oxidative metabolites"), carried as a
#     first-pass fraction of the oral dose.  It enters through the
#     absorption step, so after an oral dose the formed oxymorphone peaks at
#     about 2 h, later than the 1 h Lalovic observed.
# The AUC ratio is formation clearance x mwRatio over oxymorphone's
# clearance, so the formation clearance is taken from the oxymorphone model
# for the same patient and switch, and the IV ratio holds at any size.
#
# Formation is an independent transfer and is not subtracted from the parent,
# whose fitted clearance already subsumes it (R/metaboliteCoefficients.R);
# oxycodone's own curves are the same with and without oxymorphone, and
# Balyan found no CYP2D6 effect on oxycodone exposure.
#
# CYP2D6 weights are relative formation of oxymorphone, normal = 1.  Samer
# 2010 measured oxymorphone peak concentration 62% lower in poor than in
# extensive metabolisers (the floor) and 75% lower in poor than in
# ultrarapid (the top).  The intermediate value interpolates between the
# floor and normal using the relative CYP2D6 activity implied by Ashraf
# 2024's activity-score groups.  Balyan 2017 measured 0.017 / 0.027 = 0.63
# in a poor/intermediate group of 13 intermediate and one poor metaboliser,
# which corroborates it.
#
# PREGNANCY
# =========
# Nothing is applied: the Pregnant field does not change this model.  Until
# 2026-10-28 it multiplied the elimination clearance by 3.8/2.6 = 1.46, the
# ratio of half-times in Ansari, Carvalho, Shafer and Flood (Anesth Analg
# 2016;122:786-804, Table 3), who took them from Kokki 2012.  Kokki 2012
# (Basic Clin Pharmacol Toxicol 111:182-188, full text read) fitted a
# population model to 15 nulliparous women given 1 mg IV boluses in
# first-stage labour: CL 0.92 L/min, central V 95 L, peripheral V 100 L
# (Table 3), at a median 82 kg and 29 years.  Its "clearance was higher" and
# 3.8 h are against Gronlund 2011's healthy volunteers (Clin Drug Investig
# 31:143-153), not against this model: this model's non-pregnant clearance
# for an 82 kg, 161 cm, 29-year-old woman is 1.03 L/min (Vss 213 L), so
# Kokki's labouring women have about the same clearance (0.89x) and a
# slightly smaller volume (0.92x).  The factor gave them 1.51 L/min, 64% more
# than Kokki measured, and was dropped by Steven L. Shafer's decision of
# 2026-10-28 (the unattended digest of Kinnunen 2019 raised it).  See
# R/pregnancy.R.
#
# References
# ----------
# Poyhia R, Olkkola KT, Seppala T, Kalso E. Br J Clin Pharmacol
#   1991;32:516-518.  https://doi.org/10.1111/j.1365-2125.1991.tb03942.x
# Kirvela M, Lindgren L, Seppala T, Olkkola KT. J Clin Anesth 1996;8:13-18.
#   https://doi.org/10.1016/0952-8180(95)00092-5
# Liukas A, Kuusniemi K, Aantaa R, et al. Drugs Aging 2011;28:41-50.
#   https://doi.org/10.2165/11586140-000000000-00000
# Lalovic B, Kharasch E, Hoffer C, et al. Clin Pharmacol Ther
#   2006;79:461-479.  https://doi.org/10.1016/j.clpt.2006.01.009
# Balyan R, Mecoli M, Venkatasubramanian R, et al. Pharmacogenomics
#   2017;18:337-348.  https://doi.org/10.2217/pgs-2016-0183
# Samer CF et al. Br J Pharmacol 2010;160:907-918 (pharmacokinetics; the
#   oxymorphone Cmax figures).  PMID 20590587.
#   https://doi.org/10.1111/j.1476-5381.2010.00673.x
# Samer CF et al. Br J Pharmacol 2010;160:919-930 (the companion
#   pharmacodynamic paper).  https://doi.org/10.1111/j.1476-5381.2010.00709.x
# Ansari J, Carvalho B, Shafer SL, Flood P.  Anesth Analg 2016;122:786-804.
#   https://doi.org/10.1213/ANE.0000000000001143 (pregnancy: the factor
#   dropped 2026-10-28)
# Kokki M, Franco MG, Raatikainen K, et al.  Basic Clin Pharmacol Toxicol
#   2012;111:182-188.  https://doi.org/10.1111/j.1742-7843.2012.00884.x
#   (pregnancy: clearance in labour)
# -----------------------------------------------------------------------------


# Relative CYP2D6 activity for oxymorphone formation, normal = 1 (see header)
OXYCODONE_CYP2D6_WEIGHT <- c(
  poor         = 0.38,
  intermediate = 0.6514113,
  normal       = 1.0,
  ultrarapid   = 1.52
)

#' Oxycodone pharmacokinetics
#'
#' Two-compartment intravenous disposition pooled from Poyhia 1991, Kirvela
#' 1996 and Liukas 2011, with clearance falling with age (Liukas) and renal
#' function (Kirvela), oral absorption and effect site from Lalovic 2006, and
#' oxymorphone formed by CYP2D6.
#'
#' @param weight weight in kg
#' @param height height in cm
#' @param age age in years
#' @param sex \code{"male"} or \code{"female"}
#' @param cyp2d6 CYP2D6 metaboliser phenotype, one of \code{CYP2D6_VALUES};
#'   scales oxymorphone formation only
#' @param adjustToFFM scale volumes to fat-free mass and clearances to its
#'   0.75 power; when \code{FALSE}, scale both linearly with total weight
#' @param creatinine serum creatinine in mg/dL, or NULL for an assumed normal
#'   value for age and sex
#' @returns a list in the shape \code{getDrugPK()} expects, naming
#'   oxymorphone as the formed active metabolite
#' @export
oxycodone <- function(weight, height, age, sex, cyp2d6 = CYP2D6_DEFAULT,
                      adjustToFFM = TRUE, creatinine = NULL)
{
  if (length(cyp2d6) != 1 || !cyp2d6 %in% CYP2D6_VALUES) {
    stop("Invalid cyp2d6: ", paste(cyp2d6, collapse = ", "),
         ". Must be one of: ", paste(CYP2D6_VALUES, collapse = ", "))
  }
  cypActivity <- unname(OXYCODONE_CYP2D6_WEIGHT[[cyp2d6]])

  # --- Reference disposition, 70 kg adult of 30 years (see header) ---
  CL_PER_KG  <- 0.0165          # L/min/kg, pooled, hydrochloride dose basis
  VSS_PER_KG <- 3.51            # L/kg, pooled, hydrochloride dose basis
  VC_FRACTION <- 0.131          # Vc/Vss, Kirvela per-subject median
  Q_OVER_CL   <- 2.33           # Q/CL, Kirvela per-subject median

  # --- Covariates on clearance (see header) ---
  AGE_SLOPE  <- 0.00527         # per year above 30, Liukas
  AGE_REF    <- 30
  RENAL_FRACTION <- 0.27        # Kirvela, linear in eGFR below 100
  EGFR_NORMAL    <- 100         # mL/min/1.73 m^2

  ageFactor <- 1 - AGE_SLOPE * max(0, age - AGE_REF)
  eGFR <- egfrCKDEPI2009(age, sex, adultEquivalentCreatinine(creatinine, age, sex))
  renalFactor <- 1 - RENAL_FRACTION * (1 - min(eGFR, EGFR_NORMAL) / EGFR_NORMAL)

  # Size scaling (see docs/weight-adjustment.md).  The published values are
  # per kilogram, so with the switch off both scale with total weight / 70.
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)

  vss <- VSS_PER_KG * 70 * size$volume
  v1  <- VC_FRACTION * vss
  v2  <- vss - v1
  v3  <- 1                      # no third compartment
  clRef <- CL_PER_KG * 70 * size$clearance
  cl1 <- clRef * ageFactor * renalFactor
  cl2 <- Q_OVER_CL * clRef      # distribution is not adjusted for age or renal function
  cl3 <- 0

  # --- Oral absorption, Lalovic 2006 (see header) ---
  bioavailability_PO <- 0.84
  ka_PO   <- 0.01               # 1/min, absorption half-time 69 min
  tlag_PO <- 0                  # no lag (see header)
  #
  # Oral liquid ("mg PO liquid", "mg/kg PO liquid") takes these same oral
  # parameters, exactly as "mg PO" does, and is deliberately not listed in
  # oralFormulations.  Two head-to-head comparisons find solution and
  # immediate-release tablet equivalent.  The ROXICODONE label, 15 mg fasted,
  # single dose: solution Cmax 21.1, AUC 130.6 ng.h/mL, tmax 1.9 h; tablet
  # 22.2, 128.2, 1.4 h.  FDA clinical pharmacology review of NDA 200535,
  # 15 mg of the 20 mg/mL solution against a 15 mg ROXICODONE tablet,
  # fasted: geometric mean ratios 105.2% (Cmax), 108.5% (AUCt) and 107.6%
  # (AUCinf), inside the 80-125% bioequivalence limits.  The liquid is not
  # absorbed faster, unlike morphine's.  (Claude Code, 2026-10-10, at the
  # request of Steven L. Shafer.)

  # --- Effect site, Lalovic 2006: t1/2 ke0 11 min ---
  tPeak <- 12.09                # min after an IV bolus, reference patient

  MEAC <- 12
  typical      <- MEAC * 1.2
  lowerTypical <- MEAC * 0.8
  upperTypical <- MEAC * 2.0

  reference <- paste0(
    "Poyhia R et al., Br J Clin Pharmacol 1991;32:516-518; ",
    "Kirvela M et al., J Clin Anesth 1996;8:13-18; ",
    "Liukas A et al., Drugs Aging 2011;28:41-50 (pooled IV disposition, ",
    "rescaled to the hydrochloride dose; ",
    "age and renal function on clearance, from the entered creatinine or an ",
    "assumed normal one); Lalovic B et al., Clin Pharmacol Ther ",
    "2006;79:461-479 (oral absorption, ke0); Balyan R et al., ",
    "Pharmacogenomics 2017;18:337-348 (CYP2D6). ",
    "https://doi.org/10.1016/j.clpt.2006.01.009"
  )

  default <- list(
    v1 = v1,
    v2 = v2,
    v3 = v3,
    cl1 = cl1,
    cl2 = cl2,
    cl3 = cl3,
    ka_PO = ka_PO,
    bioavailability_PO = bioavailability_PO,
    tlag_PO = tlag_PO
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  # --- Oxymorphone (see header) ---
  # AUC ratio = formation clearance x mwRatio / oxymorphone clearance, so the
  # formation clearance is set from oxymorphone's clearance for this patient.
  MW_RATIO <- 301.34 / 315.36   # oxymorphone over oxycodone, g/mol
  RATIO_IV <- 0.01              # normal metaboliser, intravenous
  # Oral ratio 0.0335: (0.0335 - 0.01) x F x CL_oxymorphone / (mwRatio x CL),
  # at the reference patient: 0.0235 x 0.84 x 2.0 / (0.9555 x 1.155); F / CL
  # is Lalovic's F / CL, so this did not move with the dose basis
  FIRST_PASS_NORMAL <- 0.0357
  clOxymorphone <- oxymorphone(weight, height, age, sex,
                               adjustToFFM = adjustToFFM)$PK$default$cl1
  clFormation <- RATIO_IV * clOxymorphone / MW_RATIO

  return(
    list(
      PK = PK,
      tPeak = tPeak,
      MEAC = MEAC,
      typical = typical,
      upperTypical = upperTypical,
      lowerTypical = lowerTypical,
      reference = reference,
      metabolite = list(
        name              = "oxymorphone",
        kFormation        = clFormation / v1 * cypActivity,
        firstPassFraction = FIRST_PASS_NORMAL * cypActivity,
        mwRatio           = MW_RATIO
      )
    )
  )
}
