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
#   Poyhia 1991   9 young surgical adults (27 y, 71 kg), 0.05 mg/kg base IV:
#                 CL 0.78 L/min (11.0 mL/min/kg), Vss 2.60 L/kg, t1/2 3.7 h.
#   Kirvela 1996  10 ASA I controls (36 y, 66 kg) and 10 uraemic patients
#                 (36 y, 67 kg) given 0.05 mg/kg base IV.  Medians: controls
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
# groups, each paper's own summary statistic (Kirvela reports medians)
# weighted by its number of subjects:
#
#     CL  = (9 x 11.0 + 10 x 16.7 + 11 x 11.9) / 30 = 13.2 mL/min/kg
#     Vss = (9 x 2.60 + 10 x 2.39 + 11 x 3.7)  / 30 = 2.93 L/kg
#
# Their mean age is 30 years, which is where the age effect below starts.
# Two of the three studies used free-base doses; Liukas gave "5 mg" of the
# hydrochloride and may have computed clearance on it, which would make its
# clearance about 10% high.  That is inside the spread between studies and is
# not corrected.
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
#     V1 = 0.131 x 2.93 = 0.385 L/kg      V2 = 2.93 - 0.385 = 2.55 L/kg
#     Q  = 2.33 x 13.2  = 30.8 mL/min/kg
#
# The terminal half-life is then 3.4 h, against Kirvela's 2.3 h, Poyhia's
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
# still lowers eGFR with age (69 mL/min/1.73 m^2 for a man of 84 at
# 1.0 mg/dL, against the 64 Liukas measured), and the age factor above was
# derived net of exactly that.
#
# Kirvela's uraemic patients also had a larger Vss (3.99 against 2.39 L/kg).
# That is not carried: they were volume loaded to a central venous pressure
# of 4 mmHg before sampling, and across the eGFR range of Liukas's
# non-dialysis patients Vss moved the other way, rising with eGFR.  The
# clearance effect alone takes the terminal half-life from 3.5 to 4.4 hours
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
# CL/F: 13.2 / 19.7 = 0.67, inside the 60-80% Lalovic quotes from the
# literature.  The model's AUC after 15 mg in Lalovic's subjects is
# 10.1 ug.min/mL against the 10.8 observed.
#
# ka (0.01 /min, no lag) puts the peak of the plasma curve at 30 ng/mL after
# 15 mg, the peak of the MEAN concentration curve in Lalovic's Figure 2, as
# the library matches oral peak heights (see R/drugs_diazepam.R; the 35-min
# peak kept so by decision of Steven L. Shafer, 2026-10-10).  The mean
# of the individual peaks, 38 ng/mL, is necessarily higher than the peak of
# the mean curve and is not the target.  The model then peaks at 35 min,
# earlier than Lalovic's mean tmax of 65 min, and gives 21.7, 12.4 and 3.7
# ng/mL at 3, 6 and 12 h, close to the mean curve.  Lalovic fitted most
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
# 12 ng/mL, retained from the previous model: a compromise between the lower
# values suggested by Mandema and the 45-50 ng/mL suggested by Kokki 2012.
# None of these five papers measured analgesia.  Lalovic's EC50 of 30 ng/mL
# is for miosis, not analgesia.
#
# TO BE CHECKED: kept at 12 by decision of Steven L. Shafer (2026-10-10),
# pending a separate audit of the MEAC and tPeak of every opioid in the
# library for outliers, to be done as its own piece of work.  With this model's 11-minute ke0 the effect site runs much closer
# to plasma than under the old 60-minute tPeak, so 10 mg by mouth now peaks
# at about 18 ng/mL in the effect site, against 12.
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
# References
# ----------
# Poyhia R, Olkkola KT, Seppala T, Kalso E. Br J Clin Pharmacol
#   1991;32:516-518.  https://doi.org/10.1111/j.1365-2125.1991.tb03943.x
# Kirvela M, Lindgren L, Seppala T, Olkkola KT. J Clin Anesth 1996;8:13-18.
#   https://doi.org/10.1016/0952-8180(95)00092-5
# Liukas A, Kuusniemi K, Aantaa R, et al. Drugs Aging 2011;28:41-50.
#   https://doi.org/10.2165/11586140-000000000-00000
# Lalovic B, Kharasch E, Hoffer C, et al. Clin Pharmacol Ther
#   2006;79:461-479.  https://doi.org/10.1016/j.clpt.2006.01.009
# Balyan R, Mecoli M, Venkatasubramanian R, et al. Pharmacogenomics
#   2017;18:337-348.  https://doi.org/10.2217/pgs-2016-0183
# Samer CF et al. Br J Pharmacol 2010;160:919-930.
#   https://doi.org/10.1111/j.1476-5381.2010.00709.x
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
  CL_PER_KG  <- 0.0132          # L/min/kg, pooled
  VSS_PER_KG <- 2.93            # L/kg, pooled
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
  bioavailability_PO <- 0.67
  ka_PO   <- 0.01               # 1/min, absorption half-time 69 min
  tlag_PO <- 0                  # no lag (see header)

  # --- Effect site, Lalovic 2006: t1/2 ke0 11 min ---
  tPeak <- 12.35                # min after an IV bolus, reference patient

  MEAC <- 12
  typical      <- MEAC * 1.2
  upperTypical <- MEAC * 0.8
  lowerTypical <- MEAC * 2.0

  reference <- paste0(
    "Poyhia R et al., Br J Clin Pharmacol 1991;32:516-518; ",
    "Kirvela M et al., J Clin Anesth 1996;8:13-18; ",
    "Liukas A et al., Drugs Aging 2011;28:41-50 (pooled IV disposition; ",
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
  # at the reference patient: 0.0235 x 0.67 x 2.0 / (0.9555 x 0.924)
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
