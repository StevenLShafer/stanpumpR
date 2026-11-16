# -----------------------------------------------------------------------------
# Oxaliplatin, intravenous, in adults (Bastian 2003): plasma ultrafilterable
# platinum, as oxaliplatin, with enterohepatic recirculation
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min; amounts in
# mg of oxaliplatin and concentrations in mcg/mL (= mg/L).
#
# Drafted by Claude Code, 2026-11-06, at the request of Steven L. Shafer
# (docs/chemotherapy.md section 31, inst/extdata/chemotherapyRegistry.csv,
# source C06).  Every number below was read on the page images of the PDF
# Shafer supplied (pp. 818-823): Table 1, Table 3, the covariate equations
# and the Methods.  Shafer decided (2026-11-06) to implement the model AS
# PUBLISHED, the gallbladder filling only until TLAG.
#
# THE SOURCE
# ==========
# Bastian G, Barrail A, Urien S.  Population pharmacokinetics of oxaliplatin
# in patients with metastatic cancer.  Anticancer Drugs 2003;14:817-824
# (doi 10.1097/00001813-200311000-00007).  56 adults (29 men, 27 women), 66
# courses, 41-107 kg, serum creatinine 53-567 umol/L (median 87), from two
# French trials; 50-130 mg/m2 over 2 or 4 h (33 courses each); 537 samples
# over 28 h of plasma ultrafiltrate, platinum by flameless atomic absorption;
# NONMEM.
#
# THE MODEL (Fig. 1, the Methods, Table 3)
# ========================================
# Two compartments, CL from V1, with a separate first-order transfer k1B from
# V1 into a storage site, "likely the gallbladder", whose content is released
# "as a bolus at the time of expulsion from the gallbladder, TLAG" into a gut
# depot that returns to V1 at Ka:
#
#     CL = 14.1 L/h x (BW/71)^1.10 x (SCr/87)^-0.57 x 0.60 (if female)
#     V1 = 24.9 L x (BW/71)^1.29,   Q = 34.8 L/h x (BW/71)^1.01,   V2 = 136 L
#     k1B = 1.78 /h,   TLAG = 2.00 h,   Ka = 0.23 /h
#
# BW in kg, SCr in umol/L, both centred on the cohort's medians.  The store is
# carried by the biliaryStore block and R/advanceBiliaryStore.R, which says
# how each course gets its own store and switch and why the courses add.
#
# WHAT HAPPENS AFTER TLAG: AS PUBLISHED
# =====================================
# Bastian cite Plusquellec & Houin 1992 (J Biomed Eng 14:521-526, read in
# full on the page images, with the same authors' application to an
# infusion, Plusquellec et al. 1992, J Pharm Sci 81:1020-1023, and their 1995
# Med Eng Phys 17:172-176).  In Plusquellec's equations (5)-(10) the transfer
# into the store goes on at all times (K1 = k10 + k12 + k1B throughout) and
# the store empties at each chosen time T1..Tn, measured from the start of
# the infusion; after the last emptying it fills and is never emptied, so
# with Bastian's single TLAG the store would be a sink: terminal half-life
# 4.4 h and AUC(0-inf) about 35% of Dose/CL for the typical man.  Bastian's
# results say otherwise: AUC = Dose/CL (their 230 mg / 11.3 L/h = 20.35 mg
# h/L and Table 4) and terminal half-lives of 10.5 h (men) and 14.4 h
# (women), which are the two-compartment system's own (10.3 and 15.5 h at the
# typical values: the printed ones look like posthoc means).  So the model
# whose numbers the paper prints stops filling the store at TLAG; that is
# what is implemented ("reading D" of the extraction), the conflict with the
# cited equations recorded here and on the help page.  TLAG is from the start
# of each course, as in Plusquellec (the paper does not say; a 2-h infusion
# then ends as the store empties).
#
# THE INFUSION-LENGTH ARTEFACT
# ============================
# Because the store stops taking drug at TLAG, a 4-h infusion of 230 mg peaks
# at 1.75 mcg/mL in the typical man against 1.38 for a 2-h one: the second
# half of the longer infusion meets no biliary sink.  The fitted V1, CL and
# Q are only meaningful with the store: without it the 2-h peak would be 2.71.
# The very large variability of TLAG (282%) and Ka (391%) makes the timing of
# the secondary rise for any one patient nearly arbitrary.
#
# Shafer's physiological view (2026-11-06) is that biliary flow is
# continuous.  Continuous secretion is the older linear form of
# enterohepatic recirculation (Colburn 1979, rat phenolphthalein: biliary
# transfer from the central compartment, a fixed lag for transit and
# deconjugation, reabsorption, and fecal loss), which is time-invariant.
# Bastian's k1B and Ka cannot be moved into it: as a continuous loop they
# give terminal half-lives of 20 and 32 h and concentrations 30-55% below the
# fitted curve at 2.5-12 h.  A continuous model would need refitting.
#
# CONCENTRATION BASIS
# ===================
# The assay measures platinum; the paper labels every concentration
# oxaliplatin (mg/L), says "oxaliplatin actually stands for all platinum
# derivatives in plasma", and computes AUC = dose in mg of oxaliplatin / CL.
# Doses are therefore mg of oxaliplatin and the curve is oxaliplatin
# equivalents, with no molar conversion (were the values platinum mass they
# would be about half, 195.1/397.3); the paper's own AUC arithmetic is what
# decides it.  Samples are plasma ULTRAFILTRATE, so the curve is the
# unbound (ultrafilterable) platinum, not total plasma or blood platinum.
#
# RENAL FUNCTION
# ==============
# The Patient Profile's serum creatinine, through adultEquivalentCreatinine()
# (fitted in adults; a child's is put on the adult scale), converted to
# umol/L at SCR_UMOL_PER_MG_DL (88.4); blank means the library's assumed
# normal creatinine, 0.9 or 0.8 mg/dL (79.6 or 70.7 umol/L), so a blank
# field gives a man a clearance 5% above the paper's 87-umol/L typical.  The
# power is negative, so the creatinine is held at or above the cohort's
# lowest, 53 umol/L (OXALIPLATIN_MIN_SCR), where the factor is 1.33; above
# the highest, 567 umol/L, the power is extrapolated (finite).
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# The model carries its own weight covariate, centred on 71 kg, evaluated at
# the pharmacokinetic weight (pkSizeFactors()$pkWeight) with the switch on
# and at total weight with it off.  V2 has no covariate.  Age, BSA,
# creatinine clearance, serum proteins and infusion length were tested and
# not retained.
#
# PREGNANCY (R/pregnancy.R)
# =========================
# No human pharmacokinetics of oxaliplatin in pregnancy found (PubMed,
# 2026-11-06); recorded `unknown`.  The clearance's creatinine term is an
# empirical power, not a filtration estimate, so the rise in filtration of
# pregnancy is not applied.
#
# NO EFFECT SITE, NO BAND
# =======================
# Plasma only; no therapeutic range (inst/extdata/therapeuticRanges.csv).
# -----------------------------------------------------------------------------

# Bastian 2003, Table 3: clearances in L/h, volumes in L, rate constants /h
OXALIPLATIN_BASTIAN <- list(
  cl = 14.1, v1 = 24.9, q = 34.8, v2 = 136,
  thetaBwCl = 1.10, thetaScrCl = -0.57, femaleCl = 0.60,
  thetaBwV1 = 1.29, thetaBwQ = 1.01,
  k1B = 1.78, tlag = 2.00, ka = 0.23,
  bwRef = 71,      # kg, the cohort's median
  scrRef = 87      # umol/L, the cohort's median
)

#' The lowest serum creatinine Bastian 2003 studied, umol/L
#'
#' The clearance's creatinine term is a negative power, so the creatinine is
#' held at or above it.
#' @keywords internal
OXALIPLATIN_MIN_SCR <- 53

#' Oxaliplatin, intravenous, in adults (Bastian 2003)
#'
#' Two compartments with weight, serum creatinine and sex on clearance and a
#' gallbladder store that fills from the central compartment until 2 h after
#' each course starts and then returns its content through the gut; see the
#' header of \code{R/drugs_oxaliplatinBastian.R}.
#'
#' @inheritParams cefazolin
#' @param creatinine serum creatinine, mg/dL, or NULL for an assumed normal
#'   value
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
oxaliplatinBastian <- function(weight, height, age, sex, adjustToFFM = TRUE,
                        creatinine = NULL)
{
  p <- OXALIPLATIN_BASTIAN
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume = 1, legacyClearance = 1)
  w <- (if (isTRUE(adjustToFFM)) size$pkWeight else weight) / p$bwRef
  scr <- max(adultEquivalentCreatinine(creatinine, age, sex) * SCR_UMOL_PER_MG_DL,
             OXALIPLATIN_MIN_SCR)
  female <- if (sex == SEX_FEMALE) p$femaleCl else 1

  default <- list(
    v1  = p$v1 * w^p$thetaBwV1,
    v2  = p$v2,
    v3  = 1,                                 # placeholder: two compartments
    cl1 = p$cl * w^p$thetaBwCl * (scr / p$scrRef)^p$thetaScrCl * female / 60,
    cl2 = p$q * w^p$thetaBwQ / 60,
    cl3 = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Bastian G, Barrail A, Urien S. Population pharmacokinetics of oxaliplatin in ",
    "patients with metastatic cancer. Anticancer Drugs 2003;14:817-824. 56 adults, ",
    "plasma ultrafilterable platinum as oxaliplatin: two compartments, CL 14.1 L/h x ",
    "(BW/71)^1.10 x (SCr/87 umol/L)^-0.57 x 0.60 if female (creatinine entered or ",
    "assumed normal, held at 53 umol/L or above), V1 24.9 L x (BW/71)^1.29, Q 34.8 L/h ",
    "x (BW/71)^1.01, V2 136 L; a gallbladder store filled at k1B 1.78 /h until TLAG ",
    "2 h after each course starts, then returned through the gut at Ka 0.23 /h, as ",
    "published (Plusquellec & Houin 1992's equations would keep filling it). ",
    "https://doi.org/10.1097/00001813-200311000-00007"
  )

  list(
    PK = PK,
    tPeak = 0,
    MEAC = 0,
    typical = 0,
    upperTypical = 0,
    lowerTypical = 0,
    biliaryStore = list(k1B = p$k1B / 60, tlag = p$tlag * 60, ka = p$ka / 60),
    reference = reference
  )
}
