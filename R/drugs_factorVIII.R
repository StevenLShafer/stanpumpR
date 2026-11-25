# -----------------------------------------------------------------------------
# Factor VIII concentrate (standard half-life): two compartments, intravenous
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min, amounts in
# IU, activity in IU/dL (= % of normal).  Plotted as the patient's own factor
# VIII activity plus the infused activity (R/factorBaselines.R; Steven L.
# Shafer, 2026-11-25).  Drafted by Claude Code, 2026-11-25, at the request of
# Steven L. Shafer (docs/library-expansion/plan.csv, Biologics (surgery));
# digest card papers/digests/PMID_32326156.md.
#
# SOURCE
# ======
# Allard Q, Djerada Z, Pouplard C, et al.  Real life population pharmacokinetics
# modelling of eight factors VIII in patients with severe haemophilia A: is it
# always relevant to switch to an extended half-life?  Pharmaceutics
# 2020;12:380.  Read on the PDF.  258 patients (87 children and adolescents,
# 171 adults) from 13 French centres, 2012-2019; age 30 (3-77) years, weight 64
# (15.1-130) kg (Table 1); 244 severe, 11 moderate, 3 minor haemophilia A, none
# with an inhibitor.  Eight products: Advate 44, Kogenate 34, Refacto 18,
# Factane 8, Kovaltry 7, Novoeight 6, Afstyla 5 and the extended-half-life
# Elocta (Eloctate in the US) 136.  FVIII:C by the one-stage clotting assay,
# 935 levels, one to 11 per patient, 15 min to 96 h after a dose; Monolix SAEM.
#
# Table 2 and the equations under it, weight (kg) and age (years) about the
# medians:
#
#     CL = 204 mL/h x (age/30)^-0.214 x (weight/64)^0.75 x exp(-0.394 EHL)
#     V1 = 2640 mL x (weight/64)^0.827
#     Q  = 135 mL/h      V2 = 339 mL
#
# EHL is 1 for Elocta and 0 for the seven standard-half-life products, whose
# brand and structure (full-length, B-domain deleted, plasma-derived) were
# tested and not kept.  This drug is the standard-half-life concentrates,
# EHL = 0, the published model for all seven; Eloctate would be a drug of its
# own (EHL = 1), not added here.  The weight exponent on CL was fixed at 0.75
# (estimated 0.779).  Endogenous production was tested as a model term and not
# kept; the patients' predose levels were treated by simulating them to steady
# state on their prophylaxis (Methods 2.2).
#
# CHECKS (CL, V1, Q and V2 above, two compartments, at the medians, 64 kg and
# 30 years): CL 3.19 mL/kg/h, Vss 46.5 mL/kg, half-lives 1.5 h and 10.4 h.
# Advate's label (12.3, Table 6, 20 adults, 50 IU/kg): CL 3 +/- 1 mL/kg/h,
# Vss 0.4 dL/kg, half-life 12.0 +/- 4.2 h, incremental recovery 2.6 +/- 0.5
# IU/dL per IU/kg; this model's incremental recovery at the end of a bolus is
# 64/26.4 = 2.42 IU/dL per IU/kg, and the label's dosing rule is 2.
# Clearance and volume agree; the terminal half-life is shorter than the
# label's mean, within its SD.  In children the label's clearance per kg is
# higher than in adults (4.1-4.8 mL/kg/h, Table 7, against 3.6); this model
# gives 6.0 mL/kg/h at 20 kg and 6 years, higher still.  Neither is
# corrected (only clearance is real, and the source is a real-life cohort
# sampled sparsely, mostly after the distribution phase).
#
# BASELINE: the patient's own activity is the Patient Profile's Factor VIII
# baseline (IU/dL), added to the infused activity.  Blank, it is the source
# population's: severe haemophilia A, below 1 IU/dL (Table 1; the authors
# took an unknown predose level as below the 0.01 IU/mL limit of
# quantification), entered as 0.5 IU/dL, the middle of that interval (a
# convention, not a measured value; Shafer's decision of 2026-11-25 that a
# blank field takes the source population's baseline).
#
# BAND: the perioperative target of the label for major surgery, 80 to 120
# IU/dL before and after the operation, the typical 100 (ADVATE prescribing
# information, Table 2); minor surgery's is 60 to 100.  Total activity, which
# is what the curve shows.
#
# AGE: the negative power of age is held at 3 years, the youngest patient the
# source studied (AGENTS.md, Age terms: the source fitted children from 3).
#
# BODY SIZE: the source's own weight terms on CL and V1, kept as fitted,
# evaluated at the pharmacokinetic weight with the switch on and at total
# weight with it off.
#
# BODY SIZE ANCHOR: Q and V2 have no size term and take the fat-free-mass
# factors, anchored at the reference man rather than at the source's cohort:
# the cohort's height is not printed and needs NCD-RisC's mean height for
# France (birth cohort about 1986), whose file could not be reached from this
# session (papers/pending-work.csv).  The cohort's typical patient (64 kg, 30
# years, a man) is close to the reference man, so the difference is small.
#
# RENAL FUNCTION: none; factor VIII is not cleared by the kidney.
#
# PREGNANCY: no pharmacokinetic study of factor VIII concentrate in
# pregnancy (inst/extdata/pregnancyRegistry.csv).  Endogenous factor VIII
# rises two- to threefold in late pregnancy; that is the baseline field's to
# carry, not this model's.
#
# References
# ----------
# Allard Q et al., Pharmaceutics 2020;12:380.
#   https://doi.org/10.3390/pharmaceutics12040380
# ADVATE prescribing information, BLA 125063, revised 4/2025.
# -----------------------------------------------------------------------------

FACTOR_VIII_CL         <- 204      # mL/h at age 30 and 64 kg
FACTOR_VIII_CL_AGE     <- -0.214
FACTOR_VIII_CL_WEIGHT  <- 0.75     # fixed in the source
FACTOR_VIII_V1         <- 2640     # mL at 64 kg
FACTOR_VIII_V1_WEIGHT  <- 0.827
FACTOR_VIII_Q          <- 135      # mL/h
FACTOR_VIII_V2         <- 339      # mL
FACTOR_VIII_REF_WEIGHT <- 64       # kg, the source's median
FACTOR_VIII_REF_AGE    <- 30       # years, the source's median
FACTOR_VIII_MIN_AGE    <- 3        # years, the youngest the source studied
# The source population's baseline, severe haemophilia A (below 1 IU/dL)
FACTOR_VIII_BASELINE   <- 0.5      # IU/dL

#' Factor VIII concentrate (Allard 2020)
#'
#' Two compartments, the seven standard-half-life concentrates of Allard
#' 2020, plotted as the patient's own factor VIII activity plus the infused
#' activity, in IU/dL; see the header of \code{R/drugs_factorVIII.R}.
#'
#' @inheritParams cefazolin
#' @param factorVIIIBaseline the patient's own factor VIII activity in IU/dL,
#'   or NULL for 0.5 IU/dL, a chosen default within the source population's
#'   severe haemophilia A (below 1 IU/dL)
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
factorVIII <- function(weight, height, age, sex, adjustToFFM = TRUE,
                       factorVIIIBaseline = NULL)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume = 1, legacyClearance = 1)
  pkW  <- if (isTRUE(adjustToFFM)) size$pkWeight else weight
  ageTerm <- (max(age, FACTOR_VIII_MIN_AGE) / FACTOR_VIII_REF_AGE)^FACTOR_VIII_CL_AGE

  cl <- FACTOR_VIII_CL * ageTerm * (pkW / FACTOR_VIII_REF_WEIGHT)^FACTOR_VIII_CL_WEIGHT
  v1 <- FACTOR_VIII_V1 * (pkW / FACTOR_VIII_REF_WEIGHT)^FACTOR_VIII_V1_WEIGHT
  q  <- FACTOR_VIII_Q  * size$clearance
  v2 <- FACTOR_VIII_V2 * size$volume

  default <- list(
    v1  = v1 / 1000, v2 = v2 / 1000, v3 = 1,
    cl1 = cl / 1000 / 60, cl2 = q / 1000 / 60, cl3 = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  baseline <- if (is.null(factorVIIIBaseline)) FACTOR_VIII_BASELINE else factorVIIIBaseline

  reference <- paste0(
    "Allard 2020 (258 patients with haemophilia A, 3-77 years, eight concentrates): ",
    "two compartments, CL 204 mL/h x (age/30)^-0.214 x (weight/64)^0.75, ",
    "V1 2640 mL x (weight/64)^0.827, Q 135 mL/h, V2 339 mL; the standard-half-life ",
    "concentrates (Elocta excluded). Plotted as the patient's own factor VIII ",
    "activity plus the infused: baseline ", signif(baseline, 3), " IU/dL",
    if (is.null(factorVIIIBaseline)) " (blank: severe haemophilia A, the source population)" else "",
    ". Band: the label's perioperative target for major surgery, 80-120 IU/dL. ",
    "https://doi.org/10.3390/pharmaceutics12040380")

  list(
    PK = PK, tPeak = 0, MEAC = 0,
    typical = 100, upperTypical = 120, lowerTypical = 80,
    reference = reference,
    endogenousActivity = baseline
  )
}
