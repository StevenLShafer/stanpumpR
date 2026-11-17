# -----------------------------------------------------------------------------
# Oxytocin: intravenous (Shafer 2025), plasma, LC/MS assay
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min, doses in
# mcg of oxytocin, concentrations in ng/mL, venous plasma.
#
# SOURCE
# ======
# Shafer SL, Ririe DG, Miller S, Curry RS, Hsu DT, Sullivan GM, Eisenach JC.
# Plasma pharmacokinetics of intravenous and intranasal oxytocin in
# nonpregnant adults.  Br J Anaesth 2025;134(5):1513-1522.  PMID 40121179,
# PMC12106900.  https://doi.org/10.1016/j.bja.2024.12.046
# Read in full from the archive's PDF (PMID_40121179.pdf), 2026-11-16, and
# its Pharmacokinetic Appendix (the supplement, supplied by Steven L. Shafer,
# 2026-11-17), whose table of eight "finalist" models A-H prints Model D as
# Table 1 does: CL1 0.86 x (Wt/80) x (Age/33)^-0.17, V1 8.2 x (Wt/80),
# CL2 0.22 x (Wt/80), V2 5.9 x (Wt/80); -2LL 46.1 below Model A (no
# covariates: CL1 0.8, V1 7.7, CL2 0.22, V2 5.6), MDAPE 18%.  Model G, the
# one the online simulator used, puts age and BMI on clearance alone
# (0.81 x (Age/33)^-0.14 x (BMI/25.8)^0.97, V1 7.73, CL2 0.22, V2 5.6;
# MDAPE 17%).  The appendix also reports that with the volumes and
# clearances scaled to weight there was no sign of a sex effect, which
# appeared only when clearance was scaled to BMI, and which the authors read
# as an artifact of BMI.
#
# Two studies of healthy men and nonpregnant women at Wake Forest, venous
# plasma assayed by a specific LC/MS method (limit 2 pg/mL):
#   Study 1, n = 11: 16.7 mcg IV over 60 s (3 subjects) or 10 min (8), sampled
#     to 240 min after the infusion; 38 (22-58) y, 55% women, 86 (SD 11) kg.
#   Study 2, n = 24, crossover: 13.7 mcg IV over 30 min, sampled to 90 min,
#     and on another day 100 mcg intranasal (TNX-1900, 25 mcg sprays, two at
#     0 and two at 5 min), sampled to 60 min; 40 (21-68) y, 50% women,
#     82 (SD 10) kg, BMI to 33.6.  All White and non-Hispanic.
# NONMEM 7.5, FOCE, two-compartment models.
#
# INTRAVENOUS (Table 1, "Model D", the model the paper presents)
# ===========
#     CL1 = 0.86 x (WT/80) x (AGE/33)^-0.17   L/min
#     V1  = 8.2  x (WT/80)                    L
#     CL2 = 0.22 x (WT/80)                    L/min
#     V2  = 5.9  x (WT/80)                    L
# Every parameter is proportional to weight, normalised to 80 kg; clearance
# falls with age.  Bias 0% and median absolute error 18%, the same out of
# sample (round-robin jackknife).  The paper's online simulator used Model G
# from the supplement when it was written; Model D is the one printed, and
# the one used here, on Steven L. Shafer's decision of 2026-11-16.
#
# INTRANASAL: NOT OFFERED
# ========================
# The paper also fits intranasal oxytocin (TNX-1900, an investigational
# product; no intranasal oxytocin is marketed in the United States): its own
# two-compartment disposition (CL1 0.75 L/min, V1 14 L, CL2 0.81 L/min,
# V2 8.6 L), ka 0.19 /min and an absolute bioavailability of 0.66%, with no
# covariates, poorly identified volumes and ka, and a median absolute error of
# 47%.  The route is not offered, on Steven L. Shafer's decision of
# 2026-11-16.
#
# DOSE BASIS
# ==========
# The marketed injection (Pitocin) is labelled and given in USP units, so
# doses are entered in units by default: a bolus in U, an infusion in U/hr,
# U/min or, as labour infusions are written, mU/min (Steven L. Shafer,
# 2026-11-16).  The paper doses in mcg, and the model is in mcg, so each dose
# in units is converted to micrograms before the engines read it, at
# OXYTOCIN_MCG_PER_USP_UNIT, the paper's own equivalence: its injection USP
# was 16.7 mcg/mL, labelled 10 units/mL, and it gives 100 mcg as 60 IU, both
# 600 units per mg (unitPotency; potencyToMass() in R/routes.R).  The mcg
# units stay offered, for the paper's doses.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# The intravenous model's own weight terms are evaluated at the
# pharmacokinetic weight with the switch on (size$pkWeight: 70 kg x FFM /
# 54.5 kg) and at total weight with it off, as for alprazolam.  The studied
# weights were about 60-110 kg, with no BMI above 33.6, so the published
# proportionality to total weight is an extrapolation in obesity, which the
# switch's default avoids.
#
# AGE
# ===
# (AGE/33)^-0.17 is infinite at age 0, so the age is held at
# ADULT_AGE_TERM_FLOOR (18) below it: the study enrolled adults aged 21 to 68.
# Children are outside the population studied.
#
# EFFECT SITE
# ===========
# The paper estimates no effect site.  tPeak is the library's earlier
# 5-minute value, a guess not from any source; the oxytocin effect that a
# clinician titrates (uterine tone) is not modelled.
#
# BAND
# ====
# 0.05 to 0.2 ng/mL, the library's earlier display band, whose source is not
# recorded (inst/extdata/therapeuticRanges.csv, library_band).  For scale,
# 16.7 mcg over 10 min peaks at 1.3 ng/mL in the reference man (70 kg,
# 170 cm, 35 y) and at 1.15 ng/mL in the paper's 80 kg, 33-year-old adult.
#
# A CHECK THE PAPER PRINTS
# ========================
# The Discussion compares the model with a study in nonpregnant women given
# 8.4 mcg IV over 30 s or 5 min (same LC/MS assay; weights not given in the
# paper): measured peaks 1000 and 700 pg/mL, the paper's simulations 900 and
# 680.  This function gives 0.99 and 0.75 ng/mL for an 80 kg, 33-year-old
# adult with the switch off, 10 to 11% above the paper's simulation.  With the
# switch on, a 70 kg, 165 cm, 30-year-old woman gets 1.41 and 1.06 ng/mL,
# because her pharmacokinetic weight is 56.4 kg: Model D has no sex term (the
# authors tested sex), and the fat-free-mass convention gives women smaller
# volumes at the same weight (docs/weight-adjustment.md, "Why the reference
# is a man").  That difference is the library's convention, not the paper's,
# and it is kept: Steven L. Shafer's decision of 2026-11-17 was to keep the
# fat-free-mass scaling for oxytocin, as for every other drug.
#
# PREGNANCY
# =========
# The source is nonpregnant.  Clearance at term is reported unchanged
# (Leake 1980, Takeda 1989: inst/extdata/pregnancyRegistry.csv); the paper's
# limitations cite faster metabolism in the third trimester (Oliver 2023,
# a study of ex vivo degradation in blood samples), so the registry records
# both and the model takes no pregnancy flag.
#
# RATS
# ====
# A weight of 1 kg or less selects the rat model of Tanaka 2018 (LC/MS, nasal
# delivery to the brain), kept from the earlier library unchanged, for
# research use.  Its parameters are not scaled.
#
# HISTORY
# =======
# Until 2026-11-16 the human model was "Eisenach (unpublished data)": V1
# 10.1 L, V2 7.03 L, CL1 0.974 and CL2 0.204 L/min, no covariates, described
# in this file as fitted in parturients.  Shafer 2025 replaces it, at the
# request of Steven L. Shafer, 2026-11-16.  Whether the unpublished data were
# an earlier analysis of Study 1 was not established.
# -----------------------------------------------------------------------------

# Shafer 2025, Table 1, intravenous (Model D); weights normalised to 80 kg
OXYTOCIN_REFERENCE_WEIGHT <- 80     # kg
OXYTOCIN_REFERENCE_AGE    <- 33     # years
OXYTOCIN_CL1   <- 0.86              # L/min at 80 kg, 33 y
OXYTOCIN_AGE_EXPONENT <- -0.17
OXYTOCIN_V1    <- 8.2               # L at 80 kg
OXYTOCIN_CL2   <- 0.22              # L/min at 80 kg
OXYTOCIN_V2    <- 5.9               # L at 80 kg

# Micrograms in one USP unit: 600 units per mg (the paper's 16.7 mcg = 10 U)
OXYTOCIN_MCG_PER_USP_UNIT <- 1000 / 600

#' Oxytocin pharmacokinetics (intravenous)
#'
#' Shafer et al. (2025), Model D: two compartments, every parameter
#' proportional to weight and clearance falling with age.  A weight of 1 kg or
#' less selects a rat model (Tanaka 2018).  See the file's header.
#'
#' @inheritParams cefazolin
#' @param adjustToFFM \code{TRUE} (the default) evaluates the model's weight
#'   terms at the pharmacokinetic weight; \code{FALSE} at total body weight.
#' @returns a list in the shape \code{getDrugPK()} expects
#' @keywords internal
oxytocin <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  if (weight <= 1) return(oxytocinRat())

  wt <- if (isTRUE(adjustToFFM)) {
    pkSizeFactors(weight, height, age, sex)$pkWeight
  } else weight
  w    <- wt / OXYTOCIN_REFERENCE_WEIGHT
  ageTerm <- (max(age, ADULT_AGE_TERM_FLOOR) / OXYTOCIN_REFERENCE_AGE)^
    OXYTOCIN_AGE_EXPONENT

  default <- list(
    v1  = OXYTOCIN_V1 * w,
    v2  = OXYTOCIN_V2 * w,
    v3  = 1,                          # placeholder: a two-compartment model
    cl1 = OXYTOCIN_CL1 * w * ageTerm,
    cl2 = OXYTOCIN_CL2 * w,
    cl3 = 0
  )

  reference <- paste0(
    "Shafer SL et al., Br J Anaesth 2025;134(5):1513-1522. ",
    "https://doi.org/10.1016/j.bja.2024.12.046 (intravenous, Table 1, ",
    "Model D: weight and age). tPeak 5 min is a guess, not from the paper."
  )

  list(
    PK = list(default = default),
    unitPotency = OXYTOCIN_MCG_PER_USP_UNIT,
    tPeak = 5,
    MEAC = 0,
    typical = 0.1,
    upperTypical = 0.2,
    lowerTypical = 0.05,
    reference = reference
  )
}

#' Oxytocin in the rat (Tanaka 2018)
#'
#' The research model a weight of 1 kg or less selects in \code{oxytocin()}.
#'
#' @returns a list in the shape \code{getDrugPK()} expects
#' @keywords internal
oxytocinRat <- function()
{
  list(
    PK = list(default = list(
      v1  = 0.10,      # L
      v2  = 0.02,      # L
      v3  = 1,         # L (no third compartment: cl3 is 0)
      cl1 = 0.017,     # L/min
      cl2 = 0.000524,  # L/min
      cl3 = 0
    )),
    tPeak = 5,         # guess
    MEAC = 0,
    typical = 0.1,
    upperTypical = 0.2,
    lowerTypical = 0.05,
    reference = paste0(
      "Tanaka A et al. Delivery of oxytocin to the brain for the treatment of ",
      "autism spectrum disorder by nasal application. Mol Pharm ",
      "2018;15(3):1105-1111. PMID 29338251."
    )
  )
}
