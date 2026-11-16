# -----------------------------------------------------------------------------
# Oxytocin: intravenous and intranasal (Shafer 2025), plasma, LC/MS assay
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
# Read in full from the archive's PDF (PMID_40121179.pdf), 2026-11-16.  The
# Supplementary material (the candidate models A-G and the NONMEM control
# streams) is not in the archive and was not read.
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
# the one used here.
#
# INTRANASAL (Table 1)
# ==========
# Fitted after the intravenous model, in two steps: with each subject's
# intravenous disposition the absolute bioavailability was estimated (0.66%);
# that fit was poor, so F was fixed at its population value and a disposition
# of its own was estimated with the absorption, again two compartments:
#     CL1 0.75 L/min, V1 14 L, CL2 0.81 L/min, V2 8.6 L,
#     ka 0.19 /min, F 0.66% (absolute), no lag
# with no covariates (none improved the fit).  The authors report the volumes
# and ka as poorly identified (irregular likelihood profiles), CL1, CL2 and F
# as well identified, and the model as imprecise (bias +9%, median absolute
# error 47%).
#
# The two routes are therefore two parallel systems (getDrugPK(), as for
# meloxicam): the drug's own system is the intravenous model and takes only
# intravenous doses; the intranasal model takes only intranasal doses; the
# plotted concentration is their sum.  This is how the paper's own simulator
# runs the intranasal route.  Using the intravenous disposition with the
# intranasal ka and F instead would mix the two steps of the fit: the
# intranasal clearance is 0.75, not 0.86 L/min at 80 kg, so exposure would be
# about 13% lower than the published intranasal model gives.
#
# The intranasal product studied (TNX-1900) is investigational; no
# intranasal oxytocin is marketed in the United States.  The route is offered
# because intranasal oxytocin is the subject of most oxytocin research, which
# is what this drug is in the library for.
#
# DOSE BASIS
# ==========
# Doses are mcg of oxytocin, as the paper doses (the injection USP was
# 16.7 mcg/mL, the labelled 10 USP units/mL).  The marketed injection
# (Pitocin) is labelled in USP units; the library does not offer units.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# The intravenous model's own weight terms are evaluated at the
# pharmacokinetic weight with the switch on (size$pkWeight: 70 kg x FFM /
# 54.5 kg) and at total weight with it off, as for alprazolam.  The studied
# weights were about 60-110 kg, with no BMI above 33.6, so the published
# proportionality to total weight is an extrapolation in obesity, which the
# switch's default avoids.  The intranasal model has no size covariate: its
# values are taken as the 70 kg reference man's and scaled by
# pkSizeFactors() with the switch on, and used as published with it off
# (legacy factors 1).
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
# is a man").  That difference is the library's convention, not the paper's.
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

# Shafer 2025, Table 1, intranasal: its own disposition, no covariates
OXYTOCIN_IN_CL1 <- 0.75             # L/min
OXYTOCIN_IN_V1  <- 14               # L
OXYTOCIN_IN_CL2 <- 0.81             # L/min
OXYTOCIN_IN_V2  <- 8.6              # L
OXYTOCIN_IN_KA  <- 0.19             # 1/min
OXYTOCIN_IN_F   <- 0.0066           # absolute (0.66%)

#' Oxytocin pharmacokinetics (intravenous and intranasal)
#'
#' Shafer et al. (2025): two compartments, every parameter proportional to
#' weight and clearance falling with age, for intravenous doses; the paper's
#' separate intranasal model (absolute bioavailability 0.66%) for intranasal
#' doses, run as a parallel system.  A weight of 1 kg or less selects a rat
#' model (Tanaka 2018).  See the file's header.
#'
#' @inheritParams cefazolin
#' @param adjustToFFM \code{TRUE} (the default) evaluates the intravenous
#'   model's weight terms at the pharmacokinetic weight and scales the
#'   intranasal model by fat-free mass; \code{FALSE} uses total body weight
#'   and the intranasal model as published.
#' @returns a list in the shape \code{getDrugPK()} expects
#' @keywords internal
oxytocin <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  if (weight <= 1) return(oxytocinRat())

  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume = 1, legacyClearance = 1)
  wt   <- if (isTRUE(adjustToFFM)) size$pkWeight else weight
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

  intranasal <- list(
    v1  = OXYTOCIN_IN_V1  * size$volume,
    v2  = OXYTOCIN_IN_V2  * size$volume,
    v3  = 1,                          # placeholder: a two-compartment model
    cl1 = OXYTOCIN_IN_CL1 * size$clearance,
    cl2 = OXYTOCIN_IN_CL2 * size$clearance,
    cl3 = 0,
    ka_IN = OXYTOCIN_IN_KA,
    bioavailability_IN = OXYTOCIN_IN_F,
    tlag_IN = 0
  )

  reference <- paste0(
    "Shafer SL et al., Br J Anaesth 2025;134(5):1513-1522. ",
    "https://doi.org/10.1016/j.bja.2024.12.046 (intravenous: Table 1, ",
    "Model D, weight and age; intranasal: Table 1, its own two-compartment ",
    "model, F 0.66%, run on intranasal doses only). tPeak 5 min is a guess, ",
    "not from the paper."
  )

  list(
    PK = list(default = default),
    routes = ROUTE_IV,
    parallelSystems = list(
      list(name = "Intranasal", routes = ROUTE_IN,
           PK = list(default = intranasal))
    ),
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
