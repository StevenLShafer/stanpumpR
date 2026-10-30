# -----------------------------------------------------------------------------
# Levodopa, oral, with carbidopa 4:1 (Sinemet and its generics): two
# compartments, apparent oral, absorbed through one transit compartment, with
# an effect site for the motor response
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in mcg/mL (= mg/L) of levodopa.
#
# Drafted by Claude Code, 2026-10-19, at the request of Steven L. Shafer, who
# asked for the drugs used to treat Parkinson's disease with their times of
# peak effect and therapeutic ranges (inst/help/models/parkinsons.md,
# inst/extdata/parkinsonsRegistry.csv).
#
# THE PLASMA MODEL
# ================
# Othman AA, Dutta S.  Population pharmacokinetics of levodopa in subjects
# with advanced Parkinson's disease: levodopa-carbidopa intestinal gel
# infusion vs. oral tablets.  Br J Clin Pharmacol 2014;78:94-105 (PMC4168384),
# read in full, and its reproduction in the FDA Clinical Pharmacology Review
# of NDA 203952 (Duopa, 2015, pp. 25-29), read in full: 68 patients with
# advanced Parkinson's disease (45 on the intestinal gel, 23 on tablets),
# 1182 concentrations; mean age 64 (39-83), weight 73 kg (45-148); carbidopa
# 4:1 throughout (Sinemet 25/100 over-encapsulated).  Two compartments "with
# a transit compartment for absorption" (review p. 26), Table 2 / the
# review's table:
#
#     KTR   2.4 /h (tablets), 9.2 /h (intestinal gel)
#     CL/F  24.8 L/h          Vc/F 58.5 L x WT/70
#     Q/F   6.8 L/h           Vp/F 72.9 L
#     Frel  1 (tablets, fixed), 0.97 (gel)
#
# Clearance had no weight or sex term (p > 0.01), entacapone no effect, and
# age "almost reached significance" (p = 0.0057) and was not included.  The
# intestinal gel is R/drugs_levodopaIntestinalGel.R.
#
# THE ABSORPTION: ONE TRANSIT COMPARTMENT
# =======================================
# The dose passes from its dosing compartment through one transit
# compartment into the central compartment, both at KTR: the gamma input of
# two equal first-order steps.  The paper's text calls 1/KTR the "mean
# absorption time" (25 min), which would be one step, but its own simulated
# typical peaks after a 200 mg dose, 75 min (tablets) and 45 min (gel), are
# produced only by two (one step gives 48 min for tablets), and Senek 2020
# describes the model as "one transit compartment between the depot and
# central compartment".  Two steps are drawn, exactly, by transit_PO
# (R/transit.R); the tests pin the 75 minutes.
#
# THE EFFECT SITE: MAO 2013
# =========================
# Mao Z, Hsu A, Gupta S, Modi NB.  Population pharmacodynamics of IPX066 ...
# and immediate-release carbidopa-levodopa in patients with advanced
# Parkinson's disease.  J Clin Pharmacol 2013;53:523-531 (PMC3798100), read
# in full: 27 patients with advanced disease (Hoehn and Yahr 2-3) on IR
# carbidopa-levodopa and IPX066, the effect models driven by each patient's
# OBSERVED plasma levodopa, so they are independent of any plasma model and
# may be placed on this one.  UPDRS Part III (Table 1): ke0 1.80 /h
# (equilibration half-life 0.39 h; the abstract's "0.4 hours"), EC50
# 812 ng/mL, Hill 2.5, maximal improvement 63% of baseline.  Tapping:
# ke0 1.17 /h, EC50 1590 ng/mL; dyskinesia: ke0 1.55 /h, EC50 601 ng/mL.
# The effect site drawn is the motor (UPDRS III) one, supplied as ke0 so that
# getDrugPK() solves the time to peak effect after a bolus against this
# disposition; after a tablet the effect peaks about 40 minutes after the
# plasma (tests).  The onset of a 15% improvement in Mao's patients was
# 0.46 h after an IR dose.  The equilibration is DISEASE-STAGE dependent:
# untreated and early patients have a long-duration response lasting days
# and slower equilibration (Contin 1998: the equilibration half-life falls
# by about 17% a year; Triggs 1996, 173 min at Hoehn and Yahr 1 against 43
# at stage 4, abstracts), so this ke0 is that of advanced, fluctuating
# patients, the ones whose dosing is timed to the effect.
#
# THE TIME UNTIL THRESHOLD
# ========================
# endCe in drugDefaults_global.csv is Mao's UPDRS EC50, 0.81 mcg/mL: with
# "time until threshold" on, the plot shows how long until the motor effect
# site falls below the concentration that gives half the maximal improvement.
# That is a modelled half-maximal UPDRS III response, not a measured ON/OFF
# boundary; it is the model's nearest estimate of when the dose wears off.
#
# THE BAND: 1.0 to 3.5 mcg/mL, 0.5 to 1 h after a dose, the reference range of
# the AGNP consensus guideline's update 2026 (Hart 2026, Pharmacopsychiatry
# 59:166-295, Table 5, read in full; marked + as revised: the 2017 update,
# Hiemke 2018, gave 0.9 to 2.0 mcg/mL 1 h after 250/25 mg).  It is the
# expected concentration after 100 mg of standard levodopa with 25 mg of a
# decarboxylase inhibitor, not an efficacy window; laboratory alert level
# 5 mcg/mL (Fabbrini 1987), level of recommendation 2.  Typical 2.25, the
# midpoint, not a target.
#
# WHAT IS NOT IN THE MODEL
# ========================
# Carbidopa dose (the model is valid at 4:1 only; carbidopa lowers levodopa
# clearance and saturates the peripheral decarboxylase at 70-100 mg/day),
# benserazide (Madopar), COMT inhibitors (entacapone raises levodopa AUC
# about 35%, more in COMT Val/Val patients: 62% against 34% in Met/Met,
# Corvol 2011), protein meals and gastric emptying (the source of the
# erratic absorption of advanced disease, IIV on KTR 88%), and sex (women's
# exposure 27-40% higher, much of it weight: Contin 2022, labels).  COMT
# Val158Met genotype has no measurable effect on levodopa's own kinetics
# (Contin 2005).
#
# BODY SIZE (docs/weight-adjustment.md): the model's own size covariate is
# weight on Vc, exponent 1, evaluated at the pharmacokinetic weight with the
# fat-free-mass switch on and at total body weight with it off; the
# size-free CL, Q and Vp are scaled by the library's factors, which are 1
# with the switch off (the published model).
#
# DOSE BASIS: mg of levodopa, as labelled ("carbidopa-levodopa 25/100" is
# 100 mg of levodopa).
#
# References
# ----------
# Othman AA, Dutta S. Br J Clin Pharmacol 2014;78:94-105.
#   https://doi.org/10.1111/bcp.12324
# FDA CDER, Clinical Pharmacology Review, NDA 203952 (Duopa).
#   https://www.accessdata.fda.gov/drugsatfda_docs/nda/2015/203952Orig1s000ClinPharmR.pdf
# Mao Z et al. J Clin Pharmacol 2013;53:523-531.
#   https://doi.org/10.1002/jcph.63
# -----------------------------------------------------------------------------

# Othman & Dutta 2014, Table 2 (and the FDA review of NDA 203952)
LEVODOPA_CL      <- 24.8    # L/h
LEVODOPA_VC      <- 58.5    # L at 70 kg, x WT/70
LEVODOPA_Q       <- 6.8     # L/h
LEVODOPA_VP      <- 72.9    # L
LEVODOPA_KTR_ORAL <- 2.4    # /h, tablets
LEVODOPA_KTR_GEL  <- 9.2    # /h, intestinal gel
LEVODOPA_FREL_GEL <- 0.97   # gel relative to tablets
# Mao 2013, Table 1: UPDRS Part III
LEVODOPA_KE0       <- 1.80  # /h
LEVODOPA_UPDRS_EC50 <- 0.812 # mcg/mL

#' Levodopa disposition shared by the tablets and the intestinal gel
#'
#' Othman & Dutta 2014's two-compartment apparent model; see the header of
#' \code{R/drugs_levodopa.R}.
#'
#' @inheritParams cefazolin
#' @returns a PK set without absorption fields
#' @keywords internal
levodopaDisposition <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume = 1, legacyClearance = 1)
  w <- if (isTRUE(adjustToFFM)) size$pkWeight else weight
  list(
    v1  = LEVODOPA_VC * w / 70,
    v2  = LEVODOPA_VP * size$volume,
    v3  = 1,
    cl1 = LEVODOPA_CL * size$clearance / 60,
    cl2 = LEVODOPA_Q * size$clearance / 60,
    cl3 = 0
  )
}

#' Levodopa, oral tablets with carbidopa 4:1
#'
#' Two compartments, apparent (/F), absorbed through one transit compartment
#' (Othman & Dutta 2014), with the motor effect site of Mao 2013.  See the
#' header of \code{R/drugs_levodopa.R}.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
levodopa <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  default <- c(levodopaDisposition(weight, height, age, sex, adjustToFFM), list(
    ka_PO = LEVODOPA_KTR_ORAL / 60,
    bioavailability_PO = 1,          # apparent parameters; tablets the reference
    tlag_PO = 0,
    transit_PO = TRUE
  ))
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  list(
    PK = PK, ke0 = LEVODOPA_KE0 / 60, MEAC = 0,
    typical = 2.25, upperTypical = 3.5, lowerTypical = 1.0,
    reference = paste0(
      "Othman & Dutta, Br J Clin Pharmacol 2014;78:94-105 (advanced Parkinson's ",
      "disease, carbidopa 4:1): two compartments, CL/F 24.8 L/h, Vc/F 58.5 L x ",
      "weight/70, Q/F 6.8 L/h, Vp/F 72.9 L, absorbed through one transit ",
      "compartment at 2.4/h; motor (UPDRS III) effect site ke0 1.80/h, EC50 ",
      "0.81 mcg/mL (Mao 2013, J Clin Pharmacol 53:523). Band 1.0-3.5 mcg/mL ",
      "(AGNP update 2026, 0.5-1 h after a dose). https://doi.org/10.1111/bcp.12324"
    )
  )
}
