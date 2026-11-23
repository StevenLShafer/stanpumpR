# -----------------------------------------------------------------------------
# Suzetrigine (Journavx): two compartments, oral, with enzyme autoinduction,
# forming its active metabolite M6-SUZ, from the FDA Integrated Review of NDA
# 219209 (the applicant's population model 2822)
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min; amounts in
# mcg, so the plotted concentrations are ng/mL (= mcg/L), total plasma.  The
# metabolite is its own drug, M6SUZ (R/drugs_M6SUZ.R).
#
# Drafted by Claude Code, 2026-11-16, at the request of Steven L. Shafer, who
# asked for suzetrigine and then for its autoinduction to be built ("Build the
# autoinduction state for suzetrigine", 2026-11-16).  The numbers below were
# read on the page images of the FDA's Integrated Review of NDA 219209
# (Journavx, Reference ID 5521299): Figure 24 (the model's schematic, p.170),
# Table 148 (the data, p.168), Table 149 (the estimates, pp.171-172) and
# Table 150 (p.177), and in the Journavx label, read in full.
#
# THE SOURCE
# ==========
# No published population model exists (PubMed, 2026-11-15: reviews,
# physiologically based interaction studies and chemistry only).  The
# applicant's final model, 2822, was fitted to 19,788 concentrations of each
# moiety from 1,435 subjects in 12 studies (healthy volunteers, hepatic
# impairment, and patients after bunionectomy or abdominoplasty; median age
# 42, weight 76.2 kg (44-128), BMI 27.8 (18.1-39.7), 76.7% men; Table 148).
# Absolute bioavailability "has not been determined", so every parameter is
# apparent (/F).  Figure 24 and Table 149:
#
#   absorption: lag ALAG1 0.182 h, then a zero-order input into the depot
#     over D1TAB 1.1 h (the tablet), then first order at KA 2.02 /h x 2.24
#     (the tablet effect); tablet F1 0.88 relative to the suspension
#   suzetrigine (SUZ): central V2P 103 L, peripheral V3P 204 L, Q3P 19 L/h;
#     clearance CLP 5.56 L/h x 1.21 in men x (BMI/ref)^-0.605
#   M6-SUZ: "SUZ metabolism to M6-SUZ ... fixed at 80%" (FR, p.169): formed
#     at CLP FR AENZ, the rest, CLP (1 - FR) AENZ, eliminated otherwise;
#     central V2M 1.7 L, peripheral V3M 29.5 L, Q3M 8.75 L/h; cleared at
#     CLM AENZ with CLM 0.273 x CLP ("Metabolite clearance relative to CLP")
#   volumes x (weight/ref) (exponent 1, fixed)
#   AENZ, the autoinduction (compartment 6 of Figure 24):
#     dAENZ/dt = KENZ (1 + AMAX Cp^AHILL / (AC50^AHILL + Cp^AHILL))
#                - KENZ AENZ,   AENZ(0) = 1,
#     KENZ 0.00939 /h (turnover half-life 74 h), AMAX 0.811, AC50 432 ng/mL,
#     AHILL 10 (fixed), Cp the parent's central concentration
#
# CLM is a multiple of CLP, not 0.273 L/h: the steady-state ratio of the
# areas is then FR / 0.273 = 2.9, the label's "approximately 3-fold" (34.7
# against 11.5 ug.h/mL), where 0.273 L/h would give about 16-fold.
#
# Figure 24 multiplies all three clearances by AENZ.  The reviewer's prose
# (p.188) calls the parent's clearances "parallel linear"; the schematic is the
# applicant's model and is followed.
#
# THE ENGINE
# ==========
# The clearances rise with the enzyme the drug induces, so the parent and the
# metabolite are advanced together by the saturable engine
# (R/advanceMichaelisMenten.R): vmax 0, the whole parent clearance its
# autoinduction clearance, with the induction's Hill coefficient, the
# metabolite formed from what the parent eliminates and cleared by the
# induced enzyme too, and the tablet's zero-order release into the depot.
# Without the induction the curve is within 4% for 48 hours but 31-55% high by
# day 14 (100 mg then 50 mg every 12 h; the research report of 2026-11-15).
# The FDA's own table shows it: apparent clearance 7.01 L/h after one dose,
# 13.9 L/h at day 14 (Table 150).
#
# DOSE DEPENDENCE (FORMS ASSUMED)
# ===============================
# Table 149 prints the parameters of two dose effects but not their
# equations: on F1, D50 162 mg, maximal effect 0.419, Hill 5 (fixed); on KA,
# D50 36 mg, maximal effect 0.895, Hill 1 (fixed).  They are applied in the
# usual form of such parameters, a fall of Emax D^h / (D50^h + D^h):
#
#     F  = 0.88 (1 - 0.419 D^5 / (162^5 + D^5))     (oralSaturation, hill 5)
#     KA = 4.52 (1 - 0.895 D / (36 + D)) /h          (kaDoseDependence)
#
# with D the dose in mg.  At the labelled doses F is 0.88 (50 mg) and 0.85
# (100 mg), and KA 2.17 and 1.55 /h.  The forms are an assumption, checked
# only by the agreement with Table 150 below.  The review also mentions a dose
# effect on D1 with no parameter printed; none is applied.
#
# THE REFERENCE PATIENT (ASSUMED)
# ===============================
# The review prints no reference weight or BMI for the covariate terms.  The
# data's medians are used, 76.2 kg and 27.8 kg/m2 (Table 148); the FDA
# reviewer's "typical subject" was 73.3 kg and 27.7.  CLP's 5.56 L/h is the
# woman's (the man's effect is 1.21).
#
# CHECKS
# ======
# Table 150 is the applicant's simulation of this model (100 mg, then 50 mg
# every 12 h, pooled patients), so it checks the implementation, not the
# model: SUZ AUC0-48 15.8 ug.h/mL, Cmax 0.558 ug/mL over the first 48 h;
# steady-state AUC0-24 11.5, Cmax 0.623; M6-SUZ steady-state AUC0-24 34.7,
# Cmax 1.52; apparent clearance 7.01 L/h after a single dose and 13.9 at day
# 14 (a mean of individual ratios).  test-drugs-suzetrigine.R holds this
# model to them.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# The source's own covariates are kept exactly as fitted: weight on every
# volume (V2P, V3P, V2M, V3M; exponent 1), at the pharmacokinetic weight with
# the switch on and total weight with it off; BMI (and sex) on clearance, from
# total weight and height in both positions, because it stands for body
# composition, not size (a heavier patient of the same height clears
# suzetrigine MORE slowly).  BMI is held within the range studied,
# 18.1-39.7 kg/m2.  CLM is 0.273 x CLP and takes nothing more.  None of these
# is anchored.
#
# The two intercompartmental clearances, Q3P 19 L/h and Q3M 8.75 L/h, carry no
# size term in the model as transcribed here (Table 149 could not be read
# again on 2026-11-19: the review is not archived).  The data's weights ran
# from 44 to 128 kg, so the absence of a weight term says only that none was
# fitted, not that these clearances are insensitive to size: under Steven L.
# Shafer's first-principles rule of 2026-11-19 (a covariate not detected is
# not evidence of no size effect), with the switch on they are scaled
# allometrically, x (FFM ratio)^0.75, anchored at the data's typical subject
# (SUZETRIGINE_COHORT, below): the library's clearance factor divided by that
# subject's, 0.9833 (volume factor 0.9778), so that the data's typical subject
# receives the published 19 and 8.75 L/h.  Neither has a sex term, so the
# anchor is the cohort's pooled subject (the man's and the woman's factors at
# the medians, weighted 76.7 : 23.3, averaged geometrically).  With the switch off
# they are 19 and 8.75 L/h for everyone, as before.
#
# DOSE
# ====
# Journavx tablets, 50 mg of suzetrigine (no salt); 100 mg first, then 50 mg
# every 12 hours, fasting for the first dose.  Food (Tmax 5 h), hepatic
# impairment and CYP3A inhibitors and inducers are not modelled.  The
# abdominoplasty patients' first-dose absorption (KAD, D1D, F1D) is not used.
#
# PREGNANCY AND THE BAND
# ======================
# No pregnancy pharmacokinetics (registry row).  No therapeutic range.
# -----------------------------------------------------------------------------

# FDA Integrated Review of NDA 219209, Table 149 (model 2822)
SUZETRIGINE_CL      <- 5.56    # L/h, women, at the reference BMI
SUZETRIGINE_CL_MALE <- 1.21
SUZETRIGINE_CL_BMI  <- -0.605
SUZETRIGINE_BMI_REF <- 27.8    # kg/m2, the data's median (assumed)
SUZETRIGINE_BMI_RANGE <- c(18.1, 39.7)   # studied (Table 148)
SUZETRIGINE_WT_REF  <- 76.2    # kg, the data's median (assumed)
SUZETRIGINE_V1      <- 103     # L, V2P (central)
SUZETRIGINE_V2      <- 204     # L, V3P (peripheral)
SUZETRIGINE_Q       <- 19      # L/h, Q3P
SUZETRIGINE_KA      <- 2.02 * 2.24   # /h, the tablet
SUZETRIGINE_F       <- 0.88    # tablet relative to suspension
SUZETRIGINE_TLAG    <- 0.182   # h
SUZETRIGINE_D1      <- 1.1     # h, the tablet's zero-order release
SUZETRIGINE_FR      <- 0.80    # fraction of the parent's clearance forming M6-SUZ
SUZETRIGINE_KENZ    <- 0.00939 # /h
SUZETRIGINE_AMAX    <- 0.811
SUZETRIGINE_AC50    <- 432     # ng/mL
SUZETRIGINE_AHILL   <- 10
SUZETRIGINE_F_DOSE  <- list(Imax = 0.419, ID50 = 162, hill = 5)   # mg
SUZETRIGINE_KA_DOSE <- list(emax = 0.895, d50 = 36, hill = 1)     # mg

# The data's typical subject, the anchor of the size-free Q3P and Q3M (see the
# header's BODY SIZE): Table 148 (p.168) of the review as transcribed in this
# header on 2026-11-16 -- 1,435 subjects, median age 42 years, weight 76.2 kg,
# BMI 27.8 kg/m2, 76.7% men -- unconfirmed, because the review is not archived
# and could not be read again on 2026-11-19.  No height is printed: it is the
# median weight over the median BMI, 100 x sqrt(76.2 / 27.8) = 165.6 cm (two
# medians, not one subject's).  The sexes are weighted 76.7 : 23.3.
SUZETRIGINE_COHORT <- list(weight = 76.2, bmi = 27.8, age = 42,
                           men = 76.7, women = 23.3)

#' Suzetrigine's apparent clearance before induction (Table 149)
#'
#' CLP x 1.21 in men x (BMI / 27.8)^-0.605, the BMI from total weight and
#' height held within the range studied.  Shared with \code{M6SUZ()}, whose
#' clearance is 0.273 times this.
#'
#' @inheritParams cefazolin
#' @returns L/h
#' @keywords internal
suzetrigineClearance <- function(weight, height, sex)
{
  bmi <- min(max(weight / (height / 100)^2, SUZETRIGINE_BMI_RANGE[1]),
             SUZETRIGINE_BMI_RANGE[2])
  SUZETRIGINE_CL * (if (sex == SEX_MALE) SUZETRIGINE_CL_MALE else 1) *
    (bmi / SUZETRIGINE_BMI_REF)^SUZETRIGINE_CL_BMI
}

#' The weight the volumes scale with: the pharmacokinetic weight with the
#' switch on, total weight with it off
#' @keywords internal
suzetrigineVolumeFactor <- function(weight, height, age, sex, adjustToFFM)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)
  (if (isTRUE(adjustToFFM)) size$pkWeight else weight) / SUZETRIGINE_WT_REF
}

#' The factor of the size-free intercompartmental clearances, Q3P and Q3M
#'
#' With the switch on, the fat-free-mass ratio to the 0.75 power anchored at
#' the data's typical subject (\code{SUZETRIGINE_COHORT}); 1 with it off.
#' Shared with \code{M6SUZ()}.
#'
#' @inheritParams cefazolin
#' @returns a multiplier
#' @keywords internal
suzetrigineQFactor <- function(weight, height, age, sex, adjustToFFM)
{
  pkSizeFactors(weight, height, age, sex, adjustToFFM,
                legacyVolume = 1, legacyClearance = 1,
                cohort = do.call(cohortSizeFactors, SUZETRIGINE_COHORT))$clearance
}

#' Suzetrigine (FDA Integrated Review of NDA 219209, model 2822)
#'
#' Two compartments, oral, with enzyme autoinduction, forming M6-SUZ; see the
#' header of \code{R/drugs_suzetrigine.R}.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
suzetrigine <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  f <- suzetrigineVolumeFactor(weight, height, age, sex, adjustToFFM)
  cl <- suzetrigineClearance(weight, height, sex) / 60          # L/min
  m6 <- M6SUZ(weight, height, age, sex, adjustToFFM)$PK$default

  default <- list(
    v1  = SUZETRIGINE_V1 * f,
    v2  = SUZETRIGINE_V2 * f,
    v3  = 1,
    cl1 = cl,                     # before induction, for the help tables
    cl2 = SUZETRIGINE_Q * suzetrigineQFactor(weight, height, age, sex, adjustToFFM) / 60,
    cl3 = 0,
    ka_PO = SUZETRIGINE_KA / 60,  # the small-dose limit; see kaDoseDependence
    bioavailability_PO = SUZETRIGINE_F,
    tlag_PO = SUZETRIGINE_TLAG * 60
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
    oralSaturation = c(list(form = ORAL_SATURATION_SATURABLE), SUZETRIGINE_F_DOSE),
    michaelisMenten = list(
      vmax = 0,
      km = 1,                     # unused: no saturable elimination
      autoinduction = list(
        cl = cl,
        kEnz = SUZETRIGINE_KENZ / 60,
        emax = SUZETRIGINE_AMAX,
        ec50 = SUZETRIGINE_AC50,
        hill = SUZETRIGINE_AHILL,
        imax = 0
      ),
      zeroOrderDepot = SUZETRIGINE_D1 * 60,
      kaDoseDependence = SUZETRIGINE_KA_DOSE,
      # M6-SUZ's disposition is its own model's (R/drugs_M6SUZ.R)
      metabolite = list(
        name = "M6SUZ",
        fraction = SUZETRIGINE_FR,
        v1 = m6$v1,
        v2 = m6$v2,
        q = m6$cl2,
        cl = m6$cl1,
        induced = TRUE,
        mwRatio = 1               # the source's amounts are mass on both sides
      )
    ),
    reference = SUZETRIGINE_REFERENCE
  )
}

SUZETRIGINE_REFERENCE <- paste0(
  "FDA Integrated Review of NDA 219209 (Journavx, 2025), Table 149 and Figure 24, ",
  "the applicant's model 2822: suzetrigine two compartments (V2P 103 L and V3P 204 L, ",
  "each x weight/76.2; Q3P 19 L/h; CLP 5.56 L/h in women, x 1.21 in men, x (BMI/27.8)^-0.605), ",
  "lag 0.182 h, zero-order release 1.1 h, ka 4.52 /h x (1 - 0.895 D/(36 + D)), ",
  "F 0.88 x (1 - 0.419 D^5/(162^5 + D^5)); 80% of its clearance forms M6-SUZ ",
  "(V2M 1.7 L and V3M 29.5 L, each x weight/76.2; Q3M 8.75 L/h; CLM 0.273 x CLP); every ",
  "clearance x AENZ, an enzyme turning over at 0.00939 /h and induced up to 1.811-fold ",
  "(AC50 432 ng/mL, Hill 10).  With the fat-free-mass switch on, Q3P and Q3M, which carry ",
  "no size term, are scaled by the fat-free-mass ratio^0.75, anchored at the data's ",
  "median subject (76.2 kg, BMI 27.8, 42 years, 76.7% men). ",
  "https://www.accessdata.fda.gov/drugsatfda_docs/nda/2025/219209Orig1s000IntegratedR.pdf")
