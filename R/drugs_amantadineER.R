# -----------------------------------------------------------------------------
# Amantadine, extended-release tablets (Osmolex ER): one compartment,
# apparent oral, with parallel first- and zero-order absorption and MDRD eGFR
# on elimination
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in ng/mL of amantadine.
#
# Drafted by Claude Code, 2026-10-19, at the request of Steven L. Shafer
# (inst/help/models/parkinsons.md, inst/extdata/parkinsonsRegistry.csv).
#
# THE SOURCE
# ==========
# FDA Clinical Pharmacology Review of NDA 209410 (Osmolex ER, signed 26
# December 2017), section 4.2.1, Reviewer Analysis, Table 6, read in full:
# the FDA reviewers' own population model of the renal impairment study
# OS320-PKP07, a single 160 mg amantadine HCl tablet in 24 adults (8 with
# MDRD eGFR below 30, 8 at 30-59, 8 with normal function; age 37-73, weight
# 56-133 kg), sampled to 648 h:
#
#     ka 0.344 /h,  Vc 437000 mL,  k 0.02 /h x eGFR/42,  D2 8.79 h,  F1 0.799
#
# "Covariate model; k*eGFR/42 (42mL/min /1.7m2 is the median of MDRD)": the
# elimination rate constant is proportional to the MDRD eGFR (mL/min/
# 1.73 m^2), with no intercept.
#
# WHAT IS INFERRED: THE INPUT
# ===========================
# The review prints F1 and D2 without the structure, and the applicant's
# description is redacted; the published analysis of the same programme
# (Fang 2019, CNS Drugs 33:825, PMC6669187, read in full but its parameter
# table is in an electronic supplement not read) has "a depot and a central
# compartment with first-order and zero-order (duration) input processes".
# In NONMEM's convention that is a fraction F1 = 0.799 of each dose into the
# depot, absorbed first order at ka, and the rest, 0.201, into the central
# compartment at a constant rate over D2 = 8.79 h, from the dose.  (The
# alternative, the whole dose released into the depot over D2, peaks at
# 214 ng/mL in normal renal function, against 281 measured.)  The reading
# reproduces the study's means in normal renal function (Cmax 275 against
# 281 ng/mL, AUC 8600 against 10100 ng.h/mL) and severe impairment (333 and
# 36400 against 345 and 39000), and UNDER-predicts the moderate group (AUC
# 18900 against 33100), whose measured mean clearance, 5.5 L/h, sits much
# nearer the severe group's 4.6 than its eGFR does.  The tablet is an
# immediate-release layer over an extended-release core, so which input is
# which layer is not obvious; the model is the reviewers' fit, whatever its
# mechanism.  It is carried by zeroOrderPO with into = "central"
# (R/subcutaneous.R), exactly.
#
# DOSE BASIS
# ==========
# The review works in mg of amantadine HYDROCHLORIDE (its footnote: "160 ...
# mg amantadine HCl are equivalent to ... 129 ... mg of amantadine free
# base"), and the model reproduces the measured concentrations only with the
# hydrochloride dose.  Osmolex ER tablets are labelled as BASE (129, 193 and
# 258 mg, "as 160 mg, 240 mg, or 320 mg amantadine hydrochloride"), so a dose
# is entered as labelled and converted: every volume and clearance is
# multiplied by 151.25 / 187.71 (base per hydrochloride).
#
# RENAL FUNCTION: MDRD eGFR (egfrMDRD(), R/renalFunction.R) from the entered
# serum creatinine (a child's on the adult scale) or, if blank, an assumed
# normal creatinine for age and sex.  At an eGFR of 89, the normal group's
# mean, the half-life is 16 h (measured 18 h); at 21, 69 h (measured 116 h,
# the severe group).  Below 15 the label contraindicates the tablet.
# Amantadine is filtered and secreted (OCT2); quinidine or quinine lower its
# renal clearance about 30%, not applied.  No genotype is known to matter.
#
# BODY SIZE (docs/weight-adjustment.md): the model has no size covariate (24
# subjects); Vc is scaled by the library's fat-free-mass volume factor and
# the clearance k x Vc by its clearance factor, both 1 with the switch off.
#
# THE BAND: 600 to 1700 ng/mL, the reference range of the AGNP consensus
# guideline's update 2026 (Hart 2026, Pharmacopsychiatry 59:166-295, Table 5,
# read in full; marked + as revised: the 2017 update, Hiemke 2018, gave 300 to
# 600 ng/mL at trough).  It is the expected concentration 2 h after the second
# dose of the day on 200 to 600 mg/day of immediate-release tablets, so for
# this extended-release tablet it is a guide rather than a trough window
# (Table 5: 536 ng/mL at the peak after 137 mg of extended-release amantadine
# once a day); laboratory alert level 3000 ng/mL, where myoclonus, delirium
# and hallucinations are reported, level of recommendation 3.  Typical 1150,
# the midpoint, not a target.  Confusion and
# hallucinations rise with concentration, especially in the elderly and in
# renal impairment; an exploratory threshold of 1776 ng/mL for CNS adverse
# events is in Nagai 2026 (abstract).
#
# PREGNANCY
# =========
# No study of amantadine's pharmacokinetics in pregnancy was found (PubMed,
# searched 2026-10-20; inst/extdata/pregnancyRegistry.csv).  Amantadine is
# eliminated unchanged by the kidneys, and the model's elimination rate is
# proportional to the eGFR.  With the Patient Profile's Pregnant field on and
# the serum creatinine left blank, the eGFR is multiplied by
# PREGNANCY_GFR_FACTOR = 1.5 (pregnancyGfrFactor(), R/pregnancy.R), the rise
# in glomerular filtration of pregnancy, and so is the clearance; the volume
# and the absorption are unchanged.  A measured creatinine is not multiplied
# (it already carries the rise).  A physiological extrapolation with no
# amantadine data behind it; late pregnancy is the reference state.
#
# NOT THIS MODEL: immediate-release amantadine (no full-text model of its
# absorption was found) and Gocovri, the bedtime extended-release capsules,
# whose review's model is a two-stage fit in healthy subjects (registry).
#
# References
# ----------
# FDA CDER, Clinical Pharmacology Review, NDA 209410 (Osmolex ER).
#   https://www.accessdata.fda.gov/drugsatfda_docs/nda/2018/209410Orig1s000ClinPharmR.pdf
# Fang Y et al. CNS Drugs 2019;33:825-838. https://doi.org/10.1007/s40263-019-00651-1
# Muller T et al. J Neural Transm 2024;131:1247. (PMC11489222)
# -----------------------------------------------------------------------------

# FDA review of NDA 209410, Table 6 (FDA generated), hours and litres
AMANTADINE_ER_KA    <- 0.344    # /h
AMANTADINE_ER_VC    <- 437      # L, per mg of hydrochloride
AMANTADINE_ER_K     <- 0.02     # /h at the median MDRD eGFR
AMANTADINE_ER_EGFR_REF <- 42    # mL/min/1.73 m^2
AMANTADINE_ER_D2    <- 8.79     # h, the zero-order share's duration
AMANTADINE_ER_F1    <- 0.799    # the share absorbed first order
# mg of amantadine base per mg of amantadine hydrochloride (151.25 / 187.71)
AMANTADINE_BASE_PER_HCL <- 151.25 / 187.71

#' Amantadine, extended-release tablets (Osmolex ER)
#'
#' One compartment, apparent (/F), with parallel first- and zero-order oral
#' input and MDRD eGFR on elimination, from the FDA reviewers' model of NDA
#' 209410; doses in mg of amantadine base, as labelled.  See the header of
#' \code{R/drugs_amantadineER.R}.
#'
#' @inheritParams cefazolin
#' @param creatinine serum creatinine in mg/dL, or NULL for the assumed normal
#'   value for age and sex
#' @param pregnant is the patient pregnant?  \code{TRUE}, with the creatinine
#'   left blank, multiplies the eGFR by the rise in glomerular filtration of
#'   pregnancy (see the file's header).
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
amantadineER <- function(weight, height, age, sex, adjustToFFM = TRUE,
                         creatinine = NULL, pregnant = FALSE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume = 1, legacyClearance = 1)
  gfrPregnancy <- pregnancyGfrFactor(pregnant, creatinine)
  egfr <- egfrMDRD(age, sex, adultEquivalentCreatinine(creatinine, age, sex)) *
    gfrPregnancy
  base <- AMANTADINE_BASE_PER_HCL
  k <- AMANTADINE_ER_K * egfr / AMANTADINE_ER_EGFR_REF                # /h

  default <- list(
    v1  = AMANTADINE_ER_VC * size$volume * base,
    v2  = 1, v3 = 1,
    cl1 = k * AMANTADINE_ER_VC * size$clearance * base / 60,
    cl2 = 0, cl3 = 0,
    ka_PO = AMANTADINE_ER_KA / 60,
    bioavailability_PO = 1,          # apparent; the zero-order share is split off
    tlag_PO = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  list(
    PK = PK, tPeak = 0, MEAC = 0,
    typical = 1150, upperTypical = 1700, lowerTypical = 600,
    zeroOrderPO = list(duration = AMANTADINE_ER_D2 * 60, into = "central",
                       fraction = 1 - AMANTADINE_ER_F1),
    reference = paste0(
      "FDA reviewers' model of Osmolex ER, Clinical Pharmacology Review of NDA ",
      "209410, Table 6: one compartment, Vc/F 437 L, k 0.02/h x MDRD eGFR/42 from ",
      "the entered or an assumed normal creatinine; 79.9% of each dose absorbed ",
      "at 0.344/h and 20.1% at a constant rate over 8.79 h (the structure inferred); ",
      "per mg of hydrochloride, so a dose of base is converted. Band 600-1700 ng/mL ",
      "(AGNP update 2026, 2 h after a dose of IR tablets). https://www.accessdata.fda.gov/drugsatfda_docs/nda/2018/209410Orig1s000ClinPharmR.pdf",
      if (gfrPregnancy != 1) paste0("; ", pregnancyReference(paste0(
        "eGFR x ", PREGNANCY_GFR_FACTOR,
        ", the rise in glomerular filtration; no amantadine data"))) else ""
    )
  )
}
