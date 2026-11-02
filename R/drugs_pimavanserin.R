# -----------------------------------------------------------------------------
# Pimavanserin (Nuplazid), for the hallucinations and delusions of
# Parkinson's disease psychosis: one compartment, apparent oral, with height
# and age on clearance
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in ng/mL of pimavanserin.
#
# Drafted by Claude Code, 2026-10-19, at the request of Steven L. Shafer
# (inst/help/models/parkinsons.md, inst/extdata/parkinsonsRegistry.csv).
#
# THE SOURCE
# ==========
# FDA Clinical Pharmacology Review of NDA 207318 (2016), Pharmacometrics
# Review, read in full with its tables from the page images.  The sponsor's
# Stage 3 analysis of patients with Parkinson's disease psychosis (troughs,
# with the absorption fixed from the Stage 2 analysis) ended in two models,
# and the sponsor selected the first, judging the second's creatinine
# clearance term "physiologically unrealistic".  Table 6 (the selected one):
#
#     HeightFactor = 1 + 0.00851 (HT - 168)       HT in cm, 168 the median
#     AgeFactor    = 1 - 0.007558 (AGE - 71)      71 the median age
#     CL/F = 587.093 x HeightFactor x AgeFactor   L/day
#     V/F  = 2021.41 L
#     ka   = 7.4322 /day,  lag 0.0707 day (fixed from Stage 2)
#
# Table 7 (height and CKD-EPI creatinine clearance on CL/F, 588.814 L/day,
# V/F 2007.17 L) is the registry's alternative.
#
# DOSE BASIS
# ==========
# The trials' "40 mg" is pimavanserin TARTRATE, 34 mg of base (the review:
# "40 mg pimavanserin tartrate (34 mg pimavanserin base)"), and the model's
# apparent parameters are per mg of tartrate: the sponsor's typical average
# steady state at 40 mg, 68 ng/mL, is 40 mg/day / 587 L/day.  The label
# prescribes the base (34 mg capsules, 10 mg tablets), so a dose is entered as
# labelled and converted to tartrate here: every volume and clearance is
# multiplied by 34/40.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# The Stage 3 model's only size covariate is height, on clearance; its healthy
# volunteer Stage 1 model had allometric weight on both parameters (Table 3:
# 590.217 (WT/76)^0.75 L/day, 1850.09 (WT/76) L), but in patients height
# displaced it.  So the clearance carries its own size covariate, and the
# size-free volume is scaled by the library's fat-free-mass factor, which is 1
# with the switch off (the published model).
#
# HEIGHT: the linear height factor would reach zero at 50 cm, so the height
# it sees is held at 140 cm and above (PIMAVANSERIN_MIN_HEIGHT), a guard for
# children, who were not studied; the age factor stays positive to 203 years.
#
# CHECKS (168 cm, 71 y, switch off): half-life 57 h (label 57 h) and an
# average steady state of 68 ng/mL on 34 mg (the sponsor's 68.1).  The peak
# after one dose is broad and late, at 13.5 h, against the label's median
# tmax of 6 h (range 4-24): the absorption was fixed from Stage 2, fitted to
# troughs, and the curve at 6 h is already 81% of its peak.  The rise after a
# single dose is therefore drawn slower than measured; the steady state the
# model was built for is not affected.
#
# NOT MODELLED: the active N-desmethyl metabolite AC-279 (half-life about
# 200 h), whose parameters are not printed; CYP3A4 inhibitors (AUC x3, label
# dose 10 mg) and inducers (rifampin, -91%); QT prolongation, which rises with
# concentration (5-8 ms at 34 mg).  No effect site and no band: no
# concentration-response for psychosis is published, and the AGNP update
# 2026's range, 40 to 85 ng/mL (Hart 2026, Table 5), is for schizophrenia and
# "might not be applicable to patients with Parkinson's disease".
#
# References
# ----------
# FDA CDER, Clinical Pharmacology Review, NDA 207318 (Nuplazid).
#   https://www.accessdata.fda.gov/drugsatfda_docs/nda/2016/207318Orig1s000ClinPharmR.pdf
# -----------------------------------------------------------------------------

# FDA review of NDA 207318, Pharmacometrics Review, Table 6, days and litres
PIMAVANSERIN_CL        <- 587.093   # L/day, per mg of tartrate
PIMAVANSERIN_HT_SLOPE  <- 0.00851   # per cm
PIMAVANSERIN_HT_REF    <- 168       # cm
PIMAVANSERIN_AGE_SLOPE <- -0.007558 # per year
PIMAVANSERIN_AGE_REF   <- 71        # years
PIMAVANSERIN_V         <- 2021.41   # L
PIMAVANSERIN_KA        <- 7.4322    # /day
PIMAVANSERIN_TLAG      <- 0.0707    # day
# The lowest height the linear height term sees: it reaches zero at 50 cm.
# A guard for children, who were not studied, not part of the source.
PIMAVANSERIN_MIN_HEIGHT <- 140      # cm
# mg of base per mg of tartrate ("40 mg pimavanserin tartrate (34 mg
# pimavanserin base)")
PIMAVANSERIN_BASE_PER_TARTRATE <- 34 / 40

#' Pimavanserin pharmacokinetics (oral)
#'
#' One compartment, apparent (/F), from the sponsor's Stage 3 model (Table 6)
#' in the FDA review of NDA 207318; doses in mg of pimavanserin base.  See the
#' header of \code{R/drugs_pimavanserin.R}.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
pimavanserin <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1)
  base <- PIMAVANSERIN_BASE_PER_TARTRATE
  cl <- PIMAVANSERIN_CL *
    (1 + PIMAVANSERIN_HT_SLOPE * (max(height, PIMAVANSERIN_MIN_HEIGHT) - PIMAVANSERIN_HT_REF)) *
    (1 + PIMAVANSERIN_AGE_SLOPE * (age - PIMAVANSERIN_AGE_REF))     # L/day

  default <- list(
    v1  = PIMAVANSERIN_V * size$volume * base,
    v2  = 1, v3 = 1,
    cl1 = cl * base / MINS_PER_DAY,
    cl2 = 0, cl3 = 0,
    ka_PO = PIMAVANSERIN_KA / MINS_PER_DAY,
    bioavailability_PO = 1,          # apparent parameters
    tlag_PO = PIMAVANSERIN_TLAG * MINS_PER_DAY
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  list(
    PK = PK, tPeak = 0, MEAC = 0,
    typical = 0, upperTypical = 0, lowerTypical = 0,
    reference = paste0(
      "FDA Pharmacometrics Review of NDA 207318 (Nuplazid), Table 6 (patients ",
      "with Parkinson's disease psychosis): one compartment, CL/F 587.093 L/day x ",
      "(1 + 0.00851 (height - 168)) x (1 - 0.007558 (age - 71)), V/F 2021 L, ",
      "ka 7.43/day, lag 0.0707 day; per mg of tartrate, so a dose of base is ",
      "converted (34 mg base = 40 mg tartrate); concentration only. ",
      "https://www.accessdata.fda.gov/drugsatfda_docs/nda/2016/207318Orig1s000ClinPharmR.pdf"
    )
  )
}
