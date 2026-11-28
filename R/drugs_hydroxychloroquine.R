# -----------------------------------------------------------------------------
# Hydroxychloroquine (Plaquenil): oral, one compartment with a lag
# (Carmichael 2003)
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min, amounts in mg,
# concentrations in ng/mL of hydroxychloroquine in WHOLE BLOOD.  Concentration
# only: no effect is plotted.  Drafted by Claude Code, 2026-11-28, on the
# weekly library-expansion run requested by Steven L. Shafer
# (docs/library-expansion/plan.csv, Antimalarials).  The candidate came through
# the nlmixr2lib cross-check (docs/nlmixr2lib/); nlmixr2lib's files were not
# used, and every number below was read on the PDF.
#
# SOURCE
# ======
# Carmichael SJ, Charles B, Tett SE.  Population pharmacokinetics of
# hydroxychloroquine in patients with rheumatoid arthritis.  Ther Drug Monit
# 2003;25:671-681 (PMID 14639053; card papers/digests/PMID_14639053.md; read on
# the PDF, Tables 1 and 2 on the page images).  Pooled data of four studies
# coordinated from St Vincent's Hospital, Sydney: 123 adults with rheumatoid
# arthritis (35 men), 74 on hydroxychloroquine alone and 49 on it with
# methotrexate, 1,370 whole-blood concentrations by HPLC.  Nine had a single
# oral dose and a 30-minute intravenous infusion sampled every 15 minutes for 8
# hours and at 24 and 32 hours; the other 114 gave troughs only, about 24 hours
# after a dose, from 2 weeks to 192 days into treatment.
#
# Table 2, fit (d), all 123 patients (its row label is misprinted "All 23
# patients" in the upper half of the table):
#
#     Cl   = 9.89 L/h          (SE 0.405)
#     V    = 605 L             (SE 91.3), without methotrexate
#     VMTX = 1070 L            (SE 186), added to V with methotrexate
#     ka   = 0.765 /h          (SE 0.218)
#     tlag = 0.445 h           (SE 0.022)
#     F    = 0.746             fixed, from fit (a): the nine patients' oral
#                              and intravenous doses (SE 0.068)
#
# Cl and V are systemic (F is a parameter of its own), which the paper's own
# arithmetic confirms: 0.746 x 155 mg / (9.89 L/h x 24 h) = 487 ug/L, the
# average steady-state concentration its Discussion gives for 155 mg a day.
#
# WHAT THE MODEL CAN AND CANNOT SHOW.  Clearance is the well-determined
# parameter: the steady-state average the model draws is the cohort's.  The
# TIME COURSE is not physiological.  Hydroxychloroquine is multi-exponential,
# with a terminal half-life in whole blood of 40 to 50 days (the label, 12.3;
# the paper's own Introduction, "exceeding 40 days"); a one-compartment model
# fitted mostly to troughs gives ln2 x 605 / 9.89 = 42.4 hours, which the
# authors identify with the middle phase of the triexponential models.  So the
# model reaches its steady state in about 9 days where patients take weeks to
# months, and washes out in days where the drug persists for months.  The fit
# to the nine richly sampled patients alone (Table 2, (a): Cl 39.3 L/h, V 153
# L, ka 0.122 /h, over 32 hours) shows how much the first day differs.  The
# help page says all this.
#
# DOSE BASIS.  The paper states its basis: each Plaquenil tablet has "200 mg of
# racemic HCQ sulfate equivalent to 155 mg of racemic HCQ base", and its doses
# (155 and 310 mg) are the base.  The US product is labelled as the sulfate
# (Plaquenil 200 mg; "daily doses exceeding 5 mg/kg ... of hydroxychloroquine
# sulfate"), so doses are entered as the sulfate, as labelled, and the model
# runs in base: each dose x 155/200 (AGENTS.md, Dose basis), carried in the
# oral bioavailability, 0.746 x 0.775 = 0.578.  Concentrations are the base's,
# as the assay's are.
#
# MATRIX.  Whole blood.  Hydroxychloroquine concentrates in blood cells, and
# whole-blood concentrations are several times those in plasma or serum (the
# label: after one 200 mg tablet, a peak of 129.6 ng/mL in blood and 50.3 in
# plasma), so the curve must not be compared with a serum or plasma level.
#
# ROUTE.  Oral only: the US has no intravenous product.  The parameters are
# systemic, but an intravenous route is not offered for a product that does
# not exist.
#
# METHOTREXATE.  Fit (d) adds 1070 L to V in the 49 patients also taking
# methotrexate and leaves clearance unchanged.  It is not applied: there is no
# field for it, the steady-state average does not depend on V, and the authors
# themselves suspect the sampling design (that cohort gave single troughs at
# seven visits).  The curve is the patient not taking methotrexate.
#
# BODY SIZE ANCHOR.  The paper tested no size covariate (weight was missing for
# 42 of the 123), so Cl and V take the fat-free-mass factors anchored at the
# source's cohort (Steven L. Shafer, 2026-11-18 and 2026-11-19,
# docs/weight-adjustment.md).  Table 1: hydroxychloroquine alone, 23 men and 51
# women, 53.9 years, 69.2 kg (weight known for 33); with methotrexate, 12 men
# and 37 women, 56.8 years, 71.0 kg (known for 48).  No height, BMI or BSA is
# printed: each sex's height is NCD-RisC's mean height at 18 for Australia (NCD
# Risk Factor Collaboration, eLife 2016;5:e13410, the country file), for the
# birth cohorts 1941 and 1938 (the studies were published from 1993 to 2000;
# 1995 is taken as their year, less 53.9 and 56.8), less the 0.2 cm (men) and
# 0.3 cm (women) a cohort loses per decade after 18: 174.1 and 161.2 cm for the
# first group, 173.6 and 160.8 cm for the second.  With the switch off the
# published values are used for every patient.  ka and the lag are not size
# quantities.
#
# RENAL AND HEPATIC FUNCTION: the label says a dose reduction "may be
# necessary" in hepatic or renal disease and gives no number; about 16 to 30%
# of a dose is excreted unchanged in urine.  The model has no term for either.
#
# PREGNANCY: recorded, not applied (inst/extdata/pregnancyRegistry.csv).
# Balevic 2022 (22 pregnancies, serum) fitted an apparent clearance about 60%
# higher in the third trimester than post partum, at p < 0.05 with a relaxed
# criterion, where the same group's earlier paired study of 25 women had found
# no change in clearance.
#
# CHECKS (the published values, switch off)
# =========================================
# The paper (not an independent check): the steady-state average at 200 mg of
# the sulfate a day is 487 ng/mL, and at 400 mg 974, the paper's own figures;
# the half-life 42.4 h (the paper's text gives 43.3 h, from fit (b)).
# The label (12.3; not fitted here): after one 200 mg tablet in healthy men, a
# whole-blood peak of 129.6 ng/mL at 3.3 h.  The model's first-dose peak is 176
# ng/mL at 5.6 h, a third higher and later: these are patients with rheumatoid
# arthritis, two thirds of them women, fitted mostly at steady state, and the
# volume a trough-only fit gives is not the volume the first hours see.  The
# label's bioavailability, 79% (SD 12%) fasting, agrees with the paper's 0.746.
#
# References
# ----------
# Carmichael SJ, Charles B, Tett SE.  Ther Drug Monit 2003;25:671-681.
#   https://doi.org/10.1097/00007691-200312000-00005
# PLAQUENIL (hydroxychloroquine sulfate) tablets, prescribing information,
#   Advanz Pharma, NDA 009768, revised 4/2026 (DailyMed setid
#   34496b43-05a2-45fb-a769-52b12e099341).
# Balevic SJ et al.  Lupus Sci Med 2022;9:e000602 (pregnancy).
#   https://doi.org/10.1136/lupus-2021-000602
# -----------------------------------------------------------------------------
HYDROXYCHLOROQUINE_CL   <- 9.89     # L/h, Table 2 (d)
HYDROXYCHLOROQUINE_V    <- 605      # L, without methotrexate
HYDROXYCHLOROQUINE_KA   <- 0.765    # /h
HYDROXYCHLOROQUINE_TLAG <- 0.445    # h
HYDROXYCHLOROQUINE_F    <- 0.746    # tablet against intravenous, Table 2 (a)
# Recorded, not applied: the volume added with methotrexate (see the header)
HYDROXYCHLOROQUINE_V_METHOTREXATE <- 1070   # L
# mg of hydroxychloroquine base in a mg of the sulfate, as the paper and the
# label both give it (a 200 mg tablet holds 155 mg of base)
HYDROXYCHLOROQUINE_BASE_PER_SULFATE <- 155 / 200

# Carmichael 2003, Table 1 (see BODY SIZE ANCHOR): the two groups of fit (d),
# each sex at its NCD-RisC height for Australia
HYDROXYCHLOROQUINE_COHORT <- list(groups = list(
  list(weight = 69.2, height = 174.1, age = 53.9, men = 23, women = 0),
  list(weight = 69.2, height = 161.2, age = 53.9, men = 0, women = 51),
  list(weight = 71.0, height = 173.6, age = 56.8, men = 12, women = 0),
  list(weight = 71.0, height = 160.8, age = 56.8, men = 0, women = 37)))

#' Hydroxychloroquine pharmacokinetics (oral)
#'
#' One compartment with first-order absorption after a lag, in whole blood
#' (Carmichael 2003, adults with rheumatoid arthritis); see the header of the
#' file.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
hydroxychloroquine <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume = 1, legacyClearance = 1,
                        cohort = do.call(cohortSizeFactors, HYDROXYCHLOROQUINE_COHORT))

  default <- list(
    v1  = HYDROXYCHLOROQUINE_V * size$volume,
    v2  = 1,
    v3  = 1,
    cl1 = HYDROXYCHLOROQUINE_CL * size$clearance / 60,
    cl2 = 0,
    cl3 = 0,
    ka_PO = HYDROXYCHLOROQUINE_KA / 60,
    bioavailability_PO = HYDROXYCHLOROQUINE_F * HYDROXYCHLOROQUINE_BASE_PER_SULFATE,
    tlag_PO = HYDROXYCHLOROQUINE_TLAG * 60
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
    reference = paste0(
      "Carmichael SJ, Charles B, Tett SE, Ther Drug Monit 2003;25:671-681, Table 2 (d) ",
      "(123 adults with rheumatoid arthritis, whole blood): one compartment, Cl 9.89 L/h, ",
      "V 605 L, ka 0.765 /h, lag 0.445 h, F 0.746; the methotrexate term on V not ",
      "applied; doses as hydroxychloroquine sulfate, the model in base (x 155/200); ",
      "whole blood, not plasma or serum; the steady-state average is the cohort's, but ",
      "the 42-hour half-life is a middle phase (terminal half-life 40-50 days), so ",
      "accumulation and washout are drawn too fast. ",
      "https://doi.org/10.1097/00007691-200312000-00005")
  )
}
