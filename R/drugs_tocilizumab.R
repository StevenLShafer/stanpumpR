# -----------------------------------------------------------------------------
# Tocilizumab (Actemra): intravenous, one compartment, parallel linear and
# saturable (target-mediated) elimination (Moes 2022, ICU COVID-19)
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, amounts in mg, concentrations in
# mcg/mL (= mg/L) of tocilizumab in plasma.  Concentration only: no
# C-reactive protein, soluble IL-6 receptor or clinical endpoint is plotted.
# Drafted by Claude Code, 2026-11-28, at the request of Steven L. Shafer
# ("Build Moes 2022 now", 2026-11-28, on the [decision] of the weekly run of
# 2026-11-27, docs/library-expansion/plan.csv, Biologics (sepsis)).  The
# candidate was found through the nlmixr2lib cross-check (docs/nlmixr2lib/),
# which listed tocilizumab among the drugs stanpumpR lacked; nlmixr2lib's
# files were not used, and every number below was read on the PDF.
#
# Humanised IgG1 monoclonal antibody against the interleukin-6 receptor.
#
# SOURCE
# ======
# Moes DJAR, van Westerloo DJ, Arend SM, Swen JJ, de Vries A, Guchelaar H-J,
# Joosten SA, de Boer MGJ, van Gelder T, van Paassen J.  Towards fixed dosing of tocilizumab in ICU-admitted
# COVID-19 patients: results of an observational population pharmacokinetic
# and descriptive pharmacodynamic study.  Clin Pharmacokinet 2022;61:231-247
# (PMID 34633645; card papers/digests/PMID_34633645.md; read on the PDF,
# Table 3 on the page image of printed p. 239).  29 adults in the ICU with
# COVID-19 (Leiden, Dec 2020 - Mar 2021), 82.8% mechanically ventilated, all
# on dexamethasone, each given one IV dose of 8 mg/kg (maximum 800 mg; mean
# 781 mg, 472-1552); 139 plasma samples to day 20, ELISA (LLOQ 0.2 mcg/mL).
#
# Table 3, final model (= the base model: no covariate was retained):
#
#     CL   = 0.725 L/day   (linear clearance; RSE 4%)
#     Vd   = 4.34 L        (RSE 4%)
#     Vmax = 4.19 mg/day   (RSE 17%)
#     Km   = 0.22 mg/L     (RSE 19%)
#
# Elimination is CL x C + Vmax x C / (Km + C), an amount rate.  The abstract
# prints Vmax in ug/day and Km in ug/mL; Table 3 and the Results print mg/day
# and mg/L.  Table 3's units are used: Figure 2's individual curves, after a
# mean 781 mg into 4.34 L, start near 180 mcg/mL and fall below 1 mcg/mL
# between days 15 and 22, which a Vmax of 4.19 MICROgrams a day (a thousand
# times below the linear clearance's 140 mg/day at 190 mcg/mL) could not
# produce: the curves would fall with the linear half-life alone, 4.2 days,
# for weeks.  The card's [check] is resolved so.  A two-compartment model was
# unstable (Table 2, model 3), so the sparse early sampling left one
# compartment; the label's COVID-19 model has two (central 4.52 L,
# peripheral 4.23 L), which is why this model's early fall is too fast.
#
# POPULATION: adults in the ICU with COVID-19.  The weekly run of 2026-11-27
# found three sources in the archive: Jamois 2025 (PMID 40576759, cytokine
# release syndrome, parameters in its supplement, not yet obtained), Frey
# 2010 (PMID 20097931, 1,793 adults with rheumatoid arthritis, Table II to
# one or two figures) and this one, the only complete one.  Shafer chose it
# (2026-11-28): the ICU curve now, the others candidates for the model
# selector when their numbers are in hand.  It is used for every patient by
# the nearest-population rule, and the help page says so.  Children are not
# in it: the label's paediatric volumes are about half the adults' (PJIA
# central 1.98 L), which the size scaling below carries only in part.
#
# BODY SIZE.  Moes tested weight (also as a power: exponent 0.002), BMI, BSA,
# height, sex and age on CL and Vd and kept none, and argues from it for a
# fixed dose.  By the library's rule every parameter is scaled all the same
# (Shafer, 2026-11-19: failing p < 0.05 is not evidence that size does not
# matter; docs/weight-adjustment.md, *Every parameter is scaled*): Vd by the
# volume factor, CL and Vmax (a clearance capacity) by the clearance factor,
# both anchored at Moes's cohort (Table 1: 21 men and 8 women, 72.4% of 29;
# mean 96.4 kg, 177 cm, 64 years), so that a patient like the cohort's
# receives the published values; Km is a concentration and is not scaled.
# With the switch off, the published values whatever the patient.
#
# THE ENGINE.  Elimination saturates, so the drug runs on the
# Michaelis-Menten engine (R/advanceMichaelisMenten.R) with the linear
# clearance as clLinear, as evolocumab does.  Intravenous only: Moes gave no
# subcutaneous doses, and the SC product's absorption is not in this model.
#
# RENAL AND HEPATIC FUNCTION: tested (creatinine, eGFR, urea, albumin, the
# liver enzymes, bilirubin, CRP) and none retained; an IgG1 is not filtered.
#
# PREGNANCY: no pharmacokinetic study (inst/extdata/pregnancyRegistry.csv).
#
# CHECKS (the published values, switch off: 781 mg, Moes's mean dose, over
# an hour)
# ===================================================================
# Moes's own: the mean AUC(0-inf) after the first dose, 938 +/- 190
# mcg/mL.day (printed p. 235); the model gives 958.  The concentration above
# which the linear clearance exceeds the saturable one, Vmax/CL - Km = 5.56
# mcg/mL (the paper: about 5 mcg/mL, Figure 6).  The model peaks at 179
# mcg/mL, is 52 at day 7 and 12 at day 14, and falls below 1 mcg/mL at day
# 20 and below the assay's 0.2 at day 21, within Figure 2's individual
# curves (below 1 mcg/mL between about days 13 and 22).  Well below Km the
# compartment empties at (CL + Vmax/Km)/V = 4.6 /day, a half-life of 3.7
# hours, so the end of the curve is a cliff, as Figure 2
# draws it.
#
# The label's COVID-19 population model (380 adults, COVACTA and another
# study, not fitted here): after one 8 mg/kg dose a median peak of 151
# (77.5-319) mcg/mL, a linear clearance of 29 mL/h (0.70 L/day) in
# mechanically ventilated patients (OS 5), against this model's 0.725 L/day
# (clearance, the one real parameter, agrees to 4%), and a day-28
# concentration of 0.229 (0.00119-19.4) mcg/mL, which this model does not
# reach: it is below 0.001 mcg/mL by day 25.  The label also says the
# concentrations were below the limit of quantification "after 35 days on
# average".  Moes sampled to day 20 with one compartment, so this model's
# tail is shorter than the label's two-compartment model's; the help page
# says so.  The test file pins the model's values and checks the engine
# against an independent Runge-Kutta solution.
#
# References
# ----------
# Moes DJAR et al., Clin Pharmacokinet 2022;61:231-247.
#   https://doi.org/10.1007/s40262-021-01074-2
# ACTEMRA (tocilizumab) injection, prescribing information, Genentech, BLA
#   125276, revised 12/2025 (DailyMed setid 2e5365ff-cb2a-4b16-b2c7-e35c6bf2de13).
# -----------------------------------------------------------------------------
TOCILIZUMAB_CL   <- 0.725   # L/day, linear
TOCILIZUMAB_V    <- 4.34    # L
TOCILIZUMAB_VMAX <- 4.19    # mg/day
TOCILIZUMAB_KM   <- 0.22    # mg/L
# Moes 2022 Table 1: 72.4% of 29 men; mean weight, height and age.
TOCILIZUMAB_COHORT <- list(weight = 96.4, height = 177, age = 64, men = 21, women = 8)

#' Tocilizumab pharmacokinetics (intravenous)
#'
#' One compartment with parallel linear and Michaelis-Menten elimination
#' (Moes 2022, adults in the ICU with COVID-19); see the header of the file.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
tocilizumab <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume = 1, legacyClearance = 1,
                        cohort = do.call(cohortSizeFactors, TOCILIZUMAB_COHORT))

  v        <- TOCILIZUMAB_V * size$volume                              # L
  clLinear <- TOCILIZUMAB_CL * size$clearance / MINS_PER_DAY           # L/min
  vmax     <- TOCILIZUMAB_VMAX * size$clearance / MINS_PER_DAY         # mg/min
  km       <- TOCILIZUMAB_KM                                           # mg/L

  default <- list(
    v1 = v, v2 = 1, v3 = 1,
    # The low-concentration limit, linear plus Vmax / Km, for the help tables
    # only: the simulation uses the michaelisMenten block.
    cl1 = clLinear + vmax / km, cl2 = 0, cl3 = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  list(
    PK = PK, tPeak = 0, MEAC = 0,
    typical = 0, upperTypical = 0, lowerTypical = 0,
    reference = paste0(
      "Moes 2022 (29 adults in the ICU with COVID-19, 8 mg/kg IV once): one ",
      "compartment, V 4.34 L, linear CL 0.725 L/day, Vmax 4.19 mg/day, Km 0.22 ",
      "mg/L; no covariate retained; V, CL and Vmax scaled by fat-free mass from ",
      "the cohort (96.4 kg, 177 cm, 64 years, 21 men and 8 women); plasma, ",
      "concentration only. https://doi.org/10.1007/s40262-021-01074-2"
    ),
    michaelisMenten = list(
      vmax = vmax,
      km = km,
      clLinear = clLinear
    )
  )
}
