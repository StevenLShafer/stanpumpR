# -----------------------------------------------------------------------------
# Cenobamate: two compartments, apparent oral, with an absorption lag
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in mcg/mL (= mg/L), total plasma.  Drafted by Claude Code,
# 2026-10-18, at the request of Steven L. Shafer; see the antiseizure
# registry, inst/extdata/antiseizureRegistry.csv.
#
# SOURCE
# ======
# The sponsor's population model (SK Life Science report SK16007): 960
# subjects, 8700 concentrations, from eight phase 1 studies in healthy and
# special populations, two phase 2 trials and one phase 3 trial in adults with
# focal seizures; single doses of 5 to 750 mg and daily doses of 50 to 600 mg,
# two thirds of the subjects at 200 mg/day.  Published by Vashi and colleagues
# (Epilepsy Res 2023;195:107185), whose full text was not retrievable here.
# The parameter table is transcribed from the FDA clinical pharmacology review
# of NDA 212839 (2019, Reference ID 4488162), Table 2 and Figure 6, which
# reproduce the report's pages 53-54:
#
#     ka 2.74 /h   lag 0.240 h   CL/F 0.533 L/h   V1/F 42.4 L
#     Q/F 1.84 L/h   V2/F 7.27 L
#
# Two compartments, first-order absorption after a lag, linear elimination.
#
# COVARIATES NOT APPLIED
# ======================
# The review prints the covariate coefficients but not the equations or the
# centring values, so none can be evaluated exactly:
#   - daily dose on CL/F, "exponential", -0.0280.  Neither the form nor the
#     reference dose is printed.  Read as a power of the daily dose about
#     200 mg it moves CL/F by under 4% between 100 and 400 mg/day, which
#     agrees with the multiple-dose studies (AUC proportional to dose from 50
#     to 500 mg/day; Vernillet 2020) and with the label.  The single-dose
#     nonlinearity (AUC rising faster than dose, apparent clearance falling
#     from about 1.4 to 0.5 L/h; Vernillet 2020) is not reproduced: after a
#     single small dose the model, fitted mostly at steady state,
#     overpredicts exposure.
#   - body surface area on CL/F (linear, 0.611 per m^2) and weight on V1/F
#     (linear, 0.0120 per kg), centring values unprinted.  Replaced by the
#     library's fat-free-mass scaling, as for ethosuximide.
#   - bilirubin on CL/F, African American (CL/F -14.7%, V1/F -9.35%) and
#     Hispanic (CL/F -13.3%) ethnicity: no Patient Profile fields.
#   - clobazam (CL/F -19%) and carbamazepine (+15%): no co-medication field.
#     Phenytoin lowers cenobamate exposure 27-28% (label) and is not a model
#     covariate either.  The curve is cenobamate without these drugs, in a
#     non-Hispanic, non-African-American adult of the median size and
#     bilirubin of the analysis population.
#
# CHECKS (reference adult)
# ========================
# Terminal half-life 65 h (label 50-60 h; Vernillet 2020 30-76 h, rising with
# dose).  Vss 49.7 L (label Vd/F 40-50 L).  Peak 1.6 h after an oral dose
# (label median Tmax 1-4 h).  At 200 mg once daily the steady-state average
# is 15.6 mg/L, trough 13.7 and peak 17.9.  The phase 3 mean steady-state
# concentration at 200 mg/day was 15.5 mg/L (as quoted by Greene 2022), the
# population the model mostly describes.  Healthy volunteers run higher: a
# median trough of 18.4 mg/L at 200 mg/day after titration (Greene 2022, two
# thirds African American) and 19.0 mg/L in the ascending-dose study, and a
# mean peak of 23.1 mg/L at 200 mg/day and 64.0 at 500 mg/day (Darpo 2022).
# The model is 20 to 30% below those, which is the size of the source's own
# ethnicity terms and of the healthy-versus-patient difference; a healthy
# volunteer or an African American patient is underpredicted.
#
# Steady state takes about two weeks (label).  The label titration is 12.5
# mg/day for two weeks, then 25, 50, 100, 150 and 200 mg/day, each for two
# weeks.
#
# BIOAVAILABILITY: at least 88% absorbed (mass balance); the parameters are
# apparent (/F), so bioavailability_PO is 1.  Food has no clinically
# significant effect, and a crushed tablet orally or by nasogastric tube is
# bioequivalent (Vashi 2024), so one oral input serves all.
#
# BODY SIZE: the fixed values scaled to fat-free mass in place of the source's
# linear weight and body-surface-area terms; with the switch off, the
# published values for everyone (legacyVolume = 1).
#
# BAND: none.  No therapeutic reference range has been established (median
# plasma concentration at first response about 5-6 mg/L at <= 100 mg/day,
# range 0.5-17.6; Operto 2025).  Plasma only, no effect site.
#
# References
# ----------
# Vashi V et al., Epilepsy Res 2023;195:107185.
#   https://doi.org/10.1016/j.eplepsyres.2023.107185
# FDA, NDA 212839 Clinical Pharmacology Review (Xcopri), 2019, Table 2.
#   https://www.accessdata.fda.gov/drugsatfda_docs/nda/2019/212839Orig1s000ClinPharmR.pdf
# Vernillet L et al., Clin Pharmacol Drug Dev 2020;9:428-443.
#   https://doi.org/10.1002/cpdd.769
# Greene SA et al., Clin Transl Sci 2022;15:899-911.
#   https://doi.org/10.1111/cts.13204
# Darpo B et al., Clin Pharmacol Drug Dev 2022;11:523-534.
#   https://doi.org/10.1002/cpdd.1077
# Vashi V et al., J Clin Pharmacol 2024;64:922-931.
#   https://doi.org/10.1002/jcph.2439
# Operto FF et al., Front Pharmacol 2025;16:1668382.
#   https://doi.org/10.3389/fphar.2025.1668382
# -----------------------------------------------------------------------------

#' Cenobamate pharmacokinetics (oral)
#'
#' The sponsor's population model (Vashi et al., 2023), two compartments,
#' apparent oral, with an absorption lag; parameters from the FDA clinical
#' pharmacology review of NDA 212839.  See the header of the file.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
cenobamate <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1)

  default <- list(
    v1  = 42.4 * size$volume, v2 = 7.27 * size$volume, v3 = 1,
    cl1 = 0.533 * size$clearance / 60, cl2 = 1.84 * size$clearance / 60, cl3 = 0,
    ka_PO = 2.74 / 60,
    bioavailability_PO = 1,     # apparent (/F)
    tlag_PO = 0.240 * 60        # min
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  list(
    PK = PK, tPeak = 0, MEAC = 0,
    typical = 0, upperTypical = 0, lowerTypical = 0,
    reference = paste0(
      "Vashi V et al., Epilepsy Res 2023;195:107185, parameters from the FDA ",
      "clinical pharmacology review of NDA 212839 (Table 2): two ",
      "compartments, apparent oral, lag 0.24 h, CL/F 0.533 L/h, V1/F 42.4 L, ",
      "Q/F 1.84 L/h, V2/F 7.27 L, ka 2.74/h; dose, size, bilirubin, ",
      "ethnicity and co-medication terms not applied. ",
      "https://doi.org/10.1016/j.eplepsyres.2023.107185"
    )
  )
}
