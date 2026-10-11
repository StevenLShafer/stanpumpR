# -----------------------------------------------------------------------------
# Phenobarbital: one compartment, absolute (oral and intravenous)
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in mcg/mL (= mg/L), total plasma.  Drafted by Claude Code,
# 2026-10-10, at the request of Steven L. Shafer; see the antiseizure
# registry, inst/extdata/antiseizureRegistry.csv.
#
# SOURCE
# ======
# The Munich group (Epilepsia 2025, PMC12605696): 37 critically ill adults
# with refractory or super-refractory status epilepticus, 301 levels after
# intravenous and enteral phenobarbital, one compartment, first-order
# absorption, Monolix.  Read from the full text and its parameter table:
#
#     F  = 0.96 (estimated: the cohort had both routes)
#     ka = 1.9 /h, FIXED from the literature
#     V  = 34.3 L   x (IBW / 68.8 kg)
#     CL = 0.38 L/h x (IBW / 68.8 kg)^0.75
#
# IBW is ideal body weight, centred on the cohort median of 68.8 kg.  The
# half-life is 62.6 h.  Nelson's healthy-volunteer bioavailability, 0.95, is
# the paper's own cross-check of its F.
#
# The ChatGPT handoff also named Teixeira-da-Silva P et al. (Eur J Pharm Sci
# 2020;153:105484): 395 outpatients, CL/F = (0.236 + 0.115 (BSA - 1.7)) x
# 0.822 (phenytoin) x 0.711 (valproate) L/h, with V not separable from F.  It
# is apparent and oral only.  The two are not merged.
#
# IDEAL BODY WEIGHT
# =================
# Devine ideal body weight (the shared idealBodyWeightDevine() in
# R/drugs_metronidazole.R: 50 kg men or 45.5 kg women + 2.3 kg per inch over
# 60 inches), as in the source.  The formula is meaningless in children and
# short adults (it is negative below about 100 cm), and the source fitted
# adults only.  Below 18 years, or wherever IBW falls under the
# pharmacokinetic weight's floor of half the patient's weight, the model uses
# the library's pharmacokinetic weight instead; this is an EXTRAPOLATION of an
# adult critically-ill model, and the help page says so.  With the fat-free-
# mass switch off, total body weight replaces IBW.
#
# CHECKS (70 kg, 170 cm man: IBW 66.0 kg)
# =======================================
# Half-life 62.6 h; a 20 mg/kg intravenous load (1400 mg) gives about 42
# mg/L at once and 31 mg/L a day later.  100 mg/day orally reaches about 11
# mg/L at steady state, inside the 10-40 mg/L reference range.  The critically
# ill clear phenobarbital faster than healthy adults (80-100 h half-life in
# the older literature), so outpatient accumulation may be underpredicted.
#
# NOT MODELLED: co-medication (valproate inhibits, rifampicin induces),
# primidone-derived phenobarbital, renal replacement, urinary alkalinisation.
#
# BAND: 10-40 mcg/mL (ILAE, Patsalos 2008), typical 20.
#
# References
# ----------
# Epilepsia 2025, phenobarbital in refractory status epilepticus.
#   https://doi.org/10.1111/epi.18517
# Teixeira-da-Silva P et al., Eur J Pharm Sci 2020;153:105484.
#   https://doi.org/10.1016/j.ejps.2020.105484
# Patsalos PN et al., Epilepsia 2008;49:1239-1276.
#   https://doi.org/10.1111/j.1528-1167.2008.01561.x
# -----------------------------------------------------------------------------

#' Phenobarbital pharmacokinetics (oral and intravenous)
#'
#' Munich 2025 status-epilepticus model; see the header of the file.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
phenobarbital <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)
  w <- if (!isTRUE(adjustToFFM)) {
    weight
  } else {
    ibw <- idealBodyWeightDevine(height, sex)
    if (age < 18 || ibw < weight / 2) size$pkWeight else ibw
  }

  default <- list(
    v1  = 34.3 * (w / 68.8),
    v2  = 1, v3 = 1,
    cl1 = 0.38 * (w / 68.8)^0.75 / 60,
    cl2 = 0, cl3 = 0,
    ka_PO = 1.9 / 60,
    bioavailability_PO = 0.96,
    tlag_PO = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  list(
    PK = PK, tPeak = 0, MEAC = 0,
    typical = 20, upperTypical = 40, lowerTypical = 10,
    reference = paste0(
      "Epilepsia 2025 (refractory status epilepticus, 37 adults): one ",
      "compartment, F 0.96, ka 1.9/h fixed, CL 0.38 L/h and V 34.3 L ",
      "allometric on ideal body weight (pharmacokinetic weight below 18 y). ",
      "https://doi.org/10.1111/epi.18517"
    )
  )
}
