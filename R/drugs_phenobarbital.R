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
# mg/L at steady state, inside the 10-30 mg/L reference range.  The critically
# ill clear phenobarbital faster than healthy adults (80-100 h half-life in
# the older literature), so outpatient accumulation may be underpredicted.
#
# NOT MODELLED: co-medication (valproate inhibits, rifampicin induces),
# primidone-derived phenobarbital, renal replacement, urinary alkalinisation.
#
# PREGNANCY
# =========
# Dam, Christiansen, Munck and Mygind (Clin Pharmacokinet 1979;4:53-62; read
# in full, 2026-10-19) followed 14 women with epilepsy prospectively through
# pregnancy and for 20 weeks after delivery; Table III gives each woman's
# apparent phenobarbital clearance, the daily dose over the trough level
# (mL/min), in 7 of them (two on primidone, whose phenobarbital clearance
# they took from one fifth of the primidone dose).  Clearance rose to a
# maximum in 4 of the 7 in the last weeks of pregnancy, and the values at
# weeks 37-39 differed from those 4 weeks after delivery (Friedman
# 0.01 < p < 0.02).  Per woman, the mean at weeks 37-40 over the mean after
# delivery (weeks 8-20 where measured, otherwise the latest values) was
# 1.25, 1.52, 1.09, 1.39, 1.18, 0.89 and 1.35: median 1.25, geometric mean
# 1.22.  With the Patient Profile's Pregnant field on, the clearance is
# multiplied by PHENOBARBITAL_PREGNANCY_CL_FACTOR = 1.25, the median; the
# volume, absorption and bioavailability are unchanged (Dam measured only
# dose over level, which is CL/F, and phenobarbital's F is near 1).  Seven
# women, with doses adjusted as levels fell, so confidence is low.
# Phenobarbital is about half bound, so the fall in albumin moves the total
# little.  Yerby 1990 (free fraction by trimester) is still to be read.  See
# R/pregnancy.R.  (Claude Code, 2026-10-19.)
#
# BAND: 10-30 mcg/mL, typical 20 (the midpoint, not a target): Hart 2026
# (AGNP consensus guidelines, update 2026, Pharmacopsychiatry 59:166-295,
# Table 5, doi 10.1055/a-2860-7861), the phenobarbital range of its
# "Primidone and phenobarbital" row (printed p. 210), 12 h after a dose,
# alert level 50 mcg/mL, level 3, taken from the SPC.  It replaced ILAE's
# 10-40 (Patsalos 2008) by decision of Steven L. Shafer, 2026-10-27.
#
# References
# ----------
# Hart XM et al., Pharmacopsychiatry 2026;59:166-295.
#   https://doi.org/10.1055/a-2860-7861
# Epilepsia 2025, phenobarbital in refractory status epilepticus.
#   https://doi.org/10.1111/epi.18517
# Teixeira-da-Silva P et al., Eur J Pharm Sci 2020;153:105484.
#   https://doi.org/10.1016/j.ejps.2020.105484
# Patsalos PN et al., Epilepsia 2008;49:1239-1276.
#   https://doi.org/10.1111/j.1528-1167.2008.01561.x
# Dam M, Christiansen J, Munck O, Mygind KI.  Antiepileptic drugs: metabolism
#   in pregnancy.  Clin Pharmacokinet 1979;4:53-62.
#   https://doi.org/10.2165/00003088-197904010-00005 (pregnancy)
# -----------------------------------------------------------------------------

# Phenobarbital clearance in late pregnancy relative to the published value:
# the median of Dam 1979's seven per-woman ratios (see the header).
PHENOBARBITAL_PREGNANCY_CL_FACTOR <- 1.25

#' Phenobarbital pharmacokinetics (oral and intravenous)
#'
#' Munich 2025 status-epilepticus model; see the header of the file.
#'
#' @inheritParams cefazolin
#' @param pregnant when \code{TRUE}, multiply the clearance by
#'   \code{PHENOBARBITAL_PREGNANCY_CL_FACTOR} (Dam 1979; see the header).
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
phenobarbital <- function(weight, height, age, sex, adjustToFFM = TRUE,
                          pregnant = FALSE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)
  w <- if (!isTRUE(adjustToFFM)) {
    weight
  } else {
    ibw <- idealBodyWeightDevine(height, sex)
    if (age < 18 || ibw < weight / 2) size$pkWeight else ibw
  }

  clPregnancy <- if (isTRUE(pregnant)) PHENOBARBITAL_PREGNANCY_CL_FACTOR else 1
  default <- list(
    v1  = 34.3 * (w / 68.8),
    v2  = 1, v3 = 1,
    cl1 = 0.38 * (w / 68.8)^0.75 / 60 * clPregnancy,
    cl2 = 0, cl3 = 0,
    ka_PO = 1.9 / 60,
    bioavailability_PO = 0.96,
    tlag_PO = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Epilepsia 2025 (refractory status epilepticus, 37 adults): one ",
    "compartment, F 0.96, ka 1.9/h fixed, CL 0.38 L/h and V 34.3 L ",
    "allometric on ideal body weight (pharmacokinetic weight below 18 y). ",
    "https://doi.org/10.1111/epi.18517"
  )
  if (isTRUE(pregnant)) {
    reference <- paste0(reference, "; ", pregnancyReference(
      paste0("clearance x ", PHENOBARBITAL_PREGNANCY_CL_FACTOR),
      paste0("Dam M et al., Clin Pharmacokinet 1979;4:53-62. ",
             "https://doi.org/10.2165/00003088-197904010-00005")))
  }

  list(
    PK = PK, tPeak = 0, MEAC = 0,
    typical = 20, upperTypical = 30, lowerTypical = 10,
    reference = reference
  )
}
