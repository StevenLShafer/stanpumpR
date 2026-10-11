# -----------------------------------------------------------------------------
# Lamotrigine: one compartment, apparent oral (immediate release)
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in mcg/mL (= mg/L), total plasma.  Drafted by Claude Code,
# 2026-10-10, at the request of Steven L. Shafer; see the antiseizure
# registry, inst/extdata/antiseizureRegistry.csv.
#
# SOURCE
# ======
# Milosheska D et al. (Br J Clin Pharmacol 2016;82:399-411, PMC4972156): 100
# Slovenian adults with epilepsy on lamotrigine alone or with other drugs;
# one compartment, NONMEM.  The base model, from the abstract:
#
#     ka 1.96 /h   CL/F 2.32 L/h   V/F 77.6 L
#
# The final model adds UGT2B7 genotype, weight, renal function, smoking and
# co-medication; those terms are not used (the app has none of them except
# weight and creatinine, and the equations are in tables not retrieved).
#
# WHY NOT THE HANDOFF'S MODEL
# ===========================
# The handoff proposed Chavez-Castillo 2020 (J Pharm Sci): CL = 1.82 (1 -
# 0.465 VPA)(1 + 0.841 CBZ) L/h with V fixed at 1.8 L/kg.  The clearance is
# confirmed (its AAN 2019 precursor), but 1.8 L/kg gives a monotherapy half-
# life of about 48 h against the 24-33 h observed (Cohen 1987 measured V/F 1.2
# L/kg).  Milosheska's base model is complete in one source and consistent.
#
# CHECKS (70 kg adult)
# ====================
# Half-life 23 h (Cohen 1987: 24 h single dose, 25.5 h multiple dose in
# healthy volunteers); 200 mg/day averages 3.6 mg/L at steady state (COMPASS
# neutral group: about 5.9 mg/L at 200 mg/day... 142 mg.h/L over 24 h).  The
# curve is a MIXED co-medication population: valproate roughly halves
# clearance (half-life 59-70 h) and enzyme inducers roughly double it (13-15
# h); neither is applied.
#
# NOT OFFERED: Lamictal XR (no published input model; COMPASS gives peak 4-11
# h), pregnancy (clearance rises by up to 2-3 fold), oestrogen contraceptives.
#
# BODY SIZE: the base model's fixed values scaled to fat-free mass; with the
# switch off, the published values for everyone (legacyVolume = 1).
#
# BAND: 3-15 mcg/mL (ILAE, Patsalos 2008, 2.5-15), typical 8.
#
# References
# ----------
# Milosheska D et al., Br J Clin Pharmacol 2016;82:399-411.
#   https://doi.org/10.1111/bcp.12984
# Cohen AF et al., Clin Pharmacol Ther 1987;42:535-541.
#   https://doi.org/10.1038/clpt.1987.193
# Tompson DJ et al., Epilepsia 2008;49:410-417 (COMPASS).
#   https://doi.org/10.1111/j.1528-1167.2007.01274.x
# -----------------------------------------------------------------------------

#' Lamotrigine pharmacokinetics (oral, immediate release)
#'
#' Milosheska et al. (2016) base model; see the header of the file.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
lamotrigine <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1)

  default <- list(
    v1  = 77.6 * size$volume, v2 = 1, v3 = 1,
    cl1 = 2.32 * size$clearance / 60, cl2 = 0, cl3 = 0,
    ka_PO = 1.96 / 60,
    bioavailability_PO = 1,    # apparent
    tlag_PO = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  list(
    PK = PK, tPeak = 0, MEAC = 0,
    typical = 8, upperTypical = 15, lowerTypical = 3,
    reference = paste0(
      "Milosheska D et al., Br J Clin Pharmacol 2016;82:399-411, base model: ",
      "one compartment, apparent oral, ka 1.96/h, CL/F 2.32 L/h, V/F 77.6 L; ",
      "mixed co-medication, none applied. https://doi.org/10.1111/bcp.12984"
    )
  )
}
