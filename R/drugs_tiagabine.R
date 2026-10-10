# -----------------------------------------------------------------------------
# Tiagabine: one compartment, apparent oral
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in ng/mL, total plasma.  Drafted by Claude Code, 2026-10-10,
# at the request of Steven L. Shafer; see the antiseizure registry,
# inst/extdata/antiseizureRegistry.csv.
#
# SOURCE
# ======
# Ingwersen SH et al. (Eur J Pharm Sci 2000;11:247-254): 130 patients on
# tiagabine monotherapy, 593 concentrations; one compartment, first-order
# absorption, NONMEM.  From the abstract, at a height of 170 cm:
#
#     CL/F 6.10 L/h   V/F 62.0 L   ka 1.25 /h
#
# Both CL/F and V/F scale with body HEIGHT (the exponent is in a table the
# session could not retrieve), so the model is written at 170 cm and its
# size is carried, for want of the published height term, by the library's
# fat-free-mass scaling instead; with the switch off the 170 cm values are
# used for everyone (legacyVolume = 1).  That substitution is recorded in the
# registry.  Population half-life 5.7 h.
#
# ENZYME INDUCTION NOT APPLIED: Ingwersen's was a monotherapy population.
# Samara 1998 found clearance about 67% higher with enzyme-inducing drugs
# (the app has no co-medication field).
#
# CHECKS (70 kg adult): half-life 7.0 h (label 7-9 h without inducers); a
# single 4 mg dose peaks at about 25 ng/mL at 1.3 h (label Tmax 1-2 h).
#
# BAND: 20-200 ng/mL (ILAE, Patsalos 2008), typical 50.  Tiagabine is 96%
# bound; the range is total drug.
#
# References
# ----------
# Ingwersen SH et al., Eur J Pharm Sci 2000;11:247-254.
#   https://doi.org/10.1016/s0928-0987(00)00109-3
# Samara EE et al., Epilepsia 1998;39:868-873.
#   https://doi.org/10.1111/j.1528-1157.1998.tb01182.x
# Patsalos PN et al., Epilepsia 2008;49:1239-1276.
#   https://doi.org/10.1111/j.1528-1167.2008.01561.x
# -----------------------------------------------------------------------------

#' Tiagabine pharmacokinetics (oral)
#'
#' Ingwersen et al. (2000) monotherapy model; see the header of the file.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
tiagabine <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1)

  default <- list(
    v1  = 62.0 * size$volume, v2 = 1, v3 = 1,
    cl1 = 6.10 * size$clearance / 60, cl2 = 0, cl3 = 0,
    ka_PO = 1.25 / 60,
    bioavailability_PO = 1,     # apparent
    tlag_PO = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  list(
    PK = PK, tPeak = 0, MEAC = 0,
    typical = 50, upperTypical = 200, lowerTypical = 20,
    reference = paste0(
      "Ingwersen SH et al., Eur J Pharm Sci 2000;11:247-254: one compartment, ",
      "apparent oral, monotherapy, CL/F 6.10 L/h and V/F 62.0 L at 170 cm, ",
      "ka 1.25/h; size by fat-free mass in place of the source's height term. ",
      "https://doi.org/10.1016/s0928-0987(00)00109-3"
    )
  )
}
