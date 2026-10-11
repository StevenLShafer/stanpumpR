# -----------------------------------------------------------------------------
# Zonisamide: one compartment, apparent oral
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in mcg/mL (= mg/L), total plasma.  Drafted by Claude Code,
# 2026-10-10, at the request of Steven L. Shafer; see the antiseizure
# registry, inst/extdata/antiseizureRegistry.csv.
#
# SOURCE
# ======
# Silva R et al. (Eur J Pharm Sci 2025;207:107023): 64 adults with refractory
# epilepsy, 114 levels (Coimbra); one compartment, first-order absorption,
# NONMEM.  From the abstract:
#
#     CL/F 0.761 L/h   V/F 48.10 L   ka 0.671 /h
#
# An enzyme-inducer drug load raised CL/F (the equation and its definition
# were not retrievable, and the app has no co-medication field, so it is not
# applied: the curve is zonisamide without inducers).
#
# CHECKS (70 kg adult)
# ====================
# Half-life 44 h.  This is shorter than the 63-69 h healthy adults show
# (Kochak 1998, who also found CL/F 0.60-0.71 L/h): Silva's refractory cohort
# was mostly on inducers, which raise clearance, so the no-inducer intercept
# still carries some of that and the volume is on the low side.  A monotherapy
# patient's accumulation may be underpredicted.  8 mg/kg/day in children gives
# a trough near 27 mg/L (Miura 2004); the model gives about 30 for a 20 kg
# child.
#
# NON-LINEARITY NOT MODELLED: zonisamide binds saturably to red cells, so
# plasma kinetics are mildly non-linear at high doses; the model is linear,
# as the source is.  Capsule and oral suspension are label-bridged and share
# these parameters.
#
# BODY SIZE: the fixed values scaled to fat-free mass; with the switch off,
# the published values for everyone (legacyVolume = 1).
#
# BAND: 10-40 mcg/mL (ILAE, Patsalos 2008), typical 20.
#
# References
# ----------
# Silva R et al., Eur J Pharm Sci 2025;207:107023.
#   https://doi.org/10.1016/j.ejps.2025.107023
# Kochak GM et al., J Clin Pharmacol 1998;38:166-171.
#   https://doi.org/10.1002/j.1552-4604.1998.tb04406.x
# Patsalos PN et al., Epilepsia 2008;49:1239-1276.
#   https://doi.org/10.1111/j.1528-1167.2008.01561.x
# -----------------------------------------------------------------------------

#' Zonisamide pharmacokinetics (oral)
#'
#' Silva et al. (2025), one compartment, apparent oral; see the header.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
zonisamide <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1)

  default <- list(
    v1  = 48.10 * size$volume, v2 = 1, v3 = 1,
    cl1 = 0.761 * size$clearance / 60, cl2 = 0, cl3 = 0,
    ka_PO = 0.671 / 60,
    bioavailability_PO = 1,
    tlag_PO = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  list(
    PK = PK, tPeak = 0, MEAC = 0,
    typical = 20, upperTypical = 40, lowerTypical = 10,
    reference = paste0(
      "Silva R et al., Eur J Pharm Sci 2025;207:107023: one compartment, ",
      "apparent oral, CL/F 0.761 L/h, V/F 48.1 L, ka 0.671/h; refractory ",
      "adults, inducer load not applied. ",
      "https://doi.org/10.1016/j.ejps.2025.107023"
    )
  )
}
