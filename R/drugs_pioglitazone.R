# -----------------------------------------------------------------------------
# Pioglitazone: two compartments, oral (Actos)
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in ng/mL, of the parent drug.  Concentration only: the
# active metabolites M-III and M-IV, which carry much of the effect, and the
# slow insulin-sensitising action are not modelled.  Drafted by Claude Code,
# 2026-10-17, at the request of Steven L. Shafer; see the antidiabetic
# registry, inst/extdata/antidiabeticRegistry.csv.
#
# SOURCE
# ======
# Kadam R, Bourne D, Kompella U, Aquilante C.  Effect of cytochrome P450 2C8*3
# on the population pharmacokinetics of pioglitazone in healthy Caucasian
# volunteers.  Biol Pharm Bull 2013;36:245-251.  31 healthy adults, one 15 mg
# dose, sampled to 48 h.  Two compartments with first-order absorption,
# apparent parameters; Table 3 and the final model in the text:
#
#     CL/F = 2.12 L/h  (+ 1.09 L/h in a CYP2C8*3 carrier)
#     Vc/F = 21.4 L
#     Q/F  = 0.349 L/h (+ 1.03 L/h in a CYP2C8*3 carrier)
#     Vp/F = 26.3 L, FIXED
#     ka   = 1.82 /h
#
# The abstract's "52% and 286% higher" in carriers are the same model
# rounded (1.09 / 2.12 = 51%; 1.03 / 0.349 = 295%, or 294% with the text's
# 0.35).
#
# CYP2C8.  The Patient Profile has no CYP2C8 field, so the curve is the
# CYP2C8*1/*1 (non-carrier) patient's.  The study enrolled 16 *1/*1
# homozygotes and 15 *3 carriers by design, so its carrier share says
# nothing about a population's.  A carrier clears pioglitazone about half
# again as fast.
#
# BODY SIZE: no covariate (sex and weight were not significant); the
# parameters are scaled by the library's fat-free-mass factors, fixed with
# the switch off (legacyVolume = 1).
#
# References
# ----------
# Kadam R et al., Biol Pharm Bull 2013;36:245-251.
#   https://doi.org/10.1248/bpb.b12-00657
# -----------------------------------------------------------------------------

#' Pioglitazone pharmacokinetics (oral)
#'
#' Two compartments, first-order absorption (Kadam 2013); see the header of
#' the file.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
pioglitazone <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1)

  default <- list(
    v1  = 21.4 * size$volume, v2 = 26.3 * size$volume, v3 = 1,
    cl1 = 2.12 * size$clearance / 60, cl2 = 0.349 * size$clearance / 60, cl3 = 0,
    ka_PO = 1.82 / 60,
    bioavailability_PO = 1,   # apparent parameters
    tlag_PO = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  list(
    PK = PK, tPeak = 0, MEAC = 0,
    typical = 0, upperTypical = 0, lowerTypical = 0,
    reference = paste0(
      "Kadam 2013 (31 healthy Caucasian adults, 15 mg): two compartments, CL/F 2.12 ",
      "L/h, Vc/F 21.4 L, Q/F 0.349 L/h, Vp/F 26.3 L fixed, ka 1.82/h, for a CYP2C8*1/*1 ",
      "patient (carriers of *3 clear it faster); parent only, concentration only. ",
      "https://doi.org/10.1248/bpb.b12-00657"
    )
  )
}
