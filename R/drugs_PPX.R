# -----------------------------------------------------------------------------
# PPX (2',6'-pipecoloxylidide): ropivacaine's toxic metabolite
# -----------------------------------------------------------------------------
# Provenance
# ----------
# Drafted by Claude Code, 2026-11-17, at the request of Steven L. Shafer, who
# asked for the toxic metabolites to be modelled (MEGX, norpethidine and then
# ropivacaine's PPX), drawn with their labels in red.  Read from the full
# texts Shafer supplied.  Verified by tests/testthat/test-drugs-PPX.R.
#
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in mcg/mL (= mg/L) of TOTAL plasma PPX, the library's
# ropivacaine being total plasma too.
#
# SOURCE
# ======
# Sunder RA, Henthorn TK, Anderson CTM, Gentry KR, Adams TL, White KK,
# Polaner DM.  Revisiting infusion rate limitations for continuous lumbar
# plexus blocks in children and adolescents with 0.2% ropivacaine: a
# pharmacokinetic analysis.  Anesthesiology 2026;144:90-102.  20 patients aged
# 4 to 17 years, 17 to 95 kg, lumbar plexus block 2 mg/kg then 0.4 mg/kg/h,
# total and unbound ropivacaine and PPX in venous plasma for 26 h; Phoenix
# NLME, the ropivacaine post hoc parameters fixed before the PPX fit.  Table
# 2, total PPX, at 40 kg:
#
#     CLePPX/F   0.29 L/min   (SEE 0.01)
#     VPPX/F     351.0 L      (SEE 102.2)
#     Metabolic delay   4.9 min, three tanks in series
#
# with total-body-mass scaling, clearance (W/40)^0.75 and volume (W/40)^1
# (Appendix 2's code; FFM scaled no better).  ALL of ropivacaine's clearance
# forms PPX "with no accounting for molecular weight change", so the PPX
# parameters are apparent ("/F", F the fraction actually converted) and on a
# mass-for-mass scale: the link in R/drugs_ropivacaine.R forms PPX from all
# of the library ropivacaine's clearance with mwRatio 1, as fitted.  The
# FORMED concentrations do not depend on the unknown fraction, a direct dose
# would, so PPX has no dosing unit.  At 70 kg: CL 0.443 L/min (26.6 L/h), V
# 614 L, half-life 16.0 h.
#
# The unbound PPX model in the same table is not used: its estimates were
# poorly specified (fractional SEE above 0.5) and the authors simulated with
# total PPX themselves.  PPX is about half unbound (48%), ropivacaine 4%.
#
# THE DELAY
# =========
# Appendix 2's code has three delay compartments in series, each emptying at
# dt (Table 2's 4.9 min being the whole chain's mean, 3/dt).  The library's
# metabolite engine carries one transit compartment (kTransit, as MEGX's), so
# the chain is represented by one compartment with the same 4.9 min mean.
# test-drugs-PPX.R measures the cost against the three-tank chain: 0.09% of
# the peak PPX after a 150 mg block, at its worst (10 min), and nothing at
# steady state, against a half-life of 16 h.
#
# THE PARENT IS THE LIBRARY'S
# ===========================
# Sunder fitted its own two-compartment ropivacaine in children.  The library
# ropivacaine is Simon 2006's older-adult disposition (R/drugs_ropivacaine.R).
# Because all of the parent's clearance forms PPX, the amount formed is the
# dose whichever model clears it, and the steady state is the infusion rate
# over PPX's clearance; the parent changes only the timing of formation.
# METABOLITE_FORMATION_BASIS records the pair as "joint".
#
# CHILDREN'S MODEL, ADULT CHECKS
# ==============================
# Sunder studied children and adolescents; Aarons 2011 (Br J Anaesth
# 107:409-424, read in full) studied children 0 to 12 and warns against
# extrapolating to adults, and its PPX volume scales with weight^1.67 (a
# half-life of two days at 70 kg), so it is not used.  The one adult model,
# Yoo 2025 (Eur J Anaesthesiol 42:751-753, a letter, read in full), is five
# patients with chronic kidney disease sampled for 24 h, with a fraction
# metabolised of 0.00235 and no PPX volume printed, and is not used.  Sunder's
# model is checked instead against two adult studies it was not fitted to,
# both peripheral venous total PPX at the end of an infusion (read in full):
#
#   Burm 2000 (Anesthesiology 93:395-403): 50 mg epidural then 20 or 30 mg/h
#     for 72 h, mean 71 and 82 kg: 1.0 +/- 0.4 and 1.2 +/- 0.4 mg/L.
#   Ekatodramis 2003 (Anesthesiology 98:143-150): 225 mg interscalene, then
#     12 or 18 mg/h from 6 to 54 h, mean 83 and 75 kg: 0.35 +/- 0.18 and
#     0.70 +/- 0.38 mg/L.
#
# This model, with the library's ropivacaine and both studies' doses entered
# as tissue (RA) doses, gives 0.71, 0.95, 0.37 and 0.58 mg/L
# (test-drugs-PPX.R): within one SD of all four.  It is low by 29% and 21%
# against Burm, whose patients' alpha-1-acid glycoprotein doubled over the
# 72 h and raised total (not unbound) PPX, which this model, fitted over 26 h,
# does not carry; and +6% and -17% against Ekatodramis.
#
# NO EFFECT SITE, NO BAND
# =======================
# PPX is a convulsant about a twelfth as potent as unbound ropivacaine (an
# unpublished AstraZeneca rat figure, Pere 2011, quoted by Sunder, Aarons and
# Ekatodramis), and the toxic threshold the papers use is unbound ropivacaine
# + unbound (Ekatodramis, Aarons) or total (Sunder) PPX / 12 against 0.34 to
# 0.38 mg/L.  The library plots total ropivacaine and no binding, so that sum
# cannot be drawn.  No equilibration delay: plasma only, no band, label in
# red (TOXIC_METABOLITES).
#
# References
# ----------
# Sunder RA et al. Anesthesiology 2026;144:90-102.
#   https://doi.org/10.1097/ALN.0000000000005775
# Burm AGL et al. Anesthesiology 2000;93:395-403.
#   https://doi.org/10.1097/00000542-200008000-00017
# Ekatodramis G et al. Anesthesiology 2003;98:143-150.
#   https://doi.org/10.1097/00000542-200301000-00023
# -----------------------------------------------------------------------------

# Sunder 2026, Table 2, total PPX at 40 kg (apparent: all ropivacaine
# clearance forms PPX, mass for mass).
PPX_CL  <- 0.29    # L/min at 40 kg
PPX_V   <- 351.0   # L at 40 kg
PPX_REF_WEIGHT <- 40
# Mean metabolic delay of Appendix 2's three tanks, carried as one transit
# compartment (see the header).
PPX_DELAY <- 4.9   # min

PPX_REFERENCE <- paste0(
  "Sunder RA, Henthorn TK, Anderson CTM, et al. Revisiting infusion rate ",
  "limitations for continuous lumbar plexus blocks in children and ",
  "adolescents with 0.2% ropivacaine: a pharmacokinetic analysis. ",
  "Anesthesiology 2026;144:90-102. https://doi.org/10.1097/ALN.0000000000005775"
)

#' PPX (2',6'-pipecoloxylidide) pharmacokinetics
#'
#' Ropivacaine's toxic metabolite.  Has no dosing unit of its own: see the
#' file header.
#'
#' @inheritParams ropivacaine
#' @returns a list in the shape \code{getDrugPK()} expects
#' @noRd
PPX <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  # Total body mass, as fitted; adjustToFFM has no effect.
  v1  <- PPX_V * weight / PPX_REF_WEIGHT
  cl1 <- PPX_CL * (weight / PPX_REF_WEIGHT)^0.75

  default <- list(
    v1 = v1,
    v2 = 1,     # no second compartment
    v3 = 1,
    cl1 = cl1,
    cl2 = 0,
    cl3 = 0
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
    reference = PPX_REFERENCE
  )
}
