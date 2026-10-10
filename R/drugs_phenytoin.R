# -----------------------------------------------------------------------------
# Phenytoin (and fosphenytoin): one compartment, Michaelis-Menten elimination
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, Vmax in mg/min, Km and
# concentrations in mcg/mL (= mg/L), TOTAL (bound plus unbound) phenytoin acid
# in serum.  Drafted by Claude Code, 2026-10-10, at the request of Steven L.
# Shafer; the antiseizure registry is inst/extdata/antiseizureRegistry.csv.
#
# SOURCE
# ======
# Odani A et al. (Biol Pharm Bull 1996;19:444-448) fitted 531 steady-state
# serum concentrations from 116 Japanese patients with epilepsy, children and
# adults, given oral phenytoin, with a one-compartment model and Michaelis-
# Menten elimination (NONMEM).  Read from the abstract (the paper is not in
# PubMed Central):
#
#     V    = 1.23 L/kg     in a typical 42 kg patient
#     Vmax = 9.80 mg/day/kg
#     Km   = 9.19 mcg/mL   (total phenytoin; x 1.16 with zonisamide)
#     "the parameter of power function of weight to adjust V and Vmax was
#     estimated to be 0.463"
#     F    = 1, assumed
#
# The weight term is read as V = 1.23 x 42 x (WT/42)^0.463 L and Vmax = 9.80
# x 42 x (WT/42)^0.463 mg/day: both equal the per-kilogram values at the
# typical 42 kg, and V and Vmax scale together, so the steady state depends on
# weight only through Vmax.  The other reading the abstract allows, a power
# on the per-kilogram value, would make volume per kilogram RISE with weight,
# which no phenytoin study has found.  At 70 kg this gives V 65.5 L (0.94
# L/kg) and Vmax 522 mg/day (7.5 mg/kg/day).
#
# WHY THIS SOURCE, AND ITS LIMITS
# ===============================
# The ChatGPT handoff proposed a free-drug model (Cheng S et al., Drugs R D
# 2020;20:343-357, PMC7691416): 37 adults, Vmax 5.36 mg/kg/day and Km 0.532
# mg/L on UNBOUND phenytoin, with a linear binding model on albumin, and ka
# (0.225 /h) and F (0.859) fixed.  Its parameters are on the unbound scale and
# need its binding model to give the total concentration that is measured and
# banded; Odani's are on the total scale and need nothing else.  Odani is
# used, and Cheng's fixed ka is taken for the extended-release capsule below.
# Alqahtani S et al. (Pharmacology 2019;104:60-66), 43 Saudi adults, found
# V 0.61 L/kg, Vmax 6.12 mg/kg/day and Km 5.33 mg/L: a cross-check, below.
#
# Odani's subjects were Japanese; the steady state rests on Vmax and Km, which
# were estimated from troughs alone, so the shape within a dosing interval and
# the absorption are not from this source.  The handoff's own wording applies:
# an illustrative historical total-concentration model, not a universal US
# default.  The registry says so.
#
# CHECKS (70 kg, 40 y man, normal CYP2C9)
# =======================================
#     daily dose of phenytoin acid   Css = Km R / (Vmax - R)
#       200 mg/day                    5.7 mg/L
#       300 mg/day                   12.4 mg/L
#       400 mg/day                   30.2 mg/L
# Typical adults reach 10-20 mg/L on 300-400 mg/day with a disproportionate
# rise between them, the textbook observation.  Alqahtani's adults (65 kg, 330
# mg/day) had a mean trough of 11.2 mg/L; this model gives 15.4 mg/L at their
# mean dose and weight (their own parameters give 10.5).  An intravenous load
# of 18 mg/kg of phenytoin sodium gives 17.6 mg/L at the end of the load
# (1159 mg x 0.92 / 65.5 L, less what is cleared in the half hour).
#
# DOSE BASIS: ONE CONVERSION, AT INPUT
# ====================================
# Concentrations are phenytoin acid.  Each unit carries its own basis, applied
# once by the engine (saltFactor, R/advanceMichaelisMenten.R):
#
#   "mg", "mg/kg", "mg/min"     phenytoin SODIUM injection      x 0.92
#   "mg PO ER"                  phenytoin SODIUM extended       x 0.92
#                               capsules (Dilantin Kapseals)
#   "mg PO liquid"              phenytoin ACID suspension       x 1
#   "mg PO tablet"              phenytoin ACID chewable tablet  x 1
#                               (Infatabs)
#   "mg PE", "mg/kg PE",        FOSPHENYTOIN, prescribed in     x 0.92
#   "mg PE/min", "mg PE IM"     phenytoin sodium equivalents
#
# 0.92 is the ratio of molecular weights, 252.27 / 274.25 = 0.9199.  A dose of
# fosphenytoin is entered in PE, never as mg of fosphenytoin (1.5 mg of
# fosphenytoin sodium is 1 mg PE).
#
# ORAL ABSORPTION
# ===============
# Odani assumed F = 1 and estimated no absorption rate (steady-state troughs).
#   - ER capsule: ka 0.225 /h, FIXED by Cheng 2020.  A single 300 mg dose
#     then peaks at about 11 h in the reference patient, inside the label's 4
#     to 12 h for the extended capsule.  Bioavailability 1 (on Odani's scale).
#   - Suspension and chewable tablet: no population estimate exists.  The
#     label gives the peak at 1.5 to 3 h; ka 2.0 /h puts it at 2.4 h for 300
#     mg in the reference patient (1.0 /h would put it at 4.1 h).  This
#     value is a LABEL-ANCHORED calibration, not an estimate, and is recorded
#     so in the registry's parameter audit.  Bioavailability 1.
#
# FOSPHENYTOIN
# ============
# Not a separate drug in the library: its doses are a route of phenytoin, so
# that the saturable elimination sees every source of phenytoin at once.
#   - conversion half-life 15 min (Cerebyx label; Boucher 1989 measured 8.0
#     +/- 2.9 min in 10 adults, Fischer 2003 reviews 7-15 min);
#   - intramuscular: complete (Boucher 1989, 100.5 +/- 20.3%), absorbed at
#     2.47 /h (Boucher 1989) into the same conversion compartment.
# The early displacement of phenytoin from albumin by fosphenytoin, which
# raises the unbound fraction for the first hour, is not modelled: the curve is
# total phenytoin.
#
# CYP2C9
# ======
# Odani A et al. (Clin Pharmacol Ther 1997;62:287-292), 44 Japanese patients:
# Vmax 33% lower in *1/*3 heterozygotes (CPIC activity score 1).  That is the
# "intermediate" phenotype here (CYP2C9_VALUES), x 0.67.  For POOR
# metabolisers (activity score 0 or 0.5) no Vmax was estimated in a population
# model the session could retrieve; CPIC (Karnes 2021) advises a maintenance
# dose at least 50% lower, which for a drug whose steady-state dose rate is
# proportional to Vmax is Vmax x 0.5.  That factor is GUIDELINE-ANCHORED, not
# an estimate, and is recorded so in the parameter audit.  CYP2C19 reduced
# Vmax by up to 14% in Odani 1997, not significant on its own, and is not
# applied.  No CYP2D6 term: none is established for phenytoin.
#
# NOT MODELLED
# ============
# Albumin and the unbound fraction (the curve is total; in hypoalbuminaemia or
# renal failure a total of 10 mg/L may carry a therapeutic unbound level),
# valproate displacement, enzyme induction or inhibition by co-medication,
# zonisamide (Km x 1.16 in Odani), pregnancy, saturable absorption of large
# oral doses, and the time until threshold (the engine does not compute it).
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# The model carries its own weight covariate.  It is evaluated at the
# pharmacokinetic weight with the switch on and at total body weight with it
# off.
#
# BAND
# ====
# 10-20 mcg/mL total (ILAE, Patsalos 2008), typical 15.  Unbound 1-2 mcg/mL
# is the same range at normal binding.
#
# References
# ----------
# Odani A et al., Biol Pharm Bull 1996;19:444-448.
#   https://doi.org/10.1248/bpb.19.444
# Odani A et al., Clin Pharmacol Ther 1997;62:287-292.
#   https://doi.org/10.1016/S0009-9236(97)90031-X
# Karnes JH et al. (CPIC), Clin Pharmacol Ther 2021;109:302-309.
#   https://doi.org/10.1002/cpt.2008
# Cheng S et al., Drugs R D 2020;20:343-357.
#   https://doi.org/10.1007/s40268-020-00323-2
# Alqahtani S et al., Pharmacology 2019;104:60-66.
#   https://doi.org/10.1159/000500314
# Boucher BA et al., J Pharm Sci 1989;78:929-932.
#   https://doi.org/10.1002/jps.2600781110
# Fischer JH et al., Clin Pharmacokinet 2003;42:33-58.
#   https://doi.org/10.2165/00003088-200342010-00002
# Patsalos PN et al., Epilepsia 2008;49:1239-1276.
#   https://doi.org/10.1111/j.1528-1167.2008.01561.x
# Dilantin, Dilantin-125, Dilantin Infatabs, phenytoin sodium injection and
# Cerebyx (fosphenytoin) US prescribing information, section 12.3.
# -----------------------------------------------------------------------------

#' Mass of phenytoin acid in a unit mass of phenytoin sodium
PHENYTOIN_SODIUM_FRACTION <- 252.27 / 274.25

#' Vmax multiplier by CYP2C9 phenotype; see the header of R/drugs_phenytoin.R
PHENYTOIN_CYP2C9_VMAX <- c(normal = 1, intermediate = 0.67, poor = 0.5)

#' Phenytoin pharmacokinetics (intravenous, oral and as fosphenytoin)
#'
#' Odani et al. (1996): one compartment with Michaelis-Menten elimination, on
#' total phenytoin; simulated by the numerical engine
#' (\code{advanceMichaelisMenten()}).  See the header of the file.
#'
#' @inheritParams cefazolin
#' @param cyp2c9 CYP2C9 phenotype, one of \code{CYP2C9_VALUES}
#' @returns a list in the shape \code{getDrugPK()} expects, with a
#'   \code{michaelisMenten} block
#' @export
phenytoin <- function(weight, height, age, sex, adjustToFFM = TRUE,
                      cyp2c9 = CYP2C9_DEFAULT)
{
  if (length(cyp2c9) != 1 || !cyp2c9 %in% CYP2C9_VALUES)
    stop("Invalid cyp2c9 for phenytoin: ", paste(cyp2c9, collapse = ", "))
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)
  pkW  <- if (isTRUE(adjustToFFM)) size$pkWeight else weight

  # Odani 1996: per-kilogram values at the typical 42 kg, weight^0.463
  scale <- 42 * (pkW / 42)^0.463
  V    <- 1.23 * scale                                    # L
  vmax <- 9.80 * scale * PHENYTOIN_CYP2C9_VMAX[[cyp2c9]] / MINS_PER_DAY   # mg/min
  km   <- 9.19                                            # mg/L, total

  default <- list(
    v1 = V, v2 = 1, v3 = 1,
    # The low-concentration limit, Vmax / Km, for the help tables only: the
    # simulation uses the michaelisMenten block.
    cl1 = vmax / km, cl2 = 0, cl3 = 0,
    ka_PO = 0.225 / 60,          # ER capsule, Cheng 2020 (fixed)
    bioavailability_PO = 1,
    tlag_PO = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  # Suspension and chewable tablet: label-anchored ka (see the header)
  fast <- list(ka_PO = 2.0 / 60, bioavailability_PO = 1, tlag_PO = 0)

  list(
    PK = PK,
    tPeak = 0,
    MEAC = 0,
    typical = 15,
    upperTypical = 20,
    lowerTypical = 10,
    reference = paste0(
      "Odani A et al., Biol Pharm Bull 1996;19:444-448 (one compartment, ",
      "Michaelis-Menten elimination of total phenytoin, Japanese patients); ",
      "CYP2C9 from Odani 1997 and CPIC 2021; fosphenytoin conversion from the ",
      "Cerebyx label and Boucher 1989. https://doi.org/10.1248/bpb.19.444"
    ),
    oralFormulations = list(liquid = fast, tablet = fast),
    michaelisMenten = list(
      vmax = vmax,
      km = km,
      kConversion = log(2) / 15,       # fosphenytoin, 1/min
      ka_IM = 2.47 / 60,               # fosphenytoin IM, 1/min
      bioavailability_IM = 1,
      saltFactor = list(
        IV      = PHENYTOIN_SODIUM_FRACTION,
        PE      = PHENYTOIN_SODIUM_FRACTION,
        default = PHENYTOIN_SODIUM_FRACTION,   # ER sodium capsule
        liquid  = 1,
        tablet  = 1
      )
    )
  )
}
