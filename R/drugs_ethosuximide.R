# -----------------------------------------------------------------------------
# Ethosuximide: two compartments, apparent oral
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in mcg/mL (= mg/L), total plasma.  Drafted by Claude Code,
# 2026-10-10, at the request of Steven L. Shafer; see the antiseizure
# registry, inst/extdata/antiseizureRegistry.csv.
#
# SOURCE
# ======
# Diezi L et al. (Pharmacol Res Perspect 2023;11:e01032, PMC9764106): 12
# healthy adults (19-40 y, 56-87 kg), single 10 mg/kg doses of Zarontin syrup
# and two granule formulations in crossover; two compartments, NONMEM.
# Table 2, apparent (relative to syrup):
#
#     CL/F 0.569 L/h   Vc/F 31.3 L   Q/F 10.2 L/h   Vp/F 13.9 L
#     ka 5.59 /h (syrup), 2.06 /h (granules, not a US product)
#
# with a LINEAR weight term on one parameter (most likely Vc), 1 + 1.71 (WT -
# 67.2) / 67.2, which turns negative below 28 kg.
#
# ADAPTATIONS (each recorded in the registry)
# ===========================================
# 1. Size.  The linear weight term cannot be extrapolated (a 20 kg child would
#    have a negative volume), so it is replaced by the library's fat-free-mass
#    scaling of every volume and clearance; with the switch off, the
#    published 67 kg values are used for everyone (legacyVolume = 1).  The
#    paediatric model that matters for absence epilepsy (Mizuno 2023, the CAE
#    trial, 211 children) was not retrievable; children are extrapolated.
# 2. Capsules.  The US capsule has no published absorption rate; it is given
#    the syrup's ka.  Capsule and syrup are both labelled as immediate
#    release; the peak of a capsule may be later than plotted.
#
# CHECKS
# ======
# Diezi's own syrup data, 10 mg/kg (n = 6): peak 18.2 mg/L at 0.5 h, AUC
# 1210 mg.h/L, half-life 57 h; the model gives 20 mg/L at 0.6 h for a 70 kg
# man and a terminal half-life of 54 h.  Warren 1980, 250 mg twice daily in
# healthy adults: 32.2 +/- 5.6 mg/L at steady state; the model gives 36.6.
# Children's half-life is about 30 h (Buchanan 1976): the fat-free-mass
# scaling gives about 37 h at 20 kg.
#
# PHARMACODYNAMICS NOT PLOTTED.  Mizuno 2023 (CAE trial) related AUC to
# seizure freedom (AUC 1027 and 1489 mcg.h/mL for 50% and 75% probability),
# but the full logistic equation is not retrievable; no seizure probability is
# drawn.
#
# BAND: 40-100 mcg/mL (ILAE, Patsalos 2008), typical 70.
#
# References
# ----------
# Diezi L et al., Pharmacol Res Perspect 2023;11:e01032.
#   https://doi.org/10.1002/prp2.1032
# Warren JW et al., Clin Pharmacol Ther 1980;28:646-651.
#   https://doi.org/10.1038/clpt.1980.216
# Buchanan RA et al., Clin Pharmacol Ther 1976;19:143-147.
#   https://doi.org/10.1002/cpt1976192143
# Mizuno T et al., Clin Pharmacol Ther 2023;114:459-469.
#   https://doi.org/10.1002/cpt.2965
# -----------------------------------------------------------------------------

#' Ethosuximide pharmacokinetics (oral)
#'
#' Diezi et al. (2023), two compartments, with fat-free-mass scaling in place
#' of the source's linear weight term; see the header of the file.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
ethosuximide <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1)

  default <- list(
    v1  = 31.3 * size$volume, v2 = 13.9 * size$volume, v3 = 1,
    cl1 = 0.569 * size$clearance / 60, cl2 = 10.2 * size$clearance / 60, cl3 = 0,
    ka_PO = 5.59 / 60,
    bioavailability_PO = 1,     # apparent (relative to syrup)
    tlag_PO = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  list(
    PK = PK, tPeak = 0, MEAC = 0,
    typical = 70, upperTypical = 100, lowerTypical = 40,
    reference = paste0(
      "Diezi L et al., Pharmacol Res Perspect 2023;11:e01032. Two ",
      "compartments, apparent oral (syrup), healthy adults; fat-free-mass ",
      "scaling in place of the source's linear weight term. ",
      "https://doi.org/10.1002/prp2.1032"
    )
  )
}
