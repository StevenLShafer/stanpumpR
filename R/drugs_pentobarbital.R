# -----------------------------------------------------------------------------
# Pentobarbital: two compartments, intravenous only
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in mcg/mL (= mg/L), total plasma.  Drafted by Claude Code,
# 2026-10-10, at the request of Steven L. Shafer; see the antiseizure
# registry, inst/extdata/antiseizureRegistry.csv.
#
# SOURCE
# ======
# Helfer VE et al., J Clin Pharmacol 2026 (PMC13145311): children given
# intravenous pentobarbital, two compartments, NONMEM, allometric total body
# weight with the exponents FIXED at 0.75 (CL, Q) and 1 (V1, V2).  At 70 kg,
# from the parameter table:
#
#     CL 5.21 L/h   V1 37.4 L   Q 18.1 L/h   V2 63.9 L
#
# No covariate beyond weight was retained (obesity on V1 entered and left).
#
# The handoff also named Ketharanathan et al. (Clin Pharmacokinet 2023,
# PMC10338388), children with status epilepticus or severe traumatic brain
# injury: one compartment, CL 3.59 L/h and V 142 L at 70 kg, with serum
# creatinine and C-reactive protein reducing clearance.  It needs CRP, which
# the app does not take, and is not merged with the 2026 model.
#
# ADULTS ARE EXTRAPOLATED
# =======================
# Both sources are paediatric.  An adult gets the allometric 70 kg values,
# which sit at the high end of adult clearance (healthy adults 0.02-0.07 L/h/kg,
# 1.4-4.9 L/h at 70 kg; head-injured adults 0.043 L/h/kg, quoted in Helfer's
# introduction): an adult's levels during a long coma may be underpredicted.
# The help page says so.
#
# CHECKS (70 kg)
# ==============
# Corrected 2026-10-28 (the former figures, half-lives of 1.5 and 23 h and
# about 20 mg/L at 24 h, did not describe this model).  Half-lives 0.81 h
# (redistribution) and 15.1 h (terminal).  Helfer's seizure regimen, 5 mg/kg
# then 1 mg/kg/h, gives 9.4 mg/L just after the load, 6.5 at 6 h, 10.4 at
# 24 h and 13.1 at 72 h, approaching the steady state of 70 / 5.21 = 13.4
# mg/L; a 10 mg/kg load over an hour then 1 mg/kg/h gives 14.2 at 1 h, 8.3
# at 6 h and 11.1 mg/L at 24 h.  Both sit inside the band.
#
# BODY SIZE: the model carries its own weight covariate (allometric total
# weight), evaluated at the pharmacokinetic weight with the switch on.
#
# BAND: 4-21 mcg/mL, for the TREATMENT OF STATUS EPILEPTICUS only: the
# serum concentrations "observed during burst-suppression coma", which
# Helfer's simulations take as the steady-state target for seizure control.
# Helfer cites Lowenstein DH, Aminoff MJ, Simon RP, Neurology 1988;38:395-400
# (barbiturate anaesthesia for status epilepticus, 14 patients), which has
# not been read: it was not available through Lane Library.  Helfer's other
# targets are for other uses and are not drawn: 1-5 mg/L (peak) for
# preoperative sedation, 5-15 mg/L for deep sedation, and above 45 mg/L
# potentially toxic (from intracranial-pressure management).  Typical 12.5,
# the midpoint, is stanpumpR's choice.  Titration is to the EEG, not to a
# level.  Drawn by decision of Steven L. Shafer, 2026-10-12 (applied
# 2026-10-28), replacing the unsourced 20-40 mg/L candidate; recorded in
# inst/extdata/therapeuticRanges.csv.
#
# References
# ----------
# Helfer VE, Medina-Aymerich L, Muller WJ, et al.  Population
#   pharmacokinetics and dosing simulations of pentobarbital in the pediatric
#   population.  J Clin Pharmacol 2026;66(5):e70204.
#   https://doi.org/10.1002/jcph.70204
# Lowenstein DH, Aminoff MJ, Simon RP.  Barbiturate anesthesia in the
#   treatment of status epilepticus: clinical experience with 14 patients.
#   Neurology 1988;38(3):395-400.  https://doi.org/10.1212/wnl.38.3.395
#   (Not read; Helfer's source for the band.)
# Ketharanathan N et al., Clin Pharmacokinet 2023;62:1011-1022.
#   https://doi.org/10.1007/s40262-023-01249-z
# -----------------------------------------------------------------------------

#' Pentobarbital pharmacokinetics (intravenous)
#'
#' Two compartments, allometric on weight (J Clin Pharmacol 2026); see the
#' header of the file.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
pentobarbital <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)
  w <- if (isTRUE(adjustToFFM)) size$pkWeight else weight
  a <- (w / 70)^0.75
  b <- w / 70

  default <- list(
    v1  = 37.4 * b, v2 = 63.9 * b, v3 = 1,
    cl1 = 5.21 * a / 60, cl2 = 18.1 * a / 60, cl3 = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  list(
    PK = PK, tPeak = 0, MEAC = 0,
    # Band: status epilepticus, 4-21 mg/L (Helfer 2026); see the header.
    typical = 12.5, upperTypical = 21, lowerTypical = 4,
    reference = paste0(
      "Helfer VE et al., J Clin Pharmacol 2026 (children): two compartments, ",
      "allometric on weight, CL 5.21 L/h, V1 37.4 L, Q 18.1 L/h, V2 63.9 L at ",
      "70 kg; adults extrapolated. https://doi.org/10.1002/jcph.70204"
    )
  )
}
