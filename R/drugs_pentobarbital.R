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
# J Clin Pharmacol 2026 (PMC13145311): critically ill children given
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
# 1.4-4.9 L/h at 70 kg; head-injured adults 0.043 L/h/kg, quoted in both
# papers' discussions): an adult's levels during a long coma may be
# underpredicted.  The help page says so.
#
# CHECKS (70 kg)
# ==============
# Half-lives about 1.5 h (redistribution) and 23 h (terminal); a 10 mg/kg
# load over an hour followed by 1 mg/kg/h reaches about 20 mg/L in 24 h,
# inside the 20-40 mg/L reported for burst suppression.
#
# BODY SIZE: the model carries its own weight covariate (allometric total
# weight), evaluated at the pharmacokinetic weight with the switch on.
#
# BAND: 20-40 mcg/mL, the concentrations reported during barbiturate coma for
# refractory status epilepticus (orientation: titration is to the EEG, not to
# a level), typical 30.
#
# References
# ----------
# J Clin Pharmacol 2026, pentobarbital in critically ill children.
#   https://doi.org/10.1002/jcph.70204
# Ketharanathan N et al., Clin Pharmacokinet 2023;62:1137-1149.
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
    typical = 30, upperTypical = 40, lowerTypical = 20,
    reference = paste0(
      "J Clin Pharmacol 2026 (critically ill children): two compartments, ",
      "allometric on weight, CL 5.21 L/h, V1 37.4 L, Q 18.1 L/h, V2 63.9 L at ",
      "70 kg; adults extrapolated. https://doi.org/10.1002/jcph.70204"
    )
  )
}
