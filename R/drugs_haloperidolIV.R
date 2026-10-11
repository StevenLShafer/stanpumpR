# -----------------------------------------------------------------------------
# Haloperidol IV: intravenous, in critically ill adults
# -----------------------------------------------------------------------------
# Provenance
# ----------
# Drafted by Claude Code, 2026-10-09, at the request of Steven L. Shafer, from
# the antipsychotic implementation brief.  CL, V and the interindividual
# variability were checked against the full text of Li 2022 (its tables were
# not readable; the values are stated in the text).
#
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in ng/mL.
#
# WHY A SEPARATE DRUG
# ===================
# "haloperidol" (R/drugs_haloperidol.R) runs an oral model whose parameters are
# divided by an unmeasured F.  This one is ABSOLUTE, fitted to intravenous
# data only, so the two cannot be one drug with two routes (the same reasoning
# as amiodarone and amiodaroneIV).
#
# THE SOURCE
# ==========
# Li L et al., Pharmaceutics 2022;14:549: 22 critically ill adults (median age
# 67, 48 to 77) treated for delirium with 1 mg intravenous haloperidol every
# 8 h (0.5 mg from age 80, 2 mg when agitated); 139 samples.  One compartment:
#
#     CL 51.7 L/h,  V 1490 L      (t1/2 20 h)
#
# Weight, age, sex and CYP2D6 were tested and rejected.  The final model has a
# C-reactive protein effect on CL (higher CRP, lower clearance, flattening
# above about 100 mg/L) whose exact function is in a supplement that was not
# available, so it is NOT applied: this is the typical patient's clearance.
# A one-compartment fit to samples taken hours apart will not show the
# distribution phase right after a bolus.
#
# Body size: no weight covariate, so the published values are the 70 kg
# reference adult's and scale to fat-free mass (legacyVolume = 1).
#
# EFFECT
# ======
# None.  The paper established no concentration-to-delirium, sedation, D2 or
# QTc relation, and the oral chronic PANSS model must not be reused here.
# -----------------------------------------------------------------------------

HALOPERIDOL_IV_CL <- 51.7    # L/h, absolute
HALOPERIDOL_IV_V  <- 1490    # L

#' Haloperidol pharmacokinetics (intravenous)
#'
#' Li et al. (2022): one compartment, absolute parameters from critically ill
#' adults.  Plasma only; the CRP covariate is not applied.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
haloperidolIV <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1)

  default <- list(
    v1 = HALOPERIDOL_IV_V * size$volume,
    v2 = 1,                                          # one compartment
    v3 = 1,
    cl1 = HALOPERIDOL_IV_CL / 60 * size$clearance,   # L/min
    cl2 = 0,
    cl3 = 0
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Li L et al., Pharmaceutics 2022;14:549. ",
    "https://doi.org/10.3390/pharmaceutics14030549 (intravenous, critically ill ",
    "adults; CRP covariate not applied)"
  )

  list(
    PK = PK,
    tPeak = 0,
    MEAC = 0,
    typical = 0,
    upperTypical = 0,
    lowerTypical = 0,
    reference = reference
  )
}
