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
# Body size: no weight covariate, so the published values are the cohort's
# typical patient's (median 80 kg, BMI 27, 67 years, 12 men and 10 women).
# With the switch on they are anchored at that patient (Steven L. Shafer, 2026-11-18;
# docs/weight-adjustment.md, Anchoring at the source's cohort); the reference man's
# factors are 0.995 and 0.996, so he barely moves.  With it off, the published
# values (legacyVolume = 1).
#
# EFFECT SITE
# ===========
# Li established no concentration-to-delirium, sedation or QTc relation, and
# the oral chronic PANSS model must not be reused here.  The D2-occupancy
# effect site of Lim 2013 (ke0 0.213 /h, Ce50 0.791 ng/mL putamen; see
# R/drugs_haloperidol.R) relates plasma haloperidol to occupancy whatever the
# route, so ke0 is supplied here too.  Lim studied oral dosing in healthy
# men; its use after an intravenous bolus in critically ill patients is a
# transfer, recorded in antipsychoticProfiles().  CYP2D6 is not applied:
# Li tested it on this model and it was not significant.
# -----------------------------------------------------------------------------

HALOPERIDOL_IV_CL <- 51.7    # L/h, absolute
HALOPERIDOL_IV_V  <- 1490    # L

# Li 2022, Table 1 (p. 4), read on the PDF 2026-11-18: the 22 patients'
# medians, 80 kg, BMI 27 (no height printed; 172 cm from the two), 67 years,
# 12 men and 10 women.  The model is anchored at this cohort
# (docs/weight-adjustment.md, Anchoring at the source's cohort).
HALOPERIDOL_IV_COHORT <- list(weight = 80, bmi = 27, age = 67, men = 12, women = 10)

#' Haloperidol pharmacokinetics (intravenous)
#'
#' Li et al. (2022): one compartment, absolute parameters from critically ill
#' adults, with Lim 2013's D2-occupancy effect site; the CRP covariate is
#' not applied.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
haloperidolIV <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1, cohort = do.call(cohortSizeFactors, HALOPERIDOL_IV_COHORT))

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
    "adults; CRP covariate not applied); effect site: Lim HS et al., ",
    "Pharm Res 2013;30:683-693"
  )

  list(
    PK = PK,
    tPeak = 0,                       # ke0 is supplied, not solved
    ke0 = HALOPERIDOL_KE0 / 60,      # 1/min, Lim 2013
    MEAC = 0,
    typical = 0,
    upperTypical = 0,
    lowerTypical = 0,
    reference = reference
  )
}
