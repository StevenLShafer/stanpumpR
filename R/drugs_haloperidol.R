# -----------------------------------------------------------------------------
# Haloperidol, oral only
# -----------------------------------------------------------------------------
# Provenance
# ----------
# Drafted by Claude Code, 2026-10-09, at the request of Steven L. Shafer, from
# the antipsychotic implementation brief.  The full text of Pilla Reddy 2013
# could NOT be read; the abstract confirms a two-compartment model in 122
# subjects.  The parameter values below are the brief's and are UNVERIFIED,
# although Li 2022 independently cites this model with CL/F 88 L/h and a total
# volume of 3169 L (= 669 + 2500), which matches them.
#
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in ng/mL.
#
# THE SOURCE
# ==========
# Pilla Reddy V et al., J Clin Psychopharmacol 2013;33:731-739: oral
# haloperidol in patients with schizophrenia.  Two compartments, first-order
# absorption, no covariates:
#
#     CL/F 88 L/h, Q/F 233 L/h, Vc/F 669 L, Vp/F 2500 L, ka 0.236 /h
#
# Half-lives 1.3 h and 31 h.  APPARENT parameters, so oral only with
# bioavailability_PO = 1.  Intravenous haloperidol is the separate drug
# haloperidolIV (Li 2022), whose ABSOLUTE clearance is not this one times F.
#
# Body size: no weight covariate, so the published values are the 70 kg
# reference adult's and scale to fat-free mass (legacyVolume = 1).
#
# EFFECT
# ======
# None plotted.  The same paper's PANSS model drives its drug term with each
# patient's AVERAGE STEADY-STATE concentration (Emax 0.31, EC50 3.58 ng/mL),
# inside a Weibull placebo and dropout model that has not been transcribed.
# It is not a moment-to-moment effect of C(t), so it is not an effect site and
# 3.58 ng/mL is not a target; it is recorded in antipsychoticProfiles().
# -----------------------------------------------------------------------------

HALOPERIDOL_CL <- 88      # L/h, apparent
HALOPERIDOL_Q  <- 233     # L/h
HALOPERIDOL_VC <- 669     # L
HALOPERIDOL_VP <- 2500    # L
HALOPERIDOL_KA <- 0.236   # 1/h

#' Haloperidol pharmacokinetics (oral)
#'
#' Pilla Reddy et al. (2013): two compartments, apparent oral parameters.
#' Plasma only.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
haloperidol <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1)

  default <- list(
    v1 = HALOPERIDOL_VC * size$volume,
    v2 = HALOPERIDOL_VP * size$volume,
    v3 = 1,
    cl1 = HALOPERIDOL_CL / 60 * size$clearance,   # L/min
    cl2 = HALOPERIDOL_Q  / 60 * size$clearance,
    cl3 = 0,
    ka_PO = HALOPERIDOL_KA / 60,                  # 1/min
    bioavailability_PO = 1,                       # apparent (/F)
    tlag_PO = 0
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Pilla Reddy V et al., J Clin Psychopharmacol 2013;33:731-739. ",
    "https://doi.org/10.1097/JCP.0b013e3182a4ee2c (apparent oral parameters; ",
    "values not yet checked against the full text)"
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
