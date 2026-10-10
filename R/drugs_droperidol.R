# -----------------------------------------------------------------------------
# Droperidol, intravenous
# -----------------------------------------------------------------------------
# Provenance
# ----------
# Drafted by Claude Code, 2026-10-09, at the request of Steven L. Shafer, from
# the antipsychotic implementation brief.  Every value below was checked
# against the full text of Cooper 2018 (Table 2 and Table 3).
#
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in ng/mL (= mcg/L).
#
# THE SOURCE, AND WHY IT IS WEAK
# ==============================
# Cooper I, Landersdorfer CB, St John AG, Graudins A, SAGE Open Med
# 2018;6:2050312118813283: an intranasal/intravenous crossover in SEVEN healthy
# men (median 23 years, 73 kg).  The intravenous dose was 0.02 mg/kg over
# 10 min (mean 1.53 mg).  Joint two-compartment model, absolute IV branch:
#
#     CL 15.3 L/h, V1 3.16 L, Q 9.66 L/h, V2 16.9 L     (terminal t1/2 2.0 h)
#
# The FIRST intravenous sample was taken 15 min after the infusion ENDED, so the
# first 25 min of the curve are not observed at all, and the authors say the
# joint model underestimates intravenous clearance and probably overpredicts
# early concentrations.  Their own noncompartmental IV clearance was 33.8 L/h
# (median), and Fischler 1986 (anaesthetised patients, 150 mcg/kg) reported
# 14.1 mL/min/kg (about 59 L/h at 70 kg) and Vd-beta 2.04 L/kg, against
# Vss = 20 L here.  Read the early curve after a bolus as an upper bound on
# concentration, not a prediction.  The noncompartmental clearance is a
# sensitivity benchmark only; it is not substituted into this parameter set,
# because CL, V1, Q and V2 were estimated together.
#
# The intranasal ka, lag and F from the same table are not used.
# Intramuscular droperidol is the separate drug droperidolIM (Foo 2016).
#
# Body size: no weight covariate, so the published values are the reference
# adult's and scale to fat-free mass (legacyVolume = 1).
#
# EFFECT
# ======
# None.  No validated concentration-to-sedation, antiemetic or QTc relation
# exists; the published QTc data are dose-group changes (Lischke 1994,
# Charbit 2008), not a concentration-effect model.
# -----------------------------------------------------------------------------

DROPERIDOL_CL <- 15.3    # L/h, absolute
DROPERIDOL_V1 <- 3.16    # L
DROPERIDOL_Q  <- 9.66    # L/h
DROPERIDOL_V2 <- 16.9    # L
# Unobserved interval after the START of the source's 10 min infusion: the
# infusion plus the 15 min before the first sample.
DROPERIDOL_UNOBSERVED_MIN <- 25

#' Droperidol pharmacokinetics (intravenous)
#'
#' Cooper et al. (2018): two compartments, absolute intravenous branch of a
#' joint intranasal/intravenous fit in seven healthy men.  The first 25 min
#' after a dose were not sampled; see the file's header.  Plasma only.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
droperidol <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1)

  default <- list(
    v1 = DROPERIDOL_V1 * size$volume,
    v2 = DROPERIDOL_V2 * size$volume,
    v3 = 1,
    cl1 = DROPERIDOL_CL / 60 * size$clearance,   # L/min
    cl2 = DROPERIDOL_Q  / 60 * size$clearance,
    cl3 = 0
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Cooper I et al., SAGE Open Med 2018;6:2050312118813283. ",
    "https://doi.org/10.1177/2050312118813283 (intravenous branch, 7 healthy ",
    "men; no samples in the first 25 min, clearance likely underestimated)"
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
