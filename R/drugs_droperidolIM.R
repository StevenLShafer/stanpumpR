# -----------------------------------------------------------------------------
# Droperidol IM: intramuscular, in acutely agitated patients
# -----------------------------------------------------------------------------
# Provenance
# ----------
# Drafted by Claude Code, 2026-10-09, at the request of Steven L. Shafer, from
# the antipsychotic implementation brief.  CL/F, Vc/F, ka and the doses were
# checked against the abstract of Foo 2016; the full text was not available.
# Q/F and Vp/F are the brief's.  Together with CL/F and Vc/F they give
# half-lives of 0.31 and 3.0 h, matching the abstract's medians of 0.32 and
# 3.0 h, which supports them.
#
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in ng/mL (= mcg/L).
#
# THE SOURCE
# ==========
# Foo L-K, Duffull SB, Calver L, Schneider J, Isbister GK, Br J Clin Pharmacol
# 2016;82:1550-1556: 128 serum samples from 41 acutely agitated patients given
# 5 mg (17) or 10 mg (24) intramuscularly.  Two compartments, first-order
# absorption:
#
#     CL/F 41.9 L/h, Vc/F 73.6 L, Q/F 71.5 L/h, Vp/F 79.8 L, ka 10 /h (fixed)
#
# (The brief reports that the article's table labels the Vc row Vp; the
# abstract confirms 73.6 L is the central volume.)  IM bioavailability
# was not measured, so the parameters are APPARENT: droperidolIM is offered as
# mg IM only, with bioavailability_IM = 1.  Intravenous droperidol is the
# separate drug "droperidol" (Cooper 2018); neither model's parameters may be
# moved to the other route.
#
# Body size: weight was not recorded, so the published values are read as the
# 70 kg reference adult's and scale to fat-free mass (legacyVolume = 1).
#
# EFFECT
# ======
# None.  The data identify no sedation EC50, no ke0 and no QTc slope.
# -----------------------------------------------------------------------------

DROPERIDOL_IM_CL <- 41.9    # L/h, apparent
DROPERIDOL_IM_VC <- 73.6    # L
DROPERIDOL_IM_Q  <- 71.5    # L/h
DROPERIDOL_IM_VP <- 79.8    # L
DROPERIDOL_IM_KA <- 10      # 1/h, fixed in the source

#' Droperidol pharmacokinetics (intramuscular)
#'
#' Foo et al. (2016): two compartments, apparent intramuscular parameters from
#' acutely agitated patients.  Plasma only.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
droperidolIM <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1)

  default <- list(
    v1 = DROPERIDOL_IM_VC * size$volume,
    v2 = DROPERIDOL_IM_VP * size$volume,
    v3 = 1,
    cl1 = DROPERIDOL_IM_CL / 60 * size$clearance,   # L/min
    cl2 = DROPERIDOL_IM_Q  / 60 * size$clearance,
    cl3 = 0,
    ka_IM = DROPERIDOL_IM_KA / 60,                  # 1/min
    bioavailability_IM = 1,                         # apparent (/F)
    tlag_IM = 0
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Foo LK et al., Br J Clin Pharmacol 2016;82:1550-1556. ",
    "https://doi.org/10.1111/bcp.13093 (apparent intramuscular parameters, ",
    "acutely agitated patients)"
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
