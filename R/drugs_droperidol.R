# -----------------------------------------------------------------------------
# Droperidol, intravenous
# -----------------------------------------------------------------------------
# Provenance
# ----------
# Drafted by Claude Code, 2026-10-09, at the request of Steven L. Shafer.
# Rebuilt on 2026-10-11 from the full texts Dr Shafer supplied: Lehmann 1988
# (the model), Fischler 1986, Gupta 1992, Grunwald 1993 and Cooper 2018
# (comparisons).  Every number below was read from those papers.
#
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in ng/mL (= mcg/L).
#
# THE MODEL: LEHMANN 1988, POOLED OVER THREE DOSES
# ================================================
# Lehmann KA, van Peer A, Ikonomakis M, Gasparini R, Heykants J, Br J Anaesth
# 1988;61:297-301: 30 ASA I-II surgical adults under neuroleptanalgesia, 14 to
# 65 years, 48 to 90 kg (group means 65, 73 and 65 kg), given 5, 10 or 15 mg
# intravenously (10 each), plasma by radioimmunoassay from 1 to 360 min.
# Kinetics were linear over the three doses and unrelated to age or weight.
# Fig. 1 gives the triexponential fitted to each dose's mean curve (ng/mL):
#
#      5 mg   697 e^-0.54t + 41.5 e^-0.028t + 26.5 e^-0.0047t
#     10 mg   661 e^-0.30t + 73.1 e^-0.025t + 56.2 e^-0.0053t
#     15 mg   853 e^-0.38t + 83.2 e^-0.031t + 87.0 e^-0.0053t
#
# The three curves, each divided by its dose, were evaluated at the study's
# 15 sampling times and fitted together with one triexponential (equal
# weight on the log scale): per mg, 80.18 e^-0.4050t + 7.202 e^-0.02888t
# + 5.664 e^-0.005152t ng/mL.  Converted to a three-compartment model:
#
#     V1 10.75 L; k10 0.06015, k12 0.2298, k13 0.06160, k21 0.07393,
#     k31 0.01355 /min
#     CL 0.646 L/min, Vss 93.0 L, half-lives 1.7, 24 and 135 min
#
# This is the clearance of the mean curve.  Lehmann's mean of the individual
# clearances is higher, 798 mL/min (Vss 111 L, half-life 127 min), as the
# mean of individual values usually is; the mean curve is what a typical
# patient's concentrations follow.
#
# AGREEMENT WITH THE OTHER SOURCES
# ================================
# - Gupta SK, Southam M, Hwang S, Pharm Res 1992;9:694-696: eight healthy
#   men, 3 mg as a 24 h infusion (125 mcg/h).  Noncompartmental clearance
#   39.4 L/h (657 mL/min); plateau about 3.2 ng/mL.  This model predicts a
#   steady state of 3.22 ng/mL for that infusion.
# - Cooper 2018 (seven healthy men, 0.02 mg/kg): noncompartmental clearance
#   33.8 L/h (563 mL/min).  Its compartmental IV branch, used here first,
#   had no sample in the first 25 min and a Vss of about 20 L, and is not
#   used.
# - Fischler M et al., Anesthesiology 1986;64:486-489: ten anaesthetised
#   adults, 150 mcg/kg; means of individual three-compartment fits give
#   14.1 mL/min/kg (about 0.94 L/min at their 66 kg) and Vss 1.40 L/kg
#   (93 L).  The volumes agree with Lehmann's; the clearance is the highest
#   of the adult studies.
# - Grunwald Z et al., Anesth Analg 1993;76:1238-1242: twelve children of 3.5
#   to 12 years, 0.05 mg/kg: 4.66 mL/kg/min and Vss 0.58 L/kg, half-life
#   101 min (noncompartmental).  The fat-free-mass scaling of this adult
#   model gives a 20 kg child 13 mL/kg/min and 1.4 L/kg: about 2.5 times the
#   measured clearance and volume (the half-life is similar).  In children
#   the model overstates both, and its concentrations after a mg/kg dose
#   will be too LOW by about that factor.
#
# Intramuscular droperidol is the separate drug droperidolIM (Foo 2016),
# whose parameters are apparent; neither set may be moved to the other route.
#
# Body size: Lehmann found no relation to weight (48 to 90 kg) or age, so the
# pooled values are read as the 70 kg reference adult's and scale to fat-free
# mass (legacyVolume = 1, fixed values with the switch off).
#
# EFFECT
# ======
# None.  No validated concentration-to-sedation, antiemetic or QTc relation
# exists; the published QTc data are dose-group changes (Lischke 1994,
# Charbit 2008), not a concentration-effect model.  All of these authors note
# that the two-hour elimination half-life does not explain droperidol's long
# clinical action.
# -----------------------------------------------------------------------------

DROPERIDOL_V1  <- 10.75     # L
DROPERIDOL_K10 <- 0.06015   # 1/min
DROPERIDOL_K12 <- 0.2298    # 1/min
DROPERIDOL_K13 <- 0.06160   # 1/min
DROPERIDOL_K21 <- 0.07393   # 1/min
DROPERIDOL_K31 <- 0.01355   # 1/min

#' Droperidol pharmacokinetics (intravenous)
#'
#' Lehmann et al. (1988): three compartments, from the mean plasma curves of
#' 30 surgical adults given 5, 10 or 15 mg, pooled per milligram.  Plasma
#' only.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
droperidol <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1)

  default <- list(
    v1 = DROPERIDOL_V1 * size$volume,
    v2 = DROPERIDOL_V1 * DROPERIDOL_K12 / DROPERIDOL_K21 * size$volume,
    v3 = DROPERIDOL_V1 * DROPERIDOL_K13 / DROPERIDOL_K31 * size$volume,
    cl1 = DROPERIDOL_V1 * DROPERIDOL_K10 * size$clearance,   # L/min
    cl2 = DROPERIDOL_V1 * DROPERIDOL_K12 * size$clearance,
    cl3 = DROPERIDOL_V1 * DROPERIDOL_K13 * size$clearance
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Lehmann KA et al., Br J Anaesth 1988;61:297-301. ",
    "https://pubmed.ncbi.nlm.nih.gov/3179149/ (mean curves of 30 surgical adults ",
    "given 5, 10 or 15 mg IV, pooled per mg; checked against Gupta 1992, ",
    "Fischler 1986 and Cooper 2018)"
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
