# -----------------------------------------------------------------------------
# Phenylephrine, intravenous (Hengstmann 1982): serum phenylephrine in
# healthy volunteers
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min.
#
# Drafted by Claude Code, 2026-10-19, at the request of Steven L. Shafer, as
# one of the vasoactive and resuscitation drugs used in intensive care
# (inst/help/models/vasoactive.md, inst/extdata/vasoactiveRegistry.csv).  Every
# number below was read from the published article, which he supplied:
# Table 3 and the Results.
#
# THE SOURCE
# ==========
# Hengstmann JH, Goronzy J.  Pharmacokinetics of 3H-phenylephrine in man.
# Eur J Clin Pharmacol 1982;21:335-341 (doi 10.1007/BF00637623).  Four
# healthy volunteers given 0.84 +/- 0.17 mg of tritiated phenylephrine
# intravenously over 12.5 to 20 minutes; free phenylephrine separated from its
# conjugates by column chromatography and followed for 8 hours; an open
# two-compartment model fitted to each subject.  Table 3 gives each subject's
# k12, k21, kel and V1 and the derived Vdss and total clearance:
#
#     subject   V1 (L)   k12      k21      kel     (/min)   Vdss (L)  CL (mL/min)
#     He        38.2     0.1044   0.0102   0.0543           426.3     2076
#     Al        60.8     0.0899   0.0113   0.0534           543.2     3252
#     Se        25.7     0.1019   0.0165   0.0572           184.1     1474
#     Gr        39.8     0.0729   0.0174   0.0396           206.3     1580
#
# Every subject's printed clearance and Vdss follow from his rate constants
# and central volume to within rounding, which the test checks.
#
# THE TYPICAL SUBJECT
# ===================
# As for verapamil (R/drugs_verapamil.R), the model is the arithmetic mean of
# the four subjects' V1, V2 = V1 k12/k21, CL = V1 kel and Q = V1 k12:
#
#     V1 41.1 L,  V2 300 L,  CL 2.09 L/min,  Q 3.74 L/min
#
# which reproduces the paper's mean clearance (2095 mL/min) and Vdss (340 L)
# exactly, and gives half-lives of 4.6 and 164 minutes against the paper's
# mean alpha and beta of 0.1527 and 0.0046 /min (4.5 and 151 min).  The
# central volume is about the extracellular volume, as the authors note.
#
# THE OTHER MODEL, AND WHY NOT IT
# ===============================
# Anderson 2017 (Paediatr Anaesth 27:1050) pooled nasal and intravenous data
# into a two-compartment model with a 7-hour terminal half-life, labelled for
# the nasal formulation and built mostly from children.  This adult
# intravenous study, sampled to 8 hours, is the more direct source for an
# intravenous infusion; the terminal phase is in any case a small share of
# the area.
#
# DOSE BASIS
# ==========
# The paper gives the dose in mg of phenylephrine without naming the salt.
# Phenylephrine injection is labelled as the hydrochloride, so a dose is
# entered as labelled; if the source's milligrams were the base, every
# concentration here is 22% high (203.67 / 167.21).
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# Four adults whose weights are not reported: fixed values, scaled to
# fat-free mass with the switch on and left as published with it off
# (legacyVolume = 1).
#
# NO EFFECT SITE, NO BAND
# =======================
# Phenylephrine's pressor effect is immediate, but the source fitted no
# concentration-effect relation, and no pressure is predicted here.
# -----------------------------------------------------------------------------

# Hengstmann 1982, Table 3, subjects He, Al, Se, Gr
PHENYLEPHRINE_HENGSTMANN <- data.frame(
  V1  = c(38.2, 60.8, 25.7, 39.8),         # L
  k12 = c(0.1044, 0.0899, 0.1019, 0.0729), # /min
  k21 = c(0.0102, 0.0113, 0.0165, 0.0174),
  kel = c(0.0543, 0.0534, 0.0572, 0.0396)
)

#' The typical phenylephrine model from Hengstmann 1982
#'
#' The arithmetic mean of the four subjects' V1, V2, CL and Q.  See the
#' header of \code{R/drugs_phenylephrine.R}.
#'
#' @returns a named numeric vector, L and L/min
#' @keywords internal
phenylephrineTypical <- function(k = PHENYLEPHRINE_HENGSTMANN)
{
  colMeans(cbind(
    v1  = k$V1,
    v2  = k$V1 * k$k12 / k$k21,
    cl1 = k$V1 * k$kel,
    cl2 = k$V1 * k$k12))
}

#' Phenylephrine, intravenous (Hengstmann 1982)
#'
#' Two compartments, the mean of four healthy volunteers; see the header of
#' \code{R/drugs_phenylephrine.R}.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
phenylephrine <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1)
  p <- phenylephrineTypical()

  default <- list(
    v1  = p[["v1"]]  * size$volume,
    v2  = p[["v2"]]  * size$volume,
    v3  = 1,                          # placeholder: a two-compartment model
    cl1 = p[["cl1"]] * size$clearance,
    cl2 = p[["cl2"]] * size$clearance,
    cl3 = 0
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Hengstmann JH, Goronzy J, Eur J Clin Pharmacol 1982;21:335-341. ",
    "Two compartments, the mean of 4 healthy volunteers' IV fits: V1 41.1 L, ",
    "V2 300 L, CL 2.09 L/min, Q 3.74 L/min (the paper's mean CL 2095 mL/min ",
    "and Vdss 340 L). Doses as the hydrochloride. ",
    "https://doi.org/10.1007/BF00637623"
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
