# -----------------------------------------------------------------------------
# Intravenous immunoglobulin (IVIG): two compartments, intravenous
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min, amounts in
# g, concentrations in g/L.  Plotted as total serum IgG: the patient's own
# plus the IgG infused (R/factorBaselines.R; Steven L. Shafer, 2026-11-25).
# Drafted by Claude Code, 2026-11-25, at the request of Steven L. Shafer
# (docs/library-expansion/plan.csv, Biologics (sepsis)); digest card
# papers/digests/PMID_35781631.md.
#
# SOURCE
# ======
# Fokkink WJR, van Tilburg SJ, de Winter BCM, et al.  Population
# pharmacokinetic modelling of intravenous immunoglobulin treatment in
# patients with Guillain-Barre syndrome.  Clin Pharmacokinet
# 2022;61:1285-1296.  Read on the PDF.  Model building: 177 patients with
# Guillain-Barre syndrome (GBS) aged 5 to 89 years (median 53) and weighing
# 18 to 122 kg (median 72), 57% men, given Gammagard S/D 0.4 g/kg a day for 5
# days, and 15 patients with CIDP; 811 serum IgG levels.  External validation
# in 177 more patients with GBS.  NONMEM, FOCE-I (Methods 2, Table 1).
#
# Table 2 (final model) and the clearance equation (Results 3.1.2):
#
#     CL = 0.28 L/day x (WT/70)^0.75
#            x 1.31 (if methylprednisolone)
#            x 1.22 (GBS disability score 3-4) or 1.83 (score 5)
#     V1 = 2.87 L x WT/70           Q  = 0.25 L/day x (WT/70)^0.75
#     V2 = 2.65 L x WT/70
#     total IgG = CBAS + the infused, CBAS = 15.9 g/L x 0.65 in GBS = 10.3
#
# The allometric exponents are fixed (0.75 and 1).  Sex, age and the FCGRT
# VNTR genotype were tested and not kept.
#
# HELD AT THE REFERENCE (Shafer, 2026-11-25, accepting the recommendation):
# the disease-severity score (score 0-2, the reference, 1) and
# methylprednisolone (none), neither of which the Patient Profile can carry.
# A severely affected patient (score 5, ventilated) clears IVIG 1.83 times
# faster, so the curve is that of a mildly affected patient without steroids;
# the help page says so.  The plan files IVIG under sepsis; this source is
# GBS, and no sepsis model is in the archive.
#
# CHECKS (at 70 kg): half-lives 2.9 and 18.1 days.  GAMMAGARD S/D's label
# (12.3): radio-labelled IgG half-life 21 to 25 days in healthy people, 17.7
# to 37.6 days in immunodeficiency, 37.7 +/- 15 days for GAMMAGARD S/D in 10
# patients with primary immunodeficiency; and "high serum concentrations of
# IgG ... coincide with a shortened half-life", which a 2 g/kg course in GBS
# reaches.  Fokkink's simulation of 0.4 g/kg a day for 5 days in a typical
# 75 kg patient with a disability score of 4 peaks after the fifth dose and
# returns to baseline after about 4 weeks (Results, Outcomes).
#
# BASELINE: the Patient Profile's IgG baseline (g/L), added to the infused
# IgG.  Blank, the source population's, GBS, 10.3 g/L (15.9 x 0.65, Table
# 2; Shafer's decision of 2026-11-25).
#
# BAND: none.  No therapeutic range of serum IgG is established for IVIG in
# acute use; the source's target in GBS, a rise of at least 7.3 g/L above
# baseline at two weeks (from the group's earlier work, Kuitwaard et al.,
# its reference 12, not read), is a rise, not a level, and is recorded but
# not drawn.
#
# BODY SIZE: every parameter carries the source's own weight term, kept as
# fitted, evaluated at the pharmacokinetic weight with the switch on and at
# total weight with it off.
#
# PREGNANCY: no pharmacokinetic study of IVIG in pregnancy in the archive
# (inst/extdata/pregnancyRegistry.csv).
#
# References
# ----------
# Fokkink WJR et al., Clin Pharmacokinet 2022;61:1285-1296.
#   https://doi.org/10.1007/s40262-022-01136-z
# GAMMAGARD S/D prescribing information, BLA 103133, revised 2/2025.
# -----------------------------------------------------------------------------

IMMUNOGLOBULIN_CL   <- 0.28    # L/day at 70 kg
IMMUNOGLOBULIN_V1   <- 2.87    # L at 70 kg
IMMUNOGLOBULIN_Q    <- 0.25    # L/day at 70 kg
IMMUNOGLOBULIN_V2   <- 2.65    # L at 70 kg
# The source population's baseline: CBAS 15.9 g/L x 0.65 in GBS (Table 2)
IMMUNOGLOBULIN_BASELINE <- 15.9 * 0.65   # g/L

#' Intravenous immunoglobulin (Fokkink 2022)
#'
#' Two compartments, allometric in weight, plotted as total serum IgG, the
#' patient's own plus the infused, in g/L; see the header of
#' \code{R/drugs_immunoglobulin.R}.
#'
#' @inheritParams cefazolin
#' @param igGBaseline the patient's own serum IgG in g/L, or NULL for the
#'   source population's (Guillain-Barre syndrome, 10.3 g/L)
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
immunoglobulin <- function(weight, height, age, sex, adjustToFFM = TRUE,
                           igGBaseline = NULL)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)
  pkW  <- if (isTRUE(adjustToFFM)) size$pkWeight else weight
  w <- pkW / 70

  day <- 1440   # minutes
  default <- list(
    v1  = IMMUNOGLOBULIN_V1 * w, v2 = IMMUNOGLOBULIN_V2 * w, v3 = 1,
    cl1 = IMMUNOGLOBULIN_CL * w^0.75 / day,
    cl2 = IMMUNOGLOBULIN_Q * w^0.75 / day, cl3 = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  baseline <- if (is.null(igGBaseline)) IMMUNOGLOBULIN_BASELINE else igGBaseline

  reference <- paste0(
    "Fokkink 2022 (177 patients with Guillain-Barre syndrome, 5-89 years, Gammagard S/D): ",
    "two compartments, CL 0.28 L/day x (weight/70)^0.75, V1 2.87 L x weight/70, ",
    "Q 0.25 L/day x (weight/70)^0.75, V2 2.65 L x weight/70; the disability score and ",
    "methylprednisolone held at the reference (score 0-2, none). Plotted as total serum IgG: ",
    "baseline ", signif(baseline, 3), " g/L",
    if (is.null(igGBaseline)) " (blank: the GBS population's)" else "",
    " plus the infused. https://doi.org/10.1007/s40262-022-01136-z")

  list(
    PK = PK, tPeak = 0, MEAC = 0,
    typical = 0, upperTypical = 0, lowerTypical = 0,
    reference = reference,
    endogenousActivity = baseline
  )
}
