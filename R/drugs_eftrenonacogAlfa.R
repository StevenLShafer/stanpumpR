# -----------------------------------------------------------------------------
# Eftrenonacog alfa (ALPROLIX, recombinant factor IX Fc fusion protein): two
# compartments, intravenous
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min, amounts in
# IU, activity in IU/dL (= % of normal).  Plotted as the patient's own factor
# IX activity plus the infused activity (R/factorBaselines.R; Steven L.
# Shafer, 2026-11-25).  Drafted by Claude Code, 2026-11-25, at the request of
# Steven L. Shafer (docs/library-expansion/plan.csv, Biologics (surgery),
# factor IX); digest card papers/digests/PMID_37567779.md.
#
# An extended-half-life product of its own, so a drug of its own: a standard
# factor IX (BeneFIX, nonacog alfa) has a terminal half-life of about a day,
# this one of about four, and a clinician knows which is given.
#
# SOURCE
# ======
# Koopman SF, Goedhart TMHJ, Bukkems LH, et al.  A new population
# pharmacokinetic model for recombinant factor IX-Fc fusion concentrate
# including young children with haemophilia B.  Br J Clin Pharmacol
# 2024;90:220-231.  Read on the PDF.  37 patients with haemophilia B (35
# severe, FIX < 1 IU/dL), from the Dutch OPTI-CLOT TARGET study and the UK-EHL
# Outcomes Registry: age 15 (2-71) years, 14 under 12; weight 65.4 (12-103)
# kg; height 169 (85-192) cm (Table 1).  Dose 36 (10-132) IU/kg; 5 (3-7)
# samples per profile, one-stage assay, corrected for the endogenous baseline
# and the residual of the previous dose (Methods 2.2).  NONMEM.
#
# Table 2 (new model) and the equations under it, weight (kg) about 73:
#
#     CL = 1.41 dL/h x (BW/73)^0.75 x (1 - 0.0047 x (AGE - 15.8))
#     V1 = 73.1 dL x (BW/73)
#     Q  = 2.77 dL/h x (BW/73)^0.75
#     V2 = 80.1 dL x (BW/73)
#
# The weight exponents are fixed allometric ones.  The text says (Results
# 3.3.2) that a 73 kg patient's clearance falls from 1.89 dL/h at 20 years to
# 1.36 at 70; the equation gives 1.38 and 1.05, the same fall of about a
# quarter from a lower start.  The table, the equation and the text's own
# 1.41 dL/h for a 16-year-old agree, so they are used; the sentence is
# recorded on the card.
#
# CHECKS at 73 kg and 15.8 years: half-lives 8.3 h and 87.1 h, CL 1.93
# mL/h/kg, Vss 210 mL/kg, incremental recovery at the end of a bolus 1.00
# IU/dL per IU/kg.  ALPROLIX's label (12.3, Table 4, adults, 50 and 100
# IU/kg): terminal half-life 86 and 97 h, CL 3.3 and 2.6 mL/h/kg, Vss 327 and
# 236 mL/kg, recovery 1.02 and 1.12; children 2-4 years (Table 5): recovery
# 0.60, CL 4.4 mL/h/kg, half-life 68 h; this model at 4 years and 16 kg gives
# CL 3.0 mL/h/kg.  The half-life and recovery agree; the clearance is about
# a third lower than the label's, which the authors found too (their model's
# 1.41 dL/h against the earlier Diao 2014 model's 2.39): real-world sampling
# out to a week, against the trials' intensive profiles.  Not corrected.
#
# BASELINE: the Patient Profile's Factor IX baseline (IU/dL), added to the
# infused activity.  Blank, the source population's: severe haemophilia B,
# below 1 IU/dL, entered as 0.5 IU/dL (as for factor VIII; a convention).
#
# BAND: ALPROLIX's perioperative target for major surgery, an initial level of
# 60 to 100 IU/dL, the typical 80 (label, Table 2); minor surgery 50 to 80.
#
# AGE: linear in age, as fitted, about 15.8 years: 1.065 at 2 years and 0.65
# at 90, finite and positive for every legal age (it reaches zero at 228
# years), so it is not held.
#
# BODY SIZE: every parameter carries the source's own weight term, kept as
# fitted, evaluated at the pharmacokinetic weight with the switch on and at
# total weight with it off.
#
# RENAL FUNCTION: none in the model; an Fc fusion protein is not filtered.
#
# PREGNANCY: no pharmacokinetic study in pregnancy (ALPROLIX label 8.1;
# inst/extdata/pregnancyRegistry.csv).
#
# References
# ----------
# Koopman SF et al., Br J Clin Pharmacol 2024;90:220-231.
#   https://doi.org/10.1111/bcp.15881
# ALPROLIX prescribing information, BLA 125444, revised 5/2023.
# -----------------------------------------------------------------------------

EFTRENONACOG_CL        <- 1.41    # dL/h at 73 kg and 15.8 years
EFTRENONACOG_CL_AGE    <- 0.0047  # per year, linear about 15.8
EFTRENONACOG_REF_AGE   <- 15.8    # years
EFTRENONACOG_V1        <- 73.1    # dL at 73 kg
EFTRENONACOG_Q         <- 2.77    # dL/h at 73 kg
EFTRENONACOG_V2        <- 80.1    # dL at 73 kg
EFTRENONACOG_REF_WEIGHT <- 73     # kg
# The source population's baseline, severe haemophilia B (below 1 IU/dL)
EFTRENONACOG_BASELINE  <- 0.5     # IU/dL

#' Eftrenonacog alfa, ALPROLIX (Koopman 2024)
#'
#' Two compartments, recombinant factor IX Fc fusion protein, plotted as the
#' patient's own factor IX activity plus the infused activity, in IU/dL; see
#' the header of \code{R/drugs_eftrenonacogAlfa.R}.
#'
#' @inheritParams cefazolin
#' @param factorIXBaseline the patient's own factor IX activity in IU/dL, or
#'   NULL for 0.5 IU/dL, a chosen default within the source population's
#'   severe haemophilia B (below 1 IU/dL)
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
eftrenonacogAlfa <- function(weight, height, age, sex, adjustToFFM = TRUE,
                             factorIXBaseline = NULL)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)
  pkW  <- if (isTRUE(adjustToFFM)) size$pkWeight else weight
  w <- pkW / EFTRENONACOG_REF_WEIGHT

  cl <- EFTRENONACOG_CL * w^0.75 * (1 - EFTRENONACOG_CL_AGE * (age - EFTRENONACOG_REF_AGE))
  v1 <- EFTRENONACOG_V1 * w
  q  <- EFTRENONACOG_Q  * w^0.75
  v2 <- EFTRENONACOG_V2 * w

  # dL to L, and per hour to per minute
  default <- list(
    v1  = v1 / 10, v2 = v2 / 10, v3 = 1,
    cl1 = cl / 10 / 60, cl2 = q / 10 / 60, cl3 = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  baseline <- if (is.null(factorIXBaseline)) EFTRENONACOG_BASELINE else factorIXBaseline

  reference <- paste0(
    "Koopman 2024 (37 patients with haemophilia B, 2-71 years, ALPROLIX): ",
    "two compartments, CL 1.41 dL/h x (weight/73)^0.75 x (1 - 0.0047 (age - 15.8)), ",
    "V1 73.1 dL x weight/73, Q 2.77 dL/h x (weight/73)^0.75, V2 80.1 dL x weight/73. ",
    "Plotted as the patient's own factor IX activity plus the infused: baseline ",
    signif(baseline, 3), " IU/dL",
    if (is.null(factorIXBaseline)) " (blank: severe haemophilia B, the source population)" else "",
    ". Band: the label's perioperative target for major surgery, 60-100 IU/dL. ",
    "https://doi.org/10.1111/bcp.15881")

  list(
    PK = PK, tPeak = 0, MEAC = 0,
    typical = 80, upperTypical = 100, lowerTypical = 60,
    reference = reference,
    endogenousActivity = baseline
  )
}
