# -----------------------------------------------------------------------------
# Anakinra: one compartment, subcutaneous (Kineret)
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min, amounts in mg,
# concentrations in ng/mL.  Concentration only: no C-reactive protein, IL-1 or
# clinical endpoint is plotted.  Drafted by Claude Code, 2026-11-25, at the
# request of Steven L. Shafer (docs/library-expansion/plan.csv, Biologics
# (sepsis)); digest card papers/digests/PMID_23915458.md.
#
# Recombinant human interleukin-1 receptor antagonist, 17.3 kDa.
#
# SOURCE
# ======
# Urien S, Bardin C, Bader-Meunier B, Mouy R, Compeyrot-Lacassagne S, Foissac
# F, Florkin B, Wouters C, Neven B, Treluyer J-M, Quartier P.  Anakinra
# pharmacokinetics in children and adolescents with systemic-onset juvenile
# idiopathic arthritis and autoinflammatory syndromes.  BMC Pharmacol Toxicol
# 2013;14:40.  87 patients, 0.73-21 years and 4.3-83 kg (median 21 kg), given
# 2-10 mg/kg (maximum 100 mg) SC once daily; 148 plasma concentrations by
# ELISA (LOQ 0.04 mg/L), mostly 0-26 h after a dose.  Read on the PDF.
#
# Results (PDF p. 3) and Table 1, BW in kg:
#
#     CL/F = 0.847 x BW^0.47 L/h        (6.24 L/h at 70 kg)
#     V/F  = 2.581 x BW^0.76 L          (65.2 L at 70 kg)
#     Ka   = 0.38 /h
#
# One compartment with first-order absorption; two compartments, nonlinear
# elimination and zero-order or transit absorption were tested and not kept.
# Age was significant only before weight; sex and co-medication were not.
# The parameters are apparent (F unknown in the source).  The label gives an
# absolute bioavailability of 95% (70 mg SC in 11 healthy subjects, 12.3); it
# is not used, because the model is apparent and is offered only by the route
# it was fitted for.
#
# POPULATION: children and adolescents, to 21 years and 83 kg.  It is the only
# population model of anakinra in the archive, so the program uses it at every
# age, by the nearest-population rule, and the help page says so (decided by
# Steven L. Shafer, 2026-11-25: a source reaching 21 years and 83 kg should
# extrapolate well to adults through its allometric weight terms evaluated at
# the fat-free-mass-based pharmacokinetic weight; the adult checks below).
#
# BODY SIZE: the source's own weight terms, kept as fitted, evaluated at the
# pharmacokinetic weight with the switch on and at total weight with it off.
#
# RENAL FUNCTION: not in the model.  Anakinra is cleared mainly by the kidney:
# Yang 2003 (Clin Pharmacol Ther 74:85-94, PMID 12844139, abstract read; the
# paper is not in the archive, papers/pending-work.csv) found its apparent
# clearance 16%, 50%, 70% and 75% lower in mild, moderate and severe renal
# impairment and end-stage renal disease, which the label repeats (12.3).
# Urien's patients had normal renal function, so the curve is that of a patient
# with normal renal function.  No renal term is grafted onto this paediatric
# fit (Shafer, 2026-11-25, accepting the card's recommendation): Yang 2003 is
# to be read first, and an adult model carrying its renal relation would be a
# second candidate for the selector.  Likewise no intravenous route is derived
# from these apparent SC parameters: an IV route waits for a good
# intravenous model (Shafer, 2026-11-25), Yang 2003's IV arm the candidate.
#
# PREGNANCY: no pharmacokinetic study (inst/extdata/pregnancyRegistry.csv).
# Clearance is largely renal, but the model has no renal term to carry the rise
# in filtration of pregnancy, so nothing is applied.
#
# CHECKS
# ======
# At 70 kg: CL/F 6.24 L/h (104 mL/min), V/F 65.2 L, elimination half-life
# 7.2 h, time to peak 4.8 h after a single dose.  The label (12.3, adults with RA, 1-2 mg/kg SC):
# peak 3-7 h after the dose, terminal half-life 4-6 h.  The peak time agrees;
# the half-life is longer.  Yang 2003's healthy adults: CL/F 170 +/- 37 mL/min
# after 100 mg SC, 1.6 times the model's at 70 kg (only clearance is real:
# that is the difference to note).  The label's NOMID patients (12.3), median
# 3 mg/kg SC daily at steady state: Cmax 3628 (655-8511) ng/mL and C24h 203
# (53-1979) ng/mL; the model at 20-30 kg gives Cmax 1420-1660 and C24h
# 142-217 ng/mL: the trough agrees, the peak is about half the median and
# inside its range (sparse sampling in the source, mostly at the trough).
# Neither is corrected.
#
# References
# ----------
# Urien S et al., BMC Pharmacol Toxicol 2013;14:40.
#   https://doi.org/10.1186/2050-6511-14-40
# KINERET (anakinra) prescribing information, BLA 103950, revised 10/2025.
# Yang BB, Baughman S, Sullivan JT.  Clin Pharmacol Ther 2003;74:85-94.
# -----------------------------------------------------------------------------

ANAKINRA_CL_COEF <- 0.847   # L/h per kg^0.47
ANAKINRA_CL_EXP  <- 0.47
ANAKINRA_V_COEF  <- 2.581   # L per kg^0.76
ANAKINRA_V_EXP   <- 0.76
ANAKINRA_KA      <- 0.38    # /h

#' Anakinra pharmacokinetics (subcutaneous)
#'
#' One compartment with first-order subcutaneous absorption and weight on the
#' apparent clearance and volume (Urien 2013); see the header of the file.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
anakinra <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)
  pkW  <- if (isTRUE(adjustToFFM)) size$pkWeight else weight

  cl <- ANAKINRA_CL_COEF * pkW^ANAKINRA_CL_EXP   # L/h
  v  <- ANAKINRA_V_COEF  * pkW^ANAKINRA_V_EXP    # L

  default <- list(
    v1  = v, v2 = 1, v3 = 1,
    cl1 = cl / 60, cl2 = 0, cl3 = 0,
    ka_SC = ANAKINRA_KA / 60, bioavailability_SC = 1, tlag_SC = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Urien 2013 (children and adolescents with SJIA and autoinflammatory syndromes, ",
    "0.73-21 years): one compartment, apparent CL/F 0.847 x weight^0.47 L/h, ",
    "V/F 2.581 x weight^0.76 L, ka 0.38 /h; used at every age as the only ",
    "population model; no renal term (normal renal function). Concentration only. ",
    "https://doi.org/10.1186/2050-6511-14-40")

  list(
    PK = PK, tPeak = 0, MEAC = 0,
    typical = 0, upperTypical = 0, lowerTypical = 0,
    reference = reference
  )
}
