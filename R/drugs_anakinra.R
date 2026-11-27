# -----------------------------------------------------------------------------
# Anakinra (Kineret): subcutaneous, one compartment (Urien 2013); intravenous,
# two compartments (Ogungbenro 2016), as parallel systems
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
# second candidate for the selector.  No intravenous route is derived from
# these apparent SC parameters; the intravenous route is its own model, below.
#
# THE INTRAVENOUS ROUTE (Shafer, 2026-11-26, accepting the recommendation)
# ======================================================================
# Ogungbenro K, Hulme S, Rothwell N, Hopkins S, Tyrrell P, Galea J.  Study
# design and population pharmacokinetic analysis of a phase II dose-ranging
# study of interleukin-1 receptor antagonist.  J Pharmacokinet Pharmacodyn
# 2016;43:1-12.  Read on the PDF (card papers/digests/PMID_26476629.md).  31
# adults with subarachnoid haemorrhage: 6 given 100 mg IV then 2 mg/kg/h for
# 24 h (the data of Gueorguieva 2008, PMID 17875190, read on the PDF), and 25
# given one of five IV boluses (100-500 mg) with a 4-h infusion (1-10
# mg/kg/h).  Plasma and CSF fitted together (NONMEM VII); the plasma model is
# two compartments with first-order elimination, the CSF a compartment that
# takes 0.0071 L/h and returns none to plasma (not drawn).  Table 1, updated
# model (RSE %):
#
#     CL 7.86 L/h (5.1), V1 5.74 L (4.9), CLD 0.65 L/h (11.2), V2 3.56 L (10.0)
#
# No covariate was tested or retained.  The endogenous IL-1Ra, 0.57 ng/mL
# (mean) in plasma, was an additive baseline in the fit, and is not drawn:
# the curve is the drug given, above it.  Gueorguieva's earlier fit of the
# six patients (CL 6.96, V1 5.14, V2 4.96 L) is superseded.  The model runs
# as a parallel system that takes only intravenous doses (routes = IV), as
# meloxicam's ANJESO fit does; the SC system takes only SC doses.  The two
# populations differ (children with autoinflammatory disease, SC, apparent;
# adults after subarachnoid haemorrhage, IV, absolute): at 70 kg Urien's
# CL/F is 6.24 L/h against Ogungbenro's 7.86 L/h, so with the label's SC
# bioavailability of 95% the two clearances are within a quarter of each
# other.  The help page says the IV source is subarachnoid haemorrhage, not
# sepsis.
#
# BODY SIZE ANCHOR (intravenous): Ogungbenro prints no demographics; those
# of 25 of its 31 patients are in Galea 2011 (J Cereb Blood Flow Metab,
# PMID 20628399, not in the archive, papers/pending-work.csv), and
# Gueorguieva's other 6 were a mean 51 years and 70 kg (2 men, 6 women
# recruited, of whom 6 were fitted).  So the fixed values are anchored at
# the 70 kg reference man and scaled by pkSizeFactors() (volumes by $volume,
# clearances by $clearance); with the switch off they are the published
# values whatever the patient.
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
# CHECKS (intravenous): Yang 2003 (PMID 12844139, read on the PDF), 12
# healthy men (median 82 kg) given 1 mg/kg IV, was not fitted here: its Table
# III gives CL 137 mL/min (8.22 L/h), Vc 3.32 L and Vss 8.86 L; this model at
# 70 kg gives 7.86 L/h, 5.74 L and 9.30 L.  Clearance, the one real
# parameter, agrees to 4%; the central volume of a later, earlier-sampled
# study is smaller, as expected.  At 70 kg the half-lives are 0.46 and 4.2 h
# (Yang's terminal 2.64 h).  Gueorguieva 2008 prints no plasma concentrations in
# its text, only that the plasma steady state was reached quickly; the
# model's steady state for 100 mg then 2 mg/kg/h at 70 kg is 140 mg/h / 7.86
# L/h = 17.8 mcg/mL.  Renal function, again, is not a covariate: Gueorguieva's
# one patient with renal impairment cleared 1.41 L/h against the others'
# mean 6.96, and was fitted apart.
#
# References
# ----------
# Ogungbenro K et al., J Pharmacokinet Pharmacodyn 2016;43:1-12.
#   https://doi.org/10.1007/s10928-015-9450-0
# Gueorguieva I et al., Br J Clin Pharmacol 2008;65:317-325.
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
# Intravenous: Ogungbenro 2016, Table 1 (updated model), at the reference man
ANAKINRA_IV_CL  <- 7.86     # L/h
ANAKINRA_IV_V1  <- 5.74     # L
ANAKINRA_IV_CLD <- 0.65     # L/h
ANAKINRA_IV_V2  <- 3.56     # L

#' Anakinra pharmacokinetics (subcutaneous)
#'
#' Subcutaneous doses: one compartment with first-order absorption and weight
#' on the apparent clearance and volume (Urien 2013). Intravenous doses: two
#' compartments (Ogungbenro 2016), a parallel system scaled to the patient.
#' See the header of the file.
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

  # Intravenous doses: Ogungbenro 2016 at the reference man, scaled
  ivSize <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                          legacyVolume = 1, legacyClearance = 1)
  ivSet <- list(
    v1 = ANAKINRA_IV_V1 * ivSize$volume, v2 = ANAKINRA_IV_V2 * ivSize$volume, v3 = 1,
    cl1 = ANAKINRA_IV_CL / 60 * ivSize$clearance,
    cl2 = ANAKINRA_IV_CLD / 60 * ivSize$clearance, cl3 = 0
  )

  reference <- paste0(
    "Urien 2013 (children and adolescents with SJIA and autoinflammatory syndromes, ",
    "0.73-21 years): one compartment, apparent CL/F 0.847 x weight^0.47 L/h, ",
    "V/F 2.581 x weight^0.76 L, ka 0.38 /h; used at every age as the only ",
    "population model for subcutaneous doses; no renal term (normal renal function). ",
    "Intravenous doses: Ogungbenro 2016 (31 adults with subarachnoid haemorrhage), ",
    "two compartments, CL 7.86 L/h, V1 5.74 L, CLD 0.65 L/h, V2 3.56 L at the 70 kg ",
    "reference, scaled to the patient's fat-free mass. Concentration only. ",
    "https://doi.org/10.1186/2050-6511-14-40 https://doi.org/10.1007/s10928-015-9450-0")

  list(
    PK = PK,
    routes = ROUTE_SC,
    parallelSystems = list(
      list(name = "Intravenous (Ogungbenro 2016)", routes = ROUTE_IV,
           PK = list(default = ivSet))
    ),
    tPeak = 0, MEAC = 0,
    typical = 0, upperTypical = 0, lowerTypical = 0,
    reference = reference
  )
}
