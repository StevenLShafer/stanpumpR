# -----------------------------------------------------------------------------
# Landiolol: what the three published models share
# -----------------------------------------------------------------------------
# Three population models of landiolol hydrochloride have been published, all
# Japanese, all two compartments per kilogram with a lag time on the infusion,
# and all three are in the library as candidates of the group `landiolol`
# (R/modelSelection.R), which chooses one by the patient's age and weight:
#
#   landiololHonda          Honda 2008, 47 healthy men, 20-33 y, 50.7-77.2 kg
#   landiololKunisawa2014   Kunisawa 2015 (J Anesth; received 2014), 9 women
#                           having gynaecologic surgery, 37-71 y, 47.1-73.0 kg
#   landiololKunisawa2015   Kunisawa 2015 (Ther Clin Risk Manag), 8 patients
#                           with peripheral arterial disease, 64-84 y,
#                           40.4-71.8 kg
#
# Steven L. Shafer, 2026-11-25: "Hold on Landiolol for Honda 2008".  Landiolol
# had merged that morning on Kunisawa 2015 alone; Honda 2008 and Kunisawa 2014
# reached the archive the same afternoon and were read on the PDF.
#
# Each file's header gives its source; this one gives what they share.
#
# THE LAG.  Each source fitted a lag on an intravenous infusion (0.63-0.82
# min).  Honda attributes it to transit from the forearm infusion vein to the
# contralateral forearm vein sampled, through the capillary bed of that arm;
# Kunisawa's lines were connected at the nearest port to minimise dead space.
# No intravenous engine of the library represents it.  It is left out, as the
# authors left it out of the STANPUMP target-controlled infusions they gave;
# every curve is drawn 38-49 s early, and the concentrations reached are
# unchanged.
#
# DOSE BASIS.  Every source gives landiolol hydrochloride and reports the
# concentration of "landiolol hydrochloride" (Honda) or "landiolol"
# (Kunisawa).  Per-kilogram volumes and clearances do not depend on the basis
# when dose and concentration share it, so the parameters are read as they
# are printed and a dose is entered as the US product (Rapiblyk) is labelled,
# landiolol base, with the concentration plotted on the same basis (AGENTS.md,
# Dose basis).
#
# BODY SIZE (docs/weight-adjustment.md).  Every parameter is per kilogram,
# normalised by its source, so it is evaluated at the pharmacokinetic weight
# with the fat-free-mass switch on (size$pkWeight) and at total body weight
# with it off, as esmolol's.  A model published per kilogram is not anchored
# at its cohort.
#
# EFFECT SITE: tPeak 2 MINUTES, FOR HEART RATE (Krumpl 2017); NO BAND.  None
# of the three fitted a concentration-effect relation.  Krumpl G, Ulc I,
# Trebs M, Kadlecova P, Hodisch J, Eur J Clin Pharmacol 2017;73:417-428 (PMID
# 28091703, read on the PDF), gave 12 healthy volunteers landiolol 0.1, 0.2
# and 0.3 mg/kg at 0.1 mg/kg per 15 s: after 0.2 and 0.3 mg/kg the lowest
# heart rate was the 2-minute value (Table 4: 52.2 and 50.5 bpm).  The same
# tPeak is used with every candidate.  Landiolol has no therapeutic
# concentration range; it is titrated to heart rate.
#
# THE US LABEL.  Rapiblyk (NDA 217202, revised 2/2026, section 12.3) gives a
# total body clearance of 57 mL/kg/min after a 20-hour infusion in awake
# European volunteers (Krumpl 2018, whole blood).  The three Japanese
# clearances are 30.7-36.6; the difference is recorded, not reconciled.
#
# PREGNANCY.  No pharmacokinetic study in pregnancy (PubMed, 2026-11-25);
# recorded unknown for the group and each candidate.
# -----------------------------------------------------------------------------

#' Time to peak heart-rate effect of landiolol, minutes (Krumpl 2017, Table 4)
#' @keywords internal
LANDIOLOL_TPEAK <- 2

#' Landiolol's three published parameter sets, per kilogram
#'
#' CL and Q in L/min/kg, V1 and V2 in L/kg, the fitted lag (not represented)
#' in minutes; one row per source, read on each PDF.
#' @keywords internal
LANDIOLOL_SOURCES <- list(
  # Honda 2008, Table 3 (final model, 47 healthy men)
  honda = c(cl = 0.0366, v1 = 0.101, q = 0.0161, v2 = 0.0556, lag = 0.820),
  # Kunisawa 2015 (J Anesth 29:198-205), Table 2 (9 gynaecologic patients)
  kunisawa2014 = c(cl = 0.0340, v1 = 0.0749, q = 0.0709, v2 = 0.0389, lag = 0.634),
  # Kunisawa 2015 (Ther Clin Risk Manag 11:107-114), Table 2 (8 with PAD)
  kunisawa2015 = c(cl = 0.0307, v1 = 0.0650, q = 0.0483, v2 = 0.0544, lag = 0.633)
)

#' Build a landiolol PK set from a per-kilogram parameter set
#' @keywords internal
landiololPerKg <- function(p, weight, height, age, sex, adjustToFFM, reference)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)
  pkW  <- if (isTRUE(adjustToFFM)) size$pkWeight else weight

  default <- list(
    v1  = p[["v1"]] * pkW,
    v2  = p[["v2"]] * pkW,
    v3  = 1,                          # placeholder: a two-compartment model
    cl1 = p[["cl"]] * pkW,
    cl2 = p[["q"]]  * pkW,
    cl3 = 0
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  list(
    PK = PK,
    tPeak = LANDIOLOL_TPEAK,
    MEAC = 0,
    typical = 0,
    upperTypical = 0,
    lowerTypical = 0,
    reference = paste0(
      reference,
      " The fitted lag is not drawn.  Doses as landiolol base, as labelled.",
      " Time to peak heart-rate effect 2 min from Krumpl G et al., Eur J",
      " Clin Pharmacol 2017;73:417-428.")
  )
}
