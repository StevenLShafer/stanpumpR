# -----------------------------------------------------------------------------
# Verapamil, intravenous (Kates 1981): plasma verapamil in chronic atrial
# fibrillation
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min.
#
# Drafted by Claude Code, 2026-10-19, at the request of Steven L. Shafer, as
# one of the vasoactive and resuscitation drugs used in intensive care
# (inst/help/models/vasoactive.md, inst/extdata/vasoactiveRegistry.csv).  Every
# number below was read from the published article, which he supplied:
# Tables I-III.  The check is a second article he supplied.
#
# THE SOURCE
# ==========
# Kates RE, Keefe DL, Schwartz J, Harapat S, Kirsten EB, Harrison DC.
# Verapamil disposition kinetics in chronic atrial fibrillation.  Clin
# Pharmacol Ther 1981;30:44-51 (doi 10.1038/clpt.1981.125).  Twelve patients
# (8 men; 52-69 years, mean 59.8; 58-115 kg, mean 81) with chronic atrial
# fibrillation, all on digoxin, given 15 mg intravenously at 1 mg/min; plasma
# by HPLC for 24 hours; each patient's curve fitted to a three-compartment
# model with the 15-minute infusion as its input.  Table II prints each
# patient's k10, k12, k21, k13, k31 and central volume Vc, with Vc k10 and Vdss
# as derived columns.
#
# A MISPRINT, FOUND AND CORRECTED
# ===============================
# Patient 2's Vc is printed as 92.72 L.  That patient's own derived columns
# require 9.272 L: Vc k10 is printed as 0.63 L/min (92.72 x 0.06796 = 6.30,
# 9.272 x 0.06796 = 0.630), and Vdss as 4.36 L/kg, which 9.272 L gives and
# 92.72 L would make 43.5.  The table's mean Vc, 28.6 L, was averaged with
# the misprint; with 9.272 it is 21.6 L.  The model uses 9.272.
#
# THE TYPICAL PATIENT
# ===================
# For each patient the six parameters V1, V2 = Vc k12/k21, V3 = Vc k13/k31,
# CL = Vc k10, Q2 = Vc k12 and Q3 = Vc k13 are put per kilogram of that
# patient's weight (Table I), and the model is their arithmetic mean.  That
# choice is the one that reproduces the paper's own summary: "The model
# calculated average values for clearance and steady-state volume of
# distribution are 12.5 +/- 5.4 ml/min/kg and 3.50 +/- 1.3 l/kg" -- the means
# here are 12.52 and 3.49.  (Geometric means give 11.7 and 3.02.)  Every
# patient's own printed Vc k10 and Vdss are also reproduced from the rate
# constants, which the test checks.  The typical model's half-lives are 2.6
# and 37 minutes and 4.8 hours, against the model-independent terminal
# half-life of 6.3 +/- 4 h (Table III); the paper itself notes that its
# model-independent volumes ran larger than the model's, being sensitive to
# the terminal slope.
#
# AN INDEPENDENT CHECK
# ====================
# Eichelbaum M, Mikus G, Vogelgesang B.  Pharmacokinetics of (+)-, (-)- and
# (+/-)-verapamil after intravenous administration.  Br J Clin Pharmacol
# 1984;17:453-458.  Five young healthy men; racemic verapamil 10 mg in three
# of them: clearance 14.27 mL/min/kg and Vss 3.40 L/kg (Table 2).  Both are
# within 15% of this model's, the clearance higher, as expected in young
# healthy men without heart failure.  That paper's Table 1 coefficients and
# exponents give volumes and clearances 8% above its own Table 2 in every
# subject, which is the ratio of verapamil hydrochloride to base (491.07 /
# 454.60 = 1.080): its doses were the hydrochloride and its Table 2 the base.
#
# DOSE BASIS
# ==========
# Kates does not state it.  Intravenous verapamil is labelled, and was then
# supplied, as the hydrochloride, so a dose is entered as labelled; read as
# the base, every concentration would be 8% high.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# Each parameter is per kilogram, the model's own size term, linear in weight,
# evaluated at the pharmacokinetic weight with the fat-free-mass switch on
# (size$pkWeight) and at total body weight with it off.
#
# WHAT THIS MODEL IS NOT
# ======================
# Racemic verapamil; the (-)-isomer, about ten times as potent on AV
# conduction, is cleared faster and distributes more widely than the (+)
# (Eichelbaum 1984), so the racemate's concentration is not the active
# concentration.  Norverapamil was not detected after the intravenous dose
# and is not plotted.  No effect site and no band.
# -----------------------------------------------------------------------------

# Kates 1981, Table I (weight, kg) and Table II (rate constants /min, Vc L),
# patients 1-12, patient 2's Vc corrected from 92.72 to 9.272 (see header)
VERAPAMIL_KATES <- data.frame(
  weight = c(110, 58, 77, 75, 115, 72, 60, 91, 80, 60, 72, 102),
  k10 = c(0.03671, 0.06796, 0.05676, 0.1670, 0.1089, 0.04901, 0.05011, 0.05706,
          0.02735, 0.03523, 0.02113, 0.03262),
  k12 = c(0.06895, 0.3114, 0.1800, 0.1345, 0.2295, 0.1293, 0.2247, 0.1226,
          0.13267, 0.11286, 0.12693, 0.08347),
  k21 = c(0.03041, 0.07226, 0.05760, 0.0605, 0.06053, 0.02603, 0.04923, 0.0803,
          0.04114, 0.03317, 0.04941, 0.04210),
  k13 = c(0.02749, 0.1512, 0.05988, 0.1418, 0.1021, 0.02177, 0.09526, 0.1520,
          0.03075, 0.05946, 0.01616, 0.02474),
  k31 = c(0.005459, 0.006908, 0.010827, 0.00678, 0.007882, 0.00746, 0.005761,
          0.007834, 0.003170, 0.003639, 0.008965, 0.004300),
  Vc  = c(27.31, 9.272, 17.78, 11.55, 9.96, 28.44, 10.88, 11.84, 27.48, 17.14,
          55.0, 32.8)
)

#' The typical per-kilogram verapamil model from Kates 1981
#'
#' The arithmetic mean, over the twelve patients of Table II, of each
#' patient's V1, V2, V3, CL, Q2 and Q3 per kilogram.  See the header of
#' \code{R/drugs_verapamil.R}.
#'
#' @returns a named numeric vector, L/kg and L/min/kg
#' @keywords internal
verapamilTypicalPerKg <- function(k = VERAPAMIL_KATES)
{
  colMeans(cbind(
    v1  = k$Vc / k$weight,
    v2  = k$Vc * k$k12 / k$k21 / k$weight,
    v3  = k$Vc * k$k13 / k$k31 / k$weight,
    cl1 = k$Vc * k$k10 / k$weight,
    cl2 = k$Vc * k$k12 / k$weight,
    cl3 = k$Vc * k$k13 / k$weight))
}

#' Verapamil, intravenous (Kates 1981)
#'
#' Three compartments, the typical patient with chronic atrial fibrillation;
#' see the header of \code{R/drugs_verapamil.R}.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
verapamil <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)
  pkW  <- if (isTRUE(adjustToFFM)) size$pkWeight else weight
  p <- verapamilTypicalPerKg()

  default <- as.list(p * pkW)

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Kates RE, Keefe DL, Schwartz J, et al., Clin Pharmacol Ther 1981;30:44-51. ",
    "Three compartments, the mean of 12 patients with chronic atrial ",
    "fibrillation given 15 mg IV (patient 2's misprinted Vc corrected): CL ",
    "12.5 mL/min/kg, Vss 3.49 L/kg, as the paper reports; checked against ",
    "Eichelbaum 1984. Racemic verapamil. https://doi.org/10.1038/clpt.1981.125"
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
