# -----------------------------------------------------------------------------
# Propranolol, intravenous (Fagan 1982): plasma propranolol in healthy men
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min.
#
# Drafted by Claude Code, 2026-10-19, at the request of Steven L. Shafer, as
# one of the vasoactive and resuscitation drugs used in intensive care
# (inst/help/models/vasoactive.md, inst/extdata/vasoactiveRegistry.csv).  Every
# number below was read from the published letter, which he supplied (Table
# 1 and Figures 1-2), and the check from a second paper he supplied.
#
# THE SOURCE
# ==========
# Fagan TC, Walle T, Walle UK, Conradi EC, Harmon G, Gaffney TE.  Early
# kinetics of intravenous propranolol.  Br J Clin Pharmacol 1982;13:571-574
# (doi 10.1111/j.1365-2125.1982.tb01424.x).  Six healthy men, 20-31 years,
# 60-90 kg, given 0.05 mg/kg (Inderal) at 1 mg/min, sampled from the other
# arm from 1 minute to 8 hours, GC-MS.  A three-compartment model fitted
# best; the infusion intercepts were converted to bolus equivalents (Loo and
# Riegelman), and Table 1 gives each subject's triexponential and the means:
#
#     dose 3.8 mg;  P 317, A 17.2, B 9.3 ng/mL;
#     pi 0.856 /min, alpha 0.0548 /min, beta 0.203 /h;
#     AUC 57.6 ng h/mL;  total clearance Q_B 1.19 L/min
#
# THE DERIVATION
# ==============
# A triexponential and its dose fix a mammillary three-compartment model
# uniquely (threeCompartmentFromTriexponential(), R/vasoactive.R).  From the
# mean triexponential per 3.8 mg:
#
#     V1 11.06 L, V2 42.54 L, V3 210.3 L, CL 1.107 L/min, Q2 4.531, Q3 3.133
#
# (Vss 264 L), every rate positive.  Two checks inside the paper: the area
# under the mean curve is 57.2 ng h/mL against the 57.6 printed, and the
# clearance it implies, 1.11 L/min, is 7% below the mean of the individual
# clearances, 1.19, as a clearance from mean curves should be.  Each
# subject's own triexponential is also admissible (subject 5's was
# biexponential), with steady-state volumes from 253 to 417 L: the mean
# curve is the typical subject, not any one of them.
#
# AN INDEPENDENT CHECK
# ====================
# Cheymol G, Poirier JM, Barre J, Pradalier A, Dry J.  Comparative
# pharmacokinetics of intravenous propranolol in obese and normal
# volunteers.  J Clin Pharmacol 1987;27:874-879.  A different laboratory,
# assay (HPLC) and regimen (8 mg of the hydrochloride over 10 minutes):
# in the twelve normal-weight controls, clearance 75.9 L/h (1.27 L/min),
# Vss 318.6 L and terminal half-life 3.1 h.  This model gives 1.11 L/min,
# 264 L and 3.4 h, all within 17%.  The obese subjects of the same paper had
# LOWER clearance and volume (57.5 L/h, 208.9 L), which is what scaling to
# fat-free mass predicts and scaling to total weight does not.
#
# DOSE BASIS
# ==========
# Propranolol hydrochloride: Inderal injection is the hydrochloride, and
# Cheymol states it.  A dose is entered as labelled.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# The mean curve is for the study's mean man (3.8 mg at 0.05 mg/kg, so
# 76 kg) and is written as fixed values, scaled to fat-free mass with the
# switch on; with it off they are the published values for everyone
# (legacyVolume = 1).
#
# WHAT THIS MODEL IS NOT
# ======================
# Six young healthy men; clearance depends on hepatic blood flow and so on
# cardiac output, and is lower in heart failure and shock.  The racemate is
# measured; only l-propranolol is a beta blocker.  No heart rate is
# predicted.
# -----------------------------------------------------------------------------

# Fagan 1982, Table 1 means
PROPRANOLOL_DOSE  <- 3.8                        # mg
PROPRANOLOL_COEF  <- c(317, 17.2, 9.3)          # ng/mL, bolus-equivalent
PROPRANOLOL_EXP   <- c(0.856, 0.0548, 0.203 / 60)   # /min

#' Propranolol, intravenous (Fagan 1982)
#'
#' Three compartments from the mean triexponential of six healthy men; see
#' the header of \code{R/drugs_propranolol.R}.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
propranolol <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1)

  # ng/mL is mcg/L, so ng/mL per mg is 1/1000 per litre
  p <- threeCompartmentFromTriexponential(PROPRANOLOL_COEF / 1000 / PROPRANOLOL_DOSE,
                                          PROPRANOLOL_EXP)
  default <- list(
    v1  = p$v1  * size$volume,
    v2  = p$v2  * size$volume,
    v3  = p$v3  * size$volume,
    cl1 = p$cl1 * size$clearance,
    cl2 = p$cl2 * size$clearance,
    cl3 = p$cl3 * size$clearance
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Fagan TC, Walle T, Walle UK, et al., Br J Clin Pharmacol 1982;13:571-574. ",
    "Three compartments from the mean triexponential of 6 healthy men given ",
    "0.05 mg/kg: V1 11.1 L, CL 1.11 L/min, Vss 264 L; checked against ",
    "Cheymol 1987's controls (CL 1.27 L/min, Vss 319 L). ",
    "https://doi.org/10.1111/j.1365-2125.1982.tb01424.x"
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
