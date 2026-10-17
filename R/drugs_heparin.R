# -----------------------------------------------------------------------------
# Unfractionated heparin: two-compartment intravenous model of anti-IIa
# activity during and after cardiopulmonary bypass (Jia 2015)
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min, doses in
# international units (IU), and the plotted curve is plasma anti-factor IIa
# activity in IU/mL.  Concentration.Units "IU" (simCpCe()).
#
# SOURCE
# ======
# Jia Z, Tian G, Ren Y, Sun Z, Lu W, Hou X.  Pharmacokinetic model of
# unfractionated heparin during and after cardiopulmonary bypass in cardiac
# surgery.  J Transl Med 2015;13:45.  41 Chinese adults enrolled (32
# completed) at Beijing Anzhen Hospital, with kidney, liver and coagulation
# dysfunction excluded.  375 IU/kg before bypass, 125 IU/kg in the prime, and
# further doses by the coagulation tests; about 33,000 IU an operation.
# Anti-IIa activity (Heparin Chromogenic Activity Kit 820, ACL-TOP) every 30
# min until protamine, then at 2, 4, 8, 12 and 24 h.  NONMEM, two
# compartments, first-order elimination; age, weight and sex were tested and
# none was retained, so the parameters are absolute:
#
#     CL = 1.18 L/h     V1 = 3.04 L     Q = 0.171 L/h     V2 = 8.01 L
#
# PROVENANCE: THE TABLE WAS NOT SEEN
# ==================================
# These four values are the paper's Table 2 as transcribed in a specification
# Steven L. Shafer supplied (16 October 2026).  The table itself could not be
# read here (2026-10-17); the text could.  They reproduce three results the
# text states: an initial half-life of about 90 min (these give 93 min); a
# rebound after protamine, which the authors modelled by emptying the central
# compartment, that peaks at 0.04 IU/mL about 8 h after bypass and stays
# above 0.02 IU/mL at 24 h (these give 0.039 IU/mL at 8 h and 0.030 at 24 h);
# and a median of 4.8 IU/mL at the end of bypass (about 4 IU/mL for the
# study's doses in a 70 kg patient).  The rebound depends on all four
# parameters together, which is the strongest of the three checks.  They
# await a check against Table 2.
#
# WHAT THIS MODEL IS
# ==================
# Linear kinetics fitted in the setting of bypass: hemodilution, hypothermia
# to 32 C, high doses.  Heparin's clearance is saturable, and its half-life
# outside bypass rises with the dose (the label gives 0.5 to 2 h); none of
# that is in the model, which should not be read as heparin's kinetics on
# the ward or in a low-dose infusion.  Anti-IIa activity is not anti-Xa
# activity and not the ACT: no conversion between them is applied.  The
# initial half-life is 93 min and the terminal one 37 h, the slow return of
# heparin from the peripheral compartment that the authors used to explain
# rebound.  Protamine, which they represented by setting the central amount
# to zero, cannot be given in the library: the closed-form engines carry no
# such event.
#
# THRESHOLDS
# ==========
# tPeak = 0, so the curve is plasma activity.  No band, no MEAC and no time
# until threshold: anticoagulation on bypass is managed with the ACT, which
# this model does not predict.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# The parameters are fixed values with no size covariate (none was
# retained).  The demographics are in Table 1, which was not seen either, so
# the values are written as the library's 70 kg reference adult and scaled
# to fat-free mass with the switch on; with it off they are as published
# (legacyVolume = 1).  Heparin is dosed per kilogram (IU/kg), and that
# scaling of the dose is independent of the model's.
#
# (Claude Code, 2026-10-17, at the request of Steven L. Shafer.)
# -----------------------------------------------------------------------------

#' Unfractionated heparin pharmacokinetics (anti-IIa activity, bypass)
#'
#' Jia 2015; see the header of \code{R/drugs_heparin.R}.
#'
#' @inheritParams alfentanil
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
heparin <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1)

  default <- list(
    v1  = 3.04 * size$volume,
    v2  = 8.01 * size$volume,
    v3  = 1,                                   # two compartments
    cl1 = 1.18  / 60 * size$clearance,
    cl2 = 0.171 / 60 * size$clearance,
    cl3 = 0
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  list(
    PK = PK,
    tPeak = 0,
    MEAC = 0,
    typical = 0,
    upperTypical = 0,
    lowerTypical = 0,
    reference = paste0(
      "Jia Z et al., J Transl Med 2015;13:45 (two-compartment model of ",
      "anti-IIa activity during and after cardiopulmonary bypass in 41 Chinese ",
      "adults; Table 2 values as transcribed, awaiting a check against the ",
      "table). https://doi.org/10.1186/s12967-015-0404-5"
    )
  )
}
