# -----------------------------------------------------------------------------
# Atropine, intravenous (Hinderling 1985): plasma atropine in healthy men
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min.
#
# Drafted by Claude Code, 2026-10-19, at the request of Steven L. Shafer, as
# one of the vasoactive and resuscitation drugs used in intensive care
# (inst/help/models/vasoactive.md, inst/extdata/vasoactiveRegistry.csv).  Every
# number below was read from the published article, which he supplied: Table
# I and the Results.
#
# THE SOURCE
# ==========
# Hinderling PH, Gundert-Remy U, Schmidlin O.  Integrated pharmacokinetics
# and pharmacodynamics of atropine in healthy humans.  I: Pharmacokinetics.
# J Pharm Sci 1985;74:703-710 (doi 10.1002/jps.2600740702).  Three healthy
# men, 21-24 years, 66-79 kg, each given 1.35 and 2.15 mg (of the base) as a
# 3-minute infusion three or more weeks apart; plasma atropine by GC-MS to
# 10 hours, urine to 36 hours.  A linear two-compartment model was fitted
# simultaneously with heart rate and saliva flow (Part II, J Pharm Sci
# 1985;74:711-717), and the overall means of the six experiments, Table I, are
#
#     V_P (central)        19.1 L
#     CL                   1175 mL/min   (renal 656, hepatic 519)
#     k_PT                 0.560 /min
#     k_TP                 0.0557 /min
#
# so V2 = V_P k_PT / k_TP = 192.0 L and Q = k_PT V_P = 10.70 L/min.  The
# same table's other means are the checks, none used to build: the steady-
# state volume V_P (1 + k_PT/k_TP) is 211 L against 210 printed; the model's
# alpha is 0.672 /min against 0.684 and its terminal half-life 136 min
# against 132; and CL / beta is 230 L against the pseudo-steady-state volume
# of 234.
#
# DOSE BASIS
# ==========
# The source's doses are the base ("All the dosages of atropine refer to the
# base").  US injections are labelled as atropine sulfate, (C17H23NO3)2
# H2SO4 H2O, 694.83 g/mol for two molecules of base at 289.37, so a labelled
# dose carries 0.833 of its mass as base.  A dose here is entered as labelled
# and every volume and clearance is divided by that fraction, which is exact
# for a linear model.
#
# WHAT THIS MODEL IS NOT
# ======================
# Three young men.  The authors found the distribution dose-dependent (both
# volumes significantly smaller at the higher dose), so the linear model's
# volumes are averages over 1.35 and 2.15 mg.  Clearance falls and the
# half-life lengthens with age (Virtanen 1982), and renal clearance depends
# on urine flow; neither is a covariate here.  Racemic atropine is measured;
# only l-hyoscyamine is active.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# Fixed values for the source's adults, scaled to fat-free mass with the
# switch on; with it off they are the published values for everyone
# (legacyVolume = 1).
#
# NO EFFECT SITE, NO BAND
# =======================
# Part II related heart rate and saliva flow to the amount in the PERIPHERAL
# compartment, a structure the library's effect site does not represent, so
# no effect is drawn.
# -----------------------------------------------------------------------------

# Hinderling 1985, Table I, overall means (base)
ATROPINE_VP  <- 19.1      # L
ATROPINE_CL  <- 1.175     # L/min
ATROPINE_KPT <- 0.560     # /min
ATROPINE_KTP <- 0.0557    # /min
ATROPINE_BASE_MW    <- 289.37
ATROPINE_SULFATE_MW <- 694.83   # monohydrate, for two molecules of base

#' Atropine, intravenous (Hinderling 1985)
#'
#' Two compartments in healthy men; doses are atropine sulfate.  See the
#' header of \code{R/drugs_atropine.R}.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
atropine <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1)
  base <- 2 * ATROPINE_BASE_MW / ATROPINE_SULFATE_MW

  default <- list(
    v1  = ATROPINE_VP * size$volume / base,
    v2  = ATROPINE_VP * ATROPINE_KPT / ATROPINE_KTP * size$volume / base,
    v3  = 1,                          # placeholder: a two-compartment model
    cl1 = ATROPINE_CL * size$clearance / base,
    cl2 = ATROPINE_KPT * ATROPINE_VP * size$clearance / base,
    cl3 = 0
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Hinderling PH, Gundert-Remy U, Schmidlin O, J Pharm Sci 1985;74:703-710. ",
    "Two compartments in 3 healthy men (6 experiments): V_P 19.1 L, CL ",
    "1175 mL/min, k_PT 0.560 and k_TP 0.0557 /min (Vss 211 L, terminal ",
    "half-life 136 min).  Doses as atropine sulfate (the model's base x ",
    "694.83/578.74). https://doi.org/10.1002/jps.2600740702"
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
