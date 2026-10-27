# -----------------------------------------------------------------------------
# Acenocoumarol: apparent two-compartment S-enantiomer PK driving the INR
# through an indirect-response model (Verstuyft 2012)
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min, S-acenocoumarol
# concentration in mg/L, and the plotted curve is the INR.  Concentration.Units
# "INR" (R/inr.R).
#
# SOURCE
# ======
# Verstuyft C, Delavenne X, Rousseau A, Robert A, Tod M, Diquet B, Lebot M,
# Jaillon P, Becquemont L.  A pharmacokinetic-pharmacodynamic model for
# predicting the impact of CYP2C9 and VKORC1 polymorphisms on fluindione and
# acenocoumarol during induction therapy.  Clin Pharmacokinet 2012;51(1):41-53.
# 24 White healthy subjects, open-label two-period crossover, a single 4 mg dose
# of acenocoumarol; Monolix.  Table III (S-acenocoumarol PK/PD) and Table IV
# (steady-state INRmax), and Equation 1 (the indirect response model), read from
# the full text on 2026-10-18.
#
# Acenocoumarol is the 4'-nitro analogue of warfarin, a coumarin vitamin-K
# antagonist widely used in Europe.  It is given as the racemate, but the INR is
# driven by the S-enantiomer alone (S-acenocoumarol is the major active molecule;
# R-acenocoumarol "data not shown" and not a predictor of the response).  So only
# the S-enantiomer is modelled, taking HALF of every racemic dose (doseFraction
# 0.5); its (apparent, /F) disposition is two-compartment with first-order input
# and a lag, and the INR is an indirect-response model (R/inr.R, type "indirect"):
#
#     E      = C_S^gamma / (C_S^gamma + C50^gamma)    (maximal effect fixed at 1)
#     d(1/INR)/dt = kin (1 - E) - kout (1/INR),       1/INR_0 = kin / kout
#
# PARAMETERS (Table III, typical 69 kg subject)
# =============================================
#     lag = 0.269 h     ka = 0.936 /h
#     CL  = 15.3 L/h x genotype        V1 = 24.7 L x (WT/69)
#     Q   = 4.85 L/h    V2 = 52.5 L
#     C50 = 0.0023 mg/L x VKORC1    kin = 0.0269 /h   kout = 0.0268 /h   gamma = 1.2
#
# COVARIATES (Monolix parameter x (1 + beta) per category; see below)
# ===================================================================
#   * CYP2C9: a *3 allele carrier (any of *1/*3, *2/*3, *3/*3) has CL x (1 - 0.3)
#     = x 0.7.  Only *3 was a predictor; *1/*2 and *2/*2 behave as the wild type.
#     (beta_CL = -0.3, Table III theta4.)
#   * VKORC1: a carrier of the less-sensitive allele has C50 x (1 + 1.29) = x 2.29,
#     so the sensitive genotype has the lower C50 and the higher INR, as Table IV
#     shows (TT INRmax 4.1 vs CT/CC 1.9 at *1/*3).  (beta_C50 = +1.29, theta10.)
#   * Body weight acts on V1 and is the model's OWN covariate.  There is no
#     CYP1A2 effect on acenocoumarol.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# The source's weight covariate acts on V1 alone (as WT/69); it is the model's
# OWN covariate and is evaluated at the pharmacokinetic weight with the
# adjustToFFM switch on (size$pkWeight) and at total body weight with it off, as
# edoxaban and dabigatran are.  CL, Q and V2 carry no weight term in the source
# and take the library's fat-free-mass factors with the switch on
# (size$clearance / size$volume); with it off they are as published
# (legacyVolume = 1), so the switch-off position reproduces the source's values.
#
# The VKORC1 1173C>T genotype maps onto the -1639G>A Patient Profile field through
# their linkage disequilibrium (AA = the sensitive 1173 TT, base C50; GG or GA =
# the less-sensitive "CC or CT", x(1 + 1.29) C50); see R/drugs_fluindione.R.
#
# S-acenocoumarol receives HALF of each racemic dose (doseFraction 0.5), the
# mechanistically correct input for the enantioselective CL_S/V_S fit.  The
# parameters and Equation 1 above are transcribed exactly from Table III; the
# simulated steady-state INR is within about 20-30% of the paper's Table IV
# INRmax, which cannot be reproduced exactly because the paper does not state the
# daily dose or the definition used for that steady-state table.  Fluindione,
# whose low clearance lets it accumulate to a near-steady concentration, does
# reproduce its Table IV values closely.

#
# THRESHOLDS
# ==========
# No effect site, no MEAC and no time until threshold.  A research and
# teaching model; a simulated INR is not a dose recommendation, a bleeding risk
# or a procedure-safe time; see inst/help/drugs/acenocoumarol.md.
# The band is the INR target, 2.0 to 3.0 with 2.5 typical (Holbrook 2012's
# target for every vitamin K antagonist; inst/extdata/therapeuticRanges.csv), drawn on
# Steven L. Shafer's decision of 2026-10-27 as a reference, not a dosing
# instruction.
#
# (Claude Code, 2026-10-18, at the request of Steven L. Shafer.)
# -----------------------------------------------------------------------------

#' Acenocoumarol pharmacokinetics and the INR (Verstuyft 2012)
#'
#' Apparent two-compartment S-acenocoumarol disposition (half of the racemic
#' dose) and the indirect-response model that gives the INR; see the header of
#' \code{R/drugs_acenocoumarol.R}.
#'
#' @param weight weight in kg
#' @param height height in cm; no published covariate of its own, but used by
#'   \code{pkSizeFactors()} to find the fat-free mass when \code{adjustToFFM} is
#'   \code{TRUE}, so it can shift the PK then
#' @param age age in years; no published covariate of its own, but used by
#'   \code{pkSizeFactors()} for the fat-free mass when \code{adjustToFFM} is
#'   \code{TRUE}
#' @param sex sex as a string; no published covariate of its own, but used by
#'   \code{pkSizeFactors()} for the fat-free mass when \code{adjustToFFM} is
#'   \code{TRUE}
#' @param adjustToFFM when \code{TRUE} the source's weight covariate on V1 is
#'   evaluated at the fat-free-mass pharmacokinetic weight and the size-free CL,
#'   Q and V2 take the library factors; when \code{FALSE} total body weight is
#'   used and CL, Q and V2 are the published values
#' @param cyp2c9 CYP2C9 diplotype, one of \code{CYP2C9_VALUES}. A \code{*3}
#'   carrier has reduced clearance; every other diplotype takes the wild-type
#'   clearance (the app passes this field).
#' @param vkorc1 VKORC1 -1639G>A genotype, one of \code{VKORC1_VALUES};
#'   \code{"GG"} or \code{"GA"} raises the C50 (less sensitive), \code{"AA"} is
#'   the sensitive reference (the app passes this field)
#' @returns a list in the shape \code{getDrugPK()} expects, with an indirect
#'   \code{inr} block and \code{doseFraction} 0.5 (the S-enantiomer's share of
#'   the racemic dose)
#' @export
acenocoumarol <- function(weight, height, age, sex, adjustToFFM = TRUE,
                          cyp2c9 = CYP2C9_DEFAULT, vkorc1 = VKORC1_DEFAULT)
{
  if (length(cyp2c9) != 1 || !cyp2c9 %in% CYP2C9_VALUES)
    stop("Invalid cyp2c9: ", paste(cyp2c9, collapse = ", "), ". Must be one of: ",
         paste(CYP2C9_VALUES, collapse = ", "))
  if (length(vkorc1) != 1 || !vkorc1 %in% VKORC1_VALUES)
    stop("Invalid vkorc1: ", paste(vkorc1, collapse = ", "), ". Must be one of: ",
         paste(VKORC1_VALUES, collapse = ", "))

  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1)
  pkW  <- if (isTRUE(adjustToFFM)) size$pkWeight else weight

  cyp2c9Factor <- if (grepl("\\*3", cyp2c9)) 1 - 0.3 else 1     # beta_CL = -0.3
  CL  <- 15.3 * cyp2c9Factor * size$clearance                 # L/h (size-free source)
  V1  <- 24.7 * (pkW / 69)                                     # L
  Q   <- 4.85 * size$clearance                                # L/h (size-free source)
  V2  <- 52.5 * size$volume                                   # L  (size-free source)
  ka  <- 0.936 / 60                                           # 1/min
  lag <- 0.269 * 60                                           # min

  sSet <- list(
    v1 = V1, v2 = V2, v3 = 1,
    cl1 = CL / 60, cl2 = Q / 60, cl3 = 0,
    ka_PO = ka, tlag_PO = lag, bioavailability_PO = 1
  )

  c50 <- 0.0023 * (if (vkorc1 %in% c("GG", "GA")) 1 + 1.29 else 1)  # beta_C50 = +1.29

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(sSet))

  list(
    PK = PK,
    doseFraction = 0.5,                                   # S gets half the racemate
    inr = list(type = "indirect", c50 = c50, gamma = 1.2,
               kin = 0.0269 / 60, kout = 0.0268 / 60),
    tPeak = 0,
    MEAC = 0,
    typical = 2.5,      # INR target band (header)
    upperTypical = 3,
    lowerTypical = 2,
    reference = paste0(
      "Verstuyft C et al., Clin Pharmacokinet 2012;51(1):41-53 (apparent ",
      "two-compartment S-acenocoumarol PK and an indirect-response INR model ",
      "with CYP2C9 and VKORC1; 24 healthy subjects; the wild-type CYP2C9 ",
      "unless *3 is set). https://doi.org/10.2165/11595560-000000000-00000"
    )
  )
}
