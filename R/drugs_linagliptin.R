# -----------------------------------------------------------------------------
# Linagliptin: two compartments with target-mediated binding to DPP-4, oral
# (Tradjenta)
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min, amounts in
# mcg, concentrations in ng/mL (= mcg/L; 1 nmol/L of linagliptin, molecular
# weight 472.54, is 0.4725 ng/mL).  Concentration only: plasma DPP-4
# inhibition, which the source models as a direct Emax of the concentration,
# is not plotted.  Drafted by Claude Code, 2026-10-17, at the request of
# Steven L. Shafer; see the antidiabetic registry,
# inst/extdata/antidiabeticRegistry.csv.
#
# SOURCE
# ======
# Retlich S, Duval V, Graefe-Mody U, Jaehde U, Staab A.  Population
# pharmacokinetics and pharmacodynamics of linagliptin in patients with type 2
# diabetes mellitus.  Clin Pharmacokinet 2015;54:737-750.  462 patients with
# type 2 diabetes in two phase 1 and two phase 2b studies, 0.5 to 10 mg once
# daily.  Two compartments; linagliptin binds DPP-4, at equilibrium, in both
# (Fig. 1), and CL is "the clearance of the unbound concentration".  Table 4:
#
#     CL/F   = 258 L/h (unbound)     VC/F = 715 L
#     QP/F   = 412 L/h               VP/F = 1650 L
#     Bmax,C = 4.97 nmol/L (male), + 0.457 nmol/L in women,
#              x (1 + 0.00561 (age - 60))
#     Amax,P/F = 1650 nmol           Kd = 0.0652 nmol/L
#     Ka = 0.441 /h (studies 3 and 4, tablet formulation 2)
#     F  = 1 x (1 - 0.00958 (weight - 88))     (relative)
#
# QP, VP, Amax,P and Kd were fixed to the earlier model's estimates (Retlich
# et al. 2010).  The states are the total amounts; the unbound concentration
# solves Ctotal = Cu + Bmax Cu / (Kd + Cu) in the central compartment and
# the same, with Amax,P / VP, in the peripheral one.  The plasma concentration
# plotted is the total, which is what the assay measured.  The model runs in
# the numerical engine, R/advanceMichaelisMenten.R (rapidBinding), because
# the binding is saturable: the unbound fraction rises with the
# concentration, which is why linagliptin's exposure is less than dose
# proportional and its terminal phase so long.
#
# NOT APPLIED.  The covariates at the population's medians: baseline DPP-4
# activity (12,497 RFU) and GGT (33 U/L), which have no Patient Profile
# field.  The dose terms (Ka -6.51% and Bmax,C +3.41% per mg from 5 mg) are
# the 5 mg values, the only marketed strength; other doses are simulated
# with them.  Study 4's relative bioavailability of 169% and the phase 1
# formulations' faster Ka (0.933 and 0.795 /h) describe those studies.
#
# BODY SIZE: the model's own weight covariate, on F, evaluated at the
# pharmacokinetic weight with the switch on and at total weight with it off,
# with the weight held within the studied 57 to 132 kg (F would otherwise
# reach zero at 192 kg).  Age on Bmax,C is held within the studied 30 to 78
# years.  No other parameter carries a size term in the source.
#
# CHECKS
# ======
# 5 mg once daily, 60-year-old man, 88 kg, at steady state: trough about
# 5 nmol/L (paper: median steady-state trough 5.29 nmol/L, Fig. 3 of the
# supplement about 5.1 simulated and 5.6 observed) and peak about 9 nmol/L
# (the visual predictive check's median).  See
# tests/testthat/test-drugs-linagliptin.R.
#
# References
# ----------
# Retlich S et al., Clin Pharmacokinet 2015;54:737-750.
#   https://doi.org/10.1007/s40262-014-0232-4
# -----------------------------------------------------------------------------

LINAGLIPTIN_MOLECULAR_WEIGHT <- 472.54   # g/mol, so ng per nmol
LINAGLIPTIN_WEIGHT_RANGE <- c(57, 132)   # kg, studied
LINAGLIPTIN_AGE_RANGE <- c(30, 78)       # years, studied

#' Linagliptin pharmacokinetics (oral)
#'
#' Two compartments with saturable, equilibrium binding to DPP-4 (Retlich
#' 2015), run by the numerical engine (\code{advanceMichaelisMenten()}).  See
#' the header of the file.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects, with a
#'   \code{michaelisMenten} block
#' @export
linagliptin <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)
  pkW  <- if (isTRUE(adjustToFFM)) size$pkWeight else weight
  wF   <- min(max(pkW, LINAGLIPTIN_WEIGHT_RANGE[1]), LINAGLIPTIN_WEIGHT_RANGE[2])
  ageB <- min(max(age, LINAGLIPTIN_AGE_RANGE[1]), LINAGLIPTIN_AGE_RANGE[2])
  nmol <- LINAGLIPTIN_MOLECULAR_WEIGHT / 1000      # mcg per nmol

  bmax <- (4.97 + if (sex == SEX_FEMALE) 0.457 else 0) *
    (1 + 0.00561 * (ageB - 60))                     # nmol/L
  cl   <- 258 / 60                                  # L/min, unbound

  default <- list(
    v1 = 715, v2 = 1650, v3 = 1,
    # The unbound clearance, for the help tables only: the simulation uses
    # the michaelisMenten block, where the binding makes the clearance of
    # the total concentration far lower.
    cl1 = cl, cl2 = 412 / 60, cl3 = 0,
    ka_PO = 0.441 / 60,
    bioavailability_PO = 1 - 0.00958 * (wF - 88),   # relative F
    tlag_PO = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  list(
    PK = PK, tPeak = 0, MEAC = 0,
    typical = 0, upperTypical = 0, lowerTypical = 0,
    reference = paste0(
      "Retlich 2015 (462 patients with type 2 diabetes, 0.5-10 mg daily): two ",
      "compartments with equilibrium binding to DPP-4 (Bmax,C 4.97 nmol/L in men, ",
      "5.43 in women, x age; Amax,P 1650 nmol; Kd 0.0652 nmol/L), CL/F 258 L/h of ",
      "the unbound drug, VC/F 715 L, QP/F 412 L/h, VP/F 1650 L, ka 0.441/h, F x ",
      "(1 - 0.00958 (weight - 88)); total plasma concentration, DPP-4 inhibition ",
      "not plotted. https://doi.org/10.1007/s40262-014-0232-4"
    ),
    michaelisMenten = list(
      vmax = 0,
      km = 1,
      clLinear = cl,
      rapidBinding = list(
        bmaxCentral = bmax * nmol,          # mcg/L
        amaxPeripheral = 1650 * nmol,       # mcg
        kd = 0.0652 * nmol                  # mcg/L
      )
    )
  )
}
