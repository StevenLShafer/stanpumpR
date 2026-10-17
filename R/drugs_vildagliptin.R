# -----------------------------------------------------------------------------
# Vildagliptin: two compartments with target-mediated disposition, oral
# (Galvus)
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min, amounts in
# mcg, concentrations in ng/mL (= mcg/L; 1 nmol of vildagliptin, molecular
# weight 303.40, is 0.3034 mcg).  Concentration only: the DPP-4 activity the
# source fits alongside, and the GLP-1, glucose and insulin of its companion
# paper, are not plotted.  Marketed outside the United States.  Drafted by
# Claude Code, 2026-10-17, at the request of Steven L. Shafer; see the
# antidiabetic registry, inst/extdata/antidiabeticRegistry.csv.
#
# SOURCE
# ======
# Landersdorfer CB, He Y-L, Jusko WJ.  Mechanism-based population
# pharmacokinetic modelling in diabetes: vildagliptin as a tight binding
# inhibitor and substrate of dipeptidyl peptidase IV.  Br J Clin Pharmacol
# 2012;73:391-401.  13 patients with type 2 diabetes, 10, 25 and 100 mg
# twice daily for 28 days, with the intravenous data of an absolute
# bioavailability study.  The equations on p. 393-394 and Table 1:
#
#     dAG1/dt = -ka1 AG1,                     AG1(0) = Dose x F
#     dAG2/dt = ka1 AG1 - ka2 AG2
#     dAC/dt  = ka2 AG2 - (CL + CLic)/VC AC + CLic/VP AP
#               - (RmaxC - DRC) k2 (AC/VC) / (Kd + AC/VC) + koff DRC
#     dAP/dt  = CLic (AC/VC - AP/VP)
#               - (RmaxP - DRP) k2 (AP/VP) / (Kd + AP/VP) + koff DRP
#     dDRC/dt = (RmaxC - DRC) k2 (AC/VC) / (Kd + AC/VC) - (koff + kdeg) DRC
#     dDRP/dt = (RmaxP - DRP) k2 (AP/VP) / (Kd + AP/VP) - (koff + kdeg) DRP
#
#     CL   = 36.4 L/h     VC = 22.2 L     VP = 97.3 L     CLic = 40.1 L/h
#     ka1  = 1.26 /h      ka2 = 1.05 /h   F = 0.772 (fixed, intravenous data)
#     Kd   = 71.9 nmol/L  k2 = 23.4 /h    koff = 0.612 /h  kdeg = 0.110 /h
#     RmaxC = 5.0 nmol    RmaxP = 13.0 umol
#
# The plasma concentration is AC/VC, the free drug.  The model runs in the
# numerical engine, R/advanceMichaelisMenten.R (targetBinding, with the two
# absorption compartments as kaTransit and ka_PO): DPP-4 in the tissues binds
# a large share of a small dose, so clearance falls and the terminal phase
# lengthens as the dose rises.  Binding to tissue DPP-4 while it is free is
# the fastest process (a rate constant of about 0.7/min); the engine's
# 2 minute step resolves it to about 1e-5 of the concentration.
#
# No covariates: 13 patients.
#
# BODY SIZE: the library's fat-free-mass factors, fixed with the switch off
# (legacyVolume = 1): volumes and the amounts of DPP-4 by the volume factor,
# clearances by the clearance factor.  The patients weighed 91 kg on
# average (65 to 116).
#
# CHECKS
# ======
# Total clearance (F x dose / AUC) after a single 100 mg dose is 42.1 L/h
# (paper: 42.4 L/h), and after 1 mcg 74.2 L/h (paper: about 74 L/h).  At
# steady state on 10, 25 and 100 mg twice daily, 91 kg, the peaks are 40,
# 107 and 461 ng/mL; the medians of the paper's Figure 2, read from its log
# scale, are about 45, 120 and 500.  See
# tests/testthat/test-drugs-vildagliptin.R.
#
# RENAL IMPAIRMENT is not in the model (13 patients, no covariates).
# Scheen 2015 (Clin Pharmacokinet 54:1-21, Table 1; He et al.) gives 50 mg
# once daily for 14 days: steady-state AUC 990 ng.h/mL and peak 251 ng/mL
# with normal renal function (the model, 80 kg: 844 and 209), and AUC 1.40,
# 1.71 and 2.00 times higher in mild, moderate and severe impairment, which
# the model does not reproduce.
#
# References
# ----------
# Landersdorfer CB, He Y-L, Jusko WJ, Br J Clin Pharmacol 2012;73:391-401.
#   https://doi.org/10.1111/j.1365-2125.2011.04108.x
# -----------------------------------------------------------------------------

VILDAGLIPTIN_MOLECULAR_WEIGHT <- 303.40   # g/mol, so ng per nmol

#' Vildagliptin pharmacokinetics (oral)
#'
#' Two compartments with target-mediated binding to DPP-4 and its hydrolysis
#' by DPP-4 (Landersdorfer 2012), run by the numerical engine
#' (\code{advanceMichaelisMenten()}).  See the header of the file.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects, with a
#'   \code{michaelisMenten} block
#' @export
vildagliptin <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1)
  nmol <- VILDAGLIPTIN_MOLECULAR_WEIGHT / 1000   # mcg per nmol
  cl   <- 36.4 / 60 * size$clearance             # L/min

  default <- list(
    v1 = 22.2 * size$volume, v2 = 97.3 * size$volume, v3 = 1,
    # The linear clearance, for the help tables only: the simulation uses
    # the michaelisMenten block.
    cl1 = cl, cl2 = 40.1 / 60 * size$clearance, cl3 = 0,
    ka_PO = 1.05 / 60,               # ka2, out of the second compartment
    bioavailability_PO = 0.772,
    tlag_PO = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  list(
    PK = PK, tPeak = 0, MEAC = 0,
    typical = 0, upperTypical = 0, lowerTypical = 0,
    reference = paste0(
      "Landersdorfer 2012 (13 patients with type 2 diabetes, 10-100 mg twice daily): ",
      "two compartments with target-mediated binding to DPP-4 and hydrolysis by it ",
      "(Kd 71.9 nmol/L, k2 23.4/h, koff 0.612/h, kdeg 0.110/h, DPP-4 5 nmol central ",
      "and 13 umol tissue), CL 36.4 L/h, VC 22.2 L, CLic 40.1 L/h, VP 97.3 L, two ",
      "absorption compartments (1.26 and 1.05/h), F 0.772; concentration only. ",
      "https://doi.org/10.1111/j.1365-2125.2011.04108.x"
    ),
    michaelisMenten = list(
      vmax = 0,
      km = 1,
      clLinear = cl,
      kaTransit = 1.26 / 60,           # ka1
      targetBinding = list(
        rmaxCentral = 5.0 * nmol * size$volume,        # mcg
        rmaxPeripheral = 13000 * nmol * size$volume,   # mcg
        kd = 71.9 * nmol,                              # mcg/L
        k2 = 23.4 / 60,
        koff = 0.612 / 60,
        kdeg = 0.110 / 60
      )
    )
  )
}
