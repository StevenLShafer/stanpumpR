# -----------------------------------------------------------------------------
# Cetagliptin: two compartments, saturable (Michaelis-Menten) elimination,
# oral (investigational)
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min, amounts in
# mcg, concentrations in ng/mL (= mcg/L, the paper's unit).  Concentration
# only: the DPP-4 inhibition the source fits alongside is not plotted.  An
# investigational DPP-4 inhibitor (phase 3 in China when the source was
# published), not marketed in the United States.  Drafted by Claude Code,
# 2026-10-18, at the request of Steven L. Shafer; see the antidiabetic
# registry, inst/extdata/antidiabeticRegistry.csv.
#
# SOURCE
# ======
# Zhou C, Zhou S, Wang J, Xie L, Lv Z, Zhao Y, et al.  Safety, tolerability,
# pharmacokinetics and pharmacokinetic-pharmacodynamic modeling of
# cetagliptin in patients with type 2 diabetes mellitus.  Front Endocrinol
# 2024;15:1359407.  20 Chinese adults with type 2 diabetes (weight about 71
# kg, BMI 19-30), 50 or 100 mg once daily, fasted, for 14 days; 560 plasma
# concentrations.  Supplementary Table 1 (Data Sheet 1, supplied by Steven
# L. Shafer), the final model:
#
#     ka   = 0.06521 /h
#     V    = 8.668 L        (central)
#     V2   = 558.0 L        (peripheral)
#     Cl2  = 9.671 L/h      (intercompartmental)
#     Vmax = 9373 mcg/h     Km = 171.5 mcg/L
#     dV2dTBIL = 0.3723     (total bilirubin on V2)
#
# All apparent (oral data only, no F).  The text calls the model "the
# two-compartment model" and never names the elimination; the table has no
# clearance but a Km and a Vmax, so the elimination is saturable,
# dA/dt = -Vmax C / (Km + C).  The data agree: the AUC over the first day is
# 2.55 times higher at 100 mg than at 50 mg (Table 2).  The model runs in the
# numerical engine, R/advanceMichaelisMenten.R, with the peripheral
# compartment from cl2 and v2.  Below Km the clearance is Vmax/Km = 54.7 L/h,
# so the plasma concentration follows the slow absorption closely and the
# peak comes early; the long terminal phase, about 40 h, is the return from
# the peripheral volume.
#
# NOT APPLIED: total bilirubin on V2.  The Patient Profile has no bilirubin
# field, and the paper prints neither the form of the term (its coefficient,
# 0.3723, is "fixed") nor the reference bilirubin; "V2 increases with the
# increase of TBIL".  V2 is the typical value, 558 L, which reproduces the
# paper's observed exposures (below).  Sex, weight, ALT, triglycerides, LDL
# cholesterol, glucose, urea and creatinine were tested and not retained.
#
# BODY SIZE: no size covariate in the source, so the library's fat-free-mass
# factors, fixed with the switch off (legacyVolume = 1): volumes by the volume
# factor, Vmax and Cl2 by the clearance factor, Km unchanged.
#
# DOSE BASIS: the paper gives doses of 50 and 100 mg of cetagliptin and does
# not name a salt; doses are entered as the paper gives them.  Only 50 and
# 100 mg were studied, and the saturable elimination makes the curve at other
# doses an extrapolation.
#
# CHECKS (Table 2, means of 10 patients; model, switch off)
#   50 mg, day 1:   AUC0-24 751 ng.h/mL (paper 717), Cmax 62 (80.5), Tmax 0.9 h
#   100 mg, day 1:  AUC0-24 1819 (1830), Cmax 160 (219)
#   50 mg, day 14:  AUC0-24 1222 (1530), Cmax 97 (162)
#   100 mg, day 14: AUC0-24 3604 (3120), Cmax 299 (300)
#   terminal half-life 40-41 h (paper 41.9 and 34.9 h)
# See tests/testthat/test-drugs-cetagliptin.R, which checks the engine
# against an independent numerical solution of the same equations.
#
# References
# ----------
# Zhou C et al., Front Endocrinol 2024;15:1359407.
#   https://doi.org/10.3389/fendo.2024.1359407
# -----------------------------------------------------------------------------

#' Cetagliptin pharmacokinetics (oral, investigational)
#'
#' Two compartments with saturable elimination (Zhou 2024), run by the
#' numerical engine (\code{advanceMichaelisMenten()}).  See the header of the
#' file.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects, with a
#'   \code{michaelisMenten} block
#' @export
cetagliptin <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1)
  vmax <- 9373 / 60 * size$clearance       # mcg/min
  km   <- 171.5                            # mcg/L

  default <- list(
    v1 = 8.668 * size$volume, v2 = 558.0 * size$volume, v3 = 1,
    # Vmax/Km, the clearance below Km, for the help tables only: the
    # simulation uses the michaelisMenten block.
    cl1 = vmax / km, cl2 = 9.671 / 60 * size$clearance, cl3 = 0,
    ka_PO = 0.06521 / 60,
    bioavailability_PO = 1,   # apparent parameters
    tlag_PO = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  list(
    PK = PK, tPeak = 0, MEAC = 0,
    typical = 0, upperTypical = 0, lowerTypical = 0,
    reference = paste0(
      "Zhou 2024 (20 Chinese adults with type 2 diabetes, 50 or 100 mg daily for 14 ",
      "days): two compartments with saturable elimination, Vmax 9373 mcg/h, Km 171.5 ",
      "ng/mL, V 8.67 L, V2 558 L, Cl2 9.67 L/h, ka 0.0652/h (Supplementary Table 1); ",
      "the bilirubin term on V2 is not applied; investigational; concentration only. ",
      "https://doi.org/10.3389/fendo.2024.1359407"
    ),
    michaelisMenten = list(vmax = vmax, km = km)
  )
}
