# -----------------------------------------------------------------------------
# Cetagliptin: two compartments, saturable efflux back to the gut, linear
# elimination, oral (investigational)
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min, amounts in
# mcg, concentrations in ng/mL (= mcg/L, the paper's unit).  Concentration
# only: the DPP-4 inhibition the source fits alongside is not plotted.  An
# investigational DPP-4 inhibitor (phase 3 in China when the source was
# published), not marketed in the United States.  Drafted by Claude Code,
# 2026-10-18, from Zhou 2024; moved to Lu 2024 on 2026-11-19 at the request
# of Steven L. Shafer; see the antidiabetic registry,
# inst/extdata/antidiabeticRegistry.csv.
#
# SOURCE
# ======
# Lu J, Zhao J, Xie D, Ding J, Yu Q, Wang T.  Use of a PK/PD model to select
# cetagliptin dosages for patients with type 2 diabetes in phase 3 trials.
# Clin Pharmacokinet 2024;63:1463-1476 (PMID 39367290, read in full).  108
# subjects given cetagliptin in four phase 1 studies (Table 1): single doses
# of 12.5 to 400 mg in healthy volunteers, a food-effect study (100 mg),
# 50, 100 and 200 mg daily for 7 to 10 days in healthy volunteers, and 50 and
# 100 mg daily for 14 days in patients with type 2 diabetes.  "A
# two-compartment model with first-order absorption, saturated efflux, and
# first-order elimination" (Table 2, apparent parameters, no covariates):
#
#     ka   = 1.56 /h
#     CL/F = 30.55 L/h      V/F  = 204 L
#     CL2/F = 133 L/h       V2/F = 160 L
#     efflux Vmax = 41731 mcg/h, Kt = 11.63 mcg/L, from the central
#     compartment back into the absorption compartment (Fig. 3)
#
# Exposure rises faster than the dose because the efflux saturates, not
# because elimination does: the authors say so, and the clearance is linear.
# The model runs in the numerical engine, R/advanceMichaelisMenten.R, with
# vmax 0, clLinear CL/F and a saturableEfflux block.
#
# WHY NOT ZHOU 2024 (the source until 2026-11-19).  Zhou C et al., Front
# Endocrinol 2024;15:1359407 (20 patients, 50 and 100 mg) fitted saturable
# ELIMINATION (Vmax 9373 mcg/h, Km 171.5 mcg/L, V 8.67 L, V2 558 L, Cl2 9.67
# L/h, ka 0.0652 /h).  Tested on Lu 2022 (Br J Clin Pharmacol, PMID
# 34965609, healthy volunteers, 50/100/200 mg daily for 7 days), which Zhou
# never saw, it doubles the steady-state exposure at 200 mg (AUC over a day
# 14,900 against the measured 7,790 ng.h/mL; accumulation 2.8 against 1.4).
# Lu 2024 fitted Lu 2022, so that study cannot test it; Zhou's patients can,
# and Lu 2024 predicts their AUC within 5-22% and Cmax within about 10%,
# better than Zhou's model predicts its own Cmax (23-27% low).  Shafer chose
# Lu 2024 on these checks (2026-11-19).
#
# BODY SIZE: no size covariate, so the parameters are the typical subject's
# of the pooled cohort.  Lu 2024 prints no demographics; its studies'
# reports do (Wang 2021, PMID 34655432, not yet in the archive; Lu 2022;
# Zhou 2024), so until all three are read the model keeps the reference
# anchor (docs/weight-adjustment.md, Anchoring at the source's cohort): the
# library's fat-free-mass factors, fixed with the switch off (legacyVolume =
# 1): volumes by the volume factor, CL/F, CL2/F and the efflux Vmax by the
# clearance factor, Kt unchanged.
#
# DOSE BASIS: doses of cetagliptin as the studies give them; no salt is
# named.  12.5 to 400 mg were studied.
#
# CHECKS (2026-11-19; a 70 kg reference man, switch off, so the published
# values; test-drugs-cetagliptin.R)
#   Lu 2022, healthy, day 1 / day 7 AUC0-24 (ng.h/mL), measured -> model:
#     50 mg  857 -> 875,  1440 -> 1637
#     100 mg 2280 -> 2170, 3120 -> 3273
#     200 mg 5500 -> 4904, 7790 -> 6547
#   Zhou 2024, type 2 diabetes, AUC0-24 measured -> model: 50 mg day 1 717
#     -> 875, day 14 1530 -> 1637; 100 mg day 1 1830 -> 2170, day 14 3120 ->
#     3273
# test-drugs-cetagliptin.R also checks the engine against an independent
# numerical solution of the same equations.
#
# References
# ----------
# Lu J et al., Clin Pharmacokinet 2024;63:1463-1476.
#   https://doi.org/10.1007/s40262-024-01427-7
# Lu J et al., Br J Clin Pharmacol 2022 (PMID 34965609).
#   https://doi.org/10.1111/bcp.15209
# Zhou C et al., Front Endocrinol 2024;15:1359407.
#   https://doi.org/10.3389/fendo.2024.1359407
# -----------------------------------------------------------------------------

#' Cetagliptin pharmacokinetics (oral, investigational)
#'
#' Two compartments, linear elimination and a saturable efflux back into
#' the gut (Lu 2024), run by the numerical engine
#' (\code{advanceMichaelisMenten()}).  See the header of the file.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects, with a
#'   \code{michaelisMenten} block
#' @export
cetagliptin <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1)
  cl <- 30.55 / 60 * size$clearance          # L/min
  efflux <- list(vmax = 41731 / 60 * size$clearance,   # mcg/min
                 km = 11.63)                            # mcg/L

  default <- list(
    v1 = 204 * size$volume, v2 = 160 * size$volume, v3 = 1,
    cl1 = cl, cl2 = 133 / 60 * size$clearance, cl3 = 0,
    ka_PO = 1.56 / 60,
    bioavailability_PO = 1,   # apparent parameters
    tlag_PO = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  list(
    PK = PK, tPeak = 0, MEAC = 0,
    typical = 0, upperTypical = 0, lowerTypical = 0,
    reference = paste0(
      "Lu 2024 (108 healthy volunteers and patients with type 2 diabetes, four ",
      "phase 1 studies, 12.5-400 mg): two compartments, CL/F 30.55 L/h, V/F 204 L, ",
      "CL2/F 133 L/h, V2/F 160 L, ka 1.56/h, and a saturable efflux from the central ",
      "compartment back into the gut, Vmax 41731 mcg/h, Kt 11.63 ng/mL (Table 2); ",
      "investigational; concentration only. https://doi.org/10.1007/s40262-024-01427-7"
    ),
    # Linear elimination (vmax 0) and the efflux back into the oral depot
    michaelisMenten = list(vmax = 0, km = 1, clLinear = cl,
                           saturableEfflux = efflux)
  )
}
