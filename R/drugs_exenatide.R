# -----------------------------------------------------------------------------
# Exenatide, immediate release: two compartments, saturable absorption and
# saturable elimination, subcutaneous (Byetta)
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, amounts in ng, concentrations in
# pg/mL (= ng/L).  Concentration only: no glucose, insulin or HbA1c endpoint.
# Drafted by Claude Code, 2026-10-17, at the request of Steven L. Shafer; see
# the antidiabetic registry, inst/extdata/antidiabeticRegistry.csv.
#
# SOURCE
# ======
# Cirincione B, Mager DE.  Population pharmacokinetics of exenatide.  Br J
# Clin Pharmacol 2017;83:517-526.  Eight studies (healthy volunteers and
# patients with type 2 diabetes, normal renal function to end-stage renal
# disease), subcutaneous, intravenous bolus and continuous infusion; weight
# 52.6 to 162 kg.  Two compartments, parallel linear and Michaelis-Menten
# elimination, and a sequential zero-order then saturable subcutaneous
# absorption.  Table 2 and Equations 1-5:
#
#     dA1/dt = -k1 A1 - K0,                                 A1(0) = Dose
#     dA2/dt = k1 A1 + K0 - (Cld/Vc + Cl/Vc + Vmax/(Km Vc + A2)) A2
#              + (Cld/Vp) A3
#     dA3/dt = (Cld/Vc) A2 - (Cld/Vp) A3
#     K0 = F (1 - fr) Dose / tau for t <= tau, else 0
#     k1 = 0 for t <= tau, else F fr ka_max / (Km_ka + A1)
#
#     Cl     = 4.58 L/h x (eGFR / 80)^0.838     (MDRD eGFR)
#     Cld    = 3.72 L/h        Vp = 7.04 L
#     Vc     = 7.03 L x (WT / 84.8)^2.67
#     Vmax   = 1.55 mcg/h      Km = 567 pg/mL
#     ka_max = 12.8 mcg/h      Km_ka = 16.9 mcg
#     tau    = 1.35 h (fixed)  F = 1 (fixed)   fr = 0.628
#
# The equations run in the numerical engine, R/advanceMichaelisMenten.R
# (michaelisMenten block, with clLinear, a peripheral compartment from cl2
# and v2, and saturableSC), because neither the elimination nor the
# absorption is linear.  At the approved doses (5 and 10 mcg) the absorption
# is far from saturation (paper: "saturation of the absorption process is
# not expected"), so the depot empties at nearly F fr ka_max / Km_ka = 0.48/h.
#
# MULTIPLE DOSES.  The paper's tau and k1 are written for a single dose.
# Here each dose has its own zero-order box, and the remainders share one
# depot, which a second dose given before the first is absorbed (bid,
# 12 hours apart, leaves under 1% of the first) does not change in practice.
#
# RENAL FUNCTION: the MDRD eGFR, as in the paper (egfrMDRD(), without the race
# term), at the patient's creatinine (a child's on the adult scale) or an
# assumed normal creatinine for the patient's age and sex when the field is
# blank.  Not capped: the paper applies the power across its whole range.
#
# BODY SIZE: the model's own weight covariate on Vc, evaluated at the
# pharmacokinetic weight with the switch on and at total weight with it off.
# Its exponent, 2.67, is steep, so the weight is held within the studied 52.6
# to 162 kg for it: a 40 kg patient would otherwise have a Vc of 0.9 L.  The
# other parameters carry no size term in the source and none is added.
#
# CHECKS
# ======
# Linear clearance at eGFR 90: 5.06 L/h (paper: 5.06).  Total clearance at
# 210 pg/mL, Cl + Vmax / (Km + C): 5.1, 6.3 and 7.5 L/h at eGFR 50, 75 and
# 100 (paper: 5.1, 6.3 and 7.5).  A single 10 mcg dose, 75 kg, eGFR 90:
# peak 263 pg/mL at 1.35 h, the end of the zero-order phase, where the input
# falls to the depot's slower rate (paper: 287 pg/mL for the typical 75 kg
# subject, a summary of simulations with between-subject variability; the
# observed peaks were at about 2 h).  At 100 and 125 kg the peak is 22% and
# 38% lower (paper: 18% and 34%).  Twice daily, the third day's peak is 5%
# above the first.  See tests/testthat/test-drugs-exenatide.R.
#
# References
# ----------
# Cirincione B, Mager DE, Br J Clin Pharmacol 2017;83:517-526.
#   https://doi.org/10.1111/bcp.13135
# -----------------------------------------------------------------------------

EXENATIDE_WEIGHT_RANGE <- c(52.6, 162)   # kg, the studied range

#' Exenatide pharmacokinetics (immediate release, subcutaneous)
#'
#' Two compartments with parallel linear and saturable elimination and a
#' sequential zero-order then saturable absorption (Cirincione 2017), run by
#' the numerical engine (\code{advanceMichaelisMenten()}).  See the header of
#' the file.
#'
#' @inheritParams cefazolin
#' @param creatinine serum creatinine, mg/dL, or NULL for an assumed normal
#'   value
#' @returns a list in the shape \code{getDrugPK()} expects, with a
#'   \code{michaelisMenten} block
#' @export
exenatide <- function(weight, height, age, sex, adjustToFFM = TRUE,
                      creatinine = NULL)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)
  pkW  <- if (isTRUE(adjustToFFM)) size$pkWeight else weight
  wVc  <- min(max(pkW, EXENATIDE_WEIGHT_RANGE[1]), EXENATIDE_WEIGHT_RANGE[2])
  egfr <- egfrMDRD(age, sex, adultEquivalentCreatinine(creatinine, age, sex))

  vc       <- 7.03 * (wVc / 84.8)^2.67           # L
  clLinear <- 4.58 * (egfr / 80)^0.838 / 60      # L/min
  vmax     <- 1.55 * 1000 / 60                   # ng/min
  km       <- 567                                # pg/mL = ng/L

  default <- list(
    v1 = vc, v2 = 7.04, v3 = 1,
    # The low-concentration limit, linear plus Vmax / Km, for the help tables
    # only: the simulation uses the michaelisMenten block.
    cl1 = clLinear + vmax / km, cl2 = 3.72 / 60, cl3 = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  list(
    PK = PK, tPeak = 0, MEAC = 0,
    typical = 0, upperTypical = 0, lowerTypical = 0,
    reference = paste0(
      "Cirincione 2017 (eight studies, healthy volunteers and type 2 diabetes, ",
      "normal renal function to ESRD): two compartments, Cl 4.58 L/h x (MDRD ",
      "eGFR/80)^0.838 from the entered or an assumed normal creatinine, Vmax 1.55 ",
      "mcg/h, Km 567 pg/mL, Vc 7.03 L x (weight/84.8)^2.67, Cld 3.72 L/h, Vp 7.04 L; ",
      "37% of a dose zero order over 1.35 h, then saturable absorption (ka_max 12.8 ",
      "mcg/h, Km_ka 16.9 mcg); concentration only. https://doi.org/10.1111/bcp.13135"
    ),
    michaelisMenten = list(
      vmax = vmax,
      km = km,
      clLinear = clLinear,
      saturableSC = list(
        fraction = 0.628,                 # fr
        duration = 1.35 * 60,             # tau, min
        kaMax = 12.8 * 1000 / 60,         # ng/min
        kmKa = 16.9 * 1000,               # ng
        bioavailability = 1               # F, fixed
      )
    )
  )
}
