# -----------------------------------------------------------------------------
# Hydrocortisone: a LINEARISED form of the Bindellini 2024 model
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in mcg/mL of total cortisol ABOVE BASELINE (1 mcg/mL =
# 100 mcg/dL = 2759 nmol/L).
#
# WHAT THE SOURCE MODEL IS, AND WHY IT CANNOT BE RUN AS PUBLISHED
# ===============================================================
# Bindellini et al. fitted oral and intravenous hydrocortisone in 29 healthy
# men, with endogenous cortisol suppressed by dexamethasone on some
# occasions.  At 70 kg: CLu 106 L/h and Q 89.9 L/h acting on FREE cortisol,
# central volume Vc 2.15 L holding the TOTAL central amount, Vp 61.7 L, with
# allometric scaling (volumes x W/70, clearances x (W/70)^0.75).  Total and
# free central concentration are related by cortisol-binding globulin (CBG,
# Kd 9.71 nmol/L, capacity B 431 nmol/L at the source's reference CBG) plus
# a linear nonspecific term NS = 4.15:
#
#     Ct = (1 + NS) Cu + B Cu / (Kd + Cu)
#
# Binding is mass-bearing: the total amount is the state and the free
# concentration drives distribution and elimination.  That is a NONLINEAR
# system, and stanpumpR's closed-form engine is linear.  The model cannot be
# implemented as published.
#
# THE LINEARISATION USED HERE
# ===========================
# At the doses anaesthetists give (50-100 mg, total cortisol above
# 1000 nmol/L for hours), CBG is saturated: its contribution to Ct is nearly
# the constant B, and dCt/dCu is 1 + NS = 5.15 to within a few percent
# (5.4 at Cu = 110 nmol/L, 5.15 at saturation).  In that regime the model IS
# linear in the increment of total cortisol above the saturated bound pool:
#
#     dCt_exogenous = (1 + NS) Cu
#
# and rewriting the free-cortisol equations in terms of that increment gives
# an ordinary two-compartment model:
#
#     V1 = Vc            = 2.15 L         (the total-amount volume)
#     CL = CLu / (1+NS)  = 20.58 L/h
#     Q  = Qu  / (1+NS)  = 17.46 L/h
#     V2 = Vp  / (1+NS)  = 11.98 L
#
# Steady-state volume 14.1 L, clearance 20.6 L/h, mean residence time 0.69 h.
# The plotted concentration is the increment in total cortisol that an
# exogenous dose adds; endogenous cortisol and the bound pool at baseline are
# not plotted, and are not an input.
#
# WHERE IT IS WRONG
# =================
# Below about 300 nmol/L of total cortisol, which is where replacement doses
# of 5-20 mg spend most of their time, the CBG term is not saturated: it adds
# up to 44 x Cu to the total and makes the apparent volume several times
# larger and the decline several times slower than this model predicts.  The
# linearisation therefore UNDERSTATES THE TAIL of every curve, and the error
# grows as the concentration falls.  It also omits the part of the increment
# that the CBG pool itself carries, at most B = 0.156 mcg/mL (15.6 mcg/dL).
# For stress-dose simulation the first few hours are reasonable; for
# replacement dosing this model is the wrong tool and the source's full
# nonlinear model should be run instead.
#
# ORAL ROUTE
# ==========
# The source describes oral granules by a dose-dependent transit chain (mean
# 0.868 h at 5 mg) feeding a 24 /h depot, with an input scaling parameter
# BIO = 0.344.  The absorption constant here, 1.099 /h, matches the mean of
# that whole input (0.868 + 1/24 h) with a single first-order step.  The
# bioavailability is NOT the source's 0.344: that parameter sits inside a
# model whose dose-column convention could not be reproduced, and paired
# route studies of hydrocortisone tablets find F near 1 on total cortisol and
# 0.88 on calculated unbound cortisol (Johnson 2018, 14 dexamethasone-
# suppressed men).  Because the linearised model is driven by free cortisol,
# the unbound-based 0.88 is the matching estimand and is used here.  This is
# a cross-study choice and is flagged as such.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# The source's allometry on total weight is replaced by the same exponents on
# the fat-free-mass ratio with the switch on, and reproduced exactly with it
# off.
#
# References
# ----------
# Bindellini D et al., J Pharmacokinet Pharmacodyn 2024;51:809-824.
#   https://doi.org/10.1007/s10928-024-09934-7
# Johnson TN et al., J Bioequiv Availab 2018;10:001-003.
#   https://doi.org/10.4172/jbb.1000365
# -----------------------------------------------------------------------------

HYDROCORTISONE_NS <- 4.15     # Bindellini's fixed nonspecific binding factor

#' Hydrocortisone pharmacokinetics (linearised, saturated-binding regime)
#'
#' @inheritParams cefazolin
#' @param adjustToFFM scale volumes to the patient's fat-free mass and
#'   clearances to that ratio to the 0.75 power; when \code{FALSE}, use the
#'   published allometry on total body weight (volumes linear, clearances to the
#'   0.75 power). No renal-function estimate.
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
hydrocortisone <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  # Size scaling (see the header): published allometry on total weight,
  # reproduced exactly with the switch off.
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyClearance = (weight / 70)^0.75)
  bound <- 1 + HYDROCORTISONE_NS

  v1  <- 2.15 * size$volume
  v2  <- 61.7 / bound * size$volume
  v3  <- 1                                    # no third compartment
  cl1 <- 106  / bound / 60 * size$clearance   # L/min
  cl2 <- 89.9 / bound / 60 * size$clearance
  cl3 <- 0

  # Oral: see the header
  ka_PO              <- 1 / (0.868 + 1 / 24) / 60   # 1/min, = 1.099 /h
  bioavailability_PO <- 0.88                         # Johnson 2018, unbound basis
  tlag_PO            <- 0

  default <- list(
    v1 = v1,
    v2 = v2,
    v3 = v3,
    cl1 = cl1,
    cl2 = cl2,
    cl3 = cl3,
    ka_PO = ka_PO,
    bioavailability_PO = bioavailability_PO,
    tlag_PO = tlag_PO
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  tPeak <- 0     # genomic effect over hours; no effect-site model
  MEAC  <- 0

  # Band, mcg/mL above baseline: 20-100 mcg/dL, the range of a stress
  # response.  Orientation only.
  typical      <- 0.5
  upperTypical <- 1
  lowerTypical <- 0.2

  reference <- paste0(
    "Bindellini D et al., J Pharmacokinet Pharmacodyn 2024;51:809-824. ",
    "LINEARISED in the CBG-saturated regime; plotted as the increment in ",
    "total cortisol above baseline, understating the tail at low ",
    "concentrations. Oral F 0.88 from Johnson 2018. ",
    "https://doi.org/10.1007/s10928-024-09934-7"
  )

  return(
    list(
      PK = PK,
      tPeak = tPeak,
      MEAC = MEAC,
      typical = typical,
      upperTypical = upperTypical,
      lowerTypical = lowerTypical,
      reference = reference
    )
  )
}
