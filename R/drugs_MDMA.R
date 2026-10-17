# -----------------------------------------------------------------------------
# MDMA (3,4-methylenedioxymethamphetamine): oral, apparent parameters, plasma
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in ng/mL, venous plasma.
#
# RESEARCH MODEL, OPT-IN
# ======================
# MDMA is in the library as an evidence-labelled research model, behind the
# "Illicit drugs" opt-in (off by default; see R/illicit-drugs.R).  It is a
# plasma-exposure estimate, not dosing advice, a safety threshold, or a
# prediction of anyone's experience.  It describes pharmaceutical MDMA
# hydrochloride swallowed as a single dose in controlled studies, not material
# of unknown identity, purity or content.
#
# APPARENT PARAMETERS, SO ORAL ONLY
# =================================
# Huestis and colleagues fitted a one-compartment model with linear clearance
# to 2012 plasma MDMA observations from 65 healthy adults across two phase I
# studies, and reported apparent clearance and apparent volume -- clearance
# and volume over an unmeasured bioavailability.  Apparent parameters predict
# oral concentrations correctly, because the unknown factor cancels, and
# intravenous ones wrong by 1/F, so MDMA is offered as an ORAL unit only, with
# bioavailability carried as 1 because the apparent scale already contains it
# (docs/adding-a-drug.md).
#
# THE DOSE BASIS IS THE HYDROCHLORIDE
# ===================================
# The source doses MDMA hydrochloride throughout ("120 mg MDMA HCl", "a single
# oral dose of 100 mg MDMA HCl"), so its apparent parameters are on the SALT
# mass basis and the dose entered here is milligrams of MDMA hydrochloride.
# Entering a free-base mass against these parameters would under-predict
# exposure by the salt-to-base ratio.
#
# THE INPUT: A ZERO-ORDER DEPOT THE ENGINE CANNOT CARRY, DISCRETISED
# ==================================================================
# The published input is NOT a plain first-order depot.  The dose enters the
# depot at a constant rate over a duration D1 after a lag ALAG1, and the depot
# then empties into the central compartment first order with ka:
#
#     ka    = 1.17 /h      (95% CI 0.969-1.43)
#     ALAG1 = 0.322 h      (0.273-0.358)
#     D1    = 0.304 h      (0.141-0.485)
#
# (The paper's Methods and Discussion both call the absorption sequential
# zero-order then first-order; one Results sentence calls it parallel, but the
# reported parameter set has ALAG1, D1 and ka and NO mixing fraction, which a
# parallel model would need, so the sequential reading is the one implemented.)
#
# stanpumpR's closed-form engine has first-order depots only, so the zero-order
# filling is DISCRETISED, not approximated away: the dose is given as two
# halves entering first-order depots at the quarter points of the zero-order
# window,
#
#     tlag_PO  = ALAG1 + D1/4 = 23.88 min   (half the dose)
#     tlag_PO2 = ALAG1 + 3D1/4 = 33.00 min  (the other half)
#     ka_PO = ka_PO2 = 1.17 /h
#
# which is the two-point midpoint rule for the source's sliding input window.
# Against a numerical evaluation of the exact zero-order-then-first-order
# solution, this reproduces the peak concentration to 0.01% and stays within
# 0.53% of the exact curve everywhere after the input ends -- far inside the
# model's own uncertainty, whose clearance confidence interval alone spans
# about +/- 10% (test-drugs-MDMA.R pins both the agreement and the exact
# solution it is measured against).  A single depot lagged to the midpoint of
# the window, the cruder reduction, would reproduce the peak to 0.04% and the
# curve to 2.1%; the two-point rule is used because it costs nothing.
#
# WHAT IS NOT MODELLED
# ====================
#   * MDA, the active metabolite.  The source fitted it as a two-compartment
#     model (CLM 34.9 L/h, V3 20.8 L, QM 79.6 L/h, V4 213 L) driven by a
#     formation fraction FIXED at 0.1 rather than estimated, and fitted
#     sequentially from each subject's post-hoc parent estimates.  A fixed,
#     unestimated formation fraction is not a measured formation clearance, so
#     no MDA curve is offered; the help page says so.
#   * No effect site (tPeak = 0), no MEAC and no band.  No source reviewed for
#     this model fits a concentration to heart rate, blood pressure, euphoria
#     or toxicity in a form this engine could carry.
#
# SINGLE DOSE ONLY, BECAUSE MDMA INHIBITS ITS OWN METABOLISM
# ==========================================================
# MDMA is a strong mechanism-based inhibitor of CYP2D6, the enzyme that
# clears it, so its kinetics are not linear across repeated or split doses:
# that is why the source paired this linear population model with a separate
# physiologically based model for its split-dose simulations.  This model is
# calibrated on SINGLE doses and is linear, so a second dose entered a few
# hours after the first will under-predict the exposure that follows it.  The
# library cannot represent the auto-inhibition, and the help page says so.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# The source fitted its own size covariates, so they are carried rather than
# the package's: apparent clearance with an exponent of 1.03 on weight and
# apparent volume with 0.885, both normalised to 70 kg.  With the fat-free
# mass switch on, they are evaluated at the pharmacokinetic (fat-free-mass)
# weight; with it off, at total body weight, reproducing the published
# scaling.  Two covariates the source also fitted are deliberately NOT
# carried: the empirical study effect (36% lower apparent clearance and 26%
# lower apparent volume in one of the two studies, an idiosyncrasy of that
# dataset), so the parameters here are its NIDA reference individual's; and
# food on the absorption rate, which the app has no field for.  The published
# food effect is large and is on timing rather than exposure: a high-fat meal
# roughly halves ka and moved the median peak from 2.1 h to 4.0 h while area
# and peak concentration were unchanged.
#
# References
# ----------
# Huestis MA, Smith WB, Leonowens C, et al.  MDMA pharmacokinetics: a
#   population and physiologically based pharmacokinetics model-informed
#   analysis.  CPT Pharmacometrics Syst Pharmacol 2025;14(2):376-388.
#   https://doi.org/10.1002/psp4.13282  (65 healthy adults; one-compartment
#   oral model with sequential zero-order then first-order absorption, Table 2;
#   MDA fitted with a formation fraction fixed at 0.1; the fed/fasted
#   comparison in Table 1).
# -----------------------------------------------------------------------------

# Huestis 2025, Table 2, the NIDA reference individual at 70 kg
MDMA_CL_F    <- 41.5 / 60      # L/min, apparent clearance, from 41.5 L/h
MDMA_V_F     <- 462            # L, apparent central volume
MDMA_KA      <- 1.17 / 60      # 1/min, from 1.17 /h
MDMA_ALAG    <- 0.322 * 60     # min, lag before the zero-order input starts
MDMA_D1      <- 0.304 * 60     # min, duration of the zero-order input
MDMA_WT_CL   <- 1.03           # weight exponent on apparent clearance
MDMA_WT_V    <- 0.885          # weight exponent on apparent volume

#' MDMA pharmacokinetics: plasma, oral, apparent parameters
#'
#' @param weight patient weight (kg)
#' @param height patient height (cm)
#' @param age patient age (years)
#' @param sex \code{"male"} or \code{"female"}
#' @param adjustToFFM evaluate the source's own weight covariates at the
#'   patient's pharmacokinetic (fat-free-mass) weight; when \code{FALSE}, at
#'   total body weight, reproducing the published scaling
#' @returns a list in the shape \code{getDrugPK()} expects; plasma only, oral
#'   only, no effect site and no modelled metabolite (see the header)
#' @export
MDMA <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  # The source carries its own allometry on weight, so the package's factors
  # are used only to choose which weight it is evaluated at.
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)
  wt <- if (isTRUE(adjustToFFM)) size$pkWeight else weight

  v1  <- MDMA_V_F  * (wt / FFM_REFERENCE_WEIGHT)^MDMA_WT_V
  cl1 <- MDMA_CL_F * (wt / FFM_REFERENCE_WEIGHT)^MDMA_WT_CL
  v2  <- 1                       # one compartment
  v3  <- 1
  cl2 <- 0
  cl3 <- 0

  default <- list(
    v1 = v1,
    v2 = v2,
    v3 = v3,
    cl1 = cl1,
    cl2 = cl2,
    cl3 = cl3,
    # The source's zero-order depot input, discretised as two half doses at
    # the quarter points of its window (see the header).  Apparent
    # parameters, so bioavailability is 1 by convention.
    ka_PO = MDMA_KA,
    bioavailability_PO = 1,
    tlag_PO = MDMA_ALAG + MDMA_D1 / 4,
    ka_PO2 = MDMA_KA,
    fraction_PO2 = 0.5,
    tlag_PO2 = MDMA_ALAG + 3 * MDMA_D1 / 4
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  tPeak <- 0     # no concentration-effect model this engine could carry
  MEAC  <- 0

  # No established plasma range applies to this model, so no band is drawn.
  typical      <- 0
  upperTypical <- 0
  lowerTypical <- 0

  reference <- paste0(
    "Huestis MA et al., CPT Pharmacometrics Syst Pharmacol 2025;14(2):376-388 ",
    "(https://doi.org/10.1002/psp4.13282). One-compartment oral model, 65 ",
    "adults; apparent clearance and volume, so oral doses of MDMA ",
    "hydrochloride only. The source's zero-order depot input is discretised ",
    "as two half doses. Single-dose calibration: MDMA inhibits its own ",
    "CYP2D6 metabolism, which this linear model cannot represent. Plasma ",
    "only, no effect site, MDA not modelled; research use, not dosing advice."
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
