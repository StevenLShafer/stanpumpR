# -----------------------------------------------------------------------------
# Naloxone: three-compartment intravenous model (Dowling 2008) with a
# concentrated nasal-spray route derived from Laffont 2024
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in ng/mL, total plasma.
#
# INTRAVENOUS DISPOSITION
# =======================
# Dowling et al. fitted a three-compartment population model to intravenous,
# intramuscular and intranasal naloxone in six healthy men:
#
#     CL  = 91 x (LBW/70)^0.75   L/h      LBW = Janmahasatian lean body weight
#     Vc  = 2.87 x (W/70)        L        W   = total body weight
#     Vp1 = 1.49 L   Q1 = 5.66 L/h
#     Vp2 = 33.6 L   Q2 = 29.8 L/h
#
# Note the clearance is normalised to a LEAN body weight of 70 kg, not to an
# ordinary 70 kg man: the reference male of this package (70 kg, 170 cm,
# fat-free mass 54.5 kg) has CL = 75.4 L/h, not 91.  The Janmahasatian lean
# body weight is the same quantity as the package's adult fat-free mass, so
# this model carries its own fat-free-mass covariate on clearance.
#
# This replaces the Papathanasiou 2019 weight-proportional model that the
# library carried before, so that the intravenous and intranasal routes rest
# on the same specification.
#
# INTRANASAL ROUTE: DERIVED, NOT FITTED
# =====================================
# Dowling's own intranasal arm used a dilute injectable solution through an
# atomiser (F 0.04, seven quantifiable samples) and does not describe the
# concentrated 4 mg / 0.1 mL spray in use today.  Laffont et al. modelled
# that spray in 60 adults during a remifentanil challenge, but on an
# APPARENT scale: CL/F 396 L/h, Vc/F 65.7 L, with a parallel zero-order
# (18%, 0.69 h) and lagged first-order (82%, 0.998 /h, lag 0.072 h) input.
# No absolute F was estimated.
#
# To offer the spray on Dowling's disposition the engine needs one F and one
# first-order constant:
#
#   - F = 75.4 / 396 = 0.190.  This is the bioavailability that makes the
#     reference man's nasal AUC equal Laffont's fitted D / (CL/F), which is
#     the one quantity the apparent model determines absolutely.  It is also
#     the FDA review's own route to F (fixing intravenous clearance), and it
#     agrees with the Narcan label's exposure (4 mg gives an AUC near
#     7.9 ng.h/mL, i.e. CL/F about 500 L/h).  It is much lower than the
#     0.47-0.52 "relative to intramuscular" figures on labels, which assume
#     complete intramuscular absorption that Dowling (F_IM 0.36) did not find.
#   - ka = 1.064 /h, no lag: one exponential whose mean input time equals
#     that of the whole published mixture (0.94 h), the moment-matching the
#     specification uses for its own inverse-Gaussian initialisers.  Laffont's
#     4.3 min lag on the first-order branch is folded into that mean rather
#     than carried separately: the library keeps its drugs lag-free, because
#     during a lag the engine has no state for the drug and the time until
#     threshold cannot be reported (see R/recoveryStates.R).  A single
#     first-order input starts more slowly than the published immediate
#     zero-order branch, so the first few minutes after a spray are
#     understated.
#
# Both numbers are derived initialisers, flagged as such; a direct fit of
# the spray to an intravenous anchor would replace them.
#
# EFFECT SITE
# ===========
# ke0 is supplied directly from Yassen 2007, who fitted a naloxone
# equilibration half-time of 6.5 min (ke0 6.40 /h) in the reversal of
# buprenorphine-induced respiratory depression.  Against this disposition
# that puts the peak effect of a bolus at 3.4 min.  No antagonism model is
# attached; the row shows naloxone alone, not the opioid it reverses.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# Clearance uses its own lean-body-weight covariate in both switch
# positions.  With the switch on, Vc sees the pharmacokinetic weight and the
# size-free peripheral volumes and clearances follow the library convention;
# with it off, Vc sees total body weight and the peripheral parameters are
# fixed, which is the published model exactly.
#
# References
# ----------
# Dowling J et al., Ther Drug Monit 2008;30:490-496.
#   https://doi.org/10.1097/FTD.0b013e3181816214
# Laffont CM et al., Front Psychiatry 2024;15:1399803.
#   https://doi.org/10.3389/fpsyt.2024.1399803
# Yassen A et al., Clin Pharmacokinet 2007;46:965-980.
#   https://doi.org/10.2165/00003088-200746110-00004
# -----------------------------------------------------------------------------

NALOXONE_KE0 <- 6.39828 / 60     # /min; Yassen 2007, t1/2 6.5 min
NALOXONE_LAFFONT_CL_F <- 396     # L/h, apparent clearance of the 4 mg spray

#' Naloxone pharmacokinetics
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
naloxone <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  # Size scaling (see the header).  size$ffm is the Janmahasatian lean body
  # weight the clearance covariate takes in either switch position.
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)
  pkW   <- 70 * size$volume
  fixV  <- if (isTRUE(adjustToFFM)) size$volume    else 1
  fixCL <- if (isTRUE(adjustToFFM)) size$clearance else 1

  cl1 <- 91 * (size$ffm / 70)^0.75 / 60     # L/min, the model's own covariate
  v1  <- 2.87 * pkW / 70
  v2  <- 1.49 * fixV
  v3  <- 33.6 * fixV
  cl2 <- 5.66 / 60 * fixCL
  cl3 <- 29.8 / 60 * fixCL

  # Intranasal: derived from Laffont 2024, see the header.  F is anchored on
  # the reference man's clearance so that his nasal AUC is the fitted one.
  clRef <- 91 * (FFM_REFERENCE / 70)^0.75             # 75.4 L/h
  ka_IN              <- 1 / 0.9402597 / 60             # 1/min, = 1.064 /h
  bioavailability_IN <- clRef / NALOXONE_LAFFONT_CL_F  # 0.190
  tlag_IN            <- 0                              # folded into ka, see header

  default <- list(
    v1 = v1,
    v2 = v2,
    v3 = v3,
    cl1 = cl1,
    cl2 = cl2,
    cl3 = cl3,
    ka_IN = ka_IN,
    bioavailability_IN = bioavailability_IN,
    tlag_IN = tlag_IN
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  MEAC <- 0
  # Band, ng/mL: what 0.4 mg intravenously produces after distribution.
  typical      <- 3
  upperTypical <- 10
  lowerTypical <- 1

  reference <- paste0(
    "Dowling J et al., Ther Drug Monit 2008;30:490-496 (intravenous ",
    "three-compartment model, clearance on lean body weight); intranasal ",
    "route derived from Laffont CM et al., Front Psychiatry 2024;15:1399803; ",
    "ke0 from Yassen A et al., Clin Pharmacokinet 2007;46:965-980. ",
    "https://doi.org/10.1097/FTD.0b013e3181816214"
  )

  return(
    list(
      PK = PK,
      # No tPeak: ke0 is supplied directly.  See the header.
      tPeak = 0,
      ke0 = NALOXONE_KE0,
      MEAC = MEAC,
      typical = typical,
      upperTypical = upperTypical,
      lowerTypical = lowerTypical,
      reference = reference
    )
  )
}
