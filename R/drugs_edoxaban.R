# -----------------------------------------------------------------------------
# Edoxaban: apparent oral two-compartment model with an absorption lag,
# pooled across phase 1 and the Hokusai-VTE phase 3 trial (Niebecker 2015)
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in ng/mL, total plasma edoxaban.
#
# SOURCE
# ======
# Niebecker R, Jonsson S, Karlsson MO, et al.  Population pharmacokinetics of
# edoxaban in patients with symptomatic deep-vein thrombosis and/or pulmonary
# embolism - the Hokusai-VTE study.  Br J Clin Pharmacol 2015;80:1374-1387.
# 17 406 concentrations from 4130 individuals: 13 phase 1 studies (443
# subjects) pooled with 9531 sparse samples from 3707 Hokusai-VTE patients.
# NONMEM; Table 3, "Final model estimate" column, read from the full text on
# 2026-10-17.  The structure is a linear two-compartment model with first-order
# absorption preceded by a fixed lag time, allometric weight on all disposition
# parameters, and an apparent clearance split into a non-renal term and a
# piece-wise-linear renal term in creatinine clearance:
#
#     CLnr/F = 15.2 L/h          Vc/F = 209 L        Vp/F = 92.3 L
#     Q/F    = 5.91 L/h          ka   = 3.36 /h      tlag = 0.250 h (fixed)
#
#     CLr/F  = theta_slope1 . CLcr                         if CLcr <= 90
#     CLr/F  = (theta_slope1 - s2) . 90 + s2 . CLcr        if CLcr > 90
#     CL/F   = (CLnr/F + CLr/F) . (WT / 70)^0.75           (Eqs 2-5)
#
# with theta_slope1 = 0.202 L/h per mL/min and the second (above-90) slope s2.
# In phase 1 s2 = theta_slope2 = 0.0321.  The phase 3 patients had a steeper
# dependence of renal clearance on CLcr: the paper reports the second slope as
# "+274%" in patients (abstract: "inter-compartmental clearance and the CLcr
# covariate effect were different in patients (+64.6% and +274%)"), i.e. an
# INCREASE of 274%, so the phase 3 second slope is theta_slope2 x (1 + 2.74) =
# 0.0321 x 3.74 = 0.120 L/h per mL/min.  (An interim version read the Table 3
# "274%" scaling parameter as a bare x2.74 = 0.088; the abstract's "+274%",
# parallel to the "+64.6%" Q/F increase that its footnote applies as x(1+0.646),
# makes it x(1+2.74) = x3.74 = 0.120.  The nlmixr2lib reproduction of this model
# uses 0.120.)
#
# WHAT IS FIXED HERE
# ==================
# The model is the phase 3, non-dose-reduced Hokusai-VTE patient on no P-gp
# inhibitor: the clinical patient for whom edoxaban is held before surgery.
# The library has no input for study phase, concomitant P-gp inhibitors or the
# protocol dose reduction, so the phase 3 renal slope is used, the phase 3
# intercompartmental clearance (Q/F x (1 + 0.646) = 9.73 L/h, the "+64.6%" from
# the abstract, Table 3 footnote) is used, no P-gp factor is applied (phase 1
# only: a P-gp inhibitor raised CL/F 17-53% and F 67-181%), and the "study 6"
# non-renal clearance (18.3 L/h) and ka change (+69%), which belong to one
# phase 1 renal-impairment study, are not used.  Race is fixed to non-Asian;
# Asian patients had a 22.6% higher Vc/F (on the volume, not the clearance, so
# the same steady-state exposure).  The typical Hokusai patient (80.5 kg, CLcr
# 99 mL/min) then has CL/F about 38 L/h and a terminal half-life of 9.7 h,
# reproducing the ~70-90 ng/mL steady-state exposure the paper's covariate
# plots show for 60 mg once daily.
#
# CREATININE CLEARANCE
# ====================
# Cockcroft-Gault (creatinineClearanceCG(), R/renalFunction.R) in mL/min, from
# the Patient Profile's serum creatinine put on the adult scale by
# adultEquivalentCreatinine(); with the field blank it is the ASSUMED NORMAL
# creatinine for age and sex, so renal impairment is then not represented.  The
# model truncated CLcr at 150 mL/min ("if CLcr was greater than 150 ml/min, CLcr
# was set to 150 ml/min"), so the renal term is held flat above 150; the fit
# otherwise covered CLcr roughly 14-247 mL/min.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# Allometric weight is the model's own covariate, with the fixed exponents the
# Table 3 footnote gives: CL/F and Vp/F scale as (WT/70)^0.75, and Vc/F and Q/F
# as (WT/70)^1.  (The per-parameter exponents are the paper's own pairing, not
# the textbook clearance-0.75/volume-1 convention: the central volume and the
# intercompartmental clearance take the first power, the apparent clearance and
# the peripheral volume the 0.75 power.)  Weight is evaluated at the
# pharmacokinetic weight with the switch on (size$pkWeight) and at total body
# weight with it off, as for cefazolin; it also enters CL/F through the
# Cockcroft-Gault CLcr, as the published model does.
#
# EFFECT SITE AND THRESHOLDS
# ==========================
# None: tPeak = 0 and the plotted concentration is plasma.  No band, no MEAC
# and no time until threshold: no therapeutic range is established, and a
# simulated level is not an anti-Xa activity or a procedure-safe time.
#
# (Claude Code, 2026-10-17, at the request of Steven L. Shafer.)
# -----------------------------------------------------------------------------

EDOXABAN_SLOPE1        <- 0.202       # L/h per mL/min, renal slope below CLcr 90
EDOXABAN_SLOPE2_PHASE3 <- 0.0321 * (1 + 2.74)  # above-90 slope, phase 3 (+274%)
EDOXABAN_CLCR_BREAK    <- 90          # mL/min, piece-wise breakpoint
EDOXABAN_CLCR_CAP      <- 150         # mL/min, CLcr truncated here
EDOXABAN_CLNR          <- 15.2        # L/h, apparent non-renal clearance
EDOXABAN_Q_PHASE3      <- 5.91 * (1 + 0.646)   # L/h, intercompartmental (+64.6%)

#' Edoxaban pharmacokinetics (apparent oral, Hokusai-VTE patient)
#'
#' Niebecker 2015; see the header of \code{R/drugs_edoxaban.R}.
#'
#' @inheritParams apixaban
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
edoxaban <- function(weight, height, age, sex, adjustToFFM = TRUE,
                     creatinine = NULL)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1)
  pkW  <- if (isTRUE(adjustToFFM)) size$pkWeight else weight

  crcl <- creatinineClearanceCG(pkW, age, sex,                 # mL/min
                                adultEquivalentCreatinine(creatinine, age, sex))
  crcl <- min(crcl, EDOXABAN_CLCR_CAP)                         # truncated at 150

  clr <- if (crcl <= EDOXABAN_CLCR_BREAK) {
    EDOXABAN_SLOPE1 * crcl
  } else {
    (EDOXABAN_SLOPE1 - EDOXABAN_SLOPE2_PHASE3) * EDOXABAN_CLCR_BREAK +
      EDOXABAN_SLOPE2_PHASE3 * crcl
  }
  cl <- (EDOXABAN_CLNR + clr) * (pkW / 70)^0.75                # L/h, allometric

  default <- list(
    v1  = 209 * (pkW / 70),                                    # Vc/F, non-Asian
    v2  = 92.3 * (pkW / 70)^0.75,                              # Vp/F
    v3  = 1,                                                   # two compartments
    cl1 = cl / 60,
    cl2 = EDOXABAN_Q_PHASE3 * (pkW / 70) / 60,                 # Q/F, phase 3
    cl3 = 0,
    ka_PO = 3.36 / 60,                                         # 1/min
    tlag_PO = 0.250 * 60,                                      # 15 min
    bioavailability_PO = 1                                     # apparent scale
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  list(
    PK = PK,
    tPeak = 0,
    MEAC = 0,
    typical = 0,
    upperTypical = 0,
    lowerTypical = 0,
    reference = paste0(
      "Niebecker R et al., Br J Clin Pharmacol 2015;80:1374-1387 (apparent oral ",
      "two-compartment model with a 0.25 h absorption lag, Hokusai-VTE phase 3 ",
      "patient, non-Asian, on no P-gp inhibitor; allometric weight; piece-wise ",
      "renal clearance from Cockcroft-Gault creatinine clearance truncated at ",
      "150 mL/min, from the entered creatinine or an assumed normal value for ",
      "age and sex). https://doi.org/10.1111/bcp.12727"
    )
  )
}
