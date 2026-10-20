# -----------------------------------------------------------------------------
# Tolcapone (Tasmar): two compartments with first-order absorption, lean body
# weight on clearance and the central volume
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in mcg/mL (= mg/L) of tolcapone.
#
# Drafted by Claude Code, 2026-10-20, at the request of Steven L. Shafer, who
# supplied the article (inst/help/models/parkinsons.md,
# inst/extdata/parkinsonsRegistry.csv).
#
# THE SOURCE
# ==========
# Jorga K, Fotteler B, Banken L, Snell P, Steimer J-L.  Population
# pharmacokinetics of tolcapone in parkinsonian patients in dose finding
# studies.  Br J Clin Pharmacol 2000;49:39-48 (PMID 10606836), read in full.
# Three phase II dose-finding trials of tolcapone 50, 200 or 400 mg three
# times a day added to levodopa, sparse steady-state samples from 275
# patients (1414 concentrations), NONMEM IV.  The fluctuators (215 patients,
# the population tolcapone is indicated for) and the nonfluctuators (60) were
# fitted separately and gave similar models.  The fluctuator model is used
# here; Table 3:
#
#     CL  4.8 L/h  x (LBW/55)^0.73 x (protein/72)^-0.81
#     Vc  16 L     x (LBW/55)^0.65 x dose factor
#     Q   5.2 L/h
#     Vp  12 L     x (albumin/44)^2.82 x dose factor
#     ka  1.7 /h, no lag (a lag changed the objective function by 2)
#     F1  0.6 fasted, FIXED from the intravenous study (Jorga 1998), x 0.88
#         with food
#     dose factor on both volumes: 0.55 at 50 mg, 1 at 200 mg, 1.40 at 400 mg
#
# Because the absolute bioavailability was fixed, CL and the volumes are
# absolute, not apparent.  LBW is James's formula (the paper prints it, and
# cites James 1976 and Hallynck 1981), at the median 55 kg.
#
# WHAT IS HELD AT THE REFERENCE
# =============================
# Serum protein (72 g/L) and albumin (44 g/L), which the Patient Profile does
# not carry: the authors judged protein's effect on clearance at most +/-10%
# over 64-80 g/L.  The dose factor on the volumes, held at the 200 mg value
# (1): in the fluctuators the total volume rose with the regimen (15 L at
# 50 mg, 39 L at 400 mg three times a day), which the nonfluctuator data did
# not confirm and the authors attributed partly to "a few very high V
# estimates"; clearance, and so the steady-state exposure, did not depend on
# dose.  The model is drawn fasting (F 0.6); with food the exposure is 12%
# lower, which the label calls clinically irrelevant.
#
# CHECKS (LBW 55 kg, i.e. a 70 kg, 170 cm man, switch off): 200 mg three times
# a day gives a steady-state AUC over a dosing interval of 200 x 0.6 / 4.8 =
# 25 mcg.h/mL, against the 26.5 the paper reports as the mean of individual
# estimates (Table 4) and 24-27 in healthy elderly volunteers.  The terminal
# half-life is 4.9 h at the 200 mg reference (Table 4: 5.5 +/- 4.1 h in
# fluctuators), longer than the 2-3 h of healthy volunteers, whose Vss was 9 L
# against these patients' 28 L; the authors could not tell whether the larger
# volume is real or an artefact of sparse sampling, and the label's half-life
# is the volunteers'.
#
# COVARIATES AND GENOTYPE: no effect of age, sex, dopamine agonists, treatment
# duration or (in fluctuators) creatinine clearance.  Tolcapone is
# glucuronidated (UGT1A9); the label states that "polymorphic metabolism is
# unlikely", and no genotype term exists.  Hepatic impairment (cirrhosis
# halves the unbound clearance) is outside the model; the boxed warning is
# for fulminant hepatic failure.
#
# BODY SIZE (docs/weight-adjustment.md): the model's own lean body weight
# terms on CL and Vc, with James's formula evaluated at the pharmacokinetic
# weight with the fat-free-mass switch on and at total body weight with it off
# (as for istradefylline); the size-free Q and Vp are scaled by the library's
# factors, which are 1 with the switch off.
#
# BAND: the AGNP reference range, 3 to 6 mcg/mL two hours after a dose (quoted
# by Muller 2024), although therapeutic drug monitoring of tolcapone is not
# recommended.  The effect, COMT inhibition, follows the plasma concentration
# without a measurable delay and is not plotted.
#
# References
# ----------
# Jorga K et al. Br J Clin Pharmacol 2000;49:39-48.
#   https://doi.org/10.1046/j.1365-2125.2000.00113.x
# Tasmar label: https://www.accessdata.fda.gov/drugsatfda_docs/label/2013/020697s004lbl.pdf
# -----------------------------------------------------------------------------

# Jorga 2000, Table 3, fluctuator model
TOLCAPONE_CL      <- 4.8     # L/h at LBW 55 kg, protein 72 g/L
TOLCAPONE_CL_LBW  <- 0.73
TOLCAPONE_CL_PROTEIN <- -0.81 # held at the reference protein, not applied
TOLCAPONE_VC      <- 16      # L at LBW 55 kg, 200 mg
TOLCAPONE_VC_LBW  <- 0.65
TOLCAPONE_Q       <- 5.2     # L/h
TOLCAPONE_VP      <- 12      # L at albumin 44 g/L, 200 mg
TOLCAPONE_VP_ALBUMIN <- 2.82  # held at the reference albumin, not applied
TOLCAPONE_KA      <- 1.7     # /h
TOLCAPONE_F       <- 0.6     # fasted, fixed
TOLCAPONE_F_FOOD  <- 0.88    # relative, with food (not applied)
TOLCAPONE_LBW_REF <- 55      # kg, the median
TOLCAPONE_DOSE50_V  <- 0.55  # volumes at 50 mg tid (not applied)
TOLCAPONE_DOSE400_V <- 1.40  # volumes at 400 mg tid (not applied)

#' Tolcapone pharmacokinetics (oral)
#'
#' Two compartments with first-order absorption, lean body weight on
#' clearance and the central volume (Jorga 2000, fluctuator model), absolute
#' parameters with the bioavailability fixed at 0.6.  See the header of
#' \code{R/drugs_tolcapone.R}.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
tolcapone <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume = 1, legacyClearance = 1)
  w <- if (isTRUE(adjustToFFM)) size$pkWeight else weight
  lbw <- lbmJames(w, height, sex)

  default <- list(
    v1  = TOLCAPONE_VC * (lbw / TOLCAPONE_LBW_REF)^TOLCAPONE_VC_LBW,
    v2  = TOLCAPONE_VP * size$volume,
    v3  = 1,
    cl1 = TOLCAPONE_CL * (lbw / TOLCAPONE_LBW_REF)^TOLCAPONE_CL_LBW / 60,
    cl2 = TOLCAPONE_Q * size$clearance / 60,
    cl3 = 0,
    ka_PO = TOLCAPONE_KA / 60,
    bioavailability_PO = TOLCAPONE_F,
    tlag_PO = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  list(
    PK = PK, tPeak = 0, MEAC = 0,
    typical = 4.5, upperTypical = 6, lowerTypical = 3,
    reference = paste0(
      "Jorga K et al., Br J Clin Pharmacol 2000;49:39-48, fluctuator model (215 ",
      "patients): two compartments, CL 4.8 L/h x (LBW/55)^0.73, Vc 16 L x ",
      "(LBW/55)^0.65, Q 5.2 L/h, Vp 12 L, ka 1.7/h, bioavailability 0.6 fasted ",
      "(fixed); James lean body weight; serum protein, albumin and the volumes' ",
      "dose factor held at the reference (200 mg). Band 3-6 mcg/mL (AGNP). ",
      "https://doi.org/10.1046/j.1365-2125.2000.00113.x"
    )
  )
}
