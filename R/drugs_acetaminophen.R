# -----------------------------------------------------------------------------
# Acetaminophen (paracetamol): two-compartment intravenous and oral model
# (Morse 2022) with the analgesic effect site of Anderson 2001
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in mcg/mL (= mg/L), total plasma.
#
# DISPOSITION
# ===========
# Morse et al. pooled intravenous, tablet, sachet and suspension data from 116
# healthy adults (18-49 years, 49-116 kg; 6095 concentrations) and fitted a
# two-compartment model with first-order elimination:
#
#     CL = 24.0 x (NFM / NFMstd)^0.75   L/h
#     V1 = 43.7 x (TBW / 70)            L
#     Q  = 43.5 x (TBW / 70)^0.75       L/h
#     V2 = 29.7 x (TBW / 70)            L
#
#     NFM = FFM + 0.816 x (TBW - FFM)   FFM by Janmahasatian
#
# NFMstd is the normal fat mass of the paper's standard man, 70 kg and 176 cm,
# whose FFM the paper gives as 56.1 kg.  The code computes it from the same
# formula (56.13 kg, NFMstd 67.45 kg), so that man recovers 24.0 L/h exactly.
# The library's own reference man (70 kg, 170 cm, FFM 54.5 kg) is a little
# fatter, and his clearance is 23.92 L/h.
#
# All values are from Morse's Table 2 (final model): CL 24.0 L/h/70 kg,
# Q2 43.5 L/h/70 kg, V1 43.7 L/70 kg, V2 29.7 L/70 kg, FFAT on CL 0.816,
# FFAT on V fixed at 1 (volumes on total weight), oral F 0.859.  Size is
# scaled by (WT/70)^3/4 for clearances and ^1 for volumes, so Q2 is on total
# weight.  The abstract's "central volume of distribution ... 43.5 L/h/70 kg"
# is an error in the abstract: 43.5 is Q2 in the table.
#
# ORAL ROUTE
# ==========
# Fasted tablet, from the same paper's Table 2: bioavailability 0.859,
# absorption half-life 11.5 min, lag 5.30 min, all used as published.  The
# reference man's 1 g tablet peaks at 11.0 mg/L at 34 min.  (Morse's
# simulated population median, with between-subject variability, is
# 12.8 mg/L at 0.61 h.)
#
# Unlike the rest of the library, the lag is kept rather than folded into ka
# (Steven L. Shafer, 2026-10-08).  Folding it by mean input time lowered the
# tablet peak about 8%.  The cost is that for the 5.3 min between an oral
# dose and its absorption the engine holds no state for that dose, so the
# time until threshold does not count it (see R/recoveryStates.R).
#
# Food multiplies the tablet's absorption half-life by 1.87 and its lag by
# 4.63 (Morse Table 2); that is not modelled, so the oral curve is the
# FASTED curve.
#
# EFFECT SITE
# ===========
# ke0 is supplied directly from Anderson 2001: equilibration half-time 53 min
# for analgesia after tonsillectomy in children (9 +/- 3 years), Emax 5.17
# pain units (VAS 0-10), EC50 9.98 mg/L, Hill 1.  Applying a paediatric,
# oral-derived equilibration half-time to adults and to intravenous doses is
# an assumption, though it is the one the Auckland group makes throughout.
# Allegaert 2013 found a slower half-time (1.58 h) in neonates on a
# different pain scale; it is not used.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# Clearance uses its own normal-fat-mass covariate in both switch positions;
# size$ffm is the Janmahasatian FFM in adults.  With the switch on, the
# volumes see the pharmacokinetic weight and Q takes the library clearance
# factor; with it off, they see total body weight as published.
#
# NOT MODELLED
# ============
# Maturation.  The source is adult only, and the model has no maturation term,
# so it overpredicts clearance (underpredicts concentration) in neonates and
# infants under about a year, in whom clearance is still maturing.  In older
# children it is allometric extrapolation on normal fat mass.  Also not
# modelled: the glucuronide, sulfate and NAPQI metabolites, hepatotoxicity,
# hepatic impairment, food, and rectal administration.
#
# References
# ----------
# Morse JD et al., Eur J Drug Metab Pharmacokinet 2022;47:497-507.
#   https://doi.org/10.1007/s13318-022-00766-9
# Anderson BJ et al., Eur J Clin Pharmacol 2001;57:559-569.
#   https://doi.org/10.1007/s002280100367
# -----------------------------------------------------------------------------

ACETAMINOPHEN_KE0 <- log(2) / 53            # /min; Anderson 2001, t1/2 53 min
ACETAMINOPHEN_FFAT_CL <- 0.816              # Morse 2022, fat fraction in NFM

#' Acetaminophen pharmacokinetics
#'
#' @inheritParams cefazolin
#' @param adjustToFFM when \code{TRUE}, evaluate the published total-weight
#'   terms on volume and intercompartmental clearance at the pharmacokinetic
#'   weight; when \code{FALSE}, at total body weight. Clearance follows its own
#'   normal-fat-mass covariate either way.
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
acetaminophen <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  # Size scaling (see the header)
  size  <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                         legacyClearance = (weight / 70)^0.75)
  pkW   <- if (isTRUE(adjustToFFM)) size$pkWeight else weight

  # Morse's standard man: 70 kg, 176 cm
  ffmStd <- ffmAlSallami(70, 176, FFM_REFERENCE_AGE, SEX_MALE)    # 56.13 kg
  nfmStd <- ffmStd + ACETAMINOPHEN_FFAT_CL * (70 - ffmStd)       # 67.45 kg
  nfm    <- size$ffm + ACETAMINOPHEN_FFAT_CL * (weight - size$ffm)

  cl1 <- 24.0 * (nfm / nfmStd)^0.75 / 60     # L/min, the model's own covariate
  v1  <- 43.7 * pkW / 70
  v2  <- 29.7 * pkW / 70
  cl2 <- 43.5 * size$clearance / 60
  v3  <- 1                                   # two compartments
  cl3 <- 0

  # Oral, fasted tablet (Morse Table 2), lag kept as published
  ka_PO              <- log(2) / 11.5    # 1/min, absorption half-life 11.5 min
  bioavailability_PO <- 0.859            # FPARA
  tlag_PO            <- 5.3              # min

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

  # Not an opioid, so not on the MEAC panel.
  MEAC <- 0
  # Band, mg/L effect site: 3 to 15, the range adult dosing actually reaches
  # (1 g IV peaks near 7.3 mg/L in the effect site; 1 g every 6 h averages
  # about 7).  The typical line stays at Anderson's 10 mg/L target, at which
  # the expected pain reduction is about 2.6 VAS units (EC50 9.98 mg/L).  The
  # recovery threshold (endCe in the CSV) is 5 mg/L, chosen by Steven L.
  # Shafer (2026-10-08) so that time until threshold is informative for
  # ordinary adult doses; at 10 mg/L it read zero for them.
  typical      <- 10
  upperTypical <- 15
  lowerTypical <- 3

  reference <- paste0(
    "Morse JD et al., Eur J Drug Metab Pharmacokinet 2022;47:497-507 ",
    "(intravenous and fasted-tablet two-compartment model, clearance on ",
    "normal fat mass); ke0 from Anderson BJ et al., Eur J Clin Pharmacol ",
    "2001;57:559-569. https://doi.org/10.1007/s13318-022-00766-9"
  )

  return(
    list(
      PK = PK,
      # No tPeak: ke0 is supplied directly.  See the header.
      tPeak = 0,
      ke0 = ACETAMINOPHEN_KE0,
      MEAC = MEAC,
      typical = typical,
      upperTypical = upperTypical,
      lowerTypical = lowerTypical,
      reference = reference
    )
  )
}
