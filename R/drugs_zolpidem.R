# -----------------------------------------------------------------------------
# Zolpidem: oral only, one compartment, absorbed after a delay
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in ng/mL, total plasma.  Doses are mg of zolpidem TARTRATE,
# as tablets are labelled and as the source study dosed (10 mg tablets).
#
# SOURCE
# ======
# Kim HC et al. gave a single 10 mg immediate-release tablet to 30 healthy
# Korean adults (15 men, 15 women; 20-44 y, median 29.5; 50-83 kg, median
# 60.05), sampled to 12 h, and fitted a one-compartment model with transit-
# compartment absorption (NONMEM 7.5, FOCE-I).  Table 2:
#
#     CL/F = 18.0 L/h   (RSE 7.9%)       V/F = 64.0 L   (4.9%)
#     ka   = 11.7 /h    (36.8%)          MTT = 0.25 h   (8.4%)
#     NN   = 19.4       (18.8%)          BIO = 1 FIXED
#     IIV (%CV): CL/F 24.1, ka 330.8, MTT 37.7, BIO 25.9; V/F zero
#     proportional residual error 18.9%
#
# No covariate was retained: sex entered the forward step (CL/F 22% lower in
# women, dOFV -5.1) and left in the backward step.  The values were checked
# against the paper's Table 2 and agree with the ChatGPT specification this
# model was built from.  The same 30 subjects were also analysed by Cha 2024
# (CL/F 16.9 L/h, V/F 61.7 L, ka 5.41 /h, lag 0.394 h), from curves digitized
# for 23 of them; that is the same data analysed less directly, and is not used.
#
# ABSORPTION: THE TRANSIT CHAIN AS A LAG
# ======================================
# The engine has a lag and one first-order rate, not a transit chain.  With
# ktr = (NN + 1)/MTT = 81.6 /h, the chain delivers the dose as a gamma density
# of shape 20.4: a delay of 0.25 h with a standard deviation of 0.055 h, very
# nearly a pure delay.  It is replaced here by a lag equal to the mean transit
# time, 0.25 h, followed by Kim's ka of 11.7 /h, both as published.  Solved
# against the transit model (deSolve, Savic's gamma-function input, 10 mg):
# peak 142.5 against 141.7 ng/mL, at 0.58 against 0.60 h; from 0.75 h on the
# two curves agree within 0.1 ng/mL.  The difference is confined to the
# first 20 minutes, when the transit model has already begun to deliver drug
# and the lag has not (largest difference 29 ng/mL, at 0.25 h).
#
# The lag of 15 min is kept, as gabapentin's and pregabalin's are: time until
# threshold reads blank for those minutes after each dose (test-recovery-lag.R).
#
# APPARENT SCALE, ORAL ONLY
# =========================
# Zolpidem has no intravenous product and the model was fitted to oral data
# alone, so CL and V are divided by an unknown bioavailability (about 70%,
# Salva and Costa 1995).  They predict oral concentrations correctly and
# intravenous ones wrong by 1/F, so zolpidem is offered by mouth only, with
# bioavailability_PO = 1 because the apparent scale already contains F.
#
# Checks.  The reference patient (70 kg, 170 cm, 35 y man; size factors 1):
# 10 mg peaks at 142.5 ng/mL at 35 min, AUC 556 ng.h/mL, half-life 2.46 h.
# The Ambien label (n = 45): 121 (58-272) ng/mL at a mean of 1.6 h, half-life
# 2.5 (1.4-3.8) h; Greenblatt 2006 (n = 70): 14.0 ng/mL per mg, i.e. about 140
# after 10 mg, at 2.0 h.  The peak height and the half-life agree.  The peak
# is earlier than the US studies report, as it was in Kim's own subjects and
# in de Haas 2010 (median tmax 0.78 h, 0.33-2.50, n = 14): the Korean tablet
# taken fasting in the morning is absorbed fast.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# Kim retained no weight effect, so the published values are fixed and are
# read as the reference adult's.  Volume scales with fat-free mass relative to
# the 70 kg, 170 cm reference male and clearance with that ratio ^ 0.75;
# adjustToFFM = FALSE uses the published values for everyone.  (Kim's median
# subject weighed 60 kg.  Renormalising the published values to 70 kg
# allometrically would lower the reference man's peak to 123 ng/mL, closer to
# the label's 121; that is not done, because Kim found no weight effect to
# renormalise.)
#
# SEX
# ===
# The label reports peak and AUC about 45% higher in women at the same dose,
# and the FDA lowered the starting dose for women to 5 mg in 2013.  Kim did
# not retain sex.  Fat-free-mass scaling gives a typical woman part of the
# difference: 60 kg and 165 cm, her volume is 0.73 and her clearance 0.79 of
# the reference man's, so her peak is about 35% and her AUC 26% higher.  The
# rest (Greenblatt 2014: "incompletely explained by body weight") is not
# represented, and no sex term is added on top.
#
# NO EFFECT SITE
# ==============
# Kim modelled the Digit Symbol Substitution Test, choice reaction time and
# sleepiness as DIRECT functions of plasma concentration; an effect
# compartment was tested for sleepiness and did not improve the fit (dOFV
# -2.5), and the largest changes in DSST and reaction time coincided with the
# plasma peak (Yoon 2021).  No human ke0 for zolpidem has been published.  So
# only the plasma concentration is plotted, and it is the concentration that
# drives the effect.  (de Haas 2010 found effects waning faster than plasma
# concentrations fall, acute tolerance, which an effect site cannot
# represent.)  MEAC stays 0: zolpidem is not an opioid.
#
# BAND AND THRESHOLD
# ==================
# Band 80-200 ng/mL, the therapeutic range Cha 2024 quotes from a toxicology
# compilation (the compilation itself was not checked); typical 120, the
# label's mean peak after 10 mg.  The time-until-threshold level (endCe in the CSV, which for a
# drug without an effect site is read against plasma) is 50 ng/mL: "Zolpidem
# blood levels above approximately 50 ng/mL appear capable of impairing
# driving to a degree that increases the risk of a motor vehicle accident"
# (FDA Drug Safety Communication, January 2013).  Kim's DSST IC50 was 205 ng/mL
# for a 60 kg subject, and 50% impairment of reaction time 282 ng/mL; the
# threshold is far below both, because driving is impaired before a
# laboratory test detects it.
#
# NOT MODELLED
# ============
# Food (delays and lowers the peak), the extended-release and sublingual
# products (different inputs), the elderly (oral clearance about halved,
# Olubodun 2003), hepatic impairment, CYP3A4 inhibitors and inducers, and acute
# tolerance.  Kim's sample was 30 young healthy Korean adults.
#
# References
# ----------
# Kim HC et al., CPT Pharmacometrics Syst Pharmacol 2026;15:e70208.
#   https://doi.org/10.1002/psp4.70208
# Yoon S et al., Sci Rep 2021;11:19150 (the source trial, KCT0003934).
#   https://doi.org/10.1038/s41598-021-98689-z
# Cha HJ et al., Pharmaceutics 2024;16:689.
#   https://doi.org/10.3390/pharmaceutics16050689
# Salva P, Costa J, Clin Pharmacokinet 1995;29:142-153.
#   https://doi.org/10.2165/00003088-199529030-00002
# Greenblatt DJ et al., J Clin Pharmacol 2006;46:1469-1480.
#   https://doi.org/10.1177/0091270006293303
# Greenblatt DJ et al., J Clin Pharmacol 2014;54:282-290.
#   https://doi.org/10.1002/jcph.220
# de Haas SL et al., J Psychopharmacol 2010;24:1619-1629.
#   https://doi.org/10.1177/0269881109106898
# Olubodun JO et al., Br J Clin Pharmacol 2003;56:297-304.
#   https://doi.org/10.1046/j.0306-5251.2003.01852.x
# US Food and Drug Administration, Drug Safety Communication on zolpidem,
#   10 January 2013; Ambien (zolpidem tartrate) prescribing information.
# Savic RM et al., J Pharmacokinet Pharmacodyn 2007;34:711-726.
#   https://doi.org/10.1007/s10928-007-9066-0
#
# Drafted with Claude Code at the request of Steven L. Shafer, 2026-10-09,
# from a ChatGPT specification whose references and values were checked
# against the sources first.
# -----------------------------------------------------------------------------

ZOLPIDEM_CL   <- 18.0    # L/h, apparent (Kim 2026, Table 2)
ZOLPIDEM_V    <- 64.0    # L, apparent
ZOLPIDEM_KA   <- 11.7    # 1/h
ZOLPIDEM_MTT  <- 0.25    # h, mean transit time, used as the lag

#' Zolpidem pharmacokinetics (oral)
#'
#' Kim et al. (2026): one compartment with apparent (oral) clearance and
#' volume; the transit-compartment absorption is represented by a lag equal to
#' the mean transit time followed by the published ka.  No effect site: the
#' published effects are direct functions of plasma concentration.  See the
#' file's header.
#'
#' @inheritParams cefazolin
#' @param adjustToFFM \code{TRUE} (the default) scales the volume to
#'   fat-free mass and the clearance to its 0.75 power; \code{FALSE} uses the
#'   published values for everyone.
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
zolpidem <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  # Size scaling (see the header): fixed published values, read as the
  # reference adult's.
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1)

  default <- list(
    v1 = ZOLPIDEM_V * size$volume,
    v2 = 1,                                       # one compartment
    v3 = 1,
    cl1 = ZOLPIDEM_CL / 60 * size$clearance,      # L/min
    cl2 = 0,
    cl3 = 0,
    ka_PO = ZOLPIDEM_KA / 60,                     # 1/min
    bioavailability_PO = 1,                       # apparent (/F) parameters
    tlag_PO = ZOLPIDEM_MTT * 60                   # 15 min, the transit delay
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  # Band, ng/mL plasma: therapeutic range 80-200 (Cha 2024), typical the
  # label's mean peak after 10 mg.  The 50 ng/mL driving threshold is endCe
  # in the CSV.
  typical      <- 120
  upperTypical <- 200
  lowerTypical <- 80

  reference <- paste0(
    "Kim HC et al., CPT Pharmacometrics Syst Pharmacol 2026;15:e70208. ",
    "One compartment, apparent oral clearance and volume; the transit ",
    "absorption as a 0.25 h lag; plasma only (direct effect); oral only. ",
    "https://doi.org/10.1002/psp4.70208"
  )

  return(
    list(
      PK = PK,
      # No effect site: Kim's effects are direct (see the header).
      tPeak = 0,
      tPeakRoute = ROUTE_PO,
      MEAC = 0,
      typical = typical,
      upperTypical = upperTypical,
      lowerTypical = lowerTypical,
      reference = reference
    )
  )
}
