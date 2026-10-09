# -----------------------------------------------------------------------------
# Temazepam: oral only, two compartments fitted to van Steveninck 1994 and
# Halliday 1987
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in ng/mL, total plasma.
#
# WHY NOT THE SPECIFICATION'S MODEL
# =================================
# The ChatGPT specification this file was built from offered a one-compartment
# reduction of Ochs 1984 (20 mg orally in 10 young adults: V 1.45 L/kg, CL
# 2.33 mL/min/kg, half-life 8.6 h; verified against the abstract) and said
# itself that it could not be used for peaks: 30 mg spread through 1.45 L/kg
# gives at most 296 ng/mL, where the Restoril label reports 865.  Temazepam's
# decline is biphasic, with a distribution half-life of 0.4-0.6 h (label),
# so a model needs two compartments, and no published paper reports them.
#
# DISPOSITION: FITTED TO TWO INTRAVENOUS STUDIES
# =============================================
# No two-compartment temazepam model is published.  Two intravenous studies
# report mean data, and they disagree by about 1.6-fold in the first hours, so
# the model is fitted to both.
#
# van Steveninck AL et al. (part II) infused 0.4 mg/kg over 30 min into 9
# healthy young volunteers (4 men, 5 women) on two occasions six months
# apart, sampled for 24 h, fitted each subject with two compartments, and
# reported only each subject's end points (Table I):
#
#                      occasion 1     occasion 2     used
#     weight, kg       66             67             66.5 (text)
#     dose, mg         26.1 +/- 5.4   25.6 +/- 6.2   25.85
#     infusion, min    29 +/- 2       28 +/- 3       28.5
#     Cmax, ng/mL      964 +/- 167    1028 +/- 173   996
#     AUC 0-3 h        1.4            1.4            1.4 ug.h/mL
#     AUC 0-8 h        2.9            2.7            2.8
#     AUC 0-inf        6.4            5.9            6.15
#     half-life, h     10.7           10.4           10.55
#
# Halliday NJ et al. gave 11 young volunteers (68 kg) 20 mg intravenously over
# 20 s, in two vehicles, and plotted the means to 2 h (Figure 1; no table).
# Read from the figure by eye, averaging the two vehicles, which did not
# differ: 1250, 1010, 910, 810, 625, 500 and 420 ng/mL at 5, 10, 15, 30, 60,
# 90 and 120 min.  Scaled to the same dose and weight, these are about 1.6
# times van Steveninck's concentrations over the first hours.
#
# One two-compartment model, per kilogram, was fitted to both by least
# squares on log ratios, the five van Steveninck means and the seven Halliday
# points weighted equally, each study simulated at its own dose, infusion
# time and mean weight.  The script is data-raw/temazepam-fit.R; the
# derivation is written up in full in docs/temazepam.md.  Result, rounded to
# 4 decimals:
#
#     V1 0.2784 L/kg   CL 0.0661 L/h/kg (1.10 mL/min/kg)
#     Q  0.1115 L/h/kg  V2 0.5231 L/kg
#
# Half-lives 0.88 h and 10.8 h; Vss 0.80 L/kg.  Against van Steveninck: AUC
# 0-inf 0.96 and half-life 1.02 of the means, AUC 0-8 h 1.13, but Cmax 1.21
# and AUC 0-3 h 1.41.  Against Halliday: 0.80 at 5 min, 0.87-0.99 from 10 to
# 120 min.  A fit to van Steveninck alone (V1 0.2787, CL 0.0626, Q 0.4014
# L/h/kg, V2 0.6025 L/kg) reproduced its means within 1.5% but put Halliday's
# 30-120 min concentrations 35-44% low, and its 20 mg oral peak (391 ng/mL)
# below most of the oral studies; the joint fit is the one used.  Halving or
# doubling the weight on Halliday moves the 20 mg oral peak by about 5%.
# The joint fit's distribution half-life, 0.88 h, is close to the 1.03 h
# Muller fitted after morning oral doses; Drake found 0.5 h and the label
# says 0.4-0.6 h.  The clearance, 1.10 mL/min/kg, sits within the oral
# literature (1.03, Ochs 1986; 1.02 women and 1.35 men, Divoll 1981; 1.59,
# Greenblatt 1984; 2.33, Ochs 1984).
#
# This is a model built from published means, not a published model; the
# parameters are reproducible from the table, the figure and the script, and
# the test file checks that they are the fit's least-squares minimum.  Kept
# as is by decision of Steven L. Shafer, 2026-10-09.  The
# research formulations (polyethylene glycol, salicylate) are not products,
# and both of Halliday's caused "an unacceptably high incidence of venous
# thrombosis", so the route offered is oral; the intravenous data set the
# systemic disposition under it.
#
# ORAL ROUTE
# ==========
# Bioavailability 0.92: "minimal (8%) first pass metabolism" (Restoril
# label).  Absorption: Muller 1987 gave 20 mg in a soft gelatin capsule to 12
# men at 09:00 and 22:00 and fitted an absorption half-life of 0.38 h in the
# morning, 0.53 h at night; the morning value is used (the premedication
# setting, and the conditions of the intravenous study).  No lag.
#
# Checks, reference patient (70 kg, 170 cm, 35 y man):
#   20 mg peaks at 545 ng/mL at 55 min.  Observed 20 mg soft gelatin: 510 at
#     1.02 h (Muller, morning), 362 at 1.67 h (night), 617-708 at 30-40 min
#     (Drake 1991, n = 24); about 370 at 2 h and still rising (Halliday,
#     capsule), where the model gives 411.
#   30 mg peaks at 818 ng/mL.  Observed 30 mg capsules: label 865 (666-982)
#     at 1.5 h; 560 at 2.0 h (Greenblatt 1984).
#   30 mg nightly, day 7: 217 ng/mL 9 h after the dose and 82 at 24 h; label
#     (days 2-7) 260 +/- 210 and 75 +/- 80.
# Single-dose peaks vary two-fold between studies with formulation and time
# of day; the model's sit within that range.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# The infusion was dosed per kilogram and the derived parameters are per
# kilogram, so the rate constants are fixed: with the switch off, volumes and
# clearances both scale with weight / 70.  With it on (the default), volumes
# scale with fat-free mass relative to the 70 kg, 170 cm reference male and
# clearances with that ratio ^ 0.75.
#
# NO EFFECT SITE
# ==============
# Neither study found an equilibration delay.  Concentration-effect plots for
# saccadic peak velocity and EEG beta were linear and showed proteresis (the
# effect falling while the concentration was still high), not the hysteresis
# an effect-site delay produces; part I: "distribution of temazepam to the
# effect site is also not likely to have contributed".  Only the plasma
# concentration is plotted.  MEAC 0.
#
# BAND AND THRESHOLD
# ==================
# Band 250-600 ng/mL.  Psychometric performance deteriorated above about 250
# ng/mL (Saletu 1986, 10-40 mg); 597 +/- 123 ng/mL was the target van
# Steveninck chose for clear sedation short of sleep in awake volunteers (60%
# of the maximal fall in saccadic velocity).  Typical 400, between the two.
# The time-until-threshold level (endCe in the CSV, read against plasma since
# there is no effect site) is 250 ng/mL, the level above which psychometric
# performance deteriorated: after 20 mg the reference man falls below it 3.4
# h after the dose, after 30 mg 5.2 h.
#
# NOT MODELLED
# ============
# Sex (half-life longer and clearance about 25% lower in women, Divoll 1981),
# the hard capsule's slower absorption, night-time dosing, protein binding
# that varies with free fatty acids (van Steveninck part I), and the
# glucuronide (inactive).
#
# References
# ----------
# van Steveninck AL et al., Clin Pharmacol Ther 1994;55:546-555 (part II).
#   https://doi.org/10.1038/clpt.1994.68
# van Steveninck AL et al., Clin Pharmacol Ther 1994;55:535-545 (part I).
#   https://doi.org/10.1038/clpt.1994.67
# Muller FO et al., Eur J Clin Pharmacol 1987;33:211-214.
#   https://doi.org/10.1007/BF00544571
# Drake J et al., J Clin Pharm Ther 1991;16:345-351.
#   https://doi.org/10.1111/j.1365-2710.1991.tb00324.x
# Greenblatt DJ et al., J Pharm Sci 1984;73:399-401.
#   https://doi.org/10.1002/jps.2600730329
# Divoll M et al., J Pharm Sci 1981;70:1104-1107.
#   https://doi.org/10.1002/jps.2600701004
# Ochs HR et al., J Clin Pharmacol 1984;24:58-64.
#   https://doi.org/10.1002/j.1552-4604.1984.tb01814.x
# Halliday NJ et al., Br J Anaesth 1987;59:465-467.
#   https://doi.org/10.1093/bja/59.4.465
# Saletu B et al., Acta Psychiatr Scand Suppl 1986;332:67-94.
#   https://doi.org/10.1111/j.1600-0447.1986.tb08984.x
# Restoril (temazepam) prescribing information.
#
# Drafted with Claude Code at the request of Steven L. Shafer, 2026-10-09,
# from a ChatGPT specification whose references and values were checked
# against the sources first (van Steveninck parts I and II and Halliday in
# full).
# -----------------------------------------------------------------------------

# Two compartments fitted jointly to van Steveninck 1994 (part II, Table I)
# and Halliday 1987 (Figure 1), per kg: data-raw/temazepam-fit.R, derivation
# in docs/temazepam.md
TEMAZEPAM_V1_PER_KG <- 0.2784    # L/kg
TEMAZEPAM_CL_PER_KG <- 0.0661    # L/h/kg
TEMAZEPAM_Q_PER_KG  <- 0.1115    # L/h/kg
TEMAZEPAM_V2_PER_KG <- 0.5231    # L/kg

#' Temazepam pharmacokinetics (oral)
#'
#' Two compartments fitted to the intravenous mean data of van Steveninck et
#' al. (1994) and Halliday et al. (1987), with the oral absorption of Muller
#' et al. (1987) and the label's bioavailability.  Plasma only.  See the
#' file's header.
#'
#' @inheritParams cefazolin
#' @param adjustToFFM \code{TRUE} (the default) scales volumes to fat-free mass
#'   and clearances to its 0.75 power; \code{FALSE} scales both with weight.
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
temazepam <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  # Size scaling (see the header): per-kilogram source, legacy weight / 70.
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)

  default <- list(
    v1 = TEMAZEPAM_V1_PER_KG * 70 * size$volume,
    v2 = TEMAZEPAM_V2_PER_KG * 70 * size$volume,
    v3 = 1,                                              # two compartments
    cl1 = TEMAZEPAM_CL_PER_KG * 70 / 60 * size$clearance,  # L/min
    cl2 = TEMAZEPAM_Q_PER_KG * 70 / 60 * size$clearance,
    cl3 = 0,
    ka_PO = log(2) / (0.38 * 60),                        # Muller 1987, morning
    bioavailability_PO = 0.92,                           # label: 8% first pass
    tlag_PO = 0
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  # Band, ng/mL plasma: from the psychometric threshold (Saletu 1986) to the
  # sedation target of van Steveninck 1994.
  typical      <- 400
  upperTypical <- 600
  lowerTypical <- 250

  reference <- paste0(
    "van Steveninck AL et al., Clin Pharmacol Ther 1994;55:546-555, and ",
    "Halliday NJ et al., Br J Anaesth 1987;59:465-467: two compartments ",
    "fitted to the published intravenous means; oral ",
    "absorption from Muller FO et al., Eur J Clin Pharmacol 1987;33:211-214; ",
    "plasma only; oral only. https://doi.org/10.1038/clpt.1994.68"
  )

  return(
    list(
      PK = PK,
      # No effect site: no equilibration delay was found (see the header).
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
