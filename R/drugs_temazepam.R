# -----------------------------------------------------------------------------
# Temazepam: oral only, two compartments derived from van Steveninck 1994
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
# DISPOSITION: FITTED TO VAN STEVENINCK 1994
# ==========================================
# van Steveninck AL et al. (part II) infused 0.4 mg/kg intravenously over 30
# min into 9 healthy young volunteers (4 men, 5 women; 66 kg) on two occasions
# six months apart, sampled for 24 h, and fitted each subject with two
# compartments, reporting only the summary values (Table I):
#
#                      occasion 1     occasion 2     used
#     dose, mg         26.1 +/- 5.4   25.6 +/- 6.2   25.85
#     infusion, min    29 +/- 2       28 +/- 3       28.5
#     Cmax, ng/mL      964 +/- 167    1028 +/- 173   996
#     AUC 0-3 h        1.4            1.4            1.4 ug.h/mL
#     AUC 0-8 h        2.9            2.7            2.8
#     AUC 0-inf        6.4            5.9            6.15
#     half-life, h     10.7           10.4           10.55
#
# A two-compartment model was fitted to the five means (least squares on log
# ratios; script in the PR): V1 18.24 L, CL 4.165 L/h, Q 27.05 L/h, V2 40.35 L,
# which reproduce them within 1.5% (Cmax 1003, AUC 1.41, 2.76 and 6.21,
# half-life 10.5 h).  The distribution half-life is 0.30 h, against the
# label's 0.4-0.6 h, which the fit was not told.  Per kilogram of the mean
# weight, 66.5 kg:
#
#     V1 0.2743 L/kg   CL 0.06262 L/h/kg (1.04 mL/min/kg)
#     Q  0.4068 L/h/kg  V2 0.6067 L/kg
#
# This is a model built from published means, not a published model; the
# parameters are reproducible from the table and the fit.  The research
# formulation (polyethylene glycol 400) is not a product, and the earlier
# injectable forms caused "an unacceptably high incidence of venous
# thrombosis" (Halliday 1987), so the route offered is oral; the intravenous
# data set the systemic disposition under it.  The clearance, 1.04 mL/min/kg, sits within the oral literature (1.03,
# Ochs 1986; 1.02 women and 1.35 men, Divoll 1981; 1.59, Greenblatt 1984;
# 2.33, Ochs 1984).
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
#   20 mg peaks at 392 ng/mL at 0.68 h.  Observed 20 mg soft gelatin: 510 at
#     1.02 h (Muller, morning), 362 at 1.67 h (night), 617-708 at 30-40 min
#     (Drake 1991, n = 24).
#   30 mg peaks at 588 ng/mL.  Observed 30 mg capsules: 560 at 2.0 h
#     (Greenblatt 1984); label 865 (666-982) at 1.5 h.
#   30 mg nightly, day 7: 278 ng/mL 9 h after the dose and 103 at 24 h; label
#     (days 2-7) 260 +/- 210 and 75 +/- 80.
# The steady state and the overnight decline agree.  Single-dose peaks vary
# two-fold between studies with formulation and time of day; the model's
# sit within that range, below the faster soft gelatin capsules and the
# label.  (Means of individual peaks also run above the peak of a typical
# curve when absorption varies between people.)
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
# of the maximal fall in saccadic velocity).  Typical 400, about the peak
# after 20 mg.  The time-until-threshold level (endCe in the CSV, read
# against plasma since there is no effect site) is 250 ng/mL, the level above
# which psychometric performance deteriorated: after 20 mg the reference man
# falls below it 2.3 h after the dose, after 30 mg 7.1 h.
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
# against the sources first (van Steveninck parts I and II in full).
# -----------------------------------------------------------------------------

# Two compartments fitted to van Steveninck 1994 (part II, Table I), per kg
TEMAZEPAM_V1_PER_KG <- 0.2743    # L/kg
TEMAZEPAM_CL_PER_KG <- 0.06262   # L/h/kg
TEMAZEPAM_Q_PER_KG  <- 0.4068    # L/h/kg
TEMAZEPAM_V2_PER_KG <- 0.6067    # L/kg

#' Temazepam pharmacokinetics (oral)
#'
#' Two compartments fitted to the intravenous summary data of van Steveninck
#' et al. (1994), with the oral absorption of Muller et al. (1987) and the
#' label's bioavailability.  Plasma only.  See the file's header.
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
  # sedation target of van Steveninck 1994; typical about the 20 mg peak.
  typical      <- 400
  upperTypical <- 600
  lowerTypical <- 250

  reference <- paste0(
    "van Steveninck AL et al., Clin Pharmacol Ther 1994;55:546-555. ",
    "Two compartments fitted to the published intravenous means; oral ",
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
