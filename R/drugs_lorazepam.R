# -----------------------------------------------------------------------------
# Lorazepam: two-compartment intravenous kinetics (Nielsen-Kudsk 1983) with
# the oral and intramuscular routes of Greenblatt 1982 and the EEG effect site
# of Greenblatt 2000
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in ng/mL, total plasma.
#
# WHY NOT THE MODELS IN THE SPECIFICATION
# =======================================
# The ChatGPT specification this file was built from offered two intravenous
# models.  Both were checked against the papers and neither is used:
#
# - Gonzalez 2017 (145 children, 0.3-17.8 y, status epilepticus) is verified
#   as quoted, but its age term is fitted on children only.
# - Swart 2004 (critically ill adults on infusions for 24-715 h) is verified
#   from the PDF.  Table 4: CL 4.13 - 0.417 (PEEP - 5) L/h, 0.74 L/h with
#   alcohol abuse; V 0.743 L; Vss 156 - 2.07 (age - 58) L; Q 36.3 L/h.  (The
#   abstract misprints Vss as 56 - 2.1 (age - 58) and Q as 10 L/h; the
#   discussion repeats Table 4.)  A central volume of 0.74 L, a quarter of the
#   plasma volume, cannot come from a bolus: the samples were drawn during
#   long infusions, which do not identify the early distribution.  A 2 mg
#   bolus would start at 2700 ng/mL.
#
# Barr 2001 (24 surgical ICU patients; Steven L. Shafer a co-author) fitted a
# two-compartment model whose full parameters were not retrievable here; Swart
# quotes its CL 6.4 L/h, Q 112 L/h and Vss 143 L.  It would be the natural
# replacement for the disposition below.
#
# DISPOSITION
# ===========
# Nielsen-Kudsk et al. gave 6 young healthy volunteers of either sex
# intravenous and intramuscular lorazepam and fitted each intravenous curve
# with a biexponential (NONLIN).  The abstract's means:
#
#     t1/2 alpha 0.31 h, t1/2 beta 14.10 h, central volume 0.59 L/kg,
#     clearance 62.17 mL/kg/h, Vd-area 1.24 L/kg
#
# The two-compartment micro-constants follow exactly from those four numbers
# (k10 = CL/V1, k21 = alpha beta / k10, k12 = alpha + beta - k10 - k21), and
# the code derives them rather than typing them in:
#
#     k10 0.10537, k12 1.13661, k21 1.04314 /h
#     V2 0.6429 L/kg, Q 0.6706 L/h/kg, Vss 1.233 L/kg
#
# so for 70 kg: V1 41.3 L, V2 45.0 L, CL 4.35 L/h, Q 46.9 L/h.  Building a
# model from mean hybrid parameters is an approximation (a mean of ratios is
# not the ratio of means); the check is that CL/beta gives 1.265 L/kg against
# the reported mean Vd-area of 1.24.  The clearance, 1.04 mL/min/kg, is in the
# middle of the published adult range: 0.99 in young adults (Greenblatt 1979),
# 1.21 (Greenblatt 1982), 1.1 +/- 0.4 (the injection label).
#
# EFFECT SITE
# ===========
# Greenblatt et al. 2000 gave 9 volunteers a 2 mg bolus with a 4 h infusion
# and related EEG beta (13-30 Hz) to an effect site: equilibration half-life
# 8.8 min, EC50 13.1 ng/mL, Emax 12.7% over baseline, no tolerance.  ke0 =
# ln 2 / 8.8 = 0.0788 /min.  Carried by the time to peak effect, as the
# library does: on the reference patient's bolus curve that ke0 puts the
# effect-site peak at LORAZEPAM_TPEAK = 26.3 min, which getDrugPK() turns back
# into ke0 for each patient.  The EEG effect was maximal 0.5 h after the
# loading dose (Greenblatt 2000).  An oral psychomotor tracking test gave a
# slower equilibration, half-time 0.43 h (Gupta 1990): the delay depends on
# the end point.
#
# ORAL AND INTRAMUSCULAR
# ======================
# Greenblatt 1982 gave 10 volunteers 2 mg intravenously, intramuscularly,
# orally and sublingually in a crossover: absorption half-life 14.2 min
# intramuscular, 32.5 min oral; absolute availability 95.9% and 99.8%, none
# different from 100%.  Used here: those absorption rates, no lag; F 0.96
# intramuscular, and 0.90 oral, the label's "absolute bioavailability of 90
# percent" (oral and IM absorption 80-100% complete in the elderly,
# Greenblatt 1979).
# Sublingual absorption (28.5 min, 94-98%) is close to oral; there is no
# sublingual route in the engine, so a sublingual dose is entered as mg PO.
# The intranasal route (F 0.78, Wermeling 2001) has no absorption rate and is
# not offered.
#
# Checks, reference patient (70 kg, 170 cm, 35 y man; size factors 1):
# 4 mg IV gives 74 ng/mL at 15 min (label: "an initial concentration of
# approximately 70 ng/mL"); 2 mg by mouth peaks at 19.7 ng/mL at 1.4 h (label:
# about 20 ng/mL at about 2 h; Greenblatt 1982's mean tmax 2.37 h); 4 mg IM
# peaks at 53 ng/mL at 38 min (label: about 48 ng/mL, within 3 h; Greenblatt
# 1982's mean tmax after 2 mg IM 1.15 h).  The typical curve peaks earlier
# than the mean of the individual peak times, as it usually does.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# The source reports volumes and clearance per kilogram, so the rate constants
# are fixed: with the switch off, volumes and clearances both scale with
# weight / 70.  With it on (the default), volumes scale with fat-free mass
# relative to the 70 kg, 170 cm reference male and clearances with that ratio
# ^ 0.75.
#
# BAND AND THRESHOLD
# ==================
# From Barr 2001, whose predicted steady-state plasma C50s for a Ramsay score
# of at least 2, 3, 4, 5 and 6 were 34, 51, 104, 152 and 188 ng/mL, in
# postoperative ICU patients receiving fentanyl or epidural morphine.  The band
# runs from 34 (P(Ramsay >= 2) = 0.5) to 104 (P(Ramsay >= 4) = 0.5), typical
# 51 (Ramsay >= 3, the lower edge of the moderate sedation Barr targeted).
# The time-until-threshold level (endCe in the CSV) is also 51: the
# concentration below which a typical patient is more likely than not to
# emerge from light sedation, the end point of Barr's emergence times.  For
# orientation: anticonvulsant and anxiolytic concentrations are about 20-30
# ng/mL, and the EEG and memory EC50s about 13 ng/mL.
#
# NOT MODELLED
# ============
# Age (clearance about 22% lower in the elderly, Greenblatt 1979), critical
# illness, PEEP and alcohol abuse (Swart 2004), hepatic and renal disease,
# the inactive glucuronide and its accumulation in renal failure, and
# propylene glycol from the injection vehicle.
#
# References
# ----------
# Nielsen-Kudsk F et al., Acta Pharmacol Toxicol 1983;52:121-127.
#   https://doi.org/10.1111/j.1600-0773.1983.tb03413.x
# Greenblatt DJ et al., Crit Care Med 2000;28:2750-2757.
#   https://doi.org/10.1097/00003246-200008000-00011
# Greenblatt DJ et al., J Pharm Sci 1982;71:248-252.
#   https://doi.org/10.1002/jps.2600710227
# Greenblatt DJ et al., Clin Pharmacol Ther 1979;26:103-113.
#   https://doi.org/10.1002/cpt1979261103
# Barr J et al., Anesthesiology 2001;95:286-298.
#   https://doi.org/10.1097/00000542-200108000-00007
# Swart EL et al., Br J Clin Pharmacol 2004;57:135-145.
#   https://doi.org/10.1046/j.1365-2125.2003.01957.x
# Gonzalez D et al., Clin Pharmacokinet 2017;56:941-951.
#   https://doi.org/10.1007/s40262-016-0486-0
# Gupta SK et al., J Pharmacokinet Biopharm 1990;18:89-102.
#   https://doi.org/10.1007/BF01063553
# Wermeling DP et al., J Clin Pharmacol 2001;41:1225-1231.
#   https://doi.org/10.1177/00912700122012779
# Ativan (lorazepam) injection and tablet prescribing information.
#
# Drafted with Claude Code at the request of Steven L. Shafer, 2026-10-09,
# from a ChatGPT specification whose references and values were checked
# against the sources first.
# -----------------------------------------------------------------------------

# Nielsen-Kudsk 1983, abstract means (intravenous, 6 volunteers)
LORAZEPAM_V1_PER_KG <- 0.59          # L/kg
LORAZEPAM_CL_PER_KG <- 62.17 / 1000  # L/h/kg
LORAZEPAM_T_ALPHA   <- 0.31          # h
LORAZEPAM_T_BETA    <- 14.10         # h

# Greenblatt 2000: effect-site equilibration half-life 8.8 min, carried as the
# time to peak effect it gives on the reference patient's bolus curve.
LORAZEPAM_TPEAK <- 26.3              # minutes after an IV bolus

#' Lorazepam pharmacokinetics
#'
#' Two-compartment intravenous kinetics derived from the means of
#' Nielsen-Kudsk et al. (1983), oral and intramuscular absorption from
#' Greenblatt et al. (1982), and the EEG effect site of Greenblatt et al.
#' (2000).  See the file's header.
#'
#' @inheritParams cefazolin
#' @param adjustToFFM \code{TRUE} (the default) scales volumes to fat-free mass
#'   and clearances to its 0.75 power; \code{FALSE} scales both with weight,
#'   as the per-kilogram source did.
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
lorazepam <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  # Size scaling (see the header): per-kilogram source, legacy weight / 70.
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)

  # Micro-constants from the hybrid means (per hour)
  alpha <- log(2) / LORAZEPAM_T_ALPHA
  beta  <- log(2) / LORAZEPAM_T_BETA
  k10   <- LORAZEPAM_CL_PER_KG / LORAZEPAM_V1_PER_KG
  k21   <- alpha * beta / k10
  k12   <- alpha + beta - k10 - k21

  # Reference 70 kg values, L and L/min
  v1Ref  <- LORAZEPAM_V1_PER_KG * 70                # 41.3 L
  v2Ref  <- v1Ref * k12 / k21                       # 45.0 L
  cl1Ref <- LORAZEPAM_CL_PER_KG * 70 / 60           # 4.35 L/h
  cl2Ref <- k12 * v1Ref / 60                        # 46.9 L/h

  default <- list(
    v1 = v1Ref * size$volume,
    v2 = v2Ref * size$volume,
    v3 = 1,                                         # two compartments
    cl1 = cl1Ref * size$clearance,
    cl2 = cl2Ref * size$clearance,
    cl3 = 0,
    # Greenblatt 1982: absorption half-lives 32.5 min oral, 14.2 min IM
    ka_PO = log(2) / 32.5,
    bioavailability_PO = 0.90,                      # label
    tlag_PO = 0,
    ka_IM = log(2) / 14.2,
    bioavailability_IM = 0.96,                      # Greenblatt 1982, 95.9%
    tlag_IM = 0
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  # Band, ng/mL: Barr 2001 C50s for Ramsay >= 2, >= 3 and >= 4.
  typical      <- 51
  upperTypical <- 104
  lowerTypical <- 34

  reference <- paste0(
    "Nielsen-Kudsk F et al., Acta Pharmacol Toxicol 1983;52:121-127 ",
    "(two-compartment intravenous kinetics); oral and intramuscular ",
    "absorption from Greenblatt DJ et al., J Pharm Sci 1982;71:248-252; ",
    "effect site from Greenblatt DJ et al., Crit Care Med 2000;28:2750-2757. ",
    "https://doi.org/10.1111/j.1600-0773.1983.tb03413.x"
  )

  return(
    list(
      PK = PK,
      tPeak = LORAZEPAM_TPEAK,
      MEAC = 0,
      typical = typical,
      upperTypical = upperTypical,
      lowerTypical = lowerTypical,
      reference = reference
    )
  )
}
