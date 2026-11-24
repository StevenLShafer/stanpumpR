# -----------------------------------------------------------------------------
# Lorazepam: two-compartment intravenous kinetics in surgical ICU patients
# (Barr 2001), with the oral and intramuscular routes of Greenblatt 1982 and
# the EEG effect site of Greenblatt 2000
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in ng/mL, total plasma.
#
# WHY BARR 2001 (Steven L. Shafer's decision, 2026-11-23)
# =======================================================
# Until 2026-11-23 the disposition was built from the abstract means of
# Nielsen-Kudsk 1983: six young healthy volunteers (4 men, 2 women, 22-37 y,
# 60-87 kg), each 10-min intravenous curve fitted with a biexponential
# (NONLIN), venous samples to 48 h, two compartments derived from the mean
# hybrid parameters (V1 0.59 L/kg, CL 62.17 mL/kg/h, t1/2 alpha 0.31 h,
# t1/2 beta 14.10 h; for 70 kg V1 41.3 L, V2 45.0 L, CL 4.35 L/h, Q
# 46.9 L/h).  A model built from mean ratios is an approximation, and six
# volunteers on one ~2 mg dose are a thin basis for the ICU infusions that
# are lorazepam's main intravenous use.  The nlmixr2lib audit of the library
# (docs/nlmixr2lib/audit.md, PR #851) flagged it, this header had already
# named Barr 2001 as "the natural replacement", and Shafer chose it
# ("Replace rocuronium and lorazepam PK as suggested", 2026-11-23).
#
# Clearance is the parameter to compare (AGENTS.md, *Comparing sources: only
# clearance is real*).  Barr's 0.107 L/min (6.4 L/h; 1.26 mL/min/kg at the
# cohort's 85 kg) agrees with Greenblatt 2000's 109 mL/min in nine young men
# (1.44 mL/min/kg); both it and Nielsen-Kudsk's 1.04 mL/min/kg lie within
# the label's 1.1 +/- 0.4.  For the reference man (below) the clearance
# rises from 4.35 to 5.85 L/h, 35%.  The volumes differ more, and as the rule
# says, that is weak evidence about the drug: Barr's larger V2 (102 L, Vss
# 143 L, against Nielsen-Kudsk's Vss of about 86 L for 70 kg) is what larger
# doses followed longer above the assay's limit would give, the reason
# Barr's Discussion offers for the same difference from Greenblatt's earlier
# model (31.7 mg against 2-4 mg); and Barr sampled arterial blood,
# Nielsen-Kudsk venous.
#
# Recorded alternatives, not used:
# - Nielsen-Kudsk 1983 (above): young volunteers, derived from means; the
#   previous disposition.
# - Swart 2004 (critically ill adults on infusions for 24-715 h) is verified
#   from the PDF.  Table 4: CL 4.13 - 0.417 (PEEP - 5) L/h, 0.74 L/h with
#   alcohol abuse; V 0.743 L; Vss 156 - 2.07 (age - 58) L; Q 36.3 L/h.  (The
#   abstract misprints Vss as 56 - 2.1 (age - 58) and Q as 10 L/h; the
#   discussion repeats Table 4.)  A central volume of 0.74 L, a quarter of
#   the plasma volume, cannot come from a bolus: the samples were drawn during
#   long infusions, which do not identify the early distribution.  A 2 mg
#   bolus would start at 2700 ng/mL.
# - Gonzalez 2017 (145 children, 0.3-17.8 y, status epilepticus) is verified
#   as quoted, but its age term is fitted on children only.
#
# DISPOSITION (Barr 2001, read on the PDF, 2026-11-23)
# ====================================================
# 24 adults (18-85 y) intubated and ventilated after elective major vascular,
# urologic or general surgery at the VA Palo Alto surgical ICU; 14 received
# lorazepam by target-controlled infusion (STANPUMP), titrated from a target
# of 50 ng/mL to a Ramsay score of 3-4 and held 12-72 h (infusion 36.9 +/-
# 30.9 h at 0.91 +/- 0.39 mg/h, total 31.7 mg, Table 3).  Arterial samples at
# 5-120 min after each change of target and to 24 h after the infusion
# stopped; HPLC, limit of quantitation 16 ng/mL.  NONMEM, two compartments,
# "the accuracy of the lorazepam model was not improved with the addition of
# any covariates" (Results).  Table 4, revised lorazepam model (n = 14),
# value (interindividual %CV), ABSOLUTE values in L and L/min:
#
#     V1 40.8 L (33%), V2 102 L (26%), Cl1 0.107 L/min (48%),
#     Cl2 1.86 L/min (CV not estimable); intraindividual variability 11%
#
# The derived half-lives, 10.5 and 952 min, reproduce the paper's elimination
# half-life of 952 min (Discussion); Vss 142.8 L.
#
# Cohort (Table 2, lorazepam group, n = 14, mean +/- SD): age 74 +/- 6 y,
# weight 85 +/- 15 kg, height 1.76 +/- 0.09 m, BSA 2.04 m2, BMI 27.07,
# 13 men and 1 woman, APACHE II 10.1.  All 14 entered the pharmacokinetic
# analysis (the four replaced subjects were excluded from the sedation
# analysis only).
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# Table 4's values are absolute with no size covariate, so they describe the
# cohort's typical patient, not the 70 kg reference man, and are anchored at
# the cohort (Steven L. Shafer, 2026-11-18, *Anchoring at the source's
# cohort*): with the switch on (the default) every volume is divided by the
# cohort's fat-free-mass factor and every clearance by that factor ^ 0.75,
# then scaled with the patient's fat-free mass relative to the 70 kg, 170 cm
# reference male (clearances to the 0.75 power).  The cohort's factors are
# the geometric mean over 13 men and 1 woman at 85 kg, 176 cm, 74 y
# (cohortSizeFactors()): volume 1.131261, clearance 1.096913.  So the
# reference man receives V1 36.07 L, V2 90.16 L, CL 0.09755 L/min (5.85
# L/h), Q 1.6957 L/min, and the cohort's typical patient Table 4's values.
# With the switch off, everyone receives Table 4's values unscaled.  (The
# previous, per-kilogram model scaled with weight / 70 with the switch off.)
#
# EFFECT SITE
# ===========
# Barr's sedation model relates the Ramsay score to the PLASMA concentration
# (Model H, no ke0), so the effect site is still Greenblatt 2000's: 9 young
# men given a 2 mg bolus with a 4 h infusion, EEG beta (13-30 Hz) related to
# an effect site with an equilibration half-life of 8.8 min (mean of the
# individual values), EC50 13.1 ng/mL, Emax 12.7% over baseline, no
# tolerance.  ke0 = ln 2 / 8.8 = 0.0788 /min.  It is carried, as the library
# carries every ke0, by the time to peak effect it gives on the reference
# patient's bolus curve: on Barr's disposition LORAZEPAM_TPEAK = 18.1 min,
# which getDrugPK() turns back into ke0 0.0788 /min for the reference man.
# On Nielsen-Kudsk's disposition the same ke0 gave 26.3 min, the value until
# 2026-11-23: the time to peak effect moved because Barr's distribution
# phase is faster (t1/2 10.5 min against 18.6 min), not because the effect
# site did.  On Greenblatt 2000's own mean disposition (alpha 7.5 /h, beta
# 0.053 /h, k21 2.92 /h, Table 1's means of the individual values, read on
# the PDF; a mean of ratios, so approximate) the same ke0 peaks at 19.4 min,
# so 18.1 is the closer of the two to the plasma curves the ke0 was
# estimated on.
# The EEG effect was maximal 0.5 h after the loading dose (Greenblatt 2000),
# with the infusion running.  An oral psychomotor tracking test gave a slower
# equilibration, half-time 0.43 h (Gupta 1990): the delay depends on the end
# point.
# Decided by Steven L. Shafer, 2026-11-23: "Keep the time of peak effect at
# 18 minutes unless data show otherwise."  The ke0 (8.8 min) is kept and the
# time to peak effect follows the new disposition (26.3 -> 18.1 min).  No
# data show otherwise: Barr 2001 has no effect site, Greenblatt 2000's EEG
# maximum at 0.5 h followed a bolus WITH an infusion running (which peaks
# later than a bolus alone), and Gupta 1990's 0.43 h is another end point
# after an oral dose.  The alternative, keeping 26.3 min, would on Barr's
# curve need an equilibration half-life of 15.1 min (ke0 0.0459 /min), a
# value no study measured.
#
# ORAL AND INTRAMUSCULAR
# ======================
# Unchanged by the new disposition.  Greenblatt 1982 gave 10 volunteers 2 mg
# intravenously, intramuscularly, orally and sublingually in a crossover:
# absorption half-life 14.2 min intramuscular, 32.5 min oral; absolute
# availability 95.9% and 99.8%, none different from 100%.  These are read
# from Greenblatt 1982, fitted to its own subjects' curves, and were never
# calibrated on Nielsen-Kudsk's disposition, so they stand (and F, a within-
# subject ratio, is independent of the disposition by construction).  Used
# here: those absorption rates, no lag; F 0.96 intramuscular, and 0.90 oral,
# the label's "absolute bioavailability of 90 percent" (oral and IM
# absorption 80-100% complete in the elderly, Greenblatt 1979).
# Sublingual absorption (28.5 min, 94-98%) is close to oral; there is no
# sublingual route in the engine, so a sublingual dose is entered as mg PO.
# The intranasal route (F 0.78, Wermeling 2001) has no absorption rate and is
# not offered.
#
# Checks, reference patient (70 kg, 170 cm, 35 y man; Barr's values over the
# cohort factors), against the label: 4 mg IV gives 58.9 ng/mL at 15 min
# (label: "an initial concentration of approximately 70 ng/mL"; Nielsen-
# Kudsk's model gave 73.7); 2 mg by mouth peaks at 13.8 ng/mL at 54 min
# (label: about 20 ng/mL at about 2 h; Greenblatt 1982's mean tmax 2.37 h; was
# 19.7 at 81 min); 4 mg IM peaks at 42.3 ng/mL at 25 min (label: about 48
# ng/mL, within 3 h; Greenblatt 1982's mean tmax after 2 mg IM 1.15 h; was
# 53.4 at 38 min).  The typical curve peaks earlier than the mean of the
# individual peak times, as it usually does.
# The oral peak is now about 30% under the label's figure, and the
# intravenous and intramuscular values about 15% under.  That is a finding
# about the two populations, not an error to absorb into F or ka (AGENTS.md,
# *Bioavailability is read, never fitted to another study's exposure*): the
# label's figures come from young healthy volunteers, and Barr's elderly
# postoperative ICU patients distribute the drug faster (t1/2 alpha 10.5 min)
# into a larger peripheral volume (102 L) and clear it faster.  In the first
# hour after an oral or intramuscular dose that distribution is what sets
# the peak.
# Decided by Steven L. Shafer, 2026-11-23: "use Barr's 2001 model for the
# disposition for all routes."  Barr's ICU disposition serves every route,
# including oral and intramuscular lorazepam given to patients who are not
# in an ICU, for whom Nielsen-Kudsk's volunteers (or a volunteer study such
# as Greenblatt 2000's) may be the closer population; a route-limited
# system (as sildenafilMiura runs its intravenous doses on the FDA model)
# was the alternative, not taken.
#
# BAND AND THRESHOLD
# ==================
# From Barr 2001, whose predicted steady-state plasma C50s for a Ramsay score
# of at least 2, 3, 4, 5 and 6 were 34, 51, 104, 152 and 188 ng/mL (Model H,
# for a 71-year-old without fentanyl; x 0.82 with fentanyl, 18% lower per
# decade of age), in postoperative ICU patients receiving fentanyl or
# epidural morphine.  The band is moderate sedation (Ramsay 3 or 4, as Barr
# defined it), the end point the library draws for the sedatives (Steven L.
# Shafer, 2026-10-27): from 51 (P(Ramsay >= 3) = 0.5) to 152 (P(Ramsay >= 5)
# = 0.5), the concentrations at which the median patient scores 3 or 4;
# typical 104 (P(Ramsay >= 4) = 0.5).  It was 34-104, typical 51 (Ramsay >=
# 2 to >= 4).  The time-until-threshold level (endCe in the CSV) is also 51:
# the concentration below which a typical patient is more likely than not to
# emerge from light sedation, the end point of Barr's emergence times.  The
# band and the disposition now come from the same patients.  For
# orientation: anticonvulsant and anxiolytic concentrations are about 20-30
# ng/mL, and the EEG and memory EC50s about 13 ng/mL.
#
# NOT MODELLED
# ============
# Age (Barr found no covariate in 14 patients aged 74 +/- 6; clearance about
# 22% lower in the elderly in Greenblatt 1979), critical illness beyond
# Barr's postoperative patients, PEEP and alcohol abuse (Swart 2004), hepatic
# and renal disease, the inactive glucuronide and its accumulation in renal
# failure, and propylene glycol from the injection vehicle.
#
# References
# ----------
# Barr J, Zomorodi K, Bertaccini EJ, Shafer SL, Geller E.  A double-blind,
#   randomized comparison of i.v. lorazepam versus midazolam for sedation of
#   ICU patients via a pharmacologic model.  Anesthesiology 2001;95:286-298.
#   https://doi.org/10.1097/00000542-200108000-00007
# Greenblatt DJ et al., Crit Care Med 2000;28:2750-2757.
#   https://doi.org/10.1097/00003246-200008000-00011
# Greenblatt DJ et al., J Pharm Sci 1982;71:248-252.
#   https://doi.org/10.1002/jps.2600710227
# Greenblatt DJ et al., Clin Pharmacol Ther 1979;26:103-113.
#   https://doi.org/10.1002/cpt1979261103
# Nielsen-Kudsk F et al., Acta Pharmacol Toxicol 1983;52:121-127.
#   https://doi.org/10.1111/j.1600-0773.1983.tb03413.x
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
# against the sources first; disposition replaced with Barr 2001,
# 2026-11-23.
# -----------------------------------------------------------------------------

# Barr 2001, Table 4, revised lorazepam model (n = 14): absolute values
LORAZEPAM_V1  <- 40.8    # L
LORAZEPAM_V2  <- 102     # L
LORAZEPAM_CL1 <- 0.107   # L/min
LORAZEPAM_CL2 <- 1.86    # L/min

# Barr 2001, Table 2, lorazepam group (n = 14): mean age, weight, height
LORAZEPAM_COHORT <- list(weight = 85, height = 176, age = 74, men = 13, women = 1)

# Greenblatt 2000: effect-site equilibration half-life 8.8 min, carried as the
# time to peak effect it gives on the reference patient's bolus curve.
LORAZEPAM_TPEAK <- 18.1              # minutes after an IV bolus

#' Lorazepam pharmacokinetics
#'
#' Two-compartment intravenous kinetics of Barr et al. (2001) in surgical ICU
#' patients, oral and intramuscular absorption from Greenblatt et al. (1982),
#' and the EEG effect site of Greenblatt et al. (2000).  See the file's
#' header.
#'
#' @inheritParams cefazolin
#' @param adjustToFFM \code{TRUE} (the default) scales volumes to fat-free mass
#'   and clearances to its 0.75 power, anchored at the source's cohort;
#'   \code{FALSE} gives every patient the published values.
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
lorazepam <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  # Size scaling (see the header): absolute source values, anchored at
  # Barr's cohort; published values with the switch off.
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume = 1, legacyClearance = 1,
                        cohort = do.call(cohortSizeFactors, LORAZEPAM_COHORT))

  default <- list(
    v1 = LORAZEPAM_V1 * size$volume,
    v2 = LORAZEPAM_V2 * size$volume,
    v3 = 1,                                         # two compartments
    cl1 = LORAZEPAM_CL1 * size$clearance,
    cl2 = LORAZEPAM_CL2 * size$clearance,
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

  # Band, ng/mL: moderate sedation, Barr 2001 C50s for Ramsay >= 3, >= 4
  # and >= 5.
  typical      <- 104
  upperTypical <- 152
  lowerTypical <- 51

  reference <- paste0(
    "Barr J et al., Anesthesiology 2001;95:286-298 ",
    "(two-compartment intravenous kinetics in surgical ICU patients, Table 4); ",
    "oral and intramuscular absorption from Greenblatt DJ et al., ",
    "J Pharm Sci 1982;71:248-252; effect site from Greenblatt DJ et al., ",
    "Crit Care Med 2000;28:2750-2757. ",
    "https://doi.org/10.1097/00000542-200108000-00007"
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
