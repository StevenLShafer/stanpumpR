# -----------------------------------------------------------------------------
# Norpethidine (normeperidine): pethidine's toxic metabolite
# -----------------------------------------------------------------------------
# Provenance
# ----------
# Drafted by Claude Code, 2026-11-13, at the request of Steven L. Shafer.  No
# population or compartmental model of norpethidine formed from pethidine has
# been published (PubMed searched 2026-11-12/13: pethidine OR meperidine AND
# norpethidine OR normeperidine, with model, NONMEM, population, formation
# clearance), and norpethidine has never been given to people, so its own
# volume and clearance have never been measured.  Shafer decided (2026-11-13)
# that the model be BUILT from the descriptive studies, and that the
# documentation say how.  Verified by tests/testthat/test-drugs-norpethidine.R.
#
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in mcg/mL (= mg/L) of plasma norpethidine.
#
# WHAT CAN BE KNOWN, AND WHAT CANNOT
# ==================================
# When a metabolite is seen only after the parent is given, its concentration
# fixes two things: its elimination rate (the slope of its own decline) and
# the ratio of the amount formed to its volume.  The fraction of pethidine
# converted (fm) and the volume cannot be separated.  So, as Foong 2025 did
# for lidocaine's MEGX (R/drugs_MEGX.R), the model forms norpethidine from ALL
# of pethidine's clearance (fm = 1) and carries an APPARENT volume, V / fm.
# The formed concentrations are the same whatever the true split.  It follows
# that norpethidine has no dosing unit: it appears only when pethidine is
# given.
#
# THE STRUCTURE
# =============
# One compartment, formed directly from pethidine's central compartment (no
# transit): nothing published resolves a distribution phase or a formation
# delay.  Kuhnert 1979 (read as an abstract) found norpethidine in maternal
# plasma within 10 minutes of an intravenous dose, rising quickly for 20
# minutes and then slowly, which is what a one-compartment metabolite formed
# by a three-compartment parent gives.
#
# STEP 1: THE ELIMINATION RATE.  Kuhnert BR, Philipson EH, Kuhnert PM,
# Syracuse CD.  Disposition of meperidine and normeperidine following
# multiple doses during labor.  I. Mother.  Am J Obstet Gynecol
# 1985;151:406-409 (read in full): the half-life of norpethidine, from
# postpartum samples every 24 h for 72 h in four mothers, 20.55 +/- 3.61 h
# (range 17.3 to 24.8).  It is the only measured half-life of norpethidine
# in the archive, and the "about 20 hours" the reviews quote.
#
# STEP 2: THE APPARENT VOLUME.  The same paper's Table II: at delivery, in
# eleven mothers given a mean of 79.04 mg of pethidine intravenously in a
# mean of 2.58 doses (Table I), the first a mean of 303 min and the last 150
# min before delivery, plasma norpethidine was 33.6 +/- 19.9 ng/mL (and
# pethidine 129.2 +/- 57.5).  The library's own pethidine (Bjorkman 2003, 70
# kg, the switch off) was given that mean regimen -- 30.6 mg at 0, 17.8 mg at
# 76.5 min and 30.6 mg at 153 min -- and formed norpethidine from all of its
# clearance by moles (norpethidine C14H19NO2, 233.31 g/mol, over pethidine
# C15H21NO2, 247.33 g/mol), eliminated at the 20.55 h half-life.  At 303 min
# that is 36.78 mg of norpethidine in the body, so the volume that gives 33.6
# ng/mL is 36.78 / 0.0336 = 1095 L.  How the 2.58 doses are spread hardly
# matters: two doses of 39.5 mg at 0 and 153 min give 1089 L, three of 26.3
# mg give 1097 L.  The parent checks: the library's pethidine at delivery is
# 124 ng/mL against Kuhnert's 129.2, so the formation it drives is the
# formation Kuhnert's mothers had.  The apparent clearance is then 1095 x
# ln 2 / 20.55 h = 36.9 L/h (0.615 L/min).
#
# The calibration is against the library's pethidine, not a pethidine fitted
# to Kuhnert's mothers, deliberately: the volume is the number that makes
# THIS parent model form the norpethidine those mothers had.  Kuhnert's SD
# (19.9 of 33.6 ng/mL, CV 59%) is the uncertainty the volume inherits.
#
# STEP 3: AN UNFITTED CHECK, AND IT DISAGREES.  Austin KL, Stapleton JV,
# Mather LE.  Rate of formation of norpethidine from pethidine.  Br J Anaesth
# 1981;53:255-258 (abstract only; the full text was not available): after
# abdominal surgery, pethidine at an average 25 mg/h by intramuscular
# injection or intravenous infusion for two days, blood norpethidine "rose at
# 0.013 micrograms/mL/h" in both groups, steadily over the two days.  This
# model, for 25 mg/h intravenously, gives plasma norpethidine of 0.29 mcg/mL
# at 24 h and 0.48 at 48 h, or in blood (plasma about 1.25 times blood,
# Verbeeck 1981, abstract) 0.23 and 0.39 mcg/mL, where a steady 0.013/h would
# be 0.31 and 0.62.  So the model reaches about 60% of Austin's 48-hour
# figure: norpethidine may accumulate faster in postoperative patients than
# in Kuhnert's young parturients, whose filtration (a route of
# norpethidine's elimination, whose renal clearance follows creatinine
# clearance, Odar-Cederlof 1985, abstract) is raised by pregnancy.  Fitting the volume to Austin instead would give about 680 L.
# The model keeps Kuhnert's full-text calibration and the help page states
# the disagreement, the direction (this model may UNDERPREDICT accumulation
# over days) and the size.
#
# WHAT IT LEAVES OUT
# ==================
# Renal function.  Norpethidine is cleared in part by the kidney (its renal
# clearance correlates with creatinine clearance and is lower in old
# patients, Odar-Cederlof 1985; it is four times pethidine's, Pond 1981 --
# both read as abstracts), and the reviews describe it accumulating in renal
# failure; Kaiko 1983 (abstract) found compromised renal function in only 14
# of the 48 patients whose CNS excitation followed its accumulation.  No published study
# gives a relation of norpethidine clearance to creatinine clearance that
# could be applied, so the model has no renal term: in renal impairment it
# underpredicts, possibly greatly.
#
# Size: no study of norpethidine has a size covariate.  It scales exactly as
# pethidine does (fat-free mass, legacy factors 1), so that the pair's
# apparent scale stays consistent, as desethylamiodarone does with
# amiodarone.
#
# NO EFFECT SITE, NO BAND
# =======================
# Norpethidine is a convulsant with about half pethidine's analgesic potency
# (the reviews; not modelled).  No equilibration delay has been published,
# so the row plots plasma.  No threshold for seizures has been established:
# Kaiko 1983 correlated CNS excitation with norpethidine accumulation but
# gave no single concentration.  No band.  The row's label is drawn in red
# (TOXIC_METABOLITES in R/constants.R).  Its contribution to analgesia is
# not added to the MEAC: it is not an opioid row (MEAC 0).
#
# References
# ----------
# Kuhnert BR, Philipson EH, Kuhnert PM, Syracuse CD. Am J Obstet Gynecol
#   1985;151:406-409. https://doi.org/10.1016/0002-9378(85)90313-8
# Austin KL, Stapleton JV, Mather LE. Br J Anaesth 1981;53:255-258.
#   https://doi.org/10.1093/bja/53.3.255
# Verbeeck RK, Branch RA, Wilkinson GR. Clin Pharmacol Ther 1981;30:619-628.
#   https://doi.org/10.1038/clpt.1981.213
# Odar-Cederlof I, Boreus LO, Bondesson U, Holmberg L, Heyner L. Eur J Clin
#   Pharmacol 1985;28:171-175. https://doi.org/10.1007/BF00609687
# Pond SM, Kretschzmar KM. Clin Pharmacol Ther 1981;30:680-686.
#   https://doi.org/10.1038/clpt.1981.220
# Kaiko RF, Foley KM, Grabinski PY, et al. Ann Neurol 1983;13:180-185.
#   https://doi.org/10.1002/ana.410130213
# -----------------------------------------------------------------------------

# Kuhnert 1985: half-life 20.55 h; apparent volume calibrated to its Table II
# (see the header; the test reproduces the calibration).
NORPETHIDINE_HALF_LIFE <- 20.55   # h
NORPETHIDINE_V         <- 1095    # L, apparent (V / fm) at 70 kg
# Molar masses for the pethidine-to-norpethidine link, g/mol.
PETHIDINE_MW    <- 247.33
NORPETHIDINE_MW <- 233.31

NORPETHIDINE_REFERENCE <- paste0(
  "Derived (no published model): half-life and calibration from Kuhnert BR, ",
  "Philipson EH, Kuhnert PM, Syracuse CD, Am J Obstet Gynecol 1985;151:406-409. ",
  "https://doi.org/10.1016/0002-9378(85)90313-8"
)

#' Norpethidine pharmacokinetics
#'
#' Pethidine's toxic metabolite, derived from descriptive studies (see the
#' file header).  Has no dosing unit of its own.
#'
#' @inheritParams pethidine
#' @returns a list in the shape \code{getDrugPK()} expects
#' @noRd
norpethidine <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  # Identical to pethidine's scaling; see the header.
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)

  v1  <- NORPETHIDINE_V * size$volume
  v2  <- 1                                       # no second compartment
  v3  <- 1
  cl1 <- NORPETHIDINE_V * log(2) / (NORPETHIDINE_HALF_LIFE * 60) * size$clearance
  cl2 <- 0
  cl3 <- 0

  default <- list(
    v1 = v1,
    v2 = v2,
    v3 = v3,
    cl1 = cl1,
    cl2 = cl2,
    cl3 = cl3
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  list(
    PK = PK,
    tPeak = 0,           # no effect-site model; see the header
    MEAC = 0,            # not counted as an opioid
    typical = 0,         # no established threshold: no band
    upperTypical = 0,
    lowerTypical = 0,
    reference = NORPETHIDINE_REFERENCE
  )
}
