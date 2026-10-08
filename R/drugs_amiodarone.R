# -----------------------------------------------------------------------------
# Amiodarone: long-term oral therapy, with desethylamiodarone as its active
# metabolite
# -----------------------------------------------------------------------------
# Provenance
# ----------
# Drafted by Claude Code, 2026-10-07, at the request of
# Steven L. Shafer, a co-author of the source.  Every number below was checked
# against the published article (Table II, Methods, Results and Discussion).
# Verified by tests/testthat/test-drugs-amiodarone.R, which derives its
# expectations independently of this file.
#
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in mcg/mL (= mg/L) of serum amiodarone.  Doses are the
# labelled mg of amiodarone hydrochloride tablets, as Pollak's patients took
# them.  The source reports clearances in L/day; they are divided by
# MINS_PER_DAY here.
#
# THE SOURCE
# ==========
# Pollak, Bouillon and Shafer 2000: 77 patients (50 men, 27 women) treated
# for ventricular and supraventricular arrhythmias in a Halifax, Nova Scotia
# clinic, 605 trough serum samples drawn fasting before the morning dose, each
# assayed for amiodarone and desethylamiodarone by HPLC (limit of
# quantitation 0.1 mg/L), over a mean of 24 months of therapy (0.2 to 92).
# NONMEM, first-order conditional estimation with eta-epsilon interaction.  A
# two-compartment model improved the objective function by 164 points over
# one compartment; three compartments did not improve it.
#
# Table II, apparent parameters (see below), with interindividual CV:
#
#     V1/F    882 L        not estimable (%SE 32.4)
#     V2/F    12,700 L     58.4%
#     CL1/F   229 L/day    31.2%
#     CL2/F   588 L/day    56.2%
#
# The abstract prints CL2/F as 599 L/day; that is a misprint (Steven L. Shafer,
# a co-author, 2026-10-08), and Table II's 588 is the estimate.  The
# difference is small: the half-lives are 17.33 h and 55.36 days with 588,
# 17.09 h and 55.09 days with 599, and the paper reports 17 h and 55 days.
#
# APPARENT ORAL PARAMETERS, AND WHY THERE IS NO INTRAVENOUS UNIT
# ==============================================================
# No patient received intravenous amiodarone, so every parameter is divided
# by the unmeasured bioavailability F.  Apparent parameters predict oral
# concentrations correctly, because F cancels, and intravenous ones wrong by
# 1/F (docs/adding-a-drug.md, "Apparent parameters restrict the route").
# There is a second reason.  The paper's own limitations section says that
# the paucity of data in the first month (11 observations in the first week,
# 64 in the first month) may account for the large V1 and its high standard
# error, "making the model ill-suited to predict what happens after single
# doses or short-term intravenous infusions".  Acute intravenous amiodarone
# has its own population kinetics (Korth-Bradley 1996, Pollak's reference
# 37), whose clearance, about 0.22 L/h/kg, is larger than 229 L/day x F for
# any F up to 1, so no choice of F reconciles the two.  Intravenous
# amiodarone, if it is added, is a separate drug built on an intravenous
# model.
#
# ORAL INPUT AS A CONSTANT DAILY RATE
# ===================================
# Pollak modelled a daily oral dose as a constant-rate input over 24 hours
# (400 mg/day as 16.7 mg/h), for three stated reasons: no patient was sampled
# more than once within a dosing interval, so no absorption rate constant
# could be estimated; the elimination half-life exceeds the dosing interval
# 60-fold; and the daily dose is small against the amount in the body, so the
# concentration should vary by less than 10% within a dosing interval.
#
# The unit "mg/day PO" (poRateUnits, R/constants.R) is that input.  simCpCe()
# runs it as a rate on the apparent parameters, with no absorption rate
# constant and no bioavailability, both being inside the apparent scale; the
# model therefore carries no ka_PO.  Each row sets the running daily rate
# from its time, and 0 mg/day PO stops it.  No first-order "mg PO" unit is
# offered, because Pollak identified no ka: any value would be an assumption,
# and a first-order input on these parameters would put the troughs, which
# are what was fitted, below the published fit.  No salt or molecular-weight
# conversion is applied: the doses Pollak fitted were tablet mg, and F
# absorbs the difference.
#
# DESETHYLAMIODARONE
# ==================
# The input to Pollak's metabolite model was "the quantity of amiodarone
# permanently cleared from the serum (presumably by metabolism to
# desethylamiodarone)", from each patient's post hoc parent parameters.  That
# is a formation flux of CL1/F x Cp on a mass basis: every milligram of
# amiodarone eliminated becomes a milligram of desethylamiodarone input.  In
# the library's terms, kFormation = cl1 / v1 (k10), firstPassFraction = 0 and
# mwRatio = 1.  The engine does not subtract formation from the parent,
# whose fitted clearance already contains it, and here that is exactly the
# published structure (Pollak's Fig 2): all of the parent's elimination
# feeds the metabolite.  kFormation is computed from the SCALED cl1 and v1
# so that it remains the parent's own k10 under size scaling.  The molar
# ratio (617.3 / 645.3) and the hydrochloride salt factor are deliberately
# NOT applied: the metabolite's parameters were fitted on this mass basis,
# and are apparent on it (R/drugs_desethylamiodarone.R).
#
# AN ACTIVE PARENT WITH NO EFFECT SITE, NOT A PRODRUG
# ===================================================
# Amiodarone is itself active, and desethylamiodarone appears to be of equal
# potency (Nattel and Talajic 1988, Pollak's reference 40).  Neither has an
# effect-site model: no human ke0 for the antiarrhythmic effect has been
# published, and Pollak's discussion notes that serum reflects the site of
# action only at steady state.  tPeak is therefore zero, ke0 is zero, and
# the plot shows serum concentrations, which is what the therapeutic window
# refers to.  prodrug = FALSE tells the help page (R/help-drugs.R) that this
# is an active parent that merely lacks an effect-site model, not a prodrug
# like codeine whose effect is entirely the metabolite's.
#
# THERAPEUTIC WINDOW AND THE TIME UNTIL THRESHOLD
# ===============================================
# The band in inst/extdata/drugDefaults_global.csv is the 1.0 to 2.5 mg/L
# window of the product monograph, which Pollak's Fig 1 draws over the
# PARENT concentrations, with the paper's target of 1.5 mg/L as the typical
# value.  The window is for amiodarone only; no range has been established
# for desethylamiodarone.  endCe is 1.0 mg/L, so the time until threshold is
# the time for serum amiodarone to fall below the window if dosing stopped:
# Pollak's context-sensitive decrement put in clinical terms.  It is timed on
# the plasma, as for every drug without an effect site.  MEAC is zero:
# amiodarone is not an opioid.
#
# COVARIATES AND BODY SIZE (docs/weight-adjustment.md)
# ====================================================
# None.  Age, sex, height, weight and lean body mass were tested against
# every volume and clearance and none met the inclusion criterion (P < .01).
# Pollak's parameters are therefore fixed published values, and they take
# the library's default scaling for that class: volumes x the fat-free-mass
# ratio, clearances x that ratio ^ 0.75, and with the switch off the
# published values unscaled (legacy factors 1), which reproduces Pollak
# exactly whatever the patient's size.
#
# No re-anchoring is needed.  The cohort's fat-free mass at each sex's mean
# weight, height and age (men 81.4 kg, 173 cm, 61.3 y: 60.1 kg; women
# 71.7 kg, 158 cm, 69.0 y: 42.4 kg; weighted 50:27) is 53.9 kg, about 1%
# below the 54.5 kg reference man the library anchors to, so the published
# values stand as his.  Desethylamiodarone makes the identical call: its apparent
# scale is only consistent with the parent's if both scale together.  The
# cohort was adults of 22 to 86 years and 39 to 133 kg; the help page's
# child and infant are extrapolation, evaluated only because every model in
# the library is tabulated at them.
#
# References
# ----------
# Pollak PT, Bouillon T, Shafer SL. Population pharmacokinetics of long-term
#   oral amiodarone therapy. Clin Pharmacol Ther 2000;67:642-652.
#   https://doi.org/10.1067/mcp.2000.107047
# Nattel S, Talajic M. Recent advances in understanding the pharmacology of
#   amiodarone. Drugs 1988;36:121-131.
# Korth-Bradley JM, Rose GM, de Vane PJ, Peters J, Chiang ST. Population
#   pharmacokinetics of intravenous amiodarone in patients with refractory
#   ventricular tachycardia/fibrillation. J Clin Pharmacol 1996;36:715-719.
# -----------------------------------------------------------------------------

# Pollak 2000, Table II.  Apparent parameters (divided by F), litres and
# litres per day, converted to per minute in the model.
AMIODARONE_V1  <- 882     # L, V1/F
AMIODARONE_V2  <- 12700   # L, V2/F
AMIODARONE_CL1 <- 229     # L/day, CL1/F
AMIODARONE_CL2 <- 588     # L/day, CL2/F (Table II; the abstract's 599 is a misprint)

# The citation both members of the pair return.  One string, so that the
# bibliography lists one item for both drugs (test-help-drugs.R counts each
# citation exactly once).
AMIODARONE_REFERENCE <- paste0(
  "Pollak PT, Bouillon T, Shafer SL. Population pharmacokinetics of long-term ",
  "oral amiodarone therapy. Clin Pharmacol Ther 2000;67:642-652. ",
  "https://pubmed.ncbi.nlm.nih.gov/10872646/"
)

#' Amiodarone pharmacokinetics (long-term oral therapy), forming
#' desethylamiodarone
#'
#' Apparent oral parameters of Pollak, Bouillon and Shafer (2000), given as a
#' constant daily rate ("mg/day PO").  See the file header.
#'
#' @param weight weight in kg
#' @param height height in cm
#' @param age age in years
#' @param sex sex as a string
#' @param adjustToFFM scale volumes to the patient's fat-free mass and
#'   clearances to that ratio to the 0.75 power; when \code{FALSE}, use the
#'   published fixed parameters unscaled.
#' @returns a list in the shape \code{getDrugPK()} expects, naming
#'   desethylamiodarone as the active metabolite
#' @noRd
amiodarone <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  # Fixed published parameters (see the header): legacy factors 1.
  # Desethylamiodarone must make this call identically.
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1)

  v1  <- AMIODARONE_V1 * size$volume
  v2  <- AMIODARONE_V2 * size$volume
  v3  <- 1                                                # no third compartment
  cl1 <- AMIODARONE_CL1 / MINS_PER_DAY * size$clearance   # L/min
  cl2 <- AMIODARONE_CL2 / MINS_PER_DAY * size$clearance   # L/min
  cl3 <- 0

  # No ka_PO, bioavailability_PO or tlag_PO: the drug is given only as a
  # constant daily rate on the apparent parameters.  See the header.
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

  # Band, serum amiodarone mg/L: the product monograph's therapeutic window
  # and Pollak's target.  The plot reads the CSV; these mirror it.
  typical      <- 1.5
  upperTypical <- 2.5
  lowerTypical <- 1.0

  return(
    list(
      PK = PK,
      tPeak = 0,        # no effect-site model; see the header
      MEAC = 0,         # not an opioid
      typical = typical,
      upperTypical = upperTypical,
      lowerTypical = lowerTypical,
      reference = AMIODARONE_REFERENCE,
      # An active parent without an effect site, not a prodrug: read by the
      # help page only.
      prodrug = FALSE,
      metabolite = list(
        name              = "desethylamiodarone",
        # Formation flux CL1/F x Cp, mass basis: the parent's own k10
        kFormation        = cl1 / v1,
        firstPassFraction = 0,
        mwRatio           = 1
      )
    )
  )
}
