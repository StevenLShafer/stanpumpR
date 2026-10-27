# -----------------------------------------------------------------------------
# Desmetramadol (O-desmethyltramadol, M1): tramadol's active metabolite
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min.
#
# This is the species that carries tramadol's mu-opioid activity.  Tramadol's
# own analgesia is largely monoaminergic, serotonin and noradrenaline reuptake
# inhibition, which this package does not model; the parent is therefore
# treated as a prodrug and the opioid effect appears here.
#
# DISPOSITION
# ===========
# Allegaert 2015 (Clin Pharmacokinet 54:167-178, Table 3), which fitted
# parent and metabolite jointly to intravenous tramadol in 295 people from
# 25 weeks postmenstrual age to 84.8 years.  Two compartments: clearance
# 71.4 L/h, central volume 78.9 L (fixed from dogs), peripheral volume
# 133 L, intercompartmental clearance 562 L/h at 70 kg.  Clearances scale as
# (W/70)^0.75 and volumes as W/70, which is the source's own allometry.  The
# clearance matures with postmenstrual age as glomerular filtration does
# (TM50 47.7 weeks, Hill 3.4, fixed by the authors, who found M1's
# elimination indistinguishable from it): 0.36 of the adult value at term,
# 0.90 at one year (tramadolMaturation() in R/drugs_tramadol.R).  Until
# 2026-10-27 this was Holford 2014's adult fit (84.2 L/h, 131 L, 274 L/h,
# no maturation); see R/drugs_tramadol.R for why it changed.
#
# WHY THIS DRUG CANNOT BE GIVEN DIRECTLY HERE
# ===========================================
# Desmetramadol is a real drug and has been given to humans on its own
# (Zebala 2019), but this parameter set cannot predict what happens when it
# is.  Neither Allegaert nor Holford before it had human intravenous
# metabolite data; both fixed the human M1 central volume using direct
# intravenous measurements in dogs.
#
# The consequence is specific rather than general.  Multiplying the
# metabolite's clearances, its volumes AND the formation clearance feeding it
# by any common factor leaves FORMED metabolite concentrations completely
# unchanged, so the concentrations this package plots after a tramadol dose
# are identified even though the scale is not.  A directly administered dose
# is different: its concentration is dose over central volume, which moves
# with that same unidentified factor.
#
# So desmetramadol is offered as no dosing unit at all.  It appears only when
# tramadol is given.  Resolving this needs human intravenous metabolite data.
#
# NOT MODELLED
# ============
# M2 and the downstream metabolites, and the (+) and (-) enantiomers, which
# are formed and eliminated stereoselectively.  The model carries total
# racemic M1.  That matters for anyone trying to use the published analgesia
# benchmark; see the note on potency below.
#
# References
# ----------
# Allegaert K et al., Clin Pharmacokinet 2015;54:167-178.
#   https://doi.org/10.1007/s40262-014-0191-9
# Holford S et al., J Pharmacol Clin Toxicol 2014;2(1):1023.
#   https://www.jscimedcentral.com/public/assets/articles/pharmacology-spidpharmacokinetics-1023.pdf
# Zebala JA et al., J Pain 2019;20:1218-1235.
#   https://doi.org/10.1016/j.jpain.2019.04.005
# Chun D et al., CPT Pharmacometrics Syst Pharmacol 2025;14:781-795.
#   https://doi.org/10.1002/psp4.13315
# Gillen C et al., Naunyn Schmiedebergs Arch Pharmacol 2000;362:116-121.
#   https://doi.org/10.1007/s002100000266
# -----------------------------------------------------------------------------


# -----------------------------------------------------------------------------
# TO BE SUPPLIED
# -----------------------------------------------------------------------------
# Provisional values are set below (DESMETRAMADOL_KE0, DESMETRAMADOL_MEAC),
# so desmetramadol has an effect site and counts toward the opioid MEAC
# total; both still need a primary source.
#
# There IS a published human analgesia model, and it still cannot be used
# here unchanged.  Chun 2025 fitted chronic neuropathic pain against UNBOUND
# (+)-(1R,2R)-M1, reporting IC50 2.36 nmol/L and an equilibration rate of
# 0.0398 per hour.  Three things block it:
#
#   - The driver is the unbound (+) enantiomer.  This model carries total
#     racemic M1.  Converting needs a free fraction and an enantiomer ratio,
#     and formation and elimination are both stereoselective, so the ratio is
#     not one half.
#   - The equilibration rate implies a 17.4 h half-time, which belongs to a
#     delayed chronic-pain response and is not an acute analgesic onset.
#   - Its own bootstrap interval for IC50 runs from 0.03 to 22.07 nmol/L, a
#     factor of 700, and the paper's prose and final table disagree (2.91
#     against 2.36).
#
# Changing DESMETRAMADOL_MEAC also requires updating the MEAC column of
# inst/extdata/drugDefaults_global.csv; test-drugs-tramadol.R checks the two
# agree.
#
# EQUILIBRATION: ke0 SUPPLIED DIRECTLY, AND NEEDS A LITERATURE REFERENCE
# ----------------------------------------------------------------------
# Steven L. Shafer, 2026-10-06: peak analgesia is at 2.5 h after an ORAL
# dose of tramadol.  No citation is attached to that yet.
#
# Why ke0 is given here instead of a tPeak.  getDrugPK solves ke0 so the
# effect site peaks at tPeak, against whichever plasma curve the observation
# followed: a bolus by default, or the drug's own oral curve with
# tPeakRoute = ROUTE_PO.  Neither applies.  Desmetramadol is never dosed, so
# it has no curve of its own; the 2.5 h is measured against the metabolite
# profile formed from an ORAL PARENT dose, which carries tramadol's
# absorption delay and then the metabolic delay on top of it.  getDrugPK
# cannot build that curve at the point it resolves this drug, because the
# metabolite's own PK is needed before the parent's coefficients exist.  So
# the solve is done once, here, and the answer recorded.
#
#     ke0 = 0.0708247343 /min, an equilibration half-time of 9.8 min,
#     giving an effect-site peak at 150.0 min after 100 mg oral tramadol
#     in a normal metaboliser (70 kg, 171 cm, 50-year-old man).
#
# Re-solved 2026-10-27 when the disposition moved from Holford 2014 to
# Allegaert 2015: the formed metabolite now peaks at 134 min rather than
# 93 (Allegaert's M1 clearance is 71.4 against Holford's 84.2 L/h, and the
# re-fitted absorption is slower), so the same 150 min needs a faster
# equilibration than Holford's 24.1 min half-time.
#
# What it depends on, and when it stops being right.  The driving curve is
# the metabolite profile, so this number is only valid for tramadol's
# current absorption constant, formation clearance and first-pass fraction.
# Change any of those and it must be re-solved; test-drugs-tramadol.R
# asserts the 150 min peak directly, so it will fail rather than drift.
#
# It also moves with CYP2D6 phenotype and with body weight, because both
# reshape the metabolite curve.  The solve is at 70 kg, normal metaboliser.
#
# Note this was reachable only after first-pass formation was added to
# tramadol.  With systemic formation alone the metabolite peaks at 4.6 h
# (4 h on Holford's disposition), and
# an effect site cannot peak before the curve driving it, so 2.5 h was not
# merely unfitted but impossible.  See the first-pass note in
# R/drugs_tramadol.R.
DESMETRAMADOL_KE0 <- 0.0708247343  # 1/min; solved, see above

# Recorded for documentation; the engine does not use it, because the solve
# above is against a curve tPeak cannot name.
DESMETRAMADOL_TPEAK_ORAL_PARENT <- 150  # minutes after an oral tramadol dose

# MEAC: A MIXED-EXPOSURE OBSERVATION, NOT A MEASURED M1 MEAC
# ----------------------------------------------------------
# 84 ng/mL.  Lee 2019's introduction states that "the minimum effective
# concentration of M1 is 84 ug/L", citing earlier work.  The number matches
# Grond 1999 (Br J Clin Pharmacol 48:254-257, PMID 10417506, abstract read
# 2026-10-27): in postoperative patient-controlled analgesia with RACEMIC
# tramadol, the mean serum concentration of total M1 sampled immediately
# before each demand was 84 +/- 34 ng/mL, while tramadol itself was at
# 590 +/- 410 ng/mL.  So it is a pre-demand (minimum effective concentration
# type) mean taken during combined parent and metabolite exposure, of total
# racemic M1.  It is NOT an isolated M1 minimum effective analgesic
# concentration: some of the analgesia at that moment was the parent's, and
# the mu activity is the (+) enantiomer's (Grond's group given (+)-tramadol,
# the more potent enantiomer, demanded at a lower M1, 57 ng/mL).  Until
# 2026-10-27 this note
# and the help called it the right quantity for the column; the opioid
# potency audit of that date (docs/opioid-review-2026-10-27.md) corrected
# that.  The full text of Grond 1999 was not reached.
#
# It stays as the divisor, as a working value, because nothing better
# exists: no study has measured analgesia against M1 alone.  At this value a
# 100 mg oral dose of the hydrochloride in a normal metaboliser peaks at
# 44% of it in the effect site (37 ng/mL; computed 2026-10-27 on Allegaert's
# disposition), which is consistent with tramadol being a weak opioid.  The band (20 to 80 ng/mL) is a display range of formed M1, not an
# analgesic range.
DESMETRAMADOL_MEAC  <- 84  # ng/mL; provisional, see above
# -----------------------------------------------------------------------------


#' Desmetramadol (O-desmethyltramadol) pharmacokinetics
#'
#' Tramadol's active metabolite, and the species carrying its mu-opioid
#' effect.  Has no dosing unit of its own: see the file header for why a
#' directly administered dose is not identified by this parameter set.
#'
#' @param weight weight in kg
#' @param height height in cm (fat-free mass only)
#' @param age age in years: the maturation of the clearance, and fat-free
#'   mass
#' @param sex sex as a string (fat-free mass only)
#' @param adjustToFFM scale volumes to the patient's fat-free mass and
#'   clearances to that ratio to the 0.75 power; when \code{FALSE}, use the
#'   published fixed parameters unscaled.
#'
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
desmetramadol <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  # Holford 2014 allometry: clearances (W/70)^0.75, volumes W/70.  With the
  # fat-free-mass switch on (docs/weight-adjustment.md) the same exponents
  # apply to the fat-free-mass ratio instead.  Tramadol and desmetramadol
  # must make this call identically: the formation constant is a clearance
  # over a volume, and the pair was fitted jointly.
  size    <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                           legacyClearance = (weight / 70)^0.75)
  perKg   <- size$volume
  perKg75 <- size$clearance

  # Allegaert 2015 Table 3; clearance matures as glomerular filtration does.
  v1  <- 78.9 * perKg
  v2  <- 133  * perKg
  v3  <- 1                       # unused third compartment
  cl1 <- 71.4 * perKg75 * tramadolMaturation(age)[["m1"]] / 60   # L/min
  cl2 <- 562  * perKg75 / 60     # L/min
  cl3 <- 0

  reference <- paste0(
    "Allegaert K et al., Clin Pharmacokinet 2015;54:167-178. ",
    "https://pubmed.ncbi.nlm.nih.gov/25258277/ ",
    "Joint intravenous tramadol and O-desmethyltramadol model with ",
    "maturation of each clearance; the ",
    "metabolite central volume was fixed using dog data, so formed ",
    "concentrations are identified but the absolute scale is not."
  )

  # Display band, ng/mL: desmetramadol runs tens of ng/mL after ordinary
  # tramadol doses, well below the parent.
  typical      <- 40
  upperTypical <- 80
  lowerTypical <- 20

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

  return(
    list(
      PK = PK,
      # No tPeak: ke0 is supplied directly.  See the header.
      tPeak = 0,
      ke0 = DESMETRAMADOL_KE0,
      MEAC = DESMETRAMADOL_MEAC,
      typical = typical,
      upperTypical = upperTypical,
      lowerTypical = lowerTypical,
      reference = reference
    )
  )
}
