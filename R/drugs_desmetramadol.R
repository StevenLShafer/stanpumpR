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
# Holford 2014, which fitted parent and metabolite jointly to intravenous
# tramadol in 57 healthy and 56 postoperative adults.  Two compartments:
# clearance 84.2 L/h, central volume 78.9 L, peripheral volume 131 L,
# intercompartmental clearance 274 L/h at 70 kg.  Clearances scale as
# (W/70)^0.75 and volumes as W/70, which is the source's own allometry.
#
# WHY THIS DRUG CANNOT BE GIVEN DIRECTLY HERE
# ===========================================
# Desmetramadol is a real drug and has been given to humans on its own
# (Zebala 2019), but this parameter set cannot predict what happens when it
# is.  Holford had no human intravenous metabolite data and fixed the human
# M1 central volume using direct intravenous measurements in three dogs.
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
# Until these are set, desmetramadol is plotted as plasma only and contributes
# nothing to the opioid MEAC total, which means a tramadol dose currently
# shows concentrations but no effect.
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
#     ke0 = 0.0287536942 /min, an equilibration half-time of 24.1 min,
#     giving an effect-site peak at 150.0 min after 100 mg oral tramadol
#     in a normal metaboliser.
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
# tramadol.  With systemic formation alone the metabolite peaked at 4 h, and
# an effect site cannot peak before the curve driving it, so 2.5 h was not
# merely unfitted but impossible.  See the first-pass note in
# R/drugs_tramadol.R.
DESMETRAMADOL_KE0 <- 0.0287536942  # 1/min; solved, see above

# Recorded for documentation; the engine does not use it, because the solve
# above is against a curve tPeak cannot name.
DESMETRAMADOL_TPEAK_ORAL_PARENT <- 150  # minutes after an oral tramadol dose

# MEAC: a cited value, but a SECONDARY citation needing primary verification
# ---------------------------------------------------------------------------
# 84 ng/mL.  Lee 2019's introduction states that "the minimum effective
# concentration of M1 is 84 ug/L", citing earlier work.  That is the right
# quantity in the right units for this column: a minimum effective analgesic
# concentration of total M1 in ng/mL, not an in-vitro affinity and not an
# equianalgesic dose ratio, which is better than the basis for most of the
# provisional potencies in this package.
#
# What still needs doing:
#   - Read the primary source.  This value is taken from another paper's
#     introduction and the original was not retrieved.
#   - Check the analyte.  M1 is formed and eliminated stereoselectively and
#     the (+) enantiomer carries the mu activity, so a figure for total
#     racemic M1 and one for the active enantiomer differ by a factor that
#     is not two.
#   - Sanity check the scale.  At this value a 100 mg oral dose in a normal
#     metaboliser reaches roughly a quarter of the MEAC, which is consistent
#     with tramadol being a weak opioid but is worth confirming against
#     clinical experience.
DESMETRAMADOL_MEAC  <- 84  # ng/mL; provisional, see above
# -----------------------------------------------------------------------------


#' Desmetramadol (O-desmethyltramadol) pharmacokinetics
#'
#' Tramadol's active metabolite, and the species carrying its mu-opioid
#' effect.  Has no dosing unit of its own: see the file header for why a
#' directly administered dose is not identified by this parameter set.
#'
#' @param weight weight in kg
#' @param height height in cm (not used)
#' @param age age in years (not used)
#' @param sex sex as a string (not used)
#'
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
desmetramadol <- function(weight, height, age, sex)
{
  # Holford 2014 allometry: clearances (W/70)^0.75, volumes W/70
  perKg   <- weight / 70
  perKg75 <- perKg^0.75

  v1  <- 78.9 * perKg
  v2  <- 131  * perKg
  v3  <- 1                       # unused third compartment
  cl1 <- 84.2 * perKg75 / 60     # L/min
  cl2 <- 274  * perKg75 / 60     # L/min
  cl3 <- 0

  reference <- paste0(
    "Holford S et al., J Pharmacol Clin Toxicol 2014;2(1):1023. ",
    "Joint intravenous tramadol and O-desmethyltramadol model; the ",
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
