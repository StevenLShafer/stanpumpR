# -----------------------------------------------------------------------------
# Hydrocodone: oral only, with hydromorphone as its active metabolite
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min.
#
# ORAL ONLY, AND THIS IS NOT A PREFERENCE
# =======================================
# Hydrocodone's disposition is known only as APPARENT parameters: clearance
# over bioavailability, volume over bioavailability.  Melhem 2013 fitted oral
# extended-release data, and the absolute bioavailability of hydrocodone has
# never been measured; two separate FDA reviews say so.
#
# Apparent parameters predict oral concentrations correctly, because the
# unknown bioavailability cancels: Cp = F*D*f(t)/V = D*f(t)/(V/F).  They do NOT
# predict intravenous concentrations, which would be wrong by a factor of 1/F.
# Hydrocodone is therefore offered as an oral unit only, and bioavailability is
# carried as 1 because the apparent scale already contains it.
#
# The same gap is why the metabolite below is calibrated against an observed
# concentration ratio rather than built from a formation clearance: a physical
# formation flux cannot be recovered from an apparent disposition.
#
# DISPOSITION
# ===========
# Melhem 2013, reproduced in the FDA clinical pharmacology review, from 220
# subjects on a 12-hour extended-release capsule: CL/F 64.4 L/h, Vc/F 714 L,
# Q/F 0.910 L/h, Vp/F 151 L.  Note the exchange clearance really is 0.910 and
# not 91.0; the review prints the decimal.
#
# These carry no weight or body-surface scaling.  The source fitted creatinine
# clearance on CL and body surface area on Vc, but its reference values were
# not recovered, so no normaliser exists to apply them against.  Inventing one
# would be worse than leaving the model weight-independent, which is unusual
# for this package and deliberate here.
#
# The two eigenvalues are 0.0916 and 0.00594 per hour, half-times 7.6 h and
# 117 h.  The slow one carries only 1.6% of the area, so the model is nearly
# mono-exponential and its fast half-time of 7.6 h sits close to the 8.4 h
# Kapil 2015 observed.  The 117-hour component should not be described as a
# clinical elimination half-life.
#
# ABSORPTION
# ==========
# ka is set so the plasma peak falls at 60 min, which is the immediate-release
# behaviour this unit represents.  The source absorption model was a
# multi-phase extended-release structure whose routing was not recoverable, so
# it could not be transferred; a product-specific extended-release input is a
# separate piece of work.
#
# HYDROMORPHONE FORMATION
# =======================
# Calibrated against a directly observed ratio rather than a formation
# clearance.  Kapil 2015 gave 20 mg of extended-release hydrocodone to 24
# healthy adults and measured both species: hydrocodone AUC 325.3 ng.h/mL,
# hydromorphone AUC 3.8 ng.h/mL, a ratio of 0.0117.  An exposure ratio is
# independent of the input shape, so a ratio measured on an extended-release
# product transfers to an immediate-release one; only the timing differs.
#
# kFormation is scaled with weight, because the hydromorphone model it feeds
# scales with weight while hydrocodone's apparent volumes do not.  Holding the
# rate constant fixed instead would make the observed ratio drift in
# proportion to body weight, and the ratio is the thing that was measured.
#
# The same study blocked CYP2D6 with paroxetine and saw hydromorphone fall to
# 17% of control with hydrocodone unchanged, which is the cleanest available
# confirmation that this pathway is CYP2D6 and that it consumes a negligible
# share of hydrocodone clearance.
#
# CYP2D6 PHENOTYPE
# ================
# Anchored on hydrocodone's own data, not imported from codeine.  Otton 1993
# measured partial metabolic clearance to hydromorphone at 28.1 mL/h/kg in
# extensive metabolisers and 3.4 in poor metabolisers, a ratio of 0.121.  That
# floor is far above codeine's 0.035, so the two drugs genuinely differ and
# codeine's multipliers would be wrong here.
#
# Only those two points exist for hydrocodone.  The intermediate and
# ultrarapid values interpolate between them using the relative CYP2D6 ACTIVITY
# implied by Ashraf 2024's activity-score groups, rescaled so poor is zero and
# normal is one, and then applied only to the part of formation that CYP2D6
# accounts for:
#
#     weight(g) = floor + (1 - floor) * activity(g)
#
# The floor is hydrocodone-specific and measured; the shape between the
# endpoints is assumed. That assumption is the weakest part of this file.
#
# NOT MODELLED
# ============
# Norhydrocodone and hydromorphone-3-glucuronide. Norhydrocodone has opioid
# activity in mice with strongly route-dependent potency and no human potency
# estimate; the glucuronide has animal neuroexcitation evidence and human
# associations confounded by renal failure. Neither supports a human effect
# term, and omitting them is a modelling choice rather than a claim that they
# are inert.
#
# References
# ----------
# Melhem MR et al., Clin Pharmacokinet 2013;52:907-917.
#   https://doi.org/10.1007/s40262-013-0081-6
# FDA clinical pharmacology review, NDA 202880, numerical model on p.93.
#   https://www.accessdata.fda.gov/drugsatfda_docs/nda/2013/202880Orig1s000ClinPharmR.pdf
# Kapil RP et al., Clin Ther 2015;37:2286-2296.
#   https://doi.org/10.1016/j.clinthera.2015.08.007
# Otton SV et al., Clin Pharmacol Ther 1993;54:463-472.
#   https://doi.org/10.1038/clpt.1993.177
# Boswell MV et al., Pain Physician 2013;16:E227-235.
#   https://pubmed.ncbi.nlm.nih.gov/23703421/
# Navani DM, Yoburn BC, J Pharmacol Exp Ther 2013;347:497-505 (animal).
#   https://doi.org/10.1124/jpet.113.207548
# -----------------------------------------------------------------------------


# -----------------------------------------------------------------------------
# TO BE SUPPLIED
# -----------------------------------------------------------------------------
# No human concentration-effect vector for hydrocodone was found.  Until these
# two numbers are set, hydrocodone is plotted as plasma only and contributes
# nothing to the opioid MEAC total, with whatever effect it has appearing on
# the hydromorphone row.
#
# That UNDERSTATES hydrocodone.  Unlike codeine it is not a pure prodrug:
# Otton 1993 found poor metabolisers still reported opioid effects, so some of
# the effect is the parent's.  Boswell 2013 found postoperative pain relief
# tracking hydromorphone rather than hydrocodone concentrations, which points
# the other way.  The question is open, and picking a number is a clinical
# judgement rather than a literature lookup.
#
# Setting HYDROCODONE_MEAC here also requires updating the MEAC column of
# inst/extdata/drugDefaults_global.csv, which is what the plot and the opioid
# total actually read.  test-drugs-hydrocodone.R checks that the two agree.
HYDROCODONE_TPEAK <- 0   # minutes to peak effect site after an IV bolus
HYDROCODONE_MEAC  <- 0   # ng/mL
# -----------------------------------------------------------------------------


# Relative CYP2D6 formation activity, normal metaboliser = 1.  Floor from
# Otton 1993; shape between the endpoints from the Ashraf activity-score
# groups rescaled to poor = 0, normal = 1.  See the header.
HYDROCODONE_CYP2D6_WEIGHT <- c(
  poor         = 0.1209964413,
  intermediate = 0.5057888356,
  normal       = 1.0,
  ultrarapid   = 1.5024320700
)

#' Hydrocodone pharmacokinetics
#'
#' Oral only: the published disposition is apparent (divided by an unmeasured
#' bioavailability), which predicts oral concentrations correctly and
#' intravenous ones incorrectly.
#'
#' @param weight weight in kg (used only to scale metabolite formation, since
#'   the apparent disposition carries no weight term)
#' @param height height in cm (not used)
#' @param age age in years (not used)
#' @param sex sex as a string (not used)
#' @param cyp2d6 CYP2D6 metaboliser phenotype, one of \code{CYP2D6_VALUES}
#'
#' @returns a list in the shape \code{getDrugPK()} expects, naming
#'   hydromorphone as the formed active species
#' @export
hydrocodone <- function(weight, height, age, sex, cyp2d6 = CYP2D6_DEFAULT)
{
  if (length(cyp2d6) != 1 || !cyp2d6 %in% CYP2D6_VALUES) {
    stop("Invalid cyp2d6: ", paste(cyp2d6, collapse = ", "),
         ". Must be one of: ", paste(CYP2D6_VALUES, collapse = ", "))
  }
  activity <- unname(HYDROCODONE_CYP2D6_WEIGHT[[cyp2d6]])

  # --- Apparent disposition, Melhem 2013 (see header: no weight scaling) ---
  v1  <- 714                 # L,   Vc/F
  v2  <- 151                 # L,   Vp/F
  v3  <- 1                   # unused third compartment
  cl1 <- 64.4  / 60          # L/min, CL/F
  cl2 <- 0.910 / 60          # L/min, Q/F
  cl3 <- 0

  # --- Absorption: peak at 60 min ---
  ka_PO              <- 0.0637451777   # 1/min, = 3.825 /h
  bioavailability_PO <- 1              # the apparent scale already carries F
  tlag_PO            <- 0

  # --- Hydromorphone formation ---
  # Calibrated so the hydromorphone:hydrocodone AUC ratio reproduces Kapil
  # 2015's observed 3.8/325.3 = 0.01168.  Scaled by weight because the
  # hydromorphone model it feeds is weight-scaled while this one is not.
  KFORMATION_PER_KG <- 3.18574e-07     # 1/min per kg, normal metaboliser
  kFormation <- KFORMATION_PER_KG * weight * activity

  reference <- paste0(
    "Melhem MR et al., Clin Pharmacokinet 2013;52:907-917. ",
    "https://pubmed.ncbi.nlm.nih.gov/23719682/ (apparent oral disposition); ",
    "Kapil RP et al., Clin Ther 2015;37:2286-2296. ",
    "https://pubmed.ncbi.nlm.nih.gov/26350273/ (hydromorphone formation); ",
    "Otton SV et al., Clin Pharmacol Ther 1993;54:463-472. ",
    "https://pubmed.ncbi.nlm.nih.gov/7693389/ (CYP2D6)"
  )

  # Display band, ng/mL: Kapil saw 15.9 ng/mL after 20 mg extended release.
  typical      <- 20
  upperTypical <- 30
  lowerTypical <- 10

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

  return(
    list(
      PK = PK,
      tPeak = HYDROCODONE_TPEAK,
      MEAC = HYDROCODONE_MEAC,
      typical = typical,
      upperTypical = upperTypical,
      lowerTypical = lowerTypical,
      reference = reference,
      metabolite = list(
        name              = "hydromorphone",
        kFormation        = kFormation,
        # No first-pass branch.  Kapil's hydromorphone peaked at 16 h against
        # hydrocodone's 18 h, so the metabolite tracks the parent rather than
        # the absorption step, and the data give no reason to split the route.
        firstPassFraction = 0,
        # Hydromorphone 285.34, hydrocodone 299.36 g/mol
        mwRatio           = 285.34 / 299.36
      )
    )
  )
}
