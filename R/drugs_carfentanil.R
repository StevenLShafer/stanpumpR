# -----------------------------------------------------------------------------
# carfentanil: ultrapotent fentanyl analog, on sufentanil's disposition
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in ng/mL, venous plasma.
#
# RESEARCH MODEL, OPT-IN
# ======================
# Carfentanil is in the library as an evidence-labelled research model, behind
# the "Illicit drugs" opt-in (off by default; see R/illicit-drugs.R).  It is a
# plasma-exposure estimate, not dosing advice, a safety threshold, or a
# prediction of any individual's response.  Carfentanil has no human
# indication: it is a veterinary immobilising agent that now appears as an
# adulterant in the illicit opioid supply.
#
# THE PHARMACOKINETICS ARE ASSUMED, NOT MEASURED
# ==============================================
# THERE IS NO PUBLISHED HUMAN PHARMACOKINETIC MODEL OF CARFENTANIL, and this
# is an absence rather than a thin literature.  Every human study that
# measures carfentanil uses [11C]carfentanil as a PET RADIOTRACER to measure
# some other drug's mu-opioid receptor occupancy; those are microdoses
# reporting brain binding, not carfentanil's plasma kinetics.  The only
# in-vivo carfentanil pharmacokinetics are in rats (Bergh 2019), and they are
# non-linear: the area under the curve rises faster than the dose and the
# half-life lengthens as the dose rises.
#
# This model therefore uses **sufentanil's disposition**, on the judgement of
# Steven L. Shafer that carfentanil's pharmacokinetics are likely identical to
# sufentanil's.  The two are close congeners from the same 4-substituted
# phenylpiperidine series and share the physicochemistry - high lipophilicity,
# high protein binding, high hepatic extraction - that sets the volumes and
# the clearance of a drug in this class.  It is an informed assumption by a
# pharmacokineticist, attributed as such, and it is the only thing in this
# file that is not a measurement.  It is NOT a fitted carfentanil model and
# must not be cited as one.
#
# The parameters are not copied: this function calls sufentanil() and returns
# its disposition, so that the two stay identical by construction and any
# future correction to sufentanil's vector carries to carfentanil as well,
# which is what "identical to sufentanil" has to mean to be worth anything.
#
# WHAT THAT ASSUMPTION DOES AND DOES NOT COVER
# ============================================
# It covers the shape of the curve: how fast the concentration falls after a
# dose, and how much it accumulates during one.  It does NOT make the two
# drugs interchangeable, because they differ enormously in potency, and
# potency is not a pharmacokinetic quantity.  Steven L. Shafer puts
# carfentanil at about 20 times the potency of fentanyl; published estimates
# are generally higher, commonly around 100 times, derived from the frequently
# quoted "10,000 times morphine".  Nothing in this model uses any of those
# numbers: like every drug in the illicit category it is plasma only, so no
# potency ratio enters the simulation at any point.  The help page carries the
# figure so that a reader can scale the curve themselves if they want to.
#
# ROUTE
# =====
# Intravenous only.  Sufentanil's parameters are ABSOLUTE volumes and
# clearances from an intravenous study, so they predict an intravenous dose
# correctly; no absorption model for any other route of carfentanil exists,
# and stanpumpR has no pulmonary route in any case.  Carfentanil is
# encountered insufflated and smoked as well as injected, and neither of those
# is offered.  See inst/extdata/illicitRouteRegistry.csv.
#
# NO EFFECT SITE
# ==============
# tPeak 0, MEAC 0, no band, as for every drug in the illicit category.
# Sufentanil's equilibration half-time is not carried across: the
# pharmacokinetic assumption is about disposition, and an effect-site rate
# constant belongs to the receptor rather than to the body's handling of the
# drug, so borrowing it would extend the assumption past what was claimed.
# -----------------------------------------------------------------------------

#' Carfentanil pharmacokinetics
#'
#' Ultrapotent fentanyl analog.  The disposition is **assumed identical to
#' sufentanil's**, on the judgement of Steven L. Shafer, because no human
#' pharmacokinetic model of carfentanil exists.  Plasma only, intravenous
#' only, and behind the illicit-drug opt-in.
#'
#' @param weight weight in kg
#' @param height height in cm
#' @param age age in years
#' @param sex "male" or "female"
#' @param adjustToFFM scale to fat-free mass (docs/weight-adjustment.md)
#' @returns the drug's PK list, tPeak, MEAC, band and reference
#' @export
carfentanil <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  # Units **************
  # Time: Minutes
  # Volume: Liters

  # Sufentanil's disposition, taken from the function rather than copied, so
  # the two cannot drift apart (see the header).  Its size scaling comes with
  # it: volumes by fat-free mass relative to the 70 kg, 170 cm reference male,
  # clearances by that ratio ^ 0.75, and no scaling at all with the switch off.
  PK <- sufentanil(weight, height, age, sex, adjustToFFM)$PK

  # Plasma only, as every illicit-category drug is.  Sufentanil's tPeak and
  # MEAC are deliberately NOT carried across: the borrowed assumption is about
  # disposition, not about receptor kinetics or potency.
  tPeak <- 0
  MEAC <- 0
  typical <- 0
  lowerTypical <- 0
  upperTypical <- 0

  reference <- paste0(
    "No human pharmacokinetic model of carfentanil exists. The disposition ",
    "here is sufentanil's (Gepts E et al., Anesthesiology 1995;83(6):",
    "1194-1204), used on the judgement of Steven L. Shafer that carfentanil's ",
    "pharmacokinetics are likely identical to sufentanil's, the two being ",
    "close congeners of the same series. This is an attributed assumption, ",
    "not a fitted carfentanil model. ",
    "https://pubmed.ncbi.nlm.nih.gov/8533912/"
  )

  return(
    list(
      PK = PK,
      tPeak = tPeak,
      MEAC = MEAC,
      typical = typical,
      upperTypical = upperTypical,
      lowerTypical = lowerTypical,
      reference = reference
    )
  )
}
