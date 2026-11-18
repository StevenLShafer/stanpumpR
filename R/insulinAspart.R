# -----------------------------------------------------------------------------
# Subcutaneous insulin aspart: the population model of the FDA review of
# Fiasp (NDA 208751), shared by NovoLog and Fiasp
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min, doses in
# units (U), concentrations in mcU/mL (= mU/L).  Concentration only: no
# glucose, glucose infusion rate or HbA1c is plotted.  Drafted by Claude
# Code, 2026-11-18, at the request of Steven L. Shafer (top-200 review,
# docs/top200-review.md: insulin aspart, priority 1).  Each product is its own
# drug: R/drugs_insulinAspart.R (NovoLog) and R/drugs_insulinAspartFiasp.R
# (Fiasp).  Tests: tests/testthat/test-drugs-insulinAspart.R and
# test-drugs-insulinAspartFiasp.R.
#
# SOURCE
# ======
# US Food and Drug Administration, Center for Drug Evaluation and Research.
# Clinical Pharmacology Review, NDA 208751 (Fiasp, insulin aspart injection),
# resubmission of 29 March 2017, Reference ID 4149259.  Appendix 5.3,
# "Pharmacokinetic-Pharmacodynamic and Exposure-Response Analysis" (printed
# pp 84-93), the Applicant's (Novo Nordisk's) population PK-PD model, with
# which "the Reviewer is in agreement".  Read in full from the PDF: the text
# layer for the prose, the page images for every table and figure (the
# tables are pictures with no text layer).
# https://www.accessdata.fda.gov/drugsatfda_docs/nda/2017/208751Orig1s000ClinPharmR.pdf
#
# Data: 135 adults with type 1 diabetes (Table 1, printed p 84) from three
# studies, NN1218-3978 and -3891 (euglycaemic clamps, 0.2 U/kg) and -3922
# (meal test, 0.06 to 0.28 U/kg); 113 aged 18-64 and 22 aged 65 or more
# (21 to 73 years), 89 men and 46 women, weight 75.0 (SD 10.4) kg, range 50
# to 100 kg, BMI 18.9 to 28.7.  Each subject had both products.  The
# analyte is TOTAL insulin aspart (free plus bound to anti-insulin aspart
# antibodies; the resubmission's assay), in pmol/L.
#
# Structure (the review's Figure 1, printed p 86, and its text): "A
# one-compartment model with first-order absorption through 3 transit
# compartments and first-order elimination": a dose depot emptying at ka into
# the first of three transit compartments, each emptying at kt, the third
# into the central volume V, eliminated at CL.  NONMEM 7.3, FOCE+I.  The two
# products share everything but kt and a relative bioavailability ("the
# different absorption properties ... were captured by different values for
# the transit rate constant ... The disposition part of the PK model was
# treatment-independent by assumption").  Table 5 (printed p 90), the
# estimates:
#
#     KA            1.53  /h      absorption rate constant, both products
#     KT            55.8  /h      transit rate constant, Fiasp
#     KT.NovoRapid  16.1  /h      transit rate constant, NovoLog (NovoRapid)
#     CL            67.8  L/h     clearance/F
#     V             78.6  L       volume of distribution/F
#     BA            1     -       relative bioavailability, Fiasp (fixed)
#     BA.NovoRapid  0.99  -       relative bioavailability, NovoLog
#     BWonCL, BWonV 1, 1  -       body weight on CL/F and V/F (fixed)
#     ABonCL        -0.183        antibody binding (%B/T) on CL/F
#     AGEGRPonCL    0.905         age group (65 or more) on CL/F
#     DOSEonCL      -0.198        insulin dose on CL/F
#     SEXonCL       0.914         sex on CL/F
#     BMIonKA       -1.18         BMI on ka
#
# THE CHAIN, AND WHY IT IS EXACT
# ==============================
# Three transit compartments at ONE rate kt have that eigenvalue three times
# over, which no closed-form coefficient can carry (R/advanceTransitChain.R),
# so the drug runs on the matrix-exponential engine, exact to rounding, with
# the block's subcutaneous route (route = "SC", added for this drug).  The
# engine's chain empties its first n - 1 compartments at ktr and its LAST at
# the absorption constant, where the source's depot empties at ka FIRST and
# the transits follow.  The two are the same input to the central
# compartment: compartments in series with no other outflow pass drug on by
# the convolution of their exponential densities, which does not depend on
# their order (the transfer function is the product of ka/(s + ka) and
# (kt/(s + kt))^3, and multiplication commutes).  So n = 4, ktr = kt and
# ka_SC = ka give the source's central concentrations, and the amount still
# to be absorbed (the Depot panel, the whole chain) is the same too.  The
# test file checks it against a matrix exponential of the source's own
# order.  A pump rate ("U/hr SC") is a constant input into the head of the
# chain, which the engine carries exactly.
#
# WHAT IS APPLIED
# ===============
# * Body weight: CL/F and V/F proportional to weight (the fixed exponents of
#   1).  THE SOURCE'S REFERENCE WEIGHT IS NOT PRINTED; it is taken as 75 kg
#   (INSULIN_ASPART_SOURCE_WEIGHT), the mean weight of the 135 subjects
#   (Table 1), the review's own non-compartmental comparisons being "adjusted
#   for body weight (75 kg)".  On Shafer's decision of 2026-11-18 the model is
#   then restated at the library's 70 kg reference man and scaled as every
#   library model is (docs/weight-adjustment.md): Table 5's values are moved
#   from 75 to 70 kg by the source's own proportional term, CL/F 67.8 x 70/75
#   = 63.28 L/h and V/F 78.6 x 70/75 = 73.36 L, and pkSizeFactors() scales
#   them from there.  With the fat-free-mass switch off its legacy factors
#   are weight/70 on both, which is the source's term exactly (CL/F = 67.8 x
#   weight/75), so nothing the source predicts changes.  With the switch on,
#   V/F scales by fat-free mass and CL/F by its 0.75 power, as for every
#   drug with no size term of its own; for the reference man the two
#   positions agree.  Neither is held to the studied 50 to 100 kg.
#
#   CHECK of the conversion and the reference: the review's Figure 4
#   (printed p 93) simulates "a typical subject" given 0.2 U/kg; measured
#   from the figure's pixels against its axis ticks, its central
#   concentration peaks at about 564 pmol/L at about 58 minutes (Fiasp) and
#   556 at about 67 (NovoLog).  This model at 75 kg with the size switch off (the
#   source's own weight term), with 1 U = 6 nmol, gives 545.6 pmol/L at 54.8
#   min (Fiasp) and 535.9 at 63.6 min (NovoLog) (the test file pins both):
#   3.3% and 3.6% lower and 3 minutes earlier than the figure.  Had Table 5's
#   values described a 70 kg subject instead, every peak would be 7% lower
#   (509 pmol/L for Fiasp); the 75 kg reading is the one this figure
#   supports.  The figure's "typical subject" is not described further (its
#   dose, sex and antibody level would move it through the terms not applied
#   here), so the residual 3.5% is recorded, not fitted away.
# * Age group: CL/F x 0.905 at 65 years or more (AGEGRPonCL).  The reference
#   category is not printed; it is taken as the younger group, which was 113
#   of the 135 subjects, and the review's own non-compartmental analysis
#   agrees in direction: total exposure 1.09 times higher in geriatric than
#   younger patients (printed p 43), against 1/0.905 = 1.105 here.  A step at
#   65 years, as fitted.
#
# WHAT IS NOT APPLIED, AND WHY
# ============================
# * Sex on CL/F (0.914): which sex is the reference is not printed, and the
#   review's non-compartmental analysis found total exposure "comparable" in
#   men and women (printed p 46), so neither direction can be read.
# * Insulin dose on CL/F (-0.198) and BMI on ka (-1.18): powers, presumably,
#   of quantities whose reference values (and, for the dose, units: U or
#   U/kg) are not printed.  The typical CL/F is the one at the unprinted
#   reference dose, most likely near the studies' 0.2 U/kg (15 U at 75 kg).
#   Doses away from it would carry a lower (larger dose) or higher clearance;
#   the review's own dose-proportionality analysis found AUC rising slightly
#   more than in proportion (slope 1.14, printed p 38), the same direction.
# * Antibody binding on CL/F (-0.183): the Patient Profile has no anti-insulin
#   antibody field, and the term's form is not printed.
#
# THE ANALYTE, AND THE VOLUME
# ===========================
# The curve is TOTAL insulin aspart, free plus antibody-bound, above zero
# (the subjects had type 1 diabetes and the assay is specific to insulin
# aspart, so there is no endogenous baseline to add).  Concentrations are
# converted from the source's pmol/L at 6 pmol/L per mcU/mL (1 U of insulin
# = 6 nmol, the convention the insulin literature uses, e.g. Lindauer 2019,
# PMID 30369394; the labels give no molar equivalent), which the Figure 4
# check above supports: a dose in mU over V/F in litres is then mcU/mL with
# V/F as printed.  Free insulin aspart, which the original submission and
# Heise 2015 (PMID 25846340) measured, is lower: Heise's Cmax after 0.2 U/kg
# was about 320 pmol/L, against about 540 total here.
#
# V/F is 78.6 L, about 1.05 L/kg, where the intravenous volume of insulin
# aspart is 0.15 L/kg (the review of the pediatric supplement, S-011,
# summarizing the original program), and the elimination rate constant,
# CL/V = 0.863 /h (half-life 48 min), is far slower than the intravenous
# half-life of 7.2 min.  That is the familiar flip-flop of subcutaneous
# insulin: absorption is the slowest step, and a model fitted to
# subcutaneous data alone cannot tell which of its rate constants is
# absorption.  The concentrations are unaffected (the same curve either
# way), but what the Peripheral compartments panel calls the Depot, and what
# the Visualizer draws as the central volume, follow the model's assignment,
# not the anatomy.  Clearance is the comparable number: CL/F 67.8 L/h at 75
# kg is 0.90 L/h/kg; the intravenous clearance the S-011 review reports for
# Fiasp in healthy subjects, 0.90 (L/h)/kg, divided by the absolute
# subcutaneous bioavailability it reports in the abdomen, 85%, would be 1.06
# L/h/kg.  The difference is in the direction of total (antibody-bound
# included) against free insulin, and of type 1 diabetes against health.
#
# UNFITTED CHECKS
# ===============
# * NovoLog label (revised 2/2023, section 12.3): in type 1 diabetes, 0.15
#   U/kg SC reached a mean maximum of 82 mU/L (free insulin, an older assay),
#   with a median time to maximum of 40 to 50 minutes in healthy volunteers
#   and type 1 diabetes.  This model at 75 kg: 67 mcU/mL at 64 min (total
#   insulin aspart).  Not one of the studies fitted.
# * The apparent terminal half-life: the model's slowest rate constant is
#   CL/V, 48 minutes; the review's non-compartmental terminal half-life in
#   the fitted clamp study was 68.1 (Fiasp) and 67.8 (NovoLog) minutes
#   (geometric means), and the labels say about 1.1 hours (Fiasp) and 81
#   minutes (NovoLog, healthy men).  The model's tail therefore falls faster
#   than the measured one: by six hours after 0.2 U/kg it is at about 3% of
#   its peak (the test file pins it), where the review's Figure 2 shows the
#   observed geometric mean at six hours near 30 pmol/L, about 6% of its peak
#   of about 520, read from the figure (printed p 89), with the review's own
#   population prediction drawn below the six-hour points as well.
#
# PREGNANCY
# =========
# With the Patient Profile's Pregnant field on, CL/F x INSULIN_PREGNANCY_CL_FACTOR
# (1.25; Catalano 1998, the metabolic clearance rate of infused insulin at a
# clamp, 526 mL/m2/min at 34-36 weeks against 422 before conception),
# exactly as for the Tham insulins (R/insulinTham.R), so that every insulin
# in the library moves together.  Insulin aspart's own pregnancy data (Goudie
# 2014 and 2015, by pump in 22 women with type 1 diabetes at 12 to 33 weeks)
# found the clearance per kg hardly changing within pregnancy (-0.33% per
# week, not significant) and the time to peak lengthening 1.6-1.7% per week;
# that is a trend within pregnancy, not a comparison with the non-pregnant
# state, and is recorded, not applied.  Absorption and V/F are unchanged.
#
# References
# ----------
# FDA CDER, Clinical Pharmacology Review, NDA 208751 (Fiasp), 2017.
#   https://www.accessdata.fda.gov/drugsatfda_docs/nda/2017/208751Orig1s000ClinPharmR.pdf
# Heise T et al.  Faster-acting insulin aspart: earlier onset of appearance
#   and greater early pharmacokinetic and pharmacodynamic effects than
#   insulin aspart.  Diabetes Obes Metab 2015;17:682-688.
#   https://doi.org/10.1111/dom.12468
# Catalano PM, Drago NM, Amini SB.  Diabetes Care 1998;21:403-408.
#   https://doi.org/10.2337/diacare.21.3.403 (pregnancy)
# -----------------------------------------------------------------------------

INSULIN_ASPART_REFERENCE_URL <-
  "https://www.accessdata.fda.gov/drugsatfda_docs/nda/2017/208751Orig1s000ClinPharmR.pdf"

# Table 5 (see the header): per hour, L/h, L
INSULIN_ASPART_KA <- 1.53
INSULIN_ASPART_CL <- 67.8
INSULIN_ASPART_V  <- 78.6
INSULIN_ASPART_PRODUCTS <- list(
  novolog = list(label = "NovoLog", kt = 16.1, f = 0.99),
  fiasp   = list(label = "Fiasp",   kt = 55.8, f = 1)
)

# The transit compartments between the depot and the central volume
INSULIN_ASPART_TRANSITS <- 3L

# The weight Table 5's CL/F and V/F are taken to describe, kg: not printed;
# the mean of the 135 subjects (Table 1).  The model is restated at the
# library's 70 kg reference (FFM_REFERENCE_WEIGHT) by the source's
# proportional term; see the header (Shafer, 2026-11-18).
INSULIN_ASPART_SOURCE_WEIGHT <- 75

# Age group on CL/F, at INSULIN_ASPART_GERIATRIC_AGE years or more
INSULIN_ASPART_GERIATRIC_CL <- 0.905
INSULIN_ASPART_GERIATRIC_AGE <- 65

#' An insulin aspart product as a drug model
#'
#' @inheritParams cefazolin
#' @param product a name of `INSULIN_ASPART_PRODUCTS`
#' @param pregnant when \code{TRUE}, CL/F x INSULIN_PREGNANCY_CL_FACTOR (see
#'   the file's header)
#' @returns a list in the shape \code{getDrugPK()} expects
#' @keywords internal
insulinAspartModel <- function(weight, height, age, sex, adjustToFFM, product,
                               pregnant = FALSE)
{
  p <- INSULIN_ASPART_PRODUCTS[[product]]
  # Table 5 at 75 kg, restated at the 70 kg reference man, then scaled;
  # with the switch off the legacy factors (weight/70) give the source's
  # CL/F and V/F x weight/75 exactly
  toReference <- FFM_REFERENCE_WEIGHT / INSULIN_ASPART_SOURCE_WEIGHT
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)
  geriatric <- age >= INSULIN_ASPART_GERIATRIC_AGE
  cl <- INSULIN_ASPART_CL * toReference * size$clearance *
    if (geriatric) INSULIN_ASPART_GERIATRIC_CL else 1
  if (isTRUE(pregnant)) cl <- cl * INSULIN_PREGNANCY_CL_FACTOR
  v <- INSULIN_ASPART_V * toReference * size$volume

  default <- list(
    v1  = v, v2 = 1, v3 = 1,
    cl1 = cl / 60, cl2 = 0, cl3 = 0,
    ka_SC = INSULIN_ASPART_KA / 60,
    bioavailability_SC = p$f,   # relative; CL/F and V/F are apparent
    tlag_SC = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "FDA Clinical Pharmacology Review of Fiasp (NDA 208751, 2017), Table 5, ",
    p$label, ": a depot (ka ", INSULIN_ASPART_KA, "/h), three transit ",
    "compartments (kt ", p$kt, "/h) and one compartment, CL/F ",
    INSULIN_ASPART_CL, " L/h and V/F ", INSULIN_ASPART_V, " L at an assumed ",
    INSULIN_ASPART_SOURCE_WEIGHT, " kg, restated at 70 kg (",
    sprintf("%.2f", INSULIN_ASPART_CL * 70 / INSULIN_ASPART_SOURCE_WEIGHT), " L/h, ",
    sprintf("%.2f", INSULIN_ASPART_V * 70 / INSULIN_ASPART_SOURCE_WEIGHT),
    " L) and scaled by body size, relative F ",
    p$f, ", CL/F x ", INSULIN_ASPART_GERIATRIC_CL, " from ",
    INSULIN_ASPART_GERIATRIC_AGE, " years; sex, dose, BMI and antibody terms ",
    "not applied; total insulin aspart in adults with type 1 diabetes, ",
    "concentration only. ", INSULIN_ASPART_REFERENCE_URL
  )
  if (isTRUE(pregnant)) {
    reference <- paste0(reference, "; ", pregnancyReference(
      paste0("CL/F x ", INSULIN_PREGNANCY_CL_FACTOR),
      paste0("Catalano PM et al., Diabetes Care 1998;21:403-408. ",
             "https://doi.org/10.2337/diacare.21.3.403")))
  }

  list(
    PK = PK, tPeak = 0, MEAC = 0,
    typical = 0, upperTypical = 0, lowerTypical = 0,
    # The depot and the three transits: n = 4, the first three at kt and the
    # last at ka_SC, the source's order reversed, which changes nothing (see
    # the header); exact, by matrix exponential (R/advanceTransitChain.R)
    transitChain = list(n = INSULIN_ASPART_TRANSITS + 1L, ktr = p$kt / 60,
                        kMetabolic = 0, route = ROUTE_SC),
    reference = reference
  )
}
