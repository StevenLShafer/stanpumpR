# -----------------------------------------------------------------------------
# Landiolol (Kunisawa 2015): plasma landiolol in anaesthetised elderly patients
# with peripheral arterial disease
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min.
#
# Drafted by Claude Code, 2026-11-25, at the request of Steven L. Shafer
# (docs/library-expansion/plan.csv, Anesthesia).  Every number below was read
# on the PDF of the source (stanpumpRPapers PMID_25653534.pdf; card
# papers/digests/PMID_25653534.md).
#
# THE SOURCE
# ==========
# Kunisawa T, Yamagishi A, Suno M, Nakade S, Honda N, Kurosawa A, Sugawara
# A, Tasaki Y, Iwasaki H.  Target-controlled infusion and population
# pharmacokinetics of landiolol hydrochloride in patients with peripheral
# arterial disease.  Ther Clin Risk Manag 2015;11:107-114 (doi
# 10.2147/TCRM.S74867).  Eight patients
# (6 men, 2 women; Table 1: 73 +/- 7 years, 64-84; 58.9 +/- 10.9 kg; 159 +/-
# 7 cm) anaesthetised with propofol and remifentanil by TCI, on dopamine 3
# mcg/kg/min, given landiolol hydrochloride by STANPUMP TCI to plasma targets
# of 500 then 1,000 ng/mL for 30 minutes each; 112 radial arterial samples
# (into ethanol and neostigmine, so the plasma esterases were stopped), HPLC
# with fluorescence.  NONMEM, two compartments with a lag time; one and three
# compartments did not converge; weight, lean body mass and age were tested
# and none retained.  Table 2:
#
#     CL    30.7 mL/min/kg      Q    48.3 mL/min/kg
#     V1    65.0 mL/kg          V2   54.4 mL/kg
#     ALAG  0.633 min           IIV on CL only, 13.5%
#
# WHY THIS SOURCE, AND WHAT IS MISSING [decision]
# ===============================================
# Three population models of landiolol exist, all Japanese, all two
# compartments with a lag: Honda 2008 (47 healthy men, PMID 19122339; CL
# 36.6, V1 101, Q 16.1, V2 55.6, printed again beside these values in Table
# 2), Kunisawa 2014 (9 gynaecologic surgical patients, PMID 25186494; CL
# 34.0, V1 74.9, Q 70.9, V2 38.9, from its abstract) and this one.  Only this
# one is in the archive.  It is used for every patient until the other two
# are read; Honda's larger volunteer study would then be the candidate for
# younger adults, by the library's rule of the source population
# (R/modelSelection.R).  Both are rows of papers/pending-work.csv.  The
# clearances agree within 20%; the central volumes do not (65 to 101 mL/kg),
# which the library's rule on comparing sources expects of a construct.
#
# The US label (Rapiblyk, NDA 217202, revised 2/2026, section 12.3) gives a
# total body clearance of 57 mL/kg/min after a 20-hour infusion in healthy
# volunteers (Krumpl 2018, whole blood).  That is nearly twice this model's
# clearance; the label's matrix is whole blood and its volunteers were awake
# Europeans, so the difference is recorded, not reconciled.
#
# THE LAG
# =======
# The source fitted a lag of 0.633 min on an intravenous infusion, which no
# intravenous engine of the library represents.  It is left out, as the
# authors themselves left it out of the STANPUMP TCI they gave: every curve
# is drawn 38 s early, and the concentrations reached are unchanged.
#
# DOSE BASIS
# ==========
# The paper gives landiolol hydrochloride and reports concentrations of
# "landiolol" without naming the basis of either.  The US product is
# labelled as landiolol base (280 mg landiolol = 300 mg of the
# hydrochloride; 9.3 mcg/kg/min of base = 10 of the hydrochloride).  By the
# library's rule (AGENTS.md, Dose basis) the parameters are read in the
# approved product's units and a dose is entered as labelled, landiolol base.
# Were the paper's concentrations base and its doses hydrochloride, its
# clearance would be 7% too high for base doses (546.06/509.6); that is
# inside the 20% between the three Japanese sources.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# Every parameter is per kilogram, normalised by the source, so it is
# evaluated at the pharmacokinetic weight with the fat-free-mass switch on
# (size$pkWeight) and at total body weight with it off, as esmolol's.  A
# model published per kilogram is not anchored at its cohort.
#
# EFFECT SITE: tPeak 2 MINUTES, FOR HEART RATE (Krumpl 2017); NO BAND
# ===================================================================
# Kunisawa fitted no concentration-effect relation.  Krumpl G, Ulc I, Trebs
# M, Kadlecova P, Hodisch J, Eur J Clin Pharmacol 2017;73:417-428 (PMID
# 28091703, read on the PDF), gave 12 healthy volunteers landiolol 0.1, 0.2
# and 0.3 mg/kg at 0.1 mg/kg per 15 s: after 0.2 and 0.3 mg/kg the lowest
# heart rate was the 2-minute value (Table 4: 52.2 and 50.5 bpm), so the time
# to peak heart-rate effect is 2 minutes, as for esmolol.  Heart rate fell
# significantly from 1 minute.  No heart rate is predicted.  Landiolol has no
# therapeutic concentration range; it is titrated to heart rate.
#
# PREGNANCY
# =========
# No pharmacokinetic study in pregnancy (PubMed, 2026-11-25: case reports at
# caesarean section and one randomised trial of a 0.2 mg/kg bolus at
# induction, Suehiro 2012, with no concentrations).  Recorded unknown.
# -----------------------------------------------------------------------------

# Kunisawa 2015, Table 2 (patients with peripheral arterial disease)
LANDIOLOL_CL <- 0.0307   # L/min/kg
LANDIOLOL_V1 <- 0.0650   # L/kg
LANDIOLOL_Q  <- 0.0483   # L/min/kg
LANDIOLOL_V2 <- 0.0544   # L/kg
LANDIOLOL_ALAG <- 0.633  # min, not represented (see the header)
LANDIOLOL_TPEAK <- 2     # min, heart rate (Krumpl 2017, Table 4)

#' Landiolol (Kunisawa 2015)
#'
#' Two compartments per kilogram from eight anaesthetised elderly patients
#' with peripheral arterial disease; see the header of
#' \code{R/drugs_landiolol.R}.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
landiolol <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)
  pkW  <- if (isTRUE(adjustToFFM)) size$pkWeight else weight

  default <- list(
    v1  = LANDIOLOL_V1 * pkW,
    v2  = LANDIOLOL_V2 * pkW,
    v3  = 1,                          # placeholder: a two-compartment model
    cl1 = LANDIOLOL_CL * pkW,
    cl2 = LANDIOLOL_Q  * pkW,
    cl3 = 0
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Kunisawa T et al., Ther Clin Risk Manag 2015;11:107-114. ",
    "Two compartments in 8 anaesthetised patients with peripheral arterial ",
    "disease aged 64-84: CL 30.7 mL/min/kg, V1 65.0 mL/kg, Q 48.3 ",
    "mL/min/kg, V2 54.4 mL/kg (the fitted 0.63-min lag is not drawn). ",
    "Arterial plasma; doses as landiolol base, as labelled.  Time to peak ",
    "heart-rate effect 2 min from Krumpl G et al., Eur J Clin Pharmacol ",
    "2017;73:417-428. ",
    "https://doi.org/10.2147/TCRM.S74867"
  )

  list(
    PK = PK,
    tPeak = LANDIOLOL_TPEAK,
    MEAC = 0,
    typical = 0,
    upperTypical = 0,
    lowerTypical = 0,
    reference = reference
  )
}
