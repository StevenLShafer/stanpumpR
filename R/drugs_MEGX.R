# -----------------------------------------------------------------------------
# MEGX (monoethylglycinexylidide): lidocaine's toxic metabolite
# -----------------------------------------------------------------------------
# Provenance
# ----------
# Drafted by Claude Code, 2026-11-13, at the request of Steven L. Shafer, who
# asked for the toxic metabolites of lidocaine and pethidine to be modelled as
# metabolites formed from the parent and drawn with their labels in red,
# because they add to toxicity and not to the therapeutic effect.  Read from
# the full text of Foong 2025 (supplied by Shafer).  Verified by
# tests/testthat/test-drugs-MEGX.R.
#
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in mcg/mL (= mg/L) of plasma MEGX.  The source reports
# clearances in L/h; they are divided by 60 here.
#
# SOURCE
# ======
# Foong KW, Loh PS, Chaw SH, Lo YL.  Population pharmacokinetics of IV
# lidocaine and its metabolites in adult surgical patients.  Pharm Res
# 2025;42:451-473.  98 adults (34 laparoscopic donor nephrectomy, 64
# laparoscopic cholecystectomy) given lidocaine 1.5 mg/kg over 10 min and
# 1.5 mg/kg/h to the end of surgery; 1,520 plasma concentrations of
# lidocaine, MEGX and GX by LC-MS/MS, sampled to 24 h; NONMEM, joint
# parent-metabolite model.  Table IV (the parent-metabolites final model,
# standardised to a 70 kg man with a fat-free mass of 56.1 kg):
#
#     TRTm1   transit time, lidocaine to MEGX   0.513 h    RSE 14.9%
#     CLm1    MEGX clearance                    52.8 L/h   RSE  4.24%
#     V1m1    MEGX central volume               30.7 L     RSE 20.4%
#     Q2m1    MEGX intercompartmental           442 L/h    RSE 15.1%
#     V2m1    MEGX peripheral volume, nephrectomy     302 L   RSE 8.59%
#                                    cholecystectomy  480 L   RSE 6.79%
#
# Structure (Fig. 4 and the Results): lidocaine three compartments; its
# elimination feeds ONE transit compartment emptying into MEGX's central
# compartment at 1/TRT (n = 1, KTR = n/TRT); MEGX two compartments.
#
# FRACTION METABOLISED FIXED TO ONE, SO THE PARAMETERS ARE APPARENT
# ================================================================
# MEGX was never given directly, so its volumes and the fraction of lidocaine
# converted (fm) cannot both be estimated.  Foong first fixed V1m1 to a
# literature value and estimated fm: it came out at one, and fixing it there
# changed the objective function insignificantly; they then fixed fm = 1 and
# estimated V1m1, which improved the fit (dOFV -7.44).  The parameters above
# are therefore on the scale of "all lidocaine cleared becomes MEGX": the true
# volumes and clearances are these multiplied by the true fm (more than 0.75
# per the hepatectomy paper's review), and the FORMED concentrations are
# unaffected by that scale.  So, as desethylamiodarone and desmetramadol,
# MEGX is offered with no dosing unit: it appears only when lidocaine is
# given.  The link in R/drugs_lidocaine.R forms it from ALL of the library
# lidocaine model's clearance (kFormation = k10).
#
# THE PARENT IS THE LIBRARY'S, NOT FOONG'S
# ========================================
# The library's lidocaine is Schnider 1996 (clearance 1.5 L/min = 90 L/h at
# 70 kg), not Foong's (45.9 L/h, falling to 40.5% of that postoperatively).
# Because formation is ALL of the parent's clearance, the amount of MEGX
# formed equals the lidocaine dose (times the molar ratio) whichever parent
# model clears it, and at steady state MEGX = infusion rate x 0.880 / CLm1
# whatever the parent's clearance.  What the parent model changes is the
# timing: Schnider's faster clearance forms MEGX sooner after a bolus than
# Foong's would.  Recorded in METABOLITE_FORMATION_BASIS as "joint" (the
# metabolite was fitted jointly with a parent, here not the library's).
#
# MOLAR RATIO
# ===========
# Formation is molar and concentrations are by mass: MEGX C12H18N2O, 206.28
# g/mol; lidocaine base C14H22N2O, 234.34 g/mol; ratio 0.8803 (in
# R/drugs_lidocaine.R).  Foong expressed doses as lidocaine hydrochloride and
# "accounted for" the molecular weights in the differential equations without
# printing the ratio used; if theirs was MEGX over the hydrochloride (206.28 /
# 270.80 = 0.762), this model's MEGX is 15% above theirs for the same
# parameters.  The library's lidocaine doses are entered as lidocaine, the
# convention the parent model already uses; the difference errs toward the
# higher, i.e. the safer, MEGX prediction.
#
# PERIPHERAL VOLUME: CHOLECYSTECTOMY
# ==================================
# V2m1 was the one covariate on MEGX: 302 L after donor nephrectomy, 480 L
# after cholecystectomy.  Foong found no organ-function marker that explained
# it.  The library uses the cholecystectomy value: the larger group (64 of
# 98), ordinary general surgery, and the slower approach to steady state
# (which leaves the steady state itself unchanged).  With it, at 70 kg, the
# half-lives are 2.45 min and 7.42 h; with 302 L the terminal half-life
# would be 4.80 h.  The transit adds a 0.513 h mean delay to formation.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# The model carries its own size covariate.  Foong scaled every MEGX volume
# by total body mass / 70 and every clearance by (total body mass / 70)^0.75
# (the Ffat estimates for the metabolites were imprecise and "all clearance
# and volume of distribution parameters scaled more effectively with TBM").
# That is applied as fitted, so the adjustToFFM switch changes nothing here.
#
# NO EFFECT SITE, NO BAND
# =======================
# MEGX is about 80-90% as potent as lidocaine in toxicity and antiarrhythmic
# effect (the hepatectomy and liposuction literature), but no human
# equilibration delay has been published: tPeak is zero and the row plots
# plasma.  No toxic or therapeutic threshold has been established for MEGX
# alone (the clinical convention, Crouch 2023, adds it to lidocaine against
# lidocaine's 5 mcg/mL), so no band is drawn.  The row's label is drawn in
# red (TOXIC_METABOLITES in R/constants.R): it adds to toxicity, not to
# analgesia.
#
# OTHER MODELS, NOT USED, AND HOW THEY DISAGREE
# =============================================
# MEGX clearance is the number that sets the steady state, and the published
# values differ by 3.5-fold:
#   Foong 2025 (used):          52.8 L/h at 70 kg, sampled to 24 h.
#   Crouch 2023 (Anesthesiology 138:71-81), 15 living liver donors:
#                               1.69 L/min (101 L/h) before resection, one
#                               compartment with its volume fixed to
#                               lidocaine's.
#   Tognolini 2025 (Anaesthesia 80:511-521), 30 patients with obesity
#                               (median 107 kg): unbound 4.19 L/min x fu 0.74
#                               = 3.10 L/min (186 L/h) total, sampled only to
#                               2 h after the infusion, while MEGX was still
#                               rising.
#   Hepatectomy (Drug Des Devel Ther 2025;19:6255-6268, PMID 40727924), 35 Chinese
#                               hepatectomy patients: volume fixed to
#                               lidocaine's, not a general population.
# Foong's is the lowest clearance and so the highest MEGX; it is also the
# only one sampled long enough after the infusion to see MEGX fall.  The
# help page states the range.
#
# References
# ----------
# Foong KW, Loh PS, Chaw SH, Lo YL. Pharm Res 2025;42:451-473.
#   https://doi.org/10.1007/s11095-025-03835-1
# -----------------------------------------------------------------------------

# Foong 2025, Table IV, at 70 kg.  Litres and litres per hour.
MEGX_V1  <- 30.7   # L
MEGX_V2  <- 480    # L, cholecystectomy (302 after donor nephrectomy)
MEGX_CL1 <- 52.8   # L/h
MEGX_CL2 <- 442    # L/h (Q2m1)
# Transit time from lidocaine's elimination to MEGX's central compartment.
MEGX_TRANSIT_TIME <- 0.513  # h

MEGX_REFERENCE <- paste0(
  "Foong KW, Loh PS, Chaw SH, Lo YL. Population pharmacokinetics of IV ",
  "lidocaine and its metabolites in adult surgical patients. ",
  "Pharm Res 2025;42:451-473. https://doi.org/10.1007/s11095-025-03835-1"
)

#' MEGX (monoethylglycinexylidide) pharmacokinetics
#'
#' Lidocaine's toxic metabolite.  Has no dosing unit of its own: see the file
#' header for why a directly administered dose is not identified.
#'
#' @inheritParams lidocaine
#' @returns a list in the shape \code{getDrugPK()} expects
#' @noRd
MEGX <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  # Total body mass, as fitted (see the header); adjustToFFM has no effect.
  vScale  <- weight / 70
  clScale <- (weight / 70)^0.75

  v1  <- MEGX_V1 * vScale
  v2  <- MEGX_V2 * vScale
  v3  <- 1                              # no third compartment
  cl1 <- MEGX_CL1 / 60 * clScale        # L/min
  cl2 <- MEGX_CL2 / 60 * clScale        # L/min
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
    MEAC = 0,            # not an opioid
    typical = 0,         # no established threshold: no band
    upperTypical = 0,
    lowerTypical = 0,
    reference = MEGX_REFERENCE
  )
}
