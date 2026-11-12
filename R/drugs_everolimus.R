# -----------------------------------------------------------------------------
# Everolimus (the FDA reviewer's population model, NDA 22-334): two
# compartments, apparent oral, whole blood
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min; amounts in
# mcg, so the plotted concentration is ng/mL (whole blood).
#
# Drafted by Claude Code, 2026-11-12, at the request of Steven L. Shafer, who
# asked for the common drugs with narrow therapeutic windows that the library
# lacked.  The numbers below were read from the FDA's Clinical Pharmacology
# and Pharmacometric review of NDA 22-334 (Afinitor, 2009; accessdata.fda.gov,
# 022334s000_ClinPharmR.pdf, Pharmacometric Review Tables 1 and 4 and the
# Clinical Pharmacology text) and from the Zortress label (NDA 021560, 2026),
# read in full.
#
# THE SOURCE
# ==========
# The sponsor's population analysis of 398 patients with advanced solid
# tumours (phase 1/1b studies and the registration trial C2240; 1667
# whole-blood concentrations, 1-20 per patient, LC/MS), doses 5-10 mg daily,
# "adequately described by a two-compartment model", refitted by the FDA's
# pharmacometric reviewer.  The sponsor's model carried a renal-cell-cancer
# factor on clearance (0.682); the sponsor's own sensitivity analysis found it
# "may be due to inconsistencies of trough measurements" rather than "a true
# physiological effect", and the reviewer's model, used here, omits it.
# Table 4 (estimate, %RSE), apparent parameters:
#
#     central volume    172 L (8.2)      peripheral volume  495 L (7.6)
#     clearance         14.8 L/h (3)     intercompartmental 48 L/h (7.5)
#     ka                6.07 /h (fixed), no lag
#
# "No evidence for impact of the other covariates under consideration was
# detected."  The reviewer's clearance agrees with the C2240 patients
# sampled intensively, CL/F 15.4 (5.3) L/h on day 15 of 10 mg daily; healthy
# subjects cleared more, 20.5 (6.7) L/h after a single 10 mg dose.
#
# TRANSPLANTATION AND CYCLOSPORINE
# ================================
# The source's patients took no calcineurin inhibitor.  Zortress is given
# with reduced-dose cyclosporine or tacrolimus, and cyclosporine (Neoral 175
# mg) "increased everolimus AUC by 168%" in healthy subjects.  The Zortress
# label's kidney transplant patients, on 0.75 mg twice daily with
# cyclosporine, had CL/F 8.8 L/h, Vc/F 110 L and a half-life of 30 +/- 11 h;
# so with cyclosporine, which is not modelled, the concentrations are about
# 1.7 times this model's.  The label also gives clearance falling 0.33% a
# year of age and 20% higher in black patients, neither applied.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# No weight term; the values are written for the library's reference man and
# scaled by fat-free mass with the switch on, fixed with it off.
#
# DOSE BASIS
# ==========
# Everolimus is not a salt: Zortress (0.25, 0.5, 0.75 and 1 mg tablets) and
# Afinitor are labelled in mg of everolimus.
#
# NOT MODELLED
# ============
# Cyclosporine and the other CYP3A4 and P-glycoprotein inhibitors and inducers
# (ketoconazole raised the AUC 15-fold); hepatic impairment; haematocrit,
# which whole-blood everolimus follows.
#
# THE BAND
# ========
# The Zortress label: "the recommended everolimus therapeutic range is 3 to 8
# ng/mL", a trough by LC/MS/MS with reduced-exposure cyclosporine or
# tacrolimus, drawn against the whole curve.
# -----------------------------------------------------------------------------

# NDA 22-334 Pharmacometric Review, Table 4 (reviewer's model)
EVEROLIMUS_VC <- 172     # L
EVEROLIMUS_VP <- 495     # L
EVEROLIMUS_CL <- 14.8    # L/h
EVEROLIMUS_Q  <- 48      # L/h
EVEROLIMUS_KA <- 6.07    # 1/h, fixed

#' Everolimus (NDA 22-334 population model)
#'
#' Two compartments, apparent oral, whole blood; see the header of
#' \code{R/drugs_everolimus.R}.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
everolimus <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume = 1, legacyClearance = 1)
  default <- list(
    v1  = EVEROLIMUS_VC * size$volume,
    v2  = EVEROLIMUS_VP * size$volume,
    v3  = 1,
    cl1 = EVEROLIMUS_CL / 60 * size$clearance,
    cl2 = EVEROLIMUS_Q / 60 * size$clearance,
    cl3 = 0,
    ka_PO = EVEROLIMUS_KA / 60,
    bioavailability_PO = 1,          # apparent parameters
    tlag_PO = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "FDA Clinical Pharmacology and Pharmacometric review of NDA 22-334 ",
    "(everolimus, 2009), reviewer's population model in patients with ",
    "advanced solid tumours: two compartments, apparent oral, whole blood, ",
    "CL/F 14.8 L/h, Vc/F 172 L, Q/F 48 L/h, Vp/F 495 L, ka 6.07 /h; no ",
    "cyclosporine. https://www.accessdata.fda.gov/drugsatfda_docs/nda/2009/022334s000_ClinPharmR.pdf"
  )

  list(
    PK = PK,
    tPeak = 0,
    MEAC = 0,
    typical = 5.5,
    upperTypical = 8,
    lowerTypical = 3,
    reference = reference
  )
}
