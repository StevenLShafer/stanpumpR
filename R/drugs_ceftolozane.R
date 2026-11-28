# -----------------------------------------------------------------------------
# Ceftolozane, given as Zerbaxa (ceftolozane/tazobactam; Zhang 2021): two
# compartments, Cockcroft-Gault creatinine clearance on clearance, weight on
# both volumes, and the patient's infection
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min; amounts in
# mg, so the plotted concentration is mcg/mL (= mg/L), total plasma
# ceftolozane.
#
# Drafted by Claude Code, 2026-11-25, at the request of Steven L. Shafer
# ("Let's also move on with antibiotics", 2026-11-25), from the library
# expansion plan (docs/library-expansion/plan.csv, row Zerbaxa).  Every
# number below was read on the PDF of the source (Table 1, Table 2 and its
# printed equations, Methods and Discussion).
#
# A COMPONENT OF ZERBAXA
# ======================
# Steven L. Shafer, 2026-11-18: the product is named Zerbaxa ("Use Zerbaxa
# and Avycaz too", after naming Zosyn), dosed in its label's units, and its
# curve is ceftolozane; tazobactam, a beta-lactamase inhibitor, is not drawn
# (2026-11-24, docs/library-expansion/README.md, "Which moieties a
# combination product draws").  Ceftolozane is given in the United States
# only as Zerbaxa, so this model has no dose units of its own and no
# Category: a Zerbaxa dose reaches it as 2/3 of the product's weight, in the
# same unit (R/combination-products.R, COMBINATION_PRODUCTS$Zerbaxa).  The
# label's 1.5 g is 1 g ceftolozane and 0.5 g tazobactam; its pediatric 30
# mg/kg "provides 20 mg/kg ceftolozane and 10 mg/kg tazobactam".  The label's
# grams are of the base (each vial holds "ceftolozane 1 g (equivalent to
# 1.147 g ceftolozane sulfate)"), as the source's doses are, so no salt
# conversion is made.
#
# THE SOURCE
# ==========
# Zhang Z, Patel YT, Fiedler-Kelly J, Feng HP, Bruno CJ, Gao W.  Population
# pharmacokinetic analysis for plasma and epithelial lining fluid
# ceftolozane/tazobactam concentrations in patients with ventilated
# nosocomial pneumonia.  J Clin Pharmacol 2021;61:254-268 (doi
# 10.1002/jcph.1733; PMID 32949031).  Sixteen studies, 968 adults 18-98
# years (mean 53.2) and 33.5-173 kg (mean 74.7), 583 men and 385 women:
# 277 healthy participants (with renal impairment but no infection), 176
# with cUTI, 174 with cIAI, 331 with pneumonia (305 from ASPECT-NP, all
# ventilated) and 10 "other infection" (the critically ill augmented-renal-
# clearance cohort of MK-7625A-007); CrCL 6.3-531.3 mL/min by Cockcroft-
# Gault (Table 1).  NONMEM.  Two compartments with first-order elimination.
# Table 2 and its equations, the final adult plasma model:
#
#     CL = 4.84 x (CrCL/100)^0.701 x 1.18^cUTI x 1.43^cIAI x 0.32^ESRD   L/h
#     Vc = 9.23 x (WTKG/70)^0.684 x 1.25^cUTI x 1.59^cIAI x 2.00^Pneumonia
#               x 2.14^OINF x 1.30^ESRD                                  L
#     Q  = 3.13 L/h
#     Vp = 4.78 x (WTKG/70)^0.484                                        L
#
# the reference being a healthy participant.  Race was tested and not
# retained; weight on clearance was not identified, "likely because body
# weight was partially accounted for in the Cockcroft-Gault equation".
#
# WHY THIS SOURCE
# ===============
# The largest ceftolozane fit (nlmixr2lib files it as Zhang_2021_ceftolozane;
# a lead, its code not used) and the only one with every infection the
# product is given for, including the ventilated pneumonia of the 3 g dose.
#
# THE INFECTION (R/infection.R)
# =============================
# This is the first model to read the Patient Profile's Infection.  Its
# categories map onto Zhang's as follows [decision for Shafer, put to him
# 2026-11-25; docs/library-expansion/plan.csv, row Zerbaxa]:
#     none                 the healthy reference
#     cUTI, cIAI           Zhang's own factors
#     pneumonia            Zhang's pneumonia factor (Vc x 2.00); every one
#                          of his pneumonia patients was ventilated, so for a
#                          patient who is not ventilated this extrapolates
#     pneumoniaVentilated  Zhang's pneumonia factor
#     other                Zhang's "other infection" (Vc x 2.14), fitted in
#                          10 critically ill patients with augmented renal
#                          clearance
# The default, "none", gives the healthy volunteer's curve (Shafer,
# 2026-11-24); an infected patient's central volume is 1.25 to 2.14 times
# larger, so with the default the plot overstates the peak an infected
# patient reaches.  The help page says so.
#
# END-STAGE RENAL DISEASE
# =======================
# Zhang's ESRD terms (CL x 0.320 on top of the CrCL term, Vc x 1.30) are
# not applied: the Patient Profile has no dialysis or ESRD field, and the
# Cockcroft-Gault term alone over-predicts clearance in ESRD (Zhang:
# "the Cockcroft-Gault equation may overestimate CrCL in ESRD").  The help
# page says that a patient on dialysis is not described by this curve.
#
# RENAL FUNCTION
# ==============
# CrCL is Cockcroft-Gault (creatinineClearanceCG()) on the adult-equivalent
# creatinine (adultEquivalentCreatinine(); a blank creatinine is the assumed
# normal for age and sex), at the pharmacokinetic weight with the fat-free-
# mass switch on and at total body weight with it off.  The source prints no
# cap and fitted CrCL up to 531 mL/min, so none is applied.  Fitted in adults
# only and offered at every age, as the library's other adult antibiotic
# models are; a child is put on the adult creatinine scale.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# The source's own weight terms on Vc and Vp are evaluated at the
# pharmacokinetic weight with the switch on and at total body weight with it
# off, and take nothing more; the renal clearance, with weight inside its
# Cockcroft-Gault, is never scaled.  Q has no size term in the source, so
# with the switch on it takes the library's clearance factor, (FFM /
# FFM_ref)^0.75 (Shafer, 2026-11-19: every parameter is scaled), and with it
# off it is as published.
#
# BODY SIZE ANCHOR: Q is anchored at the reference man, not at Zhang's
# cohort.  Table 1 prints the cohort's mean weight (74.7 kg), age (53.2
# years) and sexes (583 men, 385 women) but no height or BMI, and the cohort
# is multinational (66% White, 22% Japanese), so its typical height would
# have to come from NCD-RisC's heights for several countries, which could
# not be reached from the session that built this model.  A cohort factor
# would move Q by a few per cent; Q is the parameter Zhang flags as highly
# correlated with Vp (r >= 0.924).
#
# PREGNANCY
# =========
# No measured pharmacokinetics in pregnancy (see the registry row).
# Ceftolozane is cleared by filtration (label sec. 12.3: renal clearance
# "similar to the glomerular filtration rate for the unbound fraction"), so
# the creatinine clearance takes the 50% rise in filtration of late
# pregnancy when the creatinine is blank (pregnancyGfrFactor()).
#
# CHECKS (not used in the fit)
# ============================
# The Zerbaxa label, sec. 12.3: in healthy subjects after 1.5 g, plasma
# clearance 4.10-6.73 L/h and Vss 13.5 L; Table 10, at steady state on 1.5 g
# every 8 h in cIAI and cUTI, Cmax 65.7 mcg/mL and AUC0-8 186 mcg.h/mL.
# tests/testthat/test-drugs-ceftolozane.R reproduces these from the model.
#
# THE MIC LINE AND THE BAND
# =========================
# Free drug, read from R/antibioticThresholds.R and R/antibioticMbc.R.  The
# MIC line is free ceftolozane at 4 mg/L, the susceptible breakpoint for
# Pseudomonas aeruginosa.  No band is drawn: no MBC50 or MBC90 for
# ceftolozane has been found (antibioticNoBands()).  Steven L. Shafer decided
# on 2026-11-28 that Zerbaxa has no MBC band, as cefepime has none.
#
# References:
# Zhang Z et al., J Clin Pharmacol 2021;61:254-268.
#   https://doi.org/10.1002/jcph.1733
# Zerbaxa (ceftolozane and tazobactam) label, NDA 206829, Merck, DailyMed
#   2026-05-28 (papers/labels/NJ_label-zerbaxa_206829_2026-05-28.pdf).
# Ansari J, Carvalho B, Shafer SL, Flood P.  Anesth Analg 2016;122:786-804.
#   https://doi.org/10.1213/ANE.0000000000001143 (pregnancy)
# -----------------------------------------------------------------------------

# Zhang 2021, Table 2 (ceftolozane, adult plasma model)
CEFTOLOZANE_CL          <- 4.84    # L/h at CrCL 100 mL/min, healthy
CEFTOLOZANE_CRCL_REF    <- 100     # mL/min
CEFTOLOZANE_CRCL_EXP    <- 0.701
CEFTOLOZANE_VC          <- 9.23    # L at 70 kg, healthy
CEFTOLOZANE_VC_WT_EXP   <- 0.684
CEFTOLOZANE_Q           <- 3.13    # L/h
CEFTOLOZANE_VP          <- 4.78    # L at 70 kg
CEFTOLOZANE_VP_WT_EXP   <- 0.484
CEFTOLOZANE_WT_REF      <- 70      # kg
# Infection factors (Table 2), by the Patient Profile's categories (see the
# header's THE INFECTION)
CEFTOLOZANE_INFECTION_CL <- c(none = 1, cUTI = 1.18, cIAI = 1.43,
                              pneumonia = 1, pneumoniaVentilated = 1, other = 1)
CEFTOLOZANE_INFECTION_VC <- c(none = 1, cUTI = 1.25, cIAI = 1.59,
                              pneumonia = 2.00, pneumoniaVentilated = 2.00, other = 2.14)

#' Ceftolozane (Zhang 2021), a component of Zerbaxa
#'
#' Two compartments; clearance from the Cockcroft-Gault creatinine clearance
#' and the infection, central volume from weight and the infection,
#' peripheral volume from weight.  A component of Zerbaxa: see
#' R/combination-products.R and the header of R/drugs_ceftolozane.R.
#'
#' @inheritParams cefazolin
#' @param creatinine serum creatinine in mg/dL, or NULL for the assumed normal
#'   value for age and sex
#' @param infection the Patient Profile's infection, one of
#'   \code{INFECTION_VALUES}
#' @param pregnant is the patient pregnant?  \code{TRUE} raises the
#'   creatinine clearance by \code{PREGNANCY_GFR_FACTOR} when the creatinine
#'   is blank.
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
ceftolozane <- function(weight, height, age, sex, adjustToFFM = TRUE,
                        creatinine = NULL, infection = INFECTION_DEFAULT,
                        pregnant = FALSE)
{
  infection <- checkInfection(infection)
  # Size (see the header's BODY SIZE): the source's weight terms and the
  # Cockcroft-Gault weight read pkW; Q takes the clearance factor with the
  # switch on and is as published with it off.
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume = 1, legacyClearance = 1)
  pkW  <- if (isTRUE(adjustToFFM)) size$pkWeight else weight

  scr  <- adultEquivalentCreatinine(creatinine, age, sex)
  gfrPregnancy <- pregnancyGfrFactor(pregnant, creatinine)
  crcl <- creatinineClearanceCG(pkW, age, sex, scr) * gfrPregnancy   # mL/min

  cl <- CEFTOLOZANE_CL * (crcl / CEFTOLOZANE_CRCL_REF)^CEFTOLOZANE_CRCL_EXP *
    CEFTOLOZANE_INFECTION_CL[[infection]]                            # L/h
  vc <- CEFTOLOZANE_VC * (pkW / CEFTOLOZANE_WT_REF)^CEFTOLOZANE_VC_WT_EXP *
    CEFTOLOZANE_INFECTION_VC[[infection]]
  vp <- CEFTOLOZANE_VP * (pkW / CEFTOLOZANE_WT_REF)^CEFTOLOZANE_VP_WT_EXP

  default <- list(
    v1  = vc,
    v2  = vp,
    v3  = 1,
    cl1 = cl / 60,
    cl2 = CEFTOLOZANE_Q / 60 * size$clearance,
    cl3 = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  creatinineNote <- if (is.null(creatinine))
    paste0("assumed normal serum creatinine ", format(scr), " mg/dL")
  else
    paste0("serum creatinine ", format(creatinine), " mg/dL")
  reference <- paste0(
    "Zhang Z, Patel YT, Fiedler-Kelly J, et al., J Clin Pharmacol 2021;61:254-268, ",
    "Table 2: two compartments, CL = 4.84 L/h x (CrCL/100)^0.701, Vc 9.23 L x ",
    "(WT/70)^0.684, Q 3.13 L/h, Vp 4.78 L x (WT/70)^0.484; Cockcroft-Gault CrCL ",
    sprintf("%.0f", crcl), " mL/min, ", creatinineNote, "; infection: ",
    infectionLabel(infection),
    if (infection != INFECTION_NONE)
      sprintf(" (CL x %s, Vc x %s)", format(CEFTOLOZANE_INFECTION_CL[[infection]]),
              format(CEFTOLOZANE_INFECTION_VC[[infection]])) else "",
    "; end-stage renal disease not applied. Drawn for Zerbaxa (1.5 g = 1 g ",
    "ceftolozane + 0.5 g tazobactam); tazobactam not drawn. ",
    "https://doi.org/10.1002/jcph.1733")
  if (isTRUE(pregnant) && gfrPregnancy != 1)
    reference <- paste0(reference, "; ", pregnancyReference(
      paste0("creatinine clearance x ", PREGNANCY_GFR_FACTOR,
             " (physiological rise in filtration)")))

  list(
    PK = PK,
    tPeak = 0,
    MEAC = 0,
    typical = 0,
    upperTypical = 0,
    lowerTypical = 0,
    reference = reference
  )
}
