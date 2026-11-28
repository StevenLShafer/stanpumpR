# -----------------------------------------------------------------------------
# Imipenem (Patel 2022): two compartments, Cockcroft-Gault creatinine clearance
# and weight on clearance, weight on the central volume, and the patient's
# infection
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min; amounts in
# mg, so the plotted concentration is mcg/mL (= mg/L), total plasma imipenem.
#
# Drafted by Claude Code, 2026-11-25, at the request of Steven L. Shafer
# ("Let's also move on with antibiotics", 2026-11-25), from the library
# expansion plan (docs/library-expansion/plan.csv, row imipenem).  Every
# number below was read on the PDF of the source and its supplement, both
# supplied by Shafer on 2026-11-25.
#
# THE PRODUCT AND THE DOSE
# ========================
# Imipenem is given with cilastatin, which inhibits its renal metabolism by
# dehydropeptidase I and is not drawn (it changes imipenem's exposure and has
# no antibacterial effect; docs/library-expansion/README.md, "Which moieties a
# combination product draws").  Both US products state their doses in
# milligrams of imipenem (anhydrous): PRIMAXIN "500 mg imipenem (anhydrous
# equivalent) and 500 mg cilastatin" per vial, and RECARBRIO "1.25 grams
# (imipenem 500 mg, cilastatin 500 mg, relebactam 250 mg)", its pediatric
# 37.5 mg/kg providing "15 mg/kg imipenem".  So the drug is imipenem, dosed in
# mg of imipenem as PRIMAXIN is ordered.  RECARBRIO is also a combination
# product of its own, dosed in its own grams and drawing imipenem at 0.4 of
# them (R/combination-products.R; Shafer's decision of 2026-11-28, "Go with
# your recommendations").  The source's doses
# are of imipenem, so no salt conversion is made.
#
# THE SOURCE
# ==========
# Patel M, Bellanti F, Daryani NM, Noormohamed N, Hilbert DW, Young K,
# Kulkarni P, Copalu W, Gheyas F, Rizk ML.  Population pharmacokinetic/
# pharmacodynamic assessment of imipenem/cilastatin/relebactam in patients
# with hospital-acquired/ventilator-associated bacterial pneumonia.  Clin
# Transl Sci 2022;15:396-408 (doi 10.1111/cts.13158; PMID 34704389), and its
# Supplementary Information (Tables S1-S6).  Eleven studies, 1,197 adults
# (Table 2): age 18-96 years (median 55), weight 27-180 kg (median 75), CrCl
# 8-452 mL/min (median 106) by Cockcroft-Gault, 733 men and 464 women; 231
# healthy, 308 cIAI, 380 cUTI, 278 pneumonia (139 non-ventilated HABP, 30
# ventilated HABP, 92 VABP of the 261 in PN014).  NONMEM.  Two compartments.
# Table 3, with the footnote's unrounded values:
#
#     CL = 12.68 x (CrCl/105.5)^0.48 x (WT/75)^0.29 x (1 - 0.38 Pneumonia)  L/h
#     V1 = 11.39 x (WT/75)^1.03 x (1 - 0.39 Pneumonia) x (1 + 0.23 Ventilation) L
#     V2 = 7.79 L,  Q = 23.07 L/h
#
# The reference is healthy participants and patients with cIAI or cUTI
# combined (Table S4, footnote e); ventilation acts within pneumonia
# (footnote f).  Table 3's footnote prints the ventilation term as
# (1 + (Flag - 0.23 Ventilation)); Table 3 itself gives +0.23 with a 95% CI
# of 0.02 to 0.45, so the footnote's minus is a misprint.  The final control
# stream ("finalmodel.txt", an object embedded in the Word version of the
# supplement, CTS-15-396-s001.docx, supplied by Shafer on 2026-11-26; it did
# not survive into the supplement's PDF) confirms the structure: CL_IP =
# theta(1) x (WT/75)^theta(17) x (CRCL/105.5)^theta(9) x (1 + theta(10) for
# pneumonia), V1_IP = theta(2) x (WT/75)^theta(18) x (1 + theta(13) for
# pneumonia) x (1 + theta(16) for ventilation, initial estimate +0.215, so
# the footnote's minus is a misprint), V2 and Q with no covariate, and the
# reference the healthy, cIAI and cUTI groups ("Healthy/IAI/UTI").  CRCL is a
# column of the data set, computed before the fit, so the stream applies no
# cap and does not say which weight entered Cockcroft-Gault.  Its $THETA
# values are initial estimates; the final ones are Table 3's.  Race was
# tested and not retained.
#
# THE INFECTION (R/infection.R)
# =============================
# The Patient Profile's categories map onto the source's as follows (put to
# Shafer 2026-11-25, accepted by him 2026-11-28):
#     none, cUTI, cIAI     the reference
#     pneumonia            CL x 0.62, V1 x 0.61
#     pneumoniaVentilated  CL x 0.62, V1 x 0.61 x 1.23
#     other                the reference: the source fitted no other
#                          infection
# Pneumonia LOWERS clearance and the central volume together, so an infected
# patient's exposure is higher than the default (None) shows: the paper's
# healthy participants had 0.62 times the AUC of its pneumonia patients.
#
# RENAL FUNCTION
# ==============
# CrCl is Cockcroft-Gault (creatinineClearanceCG()) on the adult-equivalent
# creatinine (adultEquivalentCreatinine(); a blank creatinine is the assumed
# normal for age and sex), at the pharmacokinetic weight with the fat-free-
# mass switch on and at total body weight with it off.  No cap is applied:
# the source fitted CrCl to 452 mL/min and its control stream caps none.  Fitted in adults and
# offered at every age, as the library's other adult antibiotic models are;
# a child is put on the adult creatinine scale.  The power 0.48 takes
# clearance to zero only with the creatinine clearance; a patient on dialysis
# is not described (end-stage renal disease was 0.4% of the data).
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# The source's own weight terms on CL and V1 are evaluated at the
# pharmacokinetic weight with the switch on and at total body weight with it
# off, and take nothing more.  V2 and Q have no size term in the source, so
# with the switch on they take the library's volume and clearance factors
# (Shafer, 2026-11-19: every parameter is scaled), and with it off they are
# as published.
#
# BODY SIZE ANCHOR: V2 and Q are anchored at the reference man, not at
# Patel's cohort.  Table 2 prints the cohort's median weight (75 kg), age (55
# years) and sexes but no height or BMI, and the cohort is multinational
# (79% White, 10% Japanese), so its typical height would have to come from
# NCD-RisC's heights for several countries, which could not be reached from
# the session that built this model.
#
# PREGNANCY
# =========
# See the registry row.  Imipenem is cleared mainly by the kidney (with
# cilastatin, "approximately 63%" of a dose unchanged in urine, by filtration
# and secretion; RECARBRIO label sec. 12.3), so the creatinine clearance takes
# the 50% rise in filtration of late pregnancy when the creatinine is blank
# (pregnancyGfrFactor()).  Heikkila 1992 measured more than that (clearance
# 1.85-fold per kg at term, abstract read), which is recorded, not applied.
#
# CHECKS (not used in the fit)
# ============================
# RECARBRIO label sec. 12.3, Table 7 (the population model's own steady-
# state means in patients with CLcr >= 90 mL/min): imipenem CL 14 L/h in
# cUTI/cIAI and 10.4 L/h in HABP/VABP; Vss 24.3 L in subjects.
# tests/testthat/test-drugs-imipenem.R compares them.
#
# THE MIC LINE AND THE BAND
# =========================
# Free drug, read from R/antibioticThresholds.R and R/antibioticMbc.R.  The
# MIC line is free imipenem at 2 mg/L, the source's breakpoint for its
# target attainment.  No band is drawn: no imipenem MBC source has been read
# (antibioticNoBands()).
#
# References:
# Patel M et al., Clin Transl Sci 2022;15:396-408, and its Supplementary
#   Information.  https://doi.org/10.1111/cts.13158
# PRIMAXIN (imipenem and cilastatin) label, NDA 050587, Merck, DailyMed
#   2026-10-23; RECARBRIO (imipenem, cilastatin and relebactam) label, NDA
#   212819, Merck, DailyMed 2026-10-23 (papers/labels/).
# Heikkila A, Renkonen OV, Erkkola R.  Antimicrob Agents Chemother
#   1992;36:2652-2655 (PMID 1482132; pregnancy).
# Ansari J, Carvalho B, Shafer SL, Flood P.  Anesth Analg 2016;122:786-804.
#   https://doi.org/10.1213/ANE.0000000000001143 (pregnancy)
# -----------------------------------------------------------------------------

# Patel 2022, Table 3 and its footnote (imipenem)
IMIPENEM_CL          <- 12.68   # L/h at CrCl 105.5 mL/min and 75 kg
IMIPENEM_CRCL_REF    <- 105.5   # mL/min
IMIPENEM_CRCL_EXP    <- 0.48
IMIPENEM_CL_WT_EXP   <- 0.29
IMIPENEM_V1          <- 11.39   # L at 75 kg
IMIPENEM_V1_WT_EXP   <- 1.03
IMIPENEM_V2          <- 7.79    # L
IMIPENEM_Q           <- 23.07   # L/h
IMIPENEM_WT_REF      <- 75      # kg
# Infection factors (Table 3), by the Patient Profile's categories (see the
# header's THE INFECTION)
IMIPENEM_INFECTION_CL <- c(none = 1, cUTI = 1, cIAI = 1,
                           pneumonia = 1 - 0.38, pneumoniaVentilated = 1 - 0.38,
                           other = 1)
IMIPENEM_INFECTION_V1 <- c(none = 1, cUTI = 1, cIAI = 1,
                           pneumonia = 1 - 0.39,
                           pneumoniaVentilated = (1 - 0.39) * (1 + 0.23),
                           other = 1)

#' Imipenem (Patel 2022)
#'
#' Two compartments; clearance from the Cockcroft-Gault creatinine clearance,
#' weight and the infection; central volume from weight and the infection.
#' Doses are milligrams of imipenem, as PRIMAXIN is labelled; see the header
#' of R/drugs_imipenem.R.
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
imipenem <- function(weight, height, age, sex, adjustToFFM = TRUE,
                     creatinine = NULL, infection = INFECTION_DEFAULT,
                     pregnant = FALSE)
{
  infection <- checkInfection(infection)
  # Size (see the header's BODY SIZE): the source's weight terms and the
  # Cockcroft-Gault weight read pkW; V2 and Q take the library's factors with
  # the switch on and are as published with it off.
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume = 1, legacyClearance = 1)
  pkW  <- if (isTRUE(adjustToFFM)) size$pkWeight else weight

  scr  <- adultEquivalentCreatinine(creatinine, age, sex)
  gfrPregnancy <- pregnancyGfrFactor(pregnant, creatinine)
  crcl <- creatinineClearanceCG(pkW, age, sex, scr) * gfrPregnancy   # mL/min

  cl <- IMIPENEM_CL * (crcl / IMIPENEM_CRCL_REF)^IMIPENEM_CRCL_EXP *
    (pkW / IMIPENEM_WT_REF)^IMIPENEM_CL_WT_EXP *
    IMIPENEM_INFECTION_CL[[infection]]                               # L/h
  v1 <- IMIPENEM_V1 * (pkW / IMIPENEM_WT_REF)^IMIPENEM_V1_WT_EXP *
    IMIPENEM_INFECTION_V1[[infection]]

  default <- list(
    v1  = v1,
    v2  = IMIPENEM_V2 * size$volume,
    v3  = 1,
    cl1 = cl / 60,
    cl2 = IMIPENEM_Q / 60 * size$clearance,
    cl3 = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  creatinineNote <- if (is.null(creatinine))
    paste0("assumed normal serum creatinine ", format(signif(scr, 3)), " mg/dL")
  else
    paste0("serum creatinine ", format(creatinine), " mg/dL")
  reference <- paste0(
    "Patel M, Bellanti F, Daryani NM, et al., Clin Transl Sci 2022;15:396-408, ",
    "Table 3: two compartments, CL = 12.68 L/h x (CrCl/105.5)^0.48 x (WT/75)^0.29, ",
    "V1 11.39 L x (WT/75)^1.03, V2 7.79 L, Q 23.07 L/h; Cockcroft-Gault CrCl ",
    sprintf("%.0f", crcl), " mL/min, ", creatinineNote, "; infection: ",
    infectionLabel(infection),
    if (infection != INFECTION_NONE)
      sprintf(" (CL x %s, V1 x %s)", format(IMIPENEM_INFECTION_CL[[infection]]),
              format(signif(IMIPENEM_INFECTION_V1[[infection]], 4))) else "",
    ". Doses as mg of imipenem (PRIMAXIN; RECARBRIO 1.25 g = 500 mg imipenem); ",
    "cilastatin not drawn. https://doi.org/10.1111/cts.13158")
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
