# -----------------------------------------------------------------------------
# Ganciclovir, intravenous or as oral valganciclovir (Caldés 2009): two
# compartments, clearance proportional to creatinine clearance
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min; amounts in
# mg, so the plotted concentration is mg/L (mcg/mL).
#
# Drafted by Claude Code, 2026-11-12, at the request of Steven L. Shafer, who
# asked for the common drugs with narrow therapeutic windows that the library
# lacked.  The numbers below were read from the full text of the source (the
# PubMed Central page of PMC2772326: Methods, Table 2, Table 3 and its
# footnotes) and from the Cytovene-IV (2021) and Valcyte (2021) labels, read
# in full.
#
# THE SOURCE
# ==========
# Caldés A, Colom H, Armendariz Y, et al.  Population pharmacokinetics of
# ganciclovir after intravenous ganciclovir and oral valganciclovir
# administration in solid organ transplant patients infected with
# cytomegalovirus.  Antimicrob Agents Chemother 2009;53:4816-4824 (doi
# 10.1128/AAC.00085-09).  20 kidney, liver and heart transplant recipients
# with CMV infection (10 men, 10 women; Table 2: 66.2 kg, 55.7 years,
# Cockcroft-Gault creatinine clearance 57.0 mL/min, means), given ganciclovir
# 5 mg/kg over 1 h twice daily for 5 days and then valganciclovir 900 mg
# twice daily, both adjusted to renal function; serum sampled over a dosing
# interval on day 5 (intravenous) and day 15 (oral), 382 concentrations,
# HPLC, NONMEM VI.  "A two-compartment open linear model with first-order
# absorption process and lag time".  Table 3, final model:
#
#     CL   7.49 L/h x (CLCR / 57), CLCR by Cockcroft-Gault in mL/min
#     V1   31.90 L          CLD (Q)  10.20 L/h       V2  32.0 L
#     ka   0.895 /h         F        0.825           lag 0.382 h
#
# "No pharmacokinetic parameters were statistically related to body weight"
# (weight explained 4.7% of the variability in clearance, which creatinine
# clearance, itself scaled by weight, already carries).
#
# THE DOSE BASIS
# ==============
# Intravenous ganciclovir is labelled and dosed as ganciclovir (Cytovene-IV:
# "500 mg of ganciclovir" per vial, as the sodium salt).  Valganciclovir is
# labelled and dosed as valganciclovir (Valcyte: "496.3 mg of valganciclovir
# HCl (corresponding to 450 mg of valganciclovir)"); the source converted it
# "by multiplying the valganciclovir dose by 0.720 (corresponding to the
# ratio between the molecular weights of ganciclovir and valganciclovir)",
# which this model's `doseProducts` block does (valganciclovirUnits in
# R/constants.R), so a dose is entered as prescribed.
#
# BIOAVAILABILITY
# ===============
# The label's, 0.594, not the source's 0.825 (AGENTS.md: bioavailability is
# read from a crossover, never carried over from an exposure fit).  Valcyte's
# Table 10 gives the absolute bioavailability of ganciclovir from
# valganciclovir tablets taken with food as 59.4 +/- 6.1%, a within-subject
# comparison with intravenous ganciclovir.  The source's F compared an
# intravenous profile on day 5 with an oral one on day 15, between which
# renal function and clearance can change; with it the model overpredicts the
# label's single-dose exposure in renal impairment (Valcyte Table 13, 900 mg,
# creatinine clearance 51-70 mL/min: AUC 49.5 mg.h/L; this model at 60
# mL/min, 67.8 with F 0.825 and 48.9 with 0.594).  ka and the lag are the
# source's.
#
# CHECKED AGAINST THE CYTOVENE LABEL (not used in the fit)
# ========================================================
# "Systemic clearance ... 3.52 +/- 0.80 mL/min/kg" (14.8 L/h at 70 kg; this
# model, 14.9 at the reference man's Cockcroft-Gault 113 mL/min), Vss 0.74
# L/kg (this model 63.9 L, 0.91 L/kg), half-life 3.5 +/- 0.9 h (this model's
# terminal half-life at 113 mL/min, 4.4 h).  Table 7, by renal function:
# clearance 128, 57 and 30 mL/min at creatinine clearances 50-79, 25-49 and
# under 25; this model 142, 81 and 33 at 65, 37 and 15.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# Clearance is the source's, through the Cockcroft-Gault creatinine
# clearance, evaluated at the pharmacokinetic weight with the fat-free-mass
# switch on and at total body weight with it off (the source used total
# weight).  The volumes and intercompartmental clearance, which the source
# did not scale, are written for the library's reference man and scaled by
# fat-free mass with the switch on, fixed with it off.
#
# RENAL FUNCTION
# ==============
# A blank serum creatinine is an assumed normal value for age and sex
# (assumedCreatinine(); 0.9 mg/dL in a man, 0.8 in a woman), put on the adult
# scale for a child (adultEquivalentCreatinine()).  The source excluded
# creatinine clearances under 10 mL/min; dialysis is not modelled.
#
# PREGNANCY
# =========
# No pregnancy pharmacokinetics exist; ganciclovir is cleared unchanged by the
# kidney, so the creatinine clearance takes the 50% rise in filtration of late
# pregnancy when the serum creatinine is blank (pregnancyGfrFactor()).
#
# THE BAND
# ========
# None: no concentration range is established.  The ESICM/ESCMID position
# paper (Abdul-Aziz 2020, Intensive Care Med 46:1127-1153, read in full)
# proposes an AUC of 40-60 mg.h/L, or a trough above 2 mg/L, "if TDM is
# performed", and neither recommends nor discourages monitoring.
# -----------------------------------------------------------------------------

# Caldés 2009, Table 3
GANCICLOVIR_CL       <- 7.49     # L/h at a creatinine clearance of 57 mL/min
GANCICLOVIR_CLCR_REF <- 57       # mL/min, the population mean
GANCICLOVIR_V1       <- 31.90    # L
GANCICLOVIR_Q        <- 10.20    # L/h
GANCICLOVIR_V2       <- 32.0     # L
GANCICLOVIR_KA       <- 0.895    # 1/h
GANCICLOVIR_LAG      <- 0.382    # h
# Valcyte label (2021), Table 10: tablets with food
GANCICLOVIR_F_VALGANCICLOVIR <- 0.594
# Caldés 2009, Methods: ganciclovir per valganciclovir, by molecular weight
GANCICLOVIR_PER_VALGANCICLOVIR <- 0.720

#' Ganciclovir (Caldés 2009), intravenous or as oral valganciclovir
#'
#' Two compartments, clearance proportional to the Cockcroft-Gault creatinine
#' clearance; see the header of \code{R/drugs_ganciclovir.R}.
#'
#' @inheritParams cefazolin
#' @param creatinine serum creatinine, mg/dL, or NULL for an assumed normal
#'   value
#' @param pregnant third trimester (TRUE) or not
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
ganciclovir <- function(weight, height, age, sex, adjustToFFM = TRUE,
                        creatinine = NULL, pregnant = FALSE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume = 1, legacyClearance = 1)
  pkW  <- if (isTRUE(adjustToFFM)) size$pkWeight else weight
  scr  <- adultEquivalentCreatinine(creatinine, age, sex)
  gfrPregnancy <- pregnancyGfrFactor(pregnant, creatinine)
  crcl <- creatinineClearanceCG(pkW, age, sex, scr) * gfrPregnancy

  default <- list(
    v1  = GANCICLOVIR_V1 * size$volume,
    v2  = GANCICLOVIR_V2 * size$volume,
    v3  = 1,
    cl1 = GANCICLOVIR_CL / 60 * crcl / GANCICLOVIR_CLCR_REF,
    cl2 = GANCICLOVIR_Q / 60 * size$clearance,
    cl3 = 0,
    ka_PO = GANCICLOVIR_KA / 60,
    bioavailability_PO = GANCICLOVIR_F_VALGANCICLOVIR,
    tlag_PO = GANCICLOVIR_LAG * 60
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  creatinineNote <- if (is.null(creatinine))
    paste0("assumed normal serum creatinine ", format(scr), " mg/dL") else
    paste0("serum creatinine ", format(creatinine), " mg/dL")
  reference <- paste0(
    "Caldés A et al., Antimicrob Agents Chemother 2009;53:4816-4824: two ",
    "compartments, CL 7.49 L/h x (Cockcroft-Gault CLCR/57), V1 31.9 L, Q 10.2 ",
    "L/h, V2 32.0 L; valganciclovir as ganciclovir x 0.720, ka 0.895 /h, lag ",
    "0.382 h, F 0.594 (Valcyte label); CLCR ", sprintf("%.0f", crcl),
    " mL/min, ", creatinineNote, ". https://doi.org/10.1128/AAC.00085-09"
  )
  if (isTRUE(pregnant) && gfrPregnancy != 1) {
    reference <- paste0(reference, "; ", pregnancyReference(
      paste0("creatinine clearance x ", PREGNANCY_GFR_FACTOR,
             " (the rise in filtration of late pregnancy)"),
      "Ansari 2016"))
  }

  list(
    PK = PK,
    tPeak = 0,
    MEAC = 0,
    typical = 0,
    upperTypical = 0,
    lowerTypical = 0,
    reference = reference,
    doseProducts = stats::setNames(GANCICLOVIR_PER_VALGANCICLOVIR,
                                   VALGANCICLOVIR_PRODUCT)
  )
}
