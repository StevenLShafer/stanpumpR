# -----------------------------------------------------------------------------
# Tobramycin (Conil 2011): two compartments, creatinine clearance and height
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in mcg/mL (= mg/L), total serum.
#
# Drafted by Claude Code, 2026-11-12, at the request of Steven L. Shafer, who
# asked for the common drugs with narrow therapeutic windows that the library
# lacked.  Every number below was read from the full text of the source in
# PubMed Central (PMC3018027: Methods, Table 1, Table 2 and the NONMEM code).
#
# THE SOURCE
# ==========
# Conil JM, Georges B, Ruiz S, et al.  Tobramycin disposition in ICU
# patients receiving a once daily regimen: population approach and dosage
# simulations.  Br J Clin Pharmacol 2011;71:61-71 (doi
# 10.1111/j.1365-2125.2010.03793.x).  49 adults in intensive care (age 61,
# weight 79 +/- 19 kg, height 172 +/- 9 cm, serum creatinine 84 +/- 37
# umol/L, Cockcroft-Gault creatinine clearance 108 +/- 55 mL/min) given
# 5 mg/kg over 30 min once daily; serum by PETINIA (LLOQ 0.3 mg/L); NONMEM,
# two compartments.  Final model, Table 2 and the control stream:
#
#     TVCL = 3.83 + 0.020 x (COCK - 94) + 0.052 x (HEIG - 172)   L/h
#     V1 = 25.50 L     Q = 4.74 L/h     V2 = 30.60 L
#
# with COCK the Cockcroft-Gault creatinine clearance in mL/min and HEIG the
# height in cm.
#
# WHY THIS SOURCE
# ===============
# Of the two tobramycin models read in full, it is the one whose clearance
# behaves like an aminoglycoside's, almost entirely filtration: 3.8 L/h at a
# creatinine clearance of 94 mL/min (5.6 L/h).  The other, Inclan 2005 (151
# general inpatients), puts an intercept of 3.07 L/h on clearance, a
# non-renal clearance tobramycin does not have, and gives 1.9 times the
# library's gentamicin clearance at the same filtration rate.  The cost is
# the population: these were critically ill, with the large volumes of
# intensive care (V1 25.5 L against gentamicin's 16.6 L in the ward), so the
# peak after a dose is lower than in a ward patient.  Hennig 2013 (732
# patients) would settle it and could not be read.
#
# THE FLOORS
# ==========
# The covariates add, so a short patient with no renal function would get a
# negative clearance.  Height is held at 150 cm or more (the study's mean
# less two and a half standard deviations) and the creatinine clearance at
# zero or more, which keeps clearance at 0.81 L/h or more.
#
# RENAL FUNCTION AND BODY SIZE (docs/weight-adjustment.md)
# =======================================================
# Cockcroft-Gault from the Patient Profile's serum creatinine through
# adultEquivalentCreatinine(); blank means the assumed normal for age and sex.
# The weight in it is the pharmacokinetic weight with the fat-free-mass
# switch on and total body weight with it off (the source does not say which
# weight its Cockcroft-Gault used).  The volumes and Q were fixed values in
# the source, so they are scaled by the library's factors with the switch on
# and fixed with it off.
#
# DOSE
# ====
# The injection is tobramycin sulfate labelled as its tobramycin content
# ("Each mL contains tobramycin sulfate equivalent to 40 mg tobramycin", the
# Tobramycin Injection USP labels as a search showed them; DailyMed is outside
# this session's network), so doses are mg of tobramycin.
# -----------------------------------------------------------------------------

# Conil 2011, Table 2 (final model)
TOBRAMYCIN_CL          <- 3.83    # L/h at Cockcroft-Gault 94 mL/min and 172 cm
TOBRAMYCIN_CL_PER_CRCL <- 0.020   # L/h per mL/min
TOBRAMYCIN_CL_PER_CM   <- 0.052   # L/h per cm
TOBRAMYCIN_CRCL_REF    <- 94
TOBRAMYCIN_HEIGHT_REF  <- 172
TOBRAMYCIN_HEIGHT_MIN  <- 150     # cm, the floor described above
TOBRAMYCIN_V1          <- 25.50   # L
TOBRAMYCIN_Q           <- 4.74    # L/h
TOBRAMYCIN_V2          <- 30.60   # L

#' Tobramycin (Conil 2011)
#'
#' Two compartments; clearance from Cockcroft-Gault creatinine clearance and
#' height.  See the header of \code{R/drugs_tobramycin.R}.
#'
#' @inheritParams cefazolin
#' @param creatinine serum creatinine in mg/dL, or NULL for the assumed normal
#'   value for age and sex
#' @param pregnant is the patient pregnant?  \code{TRUE} raises the
#'   creatinine clearance by \code{PREGNANCY_GFR_FACTOR} when the creatinine
#'   is blank.
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
tobramycin <- function(weight, height, age, sex, adjustToFFM = TRUE,
                       creatinine = NULL, pregnant = FALSE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume = 1, legacyClearance = 1)
  pkW  <- if (isTRUE(adjustToFFM)) size$pkWeight else weight
  scr  <- adultEquivalentCreatinine(creatinine, age, sex)
  gfrPregnancy <- pregnancyGfrFactor(pregnant, creatinine)
  crcl <- max(creatinineClearanceCG(pkW, age, sex, scr) * gfrPregnancy, 0)
  h <- max(height, TOBRAMYCIN_HEIGHT_MIN)
  cl <- TOBRAMYCIN_CL + TOBRAMYCIN_CL_PER_CRCL * (crcl - TOBRAMYCIN_CRCL_REF) +
    TOBRAMYCIN_CL_PER_CM * (h - TOBRAMYCIN_HEIGHT_REF)

  default <- list(
    v1  = TOBRAMYCIN_V1 * size$volume,
    v2  = TOBRAMYCIN_V2 * size$volume,
    v3  = 1,
    cl1 = cl / 60,
    cl2 = TOBRAMYCIN_Q / 60 * size$clearance,
    cl3 = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  creatinineNote <- if (is.null(creatinine))
    paste0("assumed normal serum creatinine ", format(scr), " mg/dL")
  else
    paste0("serum creatinine ", format(creatinine), " mg/dL")
  reference <- paste0(
    "Conil JM et al., Br J Clin Pharmacol 2011;71:61-71: two compartments in ",
    "49 ICU patients, CL 3.83 + 0.020 (Cockcroft-Gault CrCl - 94) + 0.052 ",
    "(height - 172) L/h, V1 25.5 L, Q 4.74 L/h, V2 30.6 L; ", creatinineNote,
    ". https://doi.org/10.1111/j.1365-2125.2010.03793.x")
  if (isTRUE(pregnant) && gfrPregnancy != 1)
    reference <- paste0(reference, "; ", pregnancyReference(
      paste0("creatinine clearance x ", PREGNANCY_GFR_FACTOR,
             " (physiological rise in filtration)")))

  # Band: free drug at the expected MBC of the common pathogens, placed on
  # the plotted curve (R/antibioticMbc.R).
  band <- antibioticMbcBand("tobramycin")$plotted
  list(
    PK = PK,
    tPeak = 0,
    MEAC = 0,
    typical = band[["typical"]],
    upperTypical = band[["upper"]],
    lowerTypical = band[["lower"]],
    reference = reference
  )
}
