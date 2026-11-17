# -----------------------------------------------------------------------------
# Quinidine (Fattinger 1991): two compartments, apparent oral, zero-order
# absorption, clearance linear in creatinine clearance
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min; amounts in
# mg, so the plotted concentration is mg/L (mcg/mL) of quinidine base.
#
# Drafted by Claude Code, 2026-11-12, at the request of Steven L. Shafer, who
# asked for the common drugs with narrow therapeutic windows that the library
# lacked.  The numbers below were read on the page images of the source
# (PubMed Central's scan of PMC1368354: Methods, the creatinine clearance,
# Results and Table 1) and in the quinidine gluconate extended-release label
# (ANDA 089338, s036), read in full.
#
# THE SOURCE
# ==========
# Fattinger K, Vozeh S, Ha HR, Borner M, Follath F.  Population
# pharmacokinetics of quinidine.  Br J Clin Pharmacol 1991;31:279-286 (doi
# 10.1111/j.1365-2125.1991.tb05531.x).  60 patients with arrhythmias (46 men,
# 14 women; median 65.5 years, 28-82; median 70.5 kg, 45-105; creatinine
# clearance median 62.5 mL/min, 17 to over 100), 260 serum concentrations,
# mostly from routine monitoring, by an HPLC assay specific for quinidine;
# NONMEM; validated in 30 further patients.  Oral quinidine sulphate and slow-
# release bisulphate; "the doses were corrected for quinidine base using the
# following factors: 0.663 for quinidine bisulphate and 0.829 for quinidine
# sulphate".  "A two compartment model with zero order absorption from the
# gastrointestinal tract", parameters "assuming 100% bioavailability of oral
# quinidine sulphate".  Table 1:
#
#     CL renal     0.0566 x CLcr (mL/min), in L/h
#     CL nonrenal  12.6 L/h (6.8 in severe heart or liver failure)
#     V1 161 L     Q 12.6 L/h     V2 66.7 L
#     time to peak (the zero-order duration), sulphate 1.37 h
#
# with CLcr by the source's modified Cockcroft-Gault, (150 - age) x weight /
# serum creatinine (umol/L), plus 10% in men and minus 10% in women.  "No
# influence on the pharmacokinetics of quinidine was found for the following
# parameters: body weight, sex, nifedipine cotherapy and age."  For a patient
# without kidney disease or severe heart or liver failure the paper gives
# half-lives of 2.22 and 10.1 h and an oral clearance of 18.1 L/h at a
# creatinine clearance of 100 mL/min, which Table 1 reproduces.
#
# THE PRODUCT
# ===========
# Quinidine sulfate tablets (200 and 300 mg of the sulfate), dosed as the
# salt ("mg PO sulfate", quinidineSulfateUnits in R/constants.R) and
# converted at the source's 0.829, so a dose is entered as prescribed.  The
# zero-order absorption was fitted to the sulphate only, so the gluconate
# extended-release tablet (62.3% base, peak 3-5 h) and the bisulphate are not
# offered; intravenous quinidine gluconate is no longer marketed in the US.
# The parameters are apparent, relative to the sulphate (F 1): Holford 1981
# gives an absolute bioavailability of 70%, the label 70-80%.
#
# HELD AT THE REFERENCE
# =====================
# Severe heart or liver failure, which halves the nonrenal clearance, is
# taken as absent.  Alpha-1-acid glycoprotein, which binds quinidine and rises
# after surgery and infarction, and urine pH are not modelled.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# The source found no weight effect, so the nonrenal clearance and the
# volumes are written for the library's reference man and scaled by fat-free
# mass with the switch on, fixed with it off; the renal clearance carries
# weight through the creatinine clearance, evaluated at the pharmacokinetic
# weight with the switch on and at total weight with it off.
#
# RENAL FUNCTION AND PREGNANCY
# ============================
# A blank serum creatinine is an assumed normal value for age and sex
# (assumedCreatinine()), put on the adult scale for a child
# (adultEquivalentCreatinine()).  No pregnancy pharmacokinetics exist (the
# label: protein binding is lower in pregnancy); the creatinine clearance
# takes the 50% rise in filtration of late pregnancy when the serum
# creatinine is blank (pregnancyGfrFactor()), which raises the renal third
# of clearance.
#
# THE BAND
# ========
# The gluconate label: "A typical 'therapeutic' concentration range is 2 to
# 6 mg/L", by assays specific for quinidine (older assays read two to three
# times higher).  Fattinger: 2-5 mg/L (Follath 1983).
# -----------------------------------------------------------------------------

# Fattinger 1991, Table 1
QUINIDINE_CL_RENAL    <- 0.0566   # L/h per mL/min of creatinine clearance
QUINIDINE_CL_NONRENAL <- 12.6     # L/h
QUINIDINE_V1          <- 161      # L
QUINIDINE_Q           <- 12.6     # L/h
QUINIDINE_V2          <- 66.7     # L
QUINIDINE_TMAX        <- 1.37     # h, the zero-order duration (sulphate)
QUINIDINE_BASE_PER_SULFATE <- 0.829
CREATININE_UMOL_PER_MGDL   <- 88.4

#' Quinidine (Fattinger 1991)
#'
#' Two compartments, apparent oral (quinidine sulfate), zero-order
#' absorption, clearance linear in creatinine clearance; see the header of
#' \code{R/drugs_quinidine.R}.
#'
#' @inheritParams cefazolin
#' @param creatinine serum creatinine, mg/dL, or NULL for an assumed normal
#'   value
#' @param pregnant third trimester (TRUE) or not
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
quinidine <- function(weight, height, age, sex, adjustToFFM = TRUE,
                      creatinine = NULL, pregnant = FALSE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume = 1, legacyClearance = 1)
  pkW  <- if (isTRUE(adjustToFFM)) size$pkWeight else weight
  scr  <- adultEquivalentCreatinine(creatinine, age, sex)
  gfrPregnancy <- pregnancyGfrFactor(pregnant, creatinine)
  # The source's modified Cockcroft-Gault (Dettli 1983); age held below 150
  crcl <- (150 - min(age, 140)) * pkW / (scr * CREATININE_UMOL_PER_MGDL) *
    (if (sex == SEX_FEMALE) 0.9 else 1.1) * gfrPregnancy
  cl <- QUINIDINE_CL_NONRENAL * size$clearance + QUINIDINE_CL_RENAL * crcl

  default <- list(
    v1  = QUINIDINE_V1 * size$volume,
    v2  = QUINIDINE_V2 * size$volume,
    v3  = 1,
    cl1 = cl / 60,
    cl2 = QUINIDINE_Q / 60 * size$clearance,
    cl3 = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  creatinineNote <- if (is.null(creatinine))
    paste0("assumed normal serum creatinine ", format(scr), " mg/dL") else
    paste0("serum creatinine ", format(creatinine), " mg/dL")
  reference <- paste0(
    "Fattinger K et al., Br J Clin Pharmacol 1991;31:279-286: two ",
    "compartments, apparent oral (quinidine sulfate, doses as base x 0.829), ",
    "zero-order absorption over 1.37 h; CL/F 12.6 + 0.0566 x CLcr L/h, V1 161 ",
    "L, Q 12.6 L/h, V2 66.7 L; CLcr ", sprintf("%.0f", crcl), " mL/min, ",
    creatinineNote, "; no severe heart or liver failure. ",
    "https://doi.org/10.1111/j.1365-2125.1991.tb05531.x"
  )
  if (isTRUE(pregnant) && gfrPregnancy != 1) {
    reference <- paste0(reference, "; ", pregnancyReference(
      paste0("creatinine clearance x ", PREGNANCY_GFR_FACTOR,
             " (physiological rise in filtration)")))
  }

  list(
    PK = PK,
    tPeak = 0,
    MEAC = 0,
    typical = 4,
    upperTypical = 6,
    lowerTypical = 2,
    reference = reference,
    oralZeroOrder = list(duration = QUINIDINE_TMAX * 60, lag = 0),
    doseProducts = stats::setNames(QUINIDINE_BASE_PER_SULFATE, QUINIDINE_SALT)
  )
}
