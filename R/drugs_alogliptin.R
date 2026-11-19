# -----------------------------------------------------------------------------
# Alogliptin: two compartments, oral (Nesina)
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min, amounts in
# mg, concentrations in ng/mL.  Concentration only: the DPP-4 inhibition is
# not plotted.  Drafted by Claude Code, 2026-11-19, at the request of Steven
# L. Shafer; see the antidiabetic registry, inst/extdata/antidiabeticRegistry.csv.
#
# SOURCE
# ======
# Naik H, Czerniak R, Vakilynejad M.  Application of pharmacometric approaches
# to evaluate effect of weight and renal function on pharmacokinetics of
# alogliptin.  Br J Clin Pharmacol 2016;81:700-712 (PMID 26617339, read in
# full with its supplement).  491 subjects from two phase 1 studies (healthy
# volunteers, 12.5-400 mg; healthy volunteers and patients with mild,
# moderate and severe renal impairment and end-stage renal disease, 50 mg)
# and one phase 3 study (398 adults with type 2 diabetes, 12.5 and 25 mg).
# Two compartments, first-order absorption, apparent parameters (Table 3;
# Eq. 6 and 7):
#
#     CL/F  = 17.1 L/h x (CLcr / 72.95)^0.534 x (WT / 84.90)^0.345
#             (the coefficient is 9.61 L/h, not 17.1, below a CLcr of 30)
#     Vc/F  = 194 L x (WT / 84.90)^1.11
#     Vp/F  = 149 L       Q/F = 10.2 L/h       ka = 2.50 /h
#
# The paper's own values reproduce: CL/F 20.2, 16.7, 13.2 and 4.1 L/h at CLcr
# 100, 70, 45 and 15 mL/min (section 3.2; the model at 84.90 kg: 20.24,
# 16.72, 13.21, 4.13).
#
# THE STEP AT 30 mL/min.  Severe renal impairment has its own coefficient,
# "to account for the potential changes in non renal clearance", so CL/F
# falls by 44% between a CLcr just above 30 and just below it (10.6 against
# 6.0 L/h at 84.9 kg).  It is the published model and is reproduced, not
# smoothed (Steven L. Shafer's decision, 2026-11-19): a blend would be a model
# of our own.
#
# RENAL FUNCTION: Cockcroft-Gault.  Naik does not name the equation; the FDA
# Clinical Pharmacology reviews of Nesina (NDA 22-271, 2008-2012; read in full)
# do: the renal-impairment study grouped subjects by "the Cockcroft-Gault
# formula", total body weight and x 0.85 for women, and the sponsor's
# population analysis of the phase 3 study, whose median CLcr is the 72.95
# mL/min Naik
# normalises to, computed creatinine clearance from serum creatinine, weight,
# age and sex.  At the pharmacokinetic weight with the switch on and total
# weight with it off (as metformin's), at the entered creatinine or an
# assumed normal one for age and sex (adultEquivalentCreatinine()).  The
# renal term carries size and is not anchored.  The data reached end-stage
# renal disease.
#
# BODY SIZE: the source's own weight terms on CL/F and Vc/F, at the
# pharmacokinetic weight with the switch on and total weight with it off;
# Vp/F and Q/F have no covariate and take the library's fat-free-mass factors
# with the switch on (docs/weight-adjustment.md: parameters the source left
# without a size term), the published values with it off; a model with its
# own size covariate is not anchored at the cohort.  Age, sex, race and the
# concomitant medicines tested were not retained.
#
# DOSE BASIS: mg of alogliptin, as Nesina's strengths (6.25, 12.5 and 25 mg)
# are labelled; the tablet carries the benzoate.
#
# PREGNANCY: no human pharmacokinetics in pregnancy (PubMed searched
# 2026-11-19; a placental cell-model study, Bai 2026, PMID 42443801, measured
# permeability only).  About three quarters of a dose is excreted unchanged in
# urine, so the creatinine clearance takes the rise in glomerular filtration
# of pregnancy when the creatinine is left blank (pregnancyGfrFactor()), an
# assumption, as pramipexole's does.
#
# References
# ----------
# Naik H et al., Br J Clin Pharmacol 2016;81:700-712.
#   https://doi.org/10.1111/bcp.12853
# FDA, Clinical Pharmacology and Biopharmaceutics Review, NDA 22-271
#   (alogliptin), 2008-2012 (the file 022271Orig1s000ClinPharmR).
# -----------------------------------------------------------------------------

ALOGLIPTIN_CL        <- 17.1    # L/h at the reference CLcr and weight
ALOGLIPTIN_CL_SEVERE <- 9.61    # L/h, the coefficient below CLcr 30 mL/min
ALOGLIPTIN_SEVERE    <- 30      # mL/min
ALOGLIPTIN_CLCR_REF  <- 72.95   # mL/min, the median
ALOGLIPTIN_CLCR_EXP  <- 0.534
ALOGLIPTIN_WT_REF    <- 84.90   # kg, the median
ALOGLIPTIN_CL_WT_EXP <- 0.345
ALOGLIPTIN_VC        <- 194     # L
ALOGLIPTIN_VC_WT_EXP <- 1.11
ALOGLIPTIN_VP        <- 149     # L
ALOGLIPTIN_Q         <- 10.2    # L/h
ALOGLIPTIN_KA        <- 2.50    # /h

#' Alogliptin pharmacokinetics (oral)
#'
#' Two compartments with first-order absorption, creatinine clearance and
#' weight on CL/F and weight on Vc/F (Naik 2016); see the header of the file.
#'
#' @inheritParams cefazolin
#' @param creatinine serum creatinine, mg/dL, or NULL for an assumed normal
#'   value
#' @param pregnant when \code{TRUE} and the creatinine is blank, the creatinine
#'   clearance takes the rise in filtration of pregnancy (see the header)
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
alogliptin <- function(weight, height, age, sex, adjustToFFM = TRUE,
                       creatinine = NULL, pregnant = FALSE)
{
  # Vp/F and Q/F have no size term: the library's factors with the switch on,
  # the published values (factors of 1) with it off
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume = 1, legacyClearance = 1)
  pkW  <- if (isTRUE(adjustToFFM)) size$pkWeight else weight
  crcl <- creatinineClearanceCG(pkW, age, sex,
                                adultEquivalentCreatinine(creatinine, age, sex)) *
    pregnancyGfrFactor(pregnant, creatinine)
  theta <- if (crcl < ALOGLIPTIN_SEVERE) ALOGLIPTIN_CL_SEVERE else ALOGLIPTIN_CL
  cl <- theta * (crcl / ALOGLIPTIN_CLCR_REF)^ALOGLIPTIN_CLCR_EXP *
    (pkW / ALOGLIPTIN_WT_REF)^ALOGLIPTIN_CL_WT_EXP

  default <- list(
    v1  = ALOGLIPTIN_VC * (pkW / ALOGLIPTIN_WT_REF)^ALOGLIPTIN_VC_WT_EXP,
    v2  = ALOGLIPTIN_VP * size$volume, v3 = 1,
    cl1 = cl / 60, cl2 = ALOGLIPTIN_Q / 60 * size$clearance, cl3 = 0,
    ka_PO = ALOGLIPTIN_KA / 60,
    bioavailability_PO = 1,   # apparent parameters
    tlag_PO = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Naik 2016 (491 healthy volunteers, adults with renal impairment and adults ",
    "with type 2 diabetes): two compartments, CL/F 17.1 L/h x (Cockcroft-Gault/72.95)^0.534 ",
    "x (weight/84.9)^0.345 (coefficient 9.61 L/h below a creatinine clearance of 30 ",
    "mL/min), Vc/F 194 L x (weight/84.9)^1.11, Vp/F 149 L, Q/F 10.2 L/h, ka 2.50/h; ",
    "from the entered or an assumed normal creatinine; concentration only. ",
    "https://doi.org/10.1111/bcp.12853"
  )
  if (isTRUE(pregnant) && is.null(creatinine)) {
    reference <- paste0(reference, "; ", pregnancyReference(
      paste0("creatinine clearance x ", PREGNANCY_GFR_FACTOR,
             " (the rise in filtration; no alogliptin data)"),
      paste0("Ansari J et al., Anesth Analg 2016;122:786-804. ",
             "https://doi.org/10.1213/ANE.0000000000001143")))
  }

  list(
    PK = PK, tPeak = 0, MEAC = 0,
    typical = 0, upperTypical = 0, lowerTypical = 0,
    reference = reference
  )
}
