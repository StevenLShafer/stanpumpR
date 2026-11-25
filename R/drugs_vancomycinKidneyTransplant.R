# -----------------------------------------------------------------------------
# Vancomycin in kidney transplant recipients: one-compartment model with
# creatinine clearance and transplant status on clearance (Yu 2026)
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in mcg/mL (= mg/L), total serum.
#
# SOURCE
# ======
# Yu H, Park JM, Zeng D, Farkash EA, Zhu HJ.  Vancomycin pharmacokinetics and
# nephrotoxicity in kidney transplant recipients: a retrospective cohort
# study.  AAPS J 2026;28:172 (PMID 42963461), read in full from the PDF.
# Routine TDM concentrations (troughs and random samples, kinetic immunoassay)
# from 1539 hospitalised adults at the University of Michigan, 2007-2024: 469
# kidney transplant recipients (courses started > 30 days after the
# transplant; renal replacement therapy and multi-organ transplants excluded)
# and 1070 non-transplant patients frequency-matched on age, sex and weight.
# Monolix (SAEM).  Medians (Table I): KTR age 60.7 y, weight 84.5 kg, height
# 172.7 cm, serum creatinine 1.83 mg/dL, creatinine clearance 44.9 mL/min.
#
# THE MODEL (Table II, final model)
# =================================
#     CL = 4.32 x (CLcr / 100)^1.0 x exp(-0.28 x KT)   L/h
#     V  = 80.8                                       L
#
# KT = 1 for a transplant recipient, so this drug, which models only
# recipients, has CL = 4.32 x exp(-0.28) = 3.266 L/h at a CLcr of 100 mL/min,
# 24.4% below a non-transplant patient at the same CLcr.  The paper's own
# check, its typical CL at each group's median CLcr (1.5 vs 3.9 L/h), is
# reproduced: 4.32 x 0.449 x 0.756 = 1.47 and 4.32 x 0.910 = 3.93 L/h.
# A two-compartment model fitted worse (OFV +212.7) with 42-75% shrinkage on
# its distribution terms: TDM samples cannot identify a distribution phase,
# so the early post-infusion peak of this curve is less trustworthy than its
# troughs and its area.  Shrinkage on V was 34.9%; on CL 16.1%.
#
# CLcr is Cockcroft-Gault in mL/min, recomputed at each creatinine.  The
# weight it reads is the actual body weight, or, when that exceeds 120% of
# the Devine ideal body weight, the adjusted body weight
# IBW + 0.4 x (TBW - IBW); CLcr above 150 mL/min is truncated to 150.  No
# creatinine floor is described, so none is applied.
#
# External validation (MIMIC-IV, 479 patients, no re-estimation): the model
# OVERPREDICTED transplant recipients' concentrations (mean prediction error
# +16.8%, against +7.6% in non-transplant patients), so in another centre's
# recipients the transplant effect may be somewhat smaller than exp(-0.28).
#
# AGREEMENT WITH THE LIBRARY'S VANCOMYCIN (Thomson 2009)
# ======================================================
# Clearance is the parameter to compare.  Yu's NON-transplant clearance at a
# given CLcr agrees with Thomson's to within about 5% over 45-150 mL/min
# (4.32 x CLcr/100 against 2.99 x [1 + 0.0154 x (CLcr - 66)]: 1.94 vs 2.02
# L/h at 45, 3.93 vs 4.14 at 91, 6.48 vs 6.86 at 150), so the two sources
# agree where they overlap and the transplant factor is the new information.
# The volumes do not compare: Yu's one compartment (80.8 L) is a construct of
# trough-dominated data, Thomson's two (Vss 1.41 L/kg) of richer sampling.
#
# RENAL FUNCTION
# ==============
# The creatinine is the Patient Profile's serum creatinine on the adult scale
# (adultEquivalentCreatinine(), R/renalFunction.R).  When none is entered it
# is an ASSUMED NORMAL creatinine for the patient's age and sex, which is
# almost never right in a transplant recipient (the cohort's median was
# 1.83 mg/dL): enter it.  The model was fitted in adults only.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# The source carried no size term on V (body weight was tested and not
# retained) and carried weight into CL only through Cockcroft-Gault.  With
# the switch on, V takes the library's volume factor, and Cockcroft-Gault
# reads the pharmacokinetic weight (70 kg x FFM / FFM_ref), which already
# discounts excess fat, so the adjusted-body-weight rule is not applied a
# second time.  V is anchored at Yu's cohort (VANCOMYCIN_KT_COHORT, below:
# Table I's 469 recipients and 1070 non-transplant patients, both fitted, V
# having no transplant term; medians of about 84 kg, 172 cm and 60 years,
# about 64% men), whose typical patient sits at a volume factor of 1.0405
# from the 70 kg reference man, so a patient like Yu's receives the published
# 80.8 L and the reference man 77.7 L (Steven L. Shafer, 2026-11-18 and
# 2026-11-19; until 2026-11-20 the reference man received 80.8 L, the paper
# not being in the archive).  The renal term is not anchored and takes no
# size factor: its creatinine clearance has weight inside ("you would wind up
# double correcting clearance", Shafer, 2026-11-18).  With the switch off, V
# is 80.8 L for everyone and Cockcroft-Gault reads the published dosing
# weight (actual, or adjusted above 120% of the Devine IBW).  Where the
# Devine IBW is not positive (heights under about 100 cm, far outside the
# adult source) the actual weight is used.
#
# PREGNANCY
# =========
# No pregnant recipient is described.  As for the library's vancomycin, the
# physiological rise in filtration is applied to the creatinine clearance the
# clearance term reads (pregnancyGfrFactor(), x 1.5 only when the creatinine
# is blank), before the source's 150 mL/min truncation.
#
# TARGET, BAND AND THRESHOLD
# ==========================
# The same molecule as `vancomycin`, so the same free fraction (0.70), the
# same free-MIC threshold (1.4 mcg/mL total) and the same MBC band
# (antibioticMbcBand("vancomycin"), R/antibioticMbc.R).  The consensus target
# is an AUC24 of 400-600 mg.h/L (Rybak 2020), read off the curve.
#
# References
# ----------
# Yu H et al., AAPS J 2026;28:172.  https://doi.org/10.1208/s12248-026-01324-w
# Devine BJ.  Drug Intell Clin Pharm 1974;8:650-655 (ideal body weight).
# Rybak MJ et al., Am J Health Syst Pharm 2020;77:835-864.
# -----------------------------------------------------------------------------

# Yu 2026, Table II, final model
VANCOMYCIN_KT_CL     <- 4.32    # L/h at CLcr 100 mL/min, non-transplant
VANCOMYCIN_KT_THETA  <- -0.28   # transplant status on CL, exp(theta x KT)
VANCOMYCIN_KT_V      <- 80.8    # L
VANCOMYCIN_KT_CLCR_MAX <- 150   # mL/min, truncation in the source

# Yu 2026, Table I (p5): the two groups the model was fitted to, medians.
# Kidney transplant recipients: 469, 304 men (64.8%), 60.7 years, 172.7 cm,
# 84.5 kg; non-transplant patients: 1070, 676 men (63.2%), 59.2 years,
# 171.3 cm, 84.3 kg.  The heights are printed for each group, not by sex, so
# each group's height serves its men and women.  V, with no size term and no
# transplant term, is anchored at this cohort (Steven L. Shafer, 2026-11-18
# and 2026-11-19; see the header's BODY SIZE).
VANCOMYCIN_KT_COHORT <- list(groups = list(
  list(weight = 84.5, height = 172.7, age = 60.7, men = 304, women = 165),
  list(weight = 84.3, height = 171.3, age = 59.2, men = 676, women = 394)))

#' Vancomycin pharmacokinetics in kidney transplant recipients (Yu 2026)
#'
#' @inheritParams cefazolin
#' @param pregnant is the patient pregnant?  \code{TRUE} multiplies the
#'   creatinine clearance the clearance term reads by 1.5 (see the header).
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
vancomycinKidneyTransplant <- function(weight, height, age, sex,
                                       adjustToFFM = TRUE,
                                       creatinine = NULL, pregnant = FALSE)
{
  # No published size term on V, so it takes the library volume factor with
  # the switch on, anchored at Yu's cohort, and is fixed with it off (see the
  # header).
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume = 1, legacyClearance = 1,
                        cohort = do.call(cohortSizeFactors, VANCOMYCIN_KT_COHORT))

  # The weight Cockcroft-Gault reads
  if (isTRUE(adjustToFFM)) {
    crclWeight <- 70 * pkSizeFactors(weight, height, age, sex, TRUE)$volume
  } else {
    ibw <- idealBodyWeightDevine(height, sex)   # R/drugs_metronidazole.R
    crclWeight <- if (ibw > 0 && weight > 1.2 * ibw) ibw + 0.4 * (weight - ibw) else weight
  }

  scr  <- adultEquivalentCreatinine(creatinine, age, sex)
  crcl <- creatinineClearanceCG(crclWeight, age, sex, scr)        # mL/min
  gfrPregnancy <- pregnancyGfrFactor(pregnant, creatinine)
  crcl <- min(crcl * gfrPregnancy, VANCOMYCIN_KT_CLCR_MAX)

  cl1 <- VANCOMYCIN_KT_CL * (crcl / 100) * exp(VANCOMYCIN_KT_THETA) / 60  # L/min
  v1  <- VANCOMYCIN_KT_V * size$volume

  default <- list(
    v1 = v1,
    v2 = 1,
    v3 = 1,
    cl1 = cl1,
    cl2 = 0,
    cl3 = 0
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  tPeak <- 0     # no effect site: exposure against the MIC is the effect
  MEAC  <- 0

  # Same molecule, same band as vancomycin (R/antibioticMbc.R)
  band         <- antibioticMbcBand("vancomycin")$plotted
  typical      <- band[["typical"]]
  upperTypical <- band[["upper"]]
  lowerTypical <- band[["lower"]]

  reference <- paste0(
    "Yu H et al., AAPS J 2026;28:172. One-compartment total-serum model in ",
    "kidney transplant recipients: CL 4.32 L/h x (Cockcroft-Gault CrCL/100) ",
    "x exp(-0.28), V 80.8 L, from the entered creatinine or an assumed ",
    "normal one. https://doi.org/10.1208/s12248-026-01324-w"
  )
  if (gfrPregnancy != 1) {
    reference <- paste0(reference, "; ", pregnancyReference(
      paste0("creatinine clearance x ", PREGNANCY_GFR_FACTOR)))
  }

  return(
    list(
      PK = PK,
      tPeak = tPeak,
      MEAC = MEAC,
      typical = typical,
      upperTypical = upperTypical,
      lowerTypical = lowerTypical,
      reference = reference
    )
  )
}
