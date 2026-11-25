# -----------------------------------------------------------------------------
# Tenecteplase: two compartments, intravenous bolus (TNKase)
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min, amounts in mg,
# concentrations in mcg/mL (mg/L).  Concentration only: no fibrinogen,
# plasminogen or clot-lysis endpoint is plotted.  Drafted by Claude Code,
# 2026-11-25, at the request of Steven L. Shafer (docs/library-expansion/
# plan.csv, Biologics (surgery)); digest card papers/digests/PMID_36278839.md.
#
# SOURCE
# ======
# Tang F, Langenhorst J, Dang S, Kassir N, Owen R, Purdon B, Magnusson MO,
# Deng R.  Population pharmacokinetics of tenecteplase in patients with acute
# myocardial infarction and application of exposure-response analysis in
# patients with acute ischemic stroke.  J Clin Pharmacol 2023;63:197-209.
# 103 patients with acute myocardial infarction in the phase II TIMI 10B study,
# 30, 40 or 50 mg as one IV bolus, 785 concentrations by ELISA (LLOQ 8 ng/mL)
# to 360 min; median weight 81.8 kg, median creatinine clearance (Cockcroft-
# Gault normalised to 70 kg) 82.6 mL/min/70 kg.  Read on the PDF.
#
# Table 1 (final model, run 34) and Equation 9 (PDF pp. 5-6, printed 201-202):
#
#     CL = (91.3 mL/min + 0.322 x (CRCL-NM - 82.6)) x (WT/70)^0.75
#     Vc = 3610 mL x WT/70        Q  = 10.0 mL/min x (WT/70)^0.75
#     Vp =  914 mL x WT/70
#
# with CRCL-NM the Cockcroft-Gault creatinine clearance normalised to 70 kg,
# CG x 70/WT, in which the weight cancels: (140 - age) x 70 / (72 x SCr),
# x 0.85 in women.  The allometric exponents were fixed (run 13).  The ELISA
# also reads endogenous t-PA, which the model carried as a baseline signal of
# 18.6 ng/mL added to each prediction (Equation 2); it is not plotted.
#
# RENAL FUNCTION: Cockcroft-Gault at the entered serum creatinine (a child's
# on the adult scale, adultEquivalentCreatinine()) or, if blank, an assumed
# normal creatinine for age and sex (assumedCreatinine(): 0.9 mg/dL in men and
# 0.8 in women from 18).  The renal term is linear and cannot reach zero: at
# a creatinine clearance of 0, CL is 64.7 mL/min x (WT/70)^0.75.  The label's
# "Liver metabolism is the major clearance mechanism" predates this analysis,
# which puts the renal share at 29.2% for the median patient; the later
# finding is applied (AGENTS.md, a label is frozen at approval).
#
# BODY SIZE: the source's own weight terms, kept as fitted, evaluated at the
# pharmacokinetic weight with the switch on and at total weight with it off.
#
# PREGNANCY: no pharmacokinetic study (inst/extdata/pregnancyRegistry.csv).
# With the creatinine left blank, the renal term's creatinine clearance is
# multiplied by PREGNANCY_GFR_FACTOR (1.5, pregnancyGfrFactor()), the rise in
# filtration of pregnancy, physiological and unmeasured for this drug.
#
# CHECKS (Results, p. 203)
# ======
# CL 103 mL/min for the reference patient (81.8 kg, CRCL-NM 82.6): the paper's
# 103.  AUC = dose/CL at 81.8 kg: 0.1 mg/kg 79.4, 0.25 mg/kg 198.6, 0.5 mg/kg
# 397 and 50 mg 485 mcg.min/mL, against the paper's simulated medians 79.7,
# (145-256), 398 and 487.  The label (12.3): Vc 4.22-5.43 L, Vss 6.12-8.01 L,
# terminal half-life 90-130 min, CL 99-119 mL/min; the model at 81.8 kg gives
# CL 103 mL/min, Vc 4.22 L, Vss 5.29 L and a terminal half-life of 77 min.
# The clearances agree; the shorter half-life and smaller Vss are the kind of
# difference sampling to 6 h against the label's earlier analysis makes
# (AGENTS.md, comparing sources: only clearance is real), and are not corrected.
# External check (N1811s, acute ischaemic stroke, one sample at 1 h): the model
# over-predicted the observed means 1.39-fold, unexplained by renal function
# or weight (Results and Discussion, p. 207); not corrected.
#
# References
# ----------
# Tang F et al., J Clin Pharmacol 2023;63:197-209.
#   https://doi.org/10.1002/jcph.2164
# TNKASE (tenecteplase) prescribing information, BLA 103909, revised 2/2025.
# -----------------------------------------------------------------------------

TENECTEPLASE_CL       <- 91.3    # mL/min at 70 kg, CRCL-NM 82.6
TENECTEPLASE_CL_RENAL <- 0.322   # mL/min per mL/min/70 kg of CRCL-NM
TENECTEPLASE_CRCL_REF <- 82.6    # mL/min/70 kg
TENECTEPLASE_VC       <- 3610    # mL at 70 kg
TENECTEPLASE_Q        <- 10.0    # mL/min at 70 kg
TENECTEPLASE_VP       <- 914     # mL at 70 kg
TENECTEPLASE_WT_REF   <- 70

#' Tenecteplase pharmacokinetics (intravenous bolus)
#'
#' Two compartments with a creatinine-clearance term on clearance (Tang 2023);
#' see the header of the file.
#'
#' @inheritParams cefazolin
#' @param creatinine serum creatinine in mg/dL, or NULL for the assumed normal
#' @param pregnant is the patient pregnant?  \code{TRUE}, with the creatinine
#'   left blank, multiplies the creatinine clearance by the rise in glomerular
#'   filtration of pregnancy.
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
tenecteplase <- function(weight, height, age, sex, adjustToFFM = TRUE,
                         creatinine = NULL, pregnant = FALSE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)
  pkW  <- if (isTRUE(adjustToFFM)) size$pkWeight else weight
  w    <- pkW / TENECTEPLASE_WT_REF

  scr  <- adultEquivalentCreatinine(creatinine, age, sex)
  gfr  <- pregnancyGfrFactor(pregnant, creatinine)
  # Cockcroft-Gault normalised to 70 kg: the weight cancels
  crclNM <- creatinineClearanceCG(TENECTEPLASE_WT_REF, age, sex, scr) * gfr

  cl <- (TENECTEPLASE_CL + TENECTEPLASE_CL_RENAL * (crclNM - TENECTEPLASE_CRCL_REF)) * w^0.75

  default <- list(
    v1  = TENECTEPLASE_VC / 1000 * w, v2 = TENECTEPLASE_VP / 1000 * w, v3 = 1,
    cl1 = cl / 1000, cl2 = TENECTEPLASE_Q / 1000 * w^0.75, cl3 = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Tang 2023 (acute myocardial infarction, TIMI 10B): two compartments, CL (91.3 + ",
    "0.322 x (CrCl - 82.6)) x (weight/70)^0.75 mL/min with CrCl the Cockcroft-Gault ",
    "normalised to 70 kg (", round(crclNM, 1), " mL/min/70 kg, ",
    if (is.null(creatinine)) "assumed normal creatinine" else "entered creatinine",
    "), Vc 3.61 L and Vp 0.914 L x weight/70, Q 10.0 mL/min x (weight/70)^0.75. ",
    "Concentration only. https://doi.org/10.1002/jcph.2164")
  if (gfr != 1) reference <- paste0(reference, "; ", pregnancyReference(
    "creatinine clearance x 1.5, the rise in filtration of pregnancy (physiological)"))

  list(
    PK = PK, tPeak = 0, MEAC = 0,
    typical = 0, upperTypical = 0, lowerTypical = 0,
    reference = reference
  )
}
