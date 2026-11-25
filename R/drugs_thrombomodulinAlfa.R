# -----------------------------------------------------------------------------
# Thrombomodulin alfa (ART-123): one compartment, intravenous
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min, amounts in mg,
# concentrations in ng/mL.  Concentration only: no protein C activation,
# coagulation marker or DIC score is plotted.  Drafted by Claude Code,
# 2026-11-25, at the request of Steven L. Shafer (docs/library-expansion/
# plan.csv, Biologics (sepsis)); digest card papers/digests/PMID_27140801.md.
#
# Recombinant human soluble thrombomodulin.  Not approved in the United States
# (approved in Japan as Recomodulin); the phase III SCARLET trial (ART-123 in
# sepsis-associated coagulopathy) is the context the model was built for.
#
# SOURCE
# ======
# Mouksassi MS, Marier JF, Bax L, Osawa Y, Tsuruta K.  Population
# pharmacokinetic analysis of thrombomodulin alfa to support dosing rationale
# in patients with renal impairment.  Clin Pharmacol Drug Dev 2015;4:210-217.
# 392 subjects: 24 healthy volunteers (0.02 and 0.06 mg/kg, rich sampling to
# 144 h) and 368 adults with sepsis and suspected DIC in the phase IIb study
# (0.06 mg/kg/day, maximum 6 mg, as an IV bolus or a 15-min infusion for six
# days, sparse sampling); 18-93 years, 30-150 kg, creatinine clearance 5-150
# mL/min (Table 1).  ELISA, LLOQ 1 ng/mL.  Read on the PDF; Table 2's
# equations on the page image (PDF p. 6, printed 215):
#
#     CL = 0.14 L/h x (WT/68)^0.56 x (CRCL/72.6)^0.27
#     V  = 5.47 L   x (WT/68)^0.50
#     t1/2 = 27.08 h
#
# One compartment, chosen over two (Supplemental Table S1).  CRCL is the
# Cockcroft-Gault creatinine clearance in mL/min, truncated at 150 (Methods).
#
# RENAL FUNCTION: Cockcroft-Gault at total weight, at the entered serum
# creatinine (a child's on the adult scale, adultEquivalentCreatinine()) or,
# if blank, an assumed normal creatinine for age and sex (assumedCreatinine():
# 0.9 mg/dL in men and 0.8 in women from 18).  Held within the studied 5-150
# mL/min: the power of CRCL reaches zero clearance at no renal function, which
# the drug's measured non-renal clearance (26-46% of a dose not recovered in
# urine) rules out.
#
# BODY SIZE: the source's own weight terms, kept as fitted, about its 68 kg
# median, evaluated at the pharmacokinetic weight with the switch on and at
# total weight with it off.  The Cockcroft-Gault weight is always total weight,
# as the equation was fitted.
#
# PREGNANCY: no pharmacokinetic study (inst/extdata/pregnancyRegistry.csv).
# With the creatinine left blank, the creatinine clearance is multiplied by
# PREGNANCY_GFR_FACTOR (1.5, pregnancyGfrFactor()), the rise in filtration of
# pregnancy, before the 150 mL/min hold: physiological, unmeasured for this
# drug, 54-74% of which is excreted in urine.
#
# NO BAND: the paper estimates a "therapeutic range of the product",
# 300-5,400 ng/mL (Introduction, p. 211): the effective concentration from
# non-clinical and clinical data, and the highest concentration without
# bleeding in non-clinical toxicology.  No study has titrated a dose to it,
# and the library draws no clinical threshold on an anticoagulant (the INR and
# ACT references excepted, Shafer 2026-10-27), so it is recorded in
# inst/extdata/therapeuticRanges.csv and on the help page, not drawn.
#
# CHECKS (Results and Table 2)
# ======
# t1/2 = ln 2 x 5.47 / 0.14 = 27.08 h, the paper's.  Typical CL at CrCL 105,
# 75, 45 and 22 mL/min: 0.155, 0.141, 0.123 and 0.101 L/h at 68 kg against the
# paper's 0.158, 0.145, 0.128 and 0.105 (its typical values are medians over
# the simulated weights).
#
# References
# ----------
# Mouksassi MS et al., Clin Pharmacol Drug Dev 2015;4:210-217.
#   https://doi.org/10.1002/cpdd.163
# -----------------------------------------------------------------------------

THROMBOMODULIN_CL       <- 0.14    # L/h at 68 kg, CrCL 72.6 mL/min
THROMBOMODULIN_V        <- 5.47    # L at 68 kg
THROMBOMODULIN_CL_WT    <- 0.56
THROMBOMODULIN_V_WT     <- 0.50
THROMBOMODULIN_CL_CRCL  <- 0.27
THROMBOMODULIN_WT_REF   <- 68      # kg
THROMBOMODULIN_CRCL_REF <- 72.6    # mL/min
THROMBOMODULIN_CRCL_MIN <- 5       # mL/min, Table 1
THROMBOMODULIN_CRCL_MAX <- 150     # mL/min, the source's truncation

#' Thrombomodulin alfa pharmacokinetics (intravenous)
#'
#' One compartment with weight and Cockcroft-Gault creatinine clearance on
#' clearance (Mouksassi 2015); see the header of the file.
#'
#' @inheritParams cefazolin
#' @param creatinine serum creatinine in mg/dL, or NULL for the assumed normal
#' @param pregnant is the patient pregnant?  \code{TRUE}, with the creatinine
#'   left blank, multiplies the creatinine clearance by the rise in glomerular
#'   filtration of pregnancy.
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
thrombomodulinAlfa <- function(weight, height, age, sex, adjustToFFM = TRUE,
                               creatinine = NULL, pregnant = FALSE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)
  pkW  <- if (isTRUE(adjustToFFM)) size$pkWeight else weight
  w    <- pkW / THROMBOMODULIN_WT_REF

  scr  <- adultEquivalentCreatinine(creatinine, age, sex)
  gfr  <- pregnancyGfrFactor(pregnant, creatinine)
  crcl <- creatinineClearanceCG(weight, age, sex, scr) * gfr
  crcl <- min(max(crcl, THROMBOMODULIN_CRCL_MIN), THROMBOMODULIN_CRCL_MAX)

  cl <- THROMBOMODULIN_CL * w^THROMBOMODULIN_CL_WT *
    (crcl / THROMBOMODULIN_CRCL_REF)^THROMBOMODULIN_CL_CRCL
  v  <- THROMBOMODULIN_V * w^THROMBOMODULIN_V_WT

  default <- list(
    v1  = v, v2 = 1, v3 = 1,
    cl1 = cl / 60, cl2 = 0, cl3 = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Mouksassi 2015 (sepsis with suspected DIC, and healthy volunteers): one ",
    "compartment, CL 0.14 L/h x (weight/68)^0.56 x (CrCl/72.6)^0.27, V 5.47 L x ",
    "(weight/68)^0.50, CrCl Cockcroft-Gault (", round(crcl, 1), " mL/min, ",
    if (is.null(creatinine)) "assumed normal creatinine" else "entered creatinine",
    ", held within 5-150). Concentration only. https://doi.org/10.1002/cpdd.163")
  if (gfr != 1) reference <- paste0(reference, "; ", pregnancyReference(
    "creatinine clearance x 1.5, the rise in filtration of pregnancy (physiological)"))

  list(
    PK = PK, tPeak = 0, MEAC = 0,
    typical = 0, upperTypical = 0, lowerTypical = 0,   # no band (header)
    reference = reference
  )
}
