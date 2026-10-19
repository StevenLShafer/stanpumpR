# -----------------------------------------------------------------------------
# Metformin: one compartment, oral immediate release
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in mcg/mL (= mg/L).  Concentration only: the source's
# glucose-response model is not plotted.  Drafted by Claude Code, 2026-10-17,
# at the request of Steven L. Shafer; see the antidiabetic registry,
# inst/extdata/antidiabeticRegistry.csv.
#
# SOURCE
# ======
# Chae JW, Baek IH, Lee BY, Cho SK, Kwon KI.  Population PK/PD analysis of
# metformin using the signal transduction model.  Br J Clin Pharmacol
# 2012;74:815-823 (PMC3495146).  42 healthy Korean adults (age 21-31,
# 61-78 kg, CLcr 90-123 mL/min; Table 1), one 500 mg tablet.  "A one
# compartment body model with first order absorption and elimination", no
# lag; Table 2 and the final-model equations (full text, supplied by Steven
# L. Shafer):
#
#     CL/F = 52.6 L/h x (CLcr / 106.5)^0.782
#     V/F  = 113 L
#     ka   = 0.41 /h
#
# Table 2 prints V/F's relative standard error as 56.6%, but the text gives
# 4.18% and the bootstrap interval (100 to 126 L) agrees with the text.
#
# CLcr in mL/min.  The paper does not say how it was obtained; the model
# uses Cockcroft-Gault, the usual choice, at the patient's creatinine (a
# child's on the adult scale) or, with the field blank, an assumed normal
# creatinine for age and sex (R/renalFunction.R).  The subjects' CLcr ran
# from 90 to 123 mL/min only, so in renal impairment the power term is an
# extrapolation; it is not capped, because metformin's clearance is mostly
# renal and falls with renal function.
#
# NOT EXTENDED-RELEASE.  The extended-release tablets are a different input
# (a PAGE 2017 abstract, not a complete model) and are not offered; ER doses
# are not this drug.
#
# PHARMACODYNAMICS NOT PLOTTED.  Chae's signal-transduction glucose response
# (Emax 19.8, EC50 3.68 mcg/mL, Hill 0.547, transit 0.5 h) describes the
# single-dose glucose change in healthy adults; it is an endpoint of its own,
# not an effect site, and the app has no glucose output.
#
# BODY SIZE: Cockcroft-Gault at the pharmacokinetic weight with the switch on
# and at total weight with it off; V/F by the library factor
# (legacyVolume = 1).
#
# CHECKS (70 kg, 40 y man, assumed creatinine 0.9 mg/dL)
# =======================================================
# CLcr 108 mL/min, CL/F 53 L/h, apparent half-life 1.5 h, peak about 2.5 h
# after a dose.
#
# PREGNANCY
# =========
# Liao 2020 (Obstetric-Fetal Pharmacology Research Network, steady state,
# non-compartmental) compared pregnant women with gestational diabetes on
# metformin 500 mg (24 on metformin alone, 30 with glyburide) with 24
# nonpregnant women with type 2 diabetes: at 500 mg "significantly higher
# metformin apparent oral clearance (CL/F) (27%), weight-adjusted renal
# secretion clearance (64%), and apparent oral volume of distribution beta
# (V/F) (33%) were seen during pregnancy".  Bioavailability rose too, so
# CL/F rose less than CL (and at 1000 mg the rise in F cancelled the change
# in CL/F).  Eyal 2010 (35 pregnant women, 16 restudied postpartum) found
# renal clearance 625 +/- 130 mL/min in late pregnancy against 477 +/- 132
# postpartum (625 / 477 = 1.31), consistent with 1.27 for a drug cleared
# almost entirely unchanged in the urine.  Charles 2006, a third-trimester
# population model compared with other studies only, concluded there was no
# difference; it is not used.
#
# With the Patient Profile's Pregnant field on, CL/F is multiplied by
# METFORMIN_PREGNANCY_CL_FACTOR = 1.27 and V/F by METFORMIN_PREGNANCY_V_FACTOR
# = 1.33 (Liao at 500 mg, the dose Chae's model was fitted at); ka is
# unchanged.  The Cockcroft-Gault term is NOT also raised by the library's
# rise in filtration: Liao's 1.27 already contains it, and 1.5^0.782 = 1.37
# on top of it would count it twice.  The factors are meant for the assumed
# normal (nonpregnant) creatinine of a blank field.  A pregnant woman's
# measured creatinine is low, and Cockcroft-Gault already raises CL/F from
# it, so with a measured creatinine entered the two together overstate the
# clearance; leave the field blank to use the pregnancy factor alone.
# Metformin is essentially unbound, so total and unbound move together.
# Liao's numbers are from the abstract.  Claude Code, 2026-10-19.  See
# R/pregnancy.R.
#
# References
# ----------
# Chae JW et al., Br J Clin Pharmacol 2012;74:815-823.
#   https://doi.org/10.1111/j.1365-2125.2012.04260.x
# Liao MZ, Flood Nichols SK, Ahmed M, et al.  Effects of pregnancy on the
#   pharmacokinetics of metformin.  Drug Metab Dispos 2020;48:264-271.
#   https://doi.org/10.1124/dmd.119.088435 (pregnancy)
# Eyal S, Easterling TR, Carr D, et al.  Pharmacokinetics of metformin during
#   pregnancy.  Drug Metab Dispos 2010;38:833-840.
#   https://doi.org/10.1124/dmd.109.031245 (pregnancy)
# Charles B, Norris R, Xiao X, Hague W.  Population pharmacokinetics of
#   metformin in late pregnancy.  Ther Drug Monit 2006;28:67-72.
#   https://doi.org/10.1097/01.ftd.0000184161.52573.0e (pregnancy, not used)
# -----------------------------------------------------------------------------

# Pregnancy (see the header).  Liao 2020 at 500 mg: CL/F +27%, V/F +33%.
METFORMIN_PREGNANCY_CL_FACTOR <- 1.27
METFORMIN_PREGNANCY_V_FACTOR  <- 1.33

#' Metformin pharmacokinetics (oral, immediate release)
#'
#' One compartment with a renal covariate (Chae 2012); see the header of the
#' file.
#'
#' @inheritParams cefazolin
#' @param pregnant when \code{TRUE}, apply the changes of late pregnancy:
#'   CL/F x 1.27 and V/F x 1.33 (see the file's header)
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
metformin <- function(weight, height, age, sex, adjustToFFM = TRUE,
                      creatinine = NULL, pregnant = FALSE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1)
  pkW  <- if (isTRUE(adjustToFFM)) size$pkWeight else weight
  crcl <- creatinineClearanceCG(pkW, age, sex,
                                adultEquivalentCreatinine(creatinine, age, sex))

  # Pregnancy (see the header): CL/F and V/F directly; the renal term is not
  # also raised.
  clPregnancy <- if (isTRUE(pregnant)) METFORMIN_PREGNANCY_CL_FACTOR else 1
  vPregnancy  <- if (isTRUE(pregnant)) METFORMIN_PREGNANCY_V_FACTOR else 1

  default <- list(
    v1  = 113 * size$volume * vPregnancy, v2 = 1, v3 = 1,
    cl1 = 52.6 * (crcl / 106.5)^0.782 / 60 * clPregnancy, cl2 = 0, cl3 = 0,
    ka_PO = 0.41 / 60,
    bioavailability_PO = 1,   # apparent CL/F and V/F
    tlag_PO = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Chae 2012 (42 healthy adults, 500 mg): one compartment, CL/F 52.6 L/h x ",
    "(Cockcroft-Gault/106.5)^0.782 from the entered or an assumed normal ",
    "creatinine, V/F 113 L, ka 0.41/h; immediate release, concentration only. ",
    "https://doi.org/10.1111/j.1365-2125.2012.04260.x"
  )
  if (isTRUE(pregnant)) {
    reference <- paste0(reference, "; ", pregnancyReference(
      paste0("CL/F x ", METFORMIN_PREGNANCY_CL_FACTOR, ", V/F x ",
             METFORMIN_PREGNANCY_V_FACTOR),
      paste0("Liao MZ et al., Drug Metab Dispos 2020;48:264-271. ",
             "https://doi.org/10.1124/dmd.119.088435")))
  }

  list(
    PK = PK, tPeak = 0, MEAC = 0,
    typical = 0, upperTypical = 0, lowerTypical = 0,
    reference = reference
  )
}
