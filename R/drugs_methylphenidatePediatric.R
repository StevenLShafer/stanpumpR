# -----------------------------------------------------------------------------
# Methylphenidate in children (Shader 1999): total methylphenidate after
# immediate-release tablets
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min.
#
# Drafted by Claude Code, 2026-10-11, at the request of Steven L. Shafer.
#
# WHAT IS PLOTTED
# ===============
# Plasma methylphenidate as the source measured it: TOTAL methylphenidate
# (d + l), by a non-chiral LC/MS/MS assay, after racemic immediate-release
# methylphenidate entered as the tablet strength (mg PO, or mg/kg PO).  This
# is a different analyte from the adult entry (R/drugs_methylphenidate.R),
# which plots d-methylphenidate, so the two are separate drugs and their
# concentrations must not be compared as if they were the same quantity.
#
# SOURCE
# ======
# Shader RI, Harmatz JS, Oesterheld JR, Parmelee DX, Sallee FR, Greenblatt DJ.
# Population pharmacokinetics of methylphenidate in children with attention-
# deficit hyperactivity disorder.  J Clin Pharmacol 1999;39:775-785
# (doi 10.1177/00912709922008425).  273 children and adolescents, 5-18 y
# (mean 11.1), good responders on stable twice- (n = 109) or three-times-
# daily (n = 164) IR methylphenidate, one plasma sample each (16 had a
# second), all assumed at steady state.  Unweighted nonlinear regression
# (SAS PROC NLIN, naive pooled), one compartment, first-order absorption and
# elimination, clearance proportional to body weight.  The equations are
# printed in full in the paper's appendix.
#
#   KA   = 1.192 /h  (absorption half-life 34.9 min).  Estimable only in the
#          bid subgroup, then FIXED for the whole data set.
#   BETA = 0.154 /h  (half-life 4.5 h; 95% CI 3.1-8.1 h; RSE 22.7%)
#   CLKG = 90.7 mL/min/kg apparent oral clearance (95% CI 74.6-106.7; RSE 9%)
#
# The appendix's F1 = BETA x KA / (CLKG x WTKG x (KA - BETA)) fixes the
# volume as V/F = CL/F / BETA = 35.3 L/kg; it is derived, not separately
# estimated.  No lag; the dose is the racemic mass with F = 1 on the apparent
# scale (the appendix applies no bioavailability).
#
# DOSE BASIS
# ==========
# Racemic mg in, total methylphenidate out, as fitted: bioavailability_PO = 1
# because the apparent scale carries any incomplete absorption.  Nothing is
# converted.
#
# ORAL ONLY, IMMEDIATE RELEASE ONLY
# =================================
# Apparent oral parameters; no intravenous unit is offered.  The children took
# IR tablets.  No extended-release input is attached: Concerta's input was
# fitted on the adult d-MPH entry and is not transferred to this one.
#
# COVARIATES
# ==========
# Weight is the model's own covariate (clearance, and so volume, linear in
# body weight).  As for the other self-scaled models (docs/weight-adjustment.md)
# it is evaluated at the pharmacokinetic weight with the switch on
# (size$pkWeight) and at total body weight with it off, which reproduces the
# published model exactly.  Sex: Shader found boys and girls similar
# (CLKG 91.6 and 86.7 mL/min/kg) and kept one model; age entered only through
# weight.
#
# WHAT TO BE CAREFUL ABOUT
# ========================
# The 4.5 h half-life is longer than the 2.0-3.5 h of single-dose studies of
# IR methylphenidate, as Shader says: with one sample per child the terminal
# phase was thinly sampled, so BETA is the least certain parameter (CI
# 3.1-8.1 h), and the model probably over-predicts late concentrations and
# accumulation.  The fit explained 43% of the variance in concentrations.
# There is no interindividual variability: the method was naive pooled.
# Validity: children 5-18 y on IR tablets; adults are an extrapolation, and
# the adult entry is the better model for them.
#
# CHECK
# =====
# Shader's repeat-sample subgroup averaged 9.6 ng/mL.  The model's average
# steady-state concentration on the tid cohort's mean 0.360 mg/kg x 3 a day
# is 1.08 mg/kg/day / (5.442 L/h/kg x 24 h) = 8.3 ng/mL.
#
# NO EFFECT SITE, NO THERAPEUTIC BAND
# ===================================
# As for the adult entry: no calibrated concentration-effect model, and no
# concentration is a therapeutic threshold.  tPeak, MEAC and the band are 0.
# -----------------------------------------------------------------------------

SHADER_KA   <- 1.192   # /h, fixed from the bid subgroup
SHADER_BETA <- 0.154   # /h, elimination rate constant
SHADER_CLKG <- 90.7    # mL/min/kg, apparent oral clearance

#' Methylphenidate in children (total methylphenidate)
#'
#' Plasma total (d + l) methylphenidate after racemic immediate-release
#' methylphenidate in children and adolescents with ADHD (Shader 1999). One
#' compartment, clearance proportional to weight. Oral only.
#'
#' @param weight weight in kg
#' @param height height in cm (used only for fat-free mass)
#' @param age age in years (used only for fat-free mass)
#' @param sex sex as a string (used only for fat-free mass)
#' @param adjustToFFM evaluate the weight covariate at the pharmacokinetic
#'   weight (TRUE) or at total body weight (FALSE)
#'
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
methylphenidatePediatric <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  # Own weight covariate (header): pharmacokinetic weight with the switch on,
  # total weight with it off.
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)
  pkW  <- if (isTRUE(adjustToFFM)) size$pkWeight else weight

  cl1 <- SHADER_CLKG / 1000 * pkW              # L/min, CL/F
  v1  <- cl1 / (SHADER_BETA / 60)              # L, V/F = CL/F / BETA
  v2  <- 1                                     # one compartment
  v3  <- 1
  cl2 <- 0
  cl3 <- 0

  ka_PO              <- SHADER_KA / 60         # 1/min
  bioavailability_PO <- 1                      # apparent scale (header)
  tlag_PO            <- 0

  default <- list(
    v1 = v1,
    v2 = v2,
    v3 = v3,
    cl1 = cl1,
    cl2 = cl2,
    cl3 = cl3,
    ka_PO = ka_PO,
    bioavailability_PO = bioavailability_PO,
    tlag_PO = tlag_PO
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Shader RI et al., J Clin Pharmacol 1999;39:775-785. ",
    "https://doi.org/10.1177/00912709922008425 (total methylphenidate after ",
    "racemic IR tablets in 273 children 5-18 y; one compartment, ka fixed at ",
    "1.192/h, CL/F 90.7 mL/min/kg proportional to weight, t1/2 4.5 h)"
  )

  return(
    list(
      PK = PK,
      tPeak = 0,     # no calibrated effect model (header)
      MEAC = 0,
      typical = 0,   # no therapeutic band (header)
      upperTypical = 0,
      lowerTypical = 0,
      reference = reference
    )
  )
}
