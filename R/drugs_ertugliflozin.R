# -----------------------------------------------------------------------------
# Ertugliflozin: two compartments, oral
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in ng/mL.  Concentration only: urinary glucose excretion is
# not plotted.  Drafted by Claude Code, 2026-10-17, at the request of Steven
# L. Shafer; see the antidiabetic registry,
# inst/extdata/antidiabeticRegistry.csv.
#
# SOURCE
# ======
# Fediuk DJ et al.  Ertugliflozin population pharmacokinetic model in an
# expanded population.  Clin Pharmacol Drug Dev 2021 (PMC9291861), Table 2,
# data set 1; the model is that of Fediuk 2020 (Clin Pharmacol Drug Dev
# 2020;9:954-965, PMC8359437) re-estimated on more data.  "2-compartment model
# with first-order absorption and a lag time and first-order elimination",
# weight allometry "fixed to 0.75 and 1.0 for apparent clearance and volume"
# on all four disposition parameters.  Reference: a 65-year-old healthy
# man, 85 kg, eGFR 90 mL/min/1.73 m^2 (MDRD, values above 120 set to 120),
# fasted.
#
#     CL/F = 11.9 L/h x (WT/85)^0.75 x (eGFR/90)^0.458 x 0.921 (T2DM)
#            x 0.960 (women)
#     Vc/F = 6.51 L   x (WT/85) x (AGE/65)^-0.192 x 1.46 (women)
#     Q/F  = 7.76 L/h x (WT/85)^0.75
#     Vp/F = 107 L    x (WT/85)
#     ka   = 0.329 /h, lag 0.226 h (fasted)
#
# The equations are the supplement's (Fediuk 2021 supplementary appendix,
# and Fediuk 2020's, which has the same form): continuous covariates as
# powers of the covariate over its reference, categorical ones as
# theta^indicator.
#
# TYPE 2 DIABETES IS APPLIED: the app has no diabetes field, and the drug is
# given only to people with type 2 diabetes, so the curve is a patient's
# (x0.921 on CL/F), not the healthy reference subject's.  Left out:
# East/South-East Asian race (x1.17 on CL/F, x2.48 on Vc/F; no race field)
# and food (x0.725 on ka and F 7% lower when fed; the curve is fasted).  The
# age term is held at 18 years below 18: the source studied adults.
#
# RENAL FUNCTION: MDRD eGFR (egfrMDRD(), R/renalFunction.R) at the patient's
# creatinine (a child's on the adult scale) or an assumed normal creatinine
# for age and sex, capped at 120 as in the source.
#
# BODY SIZE: the model has its own weight covariate, evaluated at the
# pharmacokinetic weight with the switch on and total weight with it off.
#
# CHECKS (70 kg man, assumed creatinine): terminal half-life about 16 h
# (Fediuk 2020: mean terminal half-life 16.6 h in patients with type 2
# diabetes and normal renal function, 15.3 h in healthy subjects).
#
# References
# ----------
# Fediuk DJ et al., Clin Pharmacol Drug Dev 2021.
#   https://doi.org/10.1002/cpdd.970
# Fediuk DJ et al., Clin Pharmacol Drug Dev 2020;9:954-965.
#   https://doi.org/10.1002/cpdd.885
# -----------------------------------------------------------------------------

# Clearance in type 2 diabetes relative to the healthy reference (Table 2)
ERTUGLIFLOZIN_CL_T2DM <- 0.921

#' Ertugliflozin pharmacokinetics (oral)
#'
#' Two compartments, lagged first-order absorption (Fediuk 2021); see the
#' header of the file.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
ertugliflozin <- function(weight, height, age, sex, adjustToFFM = TRUE,
                          creatinine = NULL)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)
  pkW  <- if (isTRUE(adjustToFFM)) size$pkWeight else weight
  egfr <- min(120, egfrMDRD(age, sex, adultEquivalentCreatinine(creatinine, age, sex)))
  female <- sex == SEX_FEMALE

  default <- list(
    v1  = 6.51 * (pkW / 85) * (max(age, 18) / 65)^-0.192 * (if (female) 1.46 else 1),
    v2  = 107 * (pkW / 85), v3 = 1,
    cl1 = 11.9 * (pkW / 85)^0.75 * (egfr / 90)^0.458 * ERTUGLIFLOZIN_CL_T2DM *
      (if (female) 0.960 else 1) / 60,
    cl2 = 7.76 * (pkW / 85)^0.75 / 60, cl3 = 0,
    ka_PO = 0.329 / 60,
    bioavailability_PO = 1,   # apparent parameters
    tlag_PO = 0.226 * 60
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  list(
    PK = PK, tPeak = 0, MEAC = 0,
    typical = 0, upperTypical = 0, lowerTypical = 0,
    reference = paste0(
      "Fediuk 2021 (data set 1): two compartments, CL/F 11.9 L/h x (weight/85)^0.75 ",
      "x (MDRD eGFR/90)^0.458 from the entered or an assumed normal creatinine x ",
      "0.921 (type 2 diabetes) x 0.96 (women), Vc/F 6.51 L x (age/65)^-0.192 ",
      "x 1.46 (women), Q/F 7.76 L/h, Vp/F 107 L, ka 0.329/h, lag 0.226 h, fasted; ",
      "concentration only. https://doi.org/10.1002/cpdd.970"
    )
  )
}
