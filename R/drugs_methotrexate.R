# -----------------------------------------------------------------------------
# Methotrexate, high-dose intravenous, in adults (Ibarra 2023): plasma
# methotrexate
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min; amounts in
# mg and concentrations in mcg/mL (= mg/L).  1 umol/L = 0.4544 mg/L
# (METHOTREXATE_MW, R/drugs_methotrexatePediatric.R).
#
# Drafted by Claude Code, 2026-11-06, at the request of Steven L. Shafer, as a
# first-wave chemotherapy drug (docs/chemotherapy.md,
# inst/extdata/chemotherapyRegistry.csv, source P12).  Every number below was
# read from the PDF, which Shafer supplied (2026-11-06): Table 1, Table 2, the
# covariate equation and the Methods.  The children's model is
# methotrexatePediatric (Taylor 2023).
#
# THE SOURCE
# ==========
# Ibarra M, Combs R, Taylor ZL, et al.  Insights from a pharmacometric
# analysis of HDMTX in adults with cancer: clinically relevant covariates for
# application in precision dosing.  Br J Clin Pharmacol 2023;89:660-671
# (doi 10.1111/bcp.15506).  693 courses in 243 adults, 18-88 years (median
# 60), 44-162 kg, 0.5-13.4 g/m2 over 3 to 24 h, from 101 US oncology
# practices; immunoassay; NONMEM, SAEM.
#
# THE MODEL (Table 2 and the covariate equation)
# ==============================================
# Three compartments, every parameter allometric in weight (the 3/4 power on
# the clearances, linear on the volumes, centred on 70 kg):
#
#     CL = 5.92 L/h x (WT/70)^0.75 x (SCRpop/SCR)^0.694 x (ALB/4)^0.594
#     V1 = 14.4 L,  Q2 = 0.571 L/h,  V2 = 3.55 L,  Q3 = 0.0365 L/h,  V3 = 2.09 L
#
# The paper's own check: these give half-lives of 1.46, 4.93 and 39.9 h and a
# Vss of 20.0 L, against its stated 1.5, 4.9 and 40 h and 20 L (the test
# repeats it), which also confirms the mammillary arrangement.
#
# SCRpop is not printed as a number.  The Methods centre creatinine "to the
# expected values reported by Junge et al." and, in the same paragraph,
# impute a missing creatinine as "the mean value for healthy adult subjects
# (0.92 mg/dL and 0.77 mg/dL for male and female patients)"; those are taken
# as SCRpop.  An inference, said so.
#
# ALBUMIN
# =======
# The Patient Profile's Serum albumin field (g/dL; added 2026-11-06 on
# Steven L. Shafer's decision) enters as (ALB/4)^0.594, the reference 4 g/dL
# (the paper writes "mg/dL", a slip).  Left blank (NULL) it is held at the
# reference, 4 g/dL, factor 1.  This matters: the cohort's median was 3.3
# g/dL and 68% of courses were hypoalbuminaemic; at 3.3 the clearance is 11%
# lower, at 2.5 g/dL 24% lower.  The albumin used is floored at MIN_ALBUMIN
# (0.5 g/dL, the field's lower bound) so the power stays finite and positive;
# an albumin outside the range the cohort covered is an extrapolation of the
# power.  The albumin was time-varying in the source; the curve holds
# the entered value.
#
# WHAT IS HELD AT THE REFERENCE
# =============================
# The creatinine was time-varying in the source; the curve holds the entered
# value.  Shorter infusions (12% lower clearance), a previous kidney injury (8%) and
# age were found and left out of the final model by the authors.
#
# RENAL FUNCTION
# ==============
# The Patient Profile's serum creatinine, through adultEquivalentCreatinine()
# (a model fitted in adults; a child's is put on the adult scale), or the
# library's assumed normal creatinine when blank: 0.9 or 0.8 mg/dL, which
# sit a little off the paper's 0.92 and 0.77, so a blank field gives a
# clearance 1.5% above (men) or 2.6% below (women) the paper's typical.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# The model carries its own weight covariate, evaluated as for cefazolin at
# the pharmacokinetic weight with the switch on and total weight with it off.
#
# PREGNANCY (R/pregnancy.R)
# =========================
# See methotrexatePediatric: no comparison found; nothing applied.
#
# NO EFFECT SITE, NO BAND
# =======================
# Plasma only; the rescue and discharge thresholds are not a band.
# -----------------------------------------------------------------------------

# Ibarra 2023, Table 2: clearances in L/h and volumes in L at 70 kg
METHOTREXATE_IBARRA <- list(
  cl = 5.92, v1 = 14.4, q2 = 0.571, v2 = 3.55, q3 = 0.0365, v3 = 2.09,
  betaSCR = 0.694, betaALB = 0.594,
  scrPop = c(male = 0.92, female = 0.77),   # mg/dL (the Methods; see header)
  albuminReference = 4                       # g/dL
)

#' Methotrexate, high-dose intravenous, in adults (Ibarra 2023)
#'
#' Three compartments allometric in weight, with the serum creatinine and the
#' serum albumin on clearance; see the header of
#' \code{R/drugs_methotrexate.R}.
#'
#' @inheritParams cefazolin
#' @param creatinine serum creatinine, mg/dL, or NULL for an assumed normal
#'   value
#' @param albumin serum albumin, g/dL, or NULL for the reference, 4 g/dL
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
methotrexate <- function(weight, height, age, sex, adjustToFFM = TRUE,
                         creatinine = NULL, albumin = NULL)
{
  p <- METHOTREXATE_IBARRA
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume = 1, legacyClearance = 1)
  w <- if (isTRUE(adjustToFFM)) size$pkWeight else weight
  scr <- adultEquivalentCreatinine(creatinine, age, sex)
  scrPop <- p$scrPop[[if (sex == SEX_MALE) "male" else "female"]]
  alb <- if (is.null(albumin) || is.na(albumin)) p$albuminReference else max(albumin, MIN_ALBUMIN)
  fCl <- (w / 70)^0.75
  fV <- w / 70

  default <- list(
    v1  = p$v1 * fV,
    v2  = p$v2 * fV,
    v3  = p$v3 * fV,
    cl1 = p$cl * fCl * (scrPop / scr)^p$betaSCR *
      (alb / p$albuminReference)^p$betaALB / 60,
    cl2 = p$q2 * fCl / 60,
    cl3 = p$q3 * fCl / 60
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Ibarra M, Combs R, Taylor ZL, et al. Insights from a pharmacometric analysis ",
    "of HDMTX in adults with cancer. Br J Clin Pharmacol 2023;89:660-671. 693 ",
    "courses in 243 adults: three compartments allometric in weight, CL 5.92 L/h x ",
    "(SCRpop/SCR)^0.694 x (albumin/4)^0.594 (SCRpop 0.92/0.77 mg/dL in men/women; ",
    "creatinine entered or assumed normal; albumin entered or 4 g/dL), V1 14.4 L, Q2 ",
    "0.571 L/h, V2 3.55 L, Q3 0.0365 L/h, V3 2.09 L. Plasma methotrexate in mg/L ",
    "(1 umol/L = 0.454 mg/L). https://doi.org/10.1111/bcp.15506"
  )

  list(
    PK = PK,
    tPeak = 0,
    MEAC = 0,
    typical = 0,
    upperTypical = 0,
    lowerTypical = 0,
    reference = reference
  )
}
