# -----------------------------------------------------------------------------
# Milrinone, intravenous (Bailey 1994): adults having cardiac surgery
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min.
#
# Drafted by Claude Code, 2026-10-19, at the request of Steven L. Shafer, as
# one of the vasoactive and resuscitation drugs used in intensive care
# (inst/help/models/vasoactive.md, inst/extdata/vasoactiveRegistry.csv), from
# the full text, which he supplied.
#
# THE SOURCE
# ==========
# Bailey JM, Levy JH, Kikura M, Szlam F, Hug CC Jr.  Pharmacokinetics of
# intravenous milrinone in patients undergoing cardiac surgery.
# Anesthesiology 1994;81:616-622 (doi 10.1097/00000542-199409000-00014).
# Twenty-five adults (61 +/- 10 years, 87 +/- 18 kg, no renal or hepatic
# disease) given 25, 50 or 75 mcg/kg just before or just after separation
# from cardiopulmonary bypass, or 50 mcg/kg with 0.5 mcg/kg/min for the rest
# of the study; arterial plasma for 16 hours (960 min).  The pooled data were
# fitted to one three-compartment model, per kilogram (Table 2):
#
#     V1 102, V2 198, V3 1398 mL/kg     Cl1 1.88, Cl2 14.40, Cl3 2.91 mL/kg/min
#
# Those six numbers reproduce the table's own triexponential for a 50 mcg/kg
# bolus to its printed precision: exponents 0.2472, 0.01497 and 0.000754 /min
# (printed .2478, .0150, .0008) and coefficients 366, 111 and 13.4 ng/mL
# (printed 365.7, 110.8, 13.2), which the tests check.  Vss is 1.70 L/kg and
# the terminal half-life 15 h.  The parameters did not differ with the dose,
# or before against after separation from bypass.
#
# WHY BAILEY, AND NOT BUTTERWORTH 1995
# ====================================
# Butterworth JF et al., Anesth Analg 1995;81:783-792 (read in full) also fit
# three compartments after bypass (V1 11.1, V2 16.9, V3 363 L; Cl1 0.067,
# Cl2 1.05, Cl3 0.31 L/min, NONMEM), but sampled only to 6 hours, and its
# elimination clearance was not identified: the 95% interval is 0.004 to
# 1.061 L/min, and V3's could not be estimated.  Its own two-compartment fit
# (V1 12.4, V2 24.0 L; CL 0.28, Q 0.88 L/min) is coherent but describes a
# drug cleared four times faster.  Bailey's sixteen hours, including a group
# held on an infusion, pin the slow phase down; at 50 mcg/kg then 0.5
# mcg/kg/min the model predicts 141 ng/mL at 6 h and 186 at 16 h, the
# plateau of its Figure 5.
#
# In heart failure the model holds up: Jaski BE et al., J Clin Invest
# 1985;75:643-649 (doi 10.1172/JCI111742; read in full) gave 12.5, 25, 50
# and 75 mcg/kg boluses 10 minutes apart to 11 patients and measured serum
# 7-10 minutes after each: 63-66, 156 and (after the fourth) 427 ng/mL,
# against the model's 39-45, 103-115 and 397-435.  It runs about a third low
# after the small early doses and matches at the highest.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# Per kilogram, as published: total weight with the switch off, the
# pharmacokinetic weight with it on.
#
# RENAL FUNCTION IS NOT A COVARIATE
# =================================
# About 80% of milrinone is excreted unchanged in the urine, and its
# clearance falls with creatinine clearance, but Bailey's patients had no
# renal disease and the model has no renal term.  For impaired renal
# function, milrinonePediatric (Hornik 2019) carries one; this model will
# overpredict clearance in renal failure.
#
# EFFECT SITE: tPeak 5 MINUTES (Butterworth 1995)
# ===============================================
# Butterworth measured cardiac index 2.5, 5, 7.5 and 10 minutes after a
# one-minute bolus and reports that "at the 5-min measurement, most patients
# had achieved their maximal increase in cardiac index".  That is the time to
# peak effect, to the precision of its 2.5-minute sampling, and ke0 is solved
# from it.  Ritchie RH, Hii JT, Horowitz JD, J Cardiovasc Pharmacol
# 1998;31:885-893 (doi 10.1097/00005344-199806000-00012; 1 mg bolus, 12 men at
# cardiac catheterisation, read in full) put the mean individual times of
# maximal effect at 7.3 min for LV dP/dt max and 8.4 min for mean arterial
# pressure, but measured them only at 2, 5, 10 and 15 min, so about half its
# patients peaked at 5 min and half at 10; Butterworth's finer grid is kept,
# and the true peak may lie a few minutes later.  Bailey's concentration-response (a 50% rise in cardiac index at
# 167 ng/mL, Hill 1.16) is not plotted; the band is the 100-300 ng/mL target
# range quoted for adults and children (Hornik 2019), with Bailey's 167 as
# the typical value.
# -----------------------------------------------------------------------------

# Bailey 1994, Table 2, per kilogram
MILRINONE_V1  <- 0.102     # L/kg
MILRINONE_V2  <- 0.198     # L/kg
MILRINONE_V3  <- 1.398     # L/kg
MILRINONE_CL1 <- 0.00188   # L/kg/min
MILRINONE_CL2 <- 0.01440   # L/kg/min
MILRINONE_CL3 <- 0.00291   # L/kg/min
MILRINONE_TPEAK <- 5       # min, Butterworth 1995

#' Milrinone, intravenous (Bailey 1994)
#'
#' Three compartments per kilogram in adults having cardiac surgery, with
#' the time to peak effect from Butterworth 1995; see the header of
#' \code{R/drugs_milrinone.R}.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
milrinone <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)
  pkW  <- if (isTRUE(adjustToFFM)) size$pkWeight else weight

  default <- list(
    v1  = MILRINONE_V1 * pkW,
    v2  = MILRINONE_V2 * pkW,
    v3  = MILRINONE_V3 * pkW,
    cl1 = MILRINONE_CL1 * pkW,
    cl2 = MILRINONE_CL2 * pkW,
    cl3 = MILRINONE_CL3 * pkW
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Bailey JM, Levy JH, Kikura M, Szlam F, Hug CC Jr, Anesthesiology ",
    "1994;81:616-622.  Three compartments in 25 adults having cardiac surgery: ",
    "V1 102, V2 198, V3 1398 mL/kg; Cl1 1.88, Cl2 14.40, Cl3 2.91 mL/kg/min; ",
    "no renal term.  Time to peak effect 5 min from Butterworth JF et al., ",
    "Anesth Analg 1995;81:783-792. ",
    "https://doi.org/10.1097/00000542-199409000-00014"
  )

  list(
    PK = PK,
    tPeak = MILRINONE_TPEAK,
    MEAC = 0,
    typical = 167,
    upperTypical = 300,
    lowerTypical = 100,
    reference = reference
  )
}
