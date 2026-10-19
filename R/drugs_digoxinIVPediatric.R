# -----------------------------------------------------------------------------
# Digoxin, intravenous, in children (el-Desoky 2005)
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min.
#
# Drafted by Claude Code, 2026-10-19, at the request of Steven L. Shafer, from
# the full text, which he supplied.  Named for its population, as
# milrinonePediatric is; the adult intravenous model is digoxinIV.
#
# THE SOURCE
# ==========
# el-Desoky ES, Madabushi R, Amry SA, Bhattaram VA, Derendorf H.  Application
# of two-point assay of digoxin serum concentration in studying population
# pharmacokinetics in Egyptian pediatric patients with heart failure: does
# it make sense?  Am J Ther 2005;12:320-327 (doi
# 10.1097/01.mjt.0000155108.62208.82).  Forty children with heart failure,
# 0.33-15 years (mean 5.9), 4.3-42 kg, maintained on intravenous digoxin 10
# mcg/kg/day in two doses; two serum samples each (FPIA), NONMEM.  Final
# model (Table 4 and Results):
#
#     CL (L/h/kg) = 0.38 x [1 - 0.76 x (SCr - 0.6)]     SCr in mg/dL
#     V1 1.38 L/kg     Q 0.47 L/h/kg     V2 9.11 L/kg (FIXED)
#
# The abstract prints "CL (L/h) = 0.388 - [0.78 x (SCR - 0.6)]", which garbles
# this; the review of Abdel Jalil 2020 (Br J Clin Pharmacol 86:1267) copies
# the abstract.  V2 was fixed because its likelihood was flat: three
# quarters of the children gave only troughs.
#
# COVARIATES HERE
# ===============
# Serum creatinine is the child's own (patientCreatinine()), as the source
# used it, held to the studied 0.2-1.3 mg/dL: the linear term reaches zero
# clearance at 1.92 mg/dL, beyond which the model has no meaning.  A blank
# field means an assumed normal creatinine for age and sex.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# Per kilogram, as published: total weight with the switch off, the
# pharmacokinetic weight with it on.
#
# NO EFFECT SITE
# ==============
# No time of peak effect was measured in children; plasma only.  The band is
# the 0.5-2 ng/mL serum range.
# -----------------------------------------------------------------------------

# el-Desoky 2005, Table 4, final model
DIGOXIN_IV_PED_CL    <- 0.38    # L/h/kg at SCr 0.6 mg/dL
DIGOXIN_IV_PED_SCR   <- -0.76   # fractional change per mg/dL above 0.6
DIGOXIN_IV_PED_V1    <- 1.38    # L/kg
DIGOXIN_IV_PED_Q     <- 0.47    # L/h/kg
DIGOXIN_IV_PED_V2    <- 9.11    # L/kg, fixed
DIGOXIN_IV_PED_SCR_RANGE <- c(0.2, 1.3)   # mg/dL, studied

#' Digoxin, intravenous, in children (el-Desoky 2005)
#'
#' Two compartments per kilogram with serum creatinine on clearance; see the
#' header of \code{R/drugs_digoxinIVPediatric.R}.
#'
#' @inheritParams cefazolin
#' @param creatinine serum creatinine in mg/dL, or NULL for the assumed normal
#'   value
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
digoxinIVPediatric <- function(weight, height, age, sex, adjustToFFM = TRUE,
                               creatinine = NULL)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)
  pkW  <- if (isTRUE(adjustToFFM)) size$pkWeight else weight
  scr  <- patientCreatinine(creatinine, age, sex)
  scr  <- min(max(scr, DIGOXIN_IV_PED_SCR_RANGE[1]), DIGOXIN_IV_PED_SCR_RANGE[2])

  clPerKg <- DIGOXIN_IV_PED_CL * (1 + DIGOXIN_IV_PED_SCR * (scr - 0.6))   # L/h/kg

  default <- list(
    v1  = DIGOXIN_IV_PED_V1 * pkW,
    v2  = DIGOXIN_IV_PED_V2 * pkW,
    v3  = 1,                          # placeholder: a two-compartment model
    cl1 = clPerKg * pkW / 60,
    cl2 = DIGOXIN_IV_PED_Q * pkW / 60,
    cl3 = 0
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  creatinineNote <- if (is.null(creatinine))
    paste0("an assumed normal creatinine for age, ", format(round(scr, 2)), " mg/dL")
  else
    paste0("serum creatinine ", format(creatinine), " mg/dL")
  reference <- paste0(
    "el-Desoky ES, Madabushi R, Amry SA, Bhattaram VA, Derendorf H, Am J Ther ",
    "2005;12:320-327.  Two compartments in 40 children with heart failure: CL ",
    "0.38 L/h/kg x [1 - 0.76 (SCr - 0.6)], V1 1.38 L/kg, Q 0.47 L/h/kg, V2 9.11 ",
    "L/kg; ", creatinineNote, ". https://doi.org/10.1097/01.mjt.0000155108.62208.82"
  )

  list(
    PK = PK,
    tPeak = 0,
    MEAC = 0,
    typical = 1.0,
    upperTypical = 2.0,
    lowerTypical = 0.5,
    reference = reference
  )
}
