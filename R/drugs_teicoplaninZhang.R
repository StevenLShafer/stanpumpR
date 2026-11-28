# -----------------------------------------------------------------------------
# Teicoplanin: Zhang 2020, children, one compartment (a candidate of the
# teicoplanin group, R/modelSelection.R)
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min.
#
# Drafted by Claude Code, 2026-11-27, on Steven L. Shafer's decision of that
# day (R/drugs_teicoplaninKim.R).
#
# THE SOURCE
# ==========
# Zhang 2020 (Front Pharmacol 11:594562, PMID 33363469, doi
# 10.3389/fphar.2020.594562), read in full from PubMed Central (PMC7753357)
# on 2026-11-27.  159 hospitalised Chinese children (87 boys, 72 girls), age
# 0.2-14.0 years (median 3.7), weight 2.9-69.0 kg (median 14.8); 236 serum
# samples, 89.8% steady-state troughs (1.5 per child); HPLC, LLOQ 2.5 mg/L;
# NONMEM 7.2, FOCE-I, one compartment.  Results and Table 2:
#
#     CL (L/h) = 0.0694 x (1 + 2.82 x WT/16.71) x 0.882^(SCr/29.075)
#     V (L)    = 1.39 x 1.75^(WT/16.71)
#
# SCr in umol/L, the child's own (the source used it directly), so
# patientCreatinine(); blank, an assumed normal creatinine for age and sex.
# Check: at 16.71 kg and SCr 29.075 umol/L, CL 0.234 L/h (0.014 L/h/kg, the
# paper's) and V 2.43 L (0.15 L/kg, the paper's).
#
# A trough-based one-compartment fit: its clearance is informed, its volume,
# and so the size of the peaks it draws, much less so.  The help page says so.
#
# BODY SIZE: both parameters carry the source's weight terms, evaluated at the
# pharmacokinetic weight with the switch on (total weight with it off).
#
# PREGNANCY: not applicable (children).  BAND, DOSE BASIS: as
# R/drugs_teicoplaninKim.R.
# -----------------------------------------------------------------------------

TEICOPLANIN_ZHANG_CL      <- 0.0694  # L/h
TEICOPLANIN_ZHANG_CL_WT   <- 2.82
TEICOPLANIN_ZHANG_CL_SCR  <- 0.882
TEICOPLANIN_ZHANG_SCR_REF <- 29.075  # umol/L
TEICOPLANIN_ZHANG_V       <- 1.39    # L
TEICOPLANIN_ZHANG_V_WT    <- 1.75
TEICOPLANIN_ZHANG_WT_REF  <- 16.71   # kg

#' Teicoplanin (Zhang 2020)
#'
#' One compartment in children, clearance on weight and serum creatinine and
#' the volume on weight; see the header of \code{R/drugs_teicoplaninZhang.R}.
#'
#' @inheritParams teicoplaninKim
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
teicoplaninZhang <- function(weight, height, age, sex, adjustToFFM = TRUE,
                             creatinine = NULL)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)
  w <- (if (isTRUE(adjustToFFM)) size$pkWeight else weight) / TEICOPLANIN_ZHANG_WT_REF
  scr <- patientCreatinine(creatinine, age, sex) * SCR_UMOL_PER_MG_DL
  cl <- TEICOPLANIN_ZHANG_CL * (1 + TEICOPLANIN_ZHANG_CL_WT * w) *
    TEICOPLANIN_ZHANG_CL_SCR^(scr / TEICOPLANIN_ZHANG_SCR_REF)

  default <- list(
    v1  = TEICOPLANIN_ZHANG_V * TEICOPLANIN_ZHANG_V_WT^w,
    v2  = 1, v3 = 1,                  # placeholders: a one-compartment model
    cl1 = cl / 60,
    cl2 = 0, cl3 = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  teicoplaninResult(PK, paste0(
    "Zhang 2020, Front Pharmacol 11:594562, doi 10.3389/fphar.2020.594562. 159 children ",
    "(0.2-14 years), one compartment: CL 0.0694 L/h x (1 + 2.82 x weight/16.71) x ",
    "0.882^(SCr/29.075 umol/L), V 1.39 L x 1.75^(weight/16.71).",
    if (is.null(creatinine)) " Serum creatinine assumed normal for age and sex." else ""))
}
