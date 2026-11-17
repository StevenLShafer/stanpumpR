# -----------------------------------------------------------------------------
# Aprocitentan, macitentan's active metabolite: one compartment, from
# Bartolucci 2021 (see R/drugs_macitentan.R)
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min; amounts in
# mcg, so the plotted concentration is ng/mL (= mcg/L), total plasma.
#
# Drafted by Claude Code, 2026-11-16.  Metabolite only: it cannot be dosed (no
# Units and no Category in drugDefaults_global.csv) and is drawn when
# macitentan is.  Aprocitentan is also a product of its own (Tryvio, for
# hypertension), which this entry does not offer: these parameters were fitted
# to aprocitentan formed from macitentan.  Fitted jointly with macitentan's in
# one source, which reads them from here (METABOLITE_FORMATION_BASIS
# "shared"); Table 3 of Bartolucci 2021:
#
#     V2 23.5 L x (BW/68.8)^0.86 x (1 - 0.163 in Asians)
#     CL2 0.321 L/h x (BW/68.8)^0.70 x (1 + 0.157 in men)
#         x (1 - 0.131 in moderate or severe renal impairment)
#         x (1 - 0.158 in Asians)
#
# Renal impairment is the paper's category, an MDRD eGFR below 60 mL/min/1.73
# m2 (Table 2 footnote c, egfrMDRD()), from the Patient Profile's creatinine
# (assumed normal when blank); race is held at not Asian.  At steady state
# "the systemic exposure to the active metabolite is 3-times the exposure to
# macitentan and is expected to contribute approximately 40% of the total
# pharmacologic activity" (Opsumit label).
#
# THE BAND
# ========
# No therapeutic range.
# -----------------------------------------------------------------------------

# Bartolucci 2021, Table 3
APROCITENTAN_V      <- 23.5    # L
APROCITENTAN_CL     <- 0.321   # L/h
APROCITENTAN_V_WT   <- 0.86
APROCITENTAN_CL_WT  <- 0.70
APROCITENTAN_MALE   <- 0.157   # fractional change in CL2
APROCITENTAN_RENAL  <- -0.131  # moderate or severe renal impairment
APROCITENTAN_EGFR_CUT <- 60    # mL/min/1.73 m2

#' Aprocitentan, the active metabolite of macitentan (Bartolucci 2021)
#'
#' One compartment; see the header of \code{R/drugs_aprocitentan.R}.
#'
#' @inheritParams cefazolin
#' @param creatinine serum creatinine, mg/dL, or NULL for an assumed normal
#'   value
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
aprocitentan <- function(weight, height, age, sex, adjustToFFM = TRUE,
                         creatinine = NULL)
{
  wt <- macitentanWeight(weight, height, age, sex, adjustToFFM) / MACITENTAN_WT_REF
  egfr <- egfrMDRD(age, sex, adultEquivalentCreatinine(creatinine, age, sex))
  cl <- APROCITENTAN_CL * wt^APROCITENTAN_CL_WT *
    (if (sex == SEX_MALE) 1 + APROCITENTAN_MALE else 1) *
    (if (egfr < APROCITENTAN_EGFR_CUT) 1 + APROCITENTAN_RENAL else 1)

  default <- list(
    v1  = APROCITENTAN_V * wt^APROCITENTAN_V_WT,
    v2  = 1,
    v3  = 1,
    cl1 = cl / 60,
    cl2 = 0,
    cl3 = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))
  list(
    PK = PK,
    tPeak = 0,
    MEAC = 0,
    typical = 0,
    upperTypical = 0,
    lowerTypical = 0,
    reference = MACITENTAN_REFERENCE
  )
}
