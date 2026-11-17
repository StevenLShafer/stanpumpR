# -----------------------------------------------------------------------------
# M6-SUZ, suzetrigine's active metabolite: two compartments, from the FDA
# Integrated Review of NDA 219209 (see R/drugs_suzetrigine.R)
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min; amounts in
# mcg, so the plotted concentration is ng/mL (= mcg/L), total plasma.
#
# Drafted by Claude Code, 2026-11-16.  Metabolite only: it cannot be dosed (no
# Units and no Category in drugDefaults_global.csv) and is drawn when
# suzetrigine is.  Its parameters were fitted jointly with suzetrigine's in
# the applicant's model 2822 (Table 149), and suzetrigine reads them from this
# function (METABOLITE_FORMATION_BASIS "shared"):
#
#     central V2M 1.7 L, peripheral V3M 29.5 L (x weight/76.2), Q3M 8.75 L/h
#     clearance CLM 0.273 x CLP (suzetrigineClearance())
#
# The clearance here is the uninduced one.  In a suzetrigine simulation the
# saturable engine multiplies it by the same enzyme state (AENZ) as the
# parent's, so the help page's half-lives are those before induction.
# M6-SUZ is "a less potent inhibitor of NaV1.8 than suzetrigine by 3.7-fold"
# (label, sec. 12.1); the FDA's exposure-response analysis used SUZ + M6-SUZ/3.
#
# THE BAND
# ========
# No therapeutic range.
# -----------------------------------------------------------------------------

# Table 149
M6SUZ_V1 <- 1.7      # L, V2M (central)
M6SUZ_V2 <- 29.5     # L, V3M (peripheral)
M6SUZ_Q  <- 8.75     # L/h, Q3M
M6SUZ_CL_RATIO <- 0.273   # CLM relative to CLP

#' M6-SUZ, the active metabolite of suzetrigine (NDA 219209 review)
#'
#' Two compartments; see the header of \code{R/drugs_M6SUZ.R}.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
M6SUZ <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  f <- suzetrigineVolumeFactor(weight, height, age, sex, adjustToFFM)
  default <- list(
    v1  = M6SUZ_V1 * f,
    v2  = M6SUZ_V2 * f,
    v3  = 1,
    cl1 = M6SUZ_CL_RATIO * suzetrigineClearance(weight, height, sex) / 60,
    cl2 = M6SUZ_Q / 60,
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
    reference = SUZETRIGINE_REFERENCE
  )
}
