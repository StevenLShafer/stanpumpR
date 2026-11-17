# -----------------------------------------------------------------------------
# Dacarbazine, intravenous (Breithaupt 1982): plasma dacarbazine (DTIC)
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min; amounts in
# mg and concentrations in mcg/mL (= mg/L).
#
# Drafted by Claude Code, 2026-11-13, at the request of Steven L. Shafer, who
# asked for this paper (stanpumpR-papers-needed.csv, priority 7) and had it
# added to the papers archive (PMID_7172404).  Every number below was read on
# the pages of the PDF.
#
# THE SOURCE
# ==========
# Breithaupt H, Dammann A, Aigner K.  Pharmacokinetics of dacarbazine (DTIC)
# and its metabolite 5-aminoimidazole-4-carboxamide (AIC) following different
# dose schedules.  Cancer Chemother Pharmacol 1982;9:103-109 (doi
# 10.1007/BF00265388).  Eight patients with metastatic melanoma or sarcoma
# (Table 1); the six in Table 2 (patients 1, 2, 3, 4, 7, 8): 42-57 years,
# 62-92 kg, five men, given 2.65-6.85 mg/kg by intravenous bolus daily for 5
# days, sampled to 6 h; HPLC (limit 50 ng/mL).  Each patient's curve was fitted
# with two exponentials; Table 2 prints, per patient, the mean over the 5
# days of the two half-lives, the mean residence time T, Vss (= CL x T), Ve
# and CL (= dose / AUC).
#
# THE MODEL
# =========
# The paper prints no compartmental parameters, but CL, Vss and BOTH
# half-lives close a two-compartment model (docs/antihypertensives.md,
# section 4): with alpha and beta the exponents, k12 + k21 = (Vss / CL)
# alpha beta, k10 = alpha + beta - (k12 + k21), k21 = alpha beta / k10, and
# V1 = CL / k10.  Each of the six patients is solved so from his own Table 2
# row (the two BCG-preceded cycles excluded, as the paper's mean excludes
# them), and the model is the mean of the six (dacarbazineBreithaupt(), as
# verapamil is the mean of per-patient fits):
#
#     V1 0.178 L/kg   V2 0.454 L/kg   CL 15.4 mL/min/kg   Q 21.9 mL/min/kg
#
# CHECKS (test-drugs-dacarbazine.R), each against a number the derivation
# did not use: every patient's derived V1 lies near Table 2's "Ve" (mean
# 0.178 against 0.202 L/kg), and the share of the area in the fast phase
# (mean 30%) matches the text's "about 28 +/- 7%" and Discussion's 21-35%.
# The mean CL 15.4 mL/min/kg and Vss 0.632 L/kg are the paper's own means.
#
# NOT MODELLED: AIC, the inactive end product of the methyltriazene route,
# whose fraction formed the paper did not determine; MTIC, the active
# methylating species, which was not measured.  Half of the clearance is renal
# (5.2-10.9 mL/min/kg), but with six patients of normal renal function no
# renal term can be estimated; the curve is a normal kidney's.
#
# DOSE BASIS: dacarbazine has no salt (the product's citric acid is an
# excipient); doses in mg of dacarbazine.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# The source normalised every volume and clearance to body weight; they are
# multiplied by the pharmacokinetic weight with the switch on and by total
# weight with it off.
#
# PREGNANCY: see its registry row (Kantrowitz-Gordon 2018, one woman paired:
# recorded, not applied).  NO EFFECT SITE, NO BAND.
# -----------------------------------------------------------------------------

# Breithaupt 1982, Table 2: the six patients without BCG, in order 1, 2, 3, 4,
# 7, 8: the two half-lives (min), CL (mL/kg/min) and Vss (L/kg); and Ve (L/kg),
# the check
DACARBAZINE_BREITHAUPT <- data.frame(
  patient = c(1, 2, 3, 4, 7, 8),
  tAlpha = c(2.5, 3.6, 2.6, 3.5, 2.4, 2.9),
  tBeta = c(45.8, 40.0, 30.9, 36.0, 51.6, 44.0),
  cl = c(13.6, 8.7, 23.3, 15.8, 21.9, 9.3),
  vss = c(0.700, 0.406, 0.719, 0.549, 0.931, 0.487),
  ve = c(0.182, 0.165, 0.225, 0.193, 0.281, 0.166)
)

#' Two compartments from CL, Vss and both half-lives
#'
#' @param tAlpha,tBeta the two half-lives (any one time unit)
#' @param cl clearance (volume per that time unit)
#' @param vss steady-state volume
#' @returns a list: v1, v2, cl, q, and the fast phase's share of the area
#' @keywords internal
twoCompartmentFromClVss <- function(tAlpha, tBeta, cl, vss)
{
  a <- log(2) / tAlpha
  b <- log(2) / tBeta
  s <- vss / cl * a * b                    # k12 + k21
  k10 <- a + b - s
  k21 <- a * b / k10
  k12 <- s - k21
  if (!all(c(k10, k12, k21) > 0))
    stop("These half-lives, CL and Vss admit no two-compartment model")
  v1 <- cl / k10
  A <- (a - k21) / (v1 * (a - b))           # unit-dose coefficients
  B <- (k21 - b) / (v1 * (a - b))
  list(v1 = v1, v2 = v1 * k12 / k21, cl = cl, q = v1 * k12,
       fastShare = (A / a) / (A / a + B / b))
}

#' Dacarbazine: the mean of Breithaupt's six per-patient models, per kg
#'
#' @returns a list: v1, v2 (L/kg), cl, q (L/min/kg), and the per-patient
#'   models
#' @keywords internal
dacarbazineBreithaupt <- function()
{
  p <- DACARBAZINE_BREITHAUPT
  each <- do.call(rbind, lapply(seq_len(nrow(p)), function(i)
    as.data.frame(twoCompartmentFromClVss(p$tAlpha[i], p$tBeta[i],
                                          p$cl[i] / 1000, p$vss[i]))))
  list(v1 = mean(each$v1), v2 = mean(each$v2), cl = mean(each$cl), q = mean(each$q),
       each = each)
}

#' Dacarbazine, intravenous (Breithaupt 1982)
#'
#' Two compartments derived from each patient's clearance, steady-state
#' volume and two half-lives, averaged, per kilogram; see the header of
#' \code{R/drugs_dacarbazine.R}.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
dacarbazine <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  m <- dacarbazineBreithaupt()
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume = 1, legacyClearance = 1)
  w <- if (isTRUE(adjustToFFM)) size$pkWeight else weight

  default <- list(
    v1  = m$v1 * w,
    v2  = m$v2 * w,
    v3  = 1,                                 # placeholder: two compartments
    cl1 = m$cl * w,
    cl2 = m$q * w,
    cl3 = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Breithaupt H, Dammann A, Aigner K. Pharmacokinetics of dacarbazine (DTIC) and its ",
    "metabolite 5-aminoimidazole-4-carboxamide (AIC) following different dose schedules. ",
    "Cancer Chemother Pharmacol 1982;9:103-109. Six adults: two compartments derived from ",
    "each patient's CL, Vss and two half-lives and averaged, V1 0.178 and V2 0.454 L/kg, ",
    "CL 15.4 and Q 21.9 mL/min/kg. https://doi.org/10.1007/BF00265388"
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
