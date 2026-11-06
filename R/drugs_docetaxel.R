# -----------------------------------------------------------------------------
# Docetaxel, intravenous (Bruno 1996): plasma docetaxel
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min; amounts in
# mg and concentrations in mcg/mL (= mg/L).
#
# Drafted by Claude Code, 2026-11-06, at the request of Steven L. Shafer
# (docs/chemotherapy.md, inst/extdata/chemotherapyRegistry.csv).  Every
# number below was read from the PDF, which Shafer supplied (2026-11-06), on
# the page images (the scan's text layer is unreliable): Table I, Table II
# and the final equation on p. 168.
#
# THE SOURCE
# ==========
# Bruno R, Vivier N, Vergniol JC, De Phillips SL, Montay G, Sheiner LB.  A
# population pharmacokinetic model for docetaxel (Taxotere): model building
# and validation.  J Pharmacokinet Biopharm 1996;24:153-172 (doi
# 10.1007/BF02353487).  547 adults at their first cycle: 26 data-rich phase 1
# patients (70-115 mg/m2 over 1-2 h) and 521 phase 2 patients (75 or 100
# mg/m2 over 1 h, about 3 samples each over 24 h); plasma, HPLC-UV; NONMEM.
# Means: age 56, BSA 1.78 m2, alpha1-acid glycoprotein (AAG) 1.46 g/L,
# albumin 40.1 g/L.
#
# THE MODEL (Table II, whole data set; the equation on p. 168)
# ============================================================
# Three compartments, linear, every parameter estimated:
#
#     CL (L/h) = BSA x (22.1 - 3.55 AAG - 0.095 AGE + 0.225 ALB)
#                    x (1 - 0.334 HEP12)
#     V1 = 8.31 L;  k12 = 1.07, k21 = 1.74, k13 = 1.28, k31 = 0.0787 /h
#
# AAG and ALB in g/L, age in years; HEP12 is 1 when SGOT or SGPT exceeds 60
# IU AND alkaline phosphatase exceeds 300 IU.  As volumes and clearances: V2
# 5.11 L, V3 135.2 L, Q2 8.89 L/h, Q3 10.64 L/h.  Checks: at the mean
# covariates with HEP12 0 the model gives CL 36.70 L/h, k10 4.417 /h, Vss 148.6
# L and half-lives of 5.84 min, 29.75 min and 11.42 h, Table II's 36.7, 4.42,
# 149, 5.8, 29.7 and 11.4.  Only clearance carries covariates.
#
# WHAT IS HELD AT THE REFERENCE
# =============================
# AAG at 1.46 g/L (the mean) and HEP12 at 0, the Patient Profile carrying
# neither; AAG matters most (-19% at 2.6 g/L), and the HEP12 patient's
# clearance is a third lower.  Albumin is the Patient Profile's, in g/dL
# (x 10 to g/L), and when blank the mean 40.1 g/L.  The BSA formula is not
# named (Mosteller's is used, as for the library's other chemotherapy
# models).  Age enters linearly; it is held at 18 for younger patients (the
# 5th percentile studied was 39) as the library's adult age terms are.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# BSA at the pharmacokinetic weight with the switch on and total weight with
# it off; the volumes and intercompartmental clearances, which carry no
# covariate, scale by the library's factors with the switch on and are fixed
# with it off.
#
# PREGNANCY (R/pregnancy.R)
# =========================
# Recorded, not applied: van Hasselt 2014 (three pregnant women) and Janssen
# 2021 (nine) found a higher clearance and larger volumes in pregnancy, but
# only the abstracts were read.
#
# NO EFFECT SITE, NO BAND
# -----------------------------------------------------------------------------

# Bruno 1996, Table II (whole data set)
DOCETAXEL_BRUNO <- list(
  theta1 = 22.1,      # L/h per m2
  thetaAAG = -3.55,   # per g/L
  thetaAge = -0.095,  # per year
  thetaAlb = 0.225,   # per g/L
  aag = 1.46,         # g/L, held (the mean)
  albumin = 40.1,     # g/L, when the field is blank (the mean)
  v1 = 8.31,          # L
  k12 = 1.07, k21 = 1.74, k13 = 1.28, k31 = 0.0787   # /h
)

#' Docetaxel, intravenous (Bruno 1996)
#'
#' Three compartments; body surface area, age and albumin on clearance,
#' alpha1-acid glycoprotein and hepatic function held at the reference.  See
#' the header of \code{R/drugs_docetaxel.R}.
#'
#' @inheritParams cefazolin
#' @param albumin serum albumin, g/dL, or NULL for the study mean (4.01 g/dL)
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
docetaxel <- function(weight, height, age, sex, adjustToFFM = TRUE,
                      albumin = NULL)
{
  p <- DOCETAXEL_BRUNO
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume = 1, legacyClearance = 1)
  w <- if (isTRUE(adjustToFFM)) size$pkWeight else weight
  bsa <- bsaMosteller(w, height)
  alb <- if (is.null(albumin)) p$albumin else 10 * albumin         # g/L
  ageTerm <- max(age, ADULT_AGE_TERM_FLOOR)
  cl <- bsa * (p$theta1 + p$thetaAAG * p$aag + p$thetaAge * ageTerm +
                 p$thetaAlb * alb)                                    # L/h

  q2 <- p$v1 * p$k12                                                 # L/h
  q3 <- p$v1 * p$k13
  default <- list(
    v1  = p$v1 * size$volume,
    v2  = p$v1 * p$k12 / p$k21 * size$volume,
    v3  = p$v1 * p$k13 / p$k31 * size$volume,
    cl1 = cl / 60,
    cl2 = q2 / 60 * size$clearance,
    cl3 = q3 / 60 * size$clearance
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Bruno R, Vivier N, Vergniol JC, De Phillips SL, Montay G, Sheiner LB. A ",
    "population pharmacokinetic model for docetaxel (Taxotere): model building and ",
    "validation. J Pharmacokinet Biopharm 1996;24:153-172. 547 adults: three ",
    "compartments, CL = BSA x (22.1 - 3.55 AAG - 0.095 age + 0.225 albumin) L/h ",
    "(Mosteller; AAG held at 1.46 g/L, normal hepatic function, albumin ",
    if (is.null(albumin)) "held at 40.1 g/L" else "as entered", "), V1 8.31 L, ",
    "k12 1.07, k21 1.74, k13 1.28, k31 0.0787 /h. Plasma docetaxel. ",
    "https://doi.org/10.1007/BF02353487"
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
