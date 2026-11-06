# -----------------------------------------------------------------------------
# Cyclophosphamide, intravenous (Joerger 2007): plasma cyclophosphamide
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min; amounts in
# mg and concentrations in mcg/mL (= mg/L).
#
# Drafted by Claude Code, 2026-11-06, at the request of Steven L. Shafer
# (docs/chemotherapy.md, inst/extdata/chemotherapyRegistry.csv).  Every
# number below was read from the PDF, which Shafer supplied (2026-11-06), on
# the page images: Tables I and IV and Eq. 12.  The same paper is the
# library's doxorubicin model (R/drugs_doxorubicin.R).
#
# THE SOURCE
# ==========
# Joerger M, Huitema ADR, Richel DJ, et al.  Population pharmacokinetics and
# pharmacodynamics of doxorubicin and cyclophosphamide in breast cancer
# patients: a study by the EORTC-PAMM-NDDG.  Clin Pharmacokinet
# 2007;46:1051-1068 (doi 10.2165/00003088-200746120-00005).  59 women with
# breast cancer (median age 56, BSA 1.81 m2), first cycle of doxorubicin 60
# mg/m2 then cyclophosphamide 600 mg/m2, each over 15 min; cyclophosphamide
# in heparinised plasma before, at the end of the infusion and 1 and 24 h
# after its start; capillary GC; NONMEM, FOCE.
#
# THE MODEL (Table IV and Eq. 12)
# ===============================
# One compartment (two were over-parameterised), linear:
#
#     CL = 4.23 L/h x 1.89^EIAD     V = 34.6 L
#
# EIAD is 1 with an enzyme-inducing antiepileptic (all three such patients
# took carbamazepine).  The half-life is 5.7 h.  The paper prints CL in L/min
# in its Methods and 4.25 L/h in its Results: misprints for Table IV's 4.23
# L/h.  No body-size covariate was found.
#
# WHAT IT DOES NOT COVER
# ======================
# The active metabolite, 4-hydroxycyclophosphamide, was not measured; the
# autoinduction of repeated or high-dose cyclophosphamide is not represented
# (one profile per patient, in the first cycle); EIAD is held at 0 (the
# library's carbamazepine does not act on it); men; children.
#
# DOSE BASIS
# ==========
# Not stated in the paper (which worked in umol); doses are entered as
# labelled and the curve is in the same mass units.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# No size covariate: the volume and clearance scale by the library's factors
# with the switch on and are fixed with it off.
#
# PREGNANCY (R/pregnancy.R): recorded, not applied (one case report).
# NO EFFECT SITE, NO BAND
# -----------------------------------------------------------------------------

# Joerger 2007, Table IV
CYCLOPHOSPHAMIDE_JOERGER <- list(
  cl = 4.23,          # L/h
  eiadFactor = 1.89,  # with an enzyme-inducing antiepileptic (held at none)
  v = 34.6            # L
)

#' Cyclophosphamide, intravenous (Joerger 2007)
#'
#' One compartment; see the header of \code{R/drugs_cyclophosphamide.R}.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
cyclophosphamide <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  p <- CYCLOPHOSPHAMIDE_JOERGER
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume = 1, legacyClearance = 1)

  default <- list(
    v1  = p$v * size$volume,
    v2  = 1,                                 # placeholder: one compartment
    v3  = 1,
    cl1 = p$cl / 60 * size$clearance,        # no enzyme-inducing antiepileptic
    cl2 = 0,
    cl3 = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Joerger M, Huitema ADR, Richel DJ, et al. Population pharmacokinetics and ",
    "pharmacodynamics of doxorubicin and cyclophosphamide in breast cancer patients: ",
    "a study by the EORTC-PAMM-NDDG. Clin Pharmacokinet 2007;46:1051-1068. 59 women: ",
    "one compartment, CL 4.23 L/h (no enzyme-inducing antiepileptic), V 34.6 L. ",
    "Plasma cyclophosphamide; 4-hydroxycyclophosphamide and autoinduction not modelled. ",
    "https://doi.org/10.2165/00003088-200746120-00005"
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
