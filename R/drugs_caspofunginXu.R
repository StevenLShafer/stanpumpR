# -----------------------------------------------------------------------------
# Caspofungin: Xu 2026, critically ill children, two compartments (a candidate
# of the caspofungin group, R/modelSelection.R)
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min.
#
# Drafted by Claude Code, 2026-11-28, on Steven L. Shafer's decisions of that
# day: Xu 2026 for children, and Wurthwein 2013 (Antimicrob Agents Chemother
# 57:1664-1671, PMID 23335740; 46 adults with invasive aspergillosis, 70-200 mg)
# for adults once its PDF is supplied (papers/pending-work.csv).  Until then the
# group holds this model alone, and an adult receives it by the nearest rule:
# an extrapolation the help page states.  Yang 2022 (PMID 36408257) was not
# chosen: its covariates are steps at 70 kg and 35 g/L albumin (a 98% jump in
# clearance at 70 kg).  Niu 2020 (PMID 32194415) was not chosen: one
# compartment fitted mostly to troughs.
#
# THE SOURCE
# ==========
# Xu 2026 (Antimicrob Agents Chemother 70(2):e01277-25, PMID 41468548, doi
# 10.1128/aac.01277-25), read on the archive's PDF on 2026-11-28.  A prospective
# study in a Shanghai PICU: 29 children (12 boys, 17 girls), Table 1 medians age
# 5.33 years (0.33-16.0), weight 16.0 kg (4.90-74.0), height 104 cm (54-173),
# BSA 0.660 m2 (0.286-1.89, Mosteller); four on ECMO, one on CRRT.  1-h
# infusions of 70 mg/m2 on day 1 then 50 mg/m2 daily (at most 70 mg); 138
# plasma samples, intensive then optimal-design sparse, from the sixth dose;
# total caspofungin by LC-MS/MS.  NONMEM 7.5, FOCE-I.  Table 2:
#
#     CL 0.196 L/h,  V1 2.22 L,  Q 1.01 L/h,  V2 1.63 L   (at BSA 0.79 m2)
#     BSA exponent 0.66 FIXED on clearance, 1 FIXED on volume
#
# The Results say the scaling is "a fixed exponential of 1 for volume of
# distribution [V], and 0.66 for clearance [CL]"; it is applied to both
# clearances (CL and Q) and both volumes (V1 and V2), the usual allometric
# reading.  [source] Table 2 does not say so for Q and V2 separately.  The text
# calls the model "two-compartment with first-order absorption" for a 1-h
# infusion and prints no absorption parameter; the drug is given here as the
# infusion it was.  Check: CL/V1 = 0.088 /h, the elimination rate constant the
# authors quote.
#
# ECMO on V1 (18.2, its form not stated, RSE 62%) is not applied: the Patient
# Profile has no ECMO field, and the reference child is without ECMO.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# The model's own covariate, BSA by Mosteller (the source's formula), on every
# parameter, computed from TOTAL weight in both switch positions: a covariate
# fitted against the children's real size is evaluated at real size (Shafer,
# 2026-11-25, R/drugs_temozolomidePanetta.R).  pkSizeFactors() is not called.
#
# AGAINST THE LABEL (Cancidas, rev. 5/2022, Table 8): at 50 mg/m2 daily the
# label's children reach an AUC0-24 of 120-131 mg h/L; this model gives a 0.79
# m2 child 39.5 mg / 0.196 L/h = 202 (Xu's own Bayesian median 193.6).  Its
# critically ill children cleared less per m2 than the label's.
#
# PREGNANCY: see its registry row (not applicable to this children's model).
# BAND AND THRESHOLD: none; the targets are AUC/MIC ratios.
# DOSE BASIS: caspofungin as labelled (70 mg of caspofungin is 77.7 mg of
# caspofungin acetate; the label and the source dose caspofungin).
# -----------------------------------------------------------------------------

CASPOFUNGIN_XU <- list(
  cl = 0.196,     # L/h at BSA 0.79 m2
  v1 = 2.22,      # L
  q = 1.01,       # L/h
  v2 = 1.63,      # L
  bsaRef = 0.79,  # m2
  bsaCl = 0.66,   # fixed
  bsaV = 1        # fixed
)

#' Caspofungin (Xu 2026)
#'
#' Two compartments in critically ill children, every parameter scaled on
#' body-surface area; see the header of \code{R/drugs_caspofunginXu.R}.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
caspofunginXu <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  p <- CASPOFUNGIN_XU
  b <- bsaMosteller(weight, height) / p$bsaRef
  fCl <- b^p$bsaCl
  fV <- b^p$bsaV

  default <- list(
    v1  = p$v1 * fV,
    v2  = p$v2 * fV,
    v3  = 1,                          # placeholder: a two-compartment model
    cl1 = p$cl * fCl / 60,
    cl2 = p$q * fCl / 60,
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
    reference = paste0(
      "Xu 2026, Antimicrob Agents Chemother 70:e01277-25, doi 10.1128/aac.01277-25. ",
      "29 critically ill children, two compartments at BSA 0.79 m2 (Mosteller): CL 0.196 L/h, ",
      "V1 2.22 L, Q 1.01 L/h, V2 1.63 L; clearances x (BSA/0.79)^0.66, volumes x BSA/0.79. ",
      "ECMO not applied.")
  )
}
