# -----------------------------------------------------------------------------
# Etoposide, intravenous, in children (Urien 2011): plasma etoposide
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min; amounts in
# mg and concentrations in mcg/mL (= mg/L).
#
# Drafted by Claude Code, 2026-11-06, at the request of Steven L. Shafer
# (docs/chemotherapy.md, inst/extdata/chemotherapyRegistry.csv, source C30).
# Every number below was read from the PDF, which Shafer supplied
# (2026-11-06): Table 1, Table 3 and the allometric equation on the page
# images, since the text extraction drops the 3/4 glyph.
#
# THE SOURCE
# ==========
# Urien S, Doz F, Giraud C, et al.  Developmental pharmacokinetics of
# etoposide in 67 children: lack of dexamethasone effect.  Cancer Chemother
# Pharmacol 2011;67:597-603 (doi 10.1007/s00280-010-1357-2).  67 children,
# 14 weeks to 16.7 years (median 3.5), 5.3-66 kg (median 15), given 1-h
# infusions of 100 or 150 mg/m2/day (3.3 or 5 mg/kg/day under 1 year or
# 10 kg) each followed by carboplatin, on 3 or 5 days; sampled to 12 h after
# the start on the first and last day; plasma, HPLC with fluorescence, LOQ
# 0.1 mg/L (total etoposide by implication: no ultrafiltration is
# described).  Monolix, SAEM.
#
# THE MODEL (Table 3)
# ===================
# Two compartments, linear elimination, every parameter allometric in body
# weight (3/4 on the clearances, 1 on the volumes, at 70 kg):
#
#     CL = 2.05 L/h   Vc = 9.3 L   Q = 0.68 L/h   Vp = 5.7 L
#
# A postmenstrual-age maturation was tested and not retained (its estimates
# were not significant), nor were dexamethasone, the CYP3A4/3A5 and MDR1
# genotypes, creatinine or bilirubin.  Three misprints, none affecting the
# model: the Methods give the clearance exponent as 1/4, where the Abstract,
# Table 3 and the paper's own dosing formula give 3/4; Table 3's covariate
# column prints CL70 for every parameter; and its footnote's check, "2.05 x
# (15/70)^3/4 = 0.75 l/h", is 0.646 L/h by that equation (0.75 is the
# base model's median clearance at the median weight).  The Abstract gives
# Vc as 9.21 L; Table 3's 9.3 is used (a 1% difference).
#
# Derived here, not printed: at 70 kg the half-lives are 2.07 and 8.8 h and
# Vss 15.0 L.  The terminal phase rests on samples covering about 1.4 of its
# half-lives.
#
# WHAT IT DOES NOT COVER
# ======================
# Infants under 6 months (two patients), adults (the paper cites adult
# clearances of 1.8 and 2.65 L/h, which its 70-kg value lies between), oral
# etoposide, renal or hepatic impairment, unbound etoposide.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# The model carries its own weight covariate, evaluated at the
# pharmacokinetic weight with the switch on and total weight with it off.
#
# PREGNANCY (R/pregnancy.R)
# =========================
# No human comparison found; nothing applied (see the registry row).
#
# NO EFFECT SITE, NO BAND
# =======================
# Plasma only; exposure is judged by AUC, which is not drawn.
# -----------------------------------------------------------------------------

# Urien 2011, Table 3: clearances in L/h and volumes in L at 70 kg
ETOPOSIDE_URIEN <- list(cl = 2.05, v1 = 9.3, q = 0.68, v2 = 5.7)

#' Etoposide, intravenous, in children (Urien 2011)
#'
#' Two compartments allometric in weight; see the header of
#' \code{R/drugs_etoposidePediatric.R}.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
etoposidePediatric <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  p <- ETOPOSIDE_URIEN
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume = 1, legacyClearance = 1)
  w <- if (isTRUE(adjustToFFM)) size$pkWeight else weight
  fCl <- (w / 70)^0.75
  fV <- w / 70

  default <- list(
    v1  = p$v1 * fV,
    v2  = p$v2 * fV,
    v3  = 1,                                 # placeholder: two compartments
    cl1 = p$cl * fCl / 60,
    cl2 = p$q * fCl / 60,
    cl3 = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Urien S, Doz F, Giraud C, et al. Developmental pharmacokinetics of etoposide ",
    "in 67 children: lack of dexamethasone effect. Cancer Chemother Pharmacol ",
    "2011;67:597-603. 67 children, 14 weeks-16.7 years: two compartments ",
    "allometric in weight (3/4 on clearances, 1 on volumes, at 70 kg), CL 2.05 ",
    "L/h, Vc 9.3 L, Q 0.68 L/h, Vp 5.7 L. Plasma etoposide only. ",
    "https://doi.org/10.1007/s00280-010-1357-2"
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
