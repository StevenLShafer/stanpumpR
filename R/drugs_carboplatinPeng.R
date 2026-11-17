# -----------------------------------------------------------------------------
# Carboplatin, intravenous, in children (Peng 1995): ultrafilterable
# carboplatin, two compartments per m2
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min; amounts in
# mg and concentrations in mcg/mL (= mg/L).
#
# Drafted by Claude Code, 2026-11-17.  Steven L. Shafer decided (2026-11-17)
# that Peng 1995 join the carboplatin group as a candidate, so that the
# children neither Duffull 1997 (adults 49-68 years) nor Duong 2019 (children
# 1.7-8.3 years) contains, infants and children from 8.3 to 18 years, are
# given a model fitted in children of their ages.  Every number below was read
# on the pages of the PDF, which Shafer supplied.
#
# THE SOURCE
# ==========
# Peng B, Boddy AV, Cole M, Pearson ADJ, Chatelut E, Rubie H, Newell DR.
# Comparison of methods for the estimation of carboplatin pharmacokinetics in
# paediatric cancer patients.  Eur J Cancer 1995;31A:1804-1810 (doi
# 10.1016/0959-8049(95)00382-s).  Ultrafilterable platinum by atomic
# absorption, read as carboplatin; 60-90 min infusions of 93-750 mg/m2.
#
# THE MODEL (Table 2)
# ===================
# Two compartments per m2, the lognormal population means that are the prior
# of the paper's Bayesian method:
#     V 5.092252 L/m2   k10 0.020648   k12 0.025259   k21 0.018519 /min
# so CL 0.1051 L/min/m2 (6.31 L/h/m2), V2 6.95 L/m2, Vss 12.0 L/m2.  No
# covariate: the renal dosing equation the paper also tests, CL = GFR + 0.36
# BW, is a separate method, not part of the model.
#
# THE POPULATION.  The prior was fitted in 22 children of an earlier study
# (its reference 21; 16 by maximum likelihood with two compartments, 6 by
# MAP), whose ages are not printed here.  The ranges recorded for the model
# selector are those of the paper's validation set, 23 children 1 month to
# 18.4 years (221 months), 5.0-63 kg, GFR 42-213 mL/min/1.73 m2, in whom the
# Bayesian method was tested.
#
# CHECKS (test-drugs-carboplatinPeng.R): Table 2's parameters close to CL
# 6.31 L/h/m2; the validation set's model-independent clearance had a median
# of 88 mL/min/m2 (5.3 L/h/m2, range 51-129), so the prior sits 19% above the
# median of the children it was tested in, within their range.  Against
# Duong 2019 at the same child it is 40-50% higher (2.24 against 1.51 L/h at
# 6 months, 9.40 against 6.72 at 14 years).
#
# DOSE BASIS: mg of carboplatin, as labelled (carboplatin has no salt).
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# Every volume and clearance per m2 of body-surface area (the paper names no
# formula; Mosteller's is used), from the pharmacokinetic weight with the
# switch on and total weight with it off.
#
# PREGNANCY: not applicable (a children's model).
# NO EFFECT SITE, NO BAND: the AUC target is an exposure.
# -----------------------------------------------------------------------------

# Peng 1995, Table 2 (per m2, per minute)
CARBOPLATIN_PENG <- list(v1 = 5.092252, k10 = 0.020648, k12 = 0.025259, k21 = 0.018519)

#' Carboplatin, intravenous, in children (Peng 1995)
#'
#' Two compartments per m2 of body-surface area; see the header of
#' \code{R/drugs_carboplatinPeng.R}.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
carboplatinPeng <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  p <- CARBOPLATIN_PENG
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume = 1, legacyClearance = 1)
  w <- if (isTRUE(adjustToFFM)) size$pkWeight else weight
  bsa <- bsaMosteller(w, height)
  v1 <- p$v1 * bsa

  default <- list(
    v1  = v1,
    v2  = v1 * p$k12 / p$k21,
    v3  = 1,                                 # placeholder: two compartments
    cl1 = v1 * p$k10,
    cl2 = v1 * p$k12,
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
      "Peng B, Boddy AV, Cole M, et al. Comparison of methods for the estimation of ",
      "carboplatin pharmacokinetics in paediatric cancer patients. Eur J Cancer ",
      "1995;31A:1804-1810. Children: two compartments per m2 (Mosteller), V 5.09 L/m2, ",
      "k10 0.0206, k12 0.0253, k21 0.0185 /min (Table 2's population means). ",
      "Ultrafilterable carboplatin. https://doi.org/10.1016/0959-8049(95)00382-s")
  )
}
