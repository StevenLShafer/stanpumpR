# -----------------------------------------------------------------------------
# Tadalafil (Adcirca, Cialis): one compartment, apparent oral, from Ferguson-
# Sells 2021's combined adult and paediatric model in pulmonary arterial
# hypertension
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min; amounts in
# mg, so the plotted concentration is ng/mL (= mcg/L), total plasma.
#
# Drafted by Claude Code, 2026-11-15, at the request of Steven L. Shafer, who
# asked for the drugs of pulmonary arterial hypertension.  Source, read in
# full (PMC8813705): Ferguson-Sells L, Velez de Mendizabal N, Li B, Small D.
# Population pharmacokinetics of tadalafil in pediatric patients with
# pulmonary arterial hypertension: a combined adult/pediatric model.  Clin
# Pharmacokinet 2022;61:249-262.  doi 10.1007/s40262-021-01052-8.
#
# THE SOURCE
# ==========
# 305 adults of PHIRST-1 (median age 53.9, range 14.7-90.3 years; weight 73.0,
# 41.4-140 kg; 2.5 to 40 mg once daily, sparse sampling) and 19 children of
# LVIG (2.5 to 17.6 years), LC-MS/MS.  One compartment with first-order
# absorption, NONMEM.  Table 2 and its footnotes:
#
#     ka 0.860 /h
#     CL/F 3.23 L/h with bosentan; "Effect of non-bosentan" -0.418, so
#         "Not taking bosentan (calculated)" 1.88 L/h
#     V/F 88.1 L x (WT/70)^1 (exponent fixed)
#     F = 1 x (DOSE/16.27)^-0.227 x (AGE/52.4)^0.100
#
# Apparent values: "Absolute bioavailability of tadalafil following oral
# dosing has not been determined" (Adcirca label).  F is the source's relative
# bioavailability; its dose term is the library's oralSaturation block of the
# power form (oralSaturationFraction(), R/routes.R), its age term
# bioavailability_PO.  Clearance carries no size term.  Bosentan is held at
# none (CL/F 1.88 L/h); the patient is assumed not to take it.
#
# CHECKS
# ======
# The Adcirca label (from the PHIRST-1 data, so not independent): in
# pulmonary hypertension without bosentan, CL/F 1.6 L/h and terminal
# half-life 35 h; this model gives 1.88 L/h relative to the 16.27 mg dose
# (2.30 L/h at 40 mg) and 32.5 h.  Table 3, the source's own simulation of
# a typical adult (70 kg, 54 years) on 40 mg, gives CL/F 4.05 L/h WITH
# bosentan; this model gives 3.23 / 0.817 = 3.95.  Healthy subjects clear
# tadalafil faster (labels: CL/F 2.5 L/h, half-life 17.5 h (Cialis); 3.4
# L/h, 15 h at 40 mg (Adcirca)), so this model over-predicts the healthy
# man's exposure by about a third: the decision whether to offer the healthy
# value is recorded in the PR (2026-11-15) and on the help page.
#
# AGE FLOOR
# =========
# The age term is a positive power (0 at age 0).  The source fitted it in
# children from 2.5 years (Table 1) and says it "cannot be extrapolated to ages
# younger than those assessed"; by the library's rule it is held at the
# youngest age studied, 2.5 years (TADALAFIL_MIN_AGE).
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# The source's own weight term on V/F, evaluated at the pharmacokinetic weight
# with the switch on and at total weight with it off.  Clearance has none.
#
# DOSE
# ====
# Adcirca: "Each tablet contains 20 mg of tadalafil"; no salt.  Renal
# impairment (AUC doubled in mild or moderate impairment, Adcirca label) is
# not in the source's model and is not applied.
#
# PREGNANCY AND THE BAND
# ======================
# No pregnancy pharmacokinetics (registry row).  No therapeutic range.
# -----------------------------------------------------------------------------

# Ferguson-Sells 2022, Table 2
TADALAFIL_KA        <- 0.860  # /h
TADALAFIL_CL_BOS    <- 3.23   # L/h, with bosentan
TADALAFIL_NO_BOS    <- -0.418 # fractional change without bosentan
TADALAFIL_V         <- 88.1   # L at 70 kg
TADALAFIL_WT_REF    <- 70     # kg
TADALAFIL_F_DOSE_REF <- 16.27 # mg
TADALAFIL_F_DOSE_EXP <- -0.227
TADALAFIL_F_AGE_REF  <- 52.4  # years
TADALAFIL_F_AGE_EXP  <- 0.100
TADALAFIL_MIN_AGE    <- 2.5   # years, the youngest the source studied

#' Tadalafil (Ferguson-Sells 2022)
#'
#' One compartment, apparent oral, in pulmonary arterial hypertension without
#' bosentan; see the header of \code{R/drugs_tadalafil.R}.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
tadalafil <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)
  wt <- if (isTRUE(adjustToFFM)) size$pkWeight else weight
  cl <- TADALAFIL_CL_BOS * (1 + TADALAFIL_NO_BOS)
  fAge <- (max(age, TADALAFIL_MIN_AGE) / TADALAFIL_F_AGE_REF)^TADALAFIL_F_AGE_EXP

  default <- list(
    v1  = TADALAFIL_V * wt / TADALAFIL_WT_REF,
    v2  = 1,
    v3  = 1,
    cl1 = cl / 60,
    cl2 = 0,
    cl3 = 0,
    ka_PO = TADALAFIL_KA / 60,
    bioavailability_PO = fAge,     # relative; the dose term is below
    tlag_PO = 0
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
    oralSaturation = list(form = ORAL_SATURATION_POWER,
                          Dref = TADALAFIL_F_DOSE_REF,
                          exponent = TADALAFIL_F_DOSE_EXP),
    reference = paste0(
      "Ferguson-Sells L, Velez de Mendizabal N, Li B, Small D, Clin Pharmacokinet ",
      "2022;61:249-262, Table 2: one compartment in pulmonary arterial hypertension, ",
      "ka 0.860 /h, CL/F 1.88 L/h without bosentan, V/F 88.1 L x weight/70, relative ",
      "F (dose/16.27 mg)^-0.227 x (age/52.4)^0.100. ",
      "https://doi.org/10.1007/s40262-021-01052-8")
  )
}
