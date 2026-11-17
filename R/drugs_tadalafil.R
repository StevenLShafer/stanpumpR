# -----------------------------------------------------------------------------
# Tadalafil (Adcirca, Cialis): one compartment, apparent oral, from Ferguson-
# Sells 2021's combined adult and paediatric model in pulmonary arterial
# hypertension, and, for a patient without it, the FDA's model in men with
# erectile dysfunction
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
# bosentan; this model gives 3.23 / 0.817 = 3.95.
#
# WITHOUT PULMONARY HYPERTENSION (the Patient Profile's field)
# ============================================================
# Ferguson-Sells's model is the PAH population's, and is what a patient with
# the Patient Profile's Pulmonary hypertension set gets (Steven L. Shafer,
# 2026-11-17: "very different PK in the two populations, as patients with
# PAH often have severe cardiac disease").  Without it (Cialis: erectile
# dysfunction, benign prostatic hyperplasia) the model is the population
# model of study H6D-MC-LVBF, 179 men with erectile dysfunction given 2 to 25
# mg, as the FDA's Biopharmaceutics Review of the Cialis NDA 21-368 (2002-
# 2003, part 4, p.3, the scanned page read) prints its final estimates:
#
#     one compartment, first-order absorption and elimination
#     CL/F 2.24 L/h (SE 5.8%, IIV 45.3%), in men without "current active
#         cardiovascular conditions"; 1.59 L/h with them (not applied)
#     V/F 60.0 L (SE 9.88%)
#     ka 1.75 /h, fixed ("value estimated from healthy volunteers")
#
# no lag, bioavailability 1 (apparent).  Of the review's four population
# analyses it is the one printed in full in men with ED: LVBG fixed ka at
# 0.393 /h and printed no clearance; LVAC (two compartments, CL/F 1.46 L/h,
# Vss/F 125 L) printed neither Vc nor Q; LVCE's estimates are not printed.
# The review's own refit added creatinine clearance (CL/F 1.09 + 0.00898 x
# CRCL L/h, the unit of CRCL unprinted), which is not applied.  Active
# cardiovascular disease lowered clearance 29% (1.59 against 2.24 L/h); the
# Patient Profile has no field for it, so it is not applied either.  Until
# 2026-11-17 this arm was derived from the Cialis label (CL/F 2.5 L/h, V/F 63
# L, ka 2.0 /h), which this replaces.
#
# Checks (the label, healthy subjects): CL/F "2.5 L/hr", V/F "approximately 63
# L", half-life "17.5 hours", median peak at 2 hours and accumulation
# "approximately 1.6-fold" on a daily dose; this model gives 18.6 h, a peak at
# 2.2 h and 1.69.  After 20 mg its peak is 306 ng/mL; Forgue 2006 (Br J Clin
# Pharmacol 61:280, healthy subjects, abstract read) reports a mean of 378.
# So the two populations differ in clearance (1.88 against 2.24 L/h), volume
# (88.1 against 60 L at 70 kg) and absorption (0.86 against 1.75 /h), and the
# PAH half-life is about 1.7 times the other (32.5 against 18.6 h).  Not
# applied: the label's 25% higher exposure in men of 65 or more, and its
# doubled exposure in renal impairment.
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

# Without pulmonary hypertension: study LVBF, men with erectile dysfunction
# (FDA Biopharmaceutics Review of NDA 21-368, part 4, p.3)
TADALAFIL_ED_CL <- 2.24   # L/h, CL/F without active cardiovascular disease
TADALAFIL_ED_V  <- 60.0   # L, V/F
TADALAFIL_ED_KA <- 1.75   # /h, fixed from healthy volunteers

#' Tadalafil (Ferguson-Sells 2022; the FDA review of NDA 21-368)
#'
#' One compartment, apparent oral: in pulmonary arterial hypertension without
#' bosentan (Ferguson-Sells 2022), and otherwise the FDA review's model in men
#' with erectile dysfunction (study LVBF); see the header of
#' \code{R/drugs_tadalafil.R}.
#'
#' @inheritParams cefazolin
#' @param pulmonaryHypertension TRUE for a patient with pulmonary arterial
#'   hypertension (Ferguson-Sells 2022), FALSE (the default) for men with
#'   erectile dysfunction (study LVBF)
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
tadalafil <- function(weight, height, age, sex, adjustToFFM = TRUE,
                      pulmonaryHypertension = FALSE)
{
  if (!isTRUE(pulmonaryHypertension))
    return(tadalafilHealthy(weight, height, age, sex, adjustToFFM))
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

#' Tadalafil without pulmonary hypertension: study LVBF, men with erectile
#' dysfunction (FDA Biopharmaceutics Review of NDA 21-368)
#' @inheritParams tadalafil
#' @keywords internal
tadalafilHealthy <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume = 1, legacyClearance = 1)
  default <- list(
    v1  = TADALAFIL_ED_V * size$volume,
    v2  = 1,
    v3  = 1,
    cl1 = TADALAFIL_ED_CL / 60 * size$clearance,
    cl2 = 0,
    cl3 = 0,
    ka_PO = TADALAFIL_ED_KA / 60,
    bioavailability_PO = 1,        # apparent parameters
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
    reference = paste0(
      "Without pulmonary hypertension: study H6D-MC-LVBF, 179 men with erectile ",
      "dysfunction, FDA Biopharmaceutics Review of NDA 21-368 (Cialis), part 4, p.3: ",
      "one compartment, CL/F 2.24 L/h (no active cardiovascular disease), V/F 60.0 L, ",
      "ka 1.75 /h fixed, no lag. ",
      "https://www.accessdata.fda.gov/drugsatfda_docs/nda/2003/21-368_Cialis_BioPharmr_P4.pdf. ",
      "With pulmonary hypertension the source is Ferguson-Sells 2022 ",
      "(https://doi.org/10.1007/s40262-021-01052-8).")
  )
}
