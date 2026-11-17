# -----------------------------------------------------------------------------
# Tadalafil (Adcirca, Cialis): one compartment, apparent oral, from Ferguson-
# Sells 2021's combined adult and paediatric model in pulmonary arterial
# hypertension, and, for a patient without it, from the Cialis label
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
# dysfunction, benign prostatic hyperplasia), no population model was found:
# PubMed (2026-11-17, tadalafil with population pharmacokinetics or a
# pharmacokinetic model, and healthy subjects, erectile dysfunction or BPH)
# gave none, and the model of men with erectile dysfunction that the FDA's
# Adcirca review (NDA 22-332, p.79) says informed its structure is in the
# Cialis submission, which has not been obtained.  So that population's
# model is DERIVED from the Cialis label, read in full (section 12.3, healthy
# subjects): one compartment (the label: "concentrations declined nearly
# monoexponentially", Forgue 2006), with
#
#     CL/F 2.5 L/h ("mean oral clearance")
#     V/F 63 L ("mean apparent volume of distribution ... approximately 63 L")
#     ka 2.0 /h, solved so the peak falls at the label's median 2 hours
#         (TADALAFIL_HEALTHY_TMAX; tadalafilHealthyKa())
#
# no lag, and bioavailability 1 (apparent), with no dose term (the label:
# "over a dose range of 2.5 to 20 mg, tadalafil exposure (AUC) increases
# proportionally with dose").  Its half-life is 17.5 h, the label's, and its
# accumulation on a daily dose 1.63 (label: "approximately 1.6-fold"), both
# following from the two values, not fitted to them.  Forgue 2006 (Br J Clin
# Pharmacol 61:280, the integrated analysis of 237 healthy subjects behind
# the label; abstract read) reports a mean peak of 378 ng/mL after 20 mg
# where this gives 293: a terminal volume puts the peak low, and a second
# compartment would be needed to raise it, which no source gives.  Not
# applied: the label's 25% higher exposure in men of 65 or more, and its
# doubled exposure in renal impairment.  So the two populations differ in
# clearance (1.88 against 2.5 L/h), volume (88.1 against 63 L at 70 kg) and
# absorption (0.86 against 2.0 /h), and the PAH half-life is about twice
# the healthy one (32.5 against 17.5 h), as the labels say (35 and 17.5 h).
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

# Without pulmonary hypertension: the Cialis label, section 12.3
TADALAFIL_HEALTHY_CL   <- 2.5   # L/h, CL/F
TADALAFIL_HEALTHY_V    <- 63    # L, V/F
TADALAFIL_HEALTHY_TMAX <- 2     # h, the median time of the peak

#' Tadalafil's absorption rate without pulmonary hypertension: the ka that
#' puts a one-compartment peak at the Cialis label's median 2 hours
#' @returns ka, /h
#' @keywords internal
tadalafilHealthyKa <- function()
{
  k <- TADALAFIL_HEALTHY_CL / TADALAFIL_HEALTHY_V
  stats::uniroot(function(ka) log(ka / k) / (ka - k) - TADALAFIL_HEALTHY_TMAX,
                 c(k * 1.01, 100), tol = 1e-10)$root
}

#' Tadalafil (Ferguson-Sells 2022; the Cialis label)
#'
#' One compartment, apparent oral: in pulmonary arterial hypertension without
#' bosentan (Ferguson-Sells 2022), and otherwise derived from the Cialis
#' label; see the header of \code{R/drugs_tadalafil.R}.
#'
#' @inheritParams cefazolin
#' @param pulmonaryHypertension TRUE for a patient with pulmonary arterial
#'   hypertension (Ferguson-Sells 2022), FALSE (the default) for the Cialis
#'   label's healthy subjects
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

#' Tadalafil without pulmonary hypertension, derived from the Cialis label
#' @inheritParams tadalafil
#' @keywords internal
tadalafilHealthy <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume = 1, legacyClearance = 1)
  default <- list(
    v1  = TADALAFIL_HEALTHY_V * size$volume,
    v2  = 1,
    v3  = 1,
    cl1 = TADALAFIL_HEALTHY_CL / 60 * size$clearance,
    cl2 = 0,
    cl3 = 0,
    ka_PO = tadalafilHealthyKa() / 60,
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
      "Without pulmonary hypertension, derived from the Cialis label (section 12.3, ",
      "healthy subjects; no population model found): one compartment, CL/F 2.5 L/h, ",
      "V/F 63 L, ka 2.0 /h solved for the median peak at 2 h, no lag. ",
      "With pulmonary hypertension the source is Ferguson-Sells 2022 ",
      "(https://doi.org/10.1007/s40262-021-01052-8).")
  )
}
