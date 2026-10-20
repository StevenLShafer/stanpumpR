# -----------------------------------------------------------------------------
# Enoxaparin: subcutaneous two-compartment anti-Xa model in the elderly
# (Berges 2007, PROPHRE.75)
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min, anti-Xa
# activity in IU/mL.  Doses are entered in anti-Xa international units (IU)
# subcutaneously; the model is apparent (CL/F, V/F), so the activity is read
# directly from the SC dose.
#
# SOURCE
# ======
# Berges A, Laporte S, Epinat M, et al. (PROPHRE.75 study group).  Anti-factor
# Xa activity of enoxaparin administered at prophylactic dosage to patients
# over 75 years old.  Br J Clin Pharmacol 2007;64:428-438.  189 patients (mean
# age 82, weight 38-108 kg) on 4000 IU once daily for VTE prophylaxis, sparse
# anti-Xa sampling.  NONMEM; a first-order-input two-compartment model.
# Table 3, final parameter estimates, read from the full text on 2026-10-17:
#
#     ka = 0.63 /h
#     CL (L/h) = 0.70 x (WT/65)^0.78 x (CLcr/69)^0.25
#     V2 (central, L) = 6.43 x (WT/65)^1.25
#     Q  = 0.34 L/h (fixed)       V3 (peripheral, L) = 8.18
#
# with the paper's V2 the central volume and V3 the peripheral, so here
# v1 = V2 and v2 = V3.  CLcr is the simplified MDRD estimate (egfrMDRD(),
# R/renalFunction.R), centred on 69 mL/min; weight is centred on 65 kg.  The
# weight exponents (0.78 on CL, 1.25 on the central volume) are the paper's own
# fit, close to the 0.75 / 1 of standard allometry.
#
# WHAT THIS REPRESENTS
# ====================
# The elderly prophylaxis patient: the fit is in patients over 75, mostly women,
# at a low fixed dose.  The paper found the renal and weight effects
# statistically significant but of slight magnitude (the terminal half-life
# differs by under 1.5 h between a 45 kg woman with CLcr 25 and a 75 kg man with
# CLcr 69).  Extrapolation to younger or heavier patients, or to the curative
# dose, is outside the data.  Bioavailability is 1: the apparent parameters
# already carry the subcutaneous absorbed fraction.
#
# CREATININE CLEARANCE
# ====================
# Simplified MDRD (egfrMDRD()) in mL/min/1.73 m^2, from the Patient Profile's
# serum creatinine on the adult scale (adultEquivalentCreatinine()); with the
# field blank it is the ASSUMED NORMAL creatinine for age and sex, so renal
# impairment is then not represented.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# Clearance and the central volume carry the model's own weight covariate,
# evaluated at the pharmacokinetic weight with the switch on (size$pkWeight) and
# at total body weight with it off, as for cefazolin.  Q and the peripheral
# volume have no size term in the source and take the library's fat-free-mass
# factors with the switch on; with it off they are as published
# (legacyVolume = 1).
#
# EFFECT SITE AND THRESHOLDS
# ==========================
# None: tPeak = 0 and the plotted curve is anti-Xa activity, not an effect
# site.  No band, no MEAC and no time until threshold: a simulated anti-Xa
# activity is not a bleeding or clotting risk or a procedure-safe time.  The
# study's "empirical" 1.0 IU/mL bleeding threshold was defined without clinical
# evaluation and is not drawn.
#
# PREGNANCY
# =========
# Enoxaparin has the best pregnancy pharmacokinetics of any heparin.  Lebaudy
# 2008 (population model, 75 pregnant against 38 nonpregnant women) found
# the apparent clearance higher throughout pregnancy, whatever its stage,
# 0.78 +/- 0.03 against 0.52 +/- 0.03 L/h (0.78 / 0.52 = 1.50), and the
# volume rising in two steps, the second "an additional increase of 41%
# during the last 2 months of pregnancy, independent of changes in weight".
# Patel 2013 (population model, 123 antenatal women) found clearance and
# volume both higher in pregnancy; Casele 1999 (13 women, paired against 6-8
# weeks postpartum) found the anti-Xa AUC and peak lower in pregnancy; and
# the Zarek 2025 review summarises a 1.5-fold rise in apparent clearance, a
# 30% fall in AUC and a 30-50% larger volume.
#
# With the Patient Profile's Pregnant field on, the apparent clearance CL/F
# is multiplied by ENOXAPARIN_PREGNANCY_CL_FACTOR = 1.5 (Lebaudy, 0.78 /
# 0.52) and the apparent central volume by ENOXAPARIN_PREGNANCY_V_FACTOR =
# 1.4 (Lebaudy's weight-independent 41% near term; Berges' weight term
# already carries the part of the rise that follows the woman's entered
# weight).  Both are applied DIRECTLY, not through the creatinine-clearance
# term: renal excretion is only 15-20% of enoxaparin's clearance, so a 1.5-
# fold total rise is not a filtration effect (Zarek: a renal mechanism alone
# would need a 3- to 10-fold rise in renal clearance), and the model's
# (CLcr/69)^0.25 term is left as it is, read from the entered creatinine or
# the assumed normal one.  The peripheral volume, Q and absorption are
# unchanged.  Berges' patients were over 75; the factors are ratios from
# young women applied to that fit.  The numbers are from the abstracts;
# Zarek was read in full.  Claude Code, 2026-10-19.  See R/pregnancy.R.
#
# References
# ----------
# Berges A, Laporte S, Epinat M, et al.  Br J Clin Pharmacol 2007;64:428-438.
#   https://doi.org/10.1111/j.1365-2125.2007.02920.x
# Lebaudy C, Hulot JS, Amoura Z, et al.  Changes in enoxaparin
#   pharmacokinetics during pregnancy and implications for antithrombotic
#   therapeutic strategy.  Clin Pharmacol Ther 2008;84:370-377.
#   https://doi.org/10.1038/clpt.2008.73 (pregnancy)
# Patel JP, Green B, Patel RK, Marsh MS, Davies JG, Arya R.  Circulation
#   2013;128:1462-1469.  https://doi.org/10.1161/CIRCULATIONAHA.113.003198
#   (pregnancy)
# Casele HL, Laifer SA, Woelkers DA, Venkataramanan R.  Am J Obstet Gynecol
#   1999;181:1113-1117.  https://doi.org/10.1016/s0002-9378(99)70091-8
#   (pregnancy)
# Zarek J, Brydon A, Spitzer K, Laskin C, Ito S.  EULAR Rheumatol Open
#   2025;1:11-22.  https://doi.org/10.1016/j.ero.2025.03.001 (pregnancy)
#
# (Claude Code, 2026-10-17, at the request of Steven L. Shafer.)
# -----------------------------------------------------------------------------

ENOXAPARIN_WT_REF   <- 65     # kg, reference weight
ENOXAPARIN_CLCR_REF <- 69     # mL/min, reference MDRD creatinine clearance

# Pregnancy (see the header), applied directly, not through the renal term.
# Lebaudy 2008: apparent clearance 0.78 / 0.52 L/h = 1.50; the volume a
# further 41% in the last two months, independent of weight, so 1.4 on the
# apparent central volume.
ENOXAPARIN_PREGNANCY_CL_FACTOR <- 1.5
ENOXAPARIN_PREGNANCY_V_FACTOR  <- 1.4

#' Enoxaparin pharmacokinetics (subcutaneous, anti-Xa, elderly)
#'
#' Berges 2007; see the header of \code{R/drugs_enoxaparin.R}.
#'
#' @inheritParams apixaban
#' @param pregnant when \code{TRUE}, apply the changes of late pregnancy:
#'   apparent clearance x 1.5 and apparent central volume x 1.4 (see the
#'   file's header)
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
enoxaparin <- function(weight, height, age, sex, adjustToFFM = TRUE,
                       creatinine = NULL, pregnant = FALSE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1)
  pkW  <- if (isTRUE(adjustToFFM)) size$pkWeight else weight

  crcl <- egfrMDRD(age, sex, adultEquivalentCreatinine(creatinine, age, sex))

  # Pregnancy (see the header): CL/F and the central V/F, directly.
  clPregnancy <- if (isTRUE(pregnant)) ENOXAPARIN_PREGNANCY_CL_FACTOR else 1
  vPregnancy  <- if (isTRUE(pregnant)) ENOXAPARIN_PREGNANCY_V_FACTOR else 1

  cl <- 0.70 * (pkW / ENOXAPARIN_WT_REF)^0.78 *
    (crcl / ENOXAPARIN_CLCR_REF)^0.25 * clPregnancy            # L/h

  default <- list(
    v1  = 6.43 * (pkW / ENOXAPARIN_WT_REF)^1.25 * vPregnancy,  # central (V2)
    v2  = 8.18 * size$volume,                                  # peripheral (V3)
    v3  = 1,                                                   # two compartments
    cl1 = cl / 60,
    cl2 = 0.34 / 60 * size$clearance,                          # Q
    cl3 = 0,
    ka_SC = 0.63 / 60,                                         # 1/min
    tlag_SC = 0,
    bioavailability_SC = 1                                     # apparent scale
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Berges A et al., Br J Clin Pharmacol 2007;64:428-438 (PROPHRE.75: ",
    "subcutaneous first-order-input two-compartment anti-Xa model in patients ",
    "over 75; clearance on weight and simplified-MDRD creatinine clearance, ",
    "central volume on weight; apparent parameters, from the entered ",
    "creatinine or an assumed normal value for age and sex). ",
    "https://doi.org/10.1111/j.1365-2125.2007.02920.x"
  )
  if (isTRUE(pregnant)) {
    reference <- paste0(reference, "; ", pregnancyReference(
      paste0("apparent clearance x ", ENOXAPARIN_PREGNANCY_CL_FACTOR,
             ", apparent central volume x ", ENOXAPARIN_PREGNANCY_V_FACTOR),
      paste0("Lebaudy C et al., Clin Pharmacol Ther 2008;84:370-377. ",
             "https://doi.org/10.1038/clpt.2008.73")))
  }

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
