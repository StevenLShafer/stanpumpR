# -----------------------------------------------------------------------------
# Paclitaxel, intravenous, solvent-based (Crombag 2019): total plasma
# paclitaxel, three compartments with saturable elimination and saturable
# distribution, in 684 adults
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min; amounts in
# mg and concentrations in mcg/mL (= mg/L).  The source works in umol and
# hours, converted at 853.9 g/mol as in R/drugs_paclitaxelJoerger.R.
#
# Drafted by Claude Code, 2026-11-13, at the request of Steven L. Shafer, who
# asked for this paper (stanpumpR-papers-needed.csv, priority 1, the
# 684-patient model Janssen 2021 refitted) and had it added to the papers
# archive (PMID_30617624).  Every number below was read on the pages of the
# PDF.  A candidate of the paclitaxel group (R/modelSelection.R): fitted to
# four times as many patients as Joerger 2006, the program chooses it for
# adults within its 18-84 years.
#
# THE SOURCE
# ==========
# Crombag MRBS, de Vries Schultink AHM, Koolen SLW, Wijngaard S, Joerger M,
# Schellens JHM, Dorlo TPC, van Erp NP, Mathijssen RHJ, Beijnen JH, Huitema
# ADR.  Impact of older age on the exposure of paclitaxel: a population
# pharmacokinetic study.  Pharm Res 2019;36:33 (doi
# 10.1007/s11095-018-2563-6).  684 adults (383 women; 18-84 years, median 61;
# 166 aged 70 or more; BSA median 1.8 m2, IQR 1.7-2.0; bilirubin median 7
# umol/L) from the NKI and Erasmus MC trials (Joerger 2006's patients among
# them) and a prospective cohort of older patients, given paclitaxel as 1- or
# 3-h infusions (38-290 mg/m2; 24-h infusions excluded); HPLC-MS/MS; NONMEM
# 7.3, FOCE-I.
#
# THE MODEL (Equations 1-3 and 5-6; Table II, "Covariate model + AGE")
# ===================================================================
# Joerger's structure (the authors' ref 13, Joerger 2012):
#
#   dA1/dt = - VMEL C1/(KMEL + C1) - VMTR C1/(KMTR + C1) + K21 A2
#              - Q C1 + Q A3/V3                (plus the infusion)
#   dA2/dt = VMTR C1/(KMTR + C1) - K21 A2      (an amount: no volume)
#   dA3/dt = Q C1 - Q A3/V3
#
#   VMEL = 31.7 umol/h x 1.12^MALE x (BSA/1.8)^1.41 x (bilirubin/7)^-0.17
#          x (age/61)^-0.31
#   V1 12.0 L   V3 267 L   KMEL 0.46 umol/L   VMTR 177 umol/h
#   KMTR 1.63 umol/L   K21 1.20 /h   Q 16.6 L/h
#
# Equation 5's Theta1 is "the maximal elimination rate in females" and
# Theta2 the male multiplier; the continuous covariates are centred on the
# study's medians (Methods), Table I printing BSA 1.8 m2, bilirubin 7 umol/L
# and age 61 years.  Unlike Joerger 2006, only VMEL carries covariates.
#
# CHECKS (test-drugs-paclitaxelCrombag.R): the text's "decrease in VMEL of 5%
# for a 10-year increment from the median age" ((71/61)^-0.31 = 0.954); and
# the time above 0.05 uM after 80 mg/m2 over 1 h rising by less than 10% for
# ten years of age (the text: 6%).
#
# HELD AT THE REFERENCE: total bilirubin at the median, 7 umol/L (the Patient
# Profile has no bilirubin), as in Joerger 2006.
#
# AGE: a negative power of age, floored at 18 (ADULT_AGE_TERM_FLOOR, the
# source's youngest patient).
#
# DOSE BASIS: paclitaxel has no salt; doses in mg of paclitaxel.  Solvent-based
# (Cremophor) paclitaxel only, not nab-paclitaxel.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# The model's own covariate, BSA (Mosteller, the source naming none), on
# VMEL alone, at the pharmacokinetic weight with the switch on and total
# weight with it off; VMEL takes no other scaling.  V1, V3, VMTR and Q carry
# no covariate.  This file said until 2026-11-19 that "the source tested size
# and kept it on VMEL only", and held them FIXED on that ground: that was
# wrong.  Crombag took the covariate model (sex, BSA and bilirubin on VMEL)
# from Joerger 2012 (her ref 13) and evaluated only age (Methods, p3; the
# Discussion, p7: "thorough evaluation of other covariates ... [was] not
# within the scope of this study"); size was never tested on V1, V3, VMTR or
# Q in these data.  By Shafer's first-principles rule of 2026-11-19
# (docs/weight-adjustment.md, *Every parameter is scaled*) they are scaled
# with the switch on: V1 and V3 by the fat-free-mass volume factor, VMTR (a
# maximal rate) and Q by the clearance factor, anchored at Crombag's cohort
# (PACLITAXEL_CROMBAG_COHORT, below; its typical patient sits at a volume
# factor of 0.885 and a clearance factor of 0.913 from the 70 kg reference
# man), so a patient like Crombag's receives the published values.  None of
# the four has a sex term, so they are anchored at the cohort's pooled
# patient.  The cohort's weight and height are DERIVED, Table I printing
# neither (see the constant).  With the switch off they are unscaled, as
# before.  KMEL and KMTR (concentrations) and K21 (a rate constant) do not
# scale.
#
# PREGNANCY (R/pregnancy.R)
# =========================
# Janssen 2021 (Clin Pharmacokinet 60:775-784, doi 10.1007/s40262-020-00961-4,
# PDF read 2026-11-10) refitted THIS model's data set (689 nonpregnant
# patients) with 20 pregnant women: KMEL x 1.32, V1 x 2.27 and V3 x 2.28
# (Table 2), here on the same parameters, so no correspondence is needed.
# Applied under `pregnant` (Shafer, 2026-11-12).
# NO EFFECT SITE, NO BAND.
# -----------------------------------------------------------------------------

# Crombag 2019, Table II, covariate model + AGE: the typical woman at BSA 1.8
# m2, bilirubin 7 umol/L and age 61; amounts in umol, time in hours
PACLITAXEL_CROMBAG <- list(
  vmel = 31.7, vmelMale = 1.12, vmelBsa = 1.41, vmelBili = -0.17, vmelAge = -0.31,
  v1 = 12.0, v3 = 267,                          # L
  kmel = 0.46,                                  # umol/L
  vmtr = 177, kmtr = 1.63,                      # umol/h, umol/L
  k21 = 1.20,                                   # /h
  q = 16.6,                                     # L/h
  bsaReference = 1.8, ageReference = 61, bilirubinReference = 7,
  bilirubin = 7                                 # held: no Patient Profile field
)

# Crombag 2019, Table I (p5), read on the PDF 2026-11-19: 684 patients, 383
# women (56%) and 301 men; median age 61 years; BSA median 1.8 m2 (IQR
# 1.7-2.0).  No weight or height is printed, and the paper names no BSA
# formula.  The patients are Dutch (NKI Amsterdam, Erasmus MC Rotterdam,
# Radboud UMC Nijmegen).  Derived: NCD-RisC's mean height at 18 for the
# Netherlands, birth cohort 1958 (2019 less 61), less the loss with age to 61
# years, is 179.9 cm for the men and 166.3 cm for the women; their mean
# weighted by the 301 men and 383 women is 172.3 cm, and the weight that gives
# BSA 1.8 m2 at that height by Du Bois's formula is 67.6 kg (BMI 22.8).  As a
# check, Joerger 2006's 168 Dutch patients, among Crombag's, measured 68.5 kg
# at BSA 1.80 m2 (PACLITAXEL_JOERGER_COHORT).  Birth cohort 1945 (the data's
# collection, 1990s-2017, rather than the publication) gives 170.2 cm and
# 69.0 kg and moves the volume factor by 0.2%.  V1, V3, VMTR and Q are
# anchored at this cohort (Steven L. Shafer, 2026-11-19; see the header's
# BODY SIZE section).
PACLITAXEL_CROMBAG_COHORT <- list(weight = 67.6, height = 172.3, age = 61,
                                  men = 301, women = 383)

#' Paclitaxel, intravenous, solvent-based (Crombag 2019)
#'
#' Joerger's three-compartment structure with saturable elimination and
#' saturable distribution, refitted in 684 adults; sex, body-surface area and
#' age on the maximal elimination rate only.  See the header of
#' \code{R/drugs_paclitaxelCrombag.R}.
#'
#' @inheritParams cefazolin
#' @param pregnant TRUE in late pregnancy: Janssen 2021's factors
#' @returns a list in the shape \code{getDrugPK()} expects, with a
#'   \code{michaelisMenten} block
#' @export
paclitaxelCrombag <- function(weight, height, age, sex, adjustToFFM = TRUE, pregnant = FALSE)
{
  p <- PACLITAXEL_CROMBAG
  preg <- if (isTRUE(pregnant)) PACLITAXEL_PREGNANCY else list(km = 1, v1 = 1, vLinear = 1)
  # V1, V3, VMTR and Q, with no covariate, are scaled and anchored at
  # Crombag's cohort with the switch on; VMEL carries the source's BSA term at
  # the pharmacokinetic weight (which does not depend on the cohort) and takes
  # no size factor (see the header's BODY SIZE section).
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume = 1, legacyClearance = 1,
                        cohort = do.call(cohortSizeFactors, PACLITAXEL_CROMBAG_COHORT))
  w <- if (isTRUE(adjustToFFM)) size$pkWeight else weight
  bsa <- bsaMosteller(w, height) / p$bsaReference
  male <- if (identical(sex, "male")) 1 else 0
  ageTerm <- max(age, ADULT_AGE_TERM_FLOOR) / p$ageReference
  mg <- PACLITAXEL_MW / 1000                       # mg per umol

  vmel <- p$vmel * p$vmelMale^male * bsa^p$vmelBsa *
    (p$bilirubin / p$bilirubinReference)^p$vmelBili * ageTerm^p$vmelAge  # umol/h
  vmax <- vmel * mg / 60                           # mg/min
  km <- p$kmel * mg * preg$km                      # mg/L

  default <- list(
    v1 = p$v1 * size$volume * preg$v1,       # no covariate: anchored
    v2 = p$v3 * size$volume * preg$vLinear,  # the source's V3, linear
    v3 = 1,                                  # placeholder: see the header
    # The low-concentration limit of the saturable elimination, Vmax / Km,
    # for the help tables only: the simulation uses the michaelisMenten block.
    cl1 = vmax / km,
    cl2 = p$q / 60 * size$clearance,
    cl3 = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Crombag MRBS, de Vries Schultink AHM, Koolen SLW, et al. Impact of older age on the ",
    "exposure of paclitaxel: a population pharmacokinetic study. Pharm Res 2019;36:33. ",
    "684 adults 18-84 years: three compartments, saturable elimination (VMEL 31.7 umol/h ",
    "x 1.12^male x (BSA/1.8)^1.41 x (bilirubin/7)^-0.17 x (age/61)^-0.31, KMEL 0.46 ",
    "umol/L), saturable distribution to a first peripheral compartment (VMTR 177 umol/h, ",
    "KMTR 1.63 umol/L, K21 1.20 /h) and linear distribution to a second (Q 16.6 L/h, V3 ",
    "267 L), V1 12.0 L; V1, V3, VMTR and Q at the cohort's typical patient, scaled to ",
    "fat-free mass with the switch on (Mosteller BSA; bilirubin held at 7 umol/L). Solvent-based ",
    "paclitaxel only. Total plasma paclitaxel. https://doi.org/10.1007/s11095-018-2563-6"
  )
  if (isTRUE(pregnant))
    reference <- paste(reference, pregnancyReference("KMEL x 1.32, V1 x 2.27, V3 x 2.28",
      "Janssen 2021, Clin Pharmacokinet 60:775-784, https://doi.org/10.1007/s40262-020-00961-4"))

  list(
    PK = PK,
    tPeak = 0,
    MEAC = 0,
    typical = 0,
    upperTypical = 0,
    lowerTypical = 0,
    reference = reference,
    michaelisMenten = list(
      vmax = vmax, km = km,
      saturableDistribution = list(
        vmax = p$vmtr * mg / 60 * size$clearance,   # mg/min
        km = p$kmtr * mg,                        # mg/L
        k21 = p$k21 / 60                         # /min
      )
    )
  )
}
