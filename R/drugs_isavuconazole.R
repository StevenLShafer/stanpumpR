# -----------------------------------------------------------------------------
# Isavuconazole (Cresemba): intravenous and oral, two compartments, Weibull
# oral absorption discretised into pulses (Desai 2016)
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min, amounts in mg
# of isavuconazonium sulfate (the product as labelled), concentrations in
# mcg/mL (= mg/L) of isavuconazole in plasma.  Concentration only.  Drafted by
# Claude Code, 2026-11-28, on Steven L. Shafer's decisions of that day
# ("Approximate oral absorption for Isavuconazole"; "Use Vp 390 L, say so");
# docs/library-expansion/plan.csv, Antifungals.
#
# SOURCE
# ======
# Desai A, Kovanda L, Kowalski D, Lu Q, Townsend R, Bonate PL.  Population
# pharmacokinetics of isavuconazole from phase 1 and phase 3 (SECURE) trials in
# adults and target attainment in patients with invasive infections due to
# Aspergillus and other filamentous fungi.  Antimicrob Agents Chemother
# 2016;60:5483-5491 (PMID 27381396; card papers/digests/PMID_27381396.md; read
# on the PDF, Table 5 on the page).  421 adults, 189 healthy (nine phase 1
# studies, single and multiple doses, oral and 1-h intravenous) and 232
# patients with invasive fungal infections (SECURE: 200 mg every 8 h for six
# doses, then 200 mg daily), 6,363 concentrations.  NONMEM, ADVAN4 TRANS4,
# F fixed to 1, so intravenous and oral doses share every parameter.
#
# Table 5 (best covariate model), in isavuconazole:
#
#     CL = 2.36 L/h (predominantly Caucasian; 1.51 L/h Asian)
#     V1 = 49.1 L,  Q = 26.6 L/h
#     Vp = 390 L   (see below)
#     oral input: dA/dt = -WB A,  WB = KAMAX [1 - exp(-(RA t)^GAM)],
#                 KAMAX 1.08 /h, RA 0.72 /h, GAM 4.88, t the time after dose
#
# Vp.  The paper's covariate equation for Vp (theta4 417 L, theta11 260 L for
# the study population, theta10 0.060 on BMI - 24.80) does not reproduce the
# typical values its Discussion gives, about 292 L in healthy subjects and
# 390 L in patients.  On Shafer's decision of 2026-11-28 the model uses the
# patients' 390 L and no BMI term.  The paper's own half-lives support it:
# with the other parameters, Vp 390 L gives a terminal half-life of 138 h
# against the patients' reported mean of 130 h (417 L gives 147 h).
#
# NOT APPLIED.  Race: the Patient Profile has no race field, so clearance is
# the predominantly Caucasian 2.36 L/h; Asian patients cleared it 36% slower
# (1.51 L/h), which the help page says.
#
# THE WEIBULL INPUT, DISCRETISED.  The engine has no Weibull depot, so each
# "mg PO" dose is split into ISAVUCONAZOLE_PULSE_FRACTION at
# ISAVUCONAZOLE_PULSE_DELAY, each absorbed at 1.08 /h (oralPulses with the
# default-form key, R/oral-pulses.R, as hydrochlorothiazide's Weibull is).
# 1.08 /h is KAMAX, the Weibull's own asymptotic rate constant: as t grows
# WB -> KAMAX and the depot empties exactly as one dose lagged by
# KAMAX Gamma(1 + 1/GAM) / RA / KAMAX = 1.27 h, so only the upstroke has to be
# approximated, which the pulses' fractions and delays do.  They were fitted
# MINIMAX to the plasma curve of the reference patient against an exact
# integration of the Weibull system (data-raw/isavuconazolePulses.R), held to
# hydrochlorothiazide's standard, 0.53% of the peak over the curve and 0.01%
# at the peak; the test checks the shipped curve against an independent
# integration at three patients.  The pulses describe nothing about the
# capsule.
#
# BODY SIZE.  Desai retained no size covariate on CL, V1 or Q (the BMI term on
# Vp is not applied, above), so every parameter takes the package's FFM
# factors anchored at the source's cohort: 189 healthy subjects (77.8 kg,
# 175 cm, 43 years, 140 men, 49 women) and 232 patients (67.0 kg, 168.4 cm,
# 54 years, 132 men, 100 women), Table 3 medians.  The switch off gives the
# published values.
#
# DOSE BASIS.  Desai's doses are isavuconazole (372 mg isavuconazonium sulfate
# = 200 mg isavuconazole, the paper's Methods and the label); Cresemba is
# labelled and prescribed in isavuconazonium sulfate.  Doses are entered as
# labelled, so every volume and clearance is multiplied by 372/200 to give
# isavuconazole concentrations from sulfate amounts.
#
# References
# ----------
# Desai A et al., Antimicrob Agents Chemother 2016;60:5483-5491.
#   https://doi.org/10.1128/AAC.02819-15
# CRESEMBA (isavuconazonium sulfate) capsules and for injection, prescribing
#   information, Astellas, NDA 207500/207501, DailyMed version of 2025-05-05
#   (setid 8f7f73b8-586a-4df0-935f-fecd4696c16c).
# -----------------------------------------------------------------------------
ISAVUCONAZOLE_CL  <- 2.36     # L/h, predominantly Caucasian
ISAVUCONAZOLE_V1  <- 49.1     # L
ISAVUCONAZOLE_Q   <- 26.6     # L/h
ISAVUCONAZOLE_VP  <- 390      # L, the Discussion's patients (see the header)
ISAVUCONAZOLE_KAMAX <- 1.08   # 1/h, the Weibull's asymptote
ISAVUCONAZOLE_WEIBULL_RA  <- 0.72  # 1/h, NOT a rate constant
ISAVUCONAZOLE_WEIBULL_GAM <- 4.88
# mg isavuconazonium sulfate per mg isavuconazole
ISAVUCONAZOLE_SALT <- 372 / 200
ISAVUCONAZOLE_COHORT <- list(groups = list(
  list(weight = 77.8, height = 175, age = 43, men = 140, women = 49),
  list(weight = 67.0, height = 168.4, age = 54, men = 132, women = 100)))

ISAVUCONAZOLE_PULSE_FRACTION <- NULL
ISAVUCONAZOLE_PULSE_DELAY    <- NULL   # minutes

#' Isavuconazole pharmacokinetics (intravenous and oral)
#'
#' Desai et al. (2016): two compartments, with the published Weibull oral
#' input discretised into pulses of the default oral form; see the header of
#' \code{R/drugs_isavuconazole.R}.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
isavuconazole <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume = 1, legacyClearance = 1,
                        cohort = do.call(cohortSizeFactors, ISAVUCONAZOLE_COHORT))
  v  <- size$volume * ISAVUCONAZOLE_SALT
  cl <- size$clearance * ISAVUCONAZOLE_SALT

  default <- list(
    v1  = ISAVUCONAZOLE_V1 * v,
    v2  = ISAVUCONAZOLE_VP * v,
    v3  = 1,
    cl1 = ISAVUCONAZOLE_CL * cl / 60,
    cl2 = ISAVUCONAZOLE_Q * cl / 60,
    cl3 = 0,
    # The depot each pulse is absorbed through; F is 1 in the source, and the
    # lag is carried by the pulse delays.
    ka_PO = ISAVUCONAZOLE_KAMAX / 60,
    bioavailability_PO = 1,
    tlag_PO = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  list(
    PK = PK, tPeak = 0, MEAC = 0,
    typical = 0, upperTypical = 0, lowerTypical = 0,
    reference = paste0(
      "Desai A et al., Antimicrob Agents Chemother 2016;60:5483-5491, Table 5 (421 adults, ",
      "healthy and with invasive fungal infections): two compartments, CL 2.36 L/h ",
      "(predominantly Caucasian), V1 49.1 L, Q 26.6 L/h, Vp 390 L (the Discussion's patients; ",
      "the BMI term not applied), F 1; the oral Weibull input (KAMAX 1.08 /h, RA 0.72 /h, ",
      "shape 4.88) discretised into ", length(ISAVUCONAZOLE_PULSE_FRACTION),
      " lagged fractions absorbed at 1.08 /h. Scaled to fat-free mass about the source's ",
      "cohort. Doses as isavuconazonium sulfate (372 mg = 200 mg isavuconazole); plasma ",
      "isavuconazole. https://doi.org/10.1128/AAC.02819-15"),
    oralPulses = stats::setNames(
      list(list(fraction = ISAVUCONAZOLE_PULSE_FRACTION,
                delay    = ISAVUCONAZOLE_PULSE_DELAY)),
      ORAL_PULSE_DEFAULT_FORM)
  )
}
