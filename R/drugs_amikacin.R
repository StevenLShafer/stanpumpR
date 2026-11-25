# -----------------------------------------------------------------------------
# Amikacin (Tod 1998): two compartments, clearance linear in creatinine
# clearance
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in mcg/mL (= mg/L), total serum.
#
# Drafted by Claude Code, 2026-11-12, at the request of Steven L. Shafer, who
# asked for the common drugs with narrow therapeutic windows that the library
# lacked.  Every number below was read from the full text of the source in
# PubMed Central (PMC105554: Methods, Results and Table 4).
#
# THE SOURCE
# ==========
# Tod M, Lortholary O, Seytre D, et al.  Population pharmacokinetic study of
# amikacin administered once or twice daily to febrile, severely neutropenic
# adults.  Antimicrob Agents Chemother 1998;42:849-856 (doi
# 10.1128/AAC.42.4.849).  57 adults with haematological malignancy (weight
# about 64-66 kg, 44-106; age 18-85; Cockcroft-Gault creatinine clearance
# about 100 mL/min, 20-213) given 7.5 mg/kg twice or 20 mg/kg once daily over
# 30 min; 278 serum samples (EMIT); NONMEM, two compartments.  "the final
# model was first written as CL = a CLCR + b (a = 0.367, b = 1.40, and CLCR
# is in liters per hour)", CLCR the Cockcroft-Gault creatinine clearance;
# Table 4: V1 8.92 L, CLD 4.43 L/h, Vt 11.4 L.  The refined final model splits
# the CLCR term by sex and age (Table 3, step 5), with an equation printed as
# an image that could not be read unambiguously, so the first, simpler form is
# used, with Table 4's volumes.  The kinetics were linear from 7.5 to 20 mg/kg.
#
# Checked against what the fit did not see: Boidin 2019 (ICU, read in full)
# measured a median 1-h peak of 67 mg/L after 23.4 mg/kg in patients of
# median 76.5 kg and 65 years; this model gives 113 mg/L for such a patient,
# 1.7 times as much, as expected from the larger volumes of the critically
# ill.  Tod's own patients' first peaks were 75 mg/L (median 67) after
# 20 mg/kg.  The healthy volunteers Tod cites cleared amikacin
# faster (6.7-7.6 L/h).
#
# RENAL FUNCTION AND BODY SIZE (docs/weight-adjustment.md)
# =======================================================
# Cockcroft-Gault from the Patient Profile's serum creatinine through
# adultEquivalentCreatinine(); blank means the assumed normal for age and sex.
# The weight in it is the pharmacokinetic weight with the fat-free-mass
# switch on and total body weight with it off.  The volumes and CLD were
# fixed values (weight was tested on V1 and Vt and was not significant, which
# is not evidence that size does not matter: Steven L. Shafer's
# first-principles rule of 2026-11-19), so they are scaled by the library's
# factors with the switch on and fixed with it off.  The intercept, 1.40 L/h,
# is the clearance with no renal function, a fixed value with no size term,
# so it too takes the clearance factor with the switch on (until 2026-11-18 it
# was unscaled); the renal term takes none, its Cockcroft-Gault clearance
# carrying the weight already (Shafer, 2026-11-18: "you would wind up double
# correcting clearance").  V1, Vt, CLD and the intercept are anchored at
# Tod's cohort (AMIKACIN_COHORT, below: Table 1's two groups, 35 men and 22
# women, about 67 kg and 51 years, French), whose typical patient sits at a
# volume factor of 0.8921 and a clearance factor of 0.9179 from the 70 kg
# reference man, so a patient like Tod's receives Table 4's values and the
# reference man receives volumes 12.1% and the non-renal clearances 8.9%
# above them (until 2026-11-20 the reference man received them, the paper
# not being in the archive).  The card's [decision] on the clearance
# equation (papers/digests/PMID_9559795.md: the final form is now readable on
# the PDF) is not about size and is left open.
#
# DOSE
# ====
# The injection is amikacin sulfate labelled as its amikacin content (250 mg
# amikacin, as the sulfate, per mL; the Amikacin Sulfate Injection USP labels
# as a search showed them, DailyMed being outside this session's network), so
# doses are mg of amikacin.
# -----------------------------------------------------------------------------

# Tod 1998
AMIKACIN_CL_SLOPE     <- 0.367   # per (L/h of creatinine clearance)
AMIKACIN_CL_INTERCEPT <- 1.40    # L/h
AMIKACIN_V1           <- 8.92    # L
AMIKACIN_CLD          <- 4.43    # L/h
AMIKACIN_VT           <- 11.4    # L

# Tod 1998, Table 1 (PDF p2, printed 850), means: the o.d. group, 28 patients,
# 16 men and 12 women, 65.8 kg, 50.2 years, treated January to December
# 1995; the b.i.d. group, 29 patients, 19 men and 10 women, 68.1 kg, 51.3
# years, treated January 1993 to December 1994 (Methods).  The weight
# column's parenthesised values are SDs under a header misprinted "range"
# (the text, p853: 67 +/- 13 kg).  No height is printed: NCD-RisC's mean
# height at 18 for France, birth cohorts 1945 (1995 less 50.2) and 1943 (1994
# less 51.3), less the loss with age, is 172.5 and 172.1 cm for the men and
# 160.2 and 160.0 cm for the women.  V1, Vt, CLD and the non-renal intercept
# are anchored at this cohort (Steven L. Shafer, 2026-11-18 and 2026-11-19;
# see the header).  The renal term is not.
AMIKACIN_COHORT <- list(groups = list(
  list(weight = 65.8, height = 172.5, age = 50.2, men = 16, women = 0),
  list(weight = 65.8, height = 160.2, age = 50.2, men = 0, women = 12),
  list(weight = 68.1, height = 172.1, age = 51.3, men = 19, women = 0),
  list(weight = 68.1, height = 160.0, age = 51.3, men = 0, women = 10)))

#' Amikacin (Tod 1998)
#'
#' Two compartments; clearance linear in Cockcroft-Gault creatinine
#' clearance.  See the header of \code{R/drugs_amikacin.R}.
#'
#' @inheritParams cefazolin
#' @param creatinine serum creatinine in mg/dL, or NULL for the assumed normal
#'   value for age and sex
#' @param pregnant is the patient pregnant?  \code{TRUE} raises the
#'   creatinine clearance by \code{PREGNANCY_GFR_FACTOR} when the creatinine
#'   is blank.
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
amikacin <- function(weight, height, age, sex, adjustToFFM = TRUE,
                     creatinine = NULL, pregnant = FALSE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume = 1, legacyClearance = 1,
                        cohort = do.call(cohortSizeFactors, AMIKACIN_COHORT))
  # The pharmacokinetic weight is the patient's (70 kg x FFM / FFM_ref),
  # which the cohort does not change: Cockcroft-Gault is not anchored
  pkW  <- if (isTRUE(adjustToFFM)) size$pkWeight else weight
  scr  <- adultEquivalentCreatinine(creatinine, age, sex)
  gfrPregnancy <- pregnancyGfrFactor(pregnant, creatinine)
  crcl <- creatinineClearanceCG(pkW, age, sex, scr) * gfrPregnancy   # mL/min
  # The renal term takes no size factor (its CLcr carries the weight); the
  # non-renal intercept, a fixed value, takes the clearance factor
  cl <- AMIKACIN_CL_SLOPE * crcl * 60 / 1000 +
    AMIKACIN_CL_INTERCEPT * size$clearance                           # L/h

  default <- list(
    v1  = AMIKACIN_V1 * size$volume,
    v2  = AMIKACIN_VT * size$volume,
    v3  = 1,
    cl1 = cl / 60,
    cl2 = AMIKACIN_CLD / 60 * size$clearance,
    cl3 = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  creatinineNote <- if (is.null(creatinine))
    paste0("assumed normal serum creatinine ", format(scr), " mg/dL")
  else
    paste0("serum creatinine ", format(creatinine), " mg/dL")
  reference <- paste0(
    "Tod M et al., Antimicrob Agents Chemother 1998;42:849-856: two ",
    "compartments in 57 febrile neutropenic adults, CL 0.367 x Cockcroft-Gault ",
    "CrCl (L/h) + 1.40 L/h, V1 8.92 L, CLD 4.43 L/h, Vt 11.4 L; ",
    creatinineNote, ". https://doi.org/10.1128/AAC.42.4.849")
  if (isTRUE(pregnant) && gfrPregnancy != 1)
    reference <- paste0(reference, "; ", pregnancyReference(
      paste0("creatinine clearance x ", PREGNANCY_GFR_FACTOR,
             " (physiological rise in filtration)")))

  # Band: free drug at the expected MBC of the common pathogens, placed on
  # the plotted curve (R/antibioticMbc.R).
  band <- antibioticMbcBand("amikacin")$plotted
  list(
    PK = PK,
    tPeak = 0,
    MEAC = 0,
    typical = band[["typical"]],
    upperTypical = band[["upper"]],
    lowerTypical = band[["lower"]],
    reference = reference
  )
}
