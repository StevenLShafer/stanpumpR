# -----------------------------------------------------------------------------
# Cytarabine, intravenous: plasma cytarabine (ara-C), the model chosen by age
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min; amounts in
# mg and concentrations in mcg/mL (= mg/L).
#
# Drafted by Claude Code, 2026-11-10, at the request of Steven L. Shafer, the
# library's first drug built on the rule that THE PROGRAM CHOOSES THE MODEL
# (AGENTS.md, Key Rules; Shafer 2026-11-09): two published models, each for
# the population it was fitted in, under one name, the choice made from the
# patient's age.  Shafer decided (2026-11-10) to combine them so, accepting
# the jump in clearance where they meet.  Both PDFs were supplied by Shafer
# and read on the page images (Periclou 2026-11-07, Krogh-Madsen 2026-11-06).
#
# THE CHOICE
# ==========
#   age under 19 years: Periclou 1996 (children 0.17-19 years)
#   age 19 and over:    Krogh-Madsen 2012 (adults 22-68 years)
# Periclou's oldest patient was 19, so it is used up to its own range; the
# adult model is used from there (its youngest was 22).  The two disagree
# badly where they meet: for an 18.9-year-old 70-kg, 170-cm man Periclou's
# clearance is about 89 L/h, Krogh-Madsen's 272 L/h (CYTARABINE_ADULT_AGE).
#
# CHILDREN: PERICLOU 1996
# =======================
# Periclou AP, Avramis VI.  NONMEM population pharmacokinetic studies of
# cytosine arabinoside after high-dose and after loading bolus followed by
# continuous infusion of the drug in pediatric patients with leukemias.
# Cancer Chemother Pharmacol 1996;39:42-50 (doi 10.1007/s002800050536).
# Children 0.17-19 years with leukaemia or lymphoma; high-dose ara-C 3 g/m2
# over 1 or 3 h, or a loading bolus and a 72-h continuous infusion; plasma
# ara-C by HPLC; NONMEM.  Model I, two compartments, every parameter linear
# in the product of age (years) and body surface area (m2), the final
# estimates as the abstract and Results print them:
#
#     CL 2.59, Q 2.01 L/h, V1 0.48, V2 38.1 L, each x AGE x BSA
#
# Table 1's row I-2 prints 2.60, 1.82, 0.46 and 34.6 for the same model; the
# Results' "final parameter estimates" are used, the table recorded here.
# Model II-3 (ara-C with its deaminated metabolite ara-U) is not used: its
# central volume, 1.37 L, carries no size term at all.  The BSA formula is
# not named; Mosteller's is used.
#
# Limits of the children's model, which the help page states: every term is
# proportional to age, so clearance is zero at birth (the age is held at 0.17
# years, the youngest studied, CYTARABINE_PERICLOU_MIN_AGE) and grows without
# a plateau; the four parameters scale together, so the half-lives are the
# same at every age, about 4 min and 23 h, against the 17 min and 4 h the
# paper quotes from its two-stage analyses.
#
# ADULTS: KROGH-MADSEN 2012
# =========================
# Krogh-Madsen M, Bender B, Jensen MK, Nielsen OJ, Friberg LE, Honore PH.
# Population pharmacokinetics of cytarabine, etoposide, and daunorubicin in
# the treatment for acute myeloid leukemia.  Cancer Chemother Pharmacol
# 2012;69:1155-1163 (doi 10.1007/s00280-011-1800-z).  23 adults with AML,
# 22-68 years, 46-117 kg; 100 mg/m2 b.i.d. as a 5-min push; plasma, HPLC-UV
# (LLOQ 13 ng/mL); NONMEM VI, M3.  Table 2 (bootstrap):
#
#     CL 272 L/h x (1 - 0.0020 (bWBC - median)), Vc 62.8 L, Q 13.7 L/h, Vp 75.4 L
#
# The white count has no Patient Profile field and is held at the median
# (factor 1).  The women's 9% higher clearance was not kept in the final
# model.  No size, age or renal covariate survived; samples fell below the
# limit about 2 h after a dose, so the peripheral volume is weak (RSE 32%).
#
# DOSE BASIS: cytarabine is dosed as itself (no salt); mg, both papers.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# Children: the model's own body-surface-area covariate, the BSA from the
# pharmacokinetic weight with the switch on and total weight with it off.
# Adults: no size covariate, so the parameters are taken as the reference
# adult's and scaled by the library's factors with the switch on and fixed
# with it off (the cohort's mean weight was 79 kg).
#
# PREGNANCY (R/pregnancy.R): no human pharmacokinetics found; nothing applied.
# NO EFFECT SITE, NO BAND
# -----------------------------------------------------------------------------

# Periclou 1996, Model I (Results): per year of age x m2 of BSA
CYTARABINE_PERICLOU <- list(cl = 2.59, q = 2.01, v1 = 0.48, v2 = 38.1)
CYTARABINE_PERICLOU_MIN_AGE <- 0.17   # years, the youngest studied

# Krogh-Madsen 2012, Table 2 (L/h, L), white count at the median
CYTARABINE_KROGH_MADSEN <- list(cl = 272, v1 = 62.8, q = 13.7, v2 = 75.4)

# From this age the adult model is used (Periclou's oldest was 19 years)
CYTARABINE_ADULT_AGE <- 19

#' Which cytarabine model a patient gets
#'
#' @param age years
#' @returns "Periclou 1996" or "Krogh-Madsen 2012"
#' @keywords internal
cytarabineModel <- function(age)
{
  if (age < CYTARABINE_ADULT_AGE) "Periclou 1996" else "Krogh-Madsen 2012"
}

#' Cytarabine, intravenous (Periclou 1996 in children, Krogh-Madsen 2012 in adults)
#'
#' Two compartments; the program chooses the model from the patient's age.
#' See the header of \code{R/drugs_cytarabine.R}.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
cytarabine <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume = 1, legacyClearance = 1)
  if (age < CYTARABINE_ADULT_AGE)
  {
    p <- CYTARABINE_PERICLOU
    w <- if (isTRUE(adjustToFFM)) size$pkWeight else weight
    s <- max(age, CYTARABINE_PERICLOU_MIN_AGE) * bsaMosteller(w, height)
    default <- list(
      v1  = p$v1 * s,
      v2  = p$v2 * s,
      v3  = 1,                                 # placeholder: two compartments
      cl1 = p$cl * s / 60,
      cl2 = p$q * s / 60,
      cl3 = 0
    )
    reference <- paste0(
      "Chosen for a patient under 19 years: Periclou AP, Avramis VI. NONMEM population ",
      "pharmacokinetic studies of cytosine arabinoside after high-dose and after loading ",
      "bolus followed by continuous infusion of the drug in pediatric patients with ",
      "leukemias. Cancer Chemother Pharmacol 1996;39:42-50. Children 0.17-19 years: two ",
      "compartments, CL 2.59, Q 2.01 L/h, V1 0.48, V2 38.1 L, each x age (years, held at ",
      "0.17 or above) x BSA (Mosteller). https://doi.org/10.1007/s002800050536"
    )
  } else
  {
    p <- CYTARABINE_KROGH_MADSEN
    default <- list(
      v1  = p$v1 * size$volume,
      v2  = p$v2 * size$volume,
      v3  = 1,                                 # placeholder: two compartments
      cl1 = p$cl / 60 * size$clearance,
      cl2 = p$q / 60 * size$clearance,
      cl3 = 0
    )
    reference <- paste0(
      "Chosen for a patient 19 years or older: Krogh-Madsen M, Bender B, Jensen MK, et ",
      "al. Population pharmacokinetics of cytarabine, etoposide, and daunorubicin in the ",
      "treatment for acute myeloid leukemia. Cancer Chemother Pharmacol 2012;69:1155-1163. ",
      "23 adults 22-68 years: two compartments, CL 272 L/h (white count at the median), ",
      "Vc 62.8 L, Q 13.7 L/h, Vp 75.4 L. https://doi.org/10.1007/s00280-011-1800-z"
    )
  }
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

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
