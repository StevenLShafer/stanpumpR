# -----------------------------------------------------------------------------
# Risperidone, oral only, with 9-hydroxyrisperidone as its active metabolite
# -----------------------------------------------------------------------------
# Provenance
# ----------
# Drafted by Claude Code, 2026-10-09, at the request of Steven L. Shafer, from
# the antipsychotic implementation brief.  Checked against the full text of
# Storset 2024: ka, the structure, the serum matrix, the CYP2D6 allele
# activities, the NFIB effect, both age terms, the formation convention and
# the typical CL/F of 4.2 (two deficient alleles), 27.4 (*1/deficient) and
# 50.6 L/h (*1/*1).  Table 2 itself did not render, so parent V/F (333 L) and
# the metabolite's V (96 L) and CL (8.0 L/h) are the brief's, UNVERIFIED.
#
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in ng/mL (serum).
#
# THE SOURCE
# ==========
# Storset E et al., Eur J Clin Pharmacol 2024;80:1531-1541: 1565 paired serum
# concentrations of risperidone and 9-hydroxyrisperidone from 512 genotyped
# adults in therapeutic drug monitoring, mostly 9 to 30 h after the dose.  One
# compartment each, first-order absorption, all parameters APPARENT:
#
#     ka   = 2.01 /h (fixed)
#     V/F  = 333 L
#     CL/F = [4.2 + 23.2 x (a1 + a2) x n] x [1 - 0.009 x max(age - 34, 0)]
#     Vm/(F fmet)  = 96 L
#     CLm/(F fmet) = 8.0 x [1 - 0.013 x max(age - 39, 0)]
#
# where a1 and a2 are the activities of the two CYP2D6 alleles relative to *1
# (*2 with rs5758550G 0.66, *35 0.57, *9 0.39, *10 0.32, *2 with rs5758550A
# 0.30, *41 0.15, *3 to *6 zero) and n = 1.41 in an NFIB rs28379954 C carrier.
#
# CYP2D6 PHENOTYPE
# ================
# The Patient Profile carries a phenotype, not a genotype, and no NFIB status,
# so n = 1 and the phenotype is mapped onto the source's own example
# genotypes: poor = two deficient alleles (sum 0, 4.2 L/h), intermediate =
# *1/deficient (sum 1, 27.4 L/h), normal = *1/*1 (sum 2, 50.6 L/h).  The source
# reports no ultrarapid subject; ultrarapid is read as three functional copies
# (*1/*1xN, sum 3, 73.8 L/h), an extrapolation of its linear allele term.
#
# FORMATION
# =========
# F and fmet are not identifiable, so the metabolite parameters are divided by
# F x fmet and the whole of the parent's apparent clearance forms the scaled
# metabolite: kFormation = (CL/F) / (V/F).  As with every metabolite in this
# package it is added on top of a parent clearance that already contains it.
# The scaled metabolite parameters live in "hydroxyrisperidone", which is
# therefore never dosed directly: paliperidone given as itself would need its
# true, not its fmet-scaled, disposition.  Amounts are carried in risperidone
# mass (mwRatio = 1), as the source's scaled-state equations are written.
#
# Ages: both age terms are linear and are fitted in adults.  They stay positive
# up to the app's maximum age of 90 (factors 0.50 and 0.34); the function stops
# rather than return a nonpositive clearance if called beyond that.
#
# Body size: no weight covariate, so the published values are the 70 kg
# reference adult's and scale to fat-free mass (legacyVolume = 1).
#
# EFFECT
# ======
# No effect site.  D2 occupancy is a function of the ACTIVE MOIETY, the sum of
# the risperidone and 9-hydroxyrisperidone concentrations in ng/mL, with two
# complete PET fits that must not be mixed: 88 x C/(4.9 + C) (unconstrained)
# and 100 x C/(8.2 + C) (Emax fixed).  Those were calibrated in plasma, this
# model predicts serum.  See antipsychoticProfiles() in R/antipsychotics.R.
# -----------------------------------------------------------------------------

RISPERIDONE_V       <- 333     # L, apparent
RISPERIDONE_CL_BASE <- 4.2     # L/h, CYP2D6-independent
RISPERIDONE_CL_2D6  <- 23.2    # L/h per fully functional allele
RISPERIDONE_KA      <- 2.01    # 1/h, fixed in the source

# Sum of the two allele activities (relative to *1) for each phenotype; see the
# header.  Ultrarapid is an extrapolation.
RISPERIDONE_CYP2D6_ALLELES <- c(
  poor         = 0,
  intermediate = 1,
  normal       = 2,
  ultrarapid   = 3
)

#' Linear age factor of Storset 2024, guarded against a nonpositive result
#' @noRd
risperidoneAgeFactor <- function(age, slope, from)
{
  f <- 1 - slope * max(age - from, 0)
  if (f <= 0) stop("Risperidone model: age ", age, " is beyond the fitted range")
  f
}

#' Risperidone pharmacokinetics (oral)
#'
#' Storset et al. (2024): one compartment, apparent oral parameters, CYP2D6 and
#' age on clearance, with 9-hydroxyrisperidone formed as an active metabolite.
#'
#' @inheritParams cefazolin
#' @param cyp2d6 CYP2D6 metaboliser phenotype, one of \code{CYP2D6_VALUES}
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
risperidone <- function(weight, height, age, sex, cyp2d6 = CYP2D6_DEFAULT,
                        adjustToFFM = TRUE)
{
  if (length(cyp2d6) != 1 || !cyp2d6 %in% CYP2D6_VALUES) {
    stop("Invalid cyp2d6: ", paste(cyp2d6, collapse = ", "),
         ". Must be one of: ", paste(CYP2D6_VALUES, collapse = ", "))
  }
  alleles <- unname(RISPERIDONE_CYP2D6_ALLELES[[cyp2d6]])

  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1)

  cl <- (RISPERIDONE_CL_BASE + RISPERIDONE_CL_2D6 * alleles) *
    risperidoneAgeFactor(age, 0.009, 34)                       # L/h

  v1  <- RISPERIDONE_V * size$volume
  cl1 <- cl / 60 * size$clearance                              # L/min

  default <- list(
    v1 = v1,
    v2 = 1,                                                    # one compartment
    v3 = 1,
    cl1 = cl1,
    cl2 = 0,
    cl3 = 0,
    ka_PO = RISPERIDONE_KA / 60,                               # 1/min
    bioavailability_PO = 1,                                    # apparent (/F)
    tlag_PO = 0
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Storset E et al., Eur J Clin Pharmacol 2024;80:1531-1541. ",
    "https://doi.org/10.1007/s00228-024-03721-6 (apparent oral parameters, ",
    "serum; CYP2D6 phenotype mapped to example genotypes, NFIB not applied)"
  )

  list(
    PK = PK,
    tPeak = 0,
    MEAC = 0,
    typical = 0,
    upperTypical = 0,
    lowerTypical = 0,
    reference = reference,
    # Active itself: no effect-site model, but not a prodrug.
    prodrug = FALSE,
    metabolite = list(
      name              = "hydroxyrisperidone",
      kFormation        = cl1 / v1,   # all apparent clearance, fmet-scaled
      firstPassFraction = 0,
      mwRatio           = 1           # scaled amounts in risperidone mass
    )
  )
}
