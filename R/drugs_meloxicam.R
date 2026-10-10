# -----------------------------------------------------------------------------
# Meloxicam: two-compartment oral model with CYP2C9 genotype (Aoyama 2017)
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in mcg/mL (= mg/L), total plasma meloxicam.
#
# DISPOSITION (APPARENT ORAL PARAMETERS)
# ======================================
# Aoyama et al. fitted a two-compartment NONMEM model to 119 healthy men (30
# Japanese, 30 Chinese, 29 Korean and 30 white; 21-35 years, 51-100 kg) after
# one 7.5 mg oral dose sampled to 72 h.  No ethnic difference was found.
# Table 2, final model:
#
#     CL/F = 0.391 x (1 - 0.147 n*2 - 0.400 n*3)   L/h
#     Vc/F = 7.79 x (LBM / 55)^1.05                L
#     Q/F  = 1.24 L/h       Vp/F = 2.73 L
#
# n*2 and n*3 count CYP2C9 *2 and *3 alleles (so *1/*3 is 0.600 of the *1/*1
# clearance and *3/*3 0.200).  LBM is the James lean body mass, centred on
# the population median of 55 kg; the library's reference man (70 kg,
# 170 cm) has 55.3 kg.  Half-lives at the reference man 1.1 h and 19 h.
#
# The parameters are divided by an unmeasured bioavailability, so the drug is
# offered by mouth only and bioavailability_PO is 1: the apparent scale
# already contains F (docs/adding-a-drug.md).  The label's absolute F of
# about 0.89 (30 mg capsule) was not estimated in this fit and is not applied.
#
# ORAL ABSORPTION
# ===============
# Two parallel paths: 42.5% of the dose enters at a constant rate over 1.91 h
# from the dose; the other 57.5% enters a first-order depot (ka 2.00 /h)
# after a lag equal to that duration (the source fixed tlag = DT).  0.425 is
# a path fraction, not a bioavailability.
#
# The engine absorbs first-order, so the zero-order path is APPROXIMATED by a
# first-order depot with the same mean absorption time, DT / 2: ka = 2 / DT =
# 1.047 /h, no lag.  The lagged first-order path is exact (the engine's second
# oral depot, ka_PO2).  With a 19 h terminal half-life the difference is
# confined to the first few hours: for 7.5 mg in the reference man the
# approximation peaks at 0.720 mg/L at 3.2 h, the published structure (solved
# numerically) at 0.733 mg/L at 3.1 h; the approximation is 22% high at 1 h
# and 13% low at 2 h, and the two agree within 0.5% from 4 h on.
#
# EFFECT SITE
# ===========
# None: tPeak = 0, and the plotted concentration is plasma.  No band and no
# recovery threshold: no concentration-effect relationship for analgesia is
# established.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# Vc/F carries the model's own LBM covariate in both switch positions.  The
# source found no size effect on CL/F, Q/F or Vp/F; those take the library's
# fat-free-mass factors with the switch on and are left as published with it
# off (legacyVolume = 1).
#
# CYP2C9
# ======
# The function takes the genotype as `cyp2c9` (default "*1/*1").  The app has
# no CYP2C9 input, so it always simulates *1/*1; the argument serves scripts
# calling meloxicam() directly.  Diplotypes rare in the source (*2/*2,
# *2/*3, *3/*3) are extrapolated from the per-allele model.  No CYP2D6
# adjustment: the model fitted none.
#
# References
# ----------
# Aoyama T et al., CPT Pharmacometrics Syst Pharmacol 2017;6:823-832.
#   https://doi.org/10.1002/psp4.12259
# (Claude Code, 2026-10-10, at the request of Steven L. Shafer.)
# -----------------------------------------------------------------------------

MELOXICAM_CYP2C9_VALUES <- c("*1/*1", "*1/*2", "*1/*3", "*2/*2", "*2/*3", "*3/*3")
MELOXICAM_DT <- 1.91   # h, zero-order duration = lag of the first-order path

#' Meloxicam pharmacokinetics (apparent oral)
#'
#' @inheritParams cefazolin
#' @param adjustToFFM when \code{TRUE}, scale the size-free apparent
#'   parameters (CL/F, Q/F, Vp/F) to fat-free mass; Vc/F follows its own
#'   lean-body-mass covariate either way
#' @param cyp2c9 CYP2C9 diplotype, one of
#'   \code{"*1/*1", "*1/*2", "*1/*3", "*2/*2", "*2/*3", "*3/*3"}
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
meloxicam <- function(weight, height, age, sex, adjustToFFM = TRUE,
                      cyp2c9 = "*1/*1")
{
  if (length(cyp2c9) != 1 || !cyp2c9 %in% MELOXICAM_CYP2C9_VALUES)
    stop("Invalid cyp2c9: ", paste(cyp2c9, collapse = ", "), ". Must be one of: ",
         paste(MELOXICAM_CYP2C9_VALUES, collapse = ", "))
  alleles <- strsplit(cyp2c9, "/", fixed = TRUE)[[1]]
  n2 <- sum(alleles == "*2")
  n3 <- sum(alleles == "*3")

  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1)
  lbm  <- lbmJames(weight, height, sex)

  default <- list(
    v1  = 7.79 * (lbm / 55)^1.05,                       # own covariate
    v2  = 2.73 * size$volume,
    v3  = 1,                                            # two compartments
    cl1 = 0.391 * (1 - 0.147 * n2 - 0.400 * n3) / 60 * size$clearance,
    cl2 = 1.24 / 60 * size$clearance,
    cl3 = 0,
    # Apparent parameters: the whole dose, F = 1.  Path 1 (zero-order over
    # DT in the source) as first-order with the same mean absorption time.
    ka_PO              = 2 / (MELOXICAM_DT * 60),       # 1/min
    tlag_PO            = 0,
    bioavailability_PO = 1,
    ka_PO2             = 2.00 / 60,                     # 1/min, path 2
    tlag_PO2           = MELOXICAM_DT * 60,             # min
    fraction_PO2       = 0.575
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Aoyama T et al., CPT Pharmacometrics Syst Pharmacol 2017;6:823-832 ",
    "(apparent oral two-compartment model, CYP2C9 *1/*1 unless given; the ",
    "zero-order absorption path approximated as first-order). ",
    "https://doi.org/10.1002/psp4.12259"
  )

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
