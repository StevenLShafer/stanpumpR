# -----------------------------------------------------------------------------
# Emicizumab: one compartment, subcutaneous (Hemlibra)
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min, amounts in mg,
# concentrations in mcg/mL (mg/L).  Concentration only: emicizumab mimics
# activated factor VIII, but no factor VIII-equivalent activity, thrombin
# generation or bleeding rate is plotted.  Drafted by Claude Code, 2026-11-25,
# at the request of Steven L. Shafer (docs/library-expansion/plan.csv,
# Biologics (surgery)); digest card papers/digests/PMID_32504271.md.
#
# SOURCE
# ======
# Retout S, Schmitt C, Petry C, Mercier F, Frey N.  Population pharmacokinetic
# analysis and exploratory exposure-bleeding rate relationship of emicizumab
# in persons with hemophilia A.  Clin Pharmacokinet 2020;59:1611-1625.
# 389 persons with haemophilia A, with or without factor VIII inhibitors, aged
# 1.22-77.0 years (median 30.0) and weighing 9.5-156 kg (median 69.1), from
# HAVEN 1-4 and the Japanese phase I/II study, given 0.3-6 mg/kg SC weekly, every
# two weeks or every four weeks (Tables 1-3).  Read on the PDF.
#
# Table 4 and the equations under it (PDF p. 7, printed 1617), apparent
# parameters relative to the subcutaneous bioavailability of a person of 30
# years or younger:
#
#     CL/F = 0.272 L/day x (BW/70)^0.911 x (1 - 0.0157 x (ALB - 45))
#     V/F  = 10.4 L      x (BW/70)^1.00  x (1 - 0.215 x BLK)
#     KA   = 0.536 /day
#     F    = 1 if AGE <= 30, else 1 - 0.00651 x (AGE - 30)
#
# with BW in kg, ALB serum albumin in g/L and BLK 1 for black persons.  The
# model is the one-compartment model the paper chose: the distribution phase of
# an IgG is hidden under the subcutaneous absorption (Discussion), so the
# intravenous route is neither offered nor modelled.  The label's absolute
# bioavailability, 80.4-93.1% at 1 mg/kg, is not applied: the parameters are
# apparent (/F) and F above is relative.
#
# COVARIATES
#   Albumin: the Patient Profile's Serum albumin (g/dL, x10 to g/L), held within
#     the 16.8-56.6 g/L the study held (Table 2); blank is the reference 45 g/L.
#     The relation is linear and reaches zero clearance at 108.7 g/L, outside
#     any legal entry; the hold is the library's rule for a covariate fitted
#     over a range (tolcapone, epirubicin).
#   Age: on F, as fitted, held at the oldest age studied, 77 years (F 0.694).
#     The US label (revised 7/2025) says the pharmacokinetics "are not
#     influenced by age (1 year to 77 years)"; Retout 2020 is the population
#     analysis that found the effect, a 31% lower steady-state exposure at 77
#     years, and the model applies it (AGENTS.md, a label is frozen at
#     approval).
#   Race: black race lowers V/F 21.5%; the Patient Profile has no race field,
#     so V/F is the non-black value.  The paper shows the effect changes the
#     steady state by less than 2%.
#   Children under 1 year: outside the study (the youngest was 1.22 years).
#     The label (12.3) says concentrations predicted below 6 months were 19-33%
#     lower, from a maturation model this paper defers to "a future
#     publication"; that model is not applied (papers/pending-work.csv).
#
# BODY SIZE: the source's own weight terms, kept as fitted (AGENTS.md, every
# parameter is scaled), evaluated at the pharmacokinetic weight with the switch
# on and at total weight with it off.  The normalising weight, 70 kg, is the
# study's own reference (its median 69.1 kg).  KA carries no size term, as for
# every subcutaneous model in the library.
#
# CHECKS (Results 3.2, p. 1621; label Table 4)
# ======
# The reference person (70 kg, 45 g/L, 30 years) on 1.5 mg/kg weekly: Cav,ss
# = dose / (CL/F x tau) = 105 / (0.272 x 7) = 55.1 mcg/mL, the paper's 55.1;
# at albumin 33 and 57 g/L 46.4 and 68.0 (paper 46.4, 68.0); at 77 years 31%
# lower (paper 31%).  Elimination half-life ln 2 x 10.4 / 0.272 = 26.5 days
# (paper's individual mean 26.8, label 26.9), absorption half-life 1.29 days
# (paper's individual mean 1.61, label 1.6: a mean of individual half-lives
# exceeds the typical one when KA varies 72.5%).  The label's mean steady-state
# trough after the loading doses, 51.2 mcg/mL at 1.5 mg/kg weekly.
#
# References
# ----------
# Retout S et al., Clin Pharmacokinet 2020;59:1611-1625.
#   https://doi.org/10.1007/s40262-020-00904-z
# HEMLIBRA (emicizumab-kxwh) prescribing information, BLA 761083, 7/2025.
# -----------------------------------------------------------------------------

EMICIZUMAB_CL       <- 0.272    # L/day at 70 kg, 45 g/L
EMICIZUMAB_V        <- 10.4     # L at 70 kg, non-black
EMICIZUMAB_KA       <- 0.536    # /day
EMICIZUMAB_CL_WT    <- 0.911
EMICIZUMAB_V_WT     <- 1.00
EMICIZUMAB_CL_ALB   <- 0.0157   # per g/L about 45
EMICIZUMAB_ALB_REF  <- 45       # g/L
EMICIZUMAB_ALB_MIN  <- 16.8     # g/L, Table 2
EMICIZUMAB_ALB_MAX  <- 56.6     # g/L, Table 2
EMICIZUMAB_F_AGE    <- 0.00651  # per year above 30
EMICIZUMAB_AGE_REF  <- 30
EMICIZUMAB_AGE_MAX  <- 77       # oldest studied
EMICIZUMAB_WT_REF   <- 70

#' Emicizumab pharmacokinetics (subcutaneous)
#'
#' One compartment with first-order absorption, apparent parameters (Retout
#' 2020); see the header of the file.
#'
#' @inheritParams cefazolin
#' @param albumin serum albumin, g/dL, or NULL for the reference 4.5 g/dL
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
emicizumab <- function(weight, height, age, sex, adjustToFFM = TRUE,
                       albumin = NULL)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)
  pkW  <- if (isTRUE(adjustToFFM)) size$pkWeight else weight

  if (!is.null(albumin) && (length(albumin) != 1 || !is.finite(albumin))) albumin <- NULL
  alb <- if (is.null(albumin)) EMICIZUMAB_ALB_REF else
    min(max(10 * albumin, EMICIZUMAB_ALB_MIN), EMICIZUMAB_ALB_MAX)
  ageF <- min(max(age, EMICIZUMAB_AGE_REF), EMICIZUMAB_AGE_MAX)
  f <- 1 - EMICIZUMAB_F_AGE * (ageF - EMICIZUMAB_AGE_REF)

  cl <- EMICIZUMAB_CL * (pkW / EMICIZUMAB_WT_REF)^EMICIZUMAB_CL_WT *
    (1 - EMICIZUMAB_CL_ALB * (alb - EMICIZUMAB_ALB_REF))
  v  <- EMICIZUMAB_V * (pkW / EMICIZUMAB_WT_REF)^EMICIZUMAB_V_WT

  default <- list(
    v1  = v, v2 = 1, v3 = 1,
    cl1 = cl / 1440, cl2 = 0, cl3 = 0,
    ka_SC = EMICIZUMAB_KA / 1440,
    bioavailability_SC = f,
    tlag_SC = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  list(
    PK = PK, tPeak = 0, MEAC = 0,
    typical = 0, upperTypical = 0, lowerTypical = 0,
    reference = paste0(
      "Retout 2020 (haemophilia A, 1.2-77 years): one compartment, CL/F 0.272 L/day ",
      "x (weight/70)^0.911 x (1 - 0.0157 x (albumin - 45 g/L)), V/F 10.4 L x weight/70, ",
      "ka 0.536/day, relative F 1 - 0.00651 x (age - 30) above 30 years; albumin ",
      if (is.null(albumin)) "held at the reference 45 g/L" else
        paste0(signif(alb, 3), " g/L (held within the studied 16.8-56.6)"),
      "; non-black V/F. Concentration only. https://doi.org/10.1007/s40262-020-00904-z"
    )
  )
}
