# -----------------------------------------------------------------------------
# Alprazolam: oral only, one compartment (DeVane 1993, without its sex term),
# with the EEG effect site of Venkatakrishnan 2005
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in ng/mL, total plasma.
#
# SOURCE
# ======
# DeVane CL et al. collected two random samples from each of 94 psychiatric
# inpatients taking alprazolam (70 men, 24 women; 47.7 +/- 13.3 y; 175.5 +/-
# 38.1 lb; 183 concentrations, mostly at steady state) and fitted a
# one-compartment model with first-order absorption in NONMEM.  Read from the
# full paper (equations 1-3, Tables II and IV):
#
#     CL/F = 0.05 L/h/kg x WT x (1 + 0.59 female) x (1 - 0.23 if age > 60)
#                             x (1 - 0.26 with multiple concurrent illness)
#     V/F  = 0.7 L/kg x WT        ka = 1.1 /h (95% CI 0.5-1.6)
#     IIV in CL 40% after the covariates; residual error 27%
#
# Every value agrees with the ChatGPT specification this model was built from.
# Two terms are not applied.  The multiple-illness term (two or more disease
# states) has no input in stanpumpR; the model is that of a patient without
# it.  The sex term is dropped (see COVARIATES).
#
# What the data could and could not identify.  Sampling was sparse and at
# steady state, which pins down clearance well (it agrees with the dose-
# concentration regression, 0.05 L/h/kg) but the volume and ka poorly: the
# volume, 0.7 L/kg (0.6 in the basic model), is below the 1.0 L/kg of the
# manufacturer's phase I studies quoted in the same table, and no variability
# could be estimated for it.  ka was "modeled as a constant across the
# population".
#
# APPARENT SCALE, ORAL ONLY
# =========================
# Fitted to oral data alone, so clearance and volume are divided by an
# unknown bioavailability, which is 0.92 (Smith 1984; 80-100% in the
# Greenblatt and Wright review).  There is no intravenous alprazolam product.
# Alprazolam is offered by mouth only, with bioavailability_PO = 1 because
# the apparent scale already contains F.
#
# Checks, reference patient (70 kg, 170 cm, 35 y man): CL/F 3.5 L/h, V/F 49 L,
# half-life 9.7 h (label 11.2 h, 6.3-26.9).  1 mg by mouth peaks at 16.9
# ng/mL at 2.7 h; observed 12-22 ng/mL at 0.7-1.8 h (Greenblatt and Wright
# 1993) or 1-2 h (label).  The peak height is right and the peak is about an
# hour late, the consequence of a ka estimated from steady-state samples; ka
# is kept as published.  At steady state 1 mg/day averages 11.9 ng/mL (10-12
# per mg/day, Greenblatt and Wright).
#
# COVARIATES
# ==========
# Age as published: over 60, clearance is 23% lower, consistent with the
# reduced clearance in the elderly in the Greenblatt and Wright review.
#
# DeVane's sex term is NOT applied (Steven L. Shafer, 2026-10-09).  It would
# make a woman's clearance 59% higher (95% CI 30-88%), so her steady-state
# concentration 37% lower.  It was estimated in 24 women among inpatients on
# varied co-medication; DeVane's own discussion notes that a sex difference
# "has been sometimes observed ... but not consistently", and the Greenblatt
# and Wright review that "most studies show that alprazolam pharmacokinetics
# are not significantly influenced by gender".  Men and women of the same
# size therefore receive the same clearance; a woman's smaller body is still
# represented through the weight terms.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# DeVane's clearance and volume are proportional to weight.  As for the other
# models with their own weight covariate, the equations are evaluated at the
# pharmacokinetic weight with the switch on (the default; 70 kg x FFM /
# FFM_ref) and at total body weight with it off.  Abernethy 1984 found oral
# clearance no higher in obese subjects (3.98 against 5.28 L/h), which a
# total-weight model would get wrong.
#
# EFFECT SITE
# ===========
# Venkatakrishnan K et al. infused 1 mg intravenously over 30 min in 9 healthy
# men and related EEG beta (12-30 Hz) to an effect site through a sigmoid
# Emax: "effect site equilibration half-life of 4.8 minutes".  ke0 = ln 2 /
# 4.8 = 0.144 /min, supplied directly: the half-life comes from intravenous
# data on an intravenous model that was not retrievable, so it cannot be
# carried as a time to peak effect on this oral curve.  With absorption this
# slow the effect site follows plasma within minutes.  MEAC 0.
#
# BAND
# ====
# 20-40 ng/mL: "optimal reduction of anxiety associated with panic disorder
# occurs at steady-state plasma alprazolam concentrations of 20 to 40
# micrograms/L.  Concentrations higher than this may be needed for
# suppression of the actual panic attacks" (Greenblatt and Wright 1993);
# typical 30.  For orientation, the EC50 for impaired card sorting and DSST
# after intravenous alprazolam was 37-40 ng/mL in young men and 25 in the
# elderly (Bertz 1997).  No time-until-threshold level (endCe 0).
#
# NOT MODELLED
# ============
# Multiple concurrent illness (-26%), hepatic disease, obesity's longer
# half-life (from a larger volume), CYP3A4 inhibitors, smoking, the
# extended-release and orally disintegrating products, and the
# hydroxylated metabolites, which reach less than 10% of the parent's
# concentration and have lower receptor affinity.
#
# References
# ----------
# DeVane CL et al., Clin Pharmacol Ther 1993;53:521-528.
#   https://doi.org/10.1038/clpt.1993.65
# Venkatakrishnan K et al., J Clin Pharmacol 2005;45:529-537.
#   https://doi.org/10.1177/0091270004269105
# Smith RB et al., Psychopharmacology (Berl) 1984;84:452-456.
#   https://doi.org/10.1007/BF00431449
# Greenblatt DJ, Wright CE, Clin Pharmacokinet 1993;24:453-471.
#   https://doi.org/10.2165/00003088-199324060-00003
# Bertz RJ et al., J Pharmacol Exp Ther 1997;281:1317-1329.
#   https://pubmed.ncbi.nlm.nih.gov/9190868/
# Abernethy DR et al., Clin Pharmacokinet 1984;9:177-183.
#   https://doi.org/10.2165/00003088-198409020-00005
# Xanax (alprazolam) prescribing information.
#
# Drafted with Claude Code at the request of Steven L. Shafer, 2026-10-09,
# from a ChatGPT specification whose references and values were checked
# against the sources first (the DeVane paper in full).
# -----------------------------------------------------------------------------

# DeVane 1993, Tables II and IV
ALPRAZOLAM_CL_PER_KG <- 0.05     # L/h/kg, apparent
ALPRAZOLAM_V_PER_KG  <- 0.7      # L/kg, apparent
ALPRAZOLAM_KA        <- 1.1      # 1/h
ALPRAZOLAM_OVER_60   <- -0.23    # fractional change in CL/F, age > 60

# Venkatakrishnan 2005: effect-site equilibration half-life 4.8 min (EEG beta)
ALPRAZOLAM_KE0 <- log(2) / 4.8   # 1/min

#' Alprazolam pharmacokinetics (oral)
#'
#' DeVane et al. (1993): one compartment, apparent oral clearance and volume
#' proportional to weight, clearance lower above 60 (the published sex term
#' is not applied); the
#' EEG effect-site rate constant of Venkatakrishnan et al. (2005).  See the
#' file's header.
#'
#' @inheritParams cefazolin
#' @param adjustToFFM \code{TRUE} (the default) evaluates DeVane's weight
#'   terms at the pharmacokinetic weight; \code{FALSE} at total body weight.
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
alprazolam <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  # Size scaling (see the header): DeVane's own weight terms, at the
  # pharmacokinetic weight with the switch on and total weight with it off.
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)
  pkW  <- if (isTRUE(adjustToFFM)) size$pkWeight else weight

  # DeVane's sex term (+59% in women) is deliberately not applied: see the
  # header.
  cl1 <- ALPRAZOLAM_CL_PER_KG * pkW *
    (1 + if (age > 60) ALPRAZOLAM_OVER_60 else 0) / 60   # L/min

  default <- list(
    v1 = ALPRAZOLAM_V_PER_KG * pkW,
    v2 = 1,                                       # one compartment
    v3 = 1,
    cl1 = cl1,
    cl2 = 0,
    cl3 = 0,
    ka_PO = ALPRAZOLAM_KA / 60,                   # 1/min
    bioavailability_PO = 1,                       # apparent (/F) parameters
    tlag_PO = 0
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  # Band, ng/mL: steady state in panic disorder (Greenblatt and Wright 1993).
  typical      <- 30
  upperTypical <- 40
  lowerTypical <- 20

  reference <- paste0(
    "DeVane CL et al., Clin Pharmacol Ther 1993;53:521-528. ",
    "One compartment, apparent oral clearance on weight and age (the sex ",
    "term not applied); ",
    "ke0 from Venkatakrishnan K et al., J Clin Pharmacol 2005;45:529-537; ",
    "oral only. https://doi.org/10.1038/clpt.1993.65"
  )

  return(
    list(
      PK = PK,
      # No tPeak: ke0 is supplied directly (see the header).
      tPeak = 0,
      ke0 = ALPRAZOLAM_KE0,
      MEAC = 0,
      typical = typical,
      upperTypical = upperTypical,
      lowerTypical = lowerTypical,
      reference = reference
    )
  )
}
