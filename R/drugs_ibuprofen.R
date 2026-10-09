# -----------------------------------------------------------------------------
# Ibuprofen: two-compartment intravenous and oral model (Morse 2022) with the
# analgesic effect site of Hannam 2018
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in mcg/mL (= mg/L), total plasma racemic ibuprofen.
#
# DISPOSITION
# ===========
# Morse et al. pooled intravenous (300 mg over 15 min; Caldolor 400 mg over
# 30 min), tablet, sachet and suspension data from 116 healthy adults (18-49
# years, 49-116 kg, 93 men and 23 women; 6046 ibuprofen concentrations), the
# same studies as the library's acetaminophen model, and fitted a
# two-compartment model with first-order elimination.  Table 3, final model:
#
#     CL = 3.79 x (NFMcl / NFMcl,std)^0.75   L/h
#     V1 = 6.05 x (NFMv / NFMv,std)          L
#     Q2 = 10.5 x (TBW / 70)^0.75            L/h
#     V2 = 4.37 x (NFMv / NFMv,std)          L
#
#     NFMcl = FFM + 0.863 x (TBW - FFM)     FFAT on CL
#     NFMv  = FFM + 0.718 x (TBW - FFM)     FFAT on V
#
# The abstract's "central volume of distribution ... 10.5 L/h/70 kg" is the
# same error as acetaminophen's: in Table 3, 10.5 L/h is Q2 and V1 is 6.05 L.
#
# The standards are those of Morse's standard man, 70 kg and 176 cm, whose
# FFM the paper gives as 56.1 kg.  The code computes it (56.13 kg), so that
# NFMcl,std is 68.10 kg, NFMv,std 66.09 kg, and that man recovers Table 3
# exactly.  The library's reference man (70 kg, 170 cm, FFM 54.5 kg) is a
# little fatter: CL 3.78 L/h, V1 6.01 L, V2 4.34 L.  Terminal half-life
# 2.0 h; distribution half-life 9.3 min.
#
# Morse states the fat factors for "clearance and volume of distribution",
# and that size is scaled to 70 kg with exponents 3/4 for clearances and 1 for
# volumes (her Eq. 1, on total weight).  The table gives FFATV once, so it is
# applied to both volumes.  Which size descriptor Q2 takes is not stated; it
# is read as Eq. 1 on total weight, as for acetaminophen, whose code does the
# same.  For a 120 kg, 170 cm man, Q2 is 15.7 L/h on total weight and would
# be 15.4 on NFMcl, so the reading matters little.
#
# FFM: Morse used Janmahasatian's equations; the code uses the library's
# ffmAlSallami(), which equals them in adult men and differs by 1-3% in adult
# women (see R/drugs_acetaminophen.R).
#
# ORAL ROUTE
# ==========
# Fasted tablet, Table 3: bioavailability FIBU 0.941, absorption half-life
# 26.7 min, lag 6.66 min, all as published.  Like acetaminophen's, the lag
# is kept rather than folded into ka (Steven L. Shafer's decision for
# acetaminophen, 2026-10-08), so for the 6.66 min after an oral dose time
# until threshold is blank (R/recoveryStates.R).
#
# The reference man's 400 mg tablet peaks at 23.6 mg/L at 62 min; 300 mg at
# 17.7 mg/L.  Morse's Table 4 gives the 300 mg tablet's simulated median as
# 24.1 mg/L at 0.94 h, "calculated using clearance, volume of distribution,
# absorption rate constant and bioavailability".  A one-compartment
# calculation on V1 alone reproduces that (25.3 mg/L at 0.98 h; for
# acetaminophen it gives 14.0 mg/L at 0.61 h against the table's 12.8 at
# 0.61 h), so the table appears to leave out the peripheral compartment, and
# the two-compartment curve plotted here peaks lower.
#
# Not modelled: food, which multiplies the tablet's absorption half-life by
# 1.59 and its lag by 3.65 (Table 3), so the oral curve is the FASTED curve;
# and the faster suspension and sachet (absorption half-life x 0.719 and
# 0.235 when fasted).
#
# EFFECT SITE
# ===========
# ke0 is supplied directly from Hannam 2018: oral acetaminophen and ibuprofen
# in children after adenotonsillectomy, an Emax model for the two drugs (and
# tramadol) with additive effects.  Ibuprofen's equilibration half-time is
# 1.04 h (95% CI 0.75-1.77), with an EC50 of 3.95 mg/L, a fractional Emax of
# 0.65 and a Hill coefficient of 1.48 shared with acetaminophen.  As for
# acetaminophen's (Anderson 2001), applying a paediatric, oral-derived
# equilibration half-time to adults and to intravenous doses is an
# assumption.  With it, the effect site of the reference man peaks at
# 16.1 mg/L 2.7 h after a 400 mg tablet, and at 18.2 mg/L 2.0 h after
# 400 mg intravenously over 30 min.
#
# Not used: Li 2012 (adults after third-molar extraction, effect-site EC50
# 10.2 mg/L), whose equilibration rate could not be retrieved; and Hannam and
# Anderson 2011 (published adult dental data, EC50 5.07 mg/L, Hill 2), which
# also could not be read beyond its abstract.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# Clearance and both volumes use their own normal-fat-mass covariates in both
# switch positions; size$ffm is ffmAlSallami()'s FFM.  Q2 is the one
# parameter on total weight: with the switch on it takes the library
# clearance factor (the published term at the pharmacokinetic weight), with
# it off (TBW / 70)^0.75 as published.
#
# NOT MODELLED
# ============
# The enantiomers: ibuprofen is racemic, the S enantiomer carries most of the
# activity, and R is substantially inverted to S (Davies 1998); the model is
# total racemic ibuprofen, as measured.  Concentration-dependent albumin
# binding: above about 600 mg the unbound fraction rises, total clearance
# rises and the total AUC falls below proportion (Davies 1998); the model is
# linear, fitted at 150-400 mg, and overpredicts total concentrations after
# 800 mg somewhat.  Maturation: clearance reaches 90% of the adult value by
# a month after term birth and 98% by three months (Anderson and Hannam
# 2019, mature CL 3.81 L/h/70 kg), so in children over about three months
# the allometric extrapolation is reasonable, and in neonates and young
# infants it overpredicts clearance.  Also not modelled: CYP2C9 genotype,
# hepatic and renal impairment, and the glucuronide metabolites.
#
# References
# ----------
# Morse JD et al., Eur J Drug Metab Pharmacokinet 2022;47:497-507.
#   https://doi.org/10.1007/s13318-022-00766-9
# Hannam JA, Anderson BJ, Potts A, Paediatr Anaesth 2018;28:841-851.
#   https://doi.org/10.1111/pan.13464
# Anderson BJ, Hannam JA, Paediatr Anaesth 2019;29:1107-1113.
#   https://doi.org/10.1111/pan.13731
# Hannam J, Anderson BJ, Paediatr Anaesth 2011;21:1234-1240.
#   https://doi.org/10.1111/j.1460-9592.2011.03644.x
# Li H et al., J Clin Pharmacol 2012;52:89-101.
#   https://doi.org/10.1177/0091270010389470
# Davies NM, Clin Pharmacokinet 1998;34:101-154.
#   https://doi.org/10.2165/00003088-199834020-00002
# -----------------------------------------------------------------------------

IBUPROFEN_KE0 <- log(2) / (1.04 * 60)       # /min; Hannam 2018, t1/2 1.04 h
IBUPROFEN_FFAT_CL <- 0.863                  # Morse 2022, fat fraction in NFM for CL
IBUPROFEN_FFAT_V  <- 0.718                  # Morse 2022, fat fraction in NFM for V1, V2

#' Ibuprofen pharmacokinetics
#'
#' @inheritParams cefazolin
#' @param adjustToFFM when \code{TRUE}, evaluate the published total-weight
#'   term on intercompartmental clearance at the pharmacokinetic weight; when
#'   \code{FALSE}, at total body weight. Clearance and the volumes follow
#'   their own normal-fat-mass covariates either way.
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
ibuprofen <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  # Size scaling (see the header)
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyClearance = (weight / 70)^0.75)

  # Morse's standard man: 70 kg, 176 cm
  ffmStd   <- ffmAlSallami(70, 176, FFM_REFERENCE_AGE, SEX_MALE)    # 56.13 kg
  nfmClStd <- ffmStd + IBUPROFEN_FFAT_CL * (70 - ffmStd)           # 68.10 kg
  nfmVStd  <- ffmStd + IBUPROFEN_FFAT_V  * (70 - ffmStd)           # 66.09 kg
  nfmCl    <- size$ffm + IBUPROFEN_FFAT_CL * (weight - size$ffm)
  nfmV     <- size$ffm + IBUPROFEN_FFAT_V  * (weight - size$ffm)

  cl1 <- 3.79 * (nfmCl / nfmClStd)^0.75 / 60   # L/min, the model's own covariate
  v1  <- 6.05 * nfmV / nfmVStd
  v2  <- 4.37 * nfmV / nfmVStd
  cl2 <- 10.5 * size$clearance / 60
  v3  <- 1                                     # two compartments
  cl3 <- 0

  # Oral, fasted tablet (Morse Table 3), lag kept as published
  ka_PO              <- log(2) / 26.7    # 1/min, absorption half-life 26.7 min
  bioavailability_PO <- 0.941            # FIBU
  tlag_PO            <- 6.66             # min

  default <- list(
    v1 = v1,
    v2 = v2,
    v3 = v3,
    cl1 = cl1,
    cl2 = cl2,
    cl3 = cl3,
    ka_PO = ka_PO,
    bioavailability_PO = bioavailability_PO,
    tlag_PO = tlag_PO
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  # Not an opioid, so not on the MEAC panel.
  MEAC <- 0
  # Band, mg/L effect site, the range adult doses reach in the reference man.
  # Lower edge 5: the adult EC50 for dental pain (Hannam and Anderson 2011,
  # 5.07 mg/L), and about the effect-site trough of 400 mg by mouth every
  # 8 h at steady state (5.3).  Upper edge 35: the effect-site peak of the
  # largest single dose, 800 mg (32 by mouth, 36 intravenously over 30 min).
  # Typical line 17: the steady-state average of 400 mg by mouth every 6 h
  # (16.6).  The recovery threshold (endCe in the CSV) is 6.3 mg/L, the
  # effect-site concentration Anderson and Hannam 2019 give for a target
  # effect of 4 units on a 0-10 pain scale; a 400 mg tablet holds the effect
  # site above it from 48 min to 7.3 h.
  typical      <- 17
  upperTypical <- 35
  lowerTypical <- 5

  reference <- paste0(
    "Morse JD et al., Eur J Drug Metab Pharmacokinet 2022;47:497-507 ",
    "(intravenous and fasted-tablet two-compartment model, clearance and ",
    "volumes on normal fat mass); ke0 from Hannam JA et al., Paediatr ",
    "Anaesth 2018;28:841-851. https://doi.org/10.1007/s13318-022-00766-9"
  )

  return(
    list(
      PK = PK,
      # No tPeak: ke0 is supplied directly.  See the header.
      tPeak = 0,
      ke0 = IBUPROFEN_KE0,
      MEAC = MEAC,
      typical = typical,
      upperTypical = upperTypical,
      lowerTypical = lowerTypical,
      reference = reference
    )
  )
}
