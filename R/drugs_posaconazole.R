# -----------------------------------------------------------------------------
# Posaconazole, intravenous (NOXAFIL injection): two compartments derived
# exactly from the US label
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min.
#
# Drafted by Claude Code, 2026-11-26, on Steven L. Shafer's decision of that
# day ("Derive from the label for now"): the population model of the tablet
# (van Iersel 2018, Antimicrob Agents Chemother 62:e02465-17, PMID 29712663)
# prints its covariate effects without the equations or reference weight
# they need, the FDA reviews of NDAs 205053 and 205596 and the EMA product
# information hold no population model, and this label derivation stands in
# until a published model can be built (docs/library-expansion/plan.csv).
# The construction follows methyleneBlue (R/drugs_methyleneBlue.R).
#
# THE SOURCE
# ==========
# NOXAFIL (posaconazole) prescribing information, Merck Sharp & Dohme,
# revised 2/2026 (DailyMed set id b073b082-7b57-4423-8c06-4fd4263d6f84;
# injection NDA 205596), section 12.3:
#   "The mean volume of distribution of posaconazole after intravenous
#   solution administration was 261 L and ranged from 226-295 L between
#   studies and dose levels."
#   "Noxafil injection is eliminated with a mean terminal half-life (t1/2)
#   of 27 hours and a total body clearance (CL) of 7.3 L/h."
#   Table 19, healthy volunteers, 300 mg over 30 minutes by peripheral vein
#   (n = 9): AUC(0-inf) 46 400 ng h/mL, AUC(0-12) 13 000, Cmax 2840 ng/mL,
#   t1/2 24.6 h, CL 6.9 L/h.
#   "dose proportional pharmacokinetics after single doses between 200 and
#   300 mg"; the EU product information adds that single infusions are MORE
#   than dose proportional from 50 to 200 mg.
#
# THE DERIVATION
# ==============
# A two-compartment model has four parameters, fixed here by four numbers:
# the clearance (7.3 L/h), the terminal rate constant (ln 2 / 27 h), the
# mean residence time Vss / CL (the label's 261 L read as Vss), and the
# Cmax at the end of a 30-minute infusion of 300 mg (2840 ng/mL).  With the
# bolus coefficients A1, A2,
#
#     A1/lambda1   + A2/lambda2   = 1 / CL
#     A1/lambda1^2 + A2/lambda2^2 = MRT / CL
#     Cmax = (dose/T) sum Ai (1 - exp(-lambda_i T)) / lambda_i,  T = 30 min
#
# the first two give A1 and A2 for any lambda1, and the third fixes lambda1
# (posaconazoleFromLabel() solves it):
#
#     V1 95.1 L,  V2 165.9 L,  CL 7.3 L/h,  Q 34.5 L/h
#     half-lives 1.11 h and 27 h
#
# The clearance, half-life and volume are the label's summary across its
# intravenous studies; the Cmax is Table 19's healthy volunteers.  Table 19's
# own row cannot stand alone: its four numbers (AUC to infinity, AUC to 12 h,
# Cmax, t1/2) admit no two-compartment model (the 12-hour area is 5% below
# any that matches the other three), and its 261 L, read with that row's
# clearance and half-life, would exceed the terminal volume, which Vss
# cannot.  261 L is read as Vss because a terminal volume is the larger,
# and 7.3 x 27 / ln 2 = 284 L.
#
# THE CHECKS (NOT FITTED)
# =======================
# 1. Table 19, healthy volunteers, AUC(0-12) after 300 mg over 30 min:
#    13 300 ng h/mL against 13 000 (+2%).
# 2. Table 20, patients at steady state on 300 mg over 90 min once daily
#    (after twice daily on day 1, n = 49): Cmax 3480 against 3280 ng/mL
#    (+6%), Cmin 1155 against 1090 (+6%), Cav 1710 against 1500 (+14%;
#    their AUC gives CL 8.3 L/h).
# 3. Table 19, patients on day 1, 300 mg over 90 min (n = 22): Cmax 2370
#    against 1590 ng/mL (+49%), AUC(0-12) 12 900 against 8240 (+57%).  The
#    model overstates the first dose in patients by half; at steady state
#    it is within 6% of the peak and trough.  The label does not explain the
#    day-1 difference; patients' day-1 values carry a CV of 62%.
#
# BODY SIZE ANCHOR (docs/weight-adjustment.md)
# ============================================
# The label prints no weight, age or sex for any of the subjects behind its
# summary values, so the derivation is anchored at the reference man (70
# kg, FFM_REFERENCE_WEIGHT) and scaled to fat-free mass with the switch on,
# linearly in weight with it off.  The label's own statements on weight are
# for the tablet ("patients weighing greater than 120 kg may have lower
# posaconazole exposure"; patients under 60 kg higher) and are not applied
# beyond what the scaling gives.
#
# COVARIATES (NOT APPLIED)
# ========================
# The label: "No clinically significant differences in the pharmacokinetics
# of posaconazole were observed based on age, sex, renal impairment, and
# indication."  Renal clearance is a minor pathway (13% of a radiolabelled
# dose in urine, <0.2% as parent), so no renal term is applied and
# `creatinine` is not read.  Hepatic impairment raised the oral suspension's
# AUC 21-43% without a consistent order by severity; not applied.  The
# injection's vehicle (betadex sulfobutyl ether sodium) accumulates in renal
# impairment, which is a matter of the vehicle, not of posaconazole.
#
# DOSE BASIS
# ==========
# Doses are posaconazole as labelled (300 mg/16.7 mL, 18 mg/mL); the
# injection is given only as an infusion ("Do NOT administer Noxafil
# injection as an intravenous bolus injection"), usually over 90 minutes.
# Pediatric patients from 2 years: 6 mg/kg up to 300 mg.
#
# PREGNANCY (R/pregnancy.R)
# =========================
# No pharmacokinetic study of posaconazole in pregnancy was found (PubMed,
# 2026-11-26: case reports only; Pilmis 2015 J Antimicrob Chemother 70:14,
# a review, calls such data "critically needed").  The label: "Available
# data for use of Noxafil in pregnant women are insufficient".  Nothing is
# applied; the registry records pregnancy as unknown.
#
# EFFECT SITE AND BAND
# ====================
# No effect site.  No band: Abdul-Aziz 2020 (Intensive Care Med 46:1127,
# read in full) gives trough thresholds rather than a range (Cmin above
# 0.5-0.7 mg/L for prophylaxis, above 1 mg/L for treatment; a putative
# toxicity threshold of 3.75-4 mg/L "yet to be validated clinically"); see
# inst/extdata/therapeuticRanges.csv.
# -----------------------------------------------------------------------------

# NOXAFIL label, section 12.3 (intravenous solution)
POSACONAZOLE_LABEL_CL       <- 7.3     # L/h, total body clearance
POSACONAZOLE_LABEL_THALF    <- 27      # h, terminal
POSACONAZOLE_LABEL_VSS      <- 261     # L
# Table 19, healthy volunteers: 300 mg over 30 minutes
POSACONAZOLE_LABEL_DOSE     <- 300     # mg
POSACONAZOLE_LABEL_INFUSION <- 30      # min
POSACONAZOLE_LABEL_CMAX     <- 2840    # ng/mL

#' The posaconazole two-compartment model derived from the label
#'
#' Solves the NOXAFIL label's clearance, terminal half-life and volume of
#' distribution (read as Vss) and its peak after 300 mg over 30 minutes for
#' the two-compartment model they determine, at \code{FFM_REFERENCE_WEIGHT}.
#' See the header of \code{R/drugs_posaconazole.R}.
#'
#' @returns a list: \code{v1}, \code{v2} (L), \code{cl1}, \code{cl2} (L/min),
#'   the exponents \code{lambda} (/min) and bolus coefficients \code{A}
#'   (mg/L per mg)
#' @keywords internal
posaconazoleFromLabel <- function()
{
  cl  <- POSACONAZOLE_LABEL_CL / 60                               # L/min
  mrt <- POSACONAZOLE_LABEL_VSS / cl                              # min
  l2  <- log(2) / (POSACONAZOLE_LABEL_THALF * 60)                 # /min
  dose <- POSACONAZOLE_LABEL_DOSE
  T   <- POSACONAZOLE_LABEL_INFUSION

  coefficients <- function(l1)
    solve(matrix(c(1 / l1, 1 / l2, 1 / l1^2, 1 / l2^2), 2, byrow = TRUE),
          c(1 / cl, mrt / cl))
  cmax <- function(l1) {
    A <- coefficients(l1)
    l <- c(l1, l2)
    sum(A * dose / T * (1 - exp(-l * T)) / l) * 1000              # ng/mL
  }
  l1 <- stats::uniroot(function(x) cmax(x) - POSACONAZOLE_LABEL_CMAX,
                       c(2 * l2, 1), tol = 1e-14)$root
  A <- coefficients(l1)
  l <- c(l1, l2)

  v1  <- 1 / sum(A)
  k21 <- sum(A * rev(l)) / sum(A)
  k10 <- prod(l) / k21
  k12 <- sum(l) - k21 - k10
  list(v1 = v1, v2 = v1 * k12 / k21, cl1 = k10 * v1, cl2 = k12 * v1,
       lambda = l, A = A)
}

#' Posaconazole, intravenous (NOXAFIL label)
#'
#' Two compartments derived exactly from the NOXAFIL label's clearance,
#' terminal half-life and volume of distribution and its peak after 300 mg
#' over 30 minutes; see the header of \code{R/drugs_posaconazole.R}.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
posaconazole <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)
  p <- posaconazoleFromLabel()

  default <- list(
    v1  = p$v1  * size$volume,
    v2  = p$v2  * size$volume,
    v3  = 1,                          # placeholder: a two-compartment model
    cl1 = p$cl1 * size$clearance,
    cl2 = p$cl2 * size$clearance,
    cl3 = 0
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "NOXAFIL (posaconazole) prescribing information, Merck, rev. 2/2026 ",
    "(injection NDA 205596), section 12.3. Two compartments derived exactly ",
    "from the label's clearance 7.3 L/h, terminal half-life 27 h and volume ",
    "of distribution 261 L (read as Vss) and its peak of 2840 ng/mL after ",
    "300 mg over 30 min, at an assumed 70 kg: V1 95.1 L, V2 166 L, ",
    "CL 7.3 L/h, Q 34.5 L/h. Within 6% of the label's steady-state peak and ",
    "trough in patients, but half again above their first-day values. ",
    "A stand-in until a published population model can be built."
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
