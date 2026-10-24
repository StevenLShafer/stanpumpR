# -----------------------------------------------------------------------------
# Hydralazine, intravenous (Reece 1980): plasma hydralazine by a specific
# assay in healthy volunteers
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min.
#
# Drafted by Claude Code, 2026-10-24, at the request of Steven L. Shafer, as
# one of the vasoactive and resuscitation drugs used in intensive care
# (inst/help/models/vasoactive.md, inst/extdata/vasoactiveRegistry.csv).  Every
# number below was read from the published article, which he supplied:
# Tables I and II and the Methods.
#
# THE SOURCE
# ==========
# Reece PA, Cozamanis I, Zacest R.  Kinetics of hydralazine and its main
# metabolites in slow and fast acetylators.  Clin Pharmacol Ther
# 1980;28:769-778 (doi 10.1038/clpt.1980.234).  Ten healthy volunteers (five
# slow and five fast acetylators; nine men; 22-39 years, 57-86 kg) were given
# 0.375 mg/kg hydralazine (Apresoline) by constant-rate infusion over 5
# minutes; plasma was sampled to 10 hours and assayed by HPLC with
# fluorescence, a SPECIFIC assay that separates hydralazine from its
# acid-labile pyruvic acid hydrazone (the "apparent hydralazine" of earlier
# assays, which overstated the half-life and understated the clearance).  A
# two-compartment model was fitted to each subject, 1/C weighted.  Table II
# gives each subject's alpha, beta, both half-lives, clearance, the volume of
# the plasma compartment, Vss and Varea:
#
#   subject  alpha  beta (/h)  CL (L/h/kg)  V1 (L/kg)  Vss (L/kg)
#   P.R.      4.73  0.86        6.60        4.09       6.56
#   G.W.     12.56  1.33       10.75        1.96       5.58
#   S.B.     13.67  1.34        9.16        2.13       5.39
#   L.S.     10.29  0.99        6.53        1.66       4.90
#   C.R.      8.59  0.83        5.80        2.78       6.10
#   R.P.      7.80  0.78        8.25        2.89       7.86
#   C.D.      9.70  1.54       11.66        3.14       5.96
#   G.N.     10.04  1.46        9.16        2.01       4.44
#   Y.C.      9.09  0.84        6.68        2.36       6.52
#   P.F.      4.81  1.03        8.56        4.66       7.08
#
# Each subject's alpha, beta, CL and V1 fix his two-compartment model (k10 =
# CL/V1, k21 = alpha beta / k10, k12 = alpha + beta - k10 - k21), and from
# them his printed half-lives and Varea follow to rounding and his Vss to
# within 2-5%, which the test checks.  The two acetylator groups did not
# differ in any intravenous parameter (the phenotype acts on oral first-pass
# acetylation only), so all ten are pooled.
#
# THE TYPICAL SUBJECT
# ===================
# As for phenylephrine and verapamil, the arithmetic mean of the ten
# subjects' V1, V2 = V1 k12/k21, CL and Q = V1 k12, per kg:
#
#     V1 2.768 L/kg,  V2 3.132 L/kg,  CL 8.315 L/h/kg,  Q 9.433 L/h/kg
#
# which reproduces the paper's mean clearance and central volume exactly,
# gives a Vss of 5.90 L/kg against the mean 6.04, and half-lives of 5.0 min
# and 0.64 h against the means 5.1 min and 0.67 h.  The central volume is
# far larger than the plasma: hydralazine reacts with pyruvate in blood and
# binds to vessel walls within the first minutes, and the plasma clearance
# (about 9.7 L/min at 70 kg) is more than the cardiac output for the same
# reason.  Total plasma hydralazine is plotted; the pyruvic acid hydrazone,
# which is most of what circulates, is not.
#
# THE OTHER SPECIFIC-ASSAY STUDY
# ==============================
# Ludden 1980 (Clin Pharmacol Ther 28:736), eight older hypertensive
# patients, the same kind of assay: clearance 4.4 L/h/kg, about half of
# Reece's, with Vss 1.83 L/kg and a 54-min terminal half-life, and no
# distribution phase, so it does not fix a model.  Ludden's 1982 review sets
# the two side by side and attributes the difference to the subjects or to
# the methods (assay, infusion length, sampling, weighting).  The model is
# of young healthy volunteers.
#
# DOSE BASIS
# ==========
# Apresoline injection is hydralazine hydrochloride, and the paper gives the
# dose as "0.375 mg/kg HP, Apresoline", so a dose is entered as labelled.
# The paper's clearance is that dose over the AUC of hydralazine base (its
# mean dose / CL is exactly the Table III AUC in umol h/L at the base's
# 160.17 g/mol), so if the paper's milligrams were the base after all, every
# concentration here is 23% high (196.64 / 160.17).
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# Published per kilogram of body weight, mean weight 73 kg: the 70-kg values
# scaled to fat-free mass with the switch on, and linearly in weight with it
# off (the default legacy factors).
#
# PREGNANCY (R/pregnancy.R)
# =========================
# No study of intravenous hydralazine in pregnancy was found.  Han 2019 (J
# Clin Pharmacol 59:1678, oral, 12 pregnant women, no nonpregnant comparison)
# judged its apparent oral clearance comparable to nonpregnant reports; the
# registry records that, and nothing is applied.
#
# NO EFFECT SITE, NO BAND
# =======================
# No concentration-effect relation was fitted, and the plasma curve is not
# the time course of the effect: the fall in diastolic pressure came 15 to 30
# minutes after the infusion (Reece) and lasts hours after the plasma
# hydralazine is gone.  No pressure is predicted.
# -----------------------------------------------------------------------------

# Reece 1980, Table II: alpha and beta in /h, CL in L/h/kg, V1 in L/kg
HYDRALAZINE_REECE <- data.frame(
  subject = c("P.R.", "G.W.", "S.B.", "L.S.", "C.R.",
              "R.P.", "C.D.", "G.N.", "Y.C.", "P.F."),
  alpha = c(4.73, 12.56, 13.67, 10.29, 8.59, 7.80, 9.70, 10.04, 9.09, 4.81),
  beta  = c(0.86, 1.33, 1.34, 0.99, 0.83, 0.78, 1.54, 1.46, 0.84, 1.03),
  CL    = c(6.60, 10.75, 9.16, 6.53, 5.80, 8.25, 11.66, 9.16, 6.68, 8.56),
  V1    = c(4.09, 1.96, 2.13, 1.66, 2.78, 2.89, 3.14, 2.01, 2.36, 4.66)
)

#' Each subject's two-compartment model from Reece 1980
#'
#' From alpha, beta, CL and V1: k10 = CL/V1, k21 = alpha beta/k10,
#' k12 = alpha + beta - k10 - k21.  See the header of
#' \code{R/drugs_hydralazine.R}.
#'
#' @returns a data frame of V1, V2 (L/kg) and CL, Q (L/min/kg), one row per
#'   subject
#' @keywords internal
hydralazineSubjects <- function(k = HYDRALAZINE_REECE)
{
  k10 <- k$CL / k$V1
  k21 <- k$alpha * k$beta / k10
  k12 <- k$alpha + k$beta - k10 - k21
  data.frame(v1 = k$V1, v2 = k$V1 * k12 / k21,
             cl1 = k$CL / 60, cl2 = k$V1 * k12 / 60)
}

#' The typical hydralazine model from Reece 1980
#'
#' The arithmetic mean of the ten subjects' V1, V2, CL and Q, per kg.
#'
#' @returns a named numeric vector, L/kg and L/min/kg
#' @keywords internal
hydralazineTypical <- function()
{
  colMeans(hydralazineSubjects())
}

#' Hydralazine, intravenous (Reece 1980)
#'
#' Two compartments, the mean of ten healthy volunteers' fits with a specific
#' assay; see the header of \code{R/drugs_hydralazine.R}.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
hydralazine <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)
  p <- hydralazineTypical() * FFM_REFERENCE_WEIGHT

  default <- list(
    v1  = p[["v1"]]  * size$volume,
    v2  = p[["v2"]]  * size$volume,
    v3  = 1,                          # placeholder: a two-compartment model
    cl1 = p[["cl1"]] * size$clearance,
    cl2 = p[["cl2"]] * size$clearance,
    cl3 = 0
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Reece PA, Cozamanis I, Zacest R, Clin Pharmacol Ther 1980;28:769-778. ",
    "Two compartments, the mean of 10 healthy volunteers' IV fits with a ",
    "specific assay: V1 2.77 L/kg, V2 3.13 L/kg, CL 8.3 L/h/kg, Q 9.4 L/h/kg. ",
    "Plasma hydralazine (not its hydrazone). Doses as the hydrochloride. ",
    "https://doi.org/10.1038/clpt.1980.234"
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
