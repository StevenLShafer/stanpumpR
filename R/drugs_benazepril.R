# -----------------------------------------------------------------------------
# Benazepril (Lotensin) -- the curve is BENAZEPRILAT, the active metabolite
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min.
#
# Drafted by Claude Code, 2026-10-20, at the request of Steven L. Shafer, from
# the full text of Kaiser G, Ackermann R, Brechbuhler S, Dieterle W,
# Biopharm Drug Dispos 1989;10:365-376, which he supplied.  Tests:
# tests/testthat/test-drugs-benazepril.R.
#
# WHAT IS PLOTTED, AND WHY IT IS NOT THE DRUG DOSED
# =================================================
# Benazepril is an ester prodrug with, in the source's words, "virtually no
# inhibitory activity on the converting enzyme at concentrations present in
# plasma"; the paper concludes that "the kinetics of the unchanged compound
# administered are of minor importance for the pharmacokinetic
# characterization of the drug".  So the plotted curve is BENAZEPRILAT, the
# active diacid, and a dose is entered as prescribed, as milligrams of
# benazepril hydrochloride.  The parent is not plotted at all.
#
# THE SOURCE IS NON-COMPARTMENTAL, AND IT STILL CLOSES.  HERE IS WHY
# ==================================================================
# Kaiser analysed "by model independent methods": trapezoidal areas, half-lives
# by linear regression of the semi-logarithmic curve, and benazeprilat's two
# phases separated by the method of residuals.  No compartmental model was
# fitted anywhere in the paper, so the vector below is DERIVED, as
# clevidipine's is (docs/antihypertensives.md section 11).
#
# Two compartments need four numbers and a non-compartmental paper usually
# prints three (a clearance, a volume and a terminal half-life), which is why
# esmolol and losartan both fail the section 4 identifiability test.  (Fosinopril
# was listed here too until 2026-10-29, wrongly: the check had been applied to
# Gehr 1993 and carried to Hui 1991, which nobody had opened.  Hui prints six
# numbers and closes linearly, and the drug is implemented.)
# Benazeprilat escapes it because Kaiser's Table 2 prints FOUR:
#
#     t1/2,alpha  2.7 h          AUC(0-24h)  2649 (pmol/g).h
#     t1/2,beta  22.3 h          AUC(0-inf)  3185 (pmol/g).h
#
# TWO AUCs, not one.  Their difference is the area under the terminal phase
# alone, and since that phase is a single exponential, beta x AUC(24-inf)
# recovers the concentration at 24 h -- 16.66 pmol/g -- and with it the beta
# intercept.  That is the fourth number.  So:
#
#     B    = C(24) x exp(beta x 24)        = 35.1 pmol/g
#     A    = (AUC(0-inf) - B/beta) x alpha = 527.5 pmol/g
#     k21  = (A beta + B alpha) / (A + B)  = 0.04517 /h
#     k10  = alpha beta / k21              = 0.17666 /h
#     k12  = alpha + beta - k21 - k10      = 0.06598 /h
#     V1   = dose / (A + B)
#
# All three rate constants come out positive, so the decomposition is valid,
# and two independent checks hold.  The apparent clearance it implies equals
# dose/AUC exactly; and the renal fraction it implies, CLr over that apparent
# clearance, is 20.0%, which is the source's own statement that "about 20 per
# cent of the dose were excreted as benazeprilat".
#
# CONDITIONING: STRUCTURALLY AS STABLE AS CLEVIDIPINE'S, ON A NOISIER INPUT
# =========================================================================
# Section 3.3 rejected esmolol's decomposition for conditioning rather than
# identifiability, so this one was stress-tested the same way.  Perturbing the
# tail area by 10% moves V1 by 4.5%, V2 by 9% and Q by 13.5% -- almost exactly
# clevidipine's sensitivity (4%, 10%, 13% for a 5% perturbation).  The
# difference is that the INPUT here is noisier: the tail is a difference of two
# measured areas, 3185 - 2649, whose standard errors give it about +/- 25%.  At
# that, V1 carries +/- 13%, V2 +/- 22% and Q +/- 37%.
#
# Treat those as a FLOOR rather than a bound.  The two areas have different
# subject counts -- AUC(0-24h) is n = 60 and AUC(0-inf) is n = 45, because 15
# subjects had no 48 or 72 h sample -- so they are not paired subject-level
# summaries and their difference carries a selection component on top of the
# sampling error, which nothing printed quantifies.  The apparent CLEARANCE is
# unaffected either way, being dose/AUC and independent of the split, so the
# AREA under the plotted curve is firm and the SHAPE of its distribution phase
# is the soft part.
#
# EVERY PARAMETER IS APPARENT IN THE FORMATION FRACTION
# =====================================================
# The formation fraction fm cannot be recovered from this paper, and the 16.8%
# urinary recovery does not supply it: that figure is arithmetically CLr x
# AUC/dose, which is already implied by the clearance and the area, so it
# carries no independent information (it reproduces to 16.6% here).  Waldmeier
# 1991's 14C balance bounds fm loosely between about 25% and the 37% of the dose
# absorbed, which is too wide to use.
#
# None of that matters for the plotted curve, and this is worth being clear
# about: the parameters are apparent in fm exactly as an ordinary oral drug's
# are apparent in F, and an apparent parameterisation gives the CORRECT
# concentration, because the same fm divides the volume and the dose.  The three
# rate constants are absolute -- fm cancels in every ratio -- so the shape is
# fm-free and only the volumes carry it.
#
# THE SALT AND MOIETY CORRECTION
# ==============================
# The dose is benazepril hydrochloride (MW 460.96) and the curve is
# benazeprilat (MW 396.44), so the apparent volumes and clearances below are the
# molar ones multiplied by 460.96/396.44 = 1.16275.  That converts "pmol/g of
# benazeprilat per micromole of benazepril hydrochloride", which is what Kaiser
# measured, into "ng/mL of benazeprilat per milligram of benazepril
# hydrochloride", which is what the dose table supplies.  Azilsartan carries the
# same kind of factor for the same kind of reason.
#
# WHAT THE ABSORPTION RATE CONSTANT IS, AND WHAT IT IS NOT
# ========================================================
# It is an EFFECTIVE absorption-plus-conversion rate, not a measured ka: the
# real input to benazeprilat is the convolution of benazepril's absorption with
# its conversion, and no paper fits that.  It is set to the value that puts the
# metabolite's peak at the measured 1.5 h, 1.433 /h.  The corroboration is that
# this lands within 24% of benazepril's OWN elimination rate constant of
# 1.155 /h (its half-life is 0.6 h), which is what should rate-limit the
# formation -- and the fit was never told that number.
#
# THE ONE THING THE MODEL GETS WRONG, AND THE LIKELY REASON
# =========================================================
# It underpredicts the peak by 20%: 156 ng/mL against Kaiser's 195 ng/mL for a
# 10 mg dose (the area is exact, and AUC(0-24h) comes back to 0.7%).  The
# probable cause is not the model.  Kaiser's Cmax is a MEAN OF INDIVIDUAL PEAK
# CONCENTRATIONS across 60 subjects whose times to peak ranged from 1 to 6
# hours, and averaging peaks that occur at different times always exceeds the
# peak of the averaged curve.  A 20% bias is unremarkable for that spread.  It
# cannot be verified from the printed data, so it is stated rather than
# corrected, and the help page says the peak is the least reliable part of this
# curve.
#
# THE TERMINAL PHASE IS SATURABLE BINDING, LINEARISED
# ===================================================
# Benazeprilat's 22.3 h terminal phase carries only 17% of the area and runs at
# 1 to 3 per cent of peak concentrations.  Physically it is binding to
# converting enzyme, not a tissue compartment, and a linear peripheral
# compartment is its linearisation at low concentration -- the same reduction
# lisinopril's header describes, and the reason captopril was not pursued at
# all.  One caution for anyone revising this: Kaiser's accumulation ratios fall
# with dose (1.30 at 5 mg, 1.25 at 10 mg, 1.12 at 20 mg), which is weak
# evidence that the binding is already saturating across the therapeutic range.
# The model does not represent that, and must not be read as a dose-proportional
# claim beyond 20 mg.
#
# AN EXTERNAL CHECK THAT CONFIRMS THE SHAPE AND FLAGS THE SCALE
# =============================================================
# Sun 1994 (PMID 7867683) measured the same 10 mg dose in mass units and reports
# Cmax 257 ng/mL and AUC(0-24h) 1410 ng.h/mL, against Kaiser's 195 and 1050.
# The ratios are 1.32 and 1.34 -- THE SAME FACTOR ON BOTH -- so two independent
# studies agree on the shape of this curve and disagree on its scale by a third.
# That is recorded, not reconciled: rescaling a primary fit by another study's
# number would be fitting to a result rather than to data.  A user comparing
# this curve with a published benazeprilat concentration should expect it to sit
# at the low end.
#
# NO COVARIATES, NO EFFECT SITE, NO BLOOD PRESSURE
# ================================================
# Kaiser fitted none: 62 healthy young men, one dose level for the single-dose
# analysis.  There is no renal term, so the signature does not name creatinine
# even though benazeprilat is substantially renally cleared -- the source cannot
# speak to it.  tPeak, MEAC and the band are zero, as for every antihypertensive
# here: nothing in this library plots a pressure (docs/antihypertensives.md).
# -----------------------------------------------------------------------------

# Derived from Kaiser 1989 Table 2 (see the header), expressed as benazeprilat
# ng/mL per milligram of benazepril hydrochloride dosed.
BENAZEPRIL_V1_F <- 44.83    # L, apparent central
BENAZEPRIL_V2_F <- 65.48    # L, apparent peripheral
BENAZEPRIL_CL_F <- 7.920    # L/h, apparent
BENAZEPRIL_Q_F  <- 2.958    # L/h, apparent intercompartmental
BENAZEPRIL_KA   <- 1.4330   # /h, effective absorption plus conversion

#' Benazepril pharmacokinetics, plotted as benazeprilat
#'
#' Two compartments apparent in the metabolite's formation fraction, derived
#' from Kaiser 1989's non-compartmental summary; the plotted moiety is
#' benazeprilat, the active diacid, and the dose is benazepril hydrochloride as
#' prescribed.  See the file header for the derivation, its conditioning and
#' its limits.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
benazepril <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  # Kaiser fitted no size covariate -- 62 healthy men at one dose level -- so
  # the published parameters are unscaled with the switch off, and the library's
  # own factors apply with it on (docs/weight-adjustment.md).
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume = 1)

  default <- list(
    v1  = BENAZEPRIL_V1_F * size$volume,
    v2  = BENAZEPRIL_V2_F * size$volume,
    v3  = 1,                                      # two compartments
    cl1 = BENAZEPRIL_CL_F / 60 * size$clearance,  # L/h -> L/min
    cl2 = BENAZEPRIL_Q_F  / 60 * size$clearance,
    cl3 = 0,
    ka_PO = BENAZEPRIL_KA / 60,                   # /h -> /min
    # Apparent parameters already carry the formation fraction and the
    # salt/moiety ratio: the dose must not be multiplied by either again.
    bioavailability_PO = 1,
    tlag_PO = 0                                   # none was fitted
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  tPeak <- 0    # no equilibration constant published; plasma only
  MEAC  <- 0

  # No established therapeutic plasma range for benazeprilat.
  typical      <- 0
  upperTypical <- 0
  lowerTypical <- 0

  reference <- paste0(
    "Kaiser G, Ackermann R, Brechbuhler S, Dieterle W. Pharmacokinetics of the ",
    "angiotensin converting enzyme inhibitor benazepril.HCl (CGS 14 824 A) in ",
    "healthy volunteers after single and repeated administration. ",
    "Biopharm Drug Dispos 1989;10:365-76. THE PLOTTED MOIETY IS BENAZEPRILAT, ",
    "the active diacid; the source states that unchanged benazepril has ",
    "virtually no converting-enzyme activity at plasma concentrations. Two ",
    "compartments apparent in the formation fraction, DERIVED from the paper's ",
    "non-compartmental Table 2 in 62 healthy men given 10 mg: its two ",
    "half-lives (2.7 h and 22.3 h) with BOTH its areas, AUC(0-24h) 2649 and ",
    "AUC(0-inf) 3185 (pmol/g).h, whose difference recovers the terminal ",
    "intercept and closes the system. V1/fm 44.8 L, V2/fm 65.5 L, CL/fm ",
    "7.92 L/h, Q/fm 2.96 L/h as benazeprilat ng/mL per mg benazepril ",
    "hydrochloride, and an effective absorption-plus-conversion rate of ",
    "1.43 /h set from the measured 1.5 h time to peak. The derived clearance ",
    "reproduces the source's own 20% urinary recovery of the dose. The model ",
    "underpredicts the reported peak by 20%, which its mean-of-individual-peaks ",
    "definition over times to peak of 1 to 6 h would account for. Sun 1994 ",
    "measured the same dose 1.33-fold higher in both peak and area, the same ",
    "factor on each, so the shape is corroborated and the scale is not. No ",
    "covariate of any kind was fitted, including none for renal function. ",
    "https://doi.org/10.1002/bdd.2510100404"
  )

  return(
    list(
      PK = PK,
      tPeak = tPeak,
      MEAC = MEAC,
      typical = typical,
      upperTypical = upperTypical,
      lowerTypical = lowerTypical,
      reference = reference
    )
  )
}
