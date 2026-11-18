# -----------------------------------------------------------------------------
# Liothyronine (Cytomel and generics; liothyronine sodium tablets): two
# compartments, oral, from patients on established T3 therapy with no thyroid
# hormone of their own
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min; amounts in
# mcg, so the plotted concentration is ng/mL (= mcg/L) of total serum
# triiodothyronine (T3).  100 ng/dL is 1 ng/mL; T3 is usually reported in
# ng/dL (reference range about 90-215 ng/dL, 0.9-2.15 ng/mL).
#
# Drafted by Claude Code, 2026-11-18, at the request of Steven L. Shafer, who
# asked for liothyronine that day and supplied the paper below.  Every number
# was read in the full text.
#
# WHAT IS PLOTTED
# ===============
# The T3 the doses add: total serum T3 ABOVE the patient's own baseline.  The
# source's patients had had a near-total thyroidectomy and no T4 on board
# (their free T4 was at or below the assay's limit during T3 therapy, Table
# 2), so in them the curve is the serum T3.  In a patient with a working
# thyroid, or one also taking levothyroxine, the patient's own T3 (from the
# gland and from the deiodination of T4) adds to it, and exogenous T3 lowers
# TSH and with it the gland's own output; neither is modelled.
#
# SOURCE: VAN TASSELL 2019 (two compartments, steady state)
# =========================================================
# Van Tassell B, Wohlford GF IV, Linderman JD, Smith S, Yavuz S, Pucino F,
# Celi FS.  Pharmacokinetics of L-triiodothyronine in patients undergoing
# thyroid hormone therapy withdrawal.  Thyroid 2019;29:1371-1379.  doi
# 10.1089/thy.2019.0101 (PMID 31364488).  Fourteen patients after near-total
# thyroidectomy for differentiated thyroid cancer (Table 1: 48.5 +/- 16.0
# years, 7 men and 7 women, 72.3 +/- 15.8 kg), switched from levothyroxine to
# liothyronine at 1:3 mcg/mcg three times a day for at least 30 days (53.7 +/-
# 12.3 mcg/day, morning dose 18.8 +/- 4.4 mcg), then studied after their last
# morning dose, taken fasting, with samples from -30 min to 12 h and then
# every 12 h for 10 days (total T3 by immunoassay, Siemens Immulite 2500).
# Each patient's curve was fitted with "a two-compartment oral dosing model
# with first-order absorption, a lag time, and linear elimination from the
# sampling compartment" (Monolix 2019R1), assuming complete absorption.
# Table 3, total T3, means of the 14 individual estimates:
#
#     Tlag 0.62 h   ka 1.94 /h   V 86.7 dL   ke 0.12 /h   k12 0.13 /h
#     k21 0.08 /h
#
# So V1 = 8.67 L, CL = ke V1 = 1.0404 L/h (24.97 L/day), Q = k12 V1 = 1.1271
# L/h and V2 = Q/k21 = 14.089 L (Vss 22.76 L).  The half-lives the means give
# are 2.33 h and 21.5 h, against the paper's means of the individual
# half-lives, 2.3 +/- 0.11 h and 22.9 +/- 7.7 h.  Absorption was taken as
# complete, so V1, V2, CL and Q are apparent (/F); the label says T3 "is
# almost totally absorbed, 95 percent in 4 hours".
#
# WHY THE STEADY-STATE FIT
# ========================
# The same paper fitted a first dose given to five of the patients after the
# radioiodine treatment, while profoundly hypothyroid (Table 3: one
# compartment behind a transit compartment, V 149 dL, ke 0.24 /h; Cmax 140
# +/- 40 ng/dL against 320 +/- 60 at steady state): "the PK characteristics
# of LT3 as tested after withdrawal in a state of profound hypothyroidism are
# dramatically different from the ones observed at steady state", the
# authors attributing the larger volume to binding sites freed by the
# depletion of thyroid hormone.  The model uses the steady-state fit,
# because that is the state of a patient on liothyronine, which is what
# the drug is dosed in.  A first dose to a profoundly hypothyroid patient
# (myxedema, or after a withdrawal) peaks far lower than this curve.
#
# INTRAVENOUS DOSES ARE NOT OFFERED
# =================================
# Liothyronine sodium injection exists (for myxedema coma), but no source
# gives its disposition: the injection's label says "no pharmacokinetic
# studies have been performed with intravenous liothyronine (T3) in myxedema
# coma or precoma patients", and this model is apparent oral and fitted in
# the euthyroid-on-T3 state, the opposite of myxedema [decision for Shafer:
# whether to offer it with the label's 95% absorption].
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# The source fitted no covariate (its sample of 14 was too small to show
# even an effect of sex).  Its parameters are taken as the 70 kg reference
# man's (its cohort averaged 72.3 kg) and scaled by pkSizeFactors(): by
# fat-free mass with the switch on, not at all with it off (the published
# model).
#
# DOSE
# ====
# Cytomel "tablets contain liothyronine sodium equivalent to liothyronine in
# 5 mcg, 25 mcg, and 50 mcg" (label section 11), so a dose is entered as
# labelled and is already mcg of T3: no salt conversion.  The source dosed
# the same generic tablets.
#
# CHECKS (tests/testthat/test-drugs-liothyronine.R)
# =================================================
# * The source's own patients at steady state, 18.8 mcg every 8 hours: this
#   model peaks at 306 ng/dL at 1.74 h, against the measured Cmax 320 +/- 60
#   ng/dL at a Tmax of 1.8 +/- 0.3 h (Table 3 text); trough 156 ng/dL.
# * The source's Figure 4 simulation, 50 mcg a day at 72.5 kg: a mean of 206
#   ng/dL whether given three times, twice or once a day.  This model gives
#   200 ng/dL (the average is dose/CL, whatever the schedule), with peaks of
#   271, 327 and 515 ng/dL.
# * Clearance against Inada 1975 (J Clin Invest 55:1337, Table II, the T3
#   extrapolation analysis after intravenous 131I-T3 in five normal men, read
#   in full): 22.2 L/day per 60 kg, 26.8 L/day at 72.3 kg, against this
#   model's 25.0 L/day (7% lower).  Only clearance is comparable between
#   sources (AGENTS.md); Inada's one-compartment volume (54.2 L per 60 kg)
#   comes from two weeks of a tracer and is not.
# * Euthyroid volunteers, not used: Celi's group (Front Endocrinol 2022,
#   PMC8919711, read in full; 12 healthy volunteers, 52.5 +/- 9.0 mcg of an
#   oral liquid) measured a baseline-corrected peak of 327 +/- 57 ng/dL at 2
#   h.  This model gives 449 ng/dL at 1.85 h for that dose, 37% higher: a
#   volunteer with a working thyroid is not a patient on T3, and the source's
#   two states already differ more than this.
#
# PREGNANCY AND THE BAND
# ======================
# No pharmacokinetic study of liothyronine in pregnancy was found (PubMed,
# 2026-11-18); levothyroxine, not liothyronine, is the treatment in
# pregnancy.  Nothing is applied (pregnancy registry row).  No band: the
# total T3 reference interval is a laboratory normal range, not a therapeutic
# range, and the drug is titrated to TSH (therapeutic-range row).
# -----------------------------------------------------------------------------

# Van Tassell 2019, Table 3, total T3, last dose at steady state (means of
# the individual estimates)
LIOTHYRONINE_V1   <- 86.7 / 10   # L (86.7 dL)
LIOTHYRONINE_KE   <- 0.12        # /h
LIOTHYRONINE_K12  <- 0.13        # /h
LIOTHYRONINE_K21  <- 0.08        # /h
LIOTHYRONINE_KA   <- 1.94        # /h
LIOTHYRONINE_TLAG <- 0.62        # h

#' Liothyronine (Van Tassell 2019)
#'
#' Two compartments with a lagged first-order absorption, fitted at steady
#' state in thyroidectomised patients on liothyronine alone; total serum T3
#' above the patient's baseline.  See the header of
#' \code{R/drugs_liothyronine.R}.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
liothyronine <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume = 1, legacyClearance = 1)
  v1 <- LIOTHYRONINE_V1
  cl <- LIOTHYRONINE_KE * v1               # L/h
  q  <- LIOTHYRONINE_K12 * v1              # L/h
  v2 <- q / LIOTHYRONINE_K21               # L

  default <- list(
    v1  = v1 * size$volume,
    v2  = v2 * size$volume,
    v3  = 1,
    cl1 = cl / 60 * size$clearance,
    cl2 = q / 60 * size$clearance,
    cl3 = 0,
    ka_PO = LIOTHYRONINE_KA / 60,
    bioavailability_PO = 1,               # apparent: complete absorption assumed
    tlag_PO = LIOTHYRONINE_TLAG * 60
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  list(
    PK = PK,
    tPeak = 0,
    MEAC = 0,
    typical = 0,
    upperTypical = 0,
    lowerTypical = 0,
    reference = paste0(
      "Van Tassell B, Wohlford GF, Linderman JD, et al., Thyroid 2019;29:1371-1379, ",
      "Table 3 (total T3, last dose at steady state, means of 14 thyroidectomised ",
      "patients on liothyronine alone): two compartments, V 8.67 L, ke 0.12 /h, ",
      "k12 0.13 /h, k21 0.08 /h, ka 1.94 /h, lag 0.62 h, apparent (complete ",
      "absorption assumed); doses in mcg of liothyronine, plotted as total serum T3 ",
      "above the patient's own baseline. https://doi.org/10.1089/thy.2019.0101")
  )
}
