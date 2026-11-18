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
# even an effect of sex), so its parameters are its cohort's typical
# patient's, and the model is anchored there (Steven L. Shafer's standing
# rule of 2026-11-18, docs/weight-adjustment.md, *Anchoring at the source's
# cohort*).  Table 1, read on the PDF: 48.5 +/- 16.0 years, 7 men and 7
# women, 72.3 +/- 15.8 kg, BMI 25.7 +/- 3.6 kg/m2 (means), so a height of
# 167.7 cm from the BMI.  cohortSizeFactors() gives that patient volume
# factors of 1.00589 as a man and 0.82714 as a woman, and geometric means,
# weighted 7 to 7, of 0.912146 on volumes and 0.933358 on clearances.  With
# the switch on every published value is divided by these, so a patient like
# the cohort's receives Table 3's values and the 70 kg reference man V1 9.51
# L and a clearance of 26.75 L/day; with it off the published values are
# used unscaled for every patient.
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
# * Euthyroid volunteers, single dose, not used: Jonklaas 2015 (Ther Drug
#   Monit 37:110-118, PMID 24977379, read in full; 12 volunteers, 77.8 kg, one
#   50 mcg tablet fasting, Tables 2 and 4): total T3 rose from a mean predose
#   117 ng/dL to 346 at 2.5 h (a rise of 229), was back to baseline by 48 h,
#   and its baseline-corrected AUC(0-24 h) was 1860 ng.h/dL.  This model
#   peaks 428 ng/dL above baseline at 1.85 h and gives an AUC(0-24 h) of
#   3265, 1.76 times as much; from 1 to 24 h the ratio runs 1.6-2.3.  T3 had
#   already risen 35 ng/dL at 0.5 h, inside this model's 0.62 h lag.  Their
#   terminal half-life, 22 h, is this model's 21.5 h.
# * Euthyroid volunteers, placebo-subtracted, not used: Dumitrescu 2022
#   (Thyroid 32:196-205, doi 10.1089/thy.2021.0304, read in full; 50 mcg of
#   liothyronine in a capsule, nine analysed, Table 2): LT3 minus placebo 193,
#   202, 164 and 23 ng/dL at 2, 3, 4 and 24 h, against this model's 426, 366,
#   298 and 50.  Their T3 was back to baseline by 36 h: a working thyroid,
#   suppressed by the dose, gives back less of its own T3, which placebo
#   subtraction cannot remove.
# * Hypothyroid patients on once-daily liothyronine, not used: Jonklaas and
#   Burman 2016 (Thyroid 26:770-778, doi 10.1089/thy.2015.0629, read in
#   full; 18 patients, 16 with Hashimoto's thyroiditis, mean 81.8 kg; 30-60
#   mcg once a day, mean 38.3 mcg, Table 1): at week 6 total T3 rose from
#   96.1 to a peak of 292.8 +/- 152.3 ng/dL at 2.4 +/- 1.5 h, and the
#   baseline-corrected area from 0.5 to 8 h was 655.6 +/- 404.6 ng.h/dL.
#   This model at 38.3 mcg once a day rises 325 ng/dL from trough to peak
#   (the measured rise 197) and gives an area of 1488 (that paper's own
#   correction, the 0.5 h value subtracted).  Their glands still worked (TSH
#   3.8 mIU/L at week 6, free T4 0.44 ng/dL), so neither is the source's
#   patient on T3 alone.
# * So, outside its own source the model's excursions are 1.6-2 times the
#   measured ones.  The source's own patients differed 1.7-fold in volume
#   between their two thyroid states, and all three external groups had thyroid
#   function of their own; the model describes the source's patients and
#   is not adjusted to the others [decision for Shafer].
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

# Van Tassell 2019, Table 1: the cohort's typical patient (means); no height
# is printed, so the BMI gives it.
LIOTHYRONINE_COHORT <- list(weight = 72.3, bmi = 25.7, age = 48.5,
                            men = 7, women = 7)

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
  cohort <- with(LIOTHYRONINE_COHORT,
                 cohortSizeFactors(weight, age, men, women, bmi = bmi))
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume = 1, legacyClearance = 1, cohort = cohort)
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
