# -----------------------------------------------------------------------------
# Levothyroxine (Synthroid, Levoxyl, Unithroid, Tirosint; levothyroxine sodium
# for injection): one compartment, oral and intravenous, from the classical
# tracer studies of thyroxine kinetics
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min; amounts in
# mcg, so the plotted concentration is ng/mL (= mcg/L) of total serum
# thyroxine (T4).  10 ng/mL is 1 mcg/dL, the unit T4 is usually reported in.
#
# Drafted by Claude Code, 2026-11-18, at the request of Steven L. Shafer, who
# asked for levothyroxine that day and supplied the papers below.  Every
# number here was read in the full text, and the tables on the page images.
#
# WHAT IS PLOTTED
# ===============
# The thyroxine the doses add: total serum T4 ABOVE the patient's own
# baseline.  In a patient with no thyroid function (thyroidectomy, ablation,
# advanced primary hypothyroidism) the baseline is near zero and the curve is
# the serum T4.  In a patient with a working thyroid, exogenous T4 lowers TSH
# and with it the gland's own secretion; that feedback is not modelled, so
# the curve is not the change in that patient's measured T4.
#
# DISPOSITION: INADA 1975, THE EXTRAPOLATION ANALYSIS (one compartment)
# =====================================================================
# Inada M, Kasagi K, Kurata S, Kazama Y, Takayama H, Torizuka K, Fukase M,
# Soma T.  Estimation of thyroxine and triiodothyronine distribution and of
# the conversion rate of thyroxine to triiodothyronine in man.  J Clin Invest
# 1975;55:1337-1348.  doi 10.1172/JCI108053.  125I-T4 given intravenously,
# serum sampled from 1 min to 2 weeks, in five normal men aged 21-28 (52-67
# kg), four women with untreated hypothyroidism and three hypothyroid
# patients made euthyroid on 150 mcg of T4 a day.  Volumes and clearances are
# "adjusted to a 60 kg body wt", in proportion to weight (Methods, citing
# Oddie 1966).  Table II, T4, normal subjects, extrapolation method (the
# terminal exponential: K its slope, V the dose over its intercept, MCR = V x
# K, subject by subject):
#
#     V 8.4 L/60 kg   (8.7, 5.4, 10.4, 9.6, 8.1)
#     K 0.129 /day    (0.115, 0.169, 0.108, 0.130, 0.122)
#     MCR 1.06 L/day/60 kg   (1.00, 0.91, 1.13, 1.25, 0.99)
#
# The model takes the printed means of V and MCR.  Its elimination rate,
# 1.06/8.4 = 0.1262 /day (half-life 5.49 days), is 2% below the printed mean
# of K, 0.129 /day, the mean of ratios not being the ratio of means.
#
# Why one compartment.  Inada also fitted a three-pool model (serum, then
# interstitial fluid, then the cells, with loss from the cells), but for T4
# Table II prints only its total volume (8.7 L), the cellular volume V3 (2.1
# L) and the loss rate from it, lambda03 (0.504 /day; MCR 1.04 L/day).  The
# split of the rest between serum and interstitial fluid, and the two
# exchange rates, are not printed (the V2/V1 ratio of 2.59 in Results is for
# T3), so the three-pool model is short of two numbers and cannot be built
# without inventing them.  The extrapolation set is complete.  What it lacks
# is the distribution phase of the first day after a dose: the serum level
# rises above, and falls faster than, a one-compartment curve in the first
# hours (see CHECKS).  Over days and weeks, which is what levothyroxine is
# dosed over, one compartment with the right clearance and volume is right.
#
# Why Inada's normal subjects.  The three T4-treated hypothyroid patients
# (Table II: V 11.8 L, K 0.091 /day, MCR 1.08 L/day per 60 kg) are the
# population levothyroxine is given to, but they are three, and their
# clearance is the normals' (1.08 against 1.06).  The normals are the
# default; the treated group would give the same steady state with a longer
# half-life (7.6 days against 5.5) [decision for Shafer].
#
# Cross-checks, not used: Nicoloff 1972 (J Clin Invest 51:473-483, doi
# 10.1172/JCI106835, Table I, eight euthyroid adults aged 25-63, mean 73.3
# kg): T4 distribution space 11.6 L, clearance 1.30 L/day, kT4 11.2 %/day
# (half-life 6.2 days); per 60 kg, 9.50 L and 1.064 L/day, the clearance
# within 0.4% of Inada's.  Sterling 1956 (J Clin Invest 35:806-813, doi
# 10.1172/JCI103333, Table I, eight young men): half-time 6.7 days.  Hays
# 1991 (below) fitted a three-compartment model to five volunteers, but to 24
# hours of sampling, which the paper says does not separate exit from
# distribution (the exit constant's fractional SD 0.91); its mean parameters
# give a clearance of 2.2 L/day and a half-life of 2.7 days, so it is not
# used for disposition.  Only clearance is comparable between sources
# (AGENTS.md), and the two-week studies agree on it.
#
# ABSORPTION: HAYS 1991 AND HAYS AND NIELSEN 1994
# ===============================================
# Rate and lag: Hays MT.  Localization of human thyroxine absorption.
# Thyroid 1991;1:241-248, doi 10.1089/thy.1991.1.241, Table 1, the global
# absorption model (Fig. 2b) fitted to plasma 131I-T4 after oral and
# intravenous doses in five normal volunteers: a delay DT9 of 13.7 min, then
# an absorptive site emptying by absorption, k12,7 = 0.0099 /min, and by loss
# down the gut, k0,7 = 0.0095 /min (means of the five subjects).  The site's
# first-order emptying is the depot's ka = 0.0099 + 0.0095 = 0.0194 /min
# (1.164 /h), and the delay is the lag, 13.7 min.  Two readings to note: the
# delay is SAAM's delay element, taken here as a pure lag; and the paper
# absorbs into the fast-exchange (hepatic) pool, which a one-compartment
# model cannot distinguish from absorption into serum.  The mean k0,7 is
# pulled up by subject 1, whose absorption was "abnormally low" (k0,7 0.0323
# /min); without subject 1, ka would be 0.0131 /min and the lag 15.3 min
# [decision for Shafer].
#
# Extent: Hays MT, Nielsen KR.  Human thyroxine absorption: age effects and
# methodological analyses.  Thyroid 1994;4:55-64, doi 10.1089/thy.1994.4.55,
# Table 1, the double isotope method in control subjects: 69.3% at 21-69
# years (n 45), 62.8% at 70-79 (n 8, P < 0.001).  The paper prints these
# groups, not a model of age, so bioavailability steps from 0.693 to 0.628 at
# 70; below 21 it is the 21-69 value, an extrapolation.  These are oral
# tracer SOLUTIONS in albumin, not tablets.  The Synthroid label gives the
# tablet 93% of an oral solution (which would make it 0.645), and NDA
# 202231's clinical pharmacology review quotes Maxon 1983, Synthroid tablets
# 66% of intravenous.  At steady state on tablets, absorption was lower:
# Surks 1973 (J Clin Invest 52:805-811, Table I, seven treated patients)
# 48.0 +/- 2.6%, and Inada's three treated patients imply 53% (CHECKS).  The
# bioavailability is read, not fitted (AGENTS.md); which reading applies to
# the patient's product is a [decision for Shafer].
#
# DOSE
# ====
# Products are labelled in mcg of levothyroxine sodium (C15H10I4NNaO4,
# 798.86 g/mol anhydrous, Synthroid label sec. 11), and the serum analyte is
# thyroxine (C15H11I4NO4, 776.87 g/mol).  A dose is entered as labelled, and
# delivers 776.87/798.86 = 0.97247 of its mass as T4: the volume and the
# clearance are divided by 0.97247 (as R/drugs_azilsartan.R does), which is
# exact for a linear model.  Intravenous doses are offered: the disposition
# is that of an intravenous tracer, and the injection (NDA 202231) is labelled
# in mcg of levothyroxine sodium too.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# Inada's volumes and clearances are per 60 kg, in proportion to weight, so
# at the 70-kg reference V = 8.4 x 70/60 = 9.80 L and CL = 1.06 x 70/60 =
# 1.2367 L/day, and pkSizeFactors() scales them: by fat-free mass with the
# switch on, in proportion to total weight with it off (its default legacy
# factors), which reproduces the source.
#
# CHECKS (tests/testthat/test-drugs-levothyroxine.R)
# ==================================================
# XP-8121's phase 1 (Fitch 2025, Clin Transl Sci, doi 10.1111/cts.70244,
# Table 2; not used in the model): 600 mcg of oral Synthroid in healthy
# volunteers (part 1 mean 77.7 kg), baseline-adjusted total T4.  This model
# at 77.7 kg: Cmax 36.3 ng/mL at 4.9 h against 47.6 at a median 3.1 h (-24%;
# the missing distribution phase), terminal half-life 132 h against 144
# (-8%), AUC0-648h 6834 ng.h/mL against 4005 (+71%).  The AUC is not a fair
# test: in euthyroid volunteers the dose suppresses TSH and their own T4
# secretion, so subtracting the pre-dose baseline also removes some of the
# dose's own T4, and negative values were set to zero (S1).  Inada's own
# treated patients (150 mcg/day, 46.3 kg, aged 52-60): this model's average
# steady-state T4 is 12.3 mcg/dL against the 9.5 measured (+30%), which is
# a bioavailability of 0.53 rather than 0.693.
#
# PREGNANCY AND THE BAND
# ======================
# Requirements rise about 47% in the first half of pregnancy (Alexander
# 2004), a dose requirement and not a measured pharmacokinetic change; the
# registry row is unquantified and nothing is applied.  No band: the total T4
# reference interval is a laboratory normal range, not a therapeutic drug
# range, and levothyroxine is titrated to TSH (therapeutic-range row).
# -----------------------------------------------------------------------------

# Inada 1975, Table II, T4, normal subjects, extrapolation method (per 60 kg)
LEVOTHYROXINE_V_60KG  <- 8.4     # L
LEVOTHYROXINE_CL_60KG <- 1.06    # L/day
LEVOTHYROXINE_REFERENCE_WEIGHT <- 60  # kg, Inada's adjustment
# Hays 1991, Table 1, global absorption model, means of five subjects
LEVOTHYROXINE_K_ABSORBED <- 0.0099   # /min, k12,7
LEVOTHYROXINE_K_LOST     <- 0.0095   # /min, k0,7
LEVOTHYROXINE_LAG        <- 13.7     # min, DT9
# Hays and Nielsen 1994, Table 1
LEVOTHYROXINE_F_UNDER_70 <- 0.693
LEVOTHYROXINE_F_70_PLUS  <- 0.628
LEVOTHYROXINE_F_AGE_STEP <- 70       # years
# Levothyroxine sodium (anhydrous) to thyroxine
LEVOTHYROXINE_T4_PER_SALT <- 776.87 / 798.86

#' Levothyroxine (Inada 1975; Hays 1991; Hays and Nielsen 1994)
#'
#' One compartment from Inada's intravenous tracer study, oral absorption
#' from Hays's tracer studies; total serum T4 above the patient's baseline.
#' See the header of \code{R/drugs_levothyroxine.R}.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
levothyroxine <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)
  toReference <- FFM_REFERENCE_WEIGHT / LEVOTHYROXINE_REFERENCE_WEIGHT
  v  <- LEVOTHYROXINE_V_60KG * toReference * size$volume / LEVOTHYROXINE_T4_PER_SALT
  cl <- LEVOTHYROXINE_CL_60KG * toReference * size$clearance / LEVOTHYROXINE_T4_PER_SALT /
    MINS_PER_DAY
  bioavailability <- if (isTRUE(age >= LEVOTHYROXINE_F_AGE_STEP)) LEVOTHYROXINE_F_70_PLUS else
    LEVOTHYROXINE_F_UNDER_70

  default <- list(
    v1  = v,
    v2  = 1,
    v3  = 1,
    cl1 = cl,
    cl2 = 0,
    cl3 = 0,
    ka_PO = LEVOTHYROXINE_K_ABSORBED + LEVOTHYROXINE_K_LOST,
    bioavailability_PO = bioavailability,
    tlag_PO = LEVOTHYROXINE_LAG
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
      "Inada M, Kasagi K, Kurata S, et al., J Clin Invest 1975;55:1337-1348, Table II ",
      "(T4, normal subjects, extrapolation method): V 8.4 L and clearance 1.06 L/day per ",
      "60 kg, one compartment; absorption from Hays MT, Thyroid 1991;1:241-248, Table 1 ",
      "(ka 0.0194 /min, lag 13.7 min), and Hays MT, Nielsen KR, Thyroid 1994;4:55-64, ",
      "Table 1 (F 0.693 below 70 years, 0.628 from 70); doses in mcg of levothyroxine ",
      "sodium, plotted as total serum T4 above the patient's own baseline. ",
      "https://doi.org/10.1172/JCI108053")
  )
}
