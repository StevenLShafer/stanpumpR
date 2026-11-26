# -----------------------------------------------------------------------------
# Diphenhydramine, intravenous (Meredith 1984): plasma diphenhydramine in
# healthy men
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min.
#
# Drafted by Claude Code, 2026-11-25, at the request of Steven L. Shafer, who
# chose the source, the time to peak effect and the band ("Build with
# Meredith, 2 min peak effect, Gengo ranges").  Every paper below was read in
# full on its PDF unless it says otherwise.
#
# THE SOURCE
# ==========
# Meredith CG, Christian CD, Johnson RF, Madhavan SV, Schenker S.
# Diphenhydramine disposition in chronic liver disease.  Clin Pharmacol Ther
# 1984;35:474-479 (PMID 6705445, doi 10.1038/clpt.1984.63).  Nine men with
# alcoholic cirrhosis and eight healthy, age-matched men (mean age 43.4 +- 3.1
# years, 30-53) were given 0.8 mg/kg diphenhydramine (Benadryl, Parke-Davis:
# the hydrochloride) intravenously; venous plasma at 0.25, 0.5, 1, 2, 3, 4, 5,
# 6, 8, 12, 24, 32 and 48 h, by gas chromatography (Bilzer and Gundert-Remy's
# method).  Two-exponential curves were fitted (SAAM-23) to six of the normal
# subjects; the other two were too erratic between 2 and 5 h, which the
# authors put down to changes in posture.  The normal subjects' means (Table
# II and the text, mean +- SE):
#
#     beta              0.079 /h   (t1/2 beta 9.3 h, n = 8)
#     alpha             0.7292 /h  (text, n = 6)
#     CL                9.78 mL/min/kg   (biexponential, n = 6; 9.8 from the
#                                          beta phase, n = 8)
#     V1                3.3 L/kg   (n = 6, Dose / (A + B))
#     Vss               6.52 L/kg  (n = 6)
#     k10               0.197 /h   (n = 6)
#     Cp0 = A + B       256.7 ng/mL
#
# These are means of individual fits, so they are not one model's numbers.
# The model takes V1, CL and the two exponents (twoCompartmentFromCentralVolume(),
# R/vasoactive.R): clearance first, because it is the one parameter that
# measures a process (AGENTS.md, *Comparing sources*), then the two exponents
# and V1 that define the curve's shape and scale.  It then gives Vss 6.42 L/kg
# (printed 6.52, 1.5% lower), k10 0.178 /h (printed 0.197, the mean of the
# ratios), and Cp0 242 ng/mL after 0.8 mg/kg (printed 256.7).  Per kilogram:
#
#     V1 3.30 L/kg, V2 3.12 L/kg, CL 9.78 mL/min/kg, Q 16.9 mL/min/kg
#     (at 70 kg: V1 231 L, V2 218 L, CL 0.685 L/min, Q 1.18 L/min)
#
# The rate constants are used rather than the mean half-lives (1.28 h and
# 9.3 h printed), because the paper's V1, k10, k12 and k21 are built from
# them; a mean of half-lives and a mean of rate constants differ when the
# subjects differ.
#
# THE FIRST MINUTES
# =================
# The first sample was at 15 minutes, venous.  The central volume, 231 L at
# 70 kg, is a back-extrapolation from there, and any faster distribution
# phase is unseen.  The model therefore probably UNDERESTIMATES the plasma
# concentration in the first minutes after a bolus, which is when intravenous
# diphenhydramine sedates (Meredith: "All subjects experienced sedation 1 to
# 2 min after injection").  Blyden 1986 sampled from 5 minutes but printed no
# exponents.  Albert 1975 (below), supplied by Shafer on 2026-11-25, bears
# this out: at the end of a 1-hour infusion the model is 38% and 28% below
# his two men, though their clearance agrees with Meredith's.  It cannot
# replace the model: two subjects whose fits disagree (one has a 12.5-min
# distribution half-life, the other none faster than 1.6 h), no sample in the
# 30 minutes after the infusion, and, in the authors' words, "insufficient
# information in these data to make a judicious choice" of model.
#
# DOSE BASIS
# ==========
# The approved product is the hydrochloride (MW 291.82; the base 255.36), and
# Meredith gave Benadryl injection, also the hydrochloride.  The paper does not
# say whether its V1 and clearance are per mg of the salt or the base, and its
# V1 settles it: 0.8 mg/kg / 256.7 ng/mL is 3.12 L/kg on the hydrochloride and
# 2.73 on the base, against the printed 3.3.  So the parameters are read on
# the hydrochloride, and doses are entered as labelled with no conversion
# (AGENTS.md, *Dose basis*).
#
# EXTERNAL CHECKS (not fitted)
# ============================
# Carruthers SG, Shoeman DW, Hignite CE, Azarnoff DL.  Clin Pharmacol Ther
# 1978;23:375-382 (PMID 24512).  Six healthy volunteers (four women), 22-33
# years, 57-90 kg (mean 70), 50 mg diphenhydramine hydrochloride IV over one
# minute; hourly antecubital samples from 1 to 10 h.  Measured 144 +- 17 ng/mL
# (SE) at 1 h and 37 +- 3 at 8 h; the model gives 141 and 44 for a 70-kg man
# (test-drugs-diphenhydramine.R): right at 1 h, 19% high at 8 h.  Carruthers'
# subjects cleared the drug faster than Meredith's (783 mL/min, computed on
# the base, so 895 on the hydrochloride, against 696), and were younger and
# mostly women; the difference is recorded, not fitted away.  Carruthers' own
# half-life, 3.4 h, is from 10 hours of sampling and is not a check on the
# terminal phase.
#
# Blyden GT, Greenblatt DJ, Scavone JM, Shader RI.  J Clin Pharmacol
# 1986;26:529-533 (PMID 3760245).  Ten healthy volunteers (six men), 26-41
# years, 50 mg hydrochloride IV over 5 minutes, samples from 5 minutes to
# 24-48 h, with an assay that separates the N-demethyl metabolite: clearance
# 6.2 mL/min/kg, Vd(area) 4.5 L/kg, t1/2 8.4 h.  Its clearance is a third
# below Meredith's 9.8, and below Carruthers' 11.2 and Berlinger's 12.1
# (Blyden's own Table II).  An assay that counted the metabolite as drug would
# read concentrations high and clearance LOW, so the assays cannot explain
# Blyden's lower value; the difference is recorded, not reconciled.
#
# Albert KS, Hallmark MR, Sakmar E, Weidler DJ, Wagner JG.  Pharmacokinetics
# of diphenhydramine in man.  J Pharmacokinet Biopharm 1975;3:159-170
# (PMID 1159620, doi 10.1007/BF01067905; read on the page images).  Two healthy
# men, 27 years, 76 and 95 kg, 50 mg hydrochloride (assayed 43.7 mg base)
# infused over 1 hour, plasma by a specific GC assay to 24 h (Table II):
#
#     h          0.25  0.5   1     1.5   2     4     6     12    24
#     subject 1  120   222   258   150   132   83.8  61.6  19.4  6.49
#     subject 2  31.0  89.1  179   170   147   80.3  41.5  18.2  8.19
#
# Its clearances, 37.0 and 41.1 L/h on the base (footnote 5), are 42.3 and
# 47.0 L/h on the hydrochloride: 9.3 and 8.2 mL/min/kg, against Meredith's
# 9.78.  The model (switch off, 178 cm) gives 160 and 128 ng/mL at the end of
# the infusion, 62% and 72% of what was measured, and runs high from 12 h
# (1.6-1.8x and 1.15-1.3x), where Albert's assay, 5 ng/mL at best, gave him
# terminal half-lives of 4 to 7 h that he himself called inadequate.  Albert
# also saw sleep above about 70 ng/mL, and Table VI gives 1.3-1.8% unbound
# and a blood:plasma ratio of 0.82.
#
# THE EFFECT SITE
# ===============
# No source fits an effect compartment for intravenous diphenhydramine.  The
# time to peak effect, 2 minutes, is Shafer's clinical decision of 2026-11-25,
# from Meredith's observation that sedation began 1 to 2 minutes after the
# injection; getDrugPK() solves ke0 from it (about 3 /min on this model).
#
# THE BAND
# ========
# Gengo F, Gabos C, Miller JK.  Clin Pharmacol Ther 1989;45:15-21
# (PMID 2910633).  Fifteen healthy men, 50 mg orally: the concentrations at
# which drowsiness wore off were 30.4 to 41.5 ng/mL among the subjects (41.5
# for the mean regression line), and those at which mental impairment (digit
# symbol substitution) wore off 58.2 to 74.4 ng/mL (68 for the mean).  The
# band spans both, lower 30.4, upper 74.4, typical 41.5 (the mean offset of
# drowsiness), and endCe, the threshold of the time until threshold, is 41.5:
# the time until drowsiness is expected to wear off.  Shafer's decision of
# 2026-11-25.  The AGNP update 2026 (Hart 2026, Table 5, read on the PDF)
# lists 10-60 ng/mL 10 h after a dose for SLEEP DISORDERS (oral, nightly),
# from the same Gengo paper, alert level 200 ng/mL; that is another use, so
# the therapeutic-range row is band_differs and records it.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# The parameters are per kilogram (the normal subjects' weights are not
# printed; their mean dose, 57.1 mg at 0.8 mg/kg, implies about 71 kg).  A
# model published per kilogram is not anchored at its cohort: the 70-kg
# values are scaled to fat-free mass with the switch on, and linearly in
# weight with it off (the default legacy factors).
#
# PREGNANCY (R/pregnancy.R)
# =========================
# No study of diphenhydramine pharmacokinetics in human pregnancy was found
# (PubMed, 2026-11-25); the pregnancy studies are in sheep.  Recorded as
# unknown, nothing applied.
#
# WHAT THIS MODEL IS NOT
# ======================
# Not a model of cirrhosis (Meredith's other group: t1/2 beta 15.2 h,
# clearance 7.8 mL/min/kg, not applied), of the elderly, of children or of
# the intramuscular or oral routes, and it predicts no sedation, wheal or
# anticholinergic effect.  The label contraindicates it in neonates and
# premature infants.
# -----------------------------------------------------------------------------

DIPHENHYDRAMINE_V1 <- 3.3          # L/kg
DIPHENHYDRAMINE_CL <- 9.78         # mL/min/kg
DIPHENHYDRAMINE_ALPHA <- 0.7292    # /h
DIPHENHYDRAMINE_BETA <- 0.079      # /h
DIPHENHYDRAMINE_TPEAK <- 2         # min, Shafer's clinical decision

#' Diphenhydramine, intravenous (Meredith 1984)
#'
#' Two compartments per kg from the healthy men's mean V1, clearance and
#' exponents, with a clinical time to peak effect of 2 min; see the header of
#' \code{R/drugs_diphenhydramine.R}.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
diphenhydramine <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)

  # per kg, minutes
  p <- twoCompartmentFromCentralVolume(
    DIPHENHYDRAMINE_CL / 1000, DIPHENHYDRAMINE_V1,
    60 * log(2) / DIPHENHYDRAMINE_ALPHA, 60 * log(2) / DIPHENHYDRAMINE_BETA)
  p <- lapply(p, `*`, FFM_REFERENCE_WEIGHT)

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
    "Meredith CG, Christian CD, Johnson RF, Madhavan SV, Schenker S, Clin ",
    "Pharmacol Ther 1984;35:474-479. Two compartments from the healthy men's ",
    "means (Table II): V1 3.3 L/kg, CL 9.78 mL/min/kg, alpha 0.729 /h, beta ",
    "0.079 /h; doses as the hydrochloride. Time to peak effect 2 min (clinical). ",
    "Band from Gengo F et al., Clin Pharmacol Ther 1989;45:15-21. Checked ",
    "against Carruthers 1978 and Blyden 1986. https://doi.org/10.1038/clpt.1984.63"
  )

  list(
    PK = PK,
    tPeak = DIPHENHYDRAMINE_TPEAK,
    MEAC = 0,
    typical = 41.5,
    upperTypical = 74.4,
    lowerTypical = 30.4,
    reference = reference
  )
}
