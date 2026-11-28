# -----------------------------------------------------------------------------
# Methylergonovine (methylergometrine), intravenous and intramuscular: the
# two-compartment intravenous disposition of de Groot 1995, nonpregnant women
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min.  Doses are mg
# or mcg of methylergonovine MALEATE as labelled; concentrations are ng/mL of
# methylergonovine as measured.
#
# Drafted by Claude Code, 2026-11-26, at the request of Steven L. Shafer.  It
# replaces an approximation derived from the Methergine label (2026-11-25),
# written before the sources below could be read.
#
# THE SOURCES (both read in full, 2026-11-26, from copies Steven L. Shafer
# supplied)
# ==========================================================================
# A. de Groot ANJA, Vree TB, Hekster YA, van den Biggelaar-Martea M, van
#    Dongen PWJ, van Roosmalen J.  Comparison of the bioavailability and
#    pharmacokinetics of oral methylergometrine in men and women.  Int J Clin
#    Pharmacol Ther 1995;33:328-332 (PMID 7582383).  THE MODEL'S SOURCE.
# B. de Groot ANJA et al.  Variation in bioavailability of oral
#    methylergometrine in healthy male volunteers.  Drug Invest
#    1994;8:345-351 (doi 10.1007/BF03257449).  The same study's men, with
#    individual values; used here as a check only.
#
# Design: 6 men and 6 nonpregnant women, each given 0.2 mg methylergometrine
# maleate (0.152 mg base) intravenously over 1 minute and 0.125 mg (0.095 mg
# base) orally after a standard breakfast, in random order.  VENOUS plasma (an
# intravenous cannula; arterial is preferred, docs/adding-a-drug.md, and no
# arterial study was found), 0 to 9 h; HPLC with fluorescence detection, limit
# 150 pg/mL.  Two compartments were fitted to the intravenous data and one to
# the oral.  Women: 41.2 (SD 5) years, 61 (7) kg, 167 (7) cm.  Men: 35.7 (13)
# years, 75 (4) kg, 181 (8) cm.
#
# THE VALUES READ (A, Tables 1 and 2, the WOMEN, mean and SD)
# ===========================================================
# The women are the model's patient because the drug is given to women.
#   Intravenous: CL 22.2 (3.10) L/h; Vss 50.8 (8.23) L; half-lives 0.10 (0.04)
#     and 1.94 (0.34) h; mean residence time 2.31 (0.31) h; AUC 6.02 (0.63)
#     mcg.h/L.
#   Oral (not modelled, see THE ORAL ROUTE): F 96.4 (27.5) %, range 22 to
#     138 %; lag 0.09 (0.07) h; mean absorption time 0.57 (0.35) h; half-life
#     1.42 (0.31) h; Cmax 1.21 (0.40) ng/mL at 0.97 (0.74) h; AUC 3.49 (0.70)
#     mcg.h/L.
#
# THE DERIVATION
# ==============
# Disposition.  CL, Vss and the two half-lives determine a two-compartment
# model.  With alpha and beta the rate constants of the two half-lives,
#     V1 = CL / (alpha + beta - alpha x beta x Vss / CL),
#     k21 = alpha x beta x V1 / CL,  k12 = alpha + beta - CL / V1 - k21,
# giving, on the paper's base dose, V1 13.69 L, V2 37.11 L, Q 56.67 L/h.  It is
# computed below from the printed values, which are means over subjects
# combined into one model.
#
# Dose basis.  The product is the maleate and the paper states its analysis on
# the base (0.152 mg base per 0.2 mg maleate), so every volume and clearance is
# divided by 0.76 and a dose is entered as labelled (docs/adding-a-drug.md,
# When the dose basis is not the plotted analyte).  CL is then 29.21 L/h per
# labelled dose.
#
# Intramuscular.  Neither paper gave an intramuscular dose.  F 0.78 is READ
# from the Methergine US prescribing information (Drugs@FDA 006035s080, posted
# 2025,
# https://www.accessdata.fda.gov/drugsatfda_docs/label/2025/006035s080lbl.pdf):
# "During delivery, with intramuscular injection, bioavailability increased to
# 78%", with no method, comparator or number of subjects.  ka 1.7256 /h gives
# the label's time of peak after 0.2 mg intramuscularly, 0.41 h, on the
# disposition above.  THIS IS THE WEAKEST ROUTE.  Two things follow:
#   - the predicted peak is 1.75 ng/mL, where the label reports 5.9 (and an
#     AUC of 5.3 mcg.h/L against the label's 200 / 14.4 = 13.9);
#   - the label puts intramuscular absorption about 25% above the tablet's,
#     yet its 0.78 is below the oral F de Groot measured (0.964).  The two
#     figures come from different studies; bioavailability is read, not
#     adjusted to agree.  Steven L. Shafer kept 0.78, rather than assume
#     complete absorption, on 2026-11-26.
#
# THE ORAL ROUTE
# ==============
# Not offered.  Steven L. Shafer removed it on 2026-11-26, after it had been
# drafted from the oral values above, when told that de Groot's own later
# review (de Groot ANJA.  Eur J Obstet Gynecol Reprod Biol 1996;69:31-36, PMID
# 8909954, abstract) concludes that oral (methyl)ergometrine tablets are
# unstable in storage, that the drug's pharmacokinetic and dynamic properties
# are unpredictable, that no effect on blood loss after childbirth has been
# shown, and that it is not an alternative to parenteral prophylactic oxytocics
# in the active management of the third stage of labour.  The tablet remains a
# labelled product.  Do not restore the route without asking.
#
# The label copies filed in papers/labels/ are two other documents from
# DailyMed: the tablet label (Lupin, ANDA 091577, revised 3/2024, setid
# 3f04ca06-fe52-46d6-96cd-2e77ef6fa36f) and the injection label (American
# Regent, ANDA 090193, revised August 2025, setid
# a9a3079e-b62b-4858-920e-d8f5cb38481e).  Their pharmacokinetic numbers were
# compared with 006035s080 on 2026-11-25 and are the same.
#
# THE LABEL'S CONCENTRATIONS ARE HIGHER THAN THESE
# ================================================
# The label's intramuscular CL/F is 14.4 L/h; de Groot's women give
# 29.21 / 0.78 = 37 L/h, and after injection the label is about three times
# higher (peak 3.4-fold, area 2.6-fold).  The label names no assay.  Mantyla
# and Kanto's review of radioimmunoassay studies (Int J Clin Pharmacol Ther
# Toxicol 1981;19:386-391, PMID 7298246, abstract) reports a clearance of 7.2
# to 14.4 L/h, like the label's.  A radioimmunoassay that also read metabolites
# would raise concentrations and lower clearance; that this explains the
# difference is a hypothesis, not a finding.  de Groot's specific assay and
# intravenous data are followed here.
#
# HOW WELL V1 IS KNOWN
# ====================
# Poorly.  The model closes the women's MEANS, a two-stage average
# (docs/adding-a-drug.md, A two-stage source).  A prints no individual values
# for them, so the refit that is possible for the men (each man's model from his
# own four values, then a fit to the six curves) is not possible here, and A
# plots no mean curve.
#   - A closure of means can be badly biased, and V1 is where it falls.  The
#     men's gives V1 32.6 L, against 16.9 L from a fit to the median of their
#     six individual closures, anchored at time zero (B's card,
#     papers/digests/DOI_10.1007_bf03257449.md): one man's long distribution
#     half-life raised the mean.
#   - V1 is sensitive to the values closed.  The distribution half-life 1 SD
#     either way (0.06 to 0.14 h), the others fixed, moves it from 9.0 to
#     17.6 L.  That range shows spread in one value and nothing about bias.
#   - A, Figure 1, right panel, plots one woman (no. 3).  Her intravenous
#     points were read on 2026-11-27 as a check, NOT FITTED INTO THE MODEL; the
#     same reading of the man in the left panel agrees with his own published
#     values (B, Table III) to a median of 4%.  For 152 mcg over 1 minute the
#     model is 1.8 to 2.2 times her first sample (3 or 5 minutes), 1.4 times at
#     10 minutes, within 15% at 20 and 30 minutes, and 0.65 to 0.75 of her
#     levels from 1 hour on.  A two-compartment fit to her points gives V1 30
#     to 33 L, CL 18.5 L/h, Vss 53 L and half-lives of 0.11 to 0.13 and 2.1 h
#     (the figure prints 2.1).
# So the model's concentrations in the first minutes are uncertain, and against
# the one woman plotted they are about twice too high.  NO DIRECTION IS CLAIMED
# for V1.  One subject cannot show where the group's value lies (the men's
# individual values run from 10.5 to 39.3 L), and V1 depends on the design, on
# when sampling began (3 minutes here, for her as for the others).  Clearance is
# the better-determined parameter, though the women's printed values disagree
# about it too (below).  Her area, about 8 mcg.h/L, is also more than six values
# with mean 6.02 and SD 0.63 can include (about 7.3 at most), so Figure 1 and
# Table 1 disagree somewhere.
#
# WHAT DISAGREES WITHIN THE SOURCES
# =================================
# - The women's dose / AUC is 0.152 / 6.02 = 25.2 L/h against the printed CL of
#   22.2, which agrees with the printed Vss / MRT (50.8 / 2.31 = 22.0).  The
#   printed CL is used, so the model's intravenous AUC is 6.85 mcg.h/L, 14%
#   above the measured 6.02.
# - The injection solution held 93.3 (6.5) % of its stated content, which may
#   be part of that gap.  CL x AUC, here the product of the women's means, is
#   133.6 mcg, 87.9% of the nominal 152 (in five of the six men it is 90.8 to
#   96.8%).  For 93.3% of 152 mcg the model's AUC is 6.39, 6% above the
#   measured 6.02.  No correction is made: a dose is entered as labelled.
# - The men's CL is 32.2 (11.8) L/h in the tables of both papers, but 33.2 in
#   the abstract of A and 34.1 (9.7) in the summary of B.  Their Vss / MRT,
#   71.5 / 2.25 = 31.8, supports 32.2.  The text of A swaps the sexes' Vss per
#   kg against its Table 1.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# No covariate was fitted, so the values belong to the women who were studied:
# the model is anchored at their typical patient (61 kg, 167 cm, 41.2 years,
# female; A, Results), whose factors are 0.746 on volumes and 0.802 on
# clearances.  With the switch on she receives the published values and the
# reference man 1.341 and 1.246 times them; with it off everyone receives the
# published values.  As a check, not a fit: scaled to the typical man of the
# same study, the women's model predicts CL 29.7 L/h and Vss 74.8 L, against
# the men's measured 32.2 (11.8) and 71.5 (25.9).  Its V1 there is 20.2 L
# (16.9 L in the median fit above), and for 152 mcg over 1 minute its curve is
# 12% below to 26% above the GEOMETRIC MEAN of the six men's individual curves
# from 3 minutes to 6 hours.
#
# PREGNANCY (R/pregnancy.R)
# =========================
# Not applied; the registry records the findings as unquantified.  The drug is
# contraindicated in pregnancy and is given at delivery and after it.  The
# subjects here were not pregnant.  The label reports the 78% intramuscular
# bioavailability during delivery (used for every intramuscular dose, there
# being no other).  No comparison of clearance or volume with nonpregnant women
# was found (PubMed, 2026-11-25).
#
# EFFECT SITE AND BAND
# ====================
# No time of peak effect has been measured (the label gives onset only:
# immediate intravenously, 2 to 5 minutes intramuscularly), and Mantyla and
# Kanto 1981 found no apparent correlation between plasma level and clinical
# effect.  tPeak is 0, only the plasma is drawn, and there is no therapeutic
# concentration range, so no band.
# -----------------------------------------------------------------------------

# de Groot 1995, Table 1: the six women, intravenous (on the base dose)
METHYLERGONOVINE_CL <- 22.2           # L/h
METHYLERGONOVINE_VSS <- 50.8          # L
METHYLERGONOVINE_T12_ALPHA <- 0.10    # h
METHYLERGONOVINE_T12_BETA <- 1.94     # h
# 0.152 mg base per 0.2 mg of the labelled maleate
METHYLERGONOVINE_BASE_FRACTION <- 0.152 / 0.200
# Intramuscular: F from the Methergine label, ka to its time of peak, 0.41 h
METHYLERGONOVINE_F_IM <- 0.78
METHYLERGONOVINE_KA_IM <- 1.7256      # 1/h
# The typical woman of the source, at whom the model is anchored
METHYLERGONOVINE_COHORT <- list(weight = 61, height = 167, age = 41.2,
                                men = 0, women = 6)

#' Methylergonovine pharmacokinetics (intravenous and intramuscular)
#'
#' The two-compartment intravenous disposition of de Groot and colleagues
#' (1995) in nonpregnant women, with intramuscular bioavailability and time of
#' peak from the Methergine prescribing information.  No oral route is offered.
#' Doses are the labelled maleate.  Plasma only; see the header of
#' \code{R/drugs_methylergonovine.R}.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @keywords internal
methylergonovine <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1,
                        cohort = do.call(cohortSizeFactors,
                                         METHYLERGONOVINE_COHORT))

  # Two compartments from CL, Vss and the two half-lives (see above)
  alpha <- log(2) / METHYLERGONOVINE_T12_ALPHA          # 1/h
  beta  <- log(2) / METHYLERGONOVINE_T12_BETA
  V1  <- METHYLERGONOVINE_CL /
    (alpha + beta - alpha * beta * METHYLERGONOVINE_VSS / METHYLERGONOVINE_CL)
  k10 <- METHYLERGONOVINE_CL / V1
  k21 <- alpha * beta / k10
  k12 <- alpha + beta - k10 - k21
  V2  <- V1 * k12 / k21
  Q   <- k12 * V1

  # per labelled (maleate) dose
  salt <- 1 / METHYLERGONOVINE_BASE_FRACTION

  default <- list(
    v1  = V1 * salt * size$volume,
    v2  = V2 * salt * size$volume,
    v3  = 1,                          # placeholder: a two-compartment model
    cl1 = METHYLERGONOVINE_CL / 60 * salt * size$clearance,   # L/min
    cl2 = Q / 60 * salt * size$clearance,
    cl3 = 0,
    ka_IM = METHYLERGONOVINE_KA_IM / 60,                      # 1/min
    bioavailability_IM = METHYLERGONOVINE_F_IM,
    tlag_IM = 0
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "de Groot ANJA, Vree TB, Hekster YA, van den Biggelaar-Martea M, van ",
    "Dongen PWJ, van Roosmalen J. Comparison of the bioavailability and ",
    "pharmacokinetics of oral methylergometrine in men and women. Int J Clin ",
    "Pharmacol Ther 1995;33:328-332 (nonpregnant women, intravenous). ",
    "Intramuscular bioavailability and time of peak from the Methergine US ",
    "prescribing information, Drugs@FDA 006035s080 (2025), whose ",
    "intramuscular concentrations are about three times these. ",
    "https://pubmed.ncbi.nlm.nih.gov/7582383/"
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
