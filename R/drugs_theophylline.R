# -----------------------------------------------------------------------------
# Theophylline (and aminophylline): one compartment, from the FDA label
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min; amounts in
# mg, so the plotted concentration is mg/L (mcg/mL).
#
# Drafted by Claude Code, 2026-11-12, at the request of Steven L. Shafer, who
# asked for the common drugs with narrow therapeutic windows that the library
# lacked.  Every number below was read from the documents named, in full.
#
# WHY THE LABEL
# =============
# No population model of theophylline with intravenous data and a complete
# description could be read in full in this session: the adult population
# models (Kurata 2020, Ma 2018, Tanigawara 1995, and the 16 the Ma 2016 review
# collects) are oral, mostly trough-only, one-compartment fits of CL/F, and
# the classic two-compartment intravenous studies (Mitenko & Ogilvie 1973)
# are not in an open archive.  Aslaksen 1981 (Br J Clin Pharmacol 11:269,
# PMC1401615, read on the scanned pages) fitted two compartments to
# intravenous theophylline and aminophylline but printed only the terminal
# parameters.  So the disposition is the label's, as methylene blue's is
# (R/drugs_methyleneBlue.R): Theophylline in 5% Dextrose Injection, B. Braun,
# NDA 019826, revised 2012 (www.accessdata.fda.gov/drugsatfda_docs/label/2012/
# 019826s023lbl.pdf; the Hospira label, NDA 019211, carries the same text).
#
# One compartment.  Every population model the Ma 2016 review collects is
# one-compartment, and the label calls distribution complete 30 minutes after
# a loading dose ("a serum concentration obtained 30 minutes after an
# intravenous loading dose, when distribution is complete").  So the first
# half hour after a bolus, when a two-compartment drug is above this curve,
# is not represented; the loading doses the label recommends are infused over
# 30 minutes for that reason.
#
#   V  = 0.45 L/kg: "The apparent volume of distribution of theophylline is
#        approximately 0.45 L/kg (range 0.3-0.7 L/kg) based on ideal body
#        weight."
#
#   CL = Table V's initial infusion rate divided by its target, 10 mg/L
#        ("To achieve a target concentration of 10 mcg/mL"): the rate that
#        holds 10 mg/L at steady state IS the clearance times 10 mg/L.  Table
#        V gives one rate for every age, from the first day of life, where the
#        measured means of Table I leave infants without a clearance:
#
#          postnatal age up to 24 days    1 mg/kg every 12 h  -> 0.0083 L/kg/h
#          postnatal age beyond 24 days   1.5 mg/kg every 12 h -> 0.0125
#          (to 6 weeks)
#          6-52 weeks       (0.008 x age in weeks + 0.21) mg/kg/h, over 10
#          1-9 years        0.8 mg/kg/h  -> 0.08 L/kg/h
#          9-12 years       0.7          -> 0.07
#          12-16 years      0.5 (nonsmokers) -> 0.05
#          16-60 years      0.4 ("otherwise healthy nonsmokers") -> 0.04
#          over 60 years    0.3          -> 0.03
#
#        The groups meet as the table writes them (9, 12 and 16 years go to
#        the older group, 60 to the younger), so clearance steps at each
#        boundary, as the label's dosing does.
#
#   Checked against Table I, the label's measured means (mL/kg/min), which
#   the rates above were not computed from: adults 16-60 "otherwise healthy
#   non-smoking asthmatics" 0.65 (this model 0.667), elderly over 60 0.41
#   (0.50), children 1-4 years 1.7 and 6-17 years 1.4 (1.33), and the adult
#   half-life 8.7 h (this model 7.8 h).  The label's own infusion check holds
#   exactly: "a constant intravenous theophylline infusion of 0.4 mg/kg/hr
#   ... will result in a steady-state concentration of 10 mcg/mL".
#
# NOT MODELLED, AND HELD AT THE REFERENCE
# =======================================
#   Smoking: the reference is a NONSMOKER.  "Theophylline clearance has been
#     shown to increase by approximately 50% in young adult tobacco smokers
#     and by approximately 80% in elderly tobacco smokers" (label); the app
#     has no smoking field.
#   Heart failure, cor pulmonale, liver disease, sepsis with multi-organ
#     failure, shock: Table V's 0.2 mg/kg/h, half the adult clearance.
#   Saturable elimination: "non-linearity of elimination may begin in some
#     patients at serum theophylline concentrations less than 10 mcg/mL"; the
#     Table I values were "generally determined at serum theophylline
#     concentrations less than 20 mcg/mL".  No Michaelis-Menten source could
#     be read in full, so the model is linear, and above 20 mg/L it
#     underestimates the concentration and the time it takes to fall.
#   Interactions: Table II of the label (cimetidine, ciprofloxacin,
#     erythromycin, fluvoxamine, rifampin, phenytoin, ...).
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# The label scales both parameters per kilogram of IDEAL body weight
# ("Theophylline distributes poorly into body fat"; "Use ideal body weight for
# obese patients").  So the model carries its own weight covariate and is
# evaluated at the pharmacokinetic weight with the fat-free-mass switch on
# (size$pkWeight, which tracks lean rather than total weight) and at total
# body weight with it off, which reproduces the label's mg/kg arithmetic for
# a lean patient.
#
# ORAL, IMMEDIATE RELEASE
# =======================
# Taylor DR, Duffin D, Kinney CD, McDevitt DG.  Investigation of diurnal
# changes in the disposition of theophylline.  Br J Clin Pharmacol
# 1983;16:413-416 (PMC1428022, read on the scanned pages).  Eight healthy
# nonsmokers given 5 mg/kg intravenously (aminophylline over 30 min) and by
# mouth (theophylline sodium glycinate elixir), morning and evening, crossed
# over.  Table 1: absorption rate constant 2.95 /h (morning, mean of 7) and
# 1.76 /h (evening, mean of 8), not significantly different; the mean of all
# fifteen individual estimates is 2.31 /h, used here.  Bioavailability 93.0%
# and 100.4%, so F = 1, as the label's "rapidly and completely absorbed"
# holds for the immediate-release forms.  Sustained-release products differ
# product by product and are not offered.
#
# AMINOPHYLLINE
# =============
# The intravenous product is usually aminophylline, theophylline
# ethylenediamine dihydrate, (C7H8N4O2)2.C2H8N2.2H2O, which carries
# 2 x 180.167 / 456.464 = 0.7894 of its mass as anhydrous theophylline
# (C 12.011, H 1.008, N 14.007, O 15.999).  The label rounds this ("Aminophylline
# = theophylline/0.8"), and Frymoyer 2016 (J Clin Pharmacol, PMC4927421)
# multiplied aminophylline doses by 0.79.  The injection (an ANDA product,
# whose label is not on Drugs@FDA) is labelled "aminophylline (calculated as
# the dihydrate) 25 mg/mL (equivalent to 19.7 mg/mL anhydrous theophylline)",
# 0.788, as the Hospira labelling reads (seen in a search result, the
# labeller's site being outside this session's network).  The unit names the salt
# ("mg aminophylline", aminophyllineUnits in R/constants.R), as calcium's
# do, and doseProducts below converts each such dose to theophylline.  The
# curve is serum theophylline in every case.  Aslaksen 1981 (Table 2) found
# intravenous theophylline and an equivalent dose of aminophylline
# indistinguishable.
#
# THERAPEUTIC RANGE
# =================
# The band is the label's: "maintaining average serum theophylline
# concentrations between 10 and 15 mcg/mL will achieve most of the drug's
# potential therapeutic benefit while minimizing the risk of serious adverse
# events", and the time until threshold is read at 20 mg/L, above which "both
# the frequency and severity of adverse reactions increase".  The US asthma
# guideline (NAEPP EPR-3, 2007) uses 5-15 mcg/mL; it was not read here
# (inst/extdata/therapeuticRanges.csv).
#
# PREGNANCY
# =========
# Not applied (inst/extdata/pregnancyRegistry.csv, unquantified): clearance
# falls in the third trimester (label Table I half-life 13.0 h against 8.7 h;
# Gardner 1987 as quoted by Ke 2013, AUC 0.75 postpartum against the third
# trimester), but the primary papers have not been read in full.
# -----------------------------------------------------------------------------

THEOPHYLLINE_V_PER_KG <- 0.45       # L/kg, label (Distribution)
THEOPHYLLINE_TARGET   <- 10         # mg/L, the target Table V's rates reach
THEOPHYLLINE_KA_PO    <- 2.31 / 60  # 1/min, Taylor 1983 Table 1, mean of 15
# Mass of anhydrous theophylline per mass of aminophylline dihydrate
AMINOPHYLLINE_THEOPHYLLINE_FRACTION <- 2 * 180.167 / 456.464

#' Theophylline initial infusion rate from the label (mg/kg/h)
#'
#' Table V of the Theophylline in 5% Dextrose label: the rate that gives a
#' steady-state serum theophylline of 10 mcg/mL in an otherwise healthy
#' nonsmoker of the given age.  See \code{R/drugs_theophylline.R}.
#'
#' @param age age in years
#' @returns the rate, mg/kg/h
#' @keywords internal
theophyllineLabelRate <- function(age)
{
  days  <- age * 365.25
  weeks <- days / 7
  if (days <= 24)      return(1 / 12)          # 1 mg/kg every 12 h
  if (weeks < 6)       return(1.5 / 12)        # 1.5 mg/kg every 12 h
  if (weeks <= 52)     return(0.008 * weeks + 0.21)
  if (age < 9)         return(0.8)
  if (age < 12)        return(0.7)
  if (age < 16)        return(0.5)             # adolescent nonsmokers
  if (age <= 60)       return(0.4)
  0.3
}

#' Theophylline (FDA label; Taylor 1983 for oral absorption)
#'
#' One compartment, clearance from the label's age-specific infusion rates,
#' given as theophylline or as aminophylline; see the header of
#' \code{R/drugs_theophylline.R}.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
theophylline <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)
  pkW  <- if (isTRUE(adjustToFFM)) size$pkWeight else weight

  cl <- theophyllineLabelRate(age) / THEOPHYLLINE_TARGET * pkW / 60   # L/min
  default <- list(
    v1  = THEOPHYLLINE_V_PER_KG * pkW,
    v2  = 1,                          # placeholders: one compartment
    v3  = 1,
    cl1 = cl,
    cl2 = 0,
    cl3 = 0,
    ka_PO = THEOPHYLLINE_KA_PO,
    bioavailability_PO = 1,
    tlag_PO = 0
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Theophylline in 5% Dextrose Injection label (B. Braun, NDA 019826, 2012): ",
    "one compartment, V 0.45 L/kg, clearance = Table V's initial infusion rate ",
    "for age over its 10 mcg/mL target (adults 16-60 y 0.04 L/kg/h), otherwise ",
    "healthy nonsmoker; per kg of the pharmacokinetic weight.  Oral: ka 2.31 /h, ",
    "F 1 (Taylor DR et al., Br J Clin Pharmacol 1983;16:413-416).  Aminophylline ",
    "doses x 0.7894 (dihydrate).  ",
    "https://www.accessdata.fda.gov/drugsatfda_docs/label/2012/019826s023lbl.pdf"
  )

  list(
    PK = PK,
    tPeak = 0,
    MEAC = 0,
    typical = 12.5,
    upperTypical = 15,
    lowerTypical = 10,
    reference = reference,
    doseProducts = stats::setNames(AMINOPHYLLINE_THEOPHYLLINE_FRACTION,
                                   AMINOPHYLLINE_SALT)
  )
}
