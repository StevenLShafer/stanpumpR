# -----------------------------------------------------------------------------
# Caffeine in preterm neonates (Lee 2002): one compartment, linear in weight
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min; amounts in
# mg of caffeine base, so the plotted concentration is mg/L (mcg/mL).
#
# Drafted by Claude Code, 2026-11-12, at the request of Steven L. Shafer, who
# asked for the common drugs with narrow therapeutic windows that the library
# lacked.  Every number below was read from the full text of the source in
# PubMed Central (PMC1874379: the Methods, Results, Table 1 and Figure 1).
#
# THE SOURCE
# ==========
# Lee HS, Khoo YM, Chirino-Barcelo Y, Tan KL, Ong D.  Caffeine in apnoeic
# Asian neonates: a sparse data analysis.  Br J Clin Pharmacol 2002;54:31-37
# (doi 10.1046/j.1365-2125.2002.01589.x).  Eighteen preterm neonates with
# apnoea in Singapore (gestational age 28.9 +/- 2.2 weeks, 24-33; birth
# weight 1116 +/- 269 g, 676-1700 g), given "a loading dose of 20 mg kg-1 of
# caffeine citrate (10 mg caffeine base) ... by i.v. infusion over 30 min"
# and "5 mg kg-1 day-1 (2.5 caffeine base) given via i.v. bolus injections
# over 3 min", treated until free of apnoea for 2 weeks and at least 1.8 kg.
# 176 plasma concentrations (heel prick) over up to 21 days, by HPLC; P-Pharm,
# one compartment.  Weight was the only covariate (gestational age, sex and
# ethnic group were not):
#
#     CL (L/h) = 0.004248 x weight (kg) + 0.00154
#     V  (L)   = 0.6299   x weight (kg) + 0.259
#
# (population CL 0.00628 L/h and V 0.961 L; mean half-life 106 h, 75-150).
# The model predicted a separate group of 10 neonates without bias, with a
# precision of 13.8%.  The doses are caffeine base, the curve is caffeine.
#
# Checked against what the fit did not see: the Cafcit label (NDA 020793,
# 2020, read): volume 0.8-0.9 L/kg and a half-life of 3-4 days in neonates;
# this model gives 0.87 L/kg and 106 h at 1.1 kg (switch off).
#
# POSTNATAL AGE, GESTATIONAL AGE AND THE ADULT
# ============================================
# Postnatal age was not available for most of these infants, and caffeine
# clearance rises steeply over the first months as CYP1A2 matures (the label:
# by 9 months metabolism approximates the adult's).  So this model describes
# the preterm neonate in the first three weeks of treatment, from about
# 0.7 to 1.8 kg; it reads neither age nor gestational age, and anywhere else
# (an older infant, a child, an adult) it is far too slow.  It is named for
# its population, as dobutamineNeonatal is, so it cannot become an adult
# default.  Its equations are finite at every weight.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# The model carries its own weight covariate, evaluated at the pharmacokinetic
# weight with the fat-free-mass switch on and at total body weight with it
# off, which reproduces the published equations (dobutamineNeonatal's
# pattern).
#
# DOSE BASIS
# ==========
# Caffeine is labelled and dosed as CAFFEINE CITRATE: "20 mg of caffeine
# citrate is equivalent to 10 mg of caffeine base" (label).  The units name
# the salt ("mg/kg citrate", caffeineCitrateUnits in R/constants.R), and the
# doseProducts block below halves each such dose; plain mg is caffeine base.
# Only the intravenous route is offered: the label says absolute
# bioavailability "was not fully examined in preterm neonates".
#
# THE BAND
# ========
# Lee 2002: "the conventional therapeutic window of 5-20 mg l-1".  The label:
# "A therapeutic plasma concentration range of caffeine could not be
# determined from the placebo-controlled clinical trial", and "Serious
# toxicity has been reported in the literature when serum caffeine levels
# exceed 50 mg/L".  So the band is the library's own (library_band): the
# conventional window, not an established range.
# -----------------------------------------------------------------------------

# Lee 2002, Results and Figure 1
CAFFEINE_NEO_CL_SLOPE     <- 0.004248   # L/h per kg
CAFFEINE_NEO_CL_INTERCEPT <- 0.00154    # L/h
CAFFEINE_NEO_V_SLOPE      <- 0.6299     # L per kg
CAFFEINE_NEO_V_INTERCEPT  <- 0.259      # L
# Mass of caffeine base per mass of caffeine citrate (label)
CAFFEINE_BASE_PER_CITRATE <- 0.5

#' Caffeine in preterm neonates (Lee 2002)
#'
#' One compartment, clearance and volume linear in weight, dosed as caffeine
#' citrate or base; see the header of \code{R/drugs_caffeineNeonatal.R}.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
caffeineNeonatal <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)
  pkW  <- if (isTRUE(adjustToFFM)) size$pkWeight else weight

  default <- list(
    v1  = CAFFEINE_NEO_V_SLOPE * pkW + CAFFEINE_NEO_V_INTERCEPT,
    v2  = 1,                          # placeholders: one compartment
    v3  = 1,
    cl1 = (CAFFEINE_NEO_CL_SLOPE * pkW + CAFFEINE_NEO_CL_INTERCEPT) / 60,
    cl2 = 0,
    cl3 = 0
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Lee HS et al., Br J Clin Pharmacol 2002;54:31-37: one compartment in 18 ",
    "preterm neonates (24-33 weeks, 0.7-1.7 kg at birth), CL 0.004248 x WT + ",
    "0.00154 L/h, V 0.6299 x WT + 0.259 L; caffeine citrate doses x 0.5 ",
    "(label). For the first weeks of life only. ",
    "https://doi.org/10.1046/j.1365-2125.2002.01589.x"
  )

  list(
    PK = PK,
    tPeak = 0,
    MEAC = 0,
    typical = 12.5,
    upperTypical = 20,
    lowerTypical = 5,
    reference = reference,
    doseProducts = stats::setNames(CAFFEINE_BASE_PER_CITRATE, CAFFEINE_SALT)
  )
}
