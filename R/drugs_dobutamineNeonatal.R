# -----------------------------------------------------------------------------
# Dobutamine, intravenous, in neonates on the first days of life (Hallik 2020)
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min.
#
# Drafted by Claude Code, 2026-10-27, at the request of Steven L. Shafer, from
# the full text in PubMed Central (PMC7015735).  Named for its population, as
# milrinonePediatric and fenoldopamPediatric are, so that it is never an
# older patient's default; there is no adult dobutamine model in the library.
#
# THE SOURCE
# ==========
# Hallik M, Ilmoja ML, Standing JF, et al.  Population pharmacokinetics and
# pharmacodynamics of dobutamine in neonates on the first days of life.  Br J
# Clin Pharmacol 2020;86:318-328 (doi 10.1111/bcp.14146, PMID 31657867).
# Twenty-eight critically ill neonates in two Estonian NICUs, gestational age
# 30.4 weeks (median; 22.7-41.0), birth weight 1618 g (465-4380), recruited at
# 6 h of life (2-28 h), given dobutamine 5 mcg/kg/min rising by 5 every 30 min
# to at most 20, for a median 3.5 days.  119 arterial plasma concentrations
# (UHPLC-MS/MS, LLOQ 0.97 mcg/L), NONMEM 7.3, FOCE-I.  One compartment with
# first-order elimination described the data (Michaelis-Menten elimination
# did not improve the fit; parallel linear and Michaelis-Menten improved it
# by only 10 points and was set aside as over-parameterised).  Table 2:
#
#     CL (L/h) = 41.2 x (BW/1.618)^0.75 x PMA^2.67 / (37.4^2.67 + PMA^2.67)
#     V  (L)   = 5.29 x (BW/1.618)
#
# with BW the birth weight in kg, 1.618 the population median, and PMA the
# postmenstrual age in weeks.  Between-subject variability 29%, shared by CL
# and V; proportional residual error 58%, which the authors attribute largely
# to the accuracy of neonatal infusions.  The text prints CL's standard error
# as 44.5, larger than the estimate; the five PKPD fits of Tables 4 and 5
# re-estimate CL at 37.2-42.3 with standard errors near 3, so the 44.5 is
# taken as a misprint and does not touch the value.
#
# The equations themselves were stripped from the PubMed Central text.  Their
# form is fixed by the text and Table 2: allometric scaling "to population
# median BW with power coefficient of 0.75" for CL, a sigmoid maturation on
# PMA "when the maturation of the CL reaches 50% of adult values", and V at
# "5.29 L/1618 g or 3.27 L/kg", which is linear in weight (5.29 / 1.618 =
# 3.27).  41.2 is the MATURE clearance at the median weight: with the
# maturation applied, the study's median infant (PMA 30.4 weeks, maturation
# 0.365) clears 15.0 L/h, 155 mL/min/kg, against the 90 (38) mL/min/kg of the
# only earlier neonatal study the authors cite; read as the clearance of that
# median infant, it would be 424 mL/min/kg.
#
# COVARIATES HERE
# ===============
# Birth weight is the Weight field.  The model is for the first days of life,
# when the two hardly differ; past the first week the weight entered should
# still be the birth weight, not today's.  PMA is the gestational age at birth
# (the Patient Profile's Gestational age at birth field, `gestationalAge`)
# plus the postnatal age (postmenstrualAge(), R/gestationalAge.R); a blank
# field is a term birth, 40 weeks, which overstates a premature infant's
# clearance (one born at 30 weeks by 1.5 times on the first day, the
# maturation being 0.357 against 0.547 at term).  The study covered 22.7-41 weeks and 0.47-4.4 kg on
# the first days of life; outside that, the curve is an extrapolation, and
# the volume in particular falls over the first week as body water is lost,
# which the model does not carry.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# The model carries its own allometric weight terms, evaluated at the
# pharmacokinetic weight with the fat-free-mass switch on and at total body
# weight with it off, so the switch off reproduces the published model.
#
# EFFECT SITE: HEART RATE
# =======================
# ke0 is supplied directly: 6.59 /h (SE 2.18), the equilibration rate
# constant of the heart-rate model (Table 5; "mean effect time (1/ke0) of 9
# min"), the only one of the six haemodynamic responses that needed an effect
# compartment.  Mean arterial pressure and cerebral oxygen extraction followed
# the plasma concentration with no delay, and cardiac output and ejection
# fraction were fitted to the plasma concentration directly, so the effect
# site drawn here is heart rate's.  The heart-rate fit re-estimated CL and V
# (42.3 L/h, 5.42 L) within 3% of Table 2's; its ke0 is used with Table 2's
# disposition unchanged, the source's own model, rather than carried as a
# time to peak effect, because ke0 was a fixed effect with no between-subject
# variability and the time to peak in one compartment moves with each
# infant's clearance (13.5 min at the study median).  No heart rate is
# plotted; the Emax model (baseline 138, maximum 172 beats/min, EC50 39.2
# mcg/L, gamma 3.36) is recorded in the vasoactive parameter audit.
#
# NO BAND
# =======
# There is no therapeutic range for dobutamine.  The EC50s (MAP 25.4, heart
# rate 39.2, cerebral oxygen extraction 52.9, left ventricular output 117
# mcg/L) are concentrations of half-maximal effect on different endpoints in
# this population, not a target, so none is drawn and MEAC is zero.
#
# PREGNANCY
# =========
# A neonatal model: not applicable (inst/extdata/pregnancyRegistry.csv).
# -----------------------------------------------------------------------------

# Hallik 2020, Table 2 (final linear PK model)
DOBUTAMINE_NEO_CL     <- 41.2    # L/h at 1.618 kg, mature
DOBUTAMINE_NEO_V      <- 5.29    # L at 1.618 kg
DOBUTAMINE_NEO_WEIGHT <- 1.618   # kg, the population median birth weight
DOBUTAMINE_NEO_TM50   <- 37.4    # weeks PMA
DOBUTAMINE_NEO_HILL   <- 2.67
# Hallik 2020, Table 5: the heart-rate model's equilibration rate constant
DOBUTAMINE_NEO_KE0    <- 6.59    # /h

#' Dobutamine, intravenous, in neonates (Hallik 2020)
#'
#' One compartment with allometric birth weight and a postmenstrual-age
#' maturation on clearance; the effect site is heart rate's.  See the header
#' of \code{R/drugs_dobutamineNeonatal.R}.
#'
#' @inheritParams cefazolin
#' @param gestationalAge gestational age at birth in weeks, or NULL for a
#'   term birth (40 weeks)
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
dobutamineNeonatal <- function(weight, height, age, sex, adjustToFFM = TRUE,
                               gestationalAge = NULL)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)
  pkW  <- if (isTRUE(adjustToFFM)) size$pkWeight else weight

  pma <- postmenstrualAge(age, gestationalAge)
  maturation <- pma^DOBUTAMINE_NEO_HILL /
    (DOBUTAMINE_NEO_TM50^DOBUTAMINE_NEO_HILL + pma^DOBUTAMINE_NEO_HILL)
  cl <- DOBUTAMINE_NEO_CL * (pkW / DOBUTAMINE_NEO_WEIGHT)^0.75 * maturation

  default <- list(
    v1  = DOBUTAMINE_NEO_V * pkW / DOBUTAMINE_NEO_WEIGHT,
    v2  = 1,                     # one compartment
    v3  = 1,
    cl1 = cl / 60,
    cl2 = 0,
    cl3 = 0
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  gaNote <- if (is.null(gestationalAge) || is.na(gestationalAge))
    "a term birth (gestational age not entered)" else
    sprintf("a gestational age at birth of %s weeks", format(gestationalAge))
  reference <- paste0(
    "Hallik M et al., Br J Clin Pharmacol 2020;86:318-328.  One compartment ",
    "in 28 neonates on the first days of life (gestational age 22.7-41 ",
    "weeks): CL 41.2 L/h x (BW/1.618 kg)^0.75 x maturation (TM50 37.4 weeks ",
    "PMA, Hill 2.67), V 5.29 L x BW/1.618; PMA ", sprintf("%.1f", pma),
    " weeks, from ", gaNote, ".  ke0 6.59 /h, the heart-rate effect site.  ",
    "https://doi.org/10.1111/bcp.14146"
  )

  list(
    PK = PK,
    ke0 = DOBUTAMINE_NEO_KE0 / 60,
    MEAC = 0,
    typical = 0,
    upperTypical = 0,
    lowerTypical = 0,
    reference = reference
  )
}
