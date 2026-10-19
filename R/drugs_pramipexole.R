# -----------------------------------------------------------------------------
# Pramipexole, immediate-release tablets (Mirapex): two compartments, apparent
# oral, with creatinine clearance on clearance and weight on the peripheral
# volume
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in ng/mL of pramipexole base.
#
# Drafted by Claude Code, 2026-10-19, at the request of Steven L. Shafer, who
# asked for the drugs used to treat Parkinson's disease
# (inst/help/models/parkinsons.md, inst/extdata/parkinsonsRegistry.csv).
#
# THE SOURCE
# ==========
# FDA Clinical Pharmacology and Biopharmaceutics Review of NDA 22-421
# (Mirapex ER), signed 27 July 2009, read in full: Boehringer Ingelheim's
# population model of study 248.524 (early Parkinson's disease, IR and ER
# tablets, sparse sampling) with phase 1 studies 248.560 and 248.530 and the
# renal impairment study M/2730/0060, re-run by the FDA's pharmacometrics
# reviewers (F. Li, Y. Wang) with near-identical results.  Two compartments,
# first-order absorption with a lag for the IR tablet, sequential zero- then
# first-order absorption for the ER tablet (review section 4.3.3).  Table 3,
# sponsor estimates:
#
#     CL/F 29.2 L/h, V2/F 35.6 L, Q/F 115 L/h, V3/F 313 L (75 kg)
#     IR: ka 0.517 /h, lag 0.221 h
#     ER: zero-order release over D1 1.34 h, then ka 0.0873 /h
#
# and the covariates, in the review's words (pp. 58-60): "the typical CL/F was
# 29.2 L/h when CRCL was greater or equal to 121 mL/min.  Otherwise, the
# typical individual CL/F is reduced linearly by 0.74% by reducing the CRCL by
# 1 mL/min", the FDA's "hockey stick function (CL/F = thetaCL (1 + thetaCRCL
# (CRCL - 121)))", and "The estimate of the typical V3/F for a 75 kg patient
# was 313 L.  A change in 1 kg body weight changes V3/F by 2.26%."  So
#
#     CL/F = 29.2 x (1 + 0.0074 x (min(CrCl, 121) - 121))   L/h
#     V3/F = 313 x (1 + 0.0226 x (weight - 75))               L
#
# Sex, age and race had no further effect.  The ER tablet is
# R/drugs_pramipexoleER.R, the same disposition with the ER input.
#
# DOSE BASIS
# ==========
# Every US strength is mg of pramipexole DIHYDROCHLORIDE MONOHYDRATE, and the
# concentrations are of the base.  The review (p. 13): "For calculation of
# dose normalized PK parameters or the CL/F, the free base dose was determined
# as the pramipexole dihydrochloride monohydrate dose divided by a factor of
# 1.431."  So the model's CL/F and volumes are per mg of base, and a dose is
# entered as the label's mg of salt and divided by 1.431 here, by multiplying
# every volume and clearance by 1.431.  That basis is confirmed by the review's
# own simulations: the model reproduces its day-7 AUCs (12.89, 9.43 and
# 13.46 ng.h/mL for 0.375 mg ER qd at CrCl 80, and 0.1875 mg at 50 and 30)
# to 0.3%, where a salt-basis dose would be 43% high (tests).  (The European
# label states strengths as base: 0.088 mg base is 0.125 mg salt.)
#
# WHAT IS INFERRED
# ================
# Three things the review does not say, each a choice recorded here:
# 1. The creatinine clearance formula.  The 121 mL/min breakpoint and the
#    label's categories are absolute mL/min, so Cockcroft-Gault is used.  (The
#    clinical review records that the phase 3 protocol was amended to estimate
#    it by MDRD; whether the population analysis used that is not stated.)
# 2. V3 is printed "V3-ER" and its variability is "for the ER formulation";
#    no separate IR value is listed, so one V3 is taken for both tablets.
# 3. V2, Q, the IR lag and D1 carry no standard error and were probably fixed
#    from the healthy-volunteer analysis.  They are used as printed.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# The model has its own size covariates (weight on V3, and weight through
# Cockcroft-Gault on CL), evaluated at the pharmacokinetic weight with the
# fat-free-mass switch on and at total body weight with it off; the
# size-free V2 and Q are scaled by the library's factors, which are 1 with
# the switch off.  The linear weight term reaches zero at 31 kg, below anyone
# in a Parkinson's disease trial, so the weight it sees is held at 40 kg and
# above (PRAMIPEXOLE_MIN_WEIGHT): a guard, not part of the source.
#
# RENAL FUNCTION: Cockcroft-Gault at the entered serum creatinine (a child's on
# the adult scale) or, if blank, an assumed normal creatinine for age and sex
# (R/renalFunction.R).  About 90% of a dose is excreted unchanged, secreted
# by OCT2 as well as filtered; the label's cimetidine (AUC +50%) and the
# population analysis's 20% lower clearance with cationic renally secreted
# drugs are not applied.  No transporter genotype has a quantified effect.
#
# CHECKS (75 kg, CrCl at or above 121, label strengths as salt)
# =============================================================
# Terminal half-life 10.0 h (label 8 h young, 12 h elderly).  A single IR
# tablet peaks at 0.9 h, earlier than the label's 2 h: the central volume is
# small and the distribution fast (V2, Q fixed).  IR 0.25 mg tid at
# steady state Cmax 1.01 ng/mL, AUC 17.9 ng.h/mL against Jenner 2009's 1.09
# and 16.0; IR 1.5 mg tid AUC0-24 108 against 92-94 in study 248.530.
#
# NO EFFECT SITE, NO BAND: the population analysis found no relation between
# concentration and response (review p. 59), the effect builds over days of
# titration, and no concentration range is published that this review or the
# label supports.
#
# References
# ----------
# FDA CDER, Clinical Pharmacology and Biopharmaceutics Review, NDA 22-421.
#   https://www.accessdata.fda.gov/drugsatfda_docs/nda/2010/022421s000ClinPharmR.pdf
# Mirapex label: https://www.accessdata.fda.gov/drugsatfda_docs/label/2021/020667s044lbl.pdf
# -----------------------------------------------------------------------------

# FDA review of NDA 22-421, Table 3 (sponsor), hours and litres
PRAMIPEXOLE_CL        <- 29.2     # L/h at CrCl >= 121 mL/min
PRAMIPEXOLE_CRCL_REF  <- 121      # mL/min, the hockey stick's knee
PRAMIPEXOLE_CRCL_SLOPE <- 0.0074  # fraction of CL per mL/min below the knee
PRAMIPEXOLE_V2        <- 35.6     # L
PRAMIPEXOLE_Q         <- 115      # L/h
PRAMIPEXOLE_V3        <- 313      # L at 75 kg
PRAMIPEXOLE_V3_WT_REF <- 75       # kg
PRAMIPEXOLE_V3_WT_SLOPE <- 0.0226 # fraction of V3 per kg
PRAMIPEXOLE_KA_IR     <- 0.517    # /h
PRAMIPEXOLE_TLAG_IR   <- 0.221    # h
PRAMIPEXOLE_KA_ER     <- 0.0873   # /h
PRAMIPEXOLE_D1_ER     <- 1.34     # h, zero-order release before absorption
# mg of the dihydrochloride monohydrate per mg of base (review p. 13)
PRAMIPEXOLE_SALT_PER_BASE <- 1.431
# The lowest weight the linear V3 term sees: a guard, not from the source
PRAMIPEXOLE_MIN_WEIGHT <- 40

#' Pramipexole disposition shared by the IR and ER tablets
#'
#' The two-compartment apparent model of the FDA review of NDA 22-421, scaled
#' to doses of the dihydrochloride monohydrate; see the header of
#' \code{R/drugs_pramipexole.R}.
#'
#' @inheritParams cefazolin
#' @param creatinine serum creatinine in mg/dL, or NULL for the assumed normal
#'   value for age and sex
#' @returns a PK set without absorption fields, and the creatinine clearance
#' @keywords internal
pramipexoleDisposition <- function(weight, height, age, sex, adjustToFFM = TRUE,
                                   creatinine = NULL)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume = 1, legacyClearance = 1)
  pkW  <- if (isTRUE(adjustToFFM)) size$pkWeight else weight
  crcl <- creatinineClearanceCG(pkW, age, sex,
                                adultEquivalentCreatinine(creatinine, age, sex))
  salt <- PRAMIPEXOLE_SALT_PER_BASE
  cl <- PRAMIPEXOLE_CL *
    (1 + PRAMIPEXOLE_CRCL_SLOPE * (min(crcl, PRAMIPEXOLE_CRCL_REF) - PRAMIPEXOLE_CRCL_REF))
  v3 <- PRAMIPEXOLE_V3 * (1 + PRAMIPEXOLE_V3_WT_SLOPE *
                            (max(pkW, PRAMIPEXOLE_MIN_WEIGHT) - PRAMIPEXOLE_V3_WT_REF))
  list(
    pk = list(
      v1  = PRAMIPEXOLE_V2 * size$volume * salt,
      v2  = v3 * salt,
      v3  = 1,
      cl1 = cl * salt / 60,
      cl2 = PRAMIPEXOLE_Q * size$clearance * salt / 60,
      cl3 = 0
    ),
    crcl = crcl
  )
}

#' Pramipexole, immediate-release tablets
#'
#' Two compartments, apparent (/F), from the FDA review of NDA 22-421; doses
#' in mg of pramipexole dihydrochloride monohydrate, concentrations in ng/mL
#' of base.  See the header of \code{R/drugs_pramipexole.R}.
#'
#' @inheritParams pramipexoleDisposition
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
pramipexole <- function(weight, height, age, sex, adjustToFFM = TRUE,
                        creatinine = NULL)
{
  d <- pramipexoleDisposition(weight, height, age, sex, adjustToFFM, creatinine)
  default <- c(d$pk, list(
    ka_PO = PRAMIPEXOLE_KA_IR / 60,
    bioavailability_PO = 1,          # apparent parameters
    tlag_PO = PRAMIPEXOLE_TLAG_IR * 60
  ))
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  list(
    PK = PK, tPeak = 0, MEAC = 0,
    typical = 0, upperTypical = 0, lowerTypical = 0,
    reference = paste0(
      "FDA Clinical Pharmacology Review of NDA 22-421 (Mirapex ER), Table 3: ",
      "two compartments, CL/F 29.2 L/h x (1 + 0.0074 (CrCl - 121)) below a ",
      "Cockcroft-Gault CrCl of 121 mL/min from the entered or an assumed normal ",
      "creatinine, V2/F 35.6 L, Q/F 115 L/h, V3/F 313 L x (1 + 0.0226 (weight - 75)); ",
      "IR ka 0.517/h, lag 0.221 h; doses as the dihydrochloride monohydrate ",
      "(base = salt / 1.431); concentration only. ",
      "https://www.accessdata.fda.gov/drugsatfda_docs/nda/2010/022421s000ClinPharmR.pdf"
    )
  )
}
