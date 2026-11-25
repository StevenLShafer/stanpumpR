# -----------------------------------------------------------------------------
# Mycophenolate: mycophenolic acid (MPA) after oral mycophenolate mofetil;
# two compartments with a lag and first-order absorption, creatinine
# clearance and serum albumin on the central volume and clearance, sex and
# the ciclosporin dose on clearance
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in mcg/mL (= mg/L) of total mycophenolic acid in plasma.
#
# Drafted by Claude Code, 2026-11-25, at the request of Steven L. Shafer (the
# Transplant row of docs/library-expansion/plan.csv).
#
# THE SOURCE
# ==========
# van Hest RM, van Gelder T, Vulto AG, Mathot RAA.  Population
# pharmacokinetics of mycophenolic acid in renal transplant recipients.  Clin
# Pharmacokinet 2005;44(10):1083-1096 (PMID 16176120), read in full on the PDF
# (papers/digests/PMID_16176120.md).  A randomised concentration-controlled
# trial of oral mycophenolate mofetil (MMF) at Erasmus MC, Rotterdam: 140
# adult renal transplant recipients, all on ciclosporin and corticosteroids,
# sampled on nine occasions from day 3 to day 140 after transplantation (6523
# total MPA concentrations by HPLC), NONMEM V, first-order method.
# Two compartments, first-order absorption after a lag, apparent parameters
# (F not identifiable).  Table II (p. 1089), final model, and equations 6 and
# 7 (p. 1090):
#
#     V1  = 91 L x (CLCR/48)^-0.62 x (Alb/30)^-1.13
#     CL  = 32.5 L/h x (CLCR/48)^-0.12 x (Alb/30)^-1.07 x (CsA/450)^0.31
#           x 1.11 for a man
#     V2  = 237 L,  Q = 35 L/h,  ka = 4.1 /h,  tlag = 0.21 h
#
# CLCR is the Cockcroft-Gault creatinine clearance in mL/min (not indexed to
# body surface area), Alb the plasma albumin in g/L, CsA the ciclosporin daily
# dose in mg.  The reference of equation 7 is a WOMAN (gender 0): Table II's
# 33 L/h is the same estimate printed to two figures, and the text's worked
# examples (32 to 39 L/h as CLCR falls from 60 to 10 mL/min) are the woman's.
#
# The model has no enterohepatic recirculation: the authors fitted none, and
# say the maxima are slightly underpredicted (p. 1090).  The second peak of
# MPA 6-12 h after a dose, from MPA glucuronide returned through the bile, is
# not drawn.  Steven L. Shafer decided (2026-11-25) to build this model now,
# on the linear engine, and to wait for a source that fits the recirculation
# in patients on tacrolimus before proposing an engine for it.
#
# DOSE BASIS (AGENTS.md, Dose basis)
# ==================================
# The paper states its basis: "mycophenolate mofetil doses were converted to
# the equivalent MPA content by multiplying the mycophenolate mofetil dose by
# 0.739" (p. 1085), so every parameter is per mg of MPA.  CellCept and its
# generics are labelled and dosed as mycophenolate mofetil, so the unit names
# the product ("mg PO mofetil", MYCOPHENOLATE_MOFETIL_PRODUCT in
# R/constants.R) and the doseProducts block below converts each such dose by
# the paper's 0.739.  The curve is MPA.  One drug serves both mycophenolate
# products, by Shafer's decision of 2026-11-25; enteric-coated mycophenolate
# sodium (Myfortic, labelled as MPA) is not yet offered, its absorption
# needing a source of its own (the plan row's notes).
#
# WHAT IS HELD AT THE REFERENCE
# =============================
# The ciclosporin daily dose, at the equation's 450 mg (Shafer, 2026-11-25):
# the Patient Profile has no field for it.  Every patient in the source took
# ciclosporin, which lowers MPA exposure by blocking the biliary excretion of
# its glucuronide; a patient on tacrolimus, today's usual partner, typically
# has a higher MPA exposure at the same dose, which this model does not show.
#
# COVARIATES
# ==========
# CLCR: Cockcroft-Gault from the Patient Profile's serum creatinine, put on
# the adult scale by adultEquivalentCreatinine(), or from the library's
# assumed normal creatinine when the field is blank, at the pharmacokinetic
# weight (total weight with the switch off).  Held within the 4-142 mL/min the
# source studied (Table I, day 3), so that the negative powers stay finite.
# Albumin: the Patient Profile's Serum albumin (g/dL, x 10 to g/L), held
# within the 22-53 g/L the source studied (Table I, day 3 to day 140); blank
# is the equations' reference, 30 g/L (3.0 g/dL), which is low: the cohort's
# median rose from 34 g/L on day 3 to 40 g/L on day 140.  Lower renal function
# and lower albumin RAISE total MPA clearance and V1 (less binding to
# albumin); the unbound concentration, which the source did not measure, need
# not rise with them.  Sex: x 1.11 on clearance for a man, as fitted.
# Tested and not retained: age, race, weight, diabetes, AST/ALT, bilirubin,
# haemoglobin, the MMF dose.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# The source retained no size term (weight was tested and dropped), so every
# parameter belongs to its cohort, and with the switch on each is anchored at
# that cohort's typical patient (MYCOPHENOLATE_COHORT): V1 and V2 by the
# volume factor, Q by the clearance factor, and CL, which carries the
# source's own sex term, by the clearance factor of the patient's own sex
# within the cohort (cohortSexSizeFactors(), as for fluoxetine).  The CLCR
# terms are kept as fitted, with the Cockcroft-Gault weight inside them
# (Steven L. Shafer, 2026-11-25: "Use fat-free mass for mycophenolate's size
# scaling"; a renal clearance term with weight inside takes no size factor,
# but here CLCR is a covariate on a hepatic clearance and on V1, with small
# negative exponents, not a renal clearance, so the fat-free-mass factors
# apply on top, and the Cockcroft-Gault weight is the pharmacokinetic weight).  With the switch off
# every factor is 1 and the published values are used.
# The cohort: Table I (p. 1086), day 3, 140 patients, 88 men and 52 women,
# medians 69 kg and 50 years.  No height, BMI or BSA is printed, so each sex
# takes NCD-RisC's mean height at 18 for the Netherlands, birth year 1955
# (publication 2005 less the median age, the years of collection not being
# printed): men 180.2 cm, women 167.1 cm, less the 0.2 cm (men) and 0.3 cm
# (women) lost per decade after 18: 179.6 and 166.1 cm.  The median weight is
# not printed by sex, and is used for both.
#
# PREGNANCY (R/pregnancy.R): no measured pharmacokinetics of MPA in pregnancy
# were found; mycophenolate is teratogenic and contraindicated in pregnancy
# (CellCept label, boxed warning).  Nothing is applied.
#
# NO BAND: MPA is monitored by the area under the curve over a dosing
# interval (30-60 mg.h/L with ciclosporin, the target the source cites), an
# exposure target, not a concentration range (inst/extdata/therapeuticRanges.csv).
#
# References
# ----------
# van Hest RM et al. Clin Pharmacokinet 2005;44:1083-1096.
#   https://doi.org/10.2165/00003088-200544100-00006
# -----------------------------------------------------------------------------

# van Hest 2005, Table II final model and equations 6 and 7
MYCOPHENOLATE_VANHEST <- list(
  v1 = 91,              # L at CLCR 48 mL/min, albumin 30 g/L
  v1Clcr = -0.62,
  v1Albumin = -1.13,
  cl = 32.5,            # L/h, a woman at CLCR 48, albumin 30, ciclosporin 450
  clClcr = -0.12,
  clAlbumin = -1.07,
  clCiclosporin = 0.31,
  clMale = 1.11,
  v2 = 237,             # L
  q = 35,               # L/h
  ka = 4.1,             # /h
  tlag = 0.21,          # h
  clcrRef = 48,         # mL/min, Cockcroft-Gault
  albuminRef = 30,      # g/L, held when the field is blank
  ciclosporinRef = 450, # mg/day, held: no Patient Profile field
  clcrMin = 4,          # mL/min, Table I day 3
  clcrMax = 142,
  albuminMin = 22,      # g/L, Table I day 3
  albuminMax = 53       # g/L, Table I day 140
)
# "mycophenolate mofetil dose by 0.739" (p. 1085): mg MPA per mg MMF
MYCOPHENOLATE_MPA_PER_MOFETIL <- 0.739

# van Hest 2005, Table I (p. 1086), day 3: 88 men and 52 women, medians 69 kg
# and 50 years; heights from NCD-RisC (Netherlands, born 1955), see BODY SIZE
MYCOPHENOLATE_COHORT <- list(groups = list(
  list(weight = 69, height = 179.6, age = 50, men = 88, women = 0),
  list(weight = 69, height = 166.1, age = 50, men = 0, women = 52)))

#' Mycophenolic acid after oral mycophenolate mofetil (van Hest 2005)
#'
#' Two compartments with a lag and first-order absorption, apparent oral
#' parameters per mg of mycophenolic acid; Cockcroft-Gault creatinine
#' clearance and serum albumin on the central volume and clearance, sex and
#' the ciclosporin dose (held at 450 mg/day) on clearance.  Doses are
#' mycophenolate mofetil ("mg PO mofetil"), converted by 0.739.  See the
#' header of \code{R/drugs_mycophenolate.R}.
#'
#' @inheritParams cefazolin
#' @param creatinine serum creatinine, mg/dL, or NULL for an assumed normal
#'   value
#' @param albumin serum albumin, g/dL, or NULL for the equations' reference
#'   (3.0 g/dL); held within the studied 2.2-5.3 g/dL
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
mycophenolate <- function(weight, height, age, sex, adjustToFFM = TRUE,
                          creatinine = NULL, albumin = NULL)
{
  p <- MYCOPHENOLATE_VANHEST
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume = 1, legacyClearance = 1,
                        cohort = do.call(cohortSizeFactors, MYCOPHENOLATE_COHORT))
  # CL carries the source's sex term, so it is anchored at the patient's own
  # sex within the cohort
  sizeSex <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                           legacyVolume = 1, legacyClearance = 1,
                           cohort = cohortSexSizeFactors(MYCOPHENOLATE_COHORT, sex))
  w <- if (isTRUE(adjustToFFM)) size$pkWeight else weight
  scr <- adultEquivalentCreatinine(creatinine, age, sex)
  crcl <- creatinineClearanceCG(w, age, sex, scr)                     # mL/min
  crclUsed <- min(max(crcl, p$clcrMin), p$clcrMax)
  if (!is.null(albumin) && (length(albumin) != 1 || !is.finite(albumin))) albumin <- NULL
  alb <- if (is.null(albumin)) p$albuminRef else
    min(max(10 * albumin, p$albuminMin), p$albuminMax)                # g/L
  male <- identical(sex, SEX_MALE)

  v1 <- p$v1 * (crclUsed / p$clcrRef)^p$v1Clcr * (alb / p$albuminRef)^p$v1Albumin
  cl <- p$cl * (crclUsed / p$clcrRef)^p$clClcr * (alb / p$albuminRef)^p$clAlbumin *
    (p$ciclosporinRef / p$ciclosporinRef)^p$clCiclosporin *
    (if (male) p$clMale else 1)                                       # L/h

  default <- list(
    v1  = v1 * size$volume,
    v2  = p$v2 * size$volume,
    v3  = 1,
    cl1 = cl * sizeSex$clearance / 60,
    cl2 = p$q * size$clearance / 60,
    cl3 = 0,
    ka_PO = p$ka / 60,
    bioavailability_PO = 1,          # apparent parameters
    tlag_PO = p$tlag * 60
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  creatinineNote <- if (is.null(creatinine))
    paste0("assumed normal serum creatinine ", format(scr), " mg/dL") else
    paste0("serum creatinine ", format(creatinine), " mg/dL")
  reference <- paste0(
    "van Hest RM, van Gelder T, Vulto AG, Mathot RAA. Population pharmacokinetics ",
    "of mycophenolic acid in renal transplant recipients. Clin Pharmacokinet ",
    "2005;44:1083-1096. 140 adults on ciclosporin, days 3-140 after renal ",
    "transplantation: two compartments, V1 91 L x (CLCR/48)^-0.62 x (Alb/30)^-1.13, ",
    "CL 32.5 L/h x (CLCR/48)^-0.12 x (Alb/30)^-1.07 x (ciclosporin/450 mg/day)^0.31 ",
    "x 1.11 for a man, V2 237 L, Q 35 L/h, ka 4.1 /h, lag 0.21 h, apparent oral, ",
    "per mg MPA (mycophenolate mofetil x 0.739). CLCR (Cockcroft-Gault) ",
    sprintf("%.0f", crcl), " mL/min",
    if (crclUsed != crcl) sprintf(", held at %.0f, the studied limit", crclUsed) else "",
    ", ", creatinineNote, "; albumin ",
    if (is.null(albumin)) "held at the reference 30 g/L" else
      sprintf("%.0f g/L (studied 22-53 g/L)", alb),
    "; ciclosporin held at 450 mg/day. No enterohepatic recirculation. ",
    "Total plasma MPA. https://doi.org/10.2165/00003088-200544100-00006"
  )

  list(
    PK = PK,
    tPeak = 0,
    MEAC = 0,
    typical = 0,
    upperTypical = 0,
    lowerTypical = 0,
    reference = reference,
    doseProducts = stats::setNames(MYCOPHENOLATE_MPA_PER_MOFETIL,
                                   MYCOPHENOLATE_MOFETIL_PRODUCT)
  )
}
