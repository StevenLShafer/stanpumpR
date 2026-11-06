# -----------------------------------------------------------------------------
# Busulfan, intravenous (McCune 2014): plasma busulfan in hematopoietic cell
# transplant recipients from infancy to 66 years
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min.
#
# Drafted by Claude Code, 2026-11-06, at the request of Steven L. Shafer, as a
# first-wave chemotherapy drug (docs/chemotherapy.md,
# inst/extdata/chemotherapyRegistry.csv, source P23).  The numbers below come
# from the article's Table 2, which Shafer transcribed from the PDF into the
# session (2026-11-06), from the full text in PubMed Central (PMC3946385:
# Methods and Results) and from the supplement he supplied (Tables S1, S2,
# S4-S7).
#
# THE SOURCE
# ==========
# McCune JS, Bemer MJ, Barrett JS, Baker KS, Gamis AS, Holford NHG.  Busulfan
# in infant to adult hematopoietic cell transplant recipients: a population
# pharmacokinetic model for initial and Bayesian dose personalization.  Clin
# Cancer Res 2014;20:754-763 (doi 10.1158/1078-0432.CCR-13-1960).  12,380
# concentrations from 1,610 recipients, 0.1-65.8 years (92% under 20; 466
# under 2), at 51 institutions, 1999-2011; intravenous busulfan by zero-order
# infusion; GC-MS, 25-4500 ng/mL.  NONMEM VII, FOCE-I; parameters from 100
# bootstrap replications.
#
# THE MODEL (Table 2)
# ===================
# Two compartments, first-order elimination, theory-based allometry on normal
# fat mass (NFM), sigmoid maturation of clearance by postmenstrual age (PMA):
#
#     CL = 11.4 L/h x (NFM_CL/62)^0.75 x Fmat         (62 kg NFM_CL)
#     Q  = 135.2 L/h x (NFM_CL/62)^0.75               (no maturation, no sex)
#     V1 = 13.9 L x (NFM_V/59) x Fsex                 (59 kg NFM_V)
#     V2 = 29.9 L x (NFM_V/59) x Fsex
#
#     NFM_CL = FFM + 0.509 (TBW - FFM),  NFM_V = FFM + 0.203 (TBW - FFM)
#     Fmat   = PMA^2.3 / (45.7^2.3 + PMA^2.3),  PMA in weeks
#     Fsex   = 1.07 in women (V1 + V2), 1 in men
#
# The units column of Table 2 ties Q to the clearance NFM (L/h/62 kg NFM CL)
# and V2 to the volume NFM, and the Methods say Q has no maturation or sex
# term and V1 and V2 no maturation term.  The reference NFMs, 62 kg for CL
# and 59 kg for V, "correspond to an allometrically scaled total body weight
# of 70 kg" (Table 2's footnote).
#
# FFM is the Janmahasatian form the paper states, WHSmax HT^2 TBW / (WHS50
# HT^2 + TBW), HT in metres, with WHSmax 42.92 and 37.99 kg/m^2 and WHS50
# 30.93 and 35.98 kg/m^2 for men and women (Methods; Table S7).  TBW is the
# actual body weight: the paper estimated it from dosing weight where actual
# weight was missing (8% higher in women, FFEM_DW 1.08), which a user's entry
# makes unnecessary.  PMA is the gestational age at birth plus the postnatal
# age; the paper assumed 40 weeks for every infant, which is the Patient
# Profile's blank, and a preterm birth entered there moves maturation as the
# model's PMA intends.
#
# Table S7's worked example does NOT follow this FFM equation: for its
# 40-year-old 70-kg 176-cm man it gives 49.7 kg where the equation gives 56.1,
# and for its 1-year-old 10-kg 76-cm girl 8.89 where it gives 7.13.  The
# example's clearance is therefore about 5% below this model's for the man.
# The Methods' equation is what the model was fitted with, so it is used; the
# test pins the disagreement.
#
# CLEARANCE FALLS DURING THE COURSE (courseSteps)
# ===============================================
# Table 2: clearance from 6 to 36 h after the start of the course is 0.932 of
# that in the first 6 h, and from 36 h on 0.919.  The model returns a PK set
# for each step and courseSteps naming them; simCpCe() switches between them
# at 6 and 36 h after this drug's first dose, through the event engine, which
# advances the steps exactly.  The fall is measured from the course's first
# dose, so a second course in the same plot is not reset.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# The model carries its own fat-free-mass covariate (Eleveld-style), so it
# takes adjustToFFM and ignores it: both switch positions give the published
# model.
#
# PREGNANCY (R/pregnancy.R)
# =========================
# No human pharmacokinetics of busulfan in pregnancy were found (PubMed,
# 2026-11-06).  The registry records it as unknown, and nothing is applied.
#
# NO EFFECT SITE, NO BAND
# =======================
# Plasma only.  The targets (an average steady-state concentration, a daily
# or cumulative AUC; the EBMT guide, registry G05) are exposures, not a band.
# -----------------------------------------------------------------------------

# McCune 2014, Table 2 (bootstrap estimates)
BUSULFAN_MCCUNE <- list(
  cl = 11.4,          # L/h per 62 kg NFM_CL
  v1 = 13.9,          # L per 59 kg NFM_V
  q = 135.2,          # L/h per 62 kg NFM_CL
  v2 = 29.9,          # L per 59 kg NFM_V
  nfmCL = 62,         # kg
  nfmV = 59,          # kg
  ffatCL = 0.509,
  ffatV = 0.203,
  tm50 = 45.7,        # weeks PMA
  hill = 2.3,
  femaleV = 1.07,     # on V1 and V2
  courseFactor = c(0.932, 0.919),     # CL after 6 h, after 36 h
  courseAfter = c(6, 36) * 60         # min after the course's first dose
)
# Janmahasatian FFM constants as McCune 2014 states them (kg/m^2)
BUSULFAN_WHS <- list(male = c(max = 42.92, half = 30.93),
                     female = c(max = 37.99, half = 35.98))
BUSULFAN_COURSE_EVENTS <- c("busulfanCourse6h", "busulfanCourse36h")

#' Fat-free mass as McCune 2014 states it (Janmahasatian), kg
#' @noRd
busulfanFFM <- function(weight, height, sex)
{
  whs <- BUSULFAN_WHS[[if (sex == SEX_MALE) "male" else "female"]]
  h2 <- (height / 100)^2
  whs[["max"]] * h2 * weight / (whs[["half"]] * h2 + weight)
}

#' Busulfan, intravenous (McCune 2014)
#'
#' Two compartments on normal fat mass with maturation of clearance by
#' postmenstrual age, from infancy to adulthood, and a clearance that falls
#' 6.8% at 6 h and 8.1% at 36 h after the course's first dose; see the header
#' of \code{R/drugs_busulfan.R}.
#'
#' @inheritParams cefazolin
#' @param gestationalAge gestational age at birth in weeks, or NULL for a term
#'   birth (40 weeks, as the paper assumed)
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
busulfan <- function(weight, height, age, sex, adjustToFFM = TRUE,
                     gestationalAge = NULL)
{
  p <- BUSULFAN_MCCUNE
  ffm <- busulfanFFM(weight, height, sex)
  nfmCL <- ffm + p$ffatCL * (weight - ffm)
  nfmV  <- ffm + p$ffatV  * (weight - ffm)
  pma <- postmenstrualAge(age, gestationalAge)
  fmat <- pma^p$hill / (p$tm50^p$hill + pma^p$hill)
  fsex <- if (sex == SEX_FEMALE) p$femaleV else 1

  cl <- p$cl * (nfmCL / p$nfmCL)^0.75 * fmat                         # L/h
  set <- function(factor) list(
    v1  = p$v1 * nfmV / p$nfmV * fsex,
    v2  = p$v2 * nfmV / p$nfmV * fsex,
    v3  = 1,                          # placeholder: two compartments
    cl1 = cl * factor / 60,
    cl2 = p$q * (nfmCL / p$nfmCL)^0.75 / 60,
    cl3 = 0
  )
  PK <- stats::setNames(
    list(set(1), set(p$courseFactor[1]), set(p$courseFactor[2])),
    c(PK_EVENT_DEFAULT, BUSULFAN_COURSE_EVENTS))

  reference <- paste0(
    "McCune JS, Bemer MJ, Barrett JS, Baker KS, Gamis AS, Holford NHG. Busulfan ",
    "in infant to adult hematopoietic cell transplant recipients. Clin Cancer ",
    "Res 2014;20:754-763. 1,610 patients, 0.1-66 years: CL 11.4 L/h, V1 13.9 L, ",
    "Q 135.2 L/h, V2 29.9 L on normal fat mass (fat fractions 0.509 CL, 0.203 V; ",
    "62/59 kg), maturation by postmenstrual age (TM50 45.7 wk, Hill 2.3), V x 1.07 ",
    "in women, CL x 0.932 from 6 h and x 0.919 from 36 h after the first dose. ",
    "Plasma busulfan. https://doi.org/10.1158/1078-0432.CCR-13-1960"
  )

  list(
    PK = PK,
    courseSteps = list(after = p$courseAfter, event = BUSULFAN_COURSE_EVENTS),
    tPeak = 0,
    MEAC = 0,
    typical = 0,
    upperTypical = 0,
    lowerTypical = 0,
    reference = reference
  )
}
