# -----------------------------------------------------------------------------
# Desmopressin, intravenous: Agerso 2004 (disposition), Argenti 1997 (effect)
# -----------------------------------------------------------------------------
# Steven L. Shafer, 2026-11-25: desmopressin is "one of my favorite drugs as an
# anesthesiologist": 0.3 mcg/kg intravenously reverses the platelet
# dysfunction of aspirin in the oozing surgical patient, by releasing von
# Willebrand factor (vWF) and factor VIII from the endothelium.  He supplied
# Argenti 1997 on 2026-11-26; Agerso 2004 is in the archive.  Every number
# below was read on the PDF.
#
# DISPOSITION.  Agerso H, Larsen LS, Riis A, Lovgren U, Karlsson MO,
# Senderovitz T. Pharmacokinetics and renal excretion of desmopressin after
# intravenous administration to healthy subjects and renally impaired
# patients. Br J Clin Pharmacol 2004;58:352-358.  doi:10.1111/
# j.1365-2125.2004.02175.x, PMID 15373927 (card papers/digests/
# PMID_15373927.md).  24 adults aged 49-68 (BMI 23-32, height 152-184 cm; the
# paper prints neither weights nor the numbers of men and women), 6 healthy
# and 18 with renal impairment, none on dialysis; 2 mcg desmopressin acetate
# IV over 1 min; venous plasma by radioimmunoassay (LLOQ 2.5 pg/mL) and urine
# for 24 h, fitted together in NONMEM; 22 analysed.  Table 1 (PDF p5):
#   V1 13.9, V2 12.5, V3 11.7 L; Q2 4.43, Q3 27.5 L/h
#   CLr 2.41 L/h and CLnr 3.91 L/h at a creatinine clearance of 60 mL/min
#   CrCL effect 1.74% (CLr) and 0.933% (CLnr) per 1 mL/min
# The covariate is LINEAR in CrCL: Table 1's footnote gives the effect as a
# percentage change per 1 mL change, the text says a fall of 1 mL/min from 60
# lowers CLr by 1.74% and CLnr by 0.933%, and Figure 2 draws both as straight
# lines.  An exponential reading (exp(0.0174 x (CrCL - 60))) gives a renal
# clearance of 1.12 L/h at Table 2's severe group (CrCL 16) against the
# 0.65 measured; the linear one gives 0.56, and the totals of the four groups
# (10, 6.9, 4.9, 2.9 L/h measured) come out 9.7, 7.3, 4.5 and 2.9.  The
# test pins them.
#   CLr  = 2.41 x (1 + 0.0174  (CrCL - 60))   L/h
#   CLnr = 3.91 x (1 + 0.00933 (CrCL - 60))   L/h
# CrCL is the measured 24-h creatinine clearance indexed to 1.73 m^2 by
# Gehan and George's body surface area (Methods).  Here it is Cockcroft-
# Gault (which estimates exactly that measured clearance) on the patient's
# serum creatinine, indexed the same way, as hydrocodone's is
# (R/drugs_hydrocodone.R), and held within the 10-144 mL/min/1.73 m^2 the
# study spanned: the linear renal term reaches zero at 2.5, so below the
# studied range it is extrapolation the paper does not support.  A blank
# Serum creatinine field means the assumed normal creatinine for the
# patient's age and sex (R/renalFunction.R, assumedCreatinine()), not a
# normal clearance: a 70-year-old with a blank field is a 70-year-old's
# clearance.  Why the non-renal clearance also falls with CrCL is not known
# (the authors cite uraemic inhibition of hepatic metabolism).
#
# BODY SIZE (docs/weight-adjustment.md).  Weight, BMI and height were tested
# and not retained.  By the rule that every parameter is scaled whatever the
# source tested (Shafer, 2026-11-19), the volumes and intercompartmental
# clearances take the fat-free-mass factors, and the clearance, whose only
# covariate is a creatinine clearance indexed to 1.73 m^2, keeps it and takes
# the clearance factor too.  With the switch off the published values are
# used unscaled (legacy factors 1).  BODY SIZE ANCHOR: the reference man, not
# the cohort: the paper prints the cohort's ranges of age, BMI and height but
# not its weights or how many were men, which is not enough to place its
# typical patient.
#
# SAMPLING SITE.  Venous plasma.  stanpumpR prefers arterial
# pharmacokinetics (AGENTS.md; Shafer, 2026-11-26), and no arterial model of
# desmopressin exists; a venous model understates the early peak after a
# bolus.  The haemostatic dose is a 15-30 min infusion, which blunts that
# difference.
#
# EFFECT SITE.  Argenti D, Jensen BK, Heald D. The pharmacokinetics and
# pharmacodynamics of desmopressin: effect on plasma factor VIII:C and von
# Willebrand factor. Am J Ther 1997;4:3-8.  doi:10.1097/00045391-199701000-
# 00002, PMID 10423583.  28 healthy men aged 18-45 and weighing 60-90 kg,
# 0.3 mcg/kg over 15 min (the haemostatic dose), plasma desmopressin by
# Ferring's radioimmunoassay, vWF (ristocetin cofactor) and FVIII:C to 24 h;
# each subject's two-compartment model linked by a ke0 to a sigmoid Emax
# model with the baseline fixed at 100% (PCNONLIN).  Means (Tables 1 and 2):
#   vWF:    ke0 2.16 /h (CV 106%), EC50 237 pg/mL, Emax 367% of predose,
#           gamma 0.94
#   FVIII:C ke0 2.59 /h (CV 42.6%), EC50 314 pg/mL, Emax 388%, gamma 0.89
# The ke0 is vWF's, since the clinical use is platelet function and vWF is
# the factor that carries it; it is supplied directly, as glyburide's is
# (R/drugs_glyburide.R), rather than as a time to peak solved on Agerso's
# disposition, because a ke0 is the effect site's own rate and the two
# studies' dispositions differ.  [decision] vWF's ke0 rather than FVIII:C's,
# and supplied rather than solved from a tPeak: Shafer to confirm.  Both
# endpoints are drawn as % effect panels (inst/extdata/pdEffect.csv): vWF on
# this effect site, FVIII:C on an effect site of its own at its own ke0.
#
# THE TWO STUDIES DISAGREE ON CLEARANCE.  Argenti's means give V 9.65 L and
# k10 0.727 /h, a clearance of 7.0 L/h in young healthy men; Agerso's model
# gives about 10 L/h at the creatinine clearances of young healthy men and
# its healthy group measured 10 L/h.  Both used Ferring's radioimmunoassay.
# Only clearance is comparable across sources (AGENTS.md), so this is a
# finding: Argenti's EC50s were estimated on concentrations about 40% higher
# than this model predicts for the same dose, so on this model the effect is
# probably understated somewhat.
#
# DOSE BASIS.  Every US product is desmopressin acetate and is labelled in mcg
# of desmopressin acetate (DDAVP Injection 4 mcg/mL), and both studies dosed
# the acetate (Agerso: "2 mcg desmopressin acetate"; Argenti: the commercial
# 4 mcg/mL vial).  Doses are entered as labelled, with no conversion
# (AGENTS.md, Dose basis).  Concentrations are pg/mL, so amounts are in ng.
#
# NOT MODELLED.  Tachyphylaxis (the label: repeated more often than every
# 48 h the response lessens), the antidiuretic effect and hyponatraemia, the
# subcutaneous and intranasal routes, and the label's 20 mcg maximum.
#
# PREGNANCY.  Davison 1993 (PMID 8447444, read in full): metabolic clearance
# of desmopressin 0.18 L/min in late pregnancy against 0.15 at 10-12 weeks
# postpartum in the same six women, not significantly changed, while
# vasopressin's rose fivefold (vasopressinase, which desmopressin resists).
# Recorded as unchanged; no factor is applied.
# -----------------------------------------------------------------------------

#' Agerso 2004 desmopressin disposition (Table 1)
#'
#' Volumes in L, clearances in L/h; `clr` and `clnr` are at a creatinine
#' clearance of 60 mL/min/1.73 m^2.
#' @keywords internal
DESMOPRESSIN_AGERSO <- c(v1 = 13.9, v2 = 12.5, v3 = 11.7, q2 = 4.43, q3 = 27.5,
                         clr = 2.41, clnr = 3.91)

#' Agerso 2004's creatinine clearance terms: the reference (mL/min/1.73 m^2),
#' the fractional changes per mL/min, and the range the study spanned
#' @keywords internal
DESMOPRESSIN_CRCL_REFERENCE <- 60
#' @rdname DESMOPRESSIN_CRCL_REFERENCE
DESMOPRESSIN_CRCL_SLOPE_RENAL <- 0.0174
#' @rdname DESMOPRESSIN_CRCL_REFERENCE
DESMOPRESSIN_CRCL_SLOPE_NONRENAL <- 0.00933
#' @rdname DESMOPRESSIN_CRCL_REFERENCE
DESMOPRESSIN_CRCL_RANGE <- c(10, 144)

#' Argenti 1997's von Willebrand factor ke0, /h
#' @keywords internal
DESMOPRESSIN_KE0 <- 2.16

#' Desmopressin clearances at a creatinine clearance
#'
#' Agerso 2004's linear terms, the creatinine clearance held within the
#' range the study spanned.
#'
#' @param crcl creatinine clearance, mL/min/1.73 m^2
#' @return a named vector: renal and non-renal clearance in L/h
#' @keywords internal
desmopressinClearance <- function(crcl)
{
  crcl <- min(max(crcl, DESMOPRESSIN_CRCL_RANGE[1]), DESMOPRESSIN_CRCL_RANGE[2])
  d <- crcl - DESMOPRESSIN_CRCL_REFERENCE
  c(renal = DESMOPRESSIN_AGERSO[["clr"]] * (1 + DESMOPRESSIN_CRCL_SLOPE_RENAL * d),
    nonrenal = DESMOPRESSIN_AGERSO[["clnr"]] * (1 + DESMOPRESSIN_CRCL_SLOPE_NONRENAL * d))
}

#' Desmopressin, intravenous (Agerso 2004, Argenti 1997)
#'
#' Three compartments with renal and non-renal clearance that both fall with
#' the creatinine clearance (Agerso 2004), and the effect site of von
#' Willebrand factor release (Argenti 1997).  See the file header.
#'
#' @inheritParams ketamine
#' @param creatinine serum creatinine, mg/dL, or NULL for the assumed normal
#'   value for the patient's age and sex
#' @export
desmopressin <- function(weight, height, age, sex, adjustToFFM = TRUE,
                         creatinine = NULL)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume = 1, legacyClearance = 1)
  pkW  <- if (isTRUE(adjustToFFM)) size$pkWeight else weight
  scr  <- adultEquivalentCreatinine(creatinine, age, sex)
  crcl <- creatinineClearanceCG(pkW, age, sex, scr) * 1.73 /
    bsaGehanGeorge(pkW, height)                      # mL/min/1.73 m^2
  cl   <- desmopressinClearance(crcl)
  p    <- DESMOPRESSIN_AGERSO

  default <- list(
    v1  = p[["v1"]] * size$volume,
    v2  = p[["v2"]] * size$volume,
    v3  = p[["v3"]] * size$volume,
    cl1 = sum(cl) / 60 * size$clearance,
    cl2 = p[["q2"]] / 60 * size$clearance,
    cl3 = p[["q3"]] / 60 * size$clearance
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  list(
    PK = PK,
    ke0 = DESMOPRESSIN_KE0 / 60,
    MEAC = 0,
    typical = 0,
    upperTypical = 0,
    lowerTypical = 0,
    reference = paste0(
      "Agerso H et al., Br J Clin Pharmacol 2004;58:352-358 (three compartments, ",
      "renal and non-renal clearance linear in creatinine clearance; venous ",
      "plasma; doses as desmopressin acetate; a blank serum creatinine is the ",
      "assumed normal value for age and sex). https://doi.org/10.1111/",
      "j.1365-2125.2004.02175.x. Effect site: von Willebrand factor release, ",
      "ke0 2.16/h, Argenti D et al., Am J Ther 1997;4:3-8. ",
      "https://doi.org/10.1097/00045391-199701000-00002")
  )
}
