# -----------------------------------------------------------------------------
# Epirubicin, intravenous (Ralph 2003): plasma epirubicin, clearance set by
# the serum AST
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min; amounts in
# mcg and concentrations in ng/mL (doses entered in mg).
#
# Drafted by Claude Code, 2026-11-10, at the request of Steven L. Shafer, who
# decided (2026-11-10) to add a Serum AST field to the Patient Profile for
# this model.  Every number below was read on the pages of the PDF, which
# Shafer supplied (2026-11-10; papers archive PMID_12764671).
#
# THE SOURCE
# ==========
# Ralph LD, Thomson AH, Dobbs NA, Twelves C.  A population model of
# epirubicin pharmacokinetics and application to dosage guidelines.  Cancer
# Chemother Pharmacol 2003;52:34-40 (doi 10.1007/s00280-003-0608-x).  109
# women with advanced breast cancer (105 modelled, 4 lacking covariates), 72
# with liver metastases; 35-79 years, 37-89 kg, BSA 1.25-2.2 m2; AST 7-815
# IU/L (median 90), bilirubin 1-282 umol/L; epirubicin 12.5-120 mg/m2 alone,
# a rapid infusion (median 3 min); first cycle, 8-18 samples to 48 h; HPLC;
# NONMEM V, FOCE-INTER.
#
# THE MODEL (Table 3, final)
# ==========================
# Three compartments:
#
#     CL = 72.9 x (1 - 0.135 ln AST) L/h      (AST in IU/L)
#     V1 10.3 L   Q2 30.2 L/h   V2 35.7 L   Q3 61.5 L/h   V3 772 L
#
# The same values Ralph 2006 (Br J Clin Pharmacol 62:47-55) reprints in its
# Table 4; that paper's Table 2 (74.9, 0.139) is not this paper's model.  The
# AST explained about 10% of the variability in clearance; dose, liver
# metastases, bilirubin, BSA and weight each explained less than 2% and were
# dropped ("BSA does not have an identifiable effect").  The basic model,
# without AST, has CL 29.0 L/h.
#
# THE AST GUARD.  The equation gives zero clearance at AST = exp(1/0.135) =
# 1648 IU/L and a negative one above; the authors set no limit.  The AST is
# held within the 7-815 IU/L the study covered (EPIRUBICIN_AST_RANGE), so
# clearance runs from 53.7 L/h (AST 7) to 6.9 L/h (AST 815), the latter near
# the study's smallest individual clearance (4 L/h).  Left blank, the AST is a
# normal one, 20 IU/L (EPIRUBICIN_REFERENCE_AST), giving 43.4 L/h: the study
# had no reference value of its own (its median, 90, reflects its liver
# metastases).
#
# CAUTIONS.  Women only; first cycle; samples to 48 h, so the 772-L terminal
# volume is poorly identified (Ralph 2004 found the terminal half-life in the
# data at most 72 h, where its simulation gave 280 h).  Ralph 2006's external
# validation, in 79 Uppsala patients given FEC, over-predicted concentrations
# by 81% and clearance's imprecision was 43%.  No metabolite (epirubicinol).
#
# DOSE BASIS
# ==========
# The paper does not say whether its doses were epirubicin hydrochloride (the
# approved product) or base.  By Shafer's rule (2026-11-10: absent a statement
# in the paper, the parameters are in the units of the FDA-approved product,
# here the salt), doses are entered as labelled, epirubicin hydrochloride,
# with no conversion.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# No size covariate: the volumes and clearances scale by the library's factors
# with the switch on and are fixed with it off.
#
# PREGNANCY (R/pregnancy.R)
# =========================
# van Hasselt JGC, van Calsteren K, Heyns L, et al.  Ann Oncol 2014;25:2059-
# 2065 (doi 10.1093/annonc/mdu140), PDF supplied by Shafer 2026-11-10: a
# three-compartment model of epirubicin pooled from 57 nonpregnant patients
# and 10 pregnant women (20 cycles, 19-36.9 gestational weeks, 100 mg/m2 over
# 30 min), with one discrete pregnancy factor per parameter (Table 2):
#
#     CL x 1.10 (RSE 9%)   V1 x 1.55 (13%)   V2 x 2.08 (25%)   V3 x 1 (fixed)
#
# its V2 (10 L, Q 14.5 L/h) the shallow peripheral compartment and its V3
# (1110 L, Q 70.7 L/h) the deep one, as Ralph's V2 (35.7 L, Q 30.2) and V3
# (772 L, Q 61.5) are; so the factors are carried onto Ralph's compartments by
# that correspondence, the intercompartmental clearances unchanged (no factor
# was estimated on them).  The typical AUC falls 8.2% (the paper's Table 3).
# Applied under `pregnant`, for the third trimester (the factor was fitted as
# one effect over 19-36.9 weeks).
# NO EFFECT SITE, NO BAND
# -----------------------------------------------------------------------------

# Ralph 2003, Table 3 (final model)
EPIRUBICIN_RALPH <- list(
  cl = 72.9, astSlope = 0.135,   # CL = cl x (1 - astSlope ln AST), L/h
  v1 = 10.3, q2 = 30.2, v2 = 35.7, q3 = 61.5, v3 = 772
)
EPIRUBICIN_AST_RANGE <- c(7, 815)    # IU/L, the range studied
EPIRUBICIN_REFERENCE_AST <- 20       # IU/L, a normal AST, for a blank field
# van Hasselt 2014, Table 2: pregnancy factors on CL, V1 and the shallow V2
EPIRUBICIN_PREGNANCY <- list(cl = 1.10, v1 = 1.55, v2 = 2.08)

#' Epirubicin clearance from the AST (Ralph 2003)
#'
#' @param ast serum AST in IU/L, or NULL for the reference (a normal AST)
#' @returns clearance in L/h at the reference size
#' @keywords internal
epirubicinClearance <- function(ast = NULL)
{
  p <- EPIRUBICIN_RALPH
  a <- if (is.null(ast)) EPIRUBICIN_REFERENCE_AST else ast
  a <- min(max(a, EPIRUBICIN_AST_RANGE[1]), EPIRUBICIN_AST_RANGE[2])
  p$cl * (1 - p$astSlope * log(a))
}

#' Epirubicin, intravenous (Ralph 2003)
#'
#' Three compartments, clearance set by the serum AST; see the header of
#' \code{R/drugs_epirubicin.R}.
#'
#' @inheritParams cefazolin
#' @param ast serum aspartate aminotransferase in IU/L, or NULL for a normal
#'   AST (20 IU/L); held within the 7-815 IU/L the study covered
#' @param pregnant TRUE in late pregnancy: van Hasselt 2014's factors
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
epirubicin <- function(weight, height, age, sex, adjustToFFM = TRUE, ast = NULL,
                       pregnant = FALSE)
{
  p <- EPIRUBICIN_RALPH
  preg <- if (isTRUE(pregnant)) EPIRUBICIN_PREGNANCY else list(cl = 1, v1 = 1, v2 = 1)
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume = 1, legacyClearance = 1)

  default <- list(
    v1  = p$v1 * size$volume * preg$v1,
    v2  = p$v2 * size$volume * preg$v2,
    v3  = p$v3 * size$volume,
    cl1 = epirubicinClearance(ast) / 60 * size$clearance * preg$cl,
    cl2 = p$q2 / 60 * size$clearance,
    cl3 = p$q3 / 60 * size$clearance
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Ralph LD, Thomson AH, Dobbs NA, Twelves C. A population model of epirubicin ",
    "pharmacokinetics and application to dosage guidelines. Cancer Chemother Pharmacol ",
    "2003;52:34-40. 105 women with breast cancer: three compartments, CL 72.9 x (1 - ",
    "0.135 ln AST) L/h (AST held at 7-815 IU/L; a normal 20 IU/L if not entered), V1 ",
    "10.3, Q2 30.2, V2 35.7, Q3 61.5 L/h, V3 772 L. ",
    "https://doi.org/10.1007/s00280-003-0608-x"
  )
  if (isTRUE(pregnant))
    reference <- paste(reference, pregnancyReference(
      "CL x 1.10, V1 x 1.55, V2 x 2.08",
      "van Hasselt 2014, Ann Oncol 25:2059-2065, https://doi.org/10.1093/annonc/mdu140"))

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
