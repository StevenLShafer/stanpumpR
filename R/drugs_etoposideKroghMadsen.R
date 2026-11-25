# -----------------------------------------------------------------------------
# Etoposide, intravenous, in adults (Krogh-Madsen 2012): plasma etoposide
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min; amounts in
# mg and concentrations in mcg/mL (= mg/L).
#
# Drafted by Claude Code, 2026-11-06, at the request of Steven L. Shafer
# (docs/chemotherapy.md, inst/extdata/chemotherapyRegistry.csv, source C22).
# Every number below was read from the PDF, which Shafer supplied
# (2026-11-06): Tables 1 and 2 and the equations, on the page images.  The
# children's model is etoposidePediatric (Urien 2011).
#
# THE SOURCE
# ==========
# Krogh-Madsen M, Bender B, Jensen MK, Nielsen OJ, Friberg LE, Honore PH.
# Population pharmacokinetics of cytarabine, etoposide, and daunorubicin in
# the treatment for acute myeloid leukemia.  Cancer Chemother Pharmacol
# 2012;69:1155-1163 (doi 10.1007/s00280-011-1800-z).  23 Danish adults with
# AML (14 men), 22-68 years, 46-117 kg, Cockcroft-Gault 47-189 mL/min;
# etoposide 100 mg/m2 over 1 h on days 1-5 (ADE); 8-10 central venous plasma
# samples per series; HPLC with fluorescence, LLOQ 52.5 ng/mL; NONMEM.
#
# THE MODEL (Table 2, bootstrap estimates)
# ========================================
# Two compartments, linear:
#
#     CL = 0.99 + 0.00996 x CLCR  L/h  (x a baseline white-count term)
#     Vc = 9.13 L (x (1 - 0.2793) in women)    Q = 1.14 L/h    Vp = 4.75 L
#
# CLCR is Cockcroft-Gault in mL/min (creatinine in umol/L; no cap; the weight
# not stated, total weight used).  Checks: a man at CLCR 116 has CL 2.145
# L/h, 54% renal (the paper's 54%), Vss 13.9 L and half-lives of 86 min and
# 5.9 h (5.4 h in a woman).
#
# A DISCREPANCY RECORDED, NOT RESOLVED
# ====================================
# Table 2's renal slope does not reproduce the paper's own text and Figure 4:
# 0.00996 x 116 = 1.155 L/h against the footnote's 1.17; the slope gives
# -26% and +20% clearance at 60 and 160 mL/min where the text says -30% and
# +24% (the abstract 32%); and Figure 4's three AUCs fit only a slope of about
# 0.0133.  The table, which is the fitted model, is used.  Likewise the text
# calls women's Vc "32% lower", where the table's -0.2793 gives 27.9%.
#
# WHAT IS HELD AT THE REFERENCE
# =============================
# The baseline white cell count, on clearance (+0.185% per 10^6/mL above the
# unprinted median), which the Patient Profile does not carry: held at the
# median, factor 1.  Age, weight, BSA and creatinine itself were not
# retained.  Unbound etoposide is not modelled.
#
# RENAL FUNCTION
# ==============
# The Patient Profile's serum creatinine through adultEquivalentCreatinine()
# (an adult equation), or the library's assumed normal creatinine when blank.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# The Cockcroft-Gault weight is the pharmacokinetic weight with the switch on
# and total weight with it off, and the renal clearance it gives is not
# anchored further (Shafer, 2026-11-18: that would correct it twice).  The
# non-renal clearance, the volumes and Q, with no size covariate, are the
# cohort's typical patient's and are anchored at that patient with the switch
# on (ETOPOSIDE_KM_COHORT; Steven L. Shafer, 2026-11-18; factors 0.993 and
# 0.995), except Vc, which carries the source's sex term (women x 0.7207) and
# so is anchored at the cohort's patient of the patient's own sex (A2-F1).  With the
# switch off they are fixed.
#
# PREGNANCY (R/pregnancy.R)
# =========================
# No pregnancy pharmacokinetics found.  About half the clearance is renal, so
# the creatinine clearance takes the rise in filtration of pregnancy (x 1.5,
# pregnancyGfrFactor(), only when the creatinine is blank): physiological,
# an assumption.
#
# NO EFFECT SITE, NO BAND
# -----------------------------------------------------------------------------

# Krogh-Madsen 2012, Table 2 (etoposide)
ETOPOSIDE_KROGHMADSEN <- list(
  clNonRenal = 0.99,      # L/h
  clRenalSlope = 0.00996, # L/h per mL/min of Cockcroft-Gault
  v1 = 9.13,              # L, men
  v1Female = -0.2793,     # fractional change in women
  q = 1.14,               # L/h
  v2 = 4.75               # L
)

# Krogh-Madsen 2012, Table 1 (printed p. 1157), read on the PDF 2026-11-18:
# means 79.4 kg, BSA 1.91 m2, 52.3 years; 14 men and 9 women analysed (p. 1156).
# No height is printed; it is back-solved from the BSA by Du Bois's formula,
# the paper naming none (170.1 cm; Shafer, 2026-11-18).  The size-free
# parameters are anchored at this cohort (Steven L. Shafer, 2026-11-18;
# docs/weight-adjustment.md, Anchoring at the source's cohort); Vc, whose
# published value is a man's (women x 0.7207), at the cohort's man; the renal
# clearance not at all, its Cockcroft-Gault weight carrying the size.
# The same 23 patients gave the cytarabine model, which shares this cohort
# (R/drugs_cytarabine.R).
ETOPOSIDE_KM_COHORT <- list(weight = 79.4, height = 170.1, age = 52.3, men = 14, women = 9)

#' Etoposide, intravenous, in adults (Krogh-Madsen 2012)
#'
#' Two compartments; a Cockcroft-Gault renal term on clearance and sex on the
#' central volume.  See the header of \code{R/drugs_etoposideKroghMadsen.R}.
#'
#' @inheritParams cefazolin
#' @param creatinine serum creatinine, mg/dL, or NULL for an assumed normal
#'   value
#' @param pregnant is the patient pregnant?  \code{TRUE}, with the creatinine
#'   blank, raises the creatinine clearance by the rise in filtration of
#'   pregnancy
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
etoposideKroghMadsen <- function(weight, height, age, sex, adjustToFFM = TRUE,
                      creatinine = NULL, pregnant = FALSE)
{
  p <- ETOPOSIDE_KROGHMADSEN
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume = 1, legacyClearance = 1, cohort = do.call(cohortSizeFactors, ETOPOSIDE_KM_COHORT))
  # Vc carries the source's sex term (a man's value, x 0.7207 in women), so it
  # is anchored at the cohort's patient of the patient's own sex rather than
  # at its sex-weighted patient: anchored at the cohort's man for both sexes,
  # the cohort's woman received 5.42 L for the published 6.58 (Carbon's second
  # repository audit, A2-F1; the first version also called modifyList()
  # unqualified, A2-F22).
  vcSex <- 1
  if (isTRUE(adjustToFFM))
    vcSex <- do.call(cohortSizeFactors, ETOPOSIDE_KM_COHORT)$volume /
      cohortSexSizeFactors(ETOPOSIDE_KM_COHORT, sex)$volume
  w <- if (isTRUE(adjustToFFM)) size$pkWeight else weight
  scr <- adultEquivalentCreatinine(creatinine, age, sex)
  gfrPregnancy <- pregnancyGfrFactor(pregnant, creatinine)
  clcr <- creatinineClearanceCG(w, age, sex, scr) * gfrPregnancy      # mL/min

  cl <- p$clNonRenal * size$clearance + p$clRenalSlope * clcr          # L/h
  v1 <- p$v1 * (if (sex == SEX_FEMALE) 1 + p$v1Female else 1)

  default <- list(
    v1  = v1 * size$volume * vcSex,
    v2  = p$v2 * size$volume,
    v3  = 1,                                 # placeholder: two compartments
    cl1 = cl / 60,
    cl2 = p$q / 60 * size$clearance,
    cl3 = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Krogh-Madsen M, Bender B, Jensen MK, Nielsen OJ, Friberg LE, Honore PH. ",
    "Population pharmacokinetics of cytarabine, etoposide, and daunorubicin in the ",
    "treatment for acute myeloid leukemia. Cancer Chemother Pharmacol ",
    "2012;69:1155-1163. 23 adults: two compartments, CL 0.99 + 0.00996 x CLCR L/h ",
    "(Cockcroft-Gault from the entered or an assumed normal creatinine; white count ",
    "held at the median), Vc 9.13 L (27.9% lower in women), Q 1.14 L/h, Vp 4.75 L. ",
    "Plasma etoposide",
    if (gfrPregnancy != 1) paste0("; ", pregnancyReference(
      "creatinine clearance x 1.5, the rise in filtration of pregnancy")) else "",
    ". https://doi.org/10.1007/s00280-011-1800-z"
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
