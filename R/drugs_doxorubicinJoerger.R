# -----------------------------------------------------------------------------
# Doxorubicin, intravenous (Joerger 2007): plasma doxorubicin, forming
# doxorubicinol
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min; amounts in
# mg and concentrations in mcg/mL (= mg/L) of doxorubicin base.
#
# Drafted by Claude Code, 2026-11-06, at the request of Steven L. Shafer
# (docs/chemotherapy.md, inst/extdata/chemotherapyRegistry.csv, source C08).
# Every number below was read from the PDF, which Shafer supplied
# (2026-11-06): Tables I and IV and Eqs. 10-11, on the page images.  The same
# paper is the library's cyclophosphamide model.
#
# THE SOURCE
# ==========
# Joerger M, Huitema ADR, Richel DJ, et al.  Population pharmacokinetics and
# pharmacodynamics of doxorubicin and cyclophosphamide in breast cancer
# patients: a study by the EORTC-PAMM-NDDG.  Clin Pharmacokinet
# 2007;46:1051-1068 (doi 10.2165/00003088-200746120-00005).  59 women with
# breast cancer (median age 56, BSA 1.81 m2, AST 24 U/L, Cockcroft-Gault 82
# mL/min), first cycle of doxorubicin 60 mg/m2 over 15 min; EDTA plasma
# before, at the end of the infusion and 0.5, 1.5 and 24 h after it; HPLC
# (LLQ 1.8-2.4 nmol/L); NONMEM, FOCE, in umol.
#
# THE MODEL (Table IV, Eqs. 10-11)
# ================================
# Doxorubicin in two compartments; all of its clearance forms doxorubicinol
# (one compartment), parameterised as V3/fm and CL_DOL/fm since the fraction
# fm is not identifiable:
#
#     CL_DOX = 47.6 L/h x (BSA/1.8)^1.4 x (AST/21)^-0.24 x (age/56)^-0.54
#     V1 = 12.3 L    Q = 60.3 L/h    V2 = 421 L
#     CL_DOL/fm = 108 L/h x (CLcr/80)^0.54    V3/fm = 1580 L
#
# The text's examples reproduce (BSA +0.2 m2 +16%, AST +20 -15%, age +10 y
# -9%, CLcr +20 mL/min +13%).  Derived here: half-lives of 4.7 min and 11.1 h,
# Vss 433 L; doxorubicinol's 10.1 h.  The 11-h terminal half-life is shorter
# than the 20-48 h usually reported, because sampling stopped at 24 h: the
# late phase is truncated, an artefact of the design (AGENTS.md, "only
# clearance is real").
#
# In this library's metabolite interface, kFormation is the parent's own
# k10 = CL/V1 (all of the clearance, fm-scaled, as for aripiprazole), and the
# doxorubicinol model (R/drugs_doxorubicinolJoerger.R) is V3/fm and CL_DOL/fm.  The
# two were fitted jointly (METABOLITE_FORMATION_BASIS "joint").
#
# DOSE BASIS
# ==========
# Doses are entered as labelled, mg of doxorubicin HYDROCHLORIDE; the curve is
# doxorubicin base.  The paper worked in umol and does not say which molar
# mass converted its doses, but its Table VII mean AUC (4.21 umol h/L) is
# reproduced with the hydrochloride (4.27, after the expected 6% from
# between-patient variability) and missed by 8% with the base (4.56).  So
# every volume and clearance is divided by the base fraction 543.52/579.98 =
# 0.9371 (DOXORUBICIN_BASE_FRACTION), as azilsartan's are: a dose of
# hydrochloride then gives the base concentration.  The help tables show the
# divided values (V1 13.1 L, CL 50.8 L/h at the reference).  Doxorubicinol
# (545.54 g/mol) forms molar-for-molar: mwRatio = 545.54/579.98 per mg of the
# hydrochloride dosed.
#
# WHAT IS HELD AT THE REFERENCE
# =============================
# Serum AST is read from the Patient Profile's Serum AST field (ast, U/L;
# blank means the equation's reference, 21 U/L, EPIRUBICIN-style: the field
# has existed since 2026-11-10 and was not wired to this model until Gemini's
# chemotherapy audit of 2026-11-23, F6).  AST 41 U/L lowers clearance by 15%.
# The paper prints the cohort's AST as median 24, IQR 15-38, and no range, so
# the term is held only within the field's own limits (MIN_AST-MAX_AST); far
# outside the IQR it is an extrapolation.  A raised bilirubin, the usual basis
# for reducing the dose, was not a covariate.  Age is a negative power, floored at
# 18 (ADULT_AGE_TERM_FLOOR; the youngest studied was 29).  The BSA formula is
# not named (Mosteller's is used).  The creatinine clearance of the
# metabolite's equation is doxorubicinol's (R/drugs_doxorubicinolJoerger.R).
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# BSA at the pharmacokinetic weight with the switch on and total weight with
# it off.  V1, Q and V2, with no covariate, belong to the cohort's typical
# patient, and with the switch on they are anchored at her
# (DOXORUBICIN_JOERGER_COHORT, below; Steven L. Shafer, 2026-11-19, after
# Carbon's second repository audit, A2-F4): scaled from the 70 kg reference
# man, as they were, Joerger's own typical woman received a V1 18% and a Q
# 14% below the published values.  A model with a size covariate on one
# parameter is not thereby exempt on the others.  With the switch off they
# are fixed.  Doxorubicinol is anchored at the same cohort.
#
# PREGNANCY (R/pregnancy.R)
# =========================
# Janssen JM, Van Calsteren K, Dorlo TPC, et al.  Population pharmacokinetics
# of docetaxel, paclitaxel, doxorubicin and epirubicin in pregnant women.  Clin
# Pharmacokinet 2021;60:775-784 (doi 10.1007/s40262-020-00961-4), PDF read
# 2026-11-10: 22 pregnant women (27 cycles, 15.0-36.3 weeks) and 66 nonpregnant
# patients, three compartments, one discrete pregnancy factor per parameter
# (Table 2): CL x 1.14 (95% CI 1.00-1.28), V1 x 1.08 (0.85-1.35), no other.
# Applied under `pregnant` on Shafer's decision (2026-11-10, the p value not
# being the question): clearance and the central volume map directly onto
# Joerger's; Q and V2 unchanged.  Doxorubicinol's formation follows the
# parent's clearance, as always.
# NO EFFECT SITE, NO BAND
# -----------------------------------------------------------------------------

# Janssen 2021, Table 2: doxorubicin's pregnancy factors (shared by every
# adult doxorubicin model; R/drugs_doxorubicinPerezBlanco.R reads them too)
DOXORUBICIN_PREGNANCY <- list(cl = 1.14, v1 = 1.08)

# Joerger 2007, Table IV and Eq. 10
DOXORUBICIN_JOERGER <- list(
  cl = 47.6,            # L/h at BSA 1.8 m2, AST 21 U/L, age 56
  bsaReference = 1.8, bsaExponent = 1.4,
  astReference = 21, astExponent = -0.24,  # U/L; the Patient Profile's Serum AST
  ageReference = 56, ageExponent = -0.54,
  v1 = 12.3,            # L
  q = 60.3,             # L/h
  v2 = 421              # L
)

# Doxorubicin base over the hydrochloride (543.52 / 579.98 g/mol)
DOXORUBICIN_BASE_FRACTION <- 543.52 / 579.98
# Doxorubicinol over doxorubicin hydrochloride (545.54 / 579.98 g/mol)
DOXORUBICINOL_PER_HYDROCHLORIDE <- 545.54 / 579.98

# Joerger 2007, Table I (printed p. 1058), as read on the PDF 2026-11-18 for
# cyclophosphamideJoerger (the same 59 women): medians 70 kg, BSA 1.81 m2, 56
# years; the height back-solved from the BSA by Du Bois's formula (170.0 cm).
DOXORUBICIN_JOERGER_COHORT <- list(weight = 70, height = 170.0, age = 56, men = 0, women = 59)

#' Doxorubicin, intravenous (Joerger 2007)
#'
#' Two compartments, forming doxorubicinol; body surface area and age on
#' clearance, and the Patient Profile's Serum AST.  See the header of
#' \code{R/drugs_doxorubicinJoerger.R}.
#'
#' @inheritParams cefazolin
#' @param ast serum AST in U/L, or NULL for the equation's reference, 21 U/L
#' @param pregnant TRUE in late pregnancy: Janssen 2021's factors
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
doxorubicinJoerger <- function(weight, height, age, sex, adjustToFFM = TRUE,
                               ast = NULL, pregnant = FALSE)
{
  preg <- if (isTRUE(pregnant)) DOXORUBICIN_PREGNANCY else list(cl = 1, v1 = 1)
  p <- DOXORUBICIN_JOERGER
  f <- DOXORUBICIN_BASE_FRACTION
  # V1, Q and V2, with no covariate, are anchored at Joerger's 59 women
  # (DOXORUBICIN_JOERGER_COHORT); the clearance carries its own BSA term
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume = 1, legacyClearance = 1,
                        cohort = do.call(cohortSizeFactors, DOXORUBICIN_JOERGER_COHORT))
  w <- if (isTRUE(adjustToFFM)) size$pkWeight else weight
  bsa <- bsaMosteller(w, height)
  ageTerm <- max(age, ADULT_AGE_TERM_FLOOR)
  astValue <- if (is.null(ast)) p$astReference else min(max(ast, MIN_AST), MAX_AST)
  cl <- p$cl * (bsa / p$bsaReference)^p$bsaExponent *
    (astValue / p$astReference)^p$astExponent *
    (ageTerm / p$ageReference)^p$ageExponent                         # L/h

  v1 <- p$v1 * size$volume / f * preg$v1
  cl1 <- cl / 60 / f * preg$cl
  default <- list(
    v1  = v1,
    v2  = p$v2 * size$volume / f,
    v3  = 1,                                 # placeholder: two compartments
    cl1 = cl1,
    cl2 = p$q / 60 * size$clearance / f,
    cl3 = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Joerger M, Huitema ADR, Richel DJ, et al. Population pharmacokinetics and ",
    "pharmacodynamics of doxorubicin and cyclophosphamide in breast cancer patients: ",
    "a study by the EORTC-PAMM-NDDG. Clin Pharmacokinet 2007;46:1051-1068. 59 women: ",
    "two compartments, CL 47.6 L/h x (BSA/1.8)^1.4 x (age/56)^-0.54 (Mosteller) x (AST/21 U/L)^-0.24 (",
    "the Serum AST field; 21 U/L when blank), V1 12.3 L, Q 60.3 L/h, V2 421 L, all clearance forming ",
    "doxorubicinol. Doses as the hydrochloride; plasma doxorubicin base. ",
    "https://doi.org/10.2165/00003088-200746120-00005"
  )
  if (isTRUE(pregnant))
    reference <- paste(reference, pregnancyReference("CL x 1.14, V1 x 1.08", "Janssen 2021, Clin Pharmacokinet 60:775-784, https://doi.org/10.1007/s40262-020-00961-4"))

  list(
    PK = PK,
    tPeak = 0,
    MEAC = 0,
    typical = 0,
    upperTypical = 0,
    lowerTypical = 0,
    reference = reference,
    metabolite = list(
      name              = "doxorubicinolJoerger",
      kFormation        = cl1 / v1,          # all clearance, fm-scaled
      firstPassFraction = 0,
      mwRatio           = DOXORUBICINOL_PER_HYDROCHLORIDE   # per mg dosed
    )
  )
}
