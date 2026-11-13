# -----------------------------------------------------------------------------
# Doxorubicin, intravenous, in adults with non-Hodgkin lymphoma (Perez-Blanco
# 2016): plasma doxorubicin, forming doxorubicinol
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min; amounts in
# mcg and concentrations in ng/mL (doses entered in mg, as labelled).
#
# Drafted by Claude Code, 2026-11-09, at the request of Steven L. Shafer, who
# chose (2026-11-09) to add it as a third doxorubicin model beside Joerger
# 2007 (doxorubicin) and Voller 2015 (doxorubicinPediatric).  The parameters
# were read from the PubMed Central text (Tables 1-3); the PDF is still to
# confirm them.
#
# THE SOURCE
# ==========
# Perez-Blanco JS, Santos-Buelga D, Fernandez de Gatta MM, Hernandez-Rivas JM,
# Martin A, Garcia MJ.  Population pharmacokinetics of doxorubicin and
# doxorubicinol in patients diagnosed with non-Hodgkin's lymphoma.  Br J Clin
# Pharmacol 2016;82:1517-1527 (doi 10.1111/bcp.13070).  45 adults on R-CHOP
# (44 analysed), 26-84 years, 43-110 kg (mean 71, BSA 1.8 m2); 50 mg/m2 over
# 0.5 h (mean 89 mg); plasma at 0, 30, 90 and 180 min after the infusion,
# UHPLC-fluorescence (LLOQ 8 and 3 ng/mL); NONMEM 7.3, FOCE.
#
# THE MODEL (Table 2)
# ===================
# Five linear compartments, no covariates (bilirubin and AST were tested):
#
#   doxorubicin   CL 62.4 L/h, V1 17.7 L, Q2 50.7 L/h, V2 1830 L,
#                 Q3 28.4 L/h, V3 71 L
#   doxorubicinol a fraction Fm 0.22 of doxorubicin's clearance forms it;
#                 CLm 26.8 L/h, V4 79.8 L, Q5 424 L/h, V5 653 L
#
# V1, V2, V3, V4 and V5 were FIXED: the doxorubicin volumes "to those
# proposed by Kontny et al." (cited to the wrong reference number: the
# paper's ref 26 is Rudek 2004, Kontny 2013 is its ref 15), the doxorubicinol
# volumes from a sensitivity analysis.  With sampling stopping 3 h after the
# infusion, the 45.8-h terminal phase and the 1830-L V2 are the fixed
# literature volumes', not these data's.
#
# Checks: half-lives 5.1 min, 2.17 h and 45.8 h (doxorubicinol 0.11 and
# 19.9 h); the paper prints none.  AUC = dose/CL = 89/62.4 = 1.43 mg.h/L
# (Table 3's group means 1.44-1.78), and doxorubicinol's Fm x dose/CLm =
# 0.73 (Table 3 0.63-1.08), computed as the Methods do.  Table 3 labels the
# total AUC "ng/mL", a misprint: its 2.14-2.62 are mg.h/L, AUC + AUCm.
#
# In the library's metabolite interface kFormation is Fm x CL/V1, inside the
# parent's clearance, and doxorubicinolPerezBlanco carries V4, CLm, Q5 and V5
# as fitted (jointly: METABOLITE_FORMATION_BASIS "joint").  It is its own
# metabolite drug, not the library's doxorubicinol (Joerger), whose
# parameters are scaled by its own fm.
#
# DOSE BASIS
# ==========
# Doses are entered in mg as labelled (doxorubicin hydrochloride); the paper
# states neither salt nor base and works in mass throughout (its AUCs are the
# mg dose over the clearance, its metabolite AUC Fm x dose / CLm with no
# molar conversion), so no conversion is applied and doxorubicinol forms mass
# for mass (mwRatio 1), as the paper's own arithmetic does.  Joerger's
# doxorubicin, by contrast, is converted to base because its molar AUCs
# require it (R/drugs_doxorubicinJoerger.R).
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# No size covariate: the parameters are taken as the reference adult's (the
# cohort's mean weight was 71 kg), scaled by the library's factors with the
# switch on and fixed with it off.
#
# PREGNANCY (R/pregnancy.R): Janssen 2021's doxorubicin factors (CL x 1.14,
# V1 x 1.08; DOXORUBICIN_PREGNANCY, R/drugs_doxorubicinJoerger.R) under
# `pregnant`, as for Joerger 2007.
# NO EFFECT SITE, NO BAND
# -----------------------------------------------------------------------------

# Perez-Blanco 2016, Table 2 (L/h, L)
DOXORUBICIN_PEREZ_BLANCO <- list(
  cl = 62.4, v1 = 17.7, q2 = 50.7, v2 = 1830, q3 = 28.4, v3 = 71,
  fm = 0.22, clm = 26.8, v4 = 79.8, q5 = 424, v5 = 653
)

#' Doxorubicin, intravenous, in adults (Perez-Blanco 2016)
#'
#' Three compartments forming two-compartment doxorubicinol
#' (\code{doxorubicinolPerezBlanco}); see the header of
#' \code{R/drugs_doxorubicinPerezBlanco.R}.
#'
#' @inheritParams cefazolin
#' @param pregnant TRUE in late pregnancy: Janssen 2021's factors
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
doxorubicinPerezBlanco <- function(weight, height, age, sex, adjustToFFM = TRUE,
                                   pregnant = FALSE)
{
  p <- DOXORUBICIN_PEREZ_BLANCO
  preg <- if (isTRUE(pregnant)) DOXORUBICIN_PREGNANCY else list(cl = 1, v1 = 1)
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume = 1, legacyClearance = 1)
  v1 <- p$v1 * size$volume * preg$v1
  cl1 <- p$cl / 60 * size$clearance * preg$cl

  default <- list(
    v1  = v1,
    v2  = p$v2 * size$volume,
    v3  = p$v3 * size$volume,
    cl1 = cl1,
    cl2 = p$q2 / 60 * size$clearance,
    cl3 = p$q3 / 60 * size$clearance
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Perez-Blanco JS, Santos-Buelga D, Fernandez de Gatta MM, et al. Population ",
    "pharmacokinetics of doxorubicin and doxorubicinol in patients diagnosed with ",
    "non-Hodgkin's lymphoma. Br J Clin Pharmacol 2016;82:1517-1527. 44 adults: three ",
    "compartments, CL 62.4 L/h, V1 17.7, V2 1830, V3 71 L (fixed, from Kontny 2013), ",
    "Q2 50.7, Q3 28.4 L/h; 22% of the clearance forms doxorubicinol. Doses as labelled, ",
    "no conversion; plasma doxorubicin in ng/mL. https://doi.org/10.1111/bcp.13070"
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
      name              = "doxorubicinolPerezBlanco",
      kFormation        = p$fm * cl1 / v1,
      firstPassFraction = 0,
      mwRatio           = 1
    )
  )
}

#' Doxorubicinol formed from doxorubicin (Perez-Blanco 2016)
#'
#' Two compartments; formed by \code{doxorubicinPerezBlanco()}.  See the
#' header of \code{R/drugs_doxorubicinPerezBlanco.R}.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
doxorubicinolPerezBlanco <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  p <- DOXORUBICIN_PEREZ_BLANCO
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume = 1, legacyClearance = 1)
  default <- list(
    v1  = p$v4 * size$volume,
    v2  = p$v5 * size$volume,
    v3  = 1,                                 # placeholder: two compartments
    cl1 = p$clm / 60 * size$clearance,
    cl2 = p$q5 / 60 * size$clearance,
    cl3 = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  list(
    PK = PK,
    tPeak = 0,
    MEAC = 0,
    typical = 0,
    upperTypical = 0,
    lowerTypical = 0,
    reference = paste0(
      "Perez-Blanco JS, Santos-Buelga D, Fernandez de Gatta MM, et al. Br J Clin ",
      "Pharmacol 2016;82:1517-1527 (doxorubicinol formed from doxorubicin: two ",
      "compartments, CLm 26.8 L/h, V4 79.8 L, Q5 424 L/h, V5 653 L). ",
      "https://doi.org/10.1111/bcp.13070")
  )
}
