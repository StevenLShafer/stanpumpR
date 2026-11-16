# -----------------------------------------------------------------------------
# Doxorubicin, intravenous, in children (Voller 2015): plasma doxorubicin,
# forming doxorubicinol
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min; amounts in
# mcg and concentrations in ng/mL (= ug/L; doses entered in mg).
#
# Drafted by Claude Code, 2026-11-06, at the request of Steven L. Shafer
# (docs/chemotherapy.md, inst/extdata/chemotherapyRegistry.csv, source C39).
# Every number below was read from the PDF, which Shafer supplied
# (2026-11-06): Tables 1 and 2, Figure 2, and Eq. 1 on the page image.  The
# adults' model is doxorubicin (Joerger 2007).
#
# THE SOURCE
# ==========
# Voller S, Boos J, Krischke M, Wurthwein G, Kontny NE, Boddy AV, Hempel G.
# Age-dependent pharmacokinetics of doxorubicin in children with cancer.  Clin
# Pharmacokinet 2015;54:1139-1149 (doi 10.1007/s40262-015-0272-4).  94
# children (48 girls), 0.2-17.7 years (median 5.32), BSA 0.23-2.05 m2 (median
# 0.77), 3.6-88.1 kg; infusions of 0.25-24 h; two administrations each, 5-8
# samples; HPLC with fluorescence, 2-1000 ug/L; NONMEM, FOCE+I.
#
# THE MODEL (Table 2, Eq. 1, Figure 2)
# ====================================
# Doxorubicin in three compartments, half of its clearance forming
# doxorubicinol (fM fixed at 0.5 from published AUC ratios), one compartment.
# Table 2 gives each parameter per m2 at the median child (BSA 0.77 m2, age
# 5.32 y); every parameter scales LINEARLY on BSA about the median, with an
# intercept, and clearance also carries a power of age inside "1 +":
#
#     f_BSA = 1 + (BSA - 0.77) x 1.30
#     CL  = 9.2785 L/h x f_BSA x (1 + (age/5.32)^0.286)    (24.1 L/h/m2 x 0.77 / 2)
#     V1 = 9.34, V2 = 560, V3 = 27.8 L/m2;  Q2 = 26.8, Q3 = 12.1 L/h/m2
#     doxorubicinol: CLM = 42.5 L/h/m2, VM = 760 L/m2
#     (each x 0.77 x f_BSA)
#
# Eq. 1 reproduces the paper's three printed clearances: 24.1 (the median
# child), 12.0 (a newborn of 0.215 m2) and 29.1 L/h/m2 (an 18-year-old of 1.8
# m2, 52.4 L/h).  Q2/V2 is the large peripheral compartment.  Derived here:
# half-lives 0.10, 1.96 and 31.2 h, Vss 597 L/m2; doxorubicinol's 12.4 h.
# The form of the BSA scaling is printed only for CL; the text says all
# parameters scale linearly on BSA with the one factor 1.30, which is used.
# Table 2's bootstrap CL (11.7) matches none of the model's scales and is
# not used.
#
# In this library's metabolite interface kFormation = fM x CL / V1, and the
# metabolite is doxorubicinolPediatric (R/drugs_doxorubicinolPediatric.R),
# fitted jointly (METABOLITE_FORMATION_BASIS "joint"), with mwRatio 1: the
# paper prints no molar conversion (the molar masses differ by 0.4%).
#
# The BSA formula is in the electronic supplement, not read: Mosteller's is
# used.  No sex, renal, hepatic or genotype effect was found.
#
# DOSE BASIS
# ==========
# Not stated; doses are entered as labelled (mg of doxorubicin
# hydrochloride) with no conversion, unlike the adults' model, whose source
# lets the basis be checked.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# The model's own BSA covariate, from the pharmacokinetic weight with the
# switch on and total weight with it off.  Fitted in children; an adult is an
# extrapolation (use doxorubicin).
#
# PREGNANCY: not applicable (a children's model).
# NO EFFECT SITE, NO BAND
# -----------------------------------------------------------------------------

# Voller 2015, Table 2: per m2 at the median child (BSA 0.77 m2, age 5.32 y)
DOXORUBICIN_VOLLER <- list(
  bsaMedian = 0.77, bsaSlope = 1.30,
  ageMedian = 5.32, ageExponent = 0.286,
  cl = 24.1,                     # L/h/m2 at the median child
  v1 = 9.34, q2 = 26.8, v2 = 560, q3 = 12.1, v3 = 27.8,   # L/m2, L/h/m2
  fm = 0.5,                      # fixed in the source
  clm = 42.5, vm = 760           # doxorubicinol, L/h/m2, L/m2
)

# BSA and its linear scaling factor, shared with doxorubicinolPediatric
doxorubicinVollerScale <- function(weight, height, age, sex, adjustToFFM)
{
  p <- DOXORUBICIN_VOLLER
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume = 1, legacyClearance = 1)
  w <- if (isTRUE(adjustToFFM)) size$pkWeight else weight
  bsa <- bsaMosteller(w, height)
  # absolute value at the median child (per m2 x 0.77) times the factor,
  # which stays positive for any BSA above 0.0008 m2
  p$bsaMedian * max(1 + (bsa - p$bsaMedian) * p$bsaSlope, 0.01)
}

#' Doxorubicin, intravenous, in children (Voller 2015)
#'
#' Three compartments, forming doxorubicinol; body surface area on every
#' parameter and a power of age on clearance.  See the header of
#' \code{R/drugs_doxorubicinPediatric.R}.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
doxorubicinPediatric <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  p <- DOXORUBICIN_VOLLER
  s <- doxorubicinVollerScale(weight, height, age, sex, adjustToFFM)  # m2-equivalent
  ageFactor <- (1 + (max(age, 0) / p$ageMedian)^p$ageExponent) / 2  # 1 at the median
  v1 <- p$v1 * s
  cl1 <- p$cl * s * ageFactor / 60

  default <- list(
    v1  = v1,
    v2  = p$v2 * s,
    v3  = p$v3 * s,
    cl1 = cl1,
    cl2 = p$q2 * s / 60,
    cl3 = p$q3 * s / 60
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Voller S, Boos J, Krischke M, et al. Age-dependent pharmacokinetics of ",
    "doxorubicin in children with cancer. Clin Pharmacokinet 2015;54:1139-1149. 94 ",
    "children: three compartments, per m2 at the median child (BSA 0.77 m2, age 5.32 ",
    "y) CL 24.1 L/h, V1 9.34 L, Q2 26.8 L/h, V2 560 L, Q3 12.1 L/h, V3 27.8 L, each ",
    "scaled by 1 + 1.30 (BSA - 0.77) (Mosteller), CL also by (1 + (age/5.32)^0.286)/2; ",
    "half the clearance forming doxorubicinol. Doses as labelled; plasma ",
    "doxorubicin in ng/mL. https://doi.org/10.1007/s40262-015-0272-4"
  )

  list(
    PK = PK,
    tPeak = 0,
    MEAC = 0,
    typical = 0,
    upperTypical = 0,
    lowerTypical = 0,
    reference = reference,
    metabolite = list(
      name              = "doxorubicinolPediatric",
      kFormation        = p$fm * cl1 / v1,
      firstPassFraction = 0,
      mwRatio           = 1
    )
  )
}
