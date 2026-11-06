# -----------------------------------------------------------------------------
# Cisplatin, intravenous (Urien 2004): unbound (ultrafilterable) plasma
# platinum, forming protein-bound platinum
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min; amounts in
# mcg and concentrations in ng/mL (= ug/L) of PLATINUM (doses entered in mg of
# cisplatin).
#
# Drafted by Claude Code, 2026-11-06, at the request of Steven L. Shafer
# (docs/chemotherapy.md, inst/extdata/chemotherapyRegistry.csv).  Every
# number below was read from the PDF, which Shafer supplied (2026-11-06):
# Tables 1, 4 and 5, Figure 1 and the covariate equations, on the page
# images.
#
# THE SOURCE
# ==========
# Urien S, Lokiec F.  Population pharmacokinetics of total and unbound plasma
# cisplatin in adult patients.  Br J Clin Pharmacol 2004;57:756-763 (doi
# 10.1111/j.1365-2125.2004.02082.x).  43 adults (25 men), 21-76 years, BSA
# 1.38-2.10 m2, Cockcroft-Gault 44-155 mL/min, in two phase I studies with
# irofulven or 5-FU; 15-40 mg/m2 (20-80 mg) over 0.25-1 h for 5 days or twice
# a month; plasma and ultrafiltrate to 24 h; platinum by flameless atomic
# absorption, limit 20 ug/L; NONMEM.
#
# THE MODEL (Tables 4 and 5, Figure 1)
# ====================================
# Unbound platinum in two compartments; from the central one it binds
# irreversibly to plasma proteins, modelled as the formation of a "metabolite"
# with its own first-order elimination.  Total platinum is unbound plus
# bound.  Vm is not identifiable, so the bound compartment is estimated as
# fm/Vm:
#
#     unbound  V1 = 23.4 L x (BSA/1.7)^1.60
#              CL = 35.5 L/h x (BSA/1.7)^0.83 x (CLCr/81)^0.36
#              Q = 8.64 L/h, V2 = 12.0 L
#     bound    fm/Vm = 0.017 /L x (dose per m2 / 25)^-0.48 x (PROT/70)^1.33
#                      x (BSA/1.7)^-0.83 x (CLCr/81)^-0.36
#              CLm0/Vm = 0.014 /h
#
# The V1 exponent is Table 4's 1.60, on Shafer's decision (2026-11-06): the
# text's equation prints 2.3, the exponent of the paper's separate
# total-platinum model (Table 3).  Checks: the unbound half-lives 0.333 and
# 1.321 h (the paper's 0.34 and 1.32) and the bound 49.5 h (the paper's 50).
#
# In this library's metabolite interface kFormation is the unbound k10 =
# CL/V1 and the metabolite cisplatinBound (R/drugs_cisplatinBound.R) has the
# volume 1/(fm/Vm) and the clearance (CLm0/Vm)/(fm/Vm), so that its
# concentration obeys dCm/dt = (fm/Vm) CL C1 - (CLm0/Vm) Cm exactly as the
# paper's.  Fitted sequentially from one data set
# (METABOLITE_FORMATION_BASIS "joint").
#
# WHAT IS HELD AT THE REFERENCE
# =============================
# Serum protein at 70 g/L (no field; it acts only on binding) and the dose
# per m2 at 25 mg/m2 (a regimen covariate: a larger dose forms
# proportionally less bound platinum, which a linear superposition cannot
# carry).  The BSA formula is not named (Mosteller's is used).  CLCr is
# Cockcroft-Gault in mL/min with the weight below.
#
# DOSE BASIS
# ==========
# Doses in mg of cisplatin as labelled; concentrations are of platinum
# (ug/L), as measured.  The paper prints no conversion, and the unconverted
# reading reproduces its figures (unbound about 960 ug/L at the end of a
# 34.4-mg infusion, total about 760 at 1 h and 425 at 24 h), where dosing as
# platinum (0.65 of the mass) falls a third short.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# BSA and the Cockcroft-Gault weight at the pharmacokinetic weight with the
# switch on and total weight with it off; Q and V2, with no covariate, scale
# by the library's factors with the switch on and are fixed with it off.
#
# PREGNANCY (R/pregnancy.R)
# =========================
# The creatinine clearance takes the rise in filtration of pregnancy (x 1.5,
# pregnancyGfrFactor(), only when the creatinine is blank): physiological, an
# assumption, on the unbound clearance only (the bound platinum's model has
# no pregnancy flag).  Zemlickis 1994 found slightly less binding in pregnancy,
# recorded and not applied.
#
# NO EFFECT SITE, NO BAND
# -----------------------------------------------------------------------------

# Urien 2004, Tables 4 and 5
CISPLATIN_URIEN <- list(
  v1 = 23.4, v1BSAExponent = 1.60,      # Table 4 (the text's 2.3 not used)
  cl = 35.5, clBSAExponent = 0.83, clCrExponent = 0.36,
  q = 8.64, v2 = 12.0,
  bsaReference = 1.7, clcrReference = 81,
  fmOverVm = 0.017,                      # /L at the references
  doseExponent = -0.48, proteinExponent = 1.33,
  kBound = 0.014                         # CLm0/Vm, /h
)

# Cockcroft-Gault creatinine clearance and BSA shared by the two models
cisplatinCovariates <- function(weight, height, age, sex, adjustToFFM,
                                creatinine, pregnant)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume = 1, legacyClearance = 1)
  w <- if (isTRUE(adjustToFFM)) size$pkWeight else weight
  scr <- adultEquivalentCreatinine(creatinine, age, sex)
  list(
    size = size,
    bsa = bsaMosteller(w, height),
    clcr = creatinineClearanceCG(w, age, sex, scr) *
      pregnancyGfrFactor(pregnant, creatinine)                       # mL/min
  )
}

#' Cisplatin, intravenous (Urien 2004)
#'
#' Unbound platinum in two compartments, binding irreversibly to plasma
#' proteins (the metabolite \code{cisplatinBound}); body surface area and a
#' Cockcroft-Gault creatinine clearance on clearance.  See the header of
#' \code{R/drugs_cisplatin.R}.
#'
#' @inheritParams cefazolin
#' @param creatinine serum creatinine, mg/dL, or NULL for an assumed normal
#'   value
#' @param pregnant is the patient pregnant?  \code{TRUE}, with the creatinine
#'   blank, raises the creatinine clearance by the rise in filtration of
#'   pregnancy
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
cisplatin <- function(weight, height, age, sex, adjustToFFM = TRUE,
                      creatinine = NULL, pregnant = FALSE)
{
  p <- CISPLATIN_URIEN
  x <- cisplatinCovariates(weight, height, age, sex, adjustToFFM,
                           creatinine, pregnant)
  v1 <- p$v1 * (x$bsa / p$bsaReference)^p$v1BSAExponent
  cl <- p$cl * (x$bsa / p$bsaReference)^p$clBSAExponent *
    (x$clcr / p$clcrReference)^p$clCrExponent                         # L/h

  default <- list(
    v1  = v1,
    v2  = p$v2 * x$size$volume,
    v3  = 1,                                 # placeholder: two compartments
    cl1 = cl / 60,
    cl2 = p$q / 60 * x$size$clearance,
    cl3 = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Urien S, Lokiec F. Population pharmacokinetics of total and unbound plasma ",
    "cisplatin in adult patients. Br J Clin Pharmacol 2004;57:756-763. 43 adults: ",
    "unbound platinum in two compartments, V1 23.4 L x (BSA/1.7)^1.60 (Table 4), CL ",
    "35.5 L/h x (BSA/1.7)^0.83 x (CLCr/81)^0.36 (Mosteller; Cockcroft-Gault from the ",
    "entered or an assumed normal creatinine), Q 8.64 L/h, V2 12.0 L, binding ",
    "irreversibly to plasma proteins (cisplatinBound). Doses in mg of cisplatin; ",
    "ultrafilterable platinum in ug/L",
    if (pregnancyGfrFactor(pregnant, creatinine) != 1) paste0("; ", pregnancyReference(
      "creatinine clearance x 1.5, the rise in filtration of pregnancy")) else "",
    ". https://doi.org/10.1111/j.1365-2125.2004.02082.x"
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
      name              = "cisplatinBound",
      kFormation        = cl / 60 / v1,     # the unbound k10, scaled by fm/Vm
      firstPassFraction = 0,
      mwRatio           = 1                 # platinum throughout
    )
  )
}
