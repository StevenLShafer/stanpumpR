# -----------------------------------------------------------------------------
# Capecitabine, oral (Gieschke 2003): plasma 5'-DFUR, forming 5-FU
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min; amounts in
# mg and concentrations in mcg/mL (= mg/L) of 5'-DFUR (the curve) and 5-FU
# (the metabolite).
#
# Drafted by Claude Code, 2026-11-06, at the request of Steven L. Shafer
# (docs/chemotherapy.md, inst/extdata/chemotherapyRegistry.csv, source C16).
# Every number below was read from the PDFs, which Shafer supplied
# (2026-11-06): Gieschke's Tables 4-6 and Figure 1, Blesch's Table 1 and
# equations, on the page images.
#
# THE SOURCE
# ==========
# Gieschke R, Burger H-U, Reigner B, Blesch KS, Steimer J-L.  Population
# pharmacokinetics and concentration-effect relationships of capecitabine
# metabolites in colorectal cancer patients.  Br J Clin Pharmacol
# 2003;55:252-263 (doi 10.1046/j.1365-2125.2003.01765.x); the same model
# (CAP7440) with its equations in Blesch KS et al., Invest New Drugs
# 2003;21:195-223.  481 patients with colorectal cancer from two phase III
# trials and 24 from a bioequivalence study; 1250 mg/m2 twice daily; plasma
# 5'-DFUR, 5-FU and FBAL by LC-MS/MS; NONMEM V, FO.
#
# THE MODEL (Table 4)
# ===================
# A linear chain, each step complete, every volume and clearance apparent:
#
#   dose --ka 1.09/h--> 5'-DFUR (V1 90.6 L, CL1 75.8 L/h)
#        --> 5-FU (V2 17.8 L fixed, CL2 1190 L/h x (ALP/140)^-0.169)
#        --> FBAL (V3 73.6 L x (BSA/1.8)^0.812 x (CLCR/80)^0.394,
#                  CL3 27.5 L/h x (CLCR/80)^0.615)
#
# The lag (5.5e-4 h, with an IIV of 49,498%) is a numerical artefact and is
# taken as 0.  Capecitabine and 5'-DFCR are skipped: the dose enters 5'-DFUR
# in one first-order step.  The amounts are MOLAR, though Blesch writes the
# dose in mg: only a molar reading reproduces Gieschke's Table 6 (5'-DFUR AUC
# 20.3 against the median 20.1 ug h/mL, 5-FU 0.68-0.73 against 0.71, FBAL
# 24.4 against 24.7, after 2250 mg), so a mg of capecitabine (359.35 g/mol)
# becomes 246.21/359.35 = 0.685 mg of 5'-DFUR, carried as the oral
# "bioavailability", and a mg of 5'-DFUR forms 130.08/246.21 = 0.528 mg of
# 5-FU (mwRatio).
#
# WHAT IS PLOTTED, AND WHAT IS NOT
# ================================
# The drug's curve is 5'-DFUR, and its metabolite capecitabine5FU is the 5-FU
# formed (kFormation = CL1/V1, all of 5'-DFUR's clearance; fitted jointly,
# METABOLITE_FORMATION_BASIS "joint").  5-FU's half-life is 37 s, so it
# follows its formation.  FBAL, the third step, is not carried: the
# closed-form metabolite route forms one metabolite, and a cascade needs a
# general linear engine.  This 5-FU is NOT the library's fluorouracil (Briki
# and Tan, nonlinear intravenous models): it is Gieschke's apparent
# parameters for 5-FU formed from capecitabine.
#
# WHAT IS HELD AT THE REFERENCE
# =============================
# Alkaline phosphatase at 140 U/L (no field; it acts on 5-FU's clearance,
# 11% lower at twice the value).  Food: the phase III doses were taken after
# meals, as labelled.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# 5'-DFUR and 5-FU carry no size covariate (body-surface area entered the
# final model only on FBAL's volume, which is not carried), so every volume
# and clearance of both is scaled with the switch on, by Steven L. Shafer's
# first-principles rule of 2026-11-19: V1 and V2 by the fat-free-mass volume
# factor, CL1 and CL2 by the clearance factor, CL2 keeping its alkaline-
# phosphatase term (not a size covariate).  The factors are anchored at
# Gieschke's cohort (CAPECITABINE_COHORT, below: Table 1's three studies, 288
# men and 193 women in the phase III trials and 15 and 9 in the
# bioequivalence study, 61-63 years, 70-77 kg, BSA 1.8-1.9 m2), whose typical
# patient sits at a volume factor of 0.9472 and a clearance factor of 0.9602
# from the 70 kg reference man, so a patient like Gieschke's receives Table
# 4's values and the reference man receives volumes 5.6% and clearances 4.1%
# above them.  The formation of 5-FU is all of 5'-DFUR's clearance and the
# two moieties take the same factors, so the 5-FU formed is consistent at
# every size.  With the switch off the values are fixed, as published.
# (Until 2026-11-20 the values were read as the reference man's, the paper
# not being in the archive.)
#
# PREGNANCY (R/pregnancy.R): no comparison found; nothing applied.
# NO EFFECT SITE, NO BAND: the authors report that plasma 5-FU predicts
# tissue exposure, safety and efficacy poorly.
# -----------------------------------------------------------------------------

# Gieschke 2003, Table 4
CAPECITABINE_GIESCHKE <- list(
  ka = 1.09,                      # /h (printed "l/h", a misprint)
  v1 = 90.6, cl1 = 75.8,          # 5'-DFUR, L and L/h
  v2 = 17.8, cl2 = 1190,          # 5-FU, L (fixed) and L/h at ALP 140
  dfurPerCapecitabine = 246.21 / 359.35,
  fuPerDfur = 130.08 / 246.21
)

# Gieschke 2003, Table 1 (PDF p3, printed 254), mean of each study: phase III
# study 14, 148 men and 100 women, 61 years, 70 kg, BSA 1.8 m2; phase III
# study 15, 140 men and 93 women, 62 years, 77 kg, BSA 1.9 m2; the
# bioequivalence study, 15 men and 9 women, 63 years, 73 kg, BSA 1.8 m2.  No
# height is printed: it is solved from each study's BSA and weight by the
# Du Bois formula the table's footnote names (168.7, 171.9 and 164.6 cm),
# the same for both sexes, the table giving no values by sex.  Every volume
# and clearance of 5'-DFUR and 5-FU is anchored at this cohort (Steven L.
# Shafer, 2026-11-18 and 2026-11-19; see the header's BODY SIZE).  (The
# abstract's "248 males, 193 females" is a misprint; Table 1's counts are
# used.)
CAPECITABINE_COHORT <- list(groups = list(
  list(weight = 70, height = 168.7, age = 61, men = 148, women = 100),
  list(weight = 77, height = 171.9, age = 62, men = 140, women = 93),
  list(weight = 73, height = 164.6, age = 63, men = 15, women = 9)))

#' Capecitabine, oral (Gieschke 2003)
#'
#' Plasma 5'-DFUR in one compartment after first-order absorption, forming
#' 5-FU (\code{capecitabine5FU}); see the header of
#' \code{R/drugs_capecitabine.R}.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
capecitabine <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  p <- CAPECITABINE_GIESCHKE
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume = 1, legacyClearance = 1,
                        cohort = do.call(cohortSizeFactors, CAPECITABINE_COHORT))
  v1 <- p$v1 * size$volume
  cl1 <- p$cl1 / 60 * size$clearance

  default <- list(
    v1  = v1,
    v2  = 1,                                 # one compartment
    v3  = 1,
    cl1 = cl1,
    cl2 = 0,
    cl3 = 0,
    ka_PO = p$ka / 60,
    bioavailability_PO = p$dfurPerCapecitabine,   # mg of 5'-DFUR per mg dosed
    tlag_PO = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Gieschke R, Burger H-U, Reigner B, Blesch KS, Steimer J-L. Population ",
    "pharmacokinetics and concentration-effect relationships of capecitabine ",
    "metabolites in colorectal cancer patients. Br J Clin Pharmacol 2003;55:252-263. ",
    "505 patients: capecitabine absorbed at 1.09 /h into 5'-DFUR (V 90.6 L, CL 75.8 ",
    "L/h), forming 5-FU (V 17.8 L, CL 1190 L/h, alkaline phosphatase held at 140 U/L), ",
    "molar throughout; FBAL not carried. Plasma 5'-DFUR. ",
    "https://doi.org/10.1046/j.1365-2125.2003.01765.x"
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
      name              = "capecitabine5FU",
      kFormation        = cl1 / v1,          # all of 5'-DFUR's clearance
      firstPassFraction = 0,
      mwRatio           = p$fuPerDfur
    )
  )
}

#' 5-FU formed from capecitabine (Gieschke 2003)
#'
#' One compartment, apparent parameters; formed by \code{capecitabine()}.  See
#' the header of \code{R/drugs_capecitabine.R}.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
capecitabine5FU <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  p <- CAPECITABINE_GIESCHKE
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume = 1, legacyClearance = 1,
                        cohort = do.call(cohortSizeFactors, CAPECITABINE_COHORT))
  default <- list(
    v1  = p$v2 * size$volume,
    v2  = 1,                                 # one compartment
    v3  = 1,
    cl1 = p$cl2 / 60 * size$clearance,       # alkaline phosphatase 140 U/L
    cl2 = 0,
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
      "Gieschke R, Burger H-U, Reigner B, et al. Br J Clin Pharmacol 2003;55:252-263 ",
      "(5-FU formed from capecitabine's 5'-DFUR: V 17.8 L fixed, CL 1190 L/h at ",
      "alkaline phosphatase 140 U/L, apparent). https://doi.org/10.1046/j.1365-2125.2003.01765.x")
  )
}
