# -----------------------------------------------------------------------------
# Citalopram (racemic): Akil 2016's R- and S-enantiomer models, reduced
# exactly to one two-compartment mammillary model of the total
# -----------------------------------------------------------------------------
# Provenance
# ----------
# Drafted by Claude Code, 2026-10-10, at the request of Steven L. Shafer, from
# his summary of the source.  Verified by tests/testthat/test-drugs-citalopram.R,
# which derives its expectations from the published numbers independently of
# this file and checks the reduction against the two enantiomer curves.
#
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in ng/mL of total (R + S) citalopram.  The source reports
# L/h and 1/h; both are divided by 60 here.
#
# THE SOURCE
# ==========
# Akil et al., J Pharmacokinet Pharmacodyn 2016;43:99-109: 81 older patients
# with Alzheimer's disease and agitation in the CitAD trial, on oral racemic
# citalopram.  R- and S-citalopram were each described by one compartment with
# a common absorption rate constant, ka fixed at 1 /h.  Each racemic dose
# delivers D/2 of each enantiomer.  Typical values (apparent, divided by F):
#
#   R-citalopram  CL/F = 13 L/h (male) or 9.05 L/h (female) x (age/60)^-0.822
#                 V/F  = 1830 L
#   S-citalopram  CL/F = 22.1 L/h (CYP2C19 EM/RM) or 16.3 L/h (IM/PM)
#                        x (age/60)^-1.33 x (weight/70)^0.75
#                 V/F  = 1390 L
#
# The desmethyl metabolites (R- and S-DCIT) were also modelled in the source,
# under assumptions of complete conversion and equal volumes; they are NOT
# modelled here.  The interindividual-variability table's scale (variance or
# CV) could not be settled, so the model is deterministic, the typical
# patient's.
#
# WHAT IS PLOTTED
# ===============
# Total racemic citalopram, C_R + C_S: what clinical assays report, what the
# AGNP band refers to, and what Meyer 2004's serotonin-transporter occupancy
# EC50 (11.7 ng/mL) was expressed on.  Occupancy is not plotted.
#
# THE REDUCTION, AND WHY IT IS EXACT
# ==================================
# The engine takes one mammillary model per drug.  After an oral dose D with
# the common ka, the total concentration is the input ka D e^{-ka t}
# convolved with the impulse response (per unit dose of racemate)
#
#     h(t) = A e^{-l1 t} + B e^{-l2 t},
#     A = 0.5 / V_R,  B = 0.5 / V_S,
#     l1 = k_R = CL_R / V_R,  l2 = k_S = CL_S / V_S.
#
# Any biexponential with positive coefficients is exactly the central-
# compartment impulse response of a two-compartment mammillary model.  For
# that model, h(t) = (1/V1) [ (l1 - k21)/(l1 - l2) e^{-l1 t}
#                            + (k21 - l2)/(l1 - l2) e^{-l2 t} ],
# with l1 + l2 = k10 + k12 + k21 and l1 l2 = k10 k21.  Matching terms:
#
#     V1  = 1 / (A + B)                       (h(0) = 1/V1)
#     k21 = (A l2 + B l1) / (A + B)           (a weighted mean, so it lies
#                                              between l1 and l2)
#     k10 = l1 l2 / k21
#     k12 = l1 + l2 - k21 - k10 = (l1 - k21)(k21 - l2) / k21  >= 0
#
# and cl1 = k10 V1, cl2 = k12 V1, v2 = cl2 / k21.  Because ka is common to
# both enantiomers, the convolution with the absorption input is the same
# linear operation on both sides, so the oral curve is exact too, not an
# approximation.  The total clearance cl1 = 1 / (A/l1 + B/l2) =
# 1 / (0.5/CL_R + 0.5/CL_S), the harmonic mean of the two clearances, which
# makes the racemic AUC D/2/CL_R + D/2/CL_S, as it must be.
#
# When k_R and k_S coincide the model is one compartment (k12 = 0, and v2
# would be zero).  Close to coincidence the engine's quadratic for the
# eigenvalues and its coefficients, which divide by l1 - l2, lose precision,
# so within a relative separation of CITALOPRAM_COINCIDENT (1e-4) the model is
# collapsed to one compartment with V1 as above and elimination rate
# (A l1 + B l2)/(A + B), which matches h(0) and h'(0); the error is second
# order in the separation, about 1e-8 relative.  v2 = 1 and cl2 = 0 are then
# the library's usual one-compartment placeholders.
#
# CYP2C19 PHENOTYPE
# =================
# The source estimated two groups for S-citalopram's clearance: EM/RM, 22.1
# L/h, and IM/PM, 16.3 L/h.  normal, rapid and ultrarapid take 22.1;
# intermediate and poor take 16.3.  R-citalopram's clearance does not depend
# on CYP2C19 in the model.  A poor metaboliser therefore gets the IM/PM
# group's value, which probably overstates a true poor metaboliser's
# clearance.
#
# AGE AND SEX
# ===========
# Used as published, centred on 60 years.  The cohort was older (CitAD
# enrolled adults with Alzheimer's disease); the power functions of age are
# extrapolated to younger adults, and become meaningless in children: in a
# newborn they raise S-clearance about 100,000-fold.  The values
# stay finite and positive, which is all the library's pediatric check asks.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# S-citalopram's clearance carries its own weight covariate, (weight/70)^0.75.
# As for any model with its own weight covariate, it is evaluated at the
# pharmacokinetic weight with the switch on (size$pkWeight, 70 kg x
# FFM / FFM_ref) and at total body weight with it off.  The size-free
# parameters, both volumes and R-citalopram's clearance, take the library
# factors with legacyVolume = 1, so with the switch off the published
# equations are reproduced exactly.  (With the switch on, (pkWeight/70)^0.75
# equals size$clearance, so every clearance scales alike.)  The reduction is
# applied after scaling, to the patient's own enantiomer parameters.
#
# NO EFFECT SITE
# ==============
# The antidepressant (and anti-agitation) effect develops over weeks and has
# no equilibration rate constant.  tPeak = 0, MEAC = 0, plasma only.  The
# band in the CSV, 50 to 110 ng/mL with 80 as the typical line, is the AGNP
# 2018 consensus therapeutic reference range for citalopram (Hiemke et al.,
# Pharmacopsychiatry 2018;51:9-62).  Not used: Friberg 2006's QT model, which
# was fitted to overdoses; and CitAD's exposure-response for agitation, which
# does not predict remission of depression.
#
# APPARENT PARAMETERS: ORAL ONLY
# ==============================
# Fitted to oral data alone: oral units only, bioavailability_PO = 1, no lag
# (docs/adding-a-drug.md, "Apparent parameters restrict the route").
#
# References
# ----------
# Akil A, Bies RR, Pollock BG et al., J Pharmacokinet Pharmacodyn
#   2016;43:99-109.  https://doi.org/10.1007/s10928-015-9457-6
# Meyer JH et al., Am J Psychiatry 2004;161:826-835 (SERT occupancy).
# Hiemke C et al., Pharmacopsychiatry 2018;51:9-62 (AGNP consensus).
# -----------------------------------------------------------------------------

CITALOPRAM_KA       <- 1       # 1/h, common to both enantiomers, fixed
CITALOPRAM_V_R      <- 1830    # L, R-citalopram V/F
CITALOPRAM_V_S      <- 1390    # L, S-citalopram V/F
CITALOPRAM_CL_R     <- c(male = 13, female = 9.05)   # L/h at 60 years
CITALOPRAM_CL_R_AGE <- -0.822
CITALOPRAM_CL_S_AGE <- -1.33
# S-citalopram CL/F at 60 years and 70 kg, L/h, by CYP2C19 phenotype: EM/RM
# 22.1 (normal, rapid, ultrarapid), IM/PM 16.3 (intermediate, poor).
CITALOPRAM_CL_S <- c(
  poor         = 16.3,
  intermediate = 16.3,
  normal       = 22.1,
  rapid        = 22.1,
  ultrarapid   = 22.1
)
# Relative separation of the two elimination rate constants below which the
# reduced model is collapsed to one compartment.  See the header.
CITALOPRAM_COINCIDENT <- 1e-4

#' Reduce two parallel one-compartment enantiomers to one mammillary model
#'
#' Exact reduction of h(t) = A exp(-l1 t) + B exp(-l2 t), A, B > 0, to a
#' two-compartment mammillary model.  See the header of
#' \code{R/drugs_citalopram.R}.
#'
#' @param A,B coefficients of the impulse response (1/L)
#' @param l1,l2 the two elimination rate constants (any time unit)
#' @returns list of v1, v2, cl1, cl2 in L and L per that time unit
#' @noRd
citalopramReduce <- function(A, B, l1, l2)
{
  v1 <- 1 / (A + B)
  if (abs(l1 - l2) <= CITALOPRAM_COINCIDENT * max(l1, l2)) {
    # Coincident rates: one compartment.  See the header.
    k <- (A * l1 + B * l2) / (A + B)
    return(list(v1 = v1, v2 = 1, cl1 = k * v1, cl2 = 0))
  }
  k21 <- (A * l2 + B * l1) / (A + B)
  k10 <- l1 * l2 / k21
  k12 <- (l1 - k21) * (k21 - l2) / k21    # = l1 + l2 - k21 - k10, >= 0
  list(v1 = v1, v2 = k12 * v1 / k21, cl1 = k10 * v1, cl2 = k12 * v1)
}

#' Citalopram (racemic) pharmacokinetics
#'
#' Akil et al. (2016) R- and S-citalopram one-compartment models, reduced
#' exactly to one two-compartment model of total citalopram.  Oral only.  See
#' the file header.
#'
#' @param weight weight in kg
#' @param height height in cm
#' @param age age in years
#' @param sex sex as a string
#' @param cyp2c19 CYP2C19 metaboliser phenotype, one of \code{CYP2C19_VALUES}
#' @param adjustToFFM when \code{TRUE}, evaluate S-citalopram's weight term at
#'   the pharmacokinetic weight and scale the size-free parameters to fat-free
#'   mass; when \code{FALSE}, use total body weight and the published
#'   size-free parameters unscaled.
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
citalopram <- function(weight, height, age, sex, cyp2c19 = CYP2C19_DEFAULT,
                       adjustToFFM = TRUE)
{
  if (length(cyp2c19) != 1 || !cyp2c19 %in% CYP2C19_VALUES) {
    stop("Invalid cyp2c19: ", paste(cyp2c19, collapse = ", "),
         ". Must be one of: ", paste(CYP2C19_VALUES, collapse = ", "))
  }
  if (length(sex) != 1 || !sex %in% SEX_VALUES) {
    stop("Invalid sex: ", paste(sex, collapse = ", "),
         ". Must be one of: ", paste(SEX_VALUES, collapse = ", "))
  }

  # Size-free parameters: fixed published values, legacy factors 1.  The
  # S-clearance weight term: pharmacokinetic weight on, total weight off.
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1)
  pkW  <- if (isTRUE(adjustToFFM)) size$pkWeight else weight

  # Enantiomer parameters, L and L/h
  vR  <- CITALOPRAM_V_R * size$volume
  vS  <- CITALOPRAM_V_S * size$volume
  clR <- CITALOPRAM_CL_R[[sex]] * (age / 60)^CITALOPRAM_CL_R_AGE * size$clearance
  clS <- CITALOPRAM_CL_S[[cyp2c19]] * (age / 60)^CITALOPRAM_CL_S_AGE * (pkW / 70)^0.75

  # Exact reduction of the total (R + S) to two compartments; each racemic
  # dose is half R, half S.  See the header.
  r <- citalopramReduce(A = 0.5 / vR, B = 0.5 / vS, l1 = clR / vR, l2 = clS / vS)

  default <- list(
    v1 = r$v1,
    v2 = r$v2,
    v3 = 1,                         # two compartments
    cl1 = r$cl1 / 60,               # L/min
    cl2 = r$cl2 / 60,
    cl3 = 0,
    ka_PO = CITALOPRAM_KA / 60,     # 1/min
    bioavailability_PO = 1,         # the apparent scale already carries F
    tlag_PO = 0
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  # Band, ng/mL total citalopram: AGNP 2018 therapeutic reference range.  The
  # plot reads the CSV; these mirror it.
  typical      <- 80
  upperTypical <- 110
  lowerTypical <- 50

  reference <- paste0(
    "Akil A et al., J Pharmacokinet Pharmacodyn 2016;43:99-109 (R- and ",
    "S-citalopram, reduced exactly to one two-compartment model of the ",
    "racemate; band: AGNP consensus, Hiemke C et al., Pharmacopsychiatry ",
    "2018;51:9-62). https://doi.org/10.1007/s10928-015-9457-6"
  )

  list(
    PK = PK,
    tPeak = 0,       # no effect-site model; see the header
    MEAC = 0,        # not an opioid
    typical = typical,
    upperTypical = upperTypical,
    lowerTypical = lowerTypical,
    reference = reference
  )
}
