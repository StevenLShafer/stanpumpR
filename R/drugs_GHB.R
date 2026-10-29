# -----------------------------------------------------------------------------
# GHB (gamma-hydroxybutyrate): oral, saturable elimination, two compartments
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, amounts in milligrams,
# concentrations in mcg/mL (= mg/L), venous plasma.
#
# RESEARCH MODEL, OPT-IN
# ======================
# GHB is in the library as an evidence-labelled research model, behind the
# "Illicit drugs" opt-in (off by default; see R/illicit-drugs.R).  It is a
# plasma-exposure estimate, not dosing advice, a safety threshold, or a
# prediction of anyone's experience.  GHB has a famously steep
# concentration-effect relationship and a narrow margin between a recreational
# dose and unconsciousness; nothing on this curve marks that margin, because no
# source reviewed for this model fits one in a form this engine could carry.
#
# THE DRUG IS ALSO A MEDICINE
# ===========================
# The same molecule is sodium oxybate, approved as Xyrem, Xywav and Lumryz for
# cataplexy and excessive daytime sleepiness in narcolepsy, and the model here
# is fitted to that clinical programme.  It is filed under illicit drugs
# because that is the use this library is asked about; the parameters are the
# medicine's.
#
# THE DOSE IS THE SALT, THE CURVE IS THE ACID
# ===========================================
# This is the trap this drug is known for, so the unit names the basis: a dose
# is entered in grams or milligrams of SODIUM OXYBATE ("g PO oxybate"), the
# salt that is actually swallowed, whether as Xyrem or as the recreational
# sodium salt.  The curve is plasma GHB, the free acid, in mcg/mL, which is
# what every independent human study and the whole forensic literature report.
#
# The two differ by their molecular weights: GHB 104.10 g/mol over sodium
# oxybate 126.09 g/mol = 0.8256.  One gram of sodium oxybate is 0.826 g of
# GHB.  Mixing the bases is a 21% error in either direction.
#
# Chen reports doses AND concentrations as sodium oxybate, so every parameter
# below that carries a mass is rescaled onto the acid basis by that same
# factor: the dose by a salt factor, and Vmax and Km with it.  That rescaling
# is EXACT, not an approximation.  Writing A' = fA for the acid amount, the
# saturable term
#
#     Vmax C / (Km + C)   becomes   f Vmax C / (Km + C)
#                                 = (f Vmax) C' / (f Km + C'),   C' = f C,
#
# so scaling Vmax and Km by f reproduces the source's curve exactly, rescaled.
# The volume and every rate constant are unchanged, being volumes and rates.
# test-drugs-GHB.R pins that the rescaled model and the source's own basis
# differ by exactly the molecular weight ratio and nothing else.
#
# WHY THIS ENGINE
# ===============
# Two compartments with MICHAELIS-MENTEN elimination from the central one, so
# no closed-form engine applies: the saturable term is why exposure rises
# faster than the dose, which is the whole clinical and toxicological point of
# this drug.  A second gram does not add a second gram's worth of curve, it
# adds more.  advanceMichaelisMenten.R gained its optional peripheral
# compartment (k23/k32) for this model; every other drug on that engine
# declares neither and is unchanged.
#
# THE SOURCE
# ==========
# Chen C, Rosen CL, Ruoff C, et al.  Clin Transl Sci 2020;13(5):932-940.
# A population model fitted in NONMEM to 3,775 plasma observations from 174
# people pooled across five studies: 132 healthy adults, 13 adults with
# narcolepsy and 29 children and adolescents with narcolepsy, given 1 to 4.5 g
# of sodium oxybate as an oral solution: 119 of the 174 (the two healthy-adult
# studies) by day, the rest at night, some twice nightly four hours apart
# (supplement Tables S2-S3).
# Table S4 of its supplement, on the sodium oxybate basis:
#
#     Vmax 1508 mg/h    (bootstrap 1357-1699)     Km   22.0 mg/L (17.7-26.8)
#     VC   29.5 L       (28.4-30.5)               Ka   4.12 /h   (3.57-4.76)
#     K23  0.0968 /h    (0.0655-0.1407)           K32  0.536 /h  (0.449-0.633)
#     F    1.00, FIXED
#     allometric exponents  0.650 on VC, 0.633 on Vmax
#     age category on VC    7-17 +0.201, and a further 7-11 -0.328
#     food on Ka -0.681, diurnal on Vmax -0.177
#
# F WAS FIXED AT 1, SO THE PARAMETERS ARE APPARENT, SO ORAL ONLY
# ==============================================================
# The model did not estimate bioavailability, it fixed it at 1, while the
# Xyrem label puts absolute oral bioavailability at 88%.  So VC and Vmax are
# really VC/F and Vmax/F: they predict ORAL concentrations correctly, because
# the unknown factor cancels, and intravenous ones wrong by 1/F.  GHB is
# therefore offered as an ORAL unit only, with bioavailability carried as 1
# because the apparent scale already contains it (docs/adding-a-drug.md).
# That GHB was once given intravenously as an anaesthetic does not change it:
# this parameter set cannot be used for that.
#
# WHAT IS NOT CARRIED, AND WHY
# ============================
#   * FOOD, which the source fitted as a 68.1% reduction in Ka: the app has no
#     field for it, so the FASTED reference is carried.  Food moves timing far
#     more than exposure (the paper predicts Cmax about 30% lower fed, with
#     AUC within bioequivalence), and a stomach that has been eaten into is
#     the common recreational case, so a real fed peak is lower and later than
#     this curve.  Reproducing the source's own fed/fasted Cmax ratio is one
#     of the drug's tests.
#   * TIME OF DAY, which the source fitted as a 17.7% lower Vmax at one of its
#     two levels.  The app has no clock, so the model carries the REFERENCE
#     level, as MDMA carries its reference study.  The source does not print
#     which level that is; the other one would raise exposure, and at a 4.5 g
#     dose it moves the second peak by about 13%.  Validating the reference
#     parameterisation against the paper's own simulated figures is a test.
#   * CAPACITY-LIMITED ABSORPTION.  Palatini showed the peak time stretching
#     with dose (25, 30 then 45 minutes over a fourfold dose) in a way a fixed
#     first-order Ka cannot produce, and read it as saturable absorption.
#     Chen fitted a single first-order Ka and no saturable input, so none is
#     coded.  At high doses this model therefore peaks earlier than the real
#     drug.  Nothing here is calibrated above 4.5 g.
#   * EVERY END POINT.  tPeak = 0, so no effect site; MEAC = 0 and no band.
#     Liechti found subjective effects tracking plasma with NO clockwise
#     hysteresis, so an effect-site delay would be the wrong shape even if a
#     fitted one existed.
#
# BODY SIZE AND AGE (docs/weight-adjustment.md)
# =============================================
# The source fitted its own allometry, so it is carried rather than the
# package's: VC with an exponent of 0.650 on weight and Vmax with 0.633.  The
# source does NOT print the weight it normalised to; 70 kg is assumed, the
# library's own reference and the usual convention.  The dataset's median
# weight was 72.1 kg, and using that instead would move predictions by under
# 3%, inside the bootstrap interval of Vmax alone, which spans about +/-11%
# (test-drugs-GHB.R pins that sensitivity so the assumption cannot be
# forgotten).  With the fat-free mass switch on, the exponents are evaluated
# at the patient's pharmacokinetic weight; with it off, at total body weight,
# reproducing the published scaling.
#
# The source's age effect is CATEGORICAL, on VC, and is carried as published.
# The supplement prints the coefficients but not the form they enter in, and
# the two candidates differ: read EXPONENTIALLY, a 7 to 17 year old gets
# VC x exp(0.201) = 1.223 and a 7 to 11 year old exp(0.201 - 0.328) = 0.881,
# which reproduces the paper's own "children had a 12% reduction, and
# adolescents had a 22% increase" to the digit; read as a proportion, 1.201
# and 0.873, which gives 20.1% and would not round to 22%.  So the
# exponential form is carried.  The choice is worth under 2% on the central
# volume of a 7 to 17 year old and nothing at all for an adult, and a test
# pins both readings so the margin stays visible.  (The food effect in the
# same table is the other way round, a plain proportion: 1 - 0.681 is the
# "reduction of ~70% in Ka" the paper describes, where the exponential would
# be 49%.  One model, two conventions, which is why each was checked against
# a percentage the authors wrote out.)
#
# Being categorical, the age effect steps at the twelfth and eighteenth
# birthdays rather than varying smoothly.  The youngest participant was 8.00
# years (Table S3), so nothing below 8 was fitted; below 7, the category's
# published lower edge, the adult value is used.
#
# References
# ----------
# Chen C, Rosen CL, Ruoff C, et al.  Population and noncompartmental
#   pharmacokinetics of sodium oxybate support weight-based dosing in children
#   and adolescents with narcolepsy with cataplexy.  Clin Transl Sci
#   2020;13(5):932-940.  https://doi.org/10.1111/cts.12780  (the model and its
#   Table S4; 174 participants, 3,775 observations).
# Liechti ME, Quednow BB, Liakoni E, et al.  Pharmacokinetics and
#   pharmacodynamics of gamma-hydroxybutyrate in healthy subjects.  Br J Clin
#   Pharmacol 2016;81(5):980-988.  https://doi.org/10.1111/bcp.12863
#   (32 healthy men, 20 and 35 mg/kg orally; independent validation, and the
#   absence of hysteresis between plasma and subjective effect).
# Palatini P, Tedeschi L, Frison G, et al.  Dose-dependent absorption and
#   elimination of gamma-hydroxybutyric acid in healthy volunteers.  Eur J
#   Clin Pharmacol 1993;45(4):353-356.  https://doi.org/10.1007/BF00265954
#   (8 healthy men at 12.5, 25 and 50 mg/kg of the free acid; non-compartmental
#   only, so not the implemented source, but the evidence for saturable
#   absorption and for no plasma protein binding).
# -----------------------------------------------------------------------------

# Molecular weights (g/mol): the dose is the salt, the curve is the acid
GHB_MW            <- 104.10
SODIUM_OXYBATE_MW <- 126.09
GHB_PER_OXYBATE   <- GHB_MW / SODIUM_OXYBATE_MW        # 0.825601

# Chen 2020, Table S4, as published on the SODIUM OXYBATE basis
GHB_VMAX_SRC <- 1508            # mg/h
GHB_KM_SRC   <- 22.0            # mg/L
GHB_VC       <- 29.5            # L
GHB_KA       <- 4.12 / 60       # 1/min, from 4.12 /h
GHB_K23      <- 0.0968 / 60     # 1/min
GHB_K32      <- 0.536 / 60      # 1/min
GHB_EXP_VC   <- 0.650           # weight exponent on the central volume
GHB_EXP_VMAX <- 0.633           # weight exponent on Vmax

# The same model with the curve on the GHB (acid) basis: both masses rescaled
# by the molecular weight ratio, which is exact (see the header)
GHB_VMAX <- GHB_VMAX_SRC * GHB_PER_OXYBATE / 60        # mg/min
GHB_KM   <- GHB_KM_SRC   * GHB_PER_OXYBATE             # mg/L

# Chen's categorical age effects on the central volume, as published
GHB_AGE_PEDIATRIC <- 0.201      # 7 to 17 years
GHB_AGE_CHILD     <- -0.328     # a further effect, 7 to 11 years

#' GHB pharmacokinetics: plasma GHB after an oral dose of sodium oxybate
#'
#' @param weight patient weight (kg)
#' @param height patient height (cm)
#' @param age patient age (years)
#' @param sex \code{"male"} or \code{"female"}
#' @param adjustToFFM evaluate the source's own weight exponents at the
#'   patient's pharmacokinetic (fat-free-mass) weight; when \code{FALSE}, at
#'   total body weight, reproducing the published scaling
#' @returns a list in the shape \code{getDrugPK()} expects, carrying a
#'   \code{michaelisMenten} block with a peripheral compartment; plasma only,
#'   oral only, no effect site (see the header)
#' @export
GHB <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  # The source carries its own allometry, so the package's factors are used
  # only to choose which weight it is evaluated at.
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)
  wt <- if (isTRUE(adjustToFFM)) size$pkWeight else weight

  # Chen's categorical age effect on the central volume, adults as reference,
  # exponential (see the header: it is what reproduces the paper's own
  # percentages, and the coefficients add inside the exponent)
  ageTerm <- 0
  if (!is.na(age) && age >= 7 && age <= 17)
  {
    ageTerm <- ageTerm + GHB_AGE_PEDIATRIC
    if (age <= 11) ageTerm <- ageTerm + GHB_AGE_CHILD
  }
  ageFactor <- exp(ageTerm)

  v1   <- GHB_VC   * (wt / FFM_REFERENCE_WEIGHT)^GHB_EXP_VC * ageFactor
  vmax <- GHB_VMAX * (wt / FFM_REFERENCE_WEIGHT)^GHB_EXP_VMAX
  km   <- GHB_KM

  default <- list(
    v1 = v1, v2 = 1, v3 = 1,
    # The low-concentration limit, Vmax / Km, for the help tables only: the
    # simulation uses the michaelisMenten block.
    cl1 = vmax / km, cl2 = 0, cl3 = 0,
    ka_PO = GHB_KA,
    bioavailability_PO = 1,     # apparent parameters already contain F
    tlag_PO = 0
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  tPeak <- 0     # effects track plasma with no hysteresis; see the header
  MEAC  <- 0

  # No established plasma range applies to this model, so no band is drawn.
  typical      <- 0
  upperTypical <- 0
  lowerTypical <- 0

  reference <- paste0(
    "Chen C et al., Clin Transl Sci 2020;13(5):932-940 ",
    "(https://doi.org/10.1111/cts.12780), Table S4. Two compartments with ",
    "Michaelis-Menten elimination, 174 participants; apparent parameters ",
    "(bioavailability fixed at 1), so oral doses only. The dose is sodium ",
    "oxybate and the curve is plasma GHB, rescaled by the molecular weight ",
    "ratio 0.8256. Fasted, and nothing is calibrated above 4.5 g. Plasma ",
    "only, no effect site; research use, not dosing advice."
  )

  return(
    list(
      PK = PK,
      tPeak = tPeak,
      MEAC = MEAC,
      typical = typical,
      upperTypical = upperTypical,
      lowerTypical = lowerTypical,
      reference = reference,
      michaelisMenten = list(
        vmax = vmax,
        km = km,
        k23 = GHB_K23,
        k32 = GHB_K32,
        # The dose is sodium oxybate, the model is GHB (see the header)
        saltFactor = list(default = GHB_PER_OXYBATE)
      )
    )
  )
}
