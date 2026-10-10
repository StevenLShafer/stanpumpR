# -----------------------------------------------------------------------------
# Diamorphine (diacetylmorphine, heroin): intranasal and intramuscular,
# parent plasma concentration only
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in ng/mL, venous plasma.
#
# RESEARCH MODEL, OPT-IN
# ======================
# Diamorphine is in the library as an evidence-labelled research model, behind
# the "Illicit drugs" opt-in (off by default; see R/illicit-drugs.R).  It is a
# plasma-exposure estimate, not dosing advice, a safety threshold or an
# individual clinical prediction.
#
# WHAT IS MODELLED, AND WHAT IS NOT
# =================================
# The population model is Cai et al. (2025), a mixed-effects parent -> 6-MAM ->
# morphine cascade fitted to adult intramuscular (IM) and intranasal (IN)
# pharmaceutical diamorphine HCl.  stanpumpR's closed-form engine resolves at
# most one metabolite level (docs/adding-a-drug.md), so only the PARENT
# (diamorphine) disposition is plotted here; the 6-MAM and morphine analytes,
# which carry the clinical effect, are NOT modelled and so there is no effect
# site (tPeak = 0, plasma only).  This model therefore describes diamorphine
# plasma concentration after IM or IN pharmaceutical diamorphine only; it must
# not be read as an effect, and it must not be used for intravenous, smoked, or
# foil-heated-vapour heroin, for which different models apply (Rook 2006) and
# are not implemented.
#
# THE PARENT COMPARTMENT IS EXACT, THE REST DELIBERATELY OMITTED
# ==============================================================
# In Cai's structure the diamorphine central compartment is one-compartment:
# it is filled by first-order absorption (Ka) and emptied only by conversion to
# 6-MAM (K12).  So the parent plasma curve is reproduced exactly from Cai's
# central volume and that single rate, with K12 as the elimination rate
# constant:
#
#     v1  = V1                         (central volume, L)
#     ke  = K12                        (diamorphine's only loss, 1/min)
#     cl1 = ke * v1                    (L/min)
#     v2 = v3 = 1, cl2 = cl3 = 0       (one compartment)
#
# IM is the bioavailability reference (F = 1, a relative reference, not a
# measured absolute bioavailability); IN F is 0.519 relative to IM.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# Cai carries its own allometric size covariate on body weight: volumes
# proportional to (WT/70)^1, clearances to (WT/70)^0.75 and first-order rates
# to (WT/70)^-0.25.  With the fat-free-mass switch on, the covariate is
# evaluated at the pharmacokinetic weight (size$pkWeight); with it off, at
# total body weight, reproducing the published scaling.  Because cl1 = K12 * v1
# and K12 scales as (WT/70)^-0.25 while v1 scales as (WT/70)^1, cl1 scales as
# (WT/70)^0.75, exactly as Cai's clearance allometry requires.  Cai's adult set
# was ten male regular heroin users; the paediatric extrapolation and its age
# maturation are NOT reproduced here, so the model is an adult one evaluated at
# the patient's size.
#
# References
# ----------
# Cai L, Zhai J, Ji B, et al.  Intranasal diamorphine population
#   pharmacokinetics modeling and simulation in pediatric breakthrough pain.
#   CPT Pharmacometrics Syst Pharmacol 2025;14(3):435-447.
#   https://doi.org/10.1002/psp4.13186  (parent, 6-MAM and morphine typical
#   parameters, Table 2).
# -----------------------------------------------------------------------------

# Cai 2025, Table 2, 70 kg typical values used here (the parent compartment)
DIAMORPHINE_V1  <- 8.21    # central volume, L
DIAMORPHINE_K12 <- 103     # diamorphine -> 6-MAM, 1/h (its only elimination)
DIAMORPHINE_KA  <- 3.04    # first-order absorption, 1/h
DIAMORPHINE_F_IN <- 0.519  # intranasal bioavailability, relative to IM (= 1)

#' Diamorphine (heroin) pharmacokinetics: parent plasma, IN and IM
#'
#' @param weight patient weight (kg)
#' @param height patient height (cm)
#' @param age patient age (years)
#' @param sex \code{"male"} or \code{"female"}
#' @param adjustToFFM evaluate Cai's allometric covariate at the patient's
#'   pharmacokinetic (fat-free-mass) weight; when \code{FALSE}, at total body
#'   weight, reproducing the published scaling
#' @returns a list in the shape \code{getDrugPK()} expects; plasma only, no
#'   effect site and no modelled metabolite (see the header)
#' @export
diamorphine <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  # Own size covariate (Cai allometry on body weight): evaluated at the
  # pharmacokinetic weight with the switch on, total body weight with it off.
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)
  wt <- if (isTRUE(adjustToFFM)) size$pkWeight else weight

  sizeVolume <- wt / 70            # volumes proportional to (WT/70)^1
  sizeRate   <- (wt / 70)^(-0.25)  # first-order rates proportional to (WT/70)^-0.25

  v1  <- DIAMORPHINE_V1 * sizeVolume
  ke  <- (DIAMORPHINE_K12 * sizeRate) / 60   # 1/min
  cl1 <- ke * v1                             # L/min, scales as (WT/70)^0.75
  v2  <- 1                                   # one compartment
  v3  <- 1
  cl2 <- 0
  cl3 <- 0

  ka <- (DIAMORPHINE_KA * sizeRate) / 60     # 1/min

  default <- list(
    v1 = v1,
    v2 = v2,
    v3 = v3,
    cl1 = cl1,
    cl2 = cl2,
    cl3 = cl3,
    # Intramuscular: the bioavailability reference
    ka_IM = ka,
    bioavailability_IM = 1,
    tlag_IM = 0,
    # Intranasal: F relative to IM
    ka_IN = ka,
    bioavailability_IN = DIAMORPHINE_F_IN,
    tlag_IN = 0
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  tPeak <- 0     # parent plasma only; the effect is the metabolites', not modelled
  MEAC  <- 0

  # No established plasma range applies to this model, so no band is drawn
  # (helpNoBand(); all three columns are 0 in the CSV).
  typical      <- 0
  upperTypical <- 0
  lowerTypical <- 0

  reference <- paste0(
    "Cai L et al., CPT Pharmacometrics Syst Pharmacol 2025;14(3):435-447. ",
    "Adult IM/IN population model; parent diamorphine plasma only (6-MAM and ",
    "morphine metabolites not modelled); research use, not dosing advice. ",
    "https://doi.org/10.1002/psp4.13186"
  )

  return(
    list(
      PK = PK,
      tPeak = tPeak,
      MEAC = MEAC,
      typical = typical,
      upperTypical = upperTypical,
      lowerTypical = lowerTypical,
      reference = reference
    )
  )
}
