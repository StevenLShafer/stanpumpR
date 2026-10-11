# -----------------------------------------------------------------------------
# Aspirin: enteric-coated low-dose aspirin with salicylate (Koh 2025)
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in mcg/mL (= mg/L) of acetylsalicylic acid (ASA).
#
# THE SOURCE
# ==========
# Koh et al. fitted a population model (Monolix) to 669 plasma ASA and
# salicylic acid (SA) concentrations from 44 healthy Korean adults (20-49
# years, 55.6-89.5 kg) taking 100 mg enteric-coated aspirin once daily for 5
# days, as a capsule (Astrix) or a tablet (Aspirin Protect, Bayer).  ASA has
# ONE compartment, SA two (R/drugs_salicylate.R).  Table 1, final model:
#
#     dual absorption from the depot into a pre-systemic compartment:
#         fraction fr = 0.69 zero-order over Tk0 1.58 h after Lag0 2.81 h
#         the rest first-order, ka 0.053 /h (tablet) or 0.22 /h (capsule)
#     pre-systemic compartment, emptying first-order:
#         to central ASA  k23 = 2.32 /h  (80.3%)
#         to central SA   k24 = 0.57 /h  (19.7%, converted before reaching the
#                                         systemic circulation)
#     ASA:  V3/F 23.51 L;  ASA -> SA  k34 = 2.97 x (W/Wmed)^1.31 /h
#
# so the CL/F of ASA is k34 x V3/F = 69.8 L/h, all of it to SA.  The
# parameters are apparent: absolute bioavailability was not identified.
# Validated externally against 80 and 160 mg enteric-coated aspirin.
#
# HOW IT IS IMPLEMENTED
# =====================
# The engine absorbs through at most two first-order depots, each with a lag,
# and forms a metabolite by first-order transfer out of the parent's central
# compartment (metabolite block), with a first-pass fraction.  So:
#
# - ASA disposition, the formation of SA (kFormation = k34, the parent's
#   whole elimination) and the split at the pre-systemic compartment
#   (bioavailability 0.803 to ASA, firstPassFraction 0.197 to SA) are exact.
# - Each absorption path, followed by the pre-systemic compartment (rate
#   kp = k23 + k24 = 2.89 /h), is APPROXIMATED by one first-order depot with a
#   lag:
#   first-order path (31% of the dose): one depot matching the mean and
#     variance of the delay, mean = Lag0 + 1/ka + 1/kp, var = 1/ka^2 + 1/kp^2,
#     1/ka' = sqrt(var), lag' = mean - 1/ka': ka' 0.05299 /h, lag' 3.153 h.
#   zero-order path (69%): ASA's half-life is 14 min, so its plasma curve
#     follows the shape of the input, and the moment-matched depot (ka' 1.75
#     /h, lag' 3.37 h) put the ASA peak 35% too high.  Instead ka' and lag'
#     were fitted by least squares (R, optim; Claude Code, 2026-10-10) to the
#     ASA curve of Koh's exact structure (zero-order input into the
#     pre-systemic compartment, solved numerically, 100 mg tablet, 0-24 h,
#     at the median weight): ka' 1.032 /h, lag' 3.269 h.
#   Against the exact structure, 100 mg tablet at the median weight: ASA
#   peak 0.48 against 0.49 mg/L, but at 3.8 h against 4.4 h (the exact curve
#   is a plateau, the approximation a spike); salicylate peak 4.04 against
#   4.65 mg/L (13% low) at 5.2 h against 5.1 h; both AUCs identical (see
#   test-drugs-aspirin.R).  No single depot gets both peaks: the
#   moment-matched one gave salicylate within 2% and ASA 35% high, and a
#   joint fit to both curves settles on this one.  ASA, the antiplatelet
#   species, was preferred; salicylate at 4 mg/L is far below analgesic
#   concentrations.
#   The SA formed on first pass takes the same depots, as it does in the
#   source.
# - The TABLET is offered (ka 0.053 /h); the capsule's 0.22 /h is not.
#
# UNCONFIRMED (the supplement, with the model code and the demographics
# table, was not available):
# - Wmed, the median weight the weight terms are normalised to, is taken as
#   68.35 kg (from a literature summary, not checked against Table S1).
# - Whether Lag0 delays the first-order path as well as the zero-order one.
#   It is applied to both here, as enteric coating would; Figure 1 lists it
#   with Tk0 and fr without saying.
#
# WHERE TO BE CAREFUL
# ===================
# Low dose only.  Fitted at 100 mg and validated at 80 and 160 mg.  At
# analgesic and anti-inflammatory doses salicylate elimination saturates
# (Michaelis-Menten), so this linear model underpredicts salicylate, and its
# half-life, increasingly with dose.  Enteric-coated tablet only; plain
# aspirin is absorbed within an hour.  Healthy Korean adults.  The
# antiplatelet effect, irreversible acetylation of platelet COX-1 (the
# source's thromboxane B2 turnover model), is not plotted.
#
# EFFECT SITE AND BAND
# ====================
# None: tPeak = 0, and the plotted concentration is plasma ASA.  Aspirin is
# active itself (prodrug = FALSE), with salicylate folded onto its own row.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# k34 carries the model's own weight covariate, evaluated at the
# pharmacokinetic weight with the switch on and total weight with it off.
# V3/F has no size term in the source and takes the library's factors
# (legacyVolume = 1).  No CYP2D6 adjustment: the model fitted none.
#
# References
# ----------
# Koh J et al., Drug Des Devel Ther 2025;19:7853-7863.
#   https://doi.org/10.2147/DDDT.S533428
# (Claude Code, 2026-10-10, at the request of Steven L. Shafer.)
# -----------------------------------------------------------------------------

ASPIRIN_WT_MEDIAN <- 68.35        # kg, Koh 2025 covariate median (unconfirmed)
ASPIRIN_MW        <- 180.16       # g/mol, acetylsalicylic acid
SALICYLATE_MW     <- 138.12       # g/mol, salicylic acid
ASPIRIN_K23 <- 2.32               # /h, pre-systemic -> ASA
ASPIRIN_K24 <- 0.57               # /h, pre-systemic -> SA

ASPIRIN_KA_ZERO  <- 1.032        # /h, zero-order path as one depot (fitted, see header)
ASPIRIN_LAG_ZERO <- 3.269        # h

# One first-order depot with a lag matching the mean and variance of a delay.
# Returns ka (/h) and lag (h).
aspirinDepot <- function(mean, var) {
  tau <- sqrt(var)
  c(ka = 1 / tau, lag = mean - tau)
}

#' Aspirin pharmacokinetics (enteric-coated, low dose; salicylate as metabolite)
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
aspirin <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1)
  wt   <- if (isTRUE(adjustToFFM)) size$pkWeight else weight

  k34 <- 2.97 * (wt / ASPIRIN_WT_MEDIAN)^1.31          # /h
  v3  <- 23.51 * size$volume

  # Absorption: two paths, each followed by the pre-systemic compartment
  kp   <- ASPIRIN_K23 + ASPIRIN_K24
  lag0 <- 2.81; fr <- 0.69; ka1 <- 0.053               # tablet
  zero  <- c(ka = ASPIRIN_KA_ZERO, lag = ASPIRIN_LAG_ZERO)
  first <- aspirinDepot(lag0 + 1 / ka1 + 1 / kp, 1 / ka1^2 + 1 / kp^2)
  toASA <- ASPIRIN_K23 / kp

  default <- list(
    v1  = v3,
    v2  = 1,
    v3  = 1,                                   # one compartment
    cl1 = k34 * v3 / 60,
    cl2 = 0,
    cl3 = 0,
    ka_PO              = zero[["ka"]] / 60,    # 1/min, zero-order path
    tlag_PO            = zero[["lag"]] * 60,   # min
    bioavailability_PO = toASA,
    ka_PO2             = first[["ka"]] / 60,   # 1/min, first-order path
    tlag_PO2           = first[["lag"]] * 60,  # min
    fraction_PO2       = 1 - fr
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Koh J et al., Drug Des Devel Ther 2025;19:7853-7863 (enteric-coated ",
    "tablet, 80-160 mg; one-compartment ASA, salicylate as metabolite; ",
    "absorption and pre-systemic steps approximated by two lagged depots). ",
    "https://doi.org/10.2147/DDDT.S533428"
  )

  list(
    PK = PK,
    tPeak = 0,
    MEAC = 0,
    typical = 0,
    upperTypical = 0,
    lowerTypical = 0,
    reference = reference,
    # Active itself, with no effect-site model: not a prodrug.
    prodrug = FALSE,
    metabolite = list(
      name              = "salicylate",
      kFormation        = k34 / 60,                  # 1/min, all of ASA's elimination
      firstPassFraction = 1 - toASA,                 # via k24, before the circulation
      mwRatio           = SALICYLATE_MW / ASPIRIN_MW
    )
  )
}
