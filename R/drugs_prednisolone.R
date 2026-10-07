# -----------------------------------------------------------------------------
# Prednisolone, and the prednisone-prednisolone pair (Xu, Winkler and
# Derendorf 2007)
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in ng/mL of FREE (unbound) prednisolone.
#
# THE SOURCE MODEL IS A REVERSIBLE PAIR
# =====================================
# Prednisone (PN) and prednisolone (PL) interconvert by 11-beta-hydroxysteroid
# dehydrogenase.  Xu et al. fitted both species simultaneously, on a molar
# basis, to digitised human oral and intravenous profiles: one compartment
# for each, linked both ways, with the published clearance/volume vector
#
#     VN 397.3 L   CLN 101.0 L/h   LNL (PN -> PL) 90.64 L/h
#     VL 110.4 L   CLL  36.37 L/h  LLN (PL -> PN) 34.78 L/h
#
# on FREE concentrations.  Total prednisone is 4 x free (a constant free
# fraction of 0.25); total prednisolone is a nonlinear, cortisol-dependent
# function of free that is not applied here, which is why this row plots
# free prednisolone.  The volumes are effective free-concentration volumes,
# not plasma spaces.  This is a typical-profile fit, not an individual-data
# population model.
#
# HOW A REVERSIBLE PAIR FITS A MAMMILLARY ENGINE
# ==============================================
# After a dose of prednisolone, the free PL concentration is biexponential
# with the eigenvalues of the two-state linear system (half-times 0.82 and
# 2.45 h).  Any biexponential bolus response with positive coefficients is
# reproduced EXACTLY by a two-compartment mammillary model whose central
# volume is VL: the prednisone pool plays the part of the peripheral
# compartment.  prednisolonePairSystem() below does that algebra, giving
# CL_eff 54.70 L/h, Q 16.45 L/h, V2 34.10 L.  The equivalence is exact for
# an intravenous PL dose and for any infusion of PL.
#
# An ORAL prednisolone dose is not only PL: Xu's entry split sends 0.5888 of
# the dose into the circulation as PL and 0.3312 as PN (0.08 is lost), and
# the PN part becomes PL by conversion.  The engine's single first-order
# input cannot carry two species, so the oral route uses one effective
# bioavailability, 0.7454, chosen so the free PL AUC equals the exact value
# from the source's AUC identity, and the source's PL absorption constant
# 0.42 /h.  The oral AUC is exact; the oral SHAPE is approximate, because the
# PN-entered fraction reaches PL a little later than the direct fraction.
#
# Prednisone is handled in R/drugs_prednisone.R as a prodrug whose
# metabolite row is this one.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# Fixed published parameters: volumes scale with fat-free mass relative to
# the reference male, clearances with that ratio ^ 0.75; the switch off uses
# them as published.
#
# References
# ----------
# Xu J, Winkler J, Derendorf H. J Pharmacokinet Pharmacodyn 2007;34:355-372.
#   https://doi.org/10.1007/s10928-007-9050-8
# -----------------------------------------------------------------------------

# Xu 2007 free-concentration vector, L and L/h
XU_VN  <- 397.3
XU_VL  <- 110.4
XU_CLN <- 101.0
XU_CLL <- 36.37
XU_LNL <- 90.64     # prednisone  -> prednisolone
XU_LLN <- 34.78     # prednisolone -> prednisone
XU_FU_PREDNISONE <- 0.25   # constant free fraction; total PN = free / 0.25

# Oral entry coefficients (entry fraction x split), applied once
XU_ORAL_PN_AS_PN <- 0.105    # of an oral prednisone dose, entering as PN
XU_ORAL_PN_AS_PL <- 0.645    # ... entering as PL (presystemic conversion)
XU_ORAL_PL_AS_PN <- 0.3312   # of an oral prednisolone dose, entering as PN
XU_ORAL_PL_AS_PL <- 0.5888   # ... entering as PL
XU_KA_PN <- 1.08             # /h, oral prednisone
XU_KA_PL <- 0.42             # /h, oral prednisolone

#' Mammillary equivalents of the Xu prednisone-prednisolone system
#'
#' Reduces the two-state reversible system to the two-compartment mammillary
#' models the engine runs: one centred on prednisolone (for the prednisolone
#' row) and one centred on prednisone (for the prednisone row), each exact
#' for a bolus of its own species.  Also returns the effective oral
#' bioavailability of prednisolone and the formation constant that lets the
#' engine's one-way metabolite convolution reproduce the exact prednisolone
#' AUC after prednisone.
#'
#' @returns a list with \code{PL} and \code{PN} (each v1, v2, cl1, cl2 in L
#'   and L/h, on FREE concentration), \code{eigen} (per hour),
#'   \code{oralF_PL}, \code{kFormation} (per hour), and the effective oral
#'   prednisone coefficients \code{oralF_PN} and \code{firstPass_PN}
#' @keywords internal
prednisolonePairSystem <- function()
{
  a <- (XU_CLN + XU_LNL) / XU_VN     # PN exit rate
  b <- XU_LLN / XU_VL                # PL -> PN, per unit PL amount
  cc <- XU_LNL / XU_VN               # PN -> PL, per unit PN amount
  d <- (XU_CLL + XU_LLN) / XU_VL     # PL exit rate

  disc <- sqrt((a - d)^2 + 4 * b * cc)
  lambda1 <- ((a + d) + disc) / 2
  lambda2 <- ((a + d) - disc) / 2

  # A unit bolus into the central species gives an amount
  # C1 exp(-lambda1 t) + C2 exp(-lambda2 t) with C1 + C2 = 1 and an initial
  # slope of minus its own exit rate.  The mammillary model with the same
  # two exponentials has k21 = C1 lambda2 + C2 lambda1, k10 = lambda1
  # lambda2 / k21, and k12 the remainder of the trace.
  mammillary <- function(exitRate, v1) {
    C1 <- (exitRate - lambda2) / (lambda1 - lambda2)
    C2 <- (lambda1 - exitRate) / (lambda1 - lambda2)
    k21 <- C1 * lambda2 + C2 * lambda1
    k10 <- lambda1 * lambda2 / k21
    k12 <- lambda1 + lambda2 - k21 - k10
    list(v1 = v1, v2 = v1 * k12 / k21, cl1 = k10 * v1, cl2 = k12 * v1)
  }
  PL <- mammillary(d, XU_VL)
  PN <- mammillary(a, XU_VN)

  # Complete-washout AUC identity: M %*% c(AUC_N, AUC_L) = systemic input
  M <- matrix(c(XU_CLN + XU_LNL, -XU_LLN,
                -XU_LNL,         XU_CLL + XU_LLN), 2, 2, byrow = TRUE)
  aucPLbolus <- solve(M, c(0, 1))
  aucPNbolus <- solve(M, c(1, 0))
  aucPLoral  <- solve(M, c(XU_ORAL_PL_AS_PN, XU_ORAL_PL_AS_PL))
  aucPNoral  <- solve(M, c(XU_ORAL_PN_AS_PN, XU_ORAL_PN_AS_PL))

  # The engine's one-way metabolite link makes the prednisone row stand in
  # for ALL systemic prednisone after an oral prednisone dose, including the
  # prednisone regenerated from prednisolone that entered first.  Two
  # effective coefficients make both exposures exact: an oral F for the
  # prednisone row giving the exact total-prednisone AUC, and a first-pass
  # fraction giving the exact prednisolone AUC once the systemic branch
  # (kFormation below) has been accounted for.
  kFormation <- aucPNbolus[2] * PL$cl1 / (XU_VN * aucPNbolus[1])
  oralF_PN   <- aucPNoral[1] / aucPNbolus[1]
  firstPass_PN <- (aucPNoral[2] - oralF_PN * aucPNbolus[2]) / aucPLbolus[2]

  list(
    PL = PL,
    PN = PN,
    eigen = c(lambda1, lambda2),
    # The oral F that gives the exact free PL AUC with the PL mammillary model
    oralF_PL = aucPLoral[2] / aucPLbolus[2],
    # kFormation x (PN amount AUC) / CL_eff,L equals the exact PL AUC after
    # a PN bolus.  Smaller than LNL/VN because the PL model's own peripheral
    # pool already carries the PL -> PN -> PL recycling.
    kFormation = kFormation,
    oralF_PN = oralF_PN,
    firstPass_PN = firstPass_PN
  )
}

#' Prednisolone pharmacokinetics (free concentration)
#'
#' @inheritParams cefazolin
#' @param adjustToFFM scale volumes to the patient's fat-free mass and
#'   clearances to that ratio to the 0.75 power; when \code{FALSE}, use the
#'   published fixed parameters unscaled.
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
prednisolone <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  # Size scaling (see the header): fixed published parameters, unscaled
  # with the switch off.
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1)
  sys  <- prednisolonePairSystem()

  v1  <- sys$PL$v1 * size$volume
  v2  <- sys$PL$v2 * size$volume
  v3  <- 1                                  # no third compartment
  cl1 <- sys$PL$cl1 / 60 * size$clearance   # L/min
  cl2 <- sys$PL$cl2 / 60 * size$clearance
  cl3 <- 0

  ka_PO              <- XU_KA_PL / 60       # 1/min
  bioavailability_PO <- sys$oralF_PL        # 0.7454, AUC-exact; see the header
  tlag_PO            <- 0

  default <- list(
    v1 = v1,
    v2 = v2,
    v3 = v3,
    cl1 = cl1,
    cl2 = cl2,
    cl3 = cl3,
    ka_PO = ka_PO,
    bioavailability_PO = bioavailability_PO,
    tlag_PO = tlag_PO
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  tPeak <- 0     # genomic effect over hours; no effect-site model
  MEAC  <- 0

  # Band, FREE prednisolone ng/mL: what 20-40 mg produce.  Orientation only.
  typical      <- 40
  upperTypical <- 100
  lowerTypical <- 10

  reference <- paste0(
    "Xu J, Winkler J, Derendorf H. J Pharmacokinet Pharmacodyn 2007;34:355-372. ",
    "Reversible prednisone-prednisolone pair reduced to its exact mammillary ",
    "equivalent; the plotted concentration is FREE prednisolone. ",
    "https://doi.org/10.1007/s10928-007-9050-8"
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
