# -----------------------------------------------------------------------------
# Vancomycin: two-compartment model with a creatinine-clearance covariate
# (Thomson 2009)
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in mcg/mL (= mg/L), total serum.
#
# DISPOSITION
# ===========
# Thomson et al. fitted total serum vancomycin from 398 hospitalised patients
# (creatinine clearance 12-216 mL/min, renal replacement excluded) and
# evaluated the model in 100 more:
#
#     CL = 2.99 x [1 + 0.0154 x (CrCL - 66)]   L/h
#     Vc = 0.675 x W                           L
#     Vp = 0.732 x W                           L
#     Q  = 2.28                                L/h
#
# with W total body weight and CrCL the Cockcroft-Gault creatinine clearance
# in mL/min, serum creatinine floored at 60 umol/L.  The source table labels
# Q in h^-1 but identifies it as an intercompartmental CLEARANCE; L/h is the
# dimensionally consistent reading and 2.28 is not a rate constant.
#
# The clearance equation is linear in CrCL and reaches zero at 1.06 mL/min.
# It is not a model of anuria or dialysis, and the function refuses rather
# than clamps if it is ever driven there.
#
# RENAL FUNCTION
# ==============
# CrCL is Cockcroft-Gault at the patient's serum creatinine from the Patient
# Profile, floored at 60 umol/L (0.68 mg/dL) as in the source.  When none is
# entered it is an ASSUMED NORMAL creatinine (R/renalFunction.R), which
# captures the decline of renal function with age and the sex difference but
# not renal impairment; for accumulation in a patient with a raised
# creatinine, enter it.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# The source scaled both volumes linearly with total weight, carried weight
# into clearance through Cockcroft-Gault, and left Q fixed.  With the switch
# on, the weight those terms see is the pharmacokinetic weight
# (70 kg x FFM / FFM_ref), and Q follows the library convention for a
# size-free clearance (x FFM ratio ^ 0.75).  With it off, total body weight
# enters as published and Q is 2.28 L/h for everyone.
#
# TARGET
# ======
# The 2020 ASHP/IDSA/PIDS/SIDP consensus (Rybak 2020) recommends a total
# AUC over 24 h of 400-600 mg.h/L, assuming an MIC of 1 mg/L, for serious
# MRSA infection, in place of the older trough of 15-20 mg/L.  At steady
# state AUC24 = daily dose / CL.  The band below is the traditional trough
# range because that is what is read off a concentration plot; the AUC is
# the better target and can be computed from the curve.
#
# References
# ----------
# Thomson AH et al., J Antimicrob Chemother 2009;63:1050-1057.
#   https://doi.org/10.1093/jac/dkp085
# Rybak MJ et al., Am J Health Syst Pharm 2020;77:835-864.
#   https://doi.org/10.1093/ajhp/zxaa036
# -----------------------------------------------------------------------------

#' Vancomycin pharmacokinetics
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
vancomycin <- function(weight, height, age, sex, adjustToFFM = TRUE,
                       creatinine = NULL)
{
  # Size scaling (see the header).  size$volume is the weight ratio the
  # published volume and renal terms see (fat-free-mass ratio with the switch
  # on, weight/70 with it off); size$clearance scales the size-free Q, and is
  # 1 with the switch off because the source left Q fixed.
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume = weight / 70, legacyClearance = 1)
  pkW  <- 70 * size$volume

  # Thomson floored serum creatinine at 60 umol/L before Cockcroft-Gault.
  scr  <- max(patientCreatinine(creatinine, sex), 60 / 88.42)
  crcl <- creatinineClearanceCG(pkW, age, sex, scr)   # mL/min

  cl1 <- 2.99 * (1 + 0.0154 * (crcl - 66)) / 60      # L/min
  if (cl1 <= 0)
    stop("The Thomson vancomycin clearance is not defined at a creatinine ",
         "clearance of ", round(crcl, 1), " mL/min")
  v1  <- 0.675 * pkW
  v2  <- 0.732 * pkW
  cl2 <- 2.28 / 60 * size$clearance
  v3  <- 1                                            # no third compartment
  cl3 <- 0

  default <- list(
    v1 = v1,
    v2 = v2,
    v3 = v3,
    cl1 = cl1,
    cl2 = cl2,
    cl3 = cl3
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  tPeak <- 0     # no effect site: exposure against the MIC is the effect
  MEAC  <- 0

  # Band, total mg/L: the traditional trough range of 10-20 mg/L.  The
  # consensus target is an AUC24 of 400-600 mg.h/L, see the header.
  typical      <- 15
  upperTypical <- 20
  lowerTypical <- 10

  reference <- paste0(
    "Thomson AH et al., J Antimicrob Chemother 2009;63:1050-1057. ",
    "Two-compartment total-serum model with Cockcroft-Gault creatinine ",
    "clearance, from the entered creatinine or an assumed normal one. ",
    "https://doi.org/10.1093/jac/dkp085"
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
