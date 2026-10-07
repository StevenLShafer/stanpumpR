# -----------------------------------------------------------------------------
# Methylprednisolone: one-compartment intravenous model (Hong 2007) with an
# oral route from Al-Habet and Rogers 1989
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in mcg/mL, total plasma.
#
# DISPOSITION
# ===========
# Hong et al. gave intravenous methylprednisolone sodium succinate to five
# healthy men and fitted a one-compartment population model: CL 22.8 L/h,
# V 78.4 L, with F = 1 for the intravenous route.  Half-time 2.38 h.  No
# covariates were fitted.  The succinate ester hydrolyses to the active
# steroid with a half-time of about 4 min (Al-Habet), which the source
# absorbs into its parameters; the first few minutes after a fast bolus are
# the only place the approximation shows.
#
# ORAL ROUTE
# ==========
# Al-Habet and Rogers compared 20 mg tablets with intravenous succinate in
# five subjects: F = 0.82 (SD 0.11).  That bioavailability is used with
# Hong's disposition, a cross-study pairing.  Neither study yields an
# absorption constant, and the source specification left the oral input
# shape to be fitted.  stanpumpR needs one to offer the route at all, so
# METHYLPREDNISOLONE_KA_PO below is set to put the plasma peak at 90 min,
# within the 1-2 h that product information gives for the tablet.  It is a
# PROVISIONAL value and is flagged as such; the oral AUC (0.82 x dose / CL)
# does not depend on it, the oral peak does.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# Fixed published parameters for five men of ordinary size: volumes scale
# with fat-free mass relative to the reference male, clearances with that
# ratio ^ 0.75; the switch off uses them as published.
#
# References
# ----------
# Hong Y et al., Pharm Res 2007;24:1088-1097.
#   https://doi.org/10.1007/s11095-006-9232-x
# Al-Habet SM, Rogers HJ. Br J Clin Pharmacol 1989;27:285-290.
#   https://doi.org/10.1111/j.1365-2125.1989.tb05366.x
# -----------------------------------------------------------------------------

# PROVISIONAL: no published first-order absorption constant for the tablet
# was found.  Set so the oral plasma peak falls at 90 min against Hong's
# disposition.  See the header.
METHYLPREDNISOLONE_KA_PO <- 0.0212903403   # 1/min, = 1.277 /h

#' Methylprednisolone pharmacokinetics
#'
#' @inheritParams cefazolin
#' @param adjustToFFM scale volumes to the patient's fat-free mass and
#'   clearances to that ratio to the 0.75 power; when \code{FALSE}, use the
#'   published fixed parameters unscaled.
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
methylprednisolone <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  # Size scaling (see the header): fixed published parameters, unscaled
  # with the switch off.
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1)

  v1  <- 78.4 * size$volume
  cl1 <- 22.8 / 60 * size$clearance     # L/min
  v2  <- 1                              # one compartment
  v3  <- 1
  cl2 <- 0
  cl3 <- 0

  ka_PO              <- METHYLPREDNISOLONE_KA_PO
  bioavailability_PO <- 0.82            # Al-Habet and Rogers 1989
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

  # Band, mcg/mL: what 40-125 mg produce over the first hours.  Orientation.
  typical      <- 0.5
  upperTypical <- 1.5
  lowerTypical <- 0.2

  reference <- paste0(
    "Hong Y et al., Pharm Res 2007;24:1088-1097 (intravenous one-compartment ",
    "model); oral F 0.82 from Al-Habet and Rogers 1989; the oral absorption ",
    "constant is provisional. https://doi.org/10.1007/s11095-006-9232-x"
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
