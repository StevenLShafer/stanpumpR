# -----------------------------------------------------------------------------
# Cefalexin (cephalexin): oral only, apparent one-compartment model
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in mcg/mL (= mg/L), total plasma.
#
# ORAL ONLY, BECAUSE THE PARAMETERS ARE APPARENT
# ==============================================
# Haynes et al. fitted oral cefalexin (capsules and suspension) in 15
# children with musculoskeletal infection: CL/F 13.98 x (WT/70)^0.75 L/h,
# V/F 26.63 x (WT/70) L, ka 1.79 /h, no lag.  Those are clearance and volume
# divided by an unmeasured bioavailability.  They predict oral concentrations
# correctly, because F cancels, and intravenous ones wrong by 1/F, so
# cefalexin is offered as an oral unit only and bioavailability is carried
# as 1 because the apparent scale already contains it.  (There is no
# intravenous cefalexin product in any case.)
#
# PEDIATRIC ORIGIN, ADULT EXTRAPOLATION
# =====================================
# The 70 kg normalisation is a convention of the fit, not evidence that the
# model describes adults.  Ryder et al. (2026) used this vector to simulate
# adults, scaling both CL/F and V/F linearly with weight/70, supplemented it
# with oral pharmacokinetic anchors from published adult summary data, and
# judged it fit for that purpose: precedent for the extrapolation, and a
# check against adult means, but not a validation in adults.  The children had normal renal function and no renal covariate was
# fitted; the allometric weight term does not stand in for one.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# Published as allometry on total weight (volumes x WT/70, clearance x
# (WT/70)^0.75).  With the switch on the same exponents apply to the
# fat-free-mass ratio; with it off the published total-weight scaling is
# reproduced exactly.
#
# NOT MODELLED
# ============
# Protein binding (about 15%), renal impairment, food and formulation
# differences.  Doses are cefalexin equivalents of the monohydrate product.
#
# TIME UNTIL THRESHOLD: FREE DRUG AT THE MIC
# ==========================================
# The default threshold (endCe in drugDefaults_global.csv) is the TOTAL
# concentration at which FREE cefalexin equals the MIC of 4 mg/L for
# methicillin-susceptible S. aureus: the MSSA MIC90 (Haynes 2022: MIC50 2,
# MIC90 4 mg/L, none above 4) and the target of Haynes 2024's free-drug
# attainment analysis.  Neither CLSI nor EUCAST has a human cefalexin
# breakpoint for staphylococci (inferred from oxacillin or cefoxitin).
# Binding is low and linear, about 12-15% (label 10-15%; Singhvi 1977 12.4%
# by ultrafiltration), so the threshold is MIC / fu = 4 / 0.85 = 4.7 mg/L.
# See R/antibioticThresholds.R.
#
# References
# ----------
# Haynes AS et al., Antimicrob Agents Chemother 2024;68:e00182-24.
#   https://doi.org/10.1128/aac.00182-24
# Haynes AS et al., Microbiol Spectr 2022;10:e01039-22.
#   https://doi.org/10.1128/spectrum.01039-22
# Singhvi SM et al., J Lab Clin Med 1977;89:414-420.  PMID 833477.
# Ryder JH et al., Pharmacotherapy 2026;46(7):e70179.
#   https://doi.org/10.1002/phar.70179
# -----------------------------------------------------------------------------

#' Cefalexin pharmacokinetics (oral, apparent)
#'
#' @inheritParams cefazolin
#' @param adjustToFFM scale volumes to the patient's fat-free mass and
#'   clearances to that ratio to the 0.75 power; when \code{FALSE}, use the
#'   published allometry on total body weight (volumes linear, clearances to the
#'   0.75 power). No renal-function estimate.
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
cefalexin <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  # Size scaling (see the header): published allometry on total weight,
  # reproduced exactly with the switch off.
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyClearance = (weight / 70)^0.75)

  # Haynes 2024, apparent: CL/F 13.98 L/h, V/F 26.63 L at 70 kg
  v1  <- 26.63 * size$volume
  cl1 <- 13.98 * size$clearance / 60    # L/min
  v2  <- 1                              # one compartment
  v3  <- 1
  cl2 <- 0
  cl3 <- 0

  ka_PO              <- 1.79 / 60       # 1/min, as published
  bioavailability_PO <- 1               # the apparent scale already carries F
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

  tPeak <- 0     # no effect site: exposure against the MIC is the effect
  MEAC  <- 0

  # Band, total mg/L: the MICs of 1 to 4 mg/L of the S. aureus isolates in
  # Haynes 2022; streptococcal MICs are lower (0.06-0.5 mg/L, Ryder 2026).  Orientation only: MICs are free
  # drug, and with 15% binding the matching totals are about 18% higher.
  typical      <- 2
  upperTypical <- 4
  lowerTypical <- 1

  reference <- paste0(
    "Haynes AS et al., Antimicrob Agents Chemother 2024;68:e00182-24. ",
    "Apparent (CL/F, V/F) oral model fitted in children and extrapolated to ",
    "adults; oral only. https://doi.org/10.1128/aac.00182-24"
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
