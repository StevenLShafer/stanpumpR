# -----------------------------------------------------------------------------
# Psilocybin: oral, plotted as free plasma PSILOCIN, apparent parameters
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in ng/mL, venous plasma.
#
# RESEARCH MODEL, OPT-IN
# ======================
# Psilocybin is in the library as an evidence-labelled research model, behind
# the "Illicit drugs" opt-in (off by default; see R/illicit-drugs.R).  It is a
# plasma-exposure estimate, not dosing advice, a safety threshold, or a
# prediction of any individual's experience.  It describes synthetic
# psilocybin of ASSAYED content given orally in a controlled study; mushroom
# material and teas vary in content and cannot be dosed by weight of material.
#
# THE DOSE IS PSILOCYBIN, THE CURVE IS PSILOCIN
# =============================================
# Psilocybin is a prodrug: it is dephosphorylated to psilocin, and it is
# UNCONJUGATED (free) plasma psilocin that Holze and colleagues measured and
# modelled.  So the dose entered is milligrams of psilocybin, and the plotted
# concentration is free psilocin in ng/mL.
#
# The two are different molecules, so the amounts differ by their molecular
# weights: psilocin 204.27 g/mol over psilocybin 284.25 g/mol = 0.718628.
# One milligram of psilocybin can yield at most 0.718628 mg of psilocin.  That
# factor is STOICHIOMETRY, and is separate from absorption, dephosphorylation
# and systemic availability, none of which is identified here.
#
# APPARENT PARAMETERS, SO ORAL ONLY, AND THE DOSE BASIS IS CONVERTED
# ==================================================================
# Holze fitted a one-compartment model with first-order absorption and a lag
# time, and reported apparent clearance and volume -- divided by an unmeasured
# bioavailability -- on a PSILOCIN-EQUIVALENT dose basis (the assayed
# psilocybin content converted to psilocin by the factor above).  Apparent
# parameters predict oral concentrations correctly and intravenous ones wrong
# by 1/F, so psilocybin is offered as an ORAL unit only, with bioavailability
# carried as 1 because the apparent scale already contains it
# (docs/adding-a-drug.md).
#
# Because this model takes its dose in milligrams of PSILOCYBIN rather than
# psilocin equivalents, the input amount is 1/0.718628 times the source's, so
# BOTH apparent parameters are rescaled by that same factor:
#
#     V/F  = 448 L   / 0.718628 = 623.4 L
#     CL/F = 172 L/h / 0.718628 = 239.3 L/h
#
# Their ratio, and therefore every predicted concentration and the half-life,
# are unchanged: this is a change of dose basis, not of the model.  Applying a
# separate bioavailability on top of these would double-discount the exposure.
#
# THE DOSE ARM IMPLEMENTED
# ========================
# Holze fitted each dose arm separately.  This model implements the nominal
# 15 mg arm (assayed content 13.8 mg psilocybin = 9.9 mg psilocin equivalent):
#
#     ka   = 0.89 /h    (0.0148333 /min)
#     tlag = 0.30 h     (18 min)
#     CL/F = 172 L/h    on a psilocin-equivalent basis
#     V/F  = 448 L      on a psilocin-equivalent basis
#
# which gives an elimination half-life of 1.81 h and a peak at 1.96 h,
# reproducing the paper's published half-life of 1.8 h (95% CI 1.7-2.0) and
# its "maximal concentrations were reached after an average of 2 hours", and a
# peak free psilocin of about 11.7 ng/mL for that arm's assayed content,
# against the published 11 ng/mL (95% CI 10-13).
#
# The 25 mg and 30 mg arms were fitted separately and differ modestly (CL/F
# 199 and 175 L/h, V/F 413 and 450 L, ka 0.77 and 0.75 /h); the library
# carries one parameter set per drug, so larger doses are simulated here with
# the 15 mg arm's parameters and are therefore predicted dose-proportionally.
# Do not mix these with other reports: an independent oral study (Brown 2017)
# fitted different apparent parameters and an inferred glucuronide
# compartment, and a separate trial reports different peak concentrations.
#
# NO EFFECT SITE
# ==============
# tPeak = 0, MEAC = 0, no band.  Madsen's positron emission tomography study
# in eight volunteers showed that plasma psilocin and cerebral 5-HT2A receptor
# occupancy "conformed to a single-site binding model", with occupancies up to
# 72%, which is a direct function of plasma rather than an equilibration
# delay; subjective intensity is not a pharmacokinetic end point the library
# can plot.  Holze's own effect-site parameters are in a supplement that was
# not read for this implementation, so no ke0, EC50 or Emax is coded.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# Holze published no size covariate, so the apparent parameters are those of a
# reference adult: unscaled with the fat-free-mass switch off
# (legacyVolume = 1), and with it on, volumes scale with the Al-Sallami
# fat-free mass ratio and clearance with its 0.75 power.  That scaling is an
# IMPLEMENTATION CONVENTION, not a published fit.
#
# References
# ----------
# Holze F, Becker AM, Kolaczynska KE, Duthaler U, Liechti ME.
#   Pharmacokinetics and pharmacodynamics of oral psilocybin administration in
#   healthy participants.  Clin Pharmacol Ther 2023;113(4):822-831.
#   https://doi.org/10.1002/cpt.2821  (15, 25 and 30 mg in 28, 23 and 28
#   participants; unconjugated plasma psilocin; peak concentrations 11, 17 and
#   21 ng/mL reached after an average of 2 h; half-lives 1.8, 1.4 and 1.8 h).
# Madsen MK, Fisher PM, Burmester D, et al.  Psychedelic effects of psilocybin
#   correlate with serotonin 2A receptor occupancy and plasma psilocin levels.
#   Neuropsychopharmacology 2019;44(7):1328-1334.
#   https://doi.org/10.1038/s41386-019-0324-9  (single-site binding of
#   occupancy on plasma psilocin, eight volunteers).
# -----------------------------------------------------------------------------

# Molecular weights (g/mol): the stoichiometric ceiling on psilocin from
# psilocybin, which is NOT a bioavailability (see the header)
PSILOCYBIN_MW <- 284.25
PSILOCIN_MW   <- 204.27
PSILOCIN_PER_PSILOCYBIN <- PSILOCIN_MW / PSILOCYBIN_MW   # 0.718628

# Holze 2023, the nominal 15 mg arm, on a psilocin-equivalent dose basis
PSILOCYBIN_KA        <- 0.89 / 60    # 1/min, from 0.89 /h
PSILOCYBIN_TLAG      <- 0.30 * 60    # min, from 0.30 h
PSILOCYBIN_CL_F_SRC  <- 172          # L/h, apparent clearance as published
PSILOCYBIN_V_F_SRC   <- 448          # L, apparent volume as published

# The same model with the dose entered as psilocybin rather than psilocin
# equivalents: both apparent parameters rescaled by the same factor, so the
# predicted concentrations are unchanged (see the header)
PSILOCYBIN_CL_F <- PSILOCYBIN_CL_F_SRC / PSILOCIN_PER_PSILOCYBIN / 60  # L/min
PSILOCYBIN_V_F  <- PSILOCYBIN_V_F_SRC  / PSILOCIN_PER_PSILOCYBIN       # L

#' Psilocybin pharmacokinetics: free plasma psilocin after an oral dose
#'
#' @param weight patient weight (kg)
#' @param height patient height (cm)
#' @param age patient age (years)
#' @param sex \code{"male"} or \code{"female"}
#' @param adjustToFFM scale to the Al-Sallami fat-free mass; when \code{FALSE},
#'   use the published parameters unscaled (the source fitted no size
#'   covariate)
#' @returns a list in the shape \code{getDrugPK()} expects; free plasma
#'   psilocin after an oral psilocybin dose, no effect site (see the header)
#' @export
psilocybin <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1)

  v1  <- PSILOCYBIN_V_F  * size$volume
  cl1 <- PSILOCYBIN_CL_F * size$clearance
  v2  <- 1                       # one compartment
  v3  <- 1
  cl2 <- 0
  cl3 <- 0

  default <- list(
    v1 = v1,
    v2 = v2,
    v3 = v3,
    cl1 = cl1,
    cl2 = cl2,
    cl3 = cl3,
    # Oral: apparent parameters, so bioavailability is 1 by convention; the
    # psilocybin-to-psilocin conversion is carried by the rescaled parameters
    ka_PO = PSILOCYBIN_KA,
    bioavailability_PO = 1,
    tlag_PO = PSILOCYBIN_TLAG
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  tPeak <- 0     # occupancy follows plasma directly; see the header
  MEAC  <- 0

  # No established plasma range applies to this model, so no band is drawn.
  typical      <- 0
  upperTypical <- 0
  lowerTypical <- 0

  reference <- paste0(
    "Holze F et al., Clin Pharmacol Ther 2023;113(4):822-831 ",
    "(https://doi.org/10.1002/cpt.2821). One-compartment oral model, nominal ",
    "15 mg arm; the dose is psilocybin and the curve is free plasma psilocin, ",
    "with the apparent parameters rescaled from the paper's ",
    "psilocin-equivalent dose basis by the molecular weight ratio 0.718628. ",
    "Plasma only, no effect site; research use, not dosing advice."
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
