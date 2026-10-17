# -----------------------------------------------------------------------------
# Cocaine: intranasal, parent plasma concentration only
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in ng/mL, venous plasma.
#
# RESEARCH MODEL, OPT-IN
# ======================
# Cocaine is in the library as an evidence-labelled research model, behind the
# "Illicit drugs" opt-in (off by default; see R/illicit-drugs.R).  It is a
# plasma-exposure estimate, not dosing advice, a safety threshold, an
# impairment or intoxication prediction, or an individual clinical prediction.
# It describes pharmaceutical cocaine hydrochloride given intranasally under
# the studied conditions, not street cocaine, whose purity, adulterants, route
# and co-exposures differ and commonly dominate the outcome.
#
# WHAT IS MODELLED, AND WHAT IS NOT
# =================================
# Parent cocaine in plasma, after an intranasal dose, and nothing else:
#
#   * No effect site (tPeak = 0).  Chow 1985 did estimate a biophase: it put
#     the peak chronotropic effect 7.3 min after an intravenous bolus, with an
#     initial slope of 0.3 beats/min per ng/mL, and it needed a FIRST-ORDER
#     ATTENUATION term, because the chronotropic effect declines faster than
#     either the plasma or the biophase concentration.  That attenuation is
#     outside the library's single-ke0 effect site, and a ke0 taken without it
#     would over-predict sustained tachycardia, so no effect site is offered.
#     No source establishes a universal cocaine euphoria EC50 or a toxicity
#     threshold, so there is no MEAC and no concentration band.
#   * No metabolites.  Benzoylecgonine, ecgonine methyl ester, norcocaine and
#     (with ethanol) cocaethylene are separate analytes with their own
#     formation and elimination; their formation fractions are not identified
#     by the sources used here, and the engine resolves one metabolite level
#     anyway.  They are not modelled.
#
# THE DISPOSITION IS A DOCUMENTED ONE-COMPARTMENT REDUCTION
# =========================================================
# Chow and colleagues fitted a TWO-compartment model in five adults and
# published its derived summaries: steady-state volume of distribution
# 131.8 L (1.96 L/kg), elimination clearance 2.10 L/min and a terminal
# half-life of 48 min.  The micro-constants (V1, Q, V2) are not in the
# accessible report, so the two-compartment model cannot be rebuilt here.
#
# This model is therefore a ONE-COMPARTMENT reduction that uses Chow's
# steady-state volume as a SURROGATE for the central volume:
#
#     v1  = 131.8 L        (Vss used as a one-compartment volume)
#     cl1 = 2.10 L/min
#     v2 = v3 = 1, cl2 = cl3 = 0
#
# giving an elimination rate constant of 0.015933/min and a half-life of
# 43.5 min, between Javaid's independent one-compartment intravenous fit
# (41.4 min) and Chow's 48 min.
#
# What the reduction loses is the early distribution phase: Jeffcoat measured
# a distinct distribution half-life of 11 min after an intravenous dose, with
# an elimination half-life of 78 min.  A one-compartment model with Vss as its
# volume therefore UNDER-predicts the first minutes after an intravenous bolus
# badly (the true central volume is much smaller than Vss), which is why
# INTRAVENOUS COCAINE IS NOT OFFERED.  Only the intranasal route, whose
# concentrations are absorption-limited rather than distribution-limited, is
# offered.  See inst/extdata/illicitRouteRegistry.csv for the route-by-route
# evidence status.
#
# ABSORPTION: ONE INTRANASAL PATH, WHICH IS THE SIMPLIFICATION
# ============================================================
# Jeffcoat measured intranasal absorption directly: a single absorption
# half-time of 11.7 min (ka = 0.059243/min) with a bioavailability of about
# 80%.  Both are used as published.
#
# Fattinger's deconvolution of nasal cocaine shows what one path leaves out:
# systemic input after a nasal dose has TWO peaks, near 10 and 45 min, because
# part of the dose is absorbed across the nasal mucosa (19% of the nasal dose,
# 95% CI 11-26%, contributing 31% of systemic exposure, 95% CI 23-37%) and the
# rest is swallowed and absorbed from the gut (median oral bioavailability
# 33%).  Those three percentages have three different denominators and must
# never be combined as if they were one mass balance.  A two-path nasal input
# needs the deposited fractions and each path's bioavailability fitted jointly
# to concentration-time data, which is not done here; this model is Jeffcoat's
# single path, and the help page says so.
#
# One consequence is worth stating plainly: the exposure this model predicts
# is Jeffcoat's.  His bioavailability of about 80% is higher than Fattinger's
# protocol implies (19% of the nasal dose absorbed across the mucosa, with
# swallowed drug at a median oral bioavailability of 33%), so a nasal dose
# simulated here reaches a higher concentration than Fattinger's subjects
# would suggest.  The difference is the two protocols', not an error in
# either.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# Neither source published a size covariate, so the parameters are those of a
# reference adult and the package's own scaling is applied: with the fat-free
# mass switch on, volumes scale with the Al-Sallami fat-free mass ratio and
# clearance with its 0.75 power; with it off, volumes and clearance scale
# linearly with weight / 70 (pkSizeFactors()).  That scaling is an
# IMPLEMENTATION CONVENTION, not a published population fit.  Chow's subjects
# averaged about 67 kg (131.8 L / 1.96 L/kg), so reading his absolute
# parameters as those of a 70 kg adult shifts them by about 4%; the per-kg
# values are preserved in this header.  The absorption rate constant is not
# scaled.
#
# References
# ----------
# Chow MJ, Ambre JJ, Ruo TI, et al.  Kinetics of cocaine distribution,
#   elimination, and chronotropic effects.  Clin Pharmacol Ther
#   1985;38(3):318-324.  https://doi.org/10.1038/clpt.1985.179  (five adults,
#   two-compartment model: Vss 131.8 L = 1.96 L/kg, clearance 2.10 L/min,
#   terminal half-life 48 min; the biophase chronotropic analysis).
# Javaid JI, Musa MN, Fischman M, et al.  Kinetics of cocaine in humans after
#   intravenous and intranasal administration.  Biopharm Drug Dispos
#   1983;4(1):9-18.  https://doi.org/10.1002/bdd.2510040104  (one-compartment
#   intravenous fit, half-life 41.4 +/- 8.2 min, mean +/- SEM, n = 4).
# Jeffcoat AR, Perez-Reyes M, Hill JM, et al.  Cocaine disposition in humans
#   after intravenous injection, nasal insufflation (snorting), or smoking.
#   Drug Metab Dispos 1989;17(2):153-159.
#   https://pubmed.ncbi.nlm.nih.gov/2565204/  (intranasal absorption half-time
#   11.7 min and bioavailability about 80%; intravenous distribution half-life
#   11 min, elimination half-life 78 min).
# Fattinger K, Benowitz NL, Jones RT, Verotta D.  Nasal mucosal versus
#   gastrointestinal absorption of nasally administered cocaine.  Eur J Clin
#   Pharmacol 2000;56(4):305-310.  https://doi.org/10.1007/s002280000147
#   (two-path nasal input; why the single path here is a simplification).
# -----------------------------------------------------------------------------

# Chow 1985, read as the parameters of a 70 kg reference adult (see header)
COCAINE_V        <- 131.8      # L, Chow's Vss used as a one-compartment volume
COCAINE_CL       <- 2.10       # L/min, Chow's elimination clearance
# Jeffcoat 1989, intranasal
COCAINE_KA_IN    <- log(2) / 11.7   # 1/min, absorption half-time 11.7 min
COCAINE_F_IN     <- 0.80            # intranasal bioavailability

#' Cocaine pharmacokinetics: parent plasma, intranasal
#'
#' @param weight patient weight (kg)
#' @param height patient height (cm)
#' @param age patient age (years)
#' @param sex \code{"male"} or \code{"female"}
#' @param adjustToFFM scale to the Al-Sallami fat-free mass; when \code{FALSE},
#'   scale linearly with weight / 70 (neither source published a size
#'   covariate, so both are implementation conventions: see the header)
#' @returns a list in the shape \code{getDrugPK()} expects; plasma only, no
#'   effect site and no modelled metabolite (see the header)
#' @export
cocaine <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)

  v1  <- COCAINE_V  * size$volume
  cl1 <- COCAINE_CL * size$clearance
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
    # Intranasal: Jeffcoat's single absorption path
    ka_IN = COCAINE_KA_IN,
    bioavailability_IN = COCAINE_F_IN,
    tlag_IN = 0
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  tPeak <- 0     # parent plasma only; Chow's biophase needs an attenuation
                 # term the engine cannot carry (see the header)
  MEAC  <- 0

  # No established plasma range applies to this model, so no band is drawn
  # (helpNoBand(); all three columns are 0 in the CSV).
  typical      <- 0
  upperTypical <- 0
  lowerTypical <- 0

  reference <- paste0(
    "Chow MJ et al., Clin Pharmacol Ther 1985;38(3):318-324 ",
    "(https://doi.org/10.1038/clpt.1985.179), disposition, reduced to one ",
    "compartment with Vss as its volume; Jeffcoat AR et al., Drug Metab ",
    "Dispos 1989;17(2):153-159 (https://pubmed.ncbi.nlm.nih.gov/2565204/), ",
    "intranasal absorption and bioavailability. Parent cocaine plasma only, ",
    "intranasal only; no effect site, no metabolites; research use, not ",
    "dosing advice."
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
