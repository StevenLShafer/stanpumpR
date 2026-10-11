# -----------------------------------------------------------------------------
# Morphine
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min.
#
# DISPOSITION
# ===========
# Lotsch 2002 (Model A), three compartments from intravenous morphine in eight
# young volunteers, with tPeak 93.8 min after an intravenous bolus.
#
# ORAL: IMMEDIATE-RELEASE TABLET AND LIQUID
# =========================================
# Two oral formulations are offered, "mg PO tablet" and "mg PO liquid" (see
# ORAL_FORMULATIONS, R/constants.R).  The tablet is the model's default oral
# absorption (ka_PO, bioavailability_PO, tlag_PO); the liquid is listed in
# `oralFormulations` and has absorption of its own, which simCpCe() runs
# alongside on the same disposition and effect site.
#
# Both are calibrated to one crossover programme, Atrux-Tallau 2022, of
# morphine sulfate in healthy fasted adults (naltrexone block for 30 mg).  It
# ran two kinds of study, and they are used for different things:
#
#   pivotal (n = 39), 3 x 10 mg Sevredol tablets: Cmax 28.5 ng/mL, median
#     tmax 0.75 h, AUC 117.4 ng.h/mL.  Sets the tablet's absorption and the
#     bioavailability of both forms.
#   pilot (n = 17), one crossover of 30 mg tablets, capsule, orodispersible
#     tablet and 30 mg/5 mL Oramorph solution: tablet Cmax within 5% of the
#     ODT's 28.5 ng/mL, solution Cmax 37.9 ng/mL (about 30% higher), median
#     tmax 0.75 h for both, and AUC within 10% across every formulation.
#     Sets how much faster the liquid is absorbed.
#
# Bioavailability is set from the pivotal tablet AUC on Lotsch's clearance
# (74.0 L/h at the 70 kg reference): F = CL x AUC / dose = 0.290, per
# labelled mg of morphine SULFATE, which is how oral morphine is prescribed;
# it sits inside the 0.2 to 0.4 usually quoted.  The liquid is given the same
# F: within the pilot, where the two were compared in the same subjects, the
# formulations' AUCs agreed to within 10%.  The solution's published AUC
# (121.8 ng.h/mL) is a pilot figure and is NOT compared with the pivotal
# tablet's, which would be a cross-study comparison.  What differs between
# the forms is how fast they are absorbed.
#
# Absorption is first order after a lag.  Without a lag, a tablet ka fitted to
# the observed Cmax peaks at 28 min against an observed 45; with one, the
# tablet's ka (0.0130 /min, half-time 53 min) and lag (17.6 min) reproduce
# both Cmax and tmax.  The liquid shares that lag, which the engine requires
# of formulations that are added together, so its absorption constant is
# what differs: ka 0.0189 /min (half-time 37 min) reproduces its higher pilot
# Cmax and puts its peak at 35 min, between the 0.5 and 0.75 h samples and
# consistent with the observed median given the sampling.  A shared lag reads
# as gastric emptying, which a swallowed solution waits for too.
#
# Caveats.  These are means of individual Cmax and tmax fitted as one curve,
# which overstates the rate of absorption a little.  The subjects were
# fasted; food slows and can enlarge oral morphine absorption.  Modified-
# release morphine (MS Contin and others) is a different input and is not
# represented.  Morphine-6-glucuronide, which is active and is formed far
# more after oral than after intravenous morphine because of first-pass
# glucuronidation, is not modelled, so the effect of an oral dose is
# understated relative to an intravenous one at the same morphine
# concentration.
#
# References
# ----------
# Lotsch J, Skarke C, Schmidt H, Liefhold J, Geisslinger G.  Clin Pharmacol
#   Ther 2002;72:151-162.  https://doi.org/10.1067/mcp.2002.126172
# Atrux-Tallau N, Naimi Z, Jaudinot EO.  Clin Drug Investig 2022;42:1101-1112.
#   https://doi.org/10.1007/s40261-022-01214-x
# (Oral routes added by Claude Code, 2026-10-10, at the request of Steven L.
# Shafer.)
# -----------------------------------------------------------------------------

morphine <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  # Units **************
  # Time: Minutes
  # Volume: Liters
  
  v1Ref <- 0.25 * 70   # 0.25 L/kg at the 70 kg reference
  k10 <- 0.070505618
  k12 <- 0.127340824
  k13 <- 0.018258427
  k21 <- 0.025964108
  k31 <- 0.001633166
  
  tPeak <- 93.8
  MEAC <- 8/1000
  typical <- MEAC * 1.2
  upperTypical <- MEAC * 0.8
  lowerTypical <- MEAC * 2.0
  reference <- paste0(
    "Lotsch J et al., Clin Pharmacol Ther 2002;72(2):151-162. ",
    "https://pubmed.ncbi.nlm.nih.gov/12189362/ (intravenous); ",
    "Atrux-Tallau N et al., Clin Drug Investig 2022;42:1101-1112. ",
    "https://pubmed.ncbi.nlm.nih.gov/36331670/ (oral tablet and liquid: ",
    "pivotal tablet Cmax, tmax and AUC; liquid absorption rate from the ",
    "pilot Cmax, same bioavailability)"
  )

  # Oral, immediate-release tablet (the default oral form) and liquid; see
  # the header.  Per labelled mg of morphine sulfate.
  ka_PO              <- 0.01300   # 1/min, half-time 53 min
  bioavailability_PO <- 0.290
  tlag_PO            <- 17.6      # min
  oralFormulations <- list(
    liquid = list(
      ka_PO              = 0.01887,  # 1/min, half-time 37 min
      bioavailability_PO = bioavailability_PO,  # as the tablet; see header
      tlag_PO            = tlag_PO
    )
  )
  
  # Size scaling (see docs/weight-adjustment.md): the published parameters
  # describe a 70 kg adult.  Volumes scale with fat-free mass relative to the
  # 70 kg, 170 cm reference male, clearances with that ratio ^ 0.75
  # (Al-Sallami 2015).  adjustToFFM = FALSE reproduces the former behaviour
  # exactly: V1 proportional to weight with fixed rate
  # constants, so volumes and clearances both scaled with weight/70.
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)
  v1 <- v1Ref * size$volume
  v2 <- v1Ref * k12 / k21 * size$volume
  v3 <- v1Ref * k13 / k31 * size$volume
  cl1 <- v1Ref * k10 * size$clearance
  cl2 <- v1Ref * k12 * size$clearance
  cl3 <- v1Ref * k13 * size$clearance
  
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
  
  return(
    list(
      PK = PK, 
      tPeak = tPeak, 
      MEAC = MEAC, 
      typical = typical, 
      upperTypical = upperTypical, 
      lowerTypical = lowerTypical, 
      reference = reference,
      oralFormulations = oralFormulations
    )
  )
}
