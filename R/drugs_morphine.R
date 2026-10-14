# -----------------------------------------------------------------------------
# Morphine
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min.
#
# DISPOSITION
# ===========
# Lotsch 2002 (Model A), three compartments from intravenous morphine in eight
# young volunteers (four men, four women, 23-30 years).  The model's
# parameters are ABSOLUTE, not per kilogram: V1 17.5 L, V2 85.8 L, V3 195.6
# L, CL 74.0 L/h, Q2 133.7 L/h, and Q3 19.5 L/h at a body surface area of
# 1.83 m2 with the paper's one covariate, Q3 = 19.5 x (BSA / 1.83)^4 L/h.
# Until 2026-10-14 the file held the median rate constants, so the slow
# intercompartmental clearance could not follow the covariate (at 2.2 m2 the
# paper's Q3 is 2.1 times its 1.83 m2 value), and the help called the model
# per-kilogram.  The covariate is now applied, as the library does for every
# model with a size covariate of its own (naloxone, acetaminophen): Q3 takes
# it in both switch positions, and the other five parameters are the
# published values with the switch off and take the fat-free-mass scaling
# with it on.  Body surface area is Du Bois (bsaDuBois(), R/renalFunction.R).
# CAVEAT: a fourth power of body surface area from eight young adults is an
# extrapolation outside roughly 1.5 to 2.2 m2; in a small child Q3 is tiny
# and the deep compartment hardly fills.  The help says so.
#
# EFFECT SITE AND MEAC are not from Lotsch 2002, which measured no effect.
# tPeak 93.8 min is the library's own value, carried from the program's
# opioid tables.  For context, Lotsch 2001 (pupil, Anesthesiology
# 95:1329-1338) found a morphine transfer half-life of 2.8 h (range 1.8-4.4),
# and Skarke 2003 (Clin Pharmacol Ther 73:107-121) 2.6 h for pain tolerance
# and 2.8 h for the pupil, which with these kinetics would put the peak later
# still.  MEAC 8 ng/mL is likewise the library's value; Dahlstrom 1982 (Clin
# Pharmacokinet 7:266-279) measured a postoperative minimum effective
# concentration of 16 +/- 9 ng/mL.
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
# PREGNANCY
# =========
# Ansari, Carvalho, Shafer and Flood (Anesth Analg 2016;122:786-804, Table 3,
# from Gerdin 1990): "Volume of distribution unchanged, clearance >70%
# increased, decreased half-life, glucuronide conjugation by UGT".  With the
# Patient Profile's Pregnant field on, the elimination clearance CL1 is
# multiplied by MORPHINE_PREGNANCY_CL_FACTOR = 1.7 and nothing else moves:
# the volumes and the distribution clearances stay as published, so the
# terminal half-life falls as the review says.  The data are from late
# pregnancy and labour; morphine-6-glucuronide, whose formation rises with
# the clearance, is not modelled (see above).  See R/pregnancy.R.
#
# References
# ----------
# Lotsch J, Skarke C, Schmidt H, Liefhold J, Geisslinger G.  Clin Pharmacol
#   Ther 2002;72:151-162.  https://doi.org/10.1067/mcp.2002.126172
# Lotsch J, Skarke C, Schmidt H, Grosch S, Geisslinger G.  Anesthesiology
#   2001;95:1329-1338.  https://doi.org/10.1097/00000542-200112000-00009
#   (pupil: morphine transfer half-life 2.8 h; context for tPeak)
# Skarke C, Darimont J, Schmidt H, Geisslinger G, Lotsch J.  Clin Pharmacol
#   Ther 2003;73:107-121.  https://doi.org/10.1067/mcp.2003.5 (context)
# Dahlstrom B, Tamsen A, Paalzow L, Hartvig P.  Clin Pharmacokinet
#   1982;7:266-279.  https://doi.org/10.2165/00003088-198207030-00006
#   (postoperative MEC 16 +/- 9 ng/mL; context for MEAC)
# Atrux-Tallau N, Naimi Z, Jaudinot EO.  Clin Drug Investig 2022;42:1101-1112.
#   https://doi.org/10.1007/s40261-022-01214-x
# Ansari J, Carvalho B, Shafer SL, Flood P.  Anesth Analg 2016;122:786-804.
#   https://doi.org/10.1213/ANE.0000000000001143 (pregnancy)
# (Oral routes added by Claude Code, 2026-10-10, at the request of Steven L.
# Shafer; pregnancy 2026-10-13.)
# -----------------------------------------------------------------------------

# Elimination clearance in pregnancy relative to the published value: Ansari
# 2016, "clearance >70% increased" (see the header).
MORPHINE_PREGNANCY_CL_FACTOR <- 1.7

morphine <- function(weight, height, age, sex, adjustToFFM = TRUE,
                     pregnant = FALSE)
{
  # Units **************
  # Time: Minutes
  # Volume: Liters
  
  # Lotsch 2002 Model A, absolute values (see the header).  Litres and L/min.
  v1Ref  <- 17.5
  v2Ref  <- 85.82865
  v3Ref  <- 195.646
  cl1Ref <- 74.0 / 60          # 1.2333 L/min
  cl2Ref <- 133.7 / 60         # 2.2283 L/min
  cl3At183 <- 19.5 / 60        # L/min at BSA 1.83 m2; x (BSA / 1.83)^4
  
  tPeak <- 93.8                # library value, not Lotsch 2002; see the header
  MEAC <- 8/1000               # library value, not Lotsch 2002; see the header
  typical <- MEAC * 1.2
  upperTypical <- MEAC * 0.8
  lowerTypical <- MEAC * 2.0
  reference <- paste0(
    "Lotsch J et al., Clin Pharmacol Ther 2002;72(2):151-162. ",
    "https://pubmed.ncbi.nlm.nih.gov/12189362/ (intravenous, Model A, ",
    "Q3 x (BSA/1.83)^4; tPeak and MEAC are library values, not the paper's); ",
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
  
  # Size (see docs/weight-adjustment.md and the header).  The published
  # values are absolute, for young adults.  With the switch on, volumes scale
  # with fat-free mass relative to the 70 kg, 170 cm reference male and the
  # clearances with that ratio ^ 0.75 (Al-Sallami 2015); with it off they are
  # used as published (legacyVolume = 1, legacyClearance = 1).  Q3 carries the
  # model's own covariate in both positions: 19.5 L/h x (BSA / 1.83)^4.
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume = 1, legacyClearance = 1)
  v1 <- v1Ref * size$volume
  v2 <- v2Ref * size$volume
  v3 <- v3Ref * size$volume
  # Pregnancy (see the header): elimination clearance only.
  clPregnancy <- if (isTRUE(pregnant)) MORPHINE_PREGNANCY_CL_FACTOR else 1
  cl1 <- cl1Ref * size$clearance * clPregnancy
  cl2 <- cl2Ref * size$clearance
  cl3 <- cl3At183 * (bsaDuBois(weight, height) / 1.83)^4
  if (isTRUE(pregnant)) {
    reference <- paste0(reference, "; ", pregnancyReference(
      paste0("clearance x ", MORPHINE_PREGNANCY_CL_FACTOR)))
  }
  
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
