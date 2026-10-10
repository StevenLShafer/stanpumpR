vecuronium <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  # Units **************
  # Time: Minutes
  # Volume: Liters
  # Concentration: ng/mL (doses in mg are simulated in mcg)
  #
  # DISPOSITION: Vega-Villa KR et al., Br J Anaesth 2014;112:304-310
  # (PMID 24067332), three-compartment population model, the NON-BURN
  # reference values from Table 3: V1 10.25, V2 9.7, V3 43.79 L; CL 0.51,
  # Q2 0.66, Q3 0.095 L/min.  Twenty adult surgical controls (age- and
  # sex-matched to the burn patients, 23-58 years) after 0.12 mg/kg, sampled
  # for 12 h.  The values were transcribed from the paper by a literature
  # assistant; the abstract confirms Q3 = 0.095 L/min and a 6.6 h terminal
  # half-life, and these parameters give 6.4 h.  The long terminal phase is
  # at concentrations far below any effect; the 12 h sampling found it where
  # older, shorter studies reported a 1-2 h half-life.
  #
  # The parameters are absolute (L, L/min), not per kilogram.  Burn status
  # was the only covariate retained, so there is no published size term.
  #
  # EFFECT SITE: Heier T et al., Anesth Analg 1994;78:973-977 (PMID 7909212),
  # normothermic adults under isoflurane-nitrous oxide-fentanyl: ke0 0.26/min,
  # Css50 79 ng/mL, gamma 4.4 (adductor pollicis twitch).  That ke0 belongs to
  # Heier's own disposition model, so it should be carried here as the time
  # to peak effect it gives WITH HEIER'S KINETICS (Minto CF et al.,
  # Anesthesiology 2003;99:324-333), and ke0 solved again against Vega-Villa.
  # PROVISIONAL: Heier's kinetics are not in hand, so tPeak = 5.9 min is the
  # peak 0.26/min gives against the Vega-Villa model above, which is the
  # "naive" pairing Minto warns against.  Replace it with the tPeak from
  # Heier's own PK when the paper is available.
  #
  # THRESHOLDS: the library plots concentration, not twitch height.  From
  # Heier's Css50 and gamma, T1 recovers to 25% of baseline at
  # 79 * 3^(1/4.4) = 101 ng/mL (the recovery threshold, endCe = 100 in the
  # CSV), 90% block is 130 ng/mL and 95% block 154 ng/mL.  Checked against
  # the clinical record: 0.1 mg/kg in a 70 kg adult reaches 99.9% block and
  # recovers to 25% at 36 min (label: 25-40 min after 0.08-0.1 mg/kg).
  #
  # Size scaling (see docs/weight-adjustment.md): the published parameters
  # are treated as describing a 70 kg adult.  Volumes scale with fat-free
  # mass relative to the 70 kg, 170 cm reference male, clearances with that
  # ratio ^ 0.75 (Al-Sallami 2015).  adjustToFFM = FALSE uses the parameters
  # as published, with no size scaling.
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1)

  default <- list(
    v1 = 10.25 * size$volume,
    v2 = 9.7 * size$volume,
    v3 = 43.79 * size$volume,
    cl1 = 0.51 * size$clearance,
    cl2 = 0.66 * size$clearance,
    cl3 = 0.095 * size$clearance
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  tPeak <- 5.9   # PROVISIONAL, naive pairing of Heier's ke0; see above
  typical <- 130        # 90% T1 block
  upperTypical <- 155   # 95% T1 block
  lowerTypical <- 100   # T1 recovered to 25%
  MEAC <- 0
  reference <- paste(
    "Vega-Villa KR et al., Br J Anaesth 2014;112(2):304-310 (non-burn",
    "reference values). https://pubmed.ncbi.nlm.nih.gov/24067332/ Effect",
    "site from Heier T et al., Anesth Analg 1994;78(5):973-977.",
    "https://pubmed.ncbi.nlm.nih.gov/7909212/"
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
