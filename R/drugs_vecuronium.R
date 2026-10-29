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
  # EFFECT SITE AND POTENCY: Rupp SM, Castagnoli KP, Fisher DM, Miller RD.
  # Anesthesiology 1987;67:45-49 (PMID 2886080), Table 2, younger adults
  # (n = 5, ages 30-40, halothane-nitrous oxide).  That study fitted the
  # disposition and the effect together, so its ke0, Cpss50 and gamma are one
  # vector: ke0 0.169/min, Cpss50 92 ng/mL, gamma 5.80.
  #
  # ke0 is model-dependent, so it is carried here as the time to peak effect
  # it gives WITH RUPP'S OWN KINETICS (Minto CF et al., Anesthesiology
  # 2003;99:324-333), and getDrugPK solves ke0 again against the disposition
  # above.  Rupp's kinetics rebuilt from Table 2 (V1 52 mL/kg, Vss 244 mL/kg,
  # CL 5.2 mL/kg/min, half-lives 2 / 15 / 70 min) are, per 70 kg,
  # V1 3.64, V2 4.74, V3 8.70 L, CL 0.364, Q2 0.523, Q3 0.125 L/min, which
  # reproduce all three half-lives and Vss exactly.  With ke0 0.169/min they
  # put the bolus effect-site peak at 5.37 min, which is the tPeak below; it
  # becomes ke0 0.300/min against the Vega-Villa disposition.  Rupp's
  # parameters are per kilogram, so that 5.37 min does not depend on the
  # 70 kg the rebuild was done at.
  #
  # AGE.  Rupp is also the source of the age effect, because Vega-Villa
  # carries no age term.  Only two of his comparisons reached significance
  # (Table 2, P < 0.05): in the elderly, clearance was 3.7 against
  # 5.2 mL/kg/min and steady-state volume 179 against 244 mL/kg.  Those two
  # ratios are applied to every clearance and every volume respectively, so
  # Vss and CL fall by the published amounts; the rate constants then change
  # by only 3%, which preserves his third, null finding that the half-lives
  # were similar.  V1 alone (29 against 52 mL/kg) is NOT given its own
  # factor: that difference was not significant and its standard deviations
  # (22 and 26) nearly span it.
  #
  # The elderly kinetics rebuilt the same way (V1 29, Vss 179 mL/kg, CL 3.7
  # mL/kg/min, half-lives 1 / 9 / 58 min) with his elderly ke0 0.165/min put
  # the peak at 4.80 min, which is the elderly tPeak.  The vecuronium
  # subjects were 30-40 and 70-84 years old, so both the ratios and the
  # tPeak are interpolated linearly between 40 and 70 and held flat outside.
  #
  # A judgement call sits in that 4.80.  It comes from Rupp's COMPLETE
  # elderly fit, whose distribution half-lives are shorter than the young
  # ones - a difference that did not reach significance and that is
  # therefore NOT grafted onto the disposition above.  Deriving the elderly
  # tPeak the other way, from the young structure carrying only the two
  # significant ratios, gives 5.51 min instead, i.e. slightly slower rather
  # than faster.  Rupp's own fit is used because tPeak is the observable
  # Minto's method says to carry, and his elderly fit is a real integrated
  # PK/PD description of elderly patients, whereas the alternative is a
  # construction nobody fitted.  The significance filter above governs which
  # kinetic changes reach Vega-Villa's model, which is a separate question.
  # Either way the effect is small beside the volume and clearance change.
  # Shafer reviewed both derivations on 2026-10-12 and chose the 4.80.
  #
  # Rupp found NO significant age difference in ke0 (0.165 against 0.169),
  # Cpss50 (106 against 92) or gamma (4.84 against 5.80), so the potency and
  # the band below do not move with age.  Duration does: the lower volumes
  # and clearance give a 70-year-old about 45 min to 25% recovery after
  # 0.1 mg/kg against 32 min at 40, the direction d'Hollander and colleagues
  # reported from steady-state infusions.
  #
  # Heier T et al., Anesth Analg 1994;78:973-977 (PMID 7909212) measured the
  # same relationship in normothermic adults under isoflurane and agrees
  # closely (Css50 79 ng/mL, gamma 4.4), but fitted no kinetics of its own
  # (60 min of sampling after a small infusion was too short), so its ke0
  # cannot be converted to a tPeak and its vector is not the one used.
  #
  # THRESHOLDS: the library plots concentration, not twitch height.  From
  # Rupp's Cpss50 and gamma, T1 recovers to 25% of baseline at
  # 92 * 3^(1/5.8) = 111 ng/mL (the recovery threshold, endCe = 110 in the
  # CSV), 90% block is 134 ng/mL and 95% block 153 ng/mL.  Heier's values
  # give 101, 130 and 154, so the band hardly depends on the choice.
  #
  # Checked against the clinical record: 0.1 mg/kg in a 70 kg adult reaches
  # complete block and recovers to 25% at 32 min (label: 25-40 min after
  # 0.08-0.1 mg/kg).  Wright PM et al., Anesthesiology 1994;81:59-68
  # (PMID 7913801), another Fisher study, measured 15.5 and 34.3 min to 25%
  # recovery after 26 and 66 mcg/kg; this model gives 6 and 24 min for those
  # doses, because Wright's patients were under nitrous oxide-isoflurane and
  # far more sensitive (their ED95 was 21.8 mcg/kg, about half the
  # unpotentiated value).  Potentiation by the inhaled agents is not modelled.
  #
  # Size scaling (see docs/weight-adjustment.md): the published parameters
  # are treated as describing a 70 kg adult.  Volumes scale with fat-free
  # mass relative to the 70 kg, 170 cm reference male, clearances with that
  # ratio ^ 0.75 (Al-Sallami 2015).  adjustToFFM = FALSE uses the parameters
  # as published, with no size scaling.
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1)

  # 0 at 40 years and younger, 1 at 70 and older: Rupp's two groups
  elderly <- min(max((age - 40) / 30, 0), 1)
  ageVolume <- 1 + (179 / 244 - 1) * elderly      # Rupp's Vss ratio
  ageClearance <- 1 + (3.7 / 5.2 - 1) * elderly   # Rupp's CL ratio

  default <- list(
    v1 = 10.25 * ageVolume * size$volume,
    v2 = 9.7 * ageVolume * size$volume,
    v3 = 43.79 * ageVolume * size$volume,
    cl1 = 0.51 * ageClearance * size$clearance,
    cl2 = 0.66 * ageClearance * size$clearance,
    cl3 = 0.095 * ageClearance * size$clearance
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  # From Rupp's ke0 with Rupp's own kinetics, young and elderly; see above
  tPeak <- 5.37 + (4.80 - 5.37) * elderly
  typical <- 135        # 90% T1 block
  upperTypical <- 155   # 95% T1 block
  lowerTypical <- 110   # T1 recovered to 25%
  MEAC <- 0
  reference <- paste(
    "Vega-Villa KR et al., Br J Anaesth 2014;112(2):304-310 (non-burn",
    "reference values). https://pubmed.ncbi.nlm.nih.gov/24067332/ Effect",
    "site and potency from Rupp SM, Fisher DM et al., Anesthesiology",
    "1987;67(1):45-49. https://pubmed.ncbi.nlm.nih.gov/2886080/"
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
