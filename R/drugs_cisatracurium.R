cisatracurium <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  # Units **************
  # Time: Minutes
  # Volume: Liters
  # Concentration: ng/mL (doses in mg of cisatracurium, the labelled
  # strength, not the besylate salt, are simulated in mcg)
  #
  # SOURCE: Sorooshian SS, Stafford MA, Eastwood NB, Boyd AH, Hull CJ,
  # Wright PMC.  Pharmacokinetics and pharmacodynamics of cisatracurium in
  # young and elderly adult patients.  Anesthesiology 1996;84:1083-1091
  # (PMID 8624002).  Population (NONMEM) PK/PD model in 31 young (18-50 yr)
  # and 33 elderly (>65 yr) adults under nitrous oxide-fentanyl-isoflurane,
  # 0.1 mg/kg then supplementary boluses or an infusion; venous samples to
  # 2 h, adductor pollicis T1 by mechanomyography.  Read from the paper.
  #
  # Two compartments, elimination from the central one, no weight term
  # ("neither the three-compartment model nor weight normalization could be
  # justified").  Table 2: CL 319 mL/min, V1 4.76 L, both ages; Vss 9.7 L in
  # the young, so V2 = 4.94 L; t1/2 alpha 4.20 / 4.50 min and t1/2 beta
  # 28.4 / 36.3 min (young / elderly).  The only age effect on the kinetics
  # was a 37% larger V2 in the elderly (text, pp 1086 and 1089).
  #
  # Two things had to be worked out:
  #
  #  - The paper does not print the distribution clearance.  Q = 0.297 L/min
  #    is the value that, with V1, V2 and CL above, gives both of the young
  #    half-lives (4.19 and 28.4 min).  It reproduces the paper's own Table 3
  #    predictions for a young adult to within 1 min at every dose (6 mg:
  #    99% block at 5.9 min, 25% and 75% recovery at 56 and 77 min; Table 3
  #    has 6, 56 and 77).
  #
  #  - Table 2 lists the elderly Vss as 13.3 L, which is NOT 37% more V2
  #    (that would be 4.76 + 6.77 = 11.5 L; 13.3 is 37% more Vss).  The text
  #    is followed: V2 = 4.94 x 1.37 = 6.77 L with the same Q gives both
  #    elderly half-lives exactly (4.48 and 36.3 min), and V2 = 8.54 L gives
  #    neither with any single Q.  Ages 50-65 were not studied; V2 is
  #    interpolated linearly between the two groups across that gap.
  #
  # Supporting non-population studies, consistent with these kinetics:
  # Ornstein E et al., Anesthesiology 1996;84:520-525 (PMID 8659778), CL 4.6
  # and 5.0 mL/kg/min, t1/2 beta 21.5 and 25.5 min, young and elderly; Lien
  # CA et al., Anesthesiology 1996;84:300-308 (PMID 8602660), CL 5.28 and
  # 4.66 mL/kg/min, t1/2 beta 22.4 and 25.5 min after 0.1 and 0.2 mg/kg.
  # Cisatracurium is degraded by Hofmann elimination in tissue as well as
  # plasma, so a central-elimination model underestimates the true Vss
  # (by 1.55 in Sorooshian's estimate) but not the clearance or the
  # concentrations it predicts.
  #
  # EFFECT SITE: Sorooshian's ke0 is 0.071/min young and 0.060/min elderly,
  # fitted with this disposition.  Carried as the bolus time to peak effect
  # it gives with these kinetics: 12.29 min young, 12.91 min elderly,
  # interpolated across 50-65 like V2.  getDrugPK solves ke0 back from it, so
  # the reference patients recover the published ke0.
  #
  # THRESHOLDS: Sorooshian's C50 is 98 ng/mL (young, 70 kg; 90 elderly),
  # gamma 4.1 (3.7 elderly), C50 rising 10% per 10 kg of weight.  In the
  # young 70 kg adult, T1 recovers to 25% at 98 * 3^(1/4.1) = 128 ng/mL (the
  # recovery threshold, endCe = 130 in the CSV), 90% block is 166 and 95%
  # block 200 ng/mL.  The plot shows concentration, not twitch, so the
  # weight and age effects on C50 are not applied.
  #
  # Size scaling (see docs/weight-adjustment.md): Sorooshian's parameters
  # are absolute, for adults averaging 70-73 kg, and are treated as the 70 kg
  # reference.  Volumes scale with fat-free mass relative to the 70 kg,
  # 170 cm reference male, clearances with that ratio ^ 0.75 (Al-Sallami
  # 2015).  adjustToFFM = FALSE uses them as published, with no size scaling,
  # which is how the source found them best.
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1)

  # 0 at 50 years and younger, 1 at 65 and older
  elderly <- min(max((age - 50) / 15, 0), 1)

  v2Young <- 9.7 - 4.76
  default <- list(
    v1 = 4.76 * size$volume,
    v2 = v2Young * (1 + 0.37 * elderly) * size$volume,
    v3 = 1,
    cl1 = 0.319 * size$clearance,
    cl2 = 0.297 * size$clearance,
    cl3 = 0
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  tPeak <- 12.29 + (12.91 - 12.29) * elderly   # from ke0 0.071 / 0.060; see above
  typical <- 165        # 90% T1 block
  upperTypical <- 200   # 95% T1 block
  lowerTypical <- 130   # T1 recovered to 25%
  MEAC <- 0
  reference <- paste(
    "Sorooshian SS et al., Anesthesiology 1996;84(5):1083-1091; distribution",
    "clearance derived from its half-lives.",
    "https://pubmed.ncbi.nlm.nih.gov/8624002/"
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
