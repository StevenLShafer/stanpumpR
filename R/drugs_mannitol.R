# Mannitol: an osmotic agent.  What stanpumpR plots for it is not a drug
# concentration but the predicted serum osmolality,
#
#   osmolality(t) = baseline + MANNITOL_OSMOTIC_FRACTION * Cp(t)
#
# where Cp is the plasma mannitol concentration in mOsm/L and the baseline is
# the patient's own serum osmolality before mannitol (the `osmolality`
# covariate, mOsm/kg).  simCpCe() applies the transform; see `osmotic` below.
# The full write-up, including the check against independent data, is in
# docs/mannitol.md.

# 1 mmol of mannitol is 1 mOsm: it does not dissociate.
MANNITOL_MW <- 182.17   # g/mol

# Net rise in measured serum osmolality per mOsm/L of plasma mannitol.  An
# ideal solute would give 1, but mannitol is confined to the extracellular
# fluid and draws water out of cells, which dilutes sodium and the other
# endogenous solutes, so the measured osmolality rises by less than the
# mannitol concentration.  (The osmolal gap still equals the mannitol
# concentration; it is the calculated osmolality that falls.)  Taken from the
# only adult study found that reports peak plasma mannitol and peak serum
# osmolality in the same patients: 1 g/kg over 30 min gave a mean peak mannitol
# of 5.91 mg/mL and raised serum osmolality from 292 to 310 mOsm/kg
# (Rudehill A et al., J Neurosurg Anesthesiol 1993;5(1):4-12).
MANNITOL_OSMOTIC_FRACTION <- (310 - 292) / (5.91 * 1000 / MANNITOL_MW)  # 0.555

mannitol <- function(weight, height, age, sex, adjustToFFM = TRUE,
                     osmolality = OSMOLALITY_DEFAULT)
{
  # Units **************
  # Time: Minutes
  # Volume: Liters
  # Amount: mOsm (dose in g x 1000 / MANNITOL_MW), so Cp is in mOsm/L

  # Kaneda K et al., J Clin Pharmacol 2010;50(5):536-543: three-compartment
  # population means from 22 adults given 0.5 or 1.0 g/kg over 15 minutes for
  # elective craniotomy.  Clearance was dose dependent in that study, 0.04 L/min
  # in the 0.5 g/kg group and 0.07 L/min in the 1.0 g/kg group.  stanpumpR
  # simulates linear kinetics and cannot switch clearance on dose, so it uses
  # 0.07: that value reproduces the eight-hour concentration of an independent
  # 1 g/kg study (Rudehill 1993; 0.68 vs 0.58 mg/mL observed, against 1.23 with
  # 0.04), and lies closer to the clearances and half-lives reported elsewhere
  # (Cloyd 1986, Rudehill 1993) and to GFR, which is how mannitol is cleared.
  # With 0.04 the terminal half-life would be 7.3 hours.
  v1Ref  <- 2.80
  v2Ref  <- 8.86
  v3Ref  <- 12.0
  cl1Ref <- 0.07
  cl2Ref <- 2.07
  cl3Ref <- 0.16

  # Size scaling (see docs/weight-adjustment.md): Kaneda reports population
  # means without the reference weight, and only the abstract's covariates
  # (weight on V2, dose on CL1) are known, not their equations.  The means are
  # taken to describe the 70 kg reference adult and scaled to fat-free mass in
  # the usual way.  That is consistent with the study's own finding that
  # weight-based dosing gave higher than expected concentrations in obese
  # patients.  adjustToFFM = FALSE gives the published means unscaled.
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1)

  default <- list(
    v1  = v1Ref  * size$volume,
    v2  = v2Ref  * size$volume,
    v3  = v3Ref  * size$volume,
    cl1 = cl1Ref * size$clearance,
    cl2 = cl2Ref * size$clearance,
    cl3 = cl3Ref * size$clearance
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  # No effect site.  The reduction in brain water follows the plasma-to-brain
  # osmotic gradient, not an effect-site concentration, and no ke0 has been
  # published, so the plasma curve (the osmolality) is the whole display.
  tPeak <- 0
  MEAC  <- 0

  # Band: the usual target range of serum osmolality during osmotherapy, with
  # 320 mOsm/kg the conventional ceiling.
  typical      <- 310
  # Named for what they are.  Several older models in the library carry these
  # two the other way round; the plot reads the CSV's Lower and Upper instead.
  lowerTypical <- 300
  upperTypical <- 320
  reference <- paste(
    "Kaneda K et al., J Clin Pharmacol 2010;50(5):536-543. https://pubmed.ncbi.nlm.nih.gov/20051588/",
    "Osmolality: Rudehill A et al., J Neurosurg Anesthesiol 1993;5(1):4-12. https://pubmed.ncbi.nlm.nih.gov/8431668/"
  )

  return(
    list(
      PK = PK,
      tPeak = tPeak,
      MEAC = MEAC,
      typical = typical,
      upperTypical = upperTypical,
      lowerTypical = lowerTypical,
      reference = reference,
      osmotic = list(
        baseline        = osmolality,
        fraction        = MANNITOL_OSMOTIC_FRACTION,
        molecularWeight = MANNITOL_MW
      )
    )
  )
}
