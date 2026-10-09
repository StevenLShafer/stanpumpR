# -----------------------------------------------------------------------------
# Metronidazole: one-compartment intravenous model (da Silva Neto 2021) with
# an oral route from paired-route studies
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in mcg/mL (= mg/L), total plasma.
#
# DISPOSITION
# ===========
# da Silva Neto et al. fitted total plasma metronidazole from 20 adults
# receiving prophylaxis for colorectal surgery, sampling from one hour after
# the dose:
#
#     CL = 3.22 x (TBW/70)^0.75    L/h
#     V  = 0.556 x AJBW            L
#     AJBW = IBW + 0.4 x (TBW - IBW),  IBW by Devine
#
# One compartment is what the late samples supported; an early distribution
# phase cannot be excluded and the first hour after a fast infusion is the
# least certain part of the curve.  Half-time at 70 kg is 8.4 h.
#
# ORAL ROUTE: CROSS-STUDY, AND SAID SO
# ====================================
# The source is intravenous only.  Oral bioavailability comes from paired
# route studies of conventional tablets: 0.804 and 0.841 in Bergan 1984,
# essentially complete in Loft 1986.  This model uses 0.841.  The absorption
# constant, 1.336 /h, is the only published first-order estimate found, from
# six volunteers given a LABORATORY-MADE 150 mg conventional tablet; it is an
# initialiser for a commercial tablet rather than a fit to one, and
# extended-release metronidazole is a different product that it does not
# describe.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# The source scaled clearance allometrically on total weight and volume on
# adjusted body weight, which is itself a lean-weight construct.  With the
# switch on, both are expressed at the reference man (Devine adjusted weight
# 67.56 kg, V 37.56 L) and scaled by the fat-free-mass factors.  With it off,
# the published equations are evaluated on the patient's own total and
# adjusted weights.
#
# CHILDREN.  The source enrolled 20 adults: 18-81 years, 47.6-101.9 kg,
# 144-179 cm.  Devine's ideal weight is an adult formula, linear in height,
# and goes negative below about 97 cm (men) or 102 cm (women); evaluated on a
# 7 kg, 65 cm infant the published equation gave V = -8.2 L and a nonfinite
# curve.  With the switch off, a patient under 18 (or of any age at a height
# where Devine's ideal weight is not positive) therefore has V = 0.556 x TBW:
# total body weight takes the place of the adjusted weight, the usual linear
# volume scaling of the library's other total-weight models.  The published
# equation is unchanged for every adult in the source's range.  Either way the
# model is extrapolated in children; the default fat-free-mass path does not
# use Devine and is unaffected.
#
# NOT MODELLED
# ============
# Hydroxymetronidazole, an active metabolite about 65% as potent against the
# tested anaerobes, whose population disposition was not recovered.  Hepatic
# impairment.  Binding is low (free fraction 0.96; see below), so total is
# close to free.
#
# TIME UNTIL THRESHOLD: FREE DRUG AT THE MIC
# ==========================================
# The default threshold (endCe in drugDefaults_global.csv) is the TOTAL
# concentration at which FREE metronidazole equals the MIC of 4 mg/L for the
# Bacteroides fragilis group: the EUCAST susceptible breakpoint for
# Bacteroides spp. (S <= 4, R > 4 mg/L; the CLSI anaerobe breakpoint is 8),
# and the B. fragilis-group MIC da Silva Neto used for target attainment.
# Wild-type B. fragilis sits well below it (MIC50/MIC90 about 0.5/1 mg/L,
# Boiten 2024).  Binding is linear, so the threshold is MIC / fu =
# 4 / 0.96 = 4.2 mg/L.
#
# fu = 0.96 is from Dorn 2021: 0.964 +/- 0.044 by ultrafiltration in plasma
# from adults given 0.5 g IV for abdominal or bariatric surgical prophylaxis,
# independent of concentration (0.981 at 2 mg/L and 0.966 at 10 mg/L in
# spiked plasma).  It replaces the uncited "about 0.85" this header used to
# give; the labels' "less than 20% bound" is a bound, not a measurement.
# Dorn's free fraction is quoted from the article; its abstract gives only
# total concentrations, and the full text could not be read when this was
# written, so it is worth checking against the paper.  Any credible value
# (0.80-1.0) moves the threshold by less than a fifth.  See
# R/antibioticThresholds.R.  (Free fraction 0.96 confirmed by Steven L.
# Shafer, 2026-10-07.)
#
# References
# ----------
# da Silva Neto MJJ et al., J Antimicrob Chemother 2021;76:3212-3219.
#   https://doi.org/10.1093/jac/dkab337
# Dorn C et al., J Antimicrob Chemother 2021;76:2114-2120.
#   https://doi.org/10.1093/jac/dkab143
# Boiten KE et al., J Antimicrob Chemother 2024;79:868-874.
#   https://doi.org/10.1093/jac/dkae043
# Bergan T et al., 1984. https://pubmed.ncbi.nlm.nih.gov/6588489/
# Loft S et al., 1986. https://pubmed.ncbi.nlm.nih.gov/3743624/
# Experimental-tablet study, 2022. https://pmc.ncbi.nlm.nih.gov/articles/PMC9024553/
# -----------------------------------------------------------------------------

# Devine ideal body weight, kg.  50 (men) or 45.5 (women) plus 2.3 kg per inch
# of height over 60 inches.
idealBodyWeightDevine <- function(height, sex)
{
  base <- if (sex == SEX_FEMALE) 45.5 else 50
  base + 2.3 * (height / 2.54 - 60)
}

# Adjusted body weight as the metronidazole source defined it
adjustedBodyWeight <- function(weight, height, sex)
{
  ibw <- idealBodyWeightDevine(height, sex)
  ibw + 0.4 * (weight - ibw)
}

# The weight the published volume equation is evaluated at with the switch
# off: adjusted body weight, except where it has no meaning.  Devine ideal
# weight and the adjusted weight built on it are adult constructs, and the
# source enrolled adults only (18 to 81 years), so under 18 total body weight
# takes its place.  At any age, a height at which Devine gives no positive
# ideal weight (below about 97 cm in a man, 102 cm in a woman) also falls back
# to total body weight; the adjusted weight there can be zero or negative.
metronidazoleVolumeWeight <- function(weight, height, age, sex)
{
  if (age < 18 || idealBodyWeightDevine(height, sex) <= 0) return(weight)
  adjustedBodyWeight(weight, height, sex)
}

#' Metronidazole pharmacokinetics
#'
#' @inheritParams cefazolin
#' @param adjustToFFM scale volumes to the patient's fat-free mass and
#'   clearances to that ratio to the 0.75 power; when \code{FALSE}, scale
#'   clearance to total body weight to the 0.75 power and volume to the patient's
#'   adjusted body weight, as published (total body weight under 18 years,
#'   where the adult adjusted weight does not apply). No renal-function
#'   estimate.
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
metronidazole <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  # The reference man's adjusted body weight, which anchors the volume
  ajbwRef <- adjustedBodyWeight(FFM_REFERENCE_WEIGHT, FFM_REFERENCE_HEIGHT,
                                FFM_REFERENCE_SEX)

  # Size scaling (see the header).  With the switch off: CL x (TBW/70)^0.75
  # and V on the patient's own adjusted body weight, exactly as published,
  # except that a child is scaled on total body weight (see
  # metronidazoleVolumeWeight()).
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume = metronidazoleVolumeWeight(weight, height, age, sex) / ajbwRef,
                        legacyClearance = (weight / 70)^0.75)

  v1  <- 0.556 * ajbwRef * size$volume
  cl1 <- 3.22 * size$clearance / 60      # L/min
  v2  <- 1                               # one compartment
  v3  <- 1
  cl2 <- 0
  cl3 <- 0

  # Oral: cross-study, see the header
  ka_PO              <- 1.336 / 60       # 1/min, experimental conventional tablet
  bioavailability_PO <- 0.841            # Bergan 1984, conventional tablet
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

  # Band, total mg/L: the surgical model's MIC scenario of 4 mg/L up to the
  # peak a 500 mg dose produces.  Orientation only.
  typical      <- 8
  upperTypical <- 25
  lowerTypical <- 4

  reference <- paste0(
    "da Silva Neto MJJ et al., J Antimicrob Chemother 2021;76:3212-3219. ",
    "Intravenous one-compartment model; oral bioavailability 0.841 (Bergan ",
    "1984) and absorption from an experimental tablet are cross-study additions. ",
    "https://doi.org/10.1093/jac/dkab337"
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
