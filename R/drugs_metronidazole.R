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
# NOT MODELLED
# ============
# Hydroxymetronidazole, an active metabolite about 65% as potent against the
# tested anaerobes, whose population disposition was not recovered.  Hepatic
# impairment.  Binding is low (free fraction about 0.85), so total is close
# to free.
#
# References
# ----------
# da Silva Neto MJJ et al., J Antimicrob Chemother 2021;76:3212-3219.
#   https://doi.org/10.1093/jac/dkab337
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

#' Metronidazole pharmacokinetics
#'
#' @inheritParams cefazolin
#' @param adjustToFFM scale volumes to the patient's fat-free mass and
#'   clearances to that ratio to the 0.75 power; when \code{FALSE}, scale
#'   clearance to total body weight to the 0.75 power and volume to the patient's
#'   adjusted body weight, as published. No renal-function estimate.
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
metronidazole <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  # The reference man's adjusted body weight, which anchors the volume
  ajbwRef <- adjustedBodyWeight(FFM_REFERENCE_WEIGHT, FFM_REFERENCE_HEIGHT,
                                FFM_REFERENCE_SEX)

  # Size scaling (see the header).  With the switch off: CL x (TBW/70)^0.75
  # and V on the patient's own adjusted body weight, exactly as published.
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume = adjustedBodyWeight(weight, height, sex) / ajbwRef,
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
