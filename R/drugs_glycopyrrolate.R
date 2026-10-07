# -----------------------------------------------------------------------------
# Glycopyrrolate (glycopyrronium): three-compartment intravenous model
# (Bartels 2013)
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in ng/mL of the active glycopyrronium cation.
#
# DISPOSITION
# ===========
# Bartels et al. identified systemic glycopyrronium disposition from an
# intravenous reference arm in healthy volunteers (120 mcg of active moiety
# over five minutes) alongside inhalation data; only the systemic
# compartments are used here:
#
#     CL 44.9 L/h   Vc 11.3 L   Q1 25.6 L/h   Vp1 19.0 L   Q2 8.23 L/h   Vp2 71.5 L
#
# Vss 101.8 L; eigenvalue half-times 0.093, 0.81 and 7.2 h.  No covariates
# were fitted.  Renal impairment markedly reduces clearance (Kirvela), but
# no continuous relationship exists to apply.
#
# DOSE BASIS: BROMIDE ON THE LABEL, CATION IN THE MODEL
# =====================================================
# The injection is labelled as glycopyrrolate BROMIDE (molecular weight
# 398.33), while the model's dose and concentration are the active cation
# (318.43).  A labelled milligram is 0.7994 mg of cation.  The engine takes
# the labelled dose, so the conversion is folded into the parameters: every
# volume and clearance below is the published value divided by 0.7994,
# which leaves the rate constants unchanged and makes the plotted
# concentration the active cation in ng/mL for a bromide-labelled dose.
#
# NO EFFECT SITE
# ==============
# The label's onset cannot identify an equilibration constant, and heart
# rate, secretions and the vagal responses to neostigmine each need their
# own calibrated relationship.  The row is plasma only.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# Fixed published parameters: volumes scale with fat-free mass relative to
# the reference male, clearances with that ratio ^ 0.75; the switch off uses
# them as published.
#
# References
# ----------
# Bartels C et al., Br J Clin Pharmacol 2013;76:868-879.
#   https://doi.org/10.1111/bcp.12118
# -----------------------------------------------------------------------------

GLYCOPYRROLATE_CATION_FRACTION <- 318.43 / 398.33   # cation / bromide salt mass

#' Glycopyrrolate pharmacokinetics
#'
#' @inheritParams cefazolin
#' @param adjustToFFM scale volumes to the patient's fat-free mass and
#'   clearances to that ratio to the 0.75 power; when \code{FALSE}, use the
#'   published fixed parameters unscaled.
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
glycopyrrolate <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  # Size scaling (see the header): fixed published parameters, unscaled
  # with the switch off.
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1)
  f <- GLYCOPYRROLATE_CATION_FRACTION   # labelled bromide mg -> cation mg

  v1  <- 11.3 / f * size$volume
  v2  <- 19.0 / f * size$volume
  v3  <- 71.5 / f * size$volume
  cl1 <- 44.9 / f / 60 * size$clearance    # L/min
  cl2 <- 25.6 / f / 60 * size$clearance
  cl3 <- 8.23 / f / 60 * size$clearance

  default <- list(
    v1 = v1,
    v2 = v2,
    v3 = v3,
    cl1 = cl1,
    cl2 = cl2,
    cl3 = cl3
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  tPeak <- 0     # no identified equilibration; plasma only
  MEAC  <- 0

  # Band, ng/mL cation: what 0.2-0.4 mg produce after distribution.
  typical      <- 3
  upperTypical <- 10
  lowerTypical <- 1

  reference <- paste0(
    "Bartels C et al., Br J Clin Pharmacol 2013;76:868-879. ",
    "Three-compartment intravenous model of the active cation; parameters ",
    "rescaled so a bromide-labelled dose plots as cation. ",
    "https://doi.org/10.1111/bcp.12118"
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
