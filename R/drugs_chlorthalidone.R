# -----------------------------------------------------------------------------
# Chlorthalidone, the thiazide-like diuretic
# -----------------------------------------------------------------------------
# Drafted by Claude Code, 2026-10-17, at the request of Steven L. Shafer, from
# the full text of Tsai MC, Wu J, Kupfer S, Vakilynejad M, J Clin Pharmacol
# 2016;56:988-998.  Tests: tests/testthat/test-drugs-chlorthalidone.R.
#
# WHY THIS ONE MATTERS
# ====================
# Chlorthalidone is a first-line thiazide-type diuretic with better outcome
# evidence than hydrochlorothiazide, and the paper's own introduction notes
# that although it has been available for over fifty years the literature
# describing its pharmacokinetics in patients was thin, resting on studies of
# seven subjects or fewer.  This is a population fit in 1059 hypertensive
# subjects contributing 4807 concentrations, and every disposition parameter
# in it is estimated rather than fixed -- unlike azilsartan's in the same
# paper, whose Q and V2 are fixed.  Table 2.
#
# THE MATRIX IS PLASMA, AND THAT IS WHY THE VOLUME LOOKS LARGE
# ============================================================
# An apparent central volume of 62 L and a peripheral of 120 L are large for
# a small, highly bound molecule.  The reason is in the paper's discussion:
# chlorthalidone partitions markedly into red blood cells, which hold about
# 95% of the drug in whole blood and 30% of the drug in the body at steady
# state.  The samples assayed were PLASMA, so the volume is apparent in that
# partitioning as well as in F, and the authors attribute the sex effect on
# volume to men's higher haematocrit.  A concentration from this model is a
# plasma concentration and must not be read against a whole-blood figure.
#
# THE COVARIATE THAT IS APPLIED, AND THE TWO THAT ARE NOT
# =======================================================
# The source fits four covariates.  Two are applied here and two are not, for
# different reasons, and neither reason is doubt about the number.
#
#   APPLIED    age on CL/F, -0.0451 L/h per year about 58
#   APPLIED    sex on CL/F, +25.3% in men; and sex on Vd/F, +119% in men
#
#   NOT APPLIED   cystatin C on CL/F, -2.61 L/h per mg/L about 0.73 mg/L.
#                 The Patient Profile has no cystatin C field.  Serum
#                 creatinine is NOT a substitute: cystatin C is cleared only
#                 by glomerular filtration and the authors chose it over
#                 creatinine for exactly that reason, so putting creatinine
#                 into a term fitted to cystatin C would be inventing a
#                 covariate the source never estimated.  The term is dropped
#                 at its reference value, where it vanishes.
#
#   NOT APPLIED   BMI on Vd/F, 2.41 L per kg/m2 about 30.9.  Here the
#                 obstacle is the equation, not the number.  The paper prints
#
#                     Vd/F = 62.1 L x (1 + Sex x 1.19) x 2.41 x (BMI - 30.9)
#
#                 which cannot be read as written: as a product it gives a
#                 volume of zero at BMI 30.9 and a negative one below it.
#                 The text says the volume increased LINEARLY with BMI, so an
#                 additive term is the likely intent -- but "likely" is not a
#                 published model, and the supplementary material holds only
#                 a demographics table and a diagnostic figure, with no
#                 control stream to settle it.  At the reference BMI of 30.9
#                 every candidate reading gives 62.1 L, so the typical
#                 patient is unaffected and the term is left out rather than
#                 guessed.  docs/antihypertensives.md section 10 records it
#                 as the one number in this source that a reader of the
#                 printed paper cannot use.
#
# THE DOSE BASIS NEEDS NO DERIVATION
# ==================================
# Unlike azilsartan in the same paper, chlorthalidone is not a prodrug and
# its doses enter as dosed.  The paper's own simulated exposures confirm it:
# 25 mg over CL/F of 4.13 L/h is 6053 ng*h/mL against a simulated 6035, and
# 12.5 mg gives 3027 against 2956.
# -----------------------------------------------------------------------------

# Tsai 2016, Table 2, as published
CHLORTHALIDONE_CL_F    <- 4.13     # L/h, apparent oral clearance
CHLORTHALIDONE_V1_F    <- 62.1     # L, apparent central volume
CHLORTHALIDONE_V2_F    <- 120      # L, apparent peripheral volume
CHLORTHALIDONE_Q_F     <- 36.9     # L/h, apparent intercompartmental
CHLORTHALIDONE_KA      <- 1.13     # 1/h, first-order absorption
CHLORTHALIDONE_AGE_CL  <- -0.0451  # L/h per year, about age 58
CHLORTHALIDONE_SEX_CL  <- 0.253    # proportional increase in men
CHLORTHALIDONE_SEX_V   <- 1.19     # proportional increase in men
CHLORTHALIDONE_AGE_REF <- 58       # years, population median

#' Chlorthalidone pharmacokinetics (oral, apparent, plasma)
#'
#' Tsai et al. (2016), two compartments with first-order absorption in 1059
#' hypertensive subjects; see the header for the covariates that are and are
#' not applied.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
chlorthalidone <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  # The source carries no weight or height term at all: its only size
  # covariate is the BMI term on volume that cannot be read (see the header).
  # So the published parameters are reproduced unscaled with the switch off.
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume = 1, legacyClearance = 1)

  male <- identical(tolower(as.character(sex)), "male")

  # CL/F = (4.13 - 0.0451 x [Age - 58]) x (1 + Sex x 0.253), Tsai 2016 p992,
  # with the cystatin C term at its reference value (see the header).
  clF <- (CHLORTHALIDONE_CL_F +
            CHLORTHALIDONE_AGE_CL * (age - CHLORTHALIDONE_AGE_REF)) *
    (1 + if (male) CHLORTHALIDONE_SEX_CL else 0)

  # Vd/F = 62.1 x (1 + Sex x 1.19), with the BMI term left out.
  v1F <- CHLORTHALIDONE_V1_F * (1 + if (male) CHLORTHALIDONE_SEX_V else 0)

  default <- list(
    v1  = v1F * size$volume,
    v2  = CHLORTHALIDONE_V2_F * size$volume,
    v3  = 1,                            # two compartments
    cl1 = clF / 60 * size$clearance,    # L/min
    cl2 = CHLORTHALIDONE_Q_F / 60 * size$clearance,
    cl3 = 0,
    ka_PO = CHLORTHALIDONE_KA / 60,     # 1/min
    # Apparent parameters already carry F.
    bioavailability_PO = 1,
    tlag_PO = 0                         # no lag was fitted
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  tPeak <- 0    # no effect-site model published; plasma only
  MEAC  <- 0

  # No therapeutic plasma range: the source's exposure-response is driven by
  # steady-state AUC rather than by a concentration.
  typical      <- 0
  upperTypical <- 0
  lowerTypical <- 0

  reference <- paste0(
    "Tsai MC et al., J Clin Pharmacol 2016;56:988-998. ",
    "Two-compartment apparent oral model in 1059 subjects with stage 2 ",
    "hypertension, every parameter estimated: CL/F 4.13 L/h, V1/F 62.1 L, ",
    "Q/F 36.9 L/h, V2/F 120 L, absorption 1.13 /h. CL/F = (4.13 - 0.0451 x ",
    "[age - 58]) x (1 + 0.253 if male); V1/F = 62.1 x (1 + 1.19 if male). ",
    "The fitted cystatin C term on clearance is not applied, because the ",
    "Patient Profile has no cystatin C and serum creatinine is not a ",
    "substitute for it; the fitted BMI term on volume is not applied, ",
    "because the equation as printed is not readable. The matrix is ",
    "PLASMA, and the large apparent volumes reflect chlorthalidone's ",
    "partitioning into erythrocytes. No blood pressure is predicted. ",
    "https://doi.org/10.1002/jcph.684"
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
