# Size scaling of pharmacokinetic parameters to fat-free mass.
#
# Most of the models in the drug library were reported for a typical adult and
# scale, if at all, with total body weight.  Clearance tracks lean tissue, not
# fat, so stanpumpR scales those models to the patient's fat-free mass (FFM)
# instead.  The scaling is anchored to the FFM of the 70 kg, 170 cm reference
# male: his factors are exactly 1, so a 70 kg man still receives the model's
# published parameters, and a package insert's per-kilogram dose for him is
# unchanged.  Everyone else is scaled from there.
#
# Full write-up: docs/weight-adjustment.md

#' Fat-free mass from weight, height, age and sex
#'
#' Al-Sallami et al. (2015), *Clin Pharmacokinet* 54:1169-1178: the adult
#' fat-free mass of Janmahasatian et al. (2005) multiplied by a sex-specific
#' sigmoid maturation function of postnatal age.  The maturation term is 1 in
#' men from about 20 years, so for an adult man this is the Janmahasatian
#' formula.  In women it approaches 1 slowly (1.03 at 18 years, 1.012 at 50),
#' so an adult woman's fat-free mass is 1-3% above Janmahasatian's.  The model
#' was built on ages 3 to 29 years and adult data; below 3 years it is an
#' extrapolation.
#'
#' @param weight weight in kg
#' @param height height in cm
#' @param age age in years
#' @param sex `"male"` or `"female"`
#' @return fat-free mass in kg
#' @keywords internal
ffmAlSallami <- function(weight, height, age, sex)
{
  BMI <- weight / (height / 100)^2
  if (sex == SEX_MALE) {
    maturation <- 0.88 + (1 - 0.88) / (1 + (age / 13.4)^(-12.7))
    maturation * 9270 * weight / (6680 + 216 * BMI)
  } else {
    maturation <- 1.11 + (1 - 1.11) / (1 + (age / 7.1)^(-1.1))
    maturation * 9270 * weight / (8780 + 244 * BMI)
  }
}

# The reference patient every size-scaled model is anchored to.  Age 35 is the
# Eleveld reference age; the maturation term is 1 there to seven decimals.
FFM_REFERENCE_WEIGHT <- 70
FFM_REFERENCE_HEIGHT <- 170
FFM_REFERENCE_AGE    <- 35
FFM_REFERENCE_SEX    <- SEX_MALE
FFM_REFERENCE <- ffmAlSallami(FFM_REFERENCE_WEIGHT, FFM_REFERENCE_HEIGHT,
                              FFM_REFERENCE_AGE, FFM_REFERENCE_SEX)  # 54.48 kg

#' Size factors for scaling a drug model's volumes and clearances
#'
#' Returns the multipliers a drug model applies to its reference (70 kg)
#' volumes and clearances.  With `adjustToFFM = TRUE` the volume factor is the
#' patient's fat-free mass over the reference male's, and the clearance factor
#' is that ratio to the 0.75 power.  With `adjustToFFM = FALSE` the factors are
#' whatever the model used before fat-free-mass scaling was introduced, which
#' the caller supplies as `legacyVolume` and `legacyClearance`, so the switch
#' reproduces the historical output exactly.
#'
#' @inheritParams ffmAlSallami
#' @param adjustToFFM scale to fat-free mass (`TRUE`) or use the legacy factors
#' @param legacyVolume volume factor when `adjustToFFM` is `FALSE`; defaults to
#'   `weight / 70`.  Pass `1` for a model whose parameters never scaled.
#' @param legacyClearance clearance factor when `adjustToFFM` is `FALSE`;
#'   defaults to `legacyVolume` (fixed rate constants).  Pass
#'   `(weight / 70)^0.75` for a model that already scaled allometrically.
#' @return a list: `volume` and `clearance` (the multipliers), `ffm` (kg),
#'   `ffmReference` (kg), and `pkWeight`, the total body weight a model scaled on
#'   weight alone would need to produce the same volumes (70 x `volume`).
#' @keywords internal
pkSizeFactors <- function(weight, height, age, sex, adjustToFFM = TRUE,
                          legacyVolume = weight / FFM_REFERENCE_WEIGHT,
                          legacyClearance = legacyVolume)
{
  ffm <- ffmAlSallami(weight, height, age, sex)
  if (isTRUE(adjustToFFM)) {
    volume <- ffm / FFM_REFERENCE
    clearance <- volume^0.75
  } else {
    volume <- legacyVolume
    clearance <- legacyClearance
  }
  list(
    volume = volume,
    clearance = clearance,
    ffm = ffm,
    ffmReference = FFM_REFERENCE,
    pkWeight = FFM_REFERENCE_WEIGHT * ffm / FFM_REFERENCE
  )
}
