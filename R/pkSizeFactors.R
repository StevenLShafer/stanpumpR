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
#' @param cohort `NULL`, or the size factors of the source cohort's typical
#'   patient from [cohortSizeFactors()], for a model whose published parameters
#'   carry no size covariate and so belong to that patient rather than to the
#'   70 kg reference man.  With `adjustToFFM = TRUE` the factors are then
#'   relative to that patient, who receives the published values; with
#'   `adjustToFFM = FALSE` it is ignored.
#' @return a list: `volume` and `clearance` (the multipliers), `ffm` (kg),
#'   `ffmReference` (kg), and `pkWeight`, the total body weight a model scaled on
#'   weight alone would need to produce the same volumes (70 x `volume`).
#' @keywords internal
pkSizeFactors <- function(weight, height, age, sex, adjustToFFM = TRUE,
                          legacyVolume = weight / FFM_REFERENCE_WEIGHT,
                          legacyClearance = legacyVolume, cohort = NULL)
{
  ffm <- ffmAlSallami(weight, height, age, sex)
  if (isTRUE(adjustToFFM)) {
    volume <- ffm / FFM_REFERENCE
    clearance <- volume^0.75
    if (!is.null(cohort)) {
      volume <- volume / cohort$volume
      clearance <- clearance / cohort$clearance
    }
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

#' The size factors of a source cohort's typical patient
#'
#' A model whose published parameters carry no size covariate describes the
#' typical patient of the cohort it was fitted in, not the 70 kg reference man
#' (Steven L. Shafer, 2026-11-18; `docs/weight-adjustment.md`, *Anchoring at
#' the source's cohort*).  This returns that patient's factors, which
#' [pkSizeFactors()] divides out (`cohort =`), so that a patient like the
#' cohort's receives the published values.  The cohort's sexes are taken as
#' two patients of the cohort's typical weight, height (or BMI) and age, a man
#' and a woman, whose fat-free-mass factors are averaged geometrically,
#' weighted by the numbers of men and women: a 100 kg man's fat-free mass is
#' about a fifth above a 100 kg woman's, so a cohort of both is neither.
#'
#' @param weight the cohort's typical weight, kg
#' @param age the cohort's typical age, years
#' @param men,women the numbers of men and women in the cohort
#' @param height the cohort's typical height, cm; or `NULL` with `bmi`
#' @param bmi the cohort's typical BMI, kg/m2, used for the height when no
#'   height is printed
#' @return a list: `volume` and `clearance`, the typical patient's factors
#' @keywords internal
cohortSizeFactors <- function(weight, age, men, women, height = NULL, bmi = NULL)
{
  if (is.null(height)) {
    if (is.null(bmi)) stop("cohortSizeFactors() needs a height or a BMI")
    height <- 100 * sqrt(weight / bmi)
  }
  if (!(men >= 0 && women >= 0 && men + women > 0))
    stop("cohortSizeFactors() needs a cohort of at least one man or woman")
  share <- function(sex) {
    f <- pkSizeFactors(weight, height, age, sex, TRUE)
    c(volume = log(f$volume), clearance = log(f$clearance))
  }
  mix <- exp((men * share(SEX_MALE) + women * share(SEX_FEMALE)) / (men + women))
  list(volume = unname(mix["volume"]), clearance = unname(mix["clearance"]))
}
