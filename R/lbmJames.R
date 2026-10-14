# Lean body mass according to the (unfortunate) James equation (James 1976)
#
#   men:   LBM = 1.10 weight - 128 (weight/height)^2
#   women: LBM = 1.07 weight - 148 (weight/height)^2
#
# with weight in kg and height in cm.  At a given height this is a parabola in
# weight, opening downward: LBM = weight * (a - b * BMI / 10000).  It rises,
# peaks at a body mass index of a * 10000 / (2 b), 43.0 in men and 36.1 in
# women, whatever the height, then falls, and reaches zero at twice that BMI
# (85.9 and 72.3).  Past the peak a heavier patient has less lean body mass
# (Bouillon & Shafer, Anesthesiology 1998;89:557-560), which is why the TCI
# pumps limit the Schnider and Minto models to a BMI below 42 in men and 35 in
# women.
#
# stanpumpR scales its own models to the Al-Sallami fat-free mass
# (ffmAlSallami(), R/pkSizeFactors.R), which rises with weight at every weight.
# This function is kept, deliberately, for the Schnider propofol and Minto
# remifentanil models (R/alternativeModels.R), so that they behave as the
# pumps that run them do, failure at the extremes of weight included.
lbmJames <- function (weight, height, sex)
{
  if (sex == SEX_FEMALE)
    lbm = 1.07 * weight - 148 * (weight/height)^2
  else
    lbm = 1.10 * weight - 128 * (weight/height)^2
  return(lbm)
}
