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
# The raw equation is drawn by the help figure (helpJamesLBMFigure()) so the
# parabola can be seen; every model that reads James -- the Schnider propofol
# and Minto remifentanil companions (R/alternativeModels.R), istradefylline
# and tolcapone -- reads lbmJamesCapped() below.
lbmJames <- function (weight, height, sex)
{
  if (sex == SEX_FEMALE)
    lbm = 1.07 * weight - 148 * (weight/height)^2
  else
    lbm = 1.10 * weight - 128 * (weight/height)^2
  return(lbm)
}

# The James lean body mass held at its peak past the peak, for every model
# whose source used James as a covariate: the Schnider propofol and Minto
# remifentanil companions, istradefylline and tolcapone.  Past the peak the
# parabola credits a heavier patient with less lean tissue, and past twice
# the peak BMI it is negative: a 249 kg, 170 cm man (BMI 86, within the
# Patient Profile's range) made istradefylline's (LBM/55.6)^0.106 NaN and
# got no curve and no message, and Minto's V1 went negative at 200 kg
# (2026-10-31).  Holding the mass at the most the formula gives for the
# height keeps it positive and non-decreasing in weight; it is a guard, not
# a claim about the sources, which fitted no such patient.
#
# Decision (Steven L. Shafer, 2026-10-31): the parabola's downward turn was
# wholly unforeseen when the Schnider and Minto models were built, so the
# companions hold the lean body mass at the peak rather than reproduce the
# fall as the pumps' raw equation would (the pumps refuse such a patient
# instead, at a BMI of 42 in men and 35 in women); and a floor at the peak
# was chosen over refusing the patient or substituting Al-Sallami.
lbmJamesCapped <- function (weight, height, sex)
{
  a <- if (sex == SEX_FEMALE) 1.07 else 1.10
  b <- if (sex == SEX_FEMALE) 148 else 128
  peakWeight <- a * height^2 / (2 * b)
  lbmJames(min(weight, peakWeight), height, sex)
}
