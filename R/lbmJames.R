# Lean body mass according to the (unfortunate) James equation
lbmJames <- function (weight, height, sex)
{
  if (sex == SEX_FEMALE)
    lbm = 1.07 * weight - 148 * (weight/height)^2
  else
    lbm = 1.10 * weight - 128 * (weight/height)^2
  return(lbm)
}
