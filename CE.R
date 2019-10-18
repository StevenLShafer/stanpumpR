# calculate c3 based on 4 coefficients, 4 exponents, and time 
CE <- function(t, e_coef_bolus_1, e_coef_bolus_2, e_coef_bolus_3, e_coef_bolus_4, lambda_1, lambda_2, lambda_3, lambda_4)
{
  e_coef_bolus_1 * exp(-lambda_1 * t) +
    e_coef_bolus_2 * exp(-lambda_2 * t) +
    e_coef_bolus_3 * exp(-lambda_3 * t) +
    e_coef_bolus_4 * exp(-lambda_4 * t)
}
