# Calculate the error between the predicted and actual time of peak effect
# site concentration
tPeakError <-   function(lambda_4, tPeak, p_coef_bolus_1,p_coef_bolus_2,p_coef_bolus_3, lambda_1, lambda_2, lambda_3)
{
  e_coef_bolus_1 <- p_coef_bolus_1 / (lambda_4 - lambda_1) * lambda_4

  if (lambda_2 > 0)
  {
    e_coef_bolus_2 <-  p_coef_bolus_2 / (lambda_4 - lambda_2) * lambda_4
  } else {
    e_coef_bolus_2 <- 0
  }
  if (lambda_3 > 0)
  {
    e_coef_bolus_3 <- p_coef_bolus_3 / (lambda_4 - lambda_3) * lambda_4
  } else {
    e_coef_bolus_3 <- 0
  }
  e_coef_bolus_4 <- - e_coef_bolus_1 - e_coef_bolus_2 - e_coef_bolus_3

  predPeak <- stats::optimize(CE,c(0,100), e_coef_bolus_1, e_coef_bolus_2, e_coef_bolus_3, e_coef_bolus_4, lambda_1, lambda_2, lambda_3, lambda_4, maximum=TRUE)$maximum
  return((tPeak-predPeak)^2)
}
