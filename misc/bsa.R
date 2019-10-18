# Body Surface Area calculation
bsa <- function(weight, height)
{
  return( weight^0.425 * height ^ 0.725 * 0.007184)
}
