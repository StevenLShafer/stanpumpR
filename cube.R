# Solve the cubic equation required to convert k10, k12, k13, k21, and k31
# to 3 exponents 
cube <- function(k10, k12, k13, k21, k31)
{
  toradian <- asin(1.0) * 2.0 / 180.0	# pi/180
  if (k31 > 0)
  {
    # first take roots of X^3 + a2X^2 + a1X^1 + a0 <- 0
    # where the coefficients are :
    a0 <- k10 * k21 * k31
    a1 <- k10 * k31 + k21 * k31 + k21 * k13 + k10 * k21 + k31 * k12
    a2 <- k10 + k12 + k13 + k21 + k31
    
    # now transform to x^3 + px + q <- 0
    p <- a1 - (a2 * a2 / 3.0)
    q <- (2 * a2 * a2 * a2 / 27.0) - (a1 * a2 / 3.0) + a0
    r1 <- sqrt(-(p * p * p) / 27.0)
    phi <- (-q / 2.0) / r1
    if (phi > 1)
      phi <- 1
    else if (phi < -1)
      phi <- -1
    phi <- (acos(phi) / 3.0)
    r1 <- 2.0 * exp(log(r1) / 3.0)
    root1 <- -(cos(phi) * r1 - a2 / 3.0)
    root2 <- -(cos(phi + 120.0 * toradian) * r1 - a2 / 3.0)
    root3 <- -(cos(phi + 240.0 * toradian) * r1 - a2 / 3.0)
  } else {
    if (k21 > 0)
    {
      # first take roots of X^2 - a1X^1 + a0 = 0
      # where the coefficients are :
      a0 <- k10 * k21
      a1 <- -(k10 + k12 + k21)
      root1 <- (-a1 + sqrt(a1 * a1 - 4 * a0)) / 2
      root2 <- (-a1 - sqrt(a1 * a1 - 4 * a0)) / 2
      root3 <- 0
    } else {
      # one compartment model
      root1 <- k10
      root2 <- 0
      root3 <- 0
    }
  }
  
  # sort - nothing fancy is needed
  roots <- sort(c(root1, root2, root3),decreasing=TRUE)
  return(roots)
}
