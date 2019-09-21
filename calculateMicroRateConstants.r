# Calculate Microrate Constants
calculateMicroRateConstants <- function(Z)
{
  k10 = Z$cl1/Z$v1
  k12 = Z$cl2/Z$v1
  k13 = Z$cl3/Z$v1
  k21 = Z$cl2/Z$v2
  k31 = Z$cl3/Z$v3
  
  lambda <- cube (k10, k12, k13, k21, k31)
  return(
    list(
      k10 = k10,
      k12 = k12,
      k13 = k13,
      k21 = k21,
      k31 = k31,
      l1 = lambda[1],
      l2 = lambda[2],
      l3 = lambda[3]
    )
  )
}
