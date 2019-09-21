# Function to calculate Ce from Cp, dt, and ke0
calculateCe <- function(Cp, ke0, dt, L)
{
  Ce <- rep(0,L)
  for (i in 2:L)
  {
    y0 = Cp[i-1]
    y1 = Cp[i]
    
    if (y0 <= y1 || y0 == 0 || y1 == 0)
    {
      k = (y1-y0)/dt[i]
      input = k * dt[i] + (ke0[i] * y0 - k) * (1 - exp(-ke0[i] * dt[i])) / ke0[i]
    } else {
      k = (log(y1) - log(y0))/dt[i]
      input = y0 * ke0[i]/(k+ke0[i])*(exp(k * dt[i]) - exp(-ke0[i] * dt[i]))
    }
    Ce[i] = Ce[i-1] * exp(-ke0[i] * dt[i]) + input
  }
  return(Ce)
}
