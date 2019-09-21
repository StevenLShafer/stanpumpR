# Model for ODE solver
odeModel <- function(
  t, 
  state, 
  parameters
) 
{
  with(
    as.list(
      c(state, 
        parameters
      )
    ),
    {
      da1 <-  a2 * k21 + a3 * k31 - a1 * k + infusion  
      da2 <-  a1 * k12 - a2 * k21
      da3 <-  a1 * k13 - a3 * k31
      list(c(da1, da2, da3)) # return: derivatives of the state variables
    }
  )
}
