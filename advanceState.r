#Advance a single state variable over time
advanceState <- function(l, bolus, infusion, start, L)
{
  Z <- lapply(
    1:L, 
    function(i) (
      list(
        l = l[i],
        bolus = bolus[i],
        infusion = infusion[i]
      )
    )
  )
  return(
    Reduce(
      function(state, Z) {state * Z$l +  Z$bolus + Z$infusion}, 
      Z,
      init = start,
      accumulate = TRUE
    )[2:(L+1)]
  )
}
