#Advance a single state variable over time
advanceStatePO <- function(l, bolus, infusion, po, L)
{
  Z <- lapply(
    1:L, 
    function(i) (
      list(
        l = l[i],
        bolus = bolus[i],
        infusion = infusion[i],
        po = po[i]
      )
    )
  )
  return(
    Reduce(
      function(state, Z) {state * Z$l +  Z$bolus + Z$infusion + Z$po}, 
      Z,
      init = 0,
      accumulate = TRUE
    )[2:(L+1)]
  )
}
