#Advance a single state variable over time
advanceStatePO <- function(l, bolus, infusion, PO, IM, IN, L)
{
  Z <- lapply(
    1:L,
    function(i) (
      list(
        l = l[i],
        bolus = bolus[i],
        infusion = infusion[i],
        PO = PO[i],
        IM = IM[i],
        IN = IN[i]
      )
    )
  )
  return(
    Reduce(
      function(state, Z) {state * Z$l +  Z$bolus + Z$infusion + Z$PO + Z$IM + Z$IN},
      Z,
      init = 0,
      accumulate = TRUE
    )[2:(L+1)]
  )
}
