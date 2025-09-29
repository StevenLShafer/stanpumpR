# Calculate recovery time from current effect site concentration to target concentration
# using regression. Note: optimize() is twice as fast as uniroot()
recoveryCalc <-   function(
  state,
  lambda,
  target
  )
{
  if (target >= sum(state)) return(0)
  return (
    stats::optimize(
      function (t, state, lambda, target) { (target - sum(state * exp(-lambda * t)))^2 },
      c(0,1440),
      state = state,
      lambda = lambda,
      target = target,
      tol = 0.1
    )$minimum
  )
}
