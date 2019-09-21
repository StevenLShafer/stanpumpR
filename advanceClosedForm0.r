# Closed Form, on PK set only
advanceClosedForm0 <- function(dose, pkSet, maximum, plotRecovery, awake)
{
  ##############################################
  # Begin closed form approach, time invariant #
  # About 2.5 times faster than closed form    #
  # time variant model (ClosedForm1)           #
  ##############################################
  
  # Create timeline 
  timeLine <- sort(unique(c(0, dose$Time, dose$Time[dose$Bolus] - .01, maximum)))
  timeLine <- timeLine[timeLine >=0]
  
  # Fill in gaps using exponentially decreasing amounts
  gapStart <- timeLine[1:length(timeLine)-1]
  gapEnd   <- timeLine[2:length(timeLine)]
  start <- min(0.693/pkSet$lambda_4 / 4, 1)
  newTimes <- c(exp(log(start)+0:40 * log(1440/start)/41))
  for (i in 1:length(gapEnd))
  {
    distance <- gapEnd[i] - gapStart[i]
    timeLine <- c(timeLine, gapStart[i] + newTimes[newTimes <= distance])
  }
  timeLine <- sort(unique(timeLine))
  L <- length(timeLine)
  
  # Create bolusLine and infusionLine
  bolusLine <- infusionLine <- dt <- rate <- rep(0, L)
  for (i in 1:L)
  {
    bolusLine[i]    <- sum(dose$Dose[dose$Time == timeLine[i] & dose$Bolus])
    USE <- dose$Time == timeLine[i] & !dose$Bolus
    if (i == 1)
    {
      infusionLine[i] <- sum(dose$Dose[USE])
      rate[1] <- 0
      dt[1] <- 0
    } else {
      if (sum(USE) == 0)
      {
        infusionLine[i] <- infusionLine[i-1]
      } else {
        infusionLine[i] <- sum(dose$Dose[USE])
      }
      dt[i] <- timeLine[i] - timeLine[i-1]
      rate[i] <- infusionLine[i-1]
    }
  }
  
  results <- with (
    pkSet,
    {
      # Vectorize calculations
      l1 <- exp(-lambda_1 * dt)
      l2 <- exp(-lambda_2 * dt)
      l3 <- exp(-lambda_3 * dt)
      
      pbolus1 <- p_coef_bolus_1 * bolusLine
      pbolus2 <- p_coef_bolus_2 * bolusLine
      pbolus3 <- p_coef_bolus_3 * bolusLine
      pinfusion1 <- p_coef_infusion_1 * rate * (1 - l1)        
      pinfusion2 <- p_coef_infusion_2 * rate * (1 - l2)        
      pinfusion3 <- p_coef_infusion_3 * rate * (1 - l3)        
      
      p_state_1 <- advanceState(l1, pbolus1, pinfusion1, 0, L)
      p_state_2 <- advanceState(l2, pbolus2, pinfusion2, 0, L)
      p_state_3 <- advanceState(l3, pbolus3, pinfusion3, 0, L)
      
      
      # Wrap up, calculate Ce
      Cp <- p_state_1 + p_state_2 + p_state_3
      Ce <- calculateCe(Cp, rep(pkSet$lambda_4, L), dt, L)
      
      if (plotRecovery)
      {
        l4 <- exp(-lambda_4 * dt)
        ebolus1 <- e_coef_bolus_1 * bolusLine
        ebolus2 <- e_coef_bolus_2 * bolusLine
        ebolus3 <- e_coef_bolus_3 * bolusLine
        ebolus4 <- e_coef_bolus_4 * bolusLine
        einfusion1 <- e_coef_infusion_1 * rate * (1 - l1)        
        einfusion2 <- e_coef_infusion_2 * rate * (1 - l2)        
        einfusion3 <- e_coef_infusion_3 * rate * (1 - l3)        
        einfusion4 <- e_coef_infusion_4 * rate * (1 - l4)        
        e_state_1 <- advanceState(l1, ebolus1, einfusion1, 0, L)
        e_state_2 <- advanceState(l2, ebolus2, einfusion2, 0, L)
        e_state_3 <- advanceState(l3, ebolus3, einfusion3, 0, L)
        e_state_4 <- advanceState(l4, ebolus4, einfusion4, 0, L)
        recovery <- sapply(
          1:L, 
          function(i) 
          (
            recoveryCalc(
              c(
                e_state_1[i], 
                e_state_2[i], 
                e_state_3[i],
                e_state_4[i]
              ),
              c(
                lambda_1,
                lambda_2,
                lambda_3,
                lambda_4
              ),
              awake)
          )
        )
      } else {
        recovery <- rep(0, L)
      }
      results <- data.frame(
        Time = timeLine,
        Cp = Cp,
        Ce = Ce,
        Recovery = recovery
      )
      return(results)
    }
  )
  return(results)
}
