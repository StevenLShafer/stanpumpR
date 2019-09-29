# Closed Form, one PK set only, oral doses included alongside IV doses
advanceClosedFormPO <- function(dose, pkSet, maximum, plotRecovery, emerge)
{
  ##############################################
  # Begin closed form approach, time invariant #
  # About 2.5 times faster than closed form    #
  # time variant model (ClosedForm1)           #
  # Modified to include PO and IV delivery     #
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
  bolusLine <- infusionLine <- poLine <- dt <- rate <- rep(0, L)
  for (i in 1:L)
  {
    bolusLine[i]    <- sum(dose$Dose[dose$Time == timeLine[i] & dose$Bolus])
    poLine[i]       <- sum(dose$Dose[dose$Time == timeLine[i] & dose$PO])
    USE <- dose$Time == timeLine[i] & !dose$Bolus & !dose$PO
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
  # cat("poLine\n")
  # print(poLine)
  
  results <- with (
    pkSet,
    {
      # Vectorize calculations
      l1 <- exp(-lambda_1 * dt)
      l2 <- exp(-lambda_2 * dt)
      l3 <- exp(-lambda_3 * dt)
      l4 <- exp(-ka * dt)
      
      pbolus1 <- p_coef_bolus_1 * bolusLine
      pbolus2 <- p_coef_bolus_2 * bolusLine
      pbolus3 <- p_coef_bolus_3 * bolusLine
      pinfusion1 <- p_coef_infusion_1 * rate * (1 - l1)        
      pinfusion2 <- p_coef_infusion_2 * rate * (1 - l2)        
      pinfusion3 <- p_coef_infusion_3 * rate * (1 - l3)
      
      # cat("p_coef_po_1", p_coef_po_1, "\n")
      # cat("p_coef_po_2", p_coef_po_2, "\n")
      # cat("p_coef_po_3", p_coef_po_3, "\n")
      # cat("p_coef_po_4", p_coef_po_4, "\n")
      
      ppo1 <- p_coef_po_1 * poLine
      ppo2 <- p_coef_po_2 * poLine
      ppo3 <- p_coef_po_3 * poLine
      ppo4 <- p_coef_po_4 * poLine
      
      # cat("ppo1\n")
      # print(ppo1)
      # cat("ppo2\n")
      # print(ppo2)
      # cat("ppo3\n")
      # print(ppo3)
      # cat("ppo4\n")
      # print(ppo4)
      # cat("exp(-ka * dt)\n")
      # print(exp(-ka * dt))
      
      p_state_1 <- advanceStatePO(l1, pbolus1, pinfusion1, ppo1, L)
      p_state_2 <- advanceStatePO(l2, pbolus2, pinfusion2, ppo2, L)
      p_state_3 <- advanceStatePO(l3, pbolus3, pinfusion3, ppo3, L)
      p_state_4 <- advanceStatePO(exp(-ka * dt), rep(0, L)  , rep(0, L)  , ppo4, L)


      # Wrap up, calculate Ce
      # cat("p_state_1\n")
      # print(p_state_1)
      # cat("p_state_2\n")
      # print(p_state_2)
      # cat("p_state_3\n")
      # print(p_state_3)
      # cat("p_state_4\n")
      # print(p_state_4)
      Cp <- p_state_1 + p_state_2 + p_state_3 + p_state_4
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
        epo1 <- e_coef_po_1 * poLine
        epo2 <- e_coef_po_2 * poLine
        epo3 <- e_coef_po_3 * poLine
        epo4 <- e_coef_po_4 * poLine
        epo5 <- e_coef_po_5 * poLine
        
        e_state_1 <- advanceStatePO(l1, ebolus1, einfusion1, epo1, L)
        e_state_2 <- advanceStatePO(l2, ebolus2, einfusion2, epo2, L)
        e_state_3 <- advanceStatePO(l3, ebolus3, einfusion3, epo3, L)
        e_state_4 <- advanceStatePO(l4, ebolus4, einfusion4, epo4, L)
        e_state_5 <- advanceStatePO(exp(-ka * dt), rep(0, L), rep(0, L), epo5, L)
        
        recovery <- sapply(
          1:L, 
          function(i) 
          (
            recoveryCalc(
              c(
                e_state_1[i], 
                e_state_2[i], 
                e_state_3[i],
                e_state_4[i],
                e_state_5[i]
              ),
              c(
                lambda_1,
                lambda_2,
                lambda_3,
                lambda_4,
                ka
              ),
              emerge)
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
