# Closed Form, one PK set only, oral, intranasal, and IM doses included alongside IV doses
advanceClosedFormPO_IM_IN <- function(dose, pkSet, maximum, plotRecovery, emerge)
{
  ##############################################
  # Begin closed form approach, time invariant #
  # About 2.5 times faster than closed form    #
  # time variant model (ClosedForm1)           #
  # Modified to include PO and IV delivery     #
  ##############################################
  
  cat("Structure of pkSet\n")
  print(str(pkSet))

  # Add tlag_ to PO, IM, and IN dose times  
  dose$Time[dose$PO] <- dose$Time[dose$PO] + pkSet$tlag_PO
  dose$Time[dose$IM] <- dose$Time[dose$IM] + pkSet$tlag_IM
  dose$Time[dose$IN] <- dose$Time[dose$IN] + pkSet$tlag_IN
  
  # Create timeline 
  timeLine <- sort(
    unique(
      c(
        0, 
        dose$Time, 
        dose$Time - .01, # run until just before next dose
        maximum
        )
      )
    )
  timeLine <- timeLine[timeLine >=0]
  
  # Fill in gaps using exponentially decreasing amounts
  gapStart <- timeLine[1:length(timeLine)-1]
  gapEnd   <- timeLine[2:length(timeLine)]
  start <- min(0.693/pkSet$ke0 / 4, 1)
  newTimes <- c(exp(log(start)+0:40 * log(1440/start)/41))
  for (i in 1:length(gapEnd))
  {
    distance <- gapEnd[i] - gapStart[i]
    timeLine <- c(timeLine, gapStart[i] + newTimes[newTimes <= distance])
  }
  timeLine <- sort(unique(timeLine))
  L <- length(timeLine)
  doseNA <- rep(0, L)
  
  # Create bolusLine and infusionLine
  bolusLine <- infusionLine <- poLine <- imLine <- inLine<- dt <- rate <- doseNA
  for (i in 1:L)
  {
    bolusLine[i]    <- sum(dose$Dose[dose$Time == timeLine[i] & dose$Bolus])
    poLine[i]       <- sum(dose$Dose[dose$Time == timeLine[i] & dose$PO])
    imLine[i]       <- sum(dose$Dose[dose$Time == timeLine[i] & dose$IM])
    inLine[i]       <- sum(dose$Dose[dose$Time == timeLine[i] & dose$IN])
    USE <- dose$Time == timeLine[i] & !dose$Bolus & !dose$PO & !dose$IM & !dose$IN
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
      l1_dt    <- exp(-lambda_1 * dt)
      l2_dt    <- exp(-lambda_2 * dt)
      l3_dt    <- exp(-lambda_3 * dt)
      ka_PO_dt <- exp(-ka_PO * dt)
      ka_IM_dt <- exp(-ka_IM * dt)
      ka_IN_dt <- exp(-ka_IN * dt)

      p_bolus_l1 <- p_coef_bolus_l1 * bolusLine
      p_bolus_l2 <- p_coef_bolus_l2 * bolusLine
      p_bolus_l3 <- p_coef_bolus_l3 * bolusLine
      
      p_infusion_l1 <- p_coef_infusion_l1 * rate * (1 - l1_dt)        
      p_infusion_l2 <- p_coef_infusion_l2 * rate * (1 - l2_dt)        
      p_infusion_l3 <- p_coef_infusion_l3 * rate * (1 - l3_dt)
      
      # cat("p_coef_PO_1", p_coef_PO_1, "\n")
      # cat("p_coef_PO_2", p_coef_PO_2, "\n")
      # cat("p_coef_PO_3", p_coef_PO_3, "\n")
      # cat("p_coef_PO_4", p_coef_PO_4, "\n")
      
      p_PO_l1 <- p_coef_PO_l1 * poLine
      p_PO_l2 <- p_coef_PO_l2 * poLine
      p_PO_l3 <- p_coef_PO_l3 * poLine
      p_PO_ka <- p_coef_PO_ka * poLine
      
      p_IM_l1 <- p_coef_IM_l1 * imLine
      p_IM_l2 <- p_coef_IM_l2 * imLine
      p_IM_l3 <- p_coef_IM_l3 * imLine
      p_IM_ka <- p_coef_IM_ka * imLine

      p_IN_l1 <- p_coef_IN_l1 * inLine
      p_IN_l2 <- p_coef_IN_l2 * inLine
      p_IN_l3 <- p_coef_IN_l3 * inLine
      p_IN_ka <- p_coef_IN_ka * inLine
      
      # cat("ppo1\n")
      # print(ppo1)
      # cat("ppo2\n")
      # print(ppo2)
      # cat("ppo3\n")
      # print(ppo3)
      # cat("ppo4\n")
      # print(ppo4)
      # cat("exp(-ka_ * dt)\n")
      # print(exp(-ka_ * dt))

      p_state_l1    <- advanceStatePO(l1_dt,    p_bolus_l1, p_infusion_l1, p_PO_l1,    p_IM_l1,    p_IN_l1, L)
      p_state_l2    <- advanceStatePO(l2_dt,    p_bolus_l2, p_infusion_l2, p_PO_l2,    p_IM_l2,    p_IN_l2, L)
      p_state_l3    <- advanceStatePO(l3_dt,    p_bolus_l3, p_infusion_l3, p_PO_l3,    p_IM_l3,    p_IN_l3, L)
      p_state_ka_PO <- advanceStatePO(ka_PO_dt, doseNA,     doseNA,        p_PO_ka,    doseNA,     doseNA,  L)
      p_state_ka_IM <- advanceStatePO(ka_IM_dt, doseNA,     doseNA,        doseNA,     p_IM_ka,    doseNA,  L)
      p_state_ka_IN <- advanceStatePO(ka_IN_dt, doseNA,     doseNA,        doseNA,     doseNA,     p_IN_ka, L)
      

      # Wrap up, calculate Ce
      # cat("p_state_1\n")
      # print(p_state_1)
      # cat("p_state_2\n")
      # print(p_state_2)
      # cat("p_state_3\n")
      # print(p_state_3)
      # cat("p_state_4\n")
      # print(p_state_4)
      Cp <- p_state_l1 + p_state_l2 + p_state_l3 + p_state_ka_PO + p_state_ka_IM + p_state_ka_IN
      Ce <- calculateCe(Cp, rep(pkSet$ke0, L), dt, L)
      
      if (plotRecovery)
      {
        ke0_dt <- exp(-ke0 * dt)
        e_bolus_l1  <- e_coef_bolus_l1  * bolusLine
        e_bolus_l2  <- e_coef_bolus_l2  * bolusLine
        e_bolus_l3  <- e_coef_bolus_l3  * bolusLine
        e_bolus_ke0 <- e_coef_bolus_ke0 * bolusLine
        
        e_infusion_l1  <- e_coef_infusion_l1  * rate * (1 - l1_dt)        
        e_infusion_l2  <- e_coef_infusion_l2  * rate * (1 - l2_dt)        
        e_infusion_l3  <- e_coef_infusion_l3  * rate * (1 - l3_dt)        
        e_infusion_ke0 <- e_coef_infusion_ke0 * rate * (1 - ke0_dt)
        
        e_PO_l1  <- e_coef_PO_l1  * poLine
        e_PO_l2  <- e_coef_PO_l2  * poLine
        e_PO_l3  <- e_coef_PO_l3  * poLine
        e_PO_ke0 <- e_coef_PO_ke0 * poLine
        e_PO_ka  <- e_coef_PO_ka  * poLine
        
        e_IM_l1  <- e_coef_IM_l1  * imLine
        e_IM_l2  <- e_coef_IM_l2  * imLine
        e_IM_l3  <- e_coef_IM_l3  * imLine
        e_IM_ke0 <- e_coef_IM_ke0 * imLine
        e_IM_ka  <- e_coef_IM_ka  * imLine
        
        e_IN_l1  <- e_coef_IN_l1  * inLine
        e_IN_l2  <- e_coef_IN_l2  * inLine
        e_IN_l3  <- e_coef_IN_l3  * inLine
        e_IN_ke0 <- e_coef_IN_ke0 * inLine
        e_IN_ka  <- e_coef_IN_ka  * inLine
        
        e_state_l1     <- advanceStatePO(l1_dt,    e_bolus_l1,  e_infusion_l1,  e_PO_l1,  e_IM_l1,  e_IN_l1,  L)
        e_state_l2     <- advanceStatePO(l2_dt,    e_bolus_l2,  e_infusion_l2,  e_PO_l2,  e_IM_l2,  e_IN_l2,  L)
        e_state_l3     <- advanceStatePO(l3_dt,    e_bolus_l3,  e_infusion_l3,  e_PO_l3,  e_IM_l3,  e_IN_l3,  L)
        e_state_ke0    <- advanceStatePO(ke0_dt,   e_bolus_ke0, e_infusion_ke0, e_PO_ke0, e_IM_ke0, e_IN_ke0, L)
        e_state_ka_PO  <- advanceStatePO(ka_PO_dt, doseNA,      doseNA,         e_PO_ka,  doseNA,   doseNA,   L)
        e_state_ka_IM  <- advanceStatePO(ka_IM_dt, doseNA,      doseNA,         doseNA,   e_IM_ka,  doseNA,   L)
        e_state_ka_IN  <- advanceStatePO(ka_IN_dt, doseNA,      doseNA,         doseNA,   doseNA,   e_IN_ka,  L)
        
        recovery <- sapply(
          1:L, 
          function(i) 
          (
            recoveryCalc(
              c(
                e_state_l1[i], 
                e_state_l2[i], 
                e_state_l3[i],
                e_state_ke0[i],
                e_state_ka_PO[i],
                e_state_ka_IM[i],
                e_state_ka_IN[i]
              ),
              c(
                lambda_1,
                lambda_2,
                lambda_3,
                ke0,
                ka_PO,
                ka_IM,
                ka_IN
              ),
              emerge)
          )
        )
      } else {
        recovery <- doseNA
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
