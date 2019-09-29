# Closed form, multiple PK sets
advanceClosedForm1 <- function(dose, events, pkSets, maximum, plotRecovery, emerge)
{
  ##############################
  # Begin closed form approach #
  ##############################
  
  # Create timeline 
  timeLine <- sort(unique(c(0, dose$Time, events$Time, events$Time - 0.01, dose$Time[dose$Bolus] - 0.01, maximum)))
  timeLine <- timeLine[timeLine >=0]
  
  # Fill in gaps using exponentially decreasing amounts
  gapStart <- timeLine[1:length(timeLine)-1]
  gapEnd   <- timeLine[2:length(timeLine)]
  start <- min(0.693/pkSets$default$lambda_4 / 4, 1)
  newTimes <- c(exp(log(start)+0:40 * log(1440/start)/41))
  for (i in 1:length(gapEnd))
  {
    distance <- gapEnd[i] - gapStart[i]
    timeLine <- c(timeLine, gapStart[i] + newTimes[newTimes <= distance])
  }
  timeLine <- sort(unique(timeLine))
  L <- length(timeLine)
  
  # Create bolusLine and infusionLine
  bolusLine <- infusionLine <- pkLine <- dt <- rate <- rep(0, L)
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
    pkLine[i] <- events$Event[tail(which(events$Time <= timeLine[i]),1)]    
  }
  
  # Set up time varying parameters
  parameters <-   as.data.frame(
    cbind(
      v1  = map_dbl(pkSets, "v1"),
      k10 = map_dbl(pkSets, "k10"),
      k12 = map_dbl(pkSets, "k12"),
      k13 = map_dbl(pkSets, "k13"),
      k21 = map_dbl(pkSets, "k21"),
      k31 = map_dbl(pkSets, "k31"),
      ke0 = map_dbl(pkSets, "lambda_4"),
      lambda_1 = map_dbl(pkSets, "lambda_1"),
      lambda_2 = map_dbl(pkSets, "lambda_2"),
      lambda_3 = map_dbl(pkSets, "lambda_3"),
      p_coef_bolus_1 = map_dbl(pkSets, "p_coef_bolus_1"),
      p_coef_bolus_2 = map_dbl(pkSets, "p_coef_bolus_2"),
      p_coef_bolus_3 = map_dbl(pkSets, "p_coef_bolus_3"),
      p_coef_infusion_1 = map_dbl(pkSets, "p_coef_infusion_1"),
      p_coef_infusion_2 = map_dbl(pkSets, "p_coef_infusion_2"),
      p_coef_infusion_3 = map_dbl(pkSets, "p_coef_infusion_3")
    ),stringsAsFactors = FALSE)
  
  #Set up time varying parameters
  parameters$k <- parameters$k10 + parameters$k12 + parameters$k13
  
  if (sum(parameters$k21) == 0)
  {
    parameters$v2 <- 0
  } else {
    parameters$v2 <- parameters$v1 * parameters$k12 / parameters$k21
  }
  if (sum(parameters$k31) == 0)
  {
    parameters$v3 <- 1
  } else {
    parameters$v3 <- parameters$v1 * parameters$k13 / parameters$k31
  }
  
  v1  <- parameters[pkLine,"v1"]
  k10 <- parameters[pkLine,"k10"]
  k12 <- parameters[pkLine,"k12"]
  k13 <- parameters[pkLine,"k13"]
  k21 <- parameters[pkLine,"k21"]
  k31 <- parameters[pkLine,"k31"]
  ke0 <- parameters[pkLine,"ke0"]
  k   <- parameters[pkLine,"k"]
  lambda_1   <- parameters[pkLine, "lambda_1"]
  lambda_2   <- parameters[pkLine, "lambda_2"]
  lambda_3   <- parameters[pkLine, "lambda_3"]
  
  p_coef_bolus_1   <- parameters[pkLine, "p_coef_bolus_1"]
  p_coef_bolus_2   <- parameters[pkLine, "p_coef_bolus_2"]
  p_coef_bolus_3   <- parameters[pkLine, "p_coef_bolus_3"]
  
  infusionpkLine <- c(pkLine[1],pkLine[1:(L-1)]) # the prior parameters are used to move the infusion forward
  p_coef_infusion_1   <- parameters[infusionpkLine, "p_coef_infusion_1"]
  p_coef_infusion_2   <- parameters[infusionpkLine, "p_coef_infusion_2"]
  p_coef_infusion_3   <- parameters[infusionpkLine, "p_coef_infusion_3"]
  
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
  
  p_state_1 <- p_state_2 <- p_state_3 <- rep(0, L)
  
  for (i in 1:(nrow(events)-1))
  {
    times <- which(timeLine >= events$Time[i] & timeLine <= events$Time[i+1])
    p_state_1[times] <- advanceState(l1[times], pbolus1[times], pinfusion1[times], p_state_1[times[1]],length(times))
    p_state_2[times] <- advanceState(l2[times], pbolus2[times], pinfusion2[times], p_state_2[times[1]],length(times))
    p_state_3[times] <- advanceState(l3[times], pbolus3[times], pinfusion3[times], p_state_3[times[1]],length(times))
    now <- which(timeLine == events$Time[i+1])
    if (now < L)
    {
      # Reverse Bolus Dose (It will go in when the next PK starts)
      p_state_1[now] <- p_state_1[now] - pbolus1[now]
      p_state_2[now] <- p_state_2[now] - pbolus2[now]
      p_state_3[now] <- p_state_3[now] - pbolus3[now]
      
      # Convert state variables
      oldPK <- as.list(parameters[events$Event[i],])
      newPK <- as.list(parameters[events$Event[i+1],])
      oldState <- c(p_state_1[now], p_state_2[now], p_state_3[now])
      newState <- convertState(oldState, oldPK, newPK)
      
      cat("oldState\n")
      print(oldState)
      cat("newState\n")
      print(newState)

      p_state_1[now] <- newState[1]
      p_state_2[now] <- newState[2]
      p_state_3[now] <- newState[3]
      
      # Infusion was processed with prior PK, so infusion is now 0
      pinfusion1[now] <- pinfusion2[now] <- pinfusion3[now] <- 0
      
      # No decrement in time either
      l1[now] <- l2[now] <- l3[now] <- 1 
    }
  }
  
  # Wrap up, calculate Ce
  Cp <- p_state_1 + p_state_2 + p_state_3
  if (sum(is.na(Cp)) + sum(is.nan(Cp)) > 0)
  {
    cat("Problem with calculation of Cp\n")
    print(Cp)
    cat("pkLine:\n")
    print(pkLine)
  }
  Ce <- calculateCe(Cp, ke0, dt, L)
  
  if (plotRecovery)
  {
    # Note that I am looking at plasma, not effect site.
    # This is for reasons of speed. I abandoned move forward e_state variables in the interest of
    # speed, and because I would have to take them through the transformation when the PK changes.
    # That seems computationally hazardous
    recovery <- sapply(
      1:L, 
      function(i) 
        (
          recoveryCalc(
            c(
              p_state_1[i], 
              p_state_2[i], 
              p_state_3[i],
              0
            ),
            c(
              lambda_1[i],
              lambda_2[i],
              lambda_3[i],
              0
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
