advanceRxODE <- function(dose, events, pkSets, maximum)
{
  ##############################
  # Begin RxODE method         #
  ##############################
  # Create timeline 
  timeLine <- sort(unique(c(0, dose$Time, events$Time, maximum)))
  timeLine <- timeLine[timeLine >=0]
  
  # Fill in gaps using exponentially decreasing amounts
  start <- min(0.693/pkSets$default$lambda_4 / 4, 1)
  newTimes <- c(0, exp(log(start)+0:40 * log(1440/start)/41))
  
  # Create bolusLine and infusionLine
  bolusLine <- infusionLine <- dt <- rep(0, length(timeLine))
  L <- length(timeLine)
  for (i in 1:L)
  {
    bolusLine[i]    <- sum(dose$Dose[dose$Time == timeLine[i] & dose$Bolus])
    USE <- dose$Time == timeLine[i] & !dose$Bolus
    if (i == 1)
    {
      infusionLine[i] <- sum(dose$Dose[USE])
    } else {
      if (sum(USE) == 0)
      {
        infusionLine[i] <- infusionLine[i-1]
      } else {
        infusionLine[i] <- sum(dose$Dose[USE])
      }
    }
  }
  dt[1:(L-1)] <- timeLine[2:L] - timeLine[1:(L-1)]
  
  # Set up time varying parameters
  
  state <- c(
    a1 = 0, 
    a2 = 0,
    a3 = 0
  )
  
  results <- as.data.frame(matrix(nrow=0, ncol=3))
  names(results) <- c("Time","Cp","ke0")
  
  # Loop through each PK set
  for (i in 1:(nrow(events)-1))
  {
    # Advance Model
    v1  = pkSets[[i]]$v1
    ke0 = pkSets[[i]]$lambda_4
    parameters <- c(
      v1  = v1,
      k10 = pkSets[[i]]$k10,
      k12 = pkSets[[i]]$k12,
      k13 = pkSets[[i]]$k13,
      k21 = pkSets[[i]]$k21,
      k31 = pkSets[[i]]$k31,
      ke0 = pkSets[[i]]$lambda_4,
      k = pkSets[[i]]$k10 + pkSets[[i]]$k12 + pkSets[[i]]$k13
    )
    
    doseInfEvt <- eventTable()
    USE <- which (timeLine >= events$Time[i] & timeLine <  events$Time[i+1]-0.01)
    for (u in USE)
    {
      # Bolus Dose
      doseInfEvt$add.dosing(
        dose = bolusLine[u], 
        start.time = timeLine[u]
      )
      # Infusion Dose
      doseInfEvt$add.dosing(
        dose = infusionLine[u] * dt[u], 
        rate = infusionLine[u],
        start.time = timeLine[u]
      )
      distance <- timeLine[u+1] - timeLine[u] -.01
      dTimes <- sort(c(timeLine[u] + newTimes[newTimes < distance], timeLine[u+1]-0.01,timeLine[u+1]))
      doseInfEvt$add.sampling(dTimes)
    }
    
    state <- 
      solve(
        RxODEModel, 
        parameters, 
        events = doseInfEvt, 
        state
      )
    
    newResults <- data.frame(
      Time = state[,1],
      Cp   = state[,2] / v1
    )
    newResults$ke0 <- ke0
    results <- rbind(results, newResults[1:nrow(state)-1,])
    state <- as.list(state[nrow(state),c(2,3,4)])
  }
  results <- rbind(results,newResults[nrow(newResults),]) # Add last row
  results$dt <- 0
  L <- nrow(results)
  results$dt[2:L] <- results$Time[2:L] - results$Time[1:(L-1)]
  results <- results[results$dt > 0,] # we have duplicates at the times of the infusions
  results$Ce <- calculateCe(results$Cp, results$ke0, results$dt, nrow(results))
  return(results[,c("Time","Cp","Ce")])
}
