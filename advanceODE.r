advanceODE <- function(dose, events, pkSets, maximum)
{
  ##############################
  # Begin ODE method           #
  # About 10 fold slower the   #
  # Euler method above, but    #
  # very accurate.             #
  ##############################
  # Create timeline 
  timeLine <- sort(unique(c(0, dose$Time, events$Time, maximum)))
  timeLine <- timeLine[timeLine >=0]
  
  # Fill in gaps using exponentially decreasing amounts
  start <- min(0.693/pkSets$default$lambda_4 / 4, 1)
  newTimes <- c(0,exp(log(start)+0:40 * log(1440/start)/41))
  
  # Create bolusLine and infusionLine
  bolusLine <- infusionLine <- pkLine <- rep(0, length(timeLine))
  for (i in 1:length(timeLine))
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
      ke0 = map_dbl(pkSets, "lambda_4")
    ),stringsAsFactors = FALSE)
  #Set up time varying parameters
  parameters$k <- parameters$k10 + parameters$k12 + parameters$k13
  
  pkParameters <- data.frame(
    Time = timeLine,
    v1 =  parameters[pkLine,"v1"],
    k10 = parameters[pkLine,"k10"],
    k12 = parameters[pkLine,"k12"],
    k13 = parameters[pkLine,"k13"],
    k21 = parameters[pkLine,"k21"],
    k31 = parameters[pkLine,"k31"],
    ke0 = parameters[pkLine,"ke0"],
    k = parameters[pkLine,"k"]
  )
  
  state <- c(
    a1 = 0, 
    a2 = 0,
    a3 = 0 
  )
  results <- as.data.frame(matrix(nrow=0, ncol=3))
  names(results) <- c("Time","Cp","ke0")
  
  # Advance Model
  for(i in 1:(length(timeLine)-1))
  {
    state[1] <- state[1] + bolusLine[i] # Add in bolus dose
    difference <- timeLine[i+1] - timeLine[i]
    dTimes <- sort(c(timeLine[i] + newTimes[newTimes < difference], timeLine[i+1]-0.01, timeLine[i+1]))
    state <- ode(
      y = state,      # Initial values
      times = dTimes, # times for which output is wanted
      func = odeModel, # sent by call to CEmodel 
      parms = c(
        infusion = infusionLine[i],
        pkParameters[i,])
    )
    newResults <- data.frame(
      Time = state[,1],
      Cp   = state[,2] / pkParameters$v1[i],
      ke0  = pkParameters$ke0[i]
    )
    results <- rbind(results, newResults[1:length(dTimes)-1,])
    state <- state[length(dTimes),c(2,3,4)]
  }
  results <- rbind(results,newResults[length(dTimes),]) # Add last row
  L <- nrow(results)
  results$dt <- 0
  results$dt[2:L] <- results$Time[2:L] - results$Time[1:(L-1)]
  results <- results[results$dt > 0,] # we have duplicates at the times of the infusions
  results$Ce <- calculateCe(results$Cp, results$ke0, results$dt, nrow(results))
  return(results[,c("Time","Cp","Ce")])
}
