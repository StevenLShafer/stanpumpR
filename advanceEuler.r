advanceEuler <- function(dose, events, pkSets, maximum)
{
  ##############################
  # Begin Euler's method       #
  ##############################
  # Create timeline 
  timeLine <- sort(unique(c(0, dose$Time, events$Time, dose$Time[dose$Bolus] - .01, maximum)))
  timeLine <- timeLine[timeLine >=0]
  
  # Fill in gaps using exponentially decreasing amounts
  step <- 1/max(pkSets$default$lambda_1,pkSets$default$lambda_4)/2.1
  newTimes <- c(step/20 * 0:20 + step/21:1, seq(step*3,1440, by=step))
  
  # Create timeline using log spaced steps
  gapStart <- timeLine[1:length(timeLine)-1]
  gapEnd   <- timeLine[2:length(timeLine)]
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
      ke0 = map_dbl(pkSets, "lambda_4")
    ),stringsAsFactors = FALSE)
  #Set up time varying parameters
  parameters$k <- parameters$k10 + parameters$k12 + parameters$k13
  
  v1  <- parameters[pkLine,"v1"]
  k10 <- parameters[pkLine,"k10"]
  k12 <- parameters[pkLine,"k12"]
  k13 <- parameters[pkLine,"k13"]
  k21 <- parameters[pkLine,"k21"]
  k31 <- parameters[pkLine,"k31"]
  ke0 <- parameters[pkLine,"ke0"]
  k   <- parameters[pkLine,"k"]
  
  # Initialize Model
  a1 <- 0
  a2 <- 0
  a3 <- 0
  
  # Advance Model
  results <- as.data.frame(cbind(timeLine,0,0))
  names(results) <- c("Time","Cp","Ce")
  
  for (i in 1:length(timeLine))
  {
    # Process bolus
    da1 <- (rate[i] + a2 * k21[i] + a3 * k31[i] - a1 * k[i]) * dt[i] 
    da2 <- (a1 * k12[i] - a2 * k21[i]) * dt[i] 
    da3 <- (a1 * k13[i] - a3 * k31[i]) * dt[i] 
    
    a1 <- a1 + da1 + bolusLine[i]
    a2 <- a2 + da2
    a3 <- a3 + da3
    
    results$Cp[i] <- a1 / v1[i]
  }
  L <- nrow(results)
  results$Ce <- calculateCe(results$Cp, ke0, dt, L)
  return(results)
}
