# Closed Form, on PK set only
advanceClosedForm0 <- function(dose, pkSet, maximum, plotRecovery, emerge)
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
      l1_dt <- exp(-lambda_1 * dt)
      l2_dt <- exp(-lambda_2 * dt)
      l3_dt <- exp(-lambda_3 * dt)

      p_bolus_l1 <- p_coef_bolus_l1 * bolusLine
      p_bolus_l2 <- p_coef_bolus_l2 * bolusLine
      p_bolus_l3 <- p_coef_bolus_l3 * bolusLine

      p_infusion_l1 <- p_coef_infusion_l1 * rate * (1 - l1_dt)
      p_infusion_l2 <- p_coef_infusion_l2 * rate * (1 - l2_dt)
      p_infusion_l3 <- p_coef_infusion_l3 * rate * (1 - l3_dt)

      p_state_l1 <- advanceState(l1_dt, p_bolus_l1, p_infusion_l1, 0, L)
      p_state_l2 <- advanceState(l2_dt, p_bolus_l2, p_infusion_l2, 0, L)
      p_state_l3 <- advanceState(l3_dt, p_bolus_l3, p_infusion_l3, 0, L)

      # Wrap up, calculate Ce
      Cp <- p_state_l1 + p_state_l2 + p_state_l3
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

        e_state_l1  <- advanceState(l1_dt,  e_bolus_l1,  e_infusion_l1,  0, L)
        e_state_l2  <- advanceState(l2_dt,  e_bolus_l2,  e_infusion_l2,  0, L)
        e_state_l3  <- advanceState(l3_dt,  e_bolus_l3,  e_infusion_l3,  0, L)
        e_state_ke0 <- advanceState(ke0_dt, e_bolus_ke0, e_infusion_ke0, 0, L)
        recovery <- sapply(
          1:L,
          function(i)
          (
            recoveryCalc(
              c(
                e_state_l1[i],
                e_state_l2[i],
                e_state_l3[i],
                e_state_ke0[i]
              ),
              c(
                lambda_1,
                lambda_2,
                lambda_3,
                ke0
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
