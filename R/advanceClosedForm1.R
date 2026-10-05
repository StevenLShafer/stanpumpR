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
  start <- min(0.693/pkSets[[PK_EVENT_DEFAULT]]$lambda_4 / 4, 1)
  newTimes <- c(exp(log(start)+0:40 * log(MINS_PER_DAY/start)/41))
  for (i in 1:length(gapEnd))
  {
    distance <- gapEnd[i] - gapStart[i]
    timeLine <- c(timeLine, gapStart[i] + newTimes[newTimes <= distance])
  }
  timeLine <- sort(unique(timeLine))
  L <- length(timeLine)
  doseNA <- rep(0, L)

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
    pkLine[i] <- events$Event[utils::tail(which(events$Time <= timeLine[i]),1)]
  }

  # Set up time varying parameters
  parameters <-   as.data.frame(
    cbind(
      v1  = purrr::map_dbl(pkSets, "v1"),
      k10 = purrr::map_dbl(pkSets, "k10"),
      k12 = purrr::map_dbl(pkSets, "k12"),
      k13 = purrr::map_dbl(pkSets, "k13"),
      k21 = purrr::map_dbl(pkSets, "k21"),
      k31 = purrr::map_dbl(pkSets, "k31"),
      ke0 = purrr::map_dbl(pkSets, "ke0"),
      lambda_1 = purrr::map_dbl(pkSets, "lambda_1"),
      lambda_2 = purrr::map_dbl(pkSets, "lambda_2"),
      lambda_3 = purrr::map_dbl(pkSets, "lambda_3"),
      p_coef_bolus_l1 = purrr::map_dbl(pkSets, "p_coef_bolus_l1"),
      p_coef_bolus_l2 = purrr::map_dbl(pkSets, "p_coef_bolus_l2"),
      p_coef_bolus_l3 = purrr::map_dbl(pkSets, "p_coef_bolus_l3"),
      p_coef_infusion_l1 = purrr::map_dbl(pkSets, "p_coef_infusion_l1"),
      p_coef_infusion_l2 = purrr::map_dbl(pkSets, "p_coef_infusion_l2"),
      p_coef_infusion_l3 = purrr::map_dbl(pkSets, "p_coef_infusion_l3")
    ))

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

  p_coef_bolus_l1   <- parameters[pkLine, "p_coef_bolus_l1"]
  p_coef_bolus_l2   <- parameters[pkLine, "p_coef_bolus_l2"]
  p_coef_bolus_l3   <- parameters[pkLine, "p_coef_bolus_l3"]


  infusionpkLine <- c(pkLine[1],pkLine[1:(L-1)]) # the prior parameters are used to move the infusion forward
  p_coef_infusion_l1   <- parameters[infusionpkLine, "p_coef_infusion_l1"]
  p_coef_infusion_l2   <- parameters[infusionpkLine, "p_coef_infusion_l2"]
  p_coef_infusion_l3   <- parameters[infusionpkLine, "p_coef_infusion_l3"]

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

  p_state_l1 <- p_state_l2 <- p_state_l3 <- rep(0, L)

  for (i in 1:(nrow(events)-1))
  {
    times <- which(timeLine >= events$Time[i] & timeLine <= events$Time[i+1])
    p_state_l1[times] <- advanceState(l1_dt[times], p_bolus_l1[times], p_infusion_l1[times], p_state_l1[times[1]],length(times))
    p_state_l2[times] <- advanceState(l2_dt[times], p_bolus_l2[times], p_infusion_l2[times], p_state_l2[times[1]],length(times))
    p_state_l3[times] <- advanceState(l3_dt[times], p_bolus_l3[times], p_infusion_l3[times], p_state_l3[times[1]],length(times))
    now <- which(timeLine == events$Time[i+1])
    if (now < L)
    {
      # Reverse Bolus Dose (It will go in when the next PK starts)
      p_state_l1[now] <- p_state_l1[now] - p_bolus_l1[now]
      p_state_l2[now] <- p_state_l2[now] - p_bolus_l2[now]
      p_state_l3[now] <- p_state_l3[now] - p_bolus_l3[now]

      # Convert state variables
      oldPK <- as.list(parameters[events$Event[i],])
      newPK <- as.list(parameters[events$Event[i+1],])
      oldState <- c(p_state_l1[now], p_state_l2[now], p_state_l3[now])
      newState <- convertState(oldState, oldPK, newPK)

      p_state_l1[now] <- newState[1]
      p_state_l2[now] <- newState[2]
      p_state_l3[now] <- newState[3]

      # Infusion was processed with prior PK, so infusion is now 0
      p_infusion_l1[now] <- p_infusion_l2[now] <- p_infusion_l3[now] <- 0

      # No decrement in time either
      l1_dt[now] <- l2_dt[now] <- l3_dt[now] <- 1
    }
  }


  # Wrap up, calculate Ce
  Cp <- p_state_l1 + p_state_l2 + p_state_l3
  if (sum(is.na(Cp)) + sum(is.nan(Cp)) > 0)
  {
    message("Problem with calculation of Cp")
    print(Cp)
    message("pkLine:")
    print(pkLine)
  }
  Ce <- calculateCe(Cp, ke0, dt, L)

  temp <- data.frame(
    Time = round(timeLine, 2),
    State1 = round(p_state_l1, 2),
    State2 = round(p_state_l2, 2),
    State3 = round(p_state_l3, 2),
    Cp  = round(Cp, 2),
    Ce  = round(Ce, 2)
  )

  if (plotRecovery)
  {
    # Time until the EFFECT SITE falls to the threshold if delivery stops now.
    #
    # This used to look at plasma, "for reasons of speed", because the
    # effect-site states are not carried through the changes in PK.  They do not
    # need to be.  Once delivery stops, plasma is the three exponentials already
    # in hand,
    #     Cp(t) = sum_i p_i exp(-lambda_i t),
    # and the effect site, driven by that plasma from its present value Ce0, is
    #     Ce(t) = sum_i a_i exp(-lambda_i t) + (Ce0 - sum_i a_i) exp(-ke0 t),
    #     a_i   = p_i * ke0 / (ke0 - lambda_i),
    # which is exact, costs nothing, and uses the PK in force at that moment --
    # the same assumption the other two engines make.
    # (Claude Code, Claude Fable 5.1, 2026-10-05; verified against stopping
    # delivery in the simulation by tests/testthat/test-recovery-engines.R.)
    recovery <- sapply(
      1:L,
      function(i)
      {
        lam <- c(lambda_1[i], lambda_2[i], lambda_3[i])
        p   <- c(p_state_l1[i], p_state_l2[i], p_state_l3[i])
        a   <- p * ke0[i] / (ke0[i] - lam)
        recoveryCalc(c(a, Ce[i] - sum(a)), c(lam, ke0[i]), emerge)
      }
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
