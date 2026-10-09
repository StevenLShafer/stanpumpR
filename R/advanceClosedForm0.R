# Closed Form, on PK set only
advanceClosedForm0 <- function(dose, pkSet, maximum, plotRecovery, emerge)
{
  ##############################################
  # Begin closed form approach, time invariant #
  # About 2.5 times faster than closed form    #
  # time variant model (ClosedForm1)           #
  ##############################################

  # Create timeline: the doses and the instant before each bolus, filled in
  # with a grid scaled to the plot; see R/simulationTimeGrid.R.
  timeLine <- simulationTimeGrid(
    c(dose$Time, dose$Time[dose$Bolus] - PRE_DOSE_OFFSET),
    maximum,
    gridStart(pkSet$ke0)
  )
  L <- length(timeLine)

  # Create bolusLine and infusionLine
  inputs    <- doseLines(dose, timeLine)
  bolusLine <- inputs$bolus
  rate      <- inputs$rate
  dt        <- inputs$dt

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

      Cp <- p_state_l1 + p_state_l2 + p_state_l3
      # The effect site is the sum of its own four exponential states, the
      # same closed form as the plasma.  Until 2026-10-06 it was derived from
      # the plasma curve by calculateCe(), which assumes the plasma is linear
      # or log-linear within each step; after a bolus that misses the peak by
      # 0.2% for propofol and 2% for ketamine, whose ke0 is slow against its
      # fast plasma eigenvalue.  The exact states cost well under a
      # millisecond and are what the recovery time and the TCI controller
      # already use, so the curve now agrees with both (Shafer).
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

      # The states, not just the time they imply.  A drug that also receives
      # an active metabolite has to add this drug's amplitudes to the formed
      # contribution's before solving; see R/recoveryStates.R.  Built whether
      # or not recovery is plotted, because the effect site is read off it.
      effectStates <- recoveryStateSet(
        timeLine,
        list(e_state_l1, e_state_l2, e_state_l3, e_state_ke0),
        c(lambda_1, lambda_2, lambda_3, ke0)
      )

      # A drug with no effect site (ke0 = 0, tPeak = 0) has no Ce at all: NA,
      # which the plot drops, so the drug renders as plasma only.  The
      # e_coef_* are all zero in that case, so the sum would read as an
      # effect site sitting at zero, which is a different thing (the
      # metabolite drugs depend on the NA).
      Ce <- if (ke0 > 0) rowSums(effectStates$state) else rep(NA_real_, L)

      # Time until threshold is timed on the effect site, or on the plasma
      # for a drug that has none -- the antibiotics, whose threshold is the
      # MIC.  See "Which concentration is timed" in R/recoveryStates.R.
      recoveryStates <- if (ke0 > 0) effectStates else recoveryStateSet(
        timeLine,
        list(p_state_l1, p_state_l2, p_state_l3),
        c(lambda_1, lambda_2, lambda_3),
        horizon = recoveryHorizonPlasma(maximum)
      )

      recovery <- if (plotRecovery) recoveryFromStates(recoveryStates, emerge) else rep(0, L)
      results <- data.frame(
        Time = timeLine,
        Cp = Cp,
        Ce = Ce,
        Recovery = recovery
      )
      attr(results, "recoveryStates") <- if (plotRecovery) recoveryStates else NULL
      return(results)
    }
  )
  return(results)
}
