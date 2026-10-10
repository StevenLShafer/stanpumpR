# Closed Form, one PK set only, oral, intranasal, IM and regional anesthesia
# (RA, tissue injection) doses included alongside IV doses
advanceClosedFormPO_IM_IN <- function(dose, pkSet, maximum, plotRecovery, emerge)
{
  ##############################################
  # Begin closed form approach, time invariant #
  # About 2.5 times faster than closed form    #
  # time variant model (ClosedForm1)           #
  # Modified to include PO and IV delivery     #
  ##############################################

  # Add tlag_ to PO, IM, IN and RA dose times.  A hand-built dose table may
  # carry no RA column.
  if (is.null(dose$RA)) dose$RA <- rep(FALSE, nrow(dose))
  if (is.null(dose$RAslow)) dose$RAslow <- rep(FALSE, nrow(dose))
  if (is.null(dose$PO2)) dose$PO2 <- rep(FALSE, nrow(dose))
  givenAt <- dose$Time
  dose$Time[dose$PO] <- dose$Time[dose$PO] + pkSet$tlag_PO
  dose$Time[dose$IM] <- dose$Time[dose$IM] + pkSet$tlag_IM
  dose$Time[dose$IN] <- dose$Time[dose$IN] + pkSet$tlag_IN
  dose$Time[dose$RA] <- dose$Time[dose$RA] + pkSet$tlag_RA
  dose$Time[dose$RAslow] <- dose$Time[dose$RAslow] + pkSet$tlag_RAslow
  # The second oral depot (see getDrugPK()); a PK set built before it existed
  # carries no tlag_PO2, and then no PO2 rows either.
  if (any(dose$PO2)) dose$Time[dose$PO2] <- dose$Time[dose$PO2] + pkSet$tlag_PO2

  # Create timeline
  #
  # A lagged dose contributes the instant it was GIVEN as well, which is not
  # otherwise a point on the line, so that the window over which recovery
  # cannot be reported starts exactly where it should.  Only when some dose is
  # actually lagged, so that an unlagged run keeps the line it always had.
  #
  # The fill between those points is scaled to the plot; see
  # R/simulationTimeGrid.R.
  timeLine <- simulationTimeGrid(
    c(
      dose$Time,
      dose$Time - PRE_DOSE_OFFSET, # run until just before next dose
      givenAt[givenAt < dose$Time]
    ),
    maximum,
    gridStart(pkSet$ke0)
  )
  L <- length(timeLine)
  doseNA <- rep(0, L)

  # Create bolusLine and infusionLine
  inputs    <- doseLines(dose, timeLine, c("PO", "IM", "IN", "RA", "RAslow", "PO2"))
  bolusLine <- inputs$bolus
  poLine    <- inputs$PO
  imLine    <- inputs$IM
  inLine    <- inputs$IN
  raLine    <- inputs$RA
  raSlowLine <- inputs$RAslow
  po2Line   <- inputs$PO2
  rate      <- inputs$rate
  dt        <- inputs$dt

  # A PK set from before the second oral depot existed has none of its fields.
  if (is.null(pkSet$ka_PO2))
  {
    pkSet$ka_PO2 <- 0
    for (x in c("p_coef_PO2_l1", "p_coef_PO2_l2", "p_coef_PO2_l3", "p_coef_PO2_ka",
                "e_coef_PO2_l1", "e_coef_PO2_l2", "e_coef_PO2_l3", "e_coef_PO2_ke0",
                "e_coef_PO2_ka"))
      pkSet[[x]] <- 0
  }

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
      ka_RA_dt <- exp(-ka_RA * dt)
      ka_RAslow_dt <- exp(-ka_RAslow * dt)
      ka_PO2_dt <- exp(-ka_PO2 * dt)

      p_bolus_l1 <- p_coef_bolus_l1 * bolusLine
      p_bolus_l2 <- p_coef_bolus_l2 * bolusLine
      p_bolus_l3 <- p_coef_bolus_l3 * bolusLine

      p_infusion_l1 <- p_coef_infusion_l1 * rate * (1 - l1_dt)
      p_infusion_l2 <- p_coef_infusion_l2 * rate * (1 - l2_dt)
      p_infusion_l3 <- p_coef_infusion_l3 * rate * (1 - l3_dt)

      # The second oral depot, when the drug has one, feeds the same
      # disposition states as the first, as the slow RA depot does below.
      p_PO_l1 <- p_coef_PO_l1 * poLine + p_coef_PO2_l1 * po2Line
      p_PO_l2 <- p_coef_PO_l2 * poLine + p_coef_PO2_l2 * po2Line
      p_PO_l3 <- p_coef_PO_l3 * poLine + p_coef_PO2_l3 * po2Line
      p_PO_ka <- p_coef_PO_ka * poLine
      p_PO2_ka <- p_coef_PO2_ka * po2Line

      p_IM_l1 <- p_coef_IM_l1 * imLine
      p_IM_l2 <- p_coef_IM_l2 * imLine
      p_IM_l3 <- p_coef_IM_l3 * imLine
      p_IM_ka <- p_coef_IM_ka * imLine

      p_IN_l1 <- p_coef_IN_l1 * inLine
      p_IN_l2 <- p_coef_IN_l2 * inLine
      p_IN_l3 <- p_coef_IN_l3 * inLine
      p_IN_ka <- p_coef_IN_ka * inLine

      # The slow RA depot, when the drug has one, feeds the same disposition
      # states, so its inputs are added to the fast depot's there; it keeps
      # its own absorption state.
      p_RA_l1 <- p_coef_RA_l1 * raLine + p_coef_RAslow_l1 * raSlowLine
      p_RA_l2 <- p_coef_RA_l2 * raLine + p_coef_RAslow_l2 * raSlowLine
      p_RA_l3 <- p_coef_RA_l3 * raLine + p_coef_RAslow_l3 * raSlowLine
      p_RA_ka <- p_coef_RA_ka * raLine
      p_RAslow_ka <- p_coef_RAslow_ka * raSlowLine

      p_state_l1    <- advanceStatePO(l1_dt,    p_bolus_l1, p_infusion_l1, p_PO_l1,    p_IM_l1,    p_IN_l1, L, p_RA_l1)
      p_state_l2    <- advanceStatePO(l2_dt,    p_bolus_l2, p_infusion_l2, p_PO_l2,    p_IM_l2,    p_IN_l2, L, p_RA_l2)
      p_state_l3    <- advanceStatePO(l3_dt,    p_bolus_l3, p_infusion_l3, p_PO_l3,    p_IM_l3,    p_IN_l3, L, p_RA_l3)
      p_state_ka_PO <- advanceStatePO(ka_PO_dt, doseNA,     doseNA,        p_PO_ka,    doseNA,     doseNA,  L)
      p_state_ka_IM <- advanceStatePO(ka_IM_dt, doseNA,     doseNA,        doseNA,     p_IM_ka,    doseNA,  L)
      p_state_ka_IN <- advanceStatePO(ka_IN_dt, doseNA,     doseNA,        doseNA,     doseNA,     p_IN_ka, L)
      p_state_ka_RA <- advanceStatePO(ka_RA_dt, doseNA,     doseNA,        doseNA,     doseNA,     doseNA,  L, p_RA_ka)
      p_state_ka_RAslow <- advanceStatePO(ka_RAslow_dt, doseNA, doseNA,    doseNA,     doseNA,     doseNA,  L, p_RAslow_ka)
      p_state_ka_PO2 <- advanceStatePO(ka_PO2_dt, doseNA,   doseNA,        p_PO2_ka,   doseNA,     doseNA,  L)

      Cp <- p_state_l1 + p_state_l2 + p_state_l3 + p_state_ka_PO + p_state_ka_IM + p_state_ka_IN +
        p_state_ka_RA + p_state_ka_RAslow + p_state_ka_PO2
      # The effect site from its own exponential states, exactly, rather than
      # derived from the plasma curve by calculateCe(); see advanceClosedForm0().
      ke0_dt <- exp(-ke0 * dt)
      e_bolus_l1  <- e_coef_bolus_l1  * bolusLine
      e_bolus_l2  <- e_coef_bolus_l2  * bolusLine
      e_bolus_l3  <- e_coef_bolus_l3  * bolusLine
      e_bolus_ke0 <- e_coef_bolus_ke0 * bolusLine

      e_infusion_l1  <- e_coef_infusion_l1  * rate * (1 - l1_dt)
      e_infusion_l2  <- e_coef_infusion_l2  * rate * (1 - l2_dt)
      e_infusion_l3  <- e_coef_infusion_l3  * rate * (1 - l3_dt)
      e_infusion_ke0 <- e_coef_infusion_ke0 * rate * (1 - ke0_dt)

      e_PO_l1  <- e_coef_PO_l1  * poLine + e_coef_PO2_l1  * po2Line
      e_PO_l2  <- e_coef_PO_l2  * poLine + e_coef_PO2_l2  * po2Line
      e_PO_l3  <- e_coef_PO_l3  * poLine + e_coef_PO2_l3  * po2Line
      e_PO_ke0 <- e_coef_PO_ke0 * poLine + e_coef_PO2_ke0 * po2Line
      e_PO_ka  <- e_coef_PO_ka  * poLine
      e_PO2_ka <- e_coef_PO2_ka * po2Line

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

      e_RA_l1  <- e_coef_RA_l1  * raLine + e_coef_RAslow_l1  * raSlowLine
      e_RA_l2  <- e_coef_RA_l2  * raLine + e_coef_RAslow_l2  * raSlowLine
      e_RA_l3  <- e_coef_RA_l3  * raLine + e_coef_RAslow_l3  * raSlowLine
      e_RA_ke0 <- e_coef_RA_ke0 * raLine + e_coef_RAslow_ke0 * raSlowLine
      e_RA_ka  <- e_coef_RA_ka  * raLine
      e_RAslow_ka <- e_coef_RAslow_ka * raSlowLine

      e_state_l1     <- advanceStatePO(l1_dt,    e_bolus_l1,  e_infusion_l1,  e_PO_l1,  e_IM_l1,  e_IN_l1,  L, e_RA_l1)
      e_state_l2     <- advanceStatePO(l2_dt,    e_bolus_l2,  e_infusion_l2,  e_PO_l2,  e_IM_l2,  e_IN_l2,  L, e_RA_l2)
      e_state_l3     <- advanceStatePO(l3_dt,    e_bolus_l3,  e_infusion_l3,  e_PO_l3,  e_IM_l3,  e_IN_l3,  L, e_RA_l3)
      e_state_ke0    <- advanceStatePO(ke0_dt,   e_bolus_ke0, e_infusion_ke0, e_PO_ke0, e_IM_ke0, e_IN_ke0, L, e_RA_ke0)
      e_state_ka_PO  <- advanceStatePO(ka_PO_dt, doseNA,      doseNA,         e_PO_ka,  doseNA,   doseNA,   L)
      e_state_ka_IM  <- advanceStatePO(ka_IM_dt, doseNA,      doseNA,         doseNA,   e_IM_ka,  doseNA,   L)
      e_state_ka_IN  <- advanceStatePO(ka_IN_dt, doseNA,      doseNA,         doseNA,   doseNA,   e_IN_ka,  L)
      e_state_ka_RA  <- advanceStatePO(ka_RA_dt, doseNA,      doseNA,         doseNA,   doseNA,   doseNA,   L, e_RA_ka)
      e_state_ka_RAslow <- advanceStatePO(ka_RAslow_dt, doseNA,   doseNA,         doseNA,   doseNA,   doseNA,   L, e_RAslow_ka)
      e_state_ka_PO2 <- advanceStatePO(ka_PO2_dt, doseNA,      doseNA,         e_PO2_ka, doseNA,   doseNA,   L)

      # The states, not just the time they imply.  A drug that also receives
      # an active metabolite has to add this drug's amplitudes to the formed
      # contribution's before solving; see R/recoveryStates.R.  Built whether
      # or not recovery is plotted, because the effect site is read off it.
      pending <- pendingDoseTimes(givenAt, dose$Time, dose$Dose, timeLine)
      # The RA depots are carried only by a drug that has them, so that every
      # other drug keeps exactly the states it always had.
      # Likewise the second oral depot.
      raState <- function(x, xSlow, x2) c(if (ka_RA > 0) list(x), if (ka_RAslow > 0) list(xSlow),
                                          if (ka_PO2 > 0) list(x2))
      raRate  <- c(if (ka_RA > 0) ka_RA, if (ka_RAslow > 0) ka_RAslow, if (ka_PO2 > 0) ka_PO2)
      effectStates <- recoveryStateSet(
        timeLine,
        c(list(e_state_l1, e_state_l2, e_state_l3, e_state_ke0,
               e_state_ka_PO, e_state_ka_IM, e_state_ka_IN), raState(e_state_ka_RA, e_state_ka_RAslow, e_state_ka_PO2)),
        c(lambda_1, lambda_2, lambda_3, ke0, ka_PO, ka_IM, ka_IN, raRate),
        pending
      )

      # No effect site (ke0 = 0): Ce is NA, not zero; see advanceClosedForm0().
      Ce <- if (ke0 > 0) rowSums(effectStates$state) else rep(NA_real_, L)

      # Timed on the plasma when there is no effect site, as in
      # advanceClosedForm0().  The absorption states are plasma states too:
      # drug still in the depot is already given, and keeps arriving after
      # delivery stops.
      recoveryStates <- if (ke0 > 0) effectStates else recoveryStateSet(
        timeLine,
        c(list(p_state_l1, p_state_l2, p_state_l3,
               p_state_ka_PO, p_state_ka_IM, p_state_ka_IN), raState(p_state_ka_RA, p_state_ka_RAslow, p_state_ka_PO2)),
        c(lambda_1, lambda_2, lambda_3, ka_PO, ka_IM, ka_IN, raRate),
        pending,
        horizon = recoveryHorizonPlasma(maximum)
      )

      recovery <- if (plotRecovery) recoveryFromStates(recoveryStates, emerge) else doseNA
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
