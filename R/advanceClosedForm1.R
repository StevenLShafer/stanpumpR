# Closed form, multiple PK sets
#
# EXTRAVASCULAR DOSES (oral, intramuscular, intranasal)
# =====================================================
# Until 2026-10-07 this engine had no extravascular route at all.  A dose was
# either a bolus or, failing that, an infusion rate, so "10 mg PO" became a
# 10 mg/min infusion that ran until the next rate change.  No drug in the
# library had both a PK event and an extravascular route, so nothing hit it,
# but any such drug would have been wrong by orders of magnitude.
#
# The other engines carry an absorbed dose as an extra exponential state at ka.
# That does not survive a change of PK set here: convertState() maps the three
# disposition states to compartment amounts and back, and an absorption state
# is a mode of the augmented system (depot -> central), not of the disposition.
# So the depot is carried as what it physically is, an AMOUNT, which a change
# in disposition does not touch:
#
#     G(t) = G(t0) exp(-ka (t - t0)) + F x (dose landing in the depot)
#
# and its output, ka G(t), is an exponentially decaying input to the central
# compartment.  Over a step of length dt with the depot at G at its start, the
# disposition state for eigenvalue lambda_j, whose response to a unit bolus is
# b_j = p_coef_bolus_lj, gains
#
#     b_j ka G (exp(-ka dt) - exp(-lambda_j dt)) / (lambda_j - ka)
#
# (or b_j ka G dt exp(-ka dt) when lambda_j = ka), which is exact.  That
# increment is added to the infusion term, so it is advanced, carried across a
# change in PK and zeroed at the event instant exactly as an infusion is.  The
# plasma is still the sum of the three disposition states: drug in the depot is
# not in the plasma.  Bioavailability is that of the PK set in force when the
# dose lands; ka is that of the set in force over each step; an absorption lag
# moves the dose later, as in advanceClosedFormPO_IM_IN(), with the same
# not-yet-absorbed mask on the time until threshold (pendingDoseTimes()).
#
# Rewritten by Claude Code at the request of Steven L. Shafer, 2026-10-07;
# checked against advanceClosedFormPO_IM_IN() with a single PK set, and across
# a change in PK against simulating the two sides separately, by
# tests/testthat/test-closedForm1-extravascular.R.
advanceClosedForm1 <- function(dose, events, pkSets, maximum, plotRecovery, emerge)
{
  ##############################
  # Begin closed form approach #
  ##############################

  # Older callers, and some tests, build the dose table by hand without the
  # route columns simCpCe() adds.
  routes <- c("PO", "IM", "IN")
  for (r in routes) if (is.null(dose[[r]])) dose[[r]] <- rep(FALSE, nrow(dose))
  extravascular <- dose$PO | dose$IM | dose$IN

  # The PK set in force at time t: the last event at or before it.
  eventAt <- function(t) events$Event[utils::tail(which(events$Time <= t), 1)]
  pkField <- function(set, name) {
    x <- set[[name]]
    if (is.null(x) || is.na(x)) 0 else x
  }

  # Absorption lags move the dose later, using the lag of the PK set in force
  # when the dose was given.
  givenAt <- dose$Time
  for (r in routes)
  {
    for (k in which(dose[[r]]))
      dose$Time[k] <- dose$Time[k] + pkField(pkSets[[eventAt(givenAt[k])]], paste0("tlag_", r))
  }

  # Create timeline
  #
  # An extravascular dose gets the instant before it, as a bolus does: the
  # concentration does not jump, but the time until threshold does, and without
  # that point the plotted series would interpolate straight across the jump.
  # A lagged dose also contributes the instant it was GIVEN, so that the window
  # over which recovery cannot be reported starts where it should.  Neither adds
  # anything to a run with no extravascular dose.
  timeLine <- sort(unique(c(0, dose$Time, events$Time, events$Time - 0.01,
                            dose$Time[dose$Bolus | extravascular] - 0.01,
                            givenAt[givenAt < dose$Time], maximum)))
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

  # Create bolusLine, infusionLine and the amounts landing in each depot
  bolusLine <- infusionLine <- pkLine <- dt <- rate <- rep(0, L)
  depotLine <- matrix(0, L, length(routes), dimnames = list(NULL, routes))
  for (i in 1:L)
  {
    bolusLine[i]    <- sum(dose$Dose[dose$Time == timeLine[i] & dose$Bolus])
    for (r in routes)
      depotLine[i, r] <- sum(dose$Dose[dose$Time == timeLine[i] & dose[[r]]])
    USE <- dose$Time == timeLine[i] & !dose$Bolus & !extravascular
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
  # Absorption, which not every PK set carries
  for (r in routes)
  {
    parameters[[paste0("ka_", r)]] <-
      vapply(pkSets, pkField, numeric(1), name = paste0("ka_", r))
    parameters[[paste0("bioavailability_", r)]] <-
      vapply(pkSets, pkField, numeric(1), name = paste0("bioavailability_", r))
  }

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

  # The depots, and what each delivers to the central compartment over each
  # step; see the header.  Like an infusion, the step INTO point i runs on the
  # PK set in force before it (infusionpkLine), whose state coordinates the
  # increment is expressed in.
  depot <- list()
  for (r in routes)
  {
    if (!any(depotLine[, r] != 0)) next
    kaStep <- parameters[infusionpkLine, paste0("ka_", r)]
    bio    <- parameters[pkLine, paste0("bioavailability_", r)]
    G <- rep(0, L)
    G[1] <- bio[1] * depotLine[1, r]
    for (i in seq_len(L)[-1])
      G[i] <- G[i - 1] * exp(-kaStep[i] * dt[i]) + bio[i] * depotLine[i, r]
    Gstart <- c(0, G[-L])
    for (j in 1:3)
    {
      b   <- parameters[infusionpkLine, paste0("p_coef_bolus_l", j)]
      lam <- parameters[infusionpkLine, paste0("lambda_", j)]
      inc <- b * depotInput(lam, kaStep, Gstart, dt)
      if (j == 1) p_infusion_l1 <- p_infusion_l1 + inc
      if (j == 2) p_infusion_l2 <- p_infusion_l2 + inc
      if (j == 3) p_infusion_l3 <- p_infusion_l3 + inc
    }
    depot[[r]] <- G
  }

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

      # Infusion was processed with prior PK, so infusion is now 0.  So is
      # the depot's input over the same step, which travels with it.
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
  # ke0 is a per-step vector here, because the PK set can change on an event.
  # A drug with no tPeak carries zero throughout, and calculateCe() divides by
  # it; see the same guard in advanceClosedForm0().
  hasCe <- any(ke0 > 0)
  Ce <- if (hasCe) {
    calculateCe(Cp, ke0, dt, L)
  } else {
    rep(NA_real_, L)
  }

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
    # Time until the EFFECT SITE falls to the threshold if delivery stops now,
    # or the PLASMA for a drug with no effect site (ke0 = 0: the antibiotics,
    # whose threshold is the MIC).
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
    #
    # Drug still in a depot keeps being absorbed after "delivery stops" -- it
    # has already been given.  Draining a depot G at ka adds to the plasma
    #     sum_i b_i ka G (exp(-ka t) - exp(-lambda_i t)) / (lambda_i - ka),
    # so each lambda_i amplitude moves by -b_i ka G / (lambda_i - ka) and a new
    # term at ka carries the sum of those moves with the sign reversed (it is
    # zero at t = 0: the depot adds nothing to the plasma until it drains).
    # (Claude Code, 2026-10-07.)
    #
    # The eigenvalues in force change with time here, so the state set carries
    # a lambda per point as well as an amplitude per point; see
    # R/recoveryStates.R, which a metabolite fold then reads.
    lam <- cbind(lambda_1, lambda_2, lambda_3)
    P   <- cbind(p_state_l1, p_state_l2, p_state_l3)
    b   <- cbind(p_coef_bolus_l1, p_coef_bolus_l2, p_coef_bolus_l3)
    for (r in names(depot))
    {
      ka <- parameters[pkLine, paste0("ka_", r)]
      # A sum of exponentials cannot carry lambda_i == ka exactly (the term is
      # then t exp(-ka t)); a relative nudge of 1e-6 moves the answer by far
      # less than anything plotted.
      ka <- ifelse(apply(abs(lam - ka) < 1e-9 * pmax(ka, 1e-12), 1, any), ka * (1 + 1e-6), ka)
      shift <- b * ka * depot[[r]] / (lam - ka)
      shift[!is.finite(shift)] <- 0
      P   <- cbind(P - shift, rowSums(shift))
      lam <- cbind(lam, ka)
      b   <- cbind(b, 0)
    }
    if (hasCe)
    {
      a <- P * ke0 / (ke0 - lam)
      recoveryStates <- recoveryStateSet(timeLine, cbind(a, Ce - rowSums(a)), cbind(lam, ke0),
                                         pendingDoseTimes(givenAt, dose$Time, dose$Dose, timeLine))
    } else {
      recoveryStates <- recoveryStateSet(timeLine, P, lam,
                                         pendingDoseTimes(givenAt, dose$Time, dose$Dose, timeLine))
    }
    recovery <- recoveryFromStates(recoveryStates, emerge)
  } else {
    recoveryStates <- NULL
    recovery <- rep(0, L)
  }

  results <- data.frame(
    Time = timeLine,
    Cp = Cp,
    Ce = Ce,
    Recovery = recovery
  )
  attr(results, "recoveryStates") <- recoveryStates
  return(results)
}


#' Input from a draining depot to one disposition state over one step
#'
#' A depot holding \code{G} at the start of a step of length \code{dt} empties
#' into the central compartment at \code{ka G exp(-ka t)}.  A disposition state
#' with eigenvalue \code{lambda} and unit-bolus response 1 gains, over the step,
#' \code{ka G (exp(-ka dt) - exp(-lambda dt)) / (lambda - ka)}; the caller
#' multiplies by the state's bolus coefficient.  When \code{lambda} equals
#' \code{ka} the limit is \code{ka G dt exp(-ka dt)}.
#'
#' @param lambda,ka,G,dt vectors, one per step
#' @returns the gain per unit bolus coefficient, one per step
#' @keywords internal
depotInput <- function(lambda, ka, G, dt)
{
  d    <- lambda - ka
  same <- abs(d) < 1e-9 * pmax(abs(ka), 1e-12)
  out  <- ka * G * ifelse(same, dt * exp(-ka * dt),
                          (exp(-ka * dt) - exp(-lambda * dt)) / ifelse(same, 1, d))
  out[G == 0 | ka <= 0] <- 0
  out
}
