# The Bayesian update: measured concentrations individualize a drug's
# pharmacokinetics by a maximum a posteriori (MAP) estimate.
#
# What is estimated.  Two numbers per drug: the individual's clearance and
# volume, each as a multiple of the model's (`individual = list(clearance,
# volume)`, getDrugPK()).  This is the original STANPUMP's update (BAYES.C,
# Pierre Maitre and Steven Shafer; the "generic volume, clearance" case, which
# estimated CL and V1 with k12, k21, k13 and k31 held fixed), restated for
# every model in the library:
#
#   - the volume factor multiplies every volume and every intercompartmental
#     clearance, so the distribution rate constants (k12, k21, k13, k31) are
#     unchanged, as in BAYES.C: the individual is the model's patient made
#     larger or smaller, with the same shape of distribution;
#   - the clearance factor multiplies the elimination clearance: cl1, and for
#     a drug with saturable elimination Vmax and any linear clearance beside
#     it (Km, a concentration, is unchanged), so for phenytoin it is the
#     classic Vmax update with Km fixed;
#   - a metabolite's formation rate constant moves with k10 (by
#     clearance/volume), so the share of the parent's elimination that forms
#     the metabolite is the model's: kFormation for a metabolite in closed
#     form, kMetabolic for one formed in a transit chain (enalapril);
#   - a parallel system (warfarin's R-enantiomer beside its S) takes the same
#     two factors, so the enantiomers keep their ratio;
#   - a drug-target capacity held as an amount (linagliptin, vildagliptin)
#     moves with the volume; one held as a concentration does not.
#
# Absorption, bioavailability, lag, ke0 or tPeak, the INR and ACT links and
# every other part of a model are left as the model has them.
# individualizeModel() is the whole of it, applied by getDrugPK() to the
# model's own parameters before anything is derived from them, so every
# engine (closed form, Michaelis-Menten, transit chain, INR) sees the
# individual's drug without knowing it was updated.
#
# The objective.  With eta = log(factor), each eta log-normal about 0 with
# standard deviation omega (the PK variability), and each measurement
# log-normal about the prediction with standard deviation sigma (the residual
# variability), the MAP estimate minimises
#
#     sum(eta^2) / omega^2 + sum((log(measured) - log(predicted))^2) / sigma^2
#
# which is minus twice the log posterior, less a constant: Vozeh 1985's
# function, as BAYES.C's funk() computed it (there with omega^2 and sigma^2
# entered directly).  A variability entered as a coefficient of variation c is
# sqrt(log(1 + c^2)) on the log scale (bayesianSD()).  The search is
# Gauss-Newton with Levenberg-Marquardt damping over the two etas
# (bayesianSearch(): the objective is a sum of squares), from the model
# (eta = 0), within BAYES_FACTOR_LIMIT either way.
#
# The prediction is what the plot draws: processdoseTable() on the drug's
# doses, with the measured times added to its time line (withOutputTimes()),
# read from the plasma series.  So it is the INR for warfarin, a metabolite
# drug's own plus what its dosed parents form, and, with the antiseizure
# interactions on, the drug as they change it.  Drugs with measurements are
# updated one after another, in the dose table's order, each on the updated
# drugs before it.
#
# A target-controlled infusion.  The dose table holds a TCI row as a target,
# and the rates are the controller's, computed from a PK model; the update
# changes the PK model and never the dose table, so the patient received the
# rates the pump computed from the library's model (Shafer, 2026-11-14).  An
# individualized drug, and each candidate the search tries, carries that
# model as `tciModel`, which simCpCe() hands the controller.  Before, the
# controller ran on each candidate's own model, so every prediction was the
# target itself and a measurement could not move the estimate (propofol at an
# effect-site target of 3, measured 5: factors 1.000 and 1.000).  So a level
# below the prediction puts the individual's whole curve since the infusion
# began below the target, by less than the level alone suggests, the update
# being shrunk toward the model.

#' Check the individual's clearance and volume factors
#'
#' @param individual NULL, or a list of positive finite numbers `clearance` and
#'   `volume`
#' @param drug the drug's name, for the error message
#' @returns `individual`, or NULL when both factors are 1 (the model itself)
#' @keywords internal
validateIndividual <- function(individual, drug)
{
  if (is.null(individual)) return(NULL)
  ok <- function(x) is.numeric(x) && length(x) == 1 && is.finite(x) && x > 0
  if (!is.list(individual) || !ok(individual$clearance) || !ok(individual$volume))
    stop("Invalid individual pharmacokinetics for ", drug, ": needs positive ",
         "finite clearance and volume factors.")
  individual <- list(clearance = individual$clearance, volume = individual$volume)
  if (individual$clearance == 1 && individual$volume == 1) return(NULL)
  individual
}

#' A model's parameters scaled to the individual
#'
#' The volume factor multiplies the volumes and the intercompartmental
#' clearances (so the distribution rate constants are the model's), the
#' clearance factor the elimination clearance (Vmax and a linear clearance for
#' a saturable drug).  See the header of R/bayesian.R.
#'
#' @param X a drug model's output (the list a `drugs_<name>.R` function
#'   returns)
#' @param individual a list of the factors `clearance` and `volume`
#' @returns `X`, scaled
#' @keywords internal
individualizeModel <- function(X, individual)
{
  fCL <- individual$clearance
  fV <- individual$volume
  scaleSet <- function(s) {
    for (v in c("v1", "v2", "v3", "cl2", "cl3"))
      if (!is.null(s[[v]])) s[[v]] <- s[[v]] * fV
    if (!is.null(s$cl1)) s$cl1 <- s$cl1 * fCL
    s
  }
  X$PK <- lapply(X$PK, scaleSet)
  if (!is.null(X$parallelSystems))
    X$parallelSystems <- lapply(X$parallelSystems, function(sys) {
      sys$PK <- lapply(sys$PK, scaleSet)
      sys
    })
  mm <- X$michaelisMenten
  if (!is.null(mm)) {
    if (!is.null(mm$vmax)) mm$vmax <- mm$vmax * fCL
    if (!is.null(mm$clLinear)) mm$clLinear <- mm$clLinear * fCL
    # A target capacity held as an amount moves with the volume; one held
    # as a concentration (bmaxCentral, kd) does not.
    for (amount in c("rmaxCentral", "rmaxPeripheral"))
      if (!is.null(mm$targetBinding[[amount]]))
        mm$targetBinding[[amount]] <- mm$targetBinding[[amount]] * fV
    if (!is.null(mm$rapidBinding$amaxPeripheral))
      mm$rapidBinding$amaxPeripheral <- mm$rapidBinding$amaxPeripheral * fV
    X$michaelisMenten <- mm
  }
  if (!is.null(X$metabolite$kFormation))
    X$metabolite$kFormation <- X$metabolite$kFormation * fCL / fV
  # A transit chain's kMetabolic is the share of k10 that forms the
  # metabolite (R/advanceTransitChain.R), so it moves with k10 too.  Left
  # alone, a k10 slowed below it (enalapril, fCL / fV under 0.69) was refused
  # by validateTransitChain() and the update failed; above it, the share
  # formed changed (Carbon's UI audit, 2026-11-14, F2).
  if (!is.null(X$transitChain$kMetabolic))
    X$transitChain$kMetabolic <- X$transitChain$kMetabolic * fCL / fV
  X
}

#' A variability entered as a percent coefficient of variation, as the
#' standard deviation of its logarithm
#'
#' @param percent the coefficient of variation, percent
#' @returns sqrt(log(1 + (percent / 100)^2))
#' @keywords internal
bayesianSD <- function(percent) sqrt(log(1 + (percent / 100)^2))

#' The Bayesian objective: minus twice the log posterior, less a constant
#'
#' @param eta the log factors, clearance and volume
#' @param measured the measured concentrations
#' @param predicted the predictions at the same times, for `eta`
#' @param omega the PK variability, log scale (bayesianSD())
#' @param sigma the residual variability, log scale
#' @returns sum(eta^2) / omega^2 + sum((log(measured) - log(predicted))^2) /
#'   sigma^2; Inf where a prediction is not a positive number
#' @keywords internal
bayesianObjective <- function(eta, measured, predicted, omega, sigma)
{
  if (length(predicted) != length(measured) || any(!is.finite(predicted)) ||
      any(predicted <= 0))
    return(Inf)
  sum(eta^2) / omega^2 + sum((log(measured) - log(predicted))^2) / sigma^2
}

#' The measured concentrations a Bayesian update can use
#'
#' @param observations a data frame of `Drug`, `Time` (minutes) and
#'   `Concentration`, in the drug's plotted units
#' @returns the complete rows (a drug, a finite time of 0 or more, a positive
#'   concentration), in time order, with the columns as numbers
#' @export
bayesianObservations <- function(observations)
{
  empty <- data.frame(Drug = character(0), Time = numeric(0),
                      Concentration = numeric(0), stringsAsFactors = FALSE)
  if (is.null(observations) || !is.data.frame(observations) || nrow(observations) == 0 ||
      !all(c("Drug", "Time", "Concentration") %in% names(observations)))
    return(empty)
  out <- data.frame(
    Drug = as.character(observations$Drug),
    Time = suppressWarnings(as.numeric(as.character(observations$Time))),
    Concentration = suppressWarnings(as.numeric(as.character(observations$Concentration))),
    stringsAsFactors = FALSE
  )
  keep <- !is.na(out$Drug) & nzchar(out$Drug) & is.finite(out$Time) & out$Time >= 0 &
    is.finite(out$Concentration) & out$Concentration > 0
  out <- out[keep, , drop = FALSE]
  out <- out[order(out$Drug, out$Time), , drop = FALSE]
  rownames(out) <- NULL
  out
}

# The dosed drugs whose doses a drug's plotted curve depends on: itself, any
# dosed drug that forms it as a metabolite, and, with the antiseizure
# interactions on, every dosed drug (the interactions run among them).
bayesianRelevantDrugs <- function(drug, drugs, dosed, antiseizureInteractions)
{
  parents <- bayesianParents(drug, drugs, dosed)
  if (!isTRUE(antiseizureInteractions)) return(unique(c(drug, parents)))
  # With the interactions on, the drugs connected to it through them: all
  # dosed drugs made an unrelated propofol TCI hold a lamotrigine fit to a
  # week, and a dose of another drug count as one of its own (Carbon's
  # fourth UI audit, 2026-11-15, H8)
  unique(c(drug, parents, asmConnectedDrugs(c(drug, parents), dosed)))
}

# The dosed drugs joined to `start` through the active antiseizure
# interactions, in either direction (perpetrator or victim)
asmConnectedDrugs <- function(start, dosed)
{
  active <- activeAntiseizureInteractions(dosed)
  if (nrow(active) == 0) return(intersect(start, dosed))
  found <- intersect(start, dosed)
  repeat {
    more <- unique(c(active$victim[active$perpetrator %in% found],
                     active$perpetrator[active$victim %in% found]))
    more <- setdiff(intersect(more, dosed), found)
    if (length(more) == 0) break
    found <- c(found, more)
  }
  found
}

# Those of `candidates` that form `drug` as their active metabolite
bayesianParents <- function(drug, drugs, candidates)
{
  candidates[vapply(candidates, function(d) {
    name <- drugs[[d]]$metaboliteName
    !is.null(name) && identical(name, drug)
  }, logical(1))]
}

# A parent's entry rebuilt to form the individual's metabolite: `factors` is
# the metabolite's estimate (getDrugPK(metaboliteIndividual =), through
# recalculatePK(individual =)).  The parent keeps its own estimate, where the
# update made one, its pump's model and its colour.
bayesianParentEntry <- function(parent, drug, drugs, factors, rebuild)
{
  old <- drugs[[parent]]
  individual <- stats::setNames(list(factors), drug)
  if (!is.null(old$individual)) individual[[parent]] <- old$individual
  entry <- rebuild(parent, individual)
  for (field in c("Color", "tciModel", "bayesian")) entry[[field]] <- old[[field]]
  entry
}

# The latest measurement an update uses (minutes): the end of the plot, and
# never later than the longest plot the app offers
bayesianLatestTime <- function(maximum)
{
  if (is.null(maximum) || !is_valid_number(maximum)) return(BAYES_MAX_TIME)
  min(maximum, BAYES_MAX_TIME)
}

# Where a measurement set aside by bayesianLatestTime() is, in words
bayesianLateText <- function(maximum)
{
  if (bayesianLatestTime(maximum) < BAYES_MAX_TIME)
    paste0("after the end of the plot (", formatMinutes(maximum), ")")
  else
    paste0("after ", formatMinutes(BAYES_MAX_TIME), ", the longest plot,")
}

# Why a drug's doses cannot be simulated to `horizon` (minutes), or NULL: the
# plot's own limits (timeUnitViolation() and scheduleProblem() in
# app_server()), which the app's plot keeps but a caller without one need not
bayesianHorizonProblem <- function(drug, DT, horizon)
{
  acute <- DT$Units %in% tciUnits | isGasDrug(DT$Drug)
  if (any(acute) && horizon - 1 > ACUTE_MAX_PLOT_MINUTES)
    return(sprintf(paste(
      "%s could not be updated: target-controlled infusions and inhaled agents",
      "are simulated only to %s, and its last measurement is later."),
      drug, formatMinutes(ACUTE_MAX_PLOT_MINUTES)))
  n <- scheduledDoseCount(DT, horizon)
  if (n > MAX_SCHEDULED_DOSES)
    return(sprintf(paste(
      "%s could not be updated: its scheduled doses repeat %s times to its last",
      "measurement, more than the %s the app can simulate."),
      drug, format(n, big.mark = ","), format(MAX_SCHEDULED_DOSES, big.mark = ",")))
  NULL
}

#' Minimise a sum of squares within bounds
#'
#' Gauss-Newton with Levenberg-Marquardt damping (the objective of the
#' Bayesian update is a sum of squares: the etas over omega, the log errors
#' over sigma), the Jacobian by forward differences.  Each evaluation of
#' `residuals` is a simulation of the drug, and this search makes about three
#' per iteration, where stats::optim()'s L-BFGS-B, with its finite-difference
#' gradients and line searches, made 57 to 102 per fit: a phenytoin level a
#' week after the dose took 20 s, one at ten weeks about three minutes, each
#' holding the worker every session on it shares (2026-11-14).
#'
#' @param residuals function(par) giving the residuals, or NULL where they
#'   cannot be computed (a step there is refused)
#' @param start,lower,upper the starting point and the bounds
#' @param maxIterations the most iterations
#' @param delta the relative step of the finite differences
#' @returns a list: `par`, `value` (the sum of squares there) and
#'   `converged` (no step improves it, or the last one changed it by less than
#'   a relative 1e-10)
#' @keywords internal
bayesianSearch <- function(residuals, start, lower, upper,
                           maxIterations = BAYES_MAX_ITERATIONS, delta = BAYES_JACOBIAN_STEP)
{
  par <- pmin(pmax(start, lower), upper)
  r <- residuals(par)
  if (is.null(r)) return(list(par = par, value = Inf, converged = FALSE))
  value <- sum(r^2)
  n <- length(par)
  lambda <- 1e-3
  nu <- 2
  # The part of the Hessian Gauss-Newton leaves out, sum(r_i * Hessian(r_i)),
  # by the secant update of Dennis, Gay and Welsch (NL2SOL, 1981) from the
  # Jacobians already computed.  Where the residuals stay large at the
  # minimum (a measurement the prior pulls away from) Gauss-Newton alone
  # crawled along the posterior's ridge: a phenytoin fit spent 70 of its 94
  # simulations within 0.01 of the minimum.
  S <- matrix(0, n, n)
  previous <- NULL
  for (iteration in seq_len(maxIterations)) {
    # The Jacobian, each column by a forward difference (inward at a bound)
    J <- matrix(0, length(r), n)
    for (j in seq_len(n)) {
      h <- delta * max(1, abs(par[j]))
      if (par[j] + h > upper[j]) h <- -h
      p <- par
      p[j] <- p[j] + h
      rj <- residuals(p)
      if (is.null(rj)) return(list(par = par, value = value, converged = FALSE))
      J[, j] <- (rj - r) / h
    }
    g <- drop(crossprod(J, r))
    if (!is.null(previous)) {
      s <- par - previous$par
      y <- g - drop(crossprod(previous$J, previous$r))
      ySharp <- g - drop(crossprod(previous$J, r))
      ys <- sum(y * s)
      if (ys > 0) {
        sSs <- drop(crossprod(s, S %*% s))
        if (sSs != 0) S <- S * min(1, abs(sum(s * ySharp)) / abs(sSs))
        w <- ySharp - drop(S %*% s)
        S <- S + (outer(w, y) + outer(y, w)) / ys - sum(w * s) * outer(y, y) / ys^2
      }
    }
    A <- crossprod(J)
    M <- A + S
    # A parameter at a bound that the gradient pushes outward is held
    # there, and the step is solved for the others: solved together and then
    # cut at the bound, the step mixed in the held one's pull, and a fit held
    # at the clearance bound crept toward its volume in steps of 0.05.
    free <- !((par <= lower & g > 0) | (par >= upper & g < 0))
    if (!any(free)) return(list(par = par, value = value, converged = TRUE))
    D <- diag(diag(A)[free] + 1e-12, sum(free))
    # The damped step, damped further until it improves (Nielsen's rule,
    # 1999: the damping follows how well the model predicted the decrease,
    # where halving and doubling it at each try rejected every other step); a
    # step too small to move the estimate is the minimum (or the bound)
    repeat {
      step <- numeric(n)
      solved <- tryCatch({
        Mf <- M[free, free, drop = FALSE] + lambda * D
        chol(Mf)    # positive definite, or more damping
        -solve(Mf, g[free])
      }, error = function(e) NULL)
      if (is.null(solved)) {
        lambda <- lambda * nu
        nu <- 2 * nu
        if (lambda > 1e16) return(list(par = par, value = value, converged = TRUE))
        next
      }
      step[free] <- solved
      trial <- pmin(pmax(par + step, lower), upper)
      moved <- trial - par
      if (max(abs(moved)) < 1e-9)
        return(list(par = par, value = value, converged = TRUE))
      rt <- residuals(trial)
      valueT <- if (is.null(rt)) Inf else sum(rt^2)
      predicted <- -(2 * sum(g * moved) + drop(crossprod(moved, M %*% moved)))
      if (valueT < value) break
      lambda <- lambda * nu
      nu <- 2 * nu
    }
    change <- value - valueT
    gain <- if (predicted > 0) change / predicted else 0
    lambda <- max(lambda * max(1 / 3, 1 - (2 * gain - 1)^3), 1e-12)
    nu <- 2
    previous <- list(par = par, J = J, r = r)
    par <- trial
    r <- rt
    value <- valueT
    if (change <= 1e-10 * max(value, 1e-10))
      return(list(par = par, value = value, converged = TRUE))
  }
  list(par = par, value = value, converged = FALSE)
}

#' Fit one drug's clearance and volume to its measured concentrations
#'
#' @param drug the drug
#' @param drugs the drug list as recalculatePK() built it (no simulation
#'   attached), with any drug already updated
#' @param DT the dose table, times in minutes (doseTableClean())
#' @param ET the event table
#' @param measured this drug's rows of bayesianObservations()
#' @param omega,sigma the PK and residual variabilities, log scale
#' @param rebuild function(drug, individual) giving the drug's entry for the
#'   individual's clearance and volume factors
#' @param antiseizureInteractions are the antiseizure interactions on?
#' @returns a list: `clearance`, `volume` (the factors), `eta`, `objective`
#'   and `priorObjective`, `converged`, `atLimit`, and `observations`, a data
#'   frame of Time, Measured, Population and Individual (the predictions)
#' @keywords internal
bayesianFitDrug <- function(drug, drugs, DT, ET, measured, omega, sigma, rebuild,
                            antiseizureInteractions = FALSE, deadline = Inf,
                            initial = drugs)
{
  dosed <- intersect(unique(DT$Drug), names(drugs))
  relevant <- bayesianRelevantDrugs(drug, drugs, dosed, antiseizureInteractions)
  DTsub <- DT[DT$Drug %in% relevant, , drop = FALSE]
  # The entries the simulation needs: the relevant drugs, and the metabolite
  # entries they fold into (no companions).
  entries <- drugs[vapply(drugs, function(e) is.null(e$companionOf), logical(1))]
  times <- measured$Time
  maximum <- max(times) + 1
  simCache <- NULL

  # A metabolite's level is also what its dosed parents form, and they form
  # the individual's metabolite (bayesianParentEntry()): rebuilt from the
  # model's, every candidate's parents formed the same metabolite, and a
  # level of morphine formed from codeine left the estimate where it began
  # (Carbon's third UI audit, 2026-11-15, H7)
  parents <- bayesianParents(drug, drugs, setdiff(relevant, drug))
  predict <- function(entry, parentEntries = list(), base = entries, keep = TRUE) {
    # No simulation is started after the deadline (BAYES_MAX_SECONDS)
    if (proc.time()[["elapsed"]] > deadline)
      stop(structure(class = c("bayesTimeLimit", "error", "condition"),
                     list(message = "time limit", call = NULL)))
    candidate <- base
    candidate[[drug]] <- entry
    for (p in names(parentEntries)) candidate[[p]] <- parentEntries[[p]]
    out <- withOutputTimes(times, processdoseTable(
      DTsub, ET, candidate, maximum, FALSE, cache = if (keep) simCache,
      antiseizureInteractions = antiseizureInteractions))
    if (keep) simCache <<- out
    wide <- out[[drug]]$wide
    if (is.null(wide) || nrow(wide) == 0) return(rep(NA_real_, length(times)))
    at <- match(times, wide$Time)
    values <- wide$Plasma[at]
    missing <- is.na(at)
    if (any(missing))
      values[missing] <- stats::approx(wide$Time, wide$Plasma, times[missing])$y
    values
  }
  usesPump <- any(isTciUnit(DTsub$Units[DTsub$Drug == drug]))
  entryFor <- function(eta) {
    factors <- list(clearance = exp(eta[1]), volume = exp(eta[2]))
    entry <- rebuild(drug, stats::setNames(list(factors), drug))
    # The pump ran on the library's model (see the header)
    if (usesPump) entry$tciModel <- drugs[[drug]]
    entry
  }
  parentsFor <- function(eta) {
    factors <- list(clearance = exp(eta[1]), volume = exp(eta[2]))
    stats::setNames(lapply(parents, bayesianParentEntry, drug = drug, drugs = drugs,
                           factors = factors, rebuild = rebuild), parents)
  }

  population <- predict(drugs[[drug]])
  # A level the model predicts as no drug at all (before absorption begins:
  # a lag is neither clearance nor volume, so no estimate can move it) can
  # be explained by no estimate, and is set aside; until it was, it made
  # every candidate unreadable and the levels after it went unused (Carbon's
  # second UI audit, 2026-11-15, F8)
  use <- is.finite(population) & population > 0
  if (!any(use))
    stop("the model predicts no concentration at the measured times.", call. = FALSE)
  # Each prediction simulated once: the model's own (eta = 0) is the
  # population's, and the estimate's is one the search has already made
  tried <- list(list(eta = c(0, 0), prediction = population))
  predictAt <- function(eta) {
    for (t in tried) if (identical(t$eta, eta)) return(t$prediction)
    prediction <- predict(entryFor(eta), parentsFor(eta))
    tried[[length(tried) + 1]] <<- list(eta = eta, prediction = prediction)
    prediction
  }
  # The objective is the sum of the squares of these (bayesianObjective());
  # NULL where a prediction is not a positive number
  residuals <- function(eta) {
    prediction <- predictAt(eta)
    if (length(prediction) != length(times)) return(NULL)
    prediction <- prediction[use]
    if (any(!is.finite(prediction)) || any(prediction <= 0)) return(NULL)
    c(eta / omega, (log(measured$Concentration[use]) - log(prediction)) / sigma)
  }
  limit <- log(BAYES_FACTOR_LIMIT)
  fit <- bayesianSearch(residuals, c(0, 0), c(-limit, -limit), c(limit, limit))
  eta <- fit$par
  individual <- predictAt(eta)[use]
  # The model's own prediction, as its population line draws it: every drug
  # as it was before the update.  Read from the drugs as updated so far, a
  # metabolite's model prediction carried its parent's estimate, 84% off
  # (Carbon's fourth UI audit, 2026-11-15, H7).
  initialEntries <- initial[vapply(initial, function(e) is.null(e$companionOf), logical(1))]
  modelOwn <- if (identical(initialEntries, entries)) population else
    predict(initial[[drug]], base = initialEntries, keep = FALSE)
  list(
    clearance = exp(eta[1]),
    volume = exp(eta[2]),
    eta = eta,
    objective = fit$value,
    priorObjective = bayesianObjective(c(0, 0), measured$Concentration[use], population[use],
                                       omega, sigma),
    converged = fit$converged,
    evaluations = length(tried),
    atLimit = any(abs(eta) > limit * (1 - 1e-6)),
    beforeAbsorption = sum(!use),
    observations = data.frame(Time = times[use], Measured = measured$Concentration[use],
                              Population = modelOwn[use], Individual = individual)
  )
}

#' Update the drugs' pharmacokinetics from measured concentrations
#'
#' The maximum a posteriori estimate of each measured drug's clearance and
#' volume (see the header of R/bayesian.R): each drug with usable
#' measurements is rebuilt as the individual's drug, and the model's own is
#' kept beside it as a companion entry, "<drug> (population)", which the plot
#' draws as a second line in the drug's panel.
#'
#' @param drugs the drug list as recalculatePK() built it, before simulation
#' @param DT the dose table, times in minutes
#' @param ET the event table
#' @param observations measured concentrations: `Drug`, `Time` (minutes) and
#'   `Concentration` in the drug's plotted units (bayesianObservations())
#' @param pkVariability the PK variability, percent (coefficient of variation
#'   of clearance and volume about the model's)
#' @param residualVariability the residual variability, percent: the spread
#'   of a measurement about the individual's prediction (assay, timing and
#'   model error together)
#' @param rebuild function(drug, individual) returning the drug's entry, as
#'   recalculatePK() builds it, for `individual`, the estimates by drug (a
#'   named list of lists of `clearance` and `volume`, the form
#'   recalculatePK(individual =) takes, which is the usual rebuild): the
#'   drug's own, and, for a drug that forms a measured metabolite, the
#'   metabolite's, which it then forms
#' @param antiseizureInteractions are the antiseizure interactions on?
#' @param fits the `fits` of an earlier call, whose estimates are reused for
#'   any drug whose inputs have not changed
#' @param onFit NULL, or function(drug) called before a drug is fitted afresh
#'   (not when its estimate is reused): the app shows its spinner then
#' @param maximum NULL, or the length of the plot (minutes): a measurement
#'   after it is set aside.  Whatever it is, a measurement later than the
#'   longest plot the app offers (`BAYES_MAX_TIME`) is set aside too.
#' @param timeLimit the longest the update searches, seconds, all drugs
#'   together (`BAYES_MAX_SECONDS`): no simulation is started after it, and a
#'   drug not finished by then is not updated
#' @returns a list: `drugs`, updated; `fits`, one per updated drug (with its
#'   `key`); `problems`, sentences saying what could not be used; and
#'   `failed`, the drugs that could not be updated, with their keys, which a
#'   caller passes back in `fits` so that they are not tried again until
#'   their inputs change; and `cache`, every fit made or reused, with its
#'   key, which a caller passes back in `fits` to reuse them all (with the
#'   antiseizure interactions on, the update repeats its round until no
#'   estimate moves, and a round's fits are keyed by the other drugs'
#'   estimates)
#' @export
bayesianUpdateDrugs <- function(drugs, DT, ET, observations,
                                pkVariability = BAYES_PK_VARIABILITY_DEFAULT,
                                residualVariability = BAYES_RESIDUAL_VARIABILITY_DEFAULT,
                                rebuild, antiseizureInteractions = FALSE, fits = NULL,
                                onFit = NULL, maximum = NULL,
                                timeLimit = BAYES_MAX_SECONDS)
{
  observations <- bayesianObservations(observations)
  problems <- character(0)
  newFits <- list()
  failed <- list()
  deadline <- proc.time()[["elapsed"]] + timeLimit
  if (nrow(observations) == 0 || is.null(DT) || nrow(DT) == 0)
    return(list(drugs = drugs, fits = newFits, problems = problems, failed = failed,
                cache = list()))
  omega <- bayesianSD(pkVariability)
  sigma <- bayesianSD(residualVariability)
  ET <- if (is.null(ET)) data.frame(Time = numeric(0), Event = character(0)) else ET
  rownames(DT) <- NULL

  dosed <- unique(DT$Drug)
  measuredDrugs <- unique(observations$Drug)
  # In the dose table's order, so that a drug is updated after the drugs
  # before it, except that a parent is updated before the metabolite it
  # forms, which it then forms as the individual's (bayesianParentEntry()):
  # updated after, its own rebuild dropped the metabolite's estimate and the
  # plotted metabolite went back to the model's (Carbon's fourth UI audit,
  # 2026-11-15, H7)
  order <- c(intersect(dosed, measuredDrugs), setdiff(measuredDrugs, dosed))
  isParent <- vapply(order, function(d) {
    m <- drugs[[d]]$metaboliteName
    !is.null(m) && m %in% order
  }, logical(1))
  order <- c(order[isParent], order[!isParent])
  # The drugs as they were: each population line, and each fit's model
  # prediction, is theirs (the same audit, H7)
  initial <- drugs
  priors <- list()
  # Every fit made, with its key, so that one whose inputs have not changed
  # is reused, in a later pass and at a later redraw (`fits`)
  cache <- list()
  lookup <- function(key) {
    for (f in cache) if (is.list(f) && identical(f$key, key)) return(f)
    for (f in fits) if (is.list(f) && identical(f$key, key)) {
      cache[[length(cache) + 1]] <<- f
      return(f)
    }
    NULL
  }
  # With the antiseizure interactions on, measured drugs move one another's
  # curves, so they are updated in turn until no estimate moves: updated
  # once each, the first was fitted against the second's model and drawn
  # against its estimate, 21% from its own fit (the same audit, H8)
  passes <- if (isTRUE(antiseizureInteractions)) BAYES_JOINT_PASSES else 1L
  previous <- NULL
  for (pass in seq_len(passes)) {
  problems <- character(0)
  newFits <- list()
  failed <- list()
  for (drug in order)
  {
    rows <- observations[observations$Drug == drug, , drop = FALSE]
    entry <- initial[[drug]]
    if (!drug %in% dosed || is.null(entry) || !is.null(entry$companionOf)) {
      problems <- c(problems, sprintf(
        "%s is not in the dose table, so its %s cannot be used.",
        drug, ngettext(nrow(rows), "measurement", "measurements")))
      next
    }
    relevant <- bayesianRelevantDrugs(drug, drugs, intersect(dosed, names(drugs)),
                                      antiseizureInteractions)
    parents <- bayesianParents(drug, drugs, setdiff(relevant, drug))
    # A level measured before any dose that reaches it cannot be explained by
    # the drug, and is set aside.  A dose of 0 is no dose: the startup menu's
    # rows are 0 until a dose is typed, and a level entered before then was
    # fitted to a curve of zeros (Carbon's second UI audit, 2026-11-15, F8).
    # Only the drug's own doses and its parents': with the interactions on,
    # another drug's dose counted (the same audit, H8)
    given <- DT$Drug %in% c(drug, parents) & suppressWarnings(as.numeric(DT$Dose)) > 0
    if (!any(given, na.rm = TRUE)) {
      problems <- c(problems, sprintf(
        "%s has no dose above 0 yet, so its %s cannot be used.",
        drug, ngettext(nrow(rows), "measurement", "measurements")))
      next
    }
    firstDose <- min(DT$Time[which(given)])
    early <- rows$Time <= firstDose
    if (any(early)) {
      problems <- c(problems, sprintf(
        "%s: %d %s at or before the first dose %s not used.", drug, sum(early),
        ngettext(sum(early), "measurement", "measurements"),
        ngettext(sum(early), "is", "are")))
      rows <- rows[!early, , drop = FALSE]
    }
    # Nor is a level after the end of the plot, which the plot cannot show.
    # The fit simulates the drug to its last level, and the limits that keep
    # a simulation within what a worker can do (a week for a TCI target or an
    # inhaled agent, MAX_SCHEDULED_DOSES for a schedule's repeats) are held
    # against the plot's length: a level typed at 1e12 minutes held the
    # worker, and every session on it, in tciSchedule() (Carbon's UI audit,
    # 2026-11-14, F1).
    late <- rows$Time > bayesianLatestTime(maximum)
    if (any(late)) {
      problems <- c(problems, sprintf(
        "%s: %d %s %s %s not used.", drug, sum(late),
        ngettext(sum(late), "measurement", "measurements"), bayesianLateText(maximum),
        ngettext(sum(late), "is", "are")))
      rows <- rows[!late, , drop = FALSE]
    }
    if (nrow(rows) == 0) next

    DTsub <- DT[DT$Drug %in% relevant, , drop = FALSE]
    # And the plot's own limits, for a caller with no plot (the API): the
    # app's plot keeps them already (timeUnitViolation(), scheduleProblem()).
    tooLong <- bayesianHorizonProblem(drug, DTsub, max(rows$Time) + 1)
    if (!is.null(tooLong)) {
      problems <- c(problems, tooLong)
      next
    }
    stripSim <- function(e) e[setdiff(names(e), c("sim", "simKey"))]
    # The models as they were, and the estimates of the other drugs, which
    # is what the drugs the fit simulates are built from
    key <- list(
      drug = drug,
      entries = lapply(initial[intersect(names(initial), unique(c(relevant, vapply(
        relevant, function(d) { m <- initial[[d]]$metaboliteName; if (is.null(m)) "" else m },
        character(1)))))], stripSim),
      estimates = lapply(stats::setNames(setdiff(relevant, drug), setdiff(relevant, drug)),
                         function(d) drugs[[d]]$individual),
      DT = DTsub,
      ET = ET,
      measured = rows[, c("Time", "Concentration")],
      omega = omega,
      sigma = sigma,
      antiseizureInteractions = isTRUE(antiseizureInteractions)
    )
    fit <- lookup(key)
    if (is.null(fit)) {
      if (is.function(onFit)) onFit(drug)
      # Searched from the model: in a later pass the drug, and its parents'
      # metabolite, carry the last pass's estimate, from which the search
      # found nothing left to fit and reported a factor of 1
      fitDrugs <- drugs
      fitDrugs[[drug]] <- initial[[drug]]
      for (p in parents) if (!is.null(drugs[[p]]$metaboliteIndividual))
        fitDrugs[[p]] <- bayesianParentEntry(p, drug, drugs, list(clearance = 1, volume = 1), rebuild)
      fit <- tryCatch(
        bayesianFitDrug(drug, fitDrugs, DT, ET, rows, omega, sigma, rebuild,
                        antiseizureInteractions, deadline = deadline, initial = initial),
        bayesTimeLimit = function(e) list(failed = sprintf(paste(
          "%s could not be updated within %s seconds: shorten the plot, or enter",
          "measurements of fewer drugs."), drug, format(timeLimit))),
        error = function(e) list(failed = sprintf("%s could not be updated: %s", drug,
                                                  conditionMessage(e))))
      fit$key <- key
      cache[[length(cache) + 1]] <- fit
    }
    # A fit that failed is not tried again until its inputs change: one that
    # ran out of time would otherwise take its time again at every redraw
    if (!is.null(fit$failed)) {
      problems <- c(problems, fit$failed)
      failed[[drug]] <- fit
      next
    }
    if (isTRUE(fit$beforeAbsorption > 0))
      problems <- c(problems, sprintf(
        "%s: %d %s before the drug reaches the plasma (the model predicts none) %s not used.",
        drug, fit$beforeAbsorption, ngettext(fit$beforeAbsorption, "measurement", "measurements"),
        ngettext(fit$beforeAbsorption, "is", "are")))
    if (anyNA(fit$observations$Population) || anyNA(fit$observations$Individual)) {
      problems <- c(problems, sprintf(
        "%s could not be updated: its plotted concentration could not be read at the measured times.",
        drug))
      next
    }
    if (!isTRUE(fit$converged))
      problems <- c(problems, sprintf(
        "%s: the search for the best estimate did not converge; the estimate shown is the best found.",
        drug))
    if (isTRUE(fit$atLimit))
      problems <- c(problems, sprintf(
        paste("%s: the estimate reached the limit of the search (%s times the model's clearance",
              "or volume). Check the measured values and their units."),
        drug, format(BAYES_FACTOR_LIMIT)))
    newFits[[drug]] <- fit

    priors[[drug]] <- entry
    factors <- list(clearance = fit$clearance, volume = fit$volume)
    # The model's own line includes what the parents form as the model has
    # it, as the fit's population prediction did (foldMetabolites())
    priors[[drug]]$populationSources <- initial[parents]
    individual <- stats::setNames(list(factors), drug)
    # A parent keeps forming the individual's metabolite, where that was
    # updated in an earlier pass
    formed <- entry$metaboliteName
    if (!is.null(formed) && !is.null(drugs[[formed]]$bayesian))
      individual[[formed]] <- drugs[[formed]]$bayesian[c("clearance", "volume")]
    updated <- rebuild(drug, individual)
    # The pump ran on the library's model (see the header)
    if (any(isTciUnit(DTsub$Units[DTsub$Drug == drug]))) updated$tciModel <- entry
    updated$Color <- entry$Color
    updated$bayesian <- list(clearance = fit$clearance, volume = fit$volume,
                             n = nrow(rows))
    drugs[[drug]] <- updated
    # Its line without the antiseizure interactions shows what they do to
    # the drug as drawn, so it is the individual's too (Carbon's third UI
    # audit, 2026-11-15, H7)
    reference <- asmReferenceName(drug)
    if (!is.null(drugs[[reference]])) {
      kept <- drugs[[reference]][c("drug", "companionOf", "modelLabel", "companionGroup",
                                   "helpPage", "Color")]
      drugs[[reference]] <- utils::modifyList(updated, kept)
    }
    # And its parents form the individual's metabolite
    for (p in parents)
      drugs[[p]] <- bayesianParentEntry(p, drug, drugs, factors, rebuild)
  }

  # Until no estimate moves
  estimates <- lapply(newFits, function(f) c(f$clearance, f$volume))
  if (!is.null(previous) && identical(names(estimates), names(previous)) &&
      all(vapply(names(estimates), function(d)
        max(abs(estimates[[d]] / previous[[d]] - 1)) < BAYES_JOINT_TOLERANCE, logical(1))))
    break
  previous <- estimates
  }

  # The model's own prediction beside each updated drug, after every drug
  # (a panel's first series is the drug's own: R/alternativeModels.R)
  for (drug in names(priors))
  {
    prior <- priors[[drug]]
    name <- bayesianPriorName(drug)
    prior$drug <- name
    prior$companionOf <- drug
    prior$modelLabel <- BAYES_PRIOR_LABEL
    prior$companionGroup <- "bayesian"
    prior$helpPage <- BAYES_HELP_PAGE
    # Its own shade, which the other companions' (Minto and Schnider, the
    # antiseizure interactions' "no interactions") do not take: all three
    # took alternativeModelColor()'s, and a drug with two drew them in one
    # colour (Carbon's third UI audit, 2026-11-15, H7)
    prior$Color <- alternativeModelColor(prior$Color, amount = BAYES_PRIOR_COLOR_AMOUNT)
    drugs[[name]] <- prior
  }
  list(drugs = drugs, fits = newFits, problems = unique(problems), failed = failed,
       cache = cache)
}

# The help page of the Bayesian update
BAYES_HELP_PAGE <- "models/bayesian"

#' The name of a drug's population companion: "warfarin (population)"
#' @noRd
bayesianPriorName <- function(drug) paste0(drug, " (", BAYES_PRIOR_LABEL, ")")

#' A fitted drug's estimate in words, for the sidebar
#'
#' @param drug the drug
#' @param fit its fit (bayesianUpdateDrugs()$fits)
#' @returns a character vector: the clearance and volume as multiples of the
#'   model's
#' @keywords internal
bayesianFitSummary <- function(drug, fit)
{
  pct <- function(f) {
    change <- round(100 * (f - 1))
    if (change == 0) "as the model" else sprintf("%+d%%", change)
  }
  sprintf("%s: clearance x %s (%s), volume x %s (%s); %d %s.",
          drug, formatC(fit$clearance, digits = 3, format = "fg"), pct(fit$clearance),
          formatC(fit$volume, digits = 3, format = "fg"), pct(fit$volume),
          nrow(fit$observations),
          ngettext(nrow(fit$observations), "measurement", "measurements"))
}
