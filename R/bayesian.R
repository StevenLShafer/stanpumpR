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
#     the metabolite is the model's;
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
# stats::optim()'s L-BFGS-B over the two etas, from the model (eta = 0),
# within BAYES_FACTOR_LIMIT either way.
#
# The prediction is what the plot draws: processdoseTable() on the drug's
# doses, with the measured times added to its time line (withOutputTimes()),
# read from the plasma series.  So it is the INR for warfarin, a metabolite
# drug's own plus what its dosed parents form, and, with the antiseizure
# interactions on, the drug as they change it.  Drugs with measurements are
# updated one after another, in the dose table's order, each on the updated
# drugs before it.

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
  if (isTRUE(antiseizureInteractions)) return(dosed)
  parents <- dosed[vapply(dosed, function(d) {
    name <- drugs[[d]]$metaboliteName
    !is.null(name) && identical(name, drug)
  }, logical(1))]
  unique(c(drug, parents))
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
                            antiseizureInteractions = FALSE)
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

  predict <- function(entry) {
    candidate <- entries
    candidate[[drug]] <- entry
    out <- withOutputTimes(times, processdoseTable(
      DTsub, ET, candidate, maximum, FALSE, cache = simCache,
      antiseizureInteractions = antiseizureInteractions))
    simCache <<- out
    wide <- out[[drug]]$wide
    if (is.null(wide) || nrow(wide) == 0) return(rep(NA_real_, length(times)))
    at <- match(times, wide$Time)
    values <- wide$Plasma[at]
    missing <- is.na(at)
    if (any(missing))
      values[missing] <- stats::approx(wide$Time, wide$Plasma, times[missing])$y
    values
  }
  entryFor <- function(eta) rebuild(drug, list(clearance = exp(eta[1]), volume = exp(eta[2])))

  population <- predict(drugs[[drug]])
  objective <- function(eta) {
    value <- bayesianObjective(eta, measured$Concentration, predict(entryFor(eta)), omega, sigma)
    # A finite stand-in, so that the search can step back from a parameter
    # set whose prediction is not usable rather than stop
    if (is.finite(value)) value else 1e12
  }
  limit <- log(BAYES_FACTOR_LIMIT)
  fit <- stats::optim(c(0, 0), objective, method = "L-BFGS-B",
                      lower = c(-limit, -limit), upper = c(limit, limit),
                      control = list(factr = 1e7))
  eta <- fit$par
  individual <- predict(entryFor(eta))
  list(
    clearance = exp(eta[1]),
    volume = exp(eta[2]),
    eta = eta,
    objective = fit$value,
    priorObjective = bayesianObjective(c(0, 0), measured$Concentration, population, omega, sigma),
    converged = fit$convergence == 0,
    atLimit = any(abs(eta) > limit * (1 - 1e-6)),
    observations = data.frame(Time = times, Measured = measured$Concentration,
                              Population = population, Individual = individual)
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
#'   recalculatePK() builds it, for the individual's factors (a list of
#'   `clearance` and `volume`); recalculatePK(individual =) is the usual one
#' @param antiseizureInteractions are the antiseizure interactions on?
#' @param fits the `fits` of an earlier call, whose estimates are reused for
#'   any drug whose inputs have not changed
#' @param onFit NULL, or function(drug) called before a drug is fitted afresh
#'   (not when its estimate is reused): the app shows its spinner then
#' @returns a list: `drugs`, updated; `fits`, one per updated drug (with its
#'   `key`); and `problems`, sentences saying what could not be used
#' @export
bayesianUpdateDrugs <- function(drugs, DT, ET, observations,
                                pkVariability = BAYES_PK_VARIABILITY_DEFAULT,
                                residualVariability = BAYES_RESIDUAL_VARIABILITY_DEFAULT,
                                rebuild, antiseizureInteractions = FALSE, fits = NULL,
                                onFit = NULL)
{
  observations <- bayesianObservations(observations)
  problems <- character(0)
  newFits <- list()
  if (nrow(observations) == 0 || is.null(DT) || nrow(DT) == 0)
    return(list(drugs = drugs, fits = newFits, problems = problems))
  omega <- bayesianSD(pkVariability)
  sigma <- bayesianSD(residualVariability)
  ET <- if (is.null(ET)) data.frame(Time = numeric(0), Event = character(0)) else ET
  rownames(DT) <- NULL

  dosed <- unique(DT$Drug)
  measuredDrugs <- unique(observations$Drug)
  # In the dose table's order, so that a drug is updated after the drugs before it
  order <- c(intersect(dosed, measuredDrugs), setdiff(measuredDrugs, dosed))
  priors <- list()
  for (drug in order)
  {
    rows <- observations[observations$Drug == drug, , drop = FALSE]
    entry <- drugs[[drug]]
    if (!drug %in% dosed || is.null(entry) || !is.null(entry$companionOf)) {
      problems <- c(problems, sprintf(
        "%s is not in the dose table, so its %s cannot be used.",
        drug, ngettext(nrow(rows), "measurement", "measurements")))
      next
    }
    relevant <- bayesianRelevantDrugs(drug, drugs, intersect(dosed, names(drugs)),
                                      antiseizureInteractions)
    # A level measured before any dose that reaches it cannot be explained by
    # the drug, and is set aside.
    firstDose <- min(DT$Time[DT$Drug %in% relevant])
    early <- rows$Time <= firstDose
    if (any(early)) {
      problems <- c(problems, sprintf(
        "%s: %d %s at or before the first dose %s not used.", drug, sum(early),
        ngettext(sum(early), "measurement", "measurements"),
        ngettext(sum(early), "is", "are")))
      rows <- rows[!early, , drop = FALSE]
    }
    if (nrow(rows) == 0) next

    DTsub <- DT[DT$Drug %in% relevant, , drop = FALSE]
    stripSim <- function(e) e[setdiff(names(e), c("sim", "simKey"))]
    key <- list(
      drug = drug,
      entries = lapply(drugs[intersect(names(drugs), unique(c(relevant, vapply(
        relevant, function(d) { m <- drugs[[d]]$metaboliteName; if (is.null(m)) "" else m },
        character(1)))))], stripSim),
      DT = DTsub,
      ET = ET,
      measured = rows[, c("Time", "Concentration")],
      omega = omega,
      sigma = sigma,
      antiseizureInteractions = isTRUE(antiseizureInteractions)
    )
    fit <- fits[[drug]]
    if (is.null(fit) || !identical(fit$key, key)) {
      if (is.function(onFit)) onFit(drug)
      fit <- tryCatch(
        bayesianFitDrug(drug, drugs, DT, ET, rows, omega, sigma, rebuild,
                        antiseizureInteractions),
        error = function(e) structure(list(message = conditionMessage(e)), class = "bayesFailed"))
      if (inherits(fit, "bayesFailed")) {
        problems <- c(problems, sprintf("%s could not be updated: %s", drug, fit$message))
        next
      }
      fit$key <- key
    }
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
    updated <- rebuild(drug, list(clearance = fit$clearance, volume = fit$volume))
    updated$Color <- entry$Color
    updated$bayesian <- list(clearance = fit$clearance, volume = fit$volume,
                             n = nrow(rows))
    drugs[[drug]] <- updated
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
    prior$Color <- alternativeModelColor(prior$Color)
    drugs[[name]] <- prior
  }
  list(drugs = drugs, fits = newFits, problems = problems)
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
