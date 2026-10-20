# -----------------------------------------------------------------------------
# Provenance
# ----------
# Written by Claude Code, 2026-10-19, at the request of Steven L. Shafer: "Add
# a graph option to display peripheral compartment concentrations ... the
# concentrations in V2 and V3, or the MRG and FG for inhaled anesthetics ...
# grouped directly under the plasma compartment, which is V1 by definition.
# For drugs given by other routes of delivery (e.g., oral, IN, IM), it would
# also include the depot compartment."
#
# STATUS: verified by tests/testthat/test-compartments.R against a matrix
# exponential of the full compartmental system, for every engine.
# -----------------------------------------------------------------------------
#
# THE PERIPHERAL CONCENTRATIONS ARE READ OFF THE PLASMA STATES
# ============================================================
# The closed-form engines carry the plasma concentration as a sum of
# exponential states, one per mode of the system: the disposition eigenvalues
# lambda_j and, for an absorbed dose, each depot's ka (and, for a formed
# metabolite, the parent's modes as well).  Every input is into the central
# compartment, so each mode mu of the solution satisfies the peripheral
# compartment's own equation, dA2/dt = k12 A1 - k21 A2, on its own:
#
#     -mu A2,mu = k12 A1,mu - k21 A2,mu   so   A2,mu = k12 A1,mu / (k21 - mu)
#
# With A1,mu = V1 s_mu (s_mu the state, a concentration) and V1 k12 = V2 k21,
#
#     C2 = A2 / V2 = k21 * sum_mu s_mu / (k21 - mu)
#
# and the same with k31 for V3.  There is no k21 mode to add: k21 is not an
# eigenvalue of the system, and C2 starts at zero.  It is exact, it is the
# relation convertState() already uses to turn the states into amounts, and it
# costs a few vector operations, so the engines compute it on every run whether
# or not it is plotted, and it never enters the simulation cache key.
#
# An input mode that equals k21 (an absorption rate, say) is not a division by
# zero in exact arithmetic: the central response to that mode carries the
# factor (k21 - mu) itself (the numerator of the central transfer function is
# (s + k21)(s + k31)).  In floating point an exact tie is 0 / 0, so k21 is
# nudged off every mode by separateRate(), a relative 1e-8, as getDrugPK()
# nudges ke0.
#
# THE DEPOT
# =========
# The depot is an amount, not a concentration: what is still to be absorbed,
# summed over every absorption depot the drug has (oral, the second oral depot,
# sublingual, intramuscular, intranasal, subcutaneous or regional, and the slow
# tissue depot).  It is the model's depot: the bioavailable amount, F times the
# dose, entering when the absorption lag has passed.  Each depot is advanced
# exactly over every step, G <- G exp(-ka dt) + F x (dose arriving), and a
# constant rate R into it over a step adds F R (1 - exp(-ka dt)) / ka.
#
# The engines return the three as columns C2, C3 and Depot (zero where the
# drug has none); simCpCe() lifts them into $compartments, keeping only the
# compartments the drug has.
# -----------------------------------------------------------------------------


#' The compartments the plot can show beside a drug's plasma panel
#'
#' The peripheral volumes of a mammillary model, the muscle and fat groups of
#' an inhaled agent, and the depot of an absorbed dose, in the order their
#' panels are drawn under the drug's own.
#'
#' @keywords internal
COMPARTMENT_PANELS <- c("V2", "V3", "MRG", "FG", "Depot")


#' Nudge a rate constant off every mode it ties with
#'
#' @param k a rate constant, or one per point
#' @param modes the modes, a vector or a matrix with one row per point
#' @returns \code{k}, multiplied by \code{1 + 1e-8} wherever it ties with a mode
#' @keywords internal
separateRate <- function(k, modes)
{
  modes <- if (is.null(dim(modes))) matrix(modes, nrow = 1) else as.matrix(modes)
  if (nrow(modes) == 1 && length(k) > 1)
    modes <- matrix(modes, length(k), ncol(modes), byrow = TRUE)
  k <- rep_len(k, nrow(modes))
  tie <- abs(modes - k) < 1e-10 * pmax(abs(modes), abs(k))
  tie[is.na(tie)] <- FALSE
  ifelse(rowSums(tie) > 0, k * (1 + 1e-8), k)
}


#' The concentration in a peripheral compartment from the plasma states
#'
#' \code{k * sum(s / (k - mu))} over the modes; see the header of
#' \code{R/compartments.R}.
#'
#' @param states the plasma states, a list of vectors or a matrix with one
#'   column per mode
#' @param modes the modes' rate constants, one per column, or a matrix with one
#'   row per point (an engine whose PK set changes on an event)
#' @param k the compartment's return rate constant (k21 or k31), one value or
#'   one per point; zero, or not finite, where there is no such compartment
#'
#' @returns the concentration, one per point; zero where \code{k} is zero
#' @keywords internal
peripheralConcentration <- function(states, modes, k)
{
  if (is.list(states)) states <- do.call(cbind, states)
  states <- as.matrix(states)
  L <- nrow(states)
  k <- rep_len(k, L)
  k[!is.finite(k) | k < 0] <- 0
  if (!any(k > 0)) return(rep(0, L))
  modes <- if (is.null(dim(modes))) matrix(modes, nrow = 1) else as.matrix(modes)
  if (nrow(modes) == 1) modes <- matrix(modes, L, ncol(states), byrow = TRUE)
  k <- separateRate(k, modes)
  term <- states * k / (k - modes)
  # A state that is zero throughout contributes nothing, whatever its mode
  # (an unused depot at ka = 0, or a missing third eigenvalue at 0 when k31 is
  # also 0, which would be 0 / 0).
  term[states == 0] <- 0
  out <- rowSums(term)
  out[k == 0] <- 0
  out
}


#' The amount in an absorption depot over a time line
#'
#' @param ka the depot's absorption rate constant, one value or one per step
#' @param dt the step into each point
#' @param arriving the amount landing in the depot at each point, already
#'   scaled by the bioavailability
#' @param rate a constant rate into the depot over the step into each point,
#'   already scaled by the bioavailability (a continuous perineural infusion);
#'   zero by default
#'
#' @returns the amount in the depot at each point
#' @keywords internal
depotAmount <- function(ka, dt, arriving, rate = 0)
{
  L <- length(dt)
  ka <- rep_len(ka, L)
  decay <- exp(-ka * dt)
  rate <- rep_len(rate, L)
  infused <- ifelse(ka > 0, rate * (1 - decay) / ifelse(ka > 0, ka, 1), rate * dt)
  infused[rate == 0] <- 0
  advanceState(decay, arriving, infused, 0, L)
}


#' Do any of these PK sets have a second or third compartment?
#'
#' @param pkSets PK sets from \code{getDrugPK()}
#' @param n 2 or 3
#' @returns TRUE when any set has that compartment
#' @keywords internal
anyCompartment <- function(pkSets, n)
{
  any(vapply(pkSets, function(s) {
    !is.null(s$lambda_2) && !is.null(s$lambda_3) && compartmentCount(s) >= n
  }, logical(1)))
}


#' The amount unit of a drug's depot, and the factor from the engine's amount
#'
#' The engines' amount is whatever, divided by litres, gives the concentration
#' unit: mg for mcg/mL, mcg for ng/mL, ng for pg/mL.  Insulin's is mU, shown
#' in U, and an activity drug's is a thousand IU, shown in IU.
#'
#' @param concentrationUnits the drug's \code{Concentration.Units}
#' @returns a list of \code{units} and \code{factor}, or NULL for a unit with
#'   no depot to show
#' @keywords internal
depotUnits <- function(concentrationUnits)
{
  switch(
    concentrationUnits,
    mcg  = list(units = "mg",   factor = 1),
    ng   = list(units = "mcg",  factor = 1),
    pg   = list(units = "ng",   factor = 1),
    mmol = list(units = "mmol", factor = 1),
    mcU  = list(units = "U",    factor = 1 / 1000),
    IU   = list(units = "IU",   factor = 1000),
    NULL
  )
}


#' Lift the compartment columns off an engine's output
#'
#' @param results an engine's output, with \code{C2}, \code{C3} and
#'   \code{Depot} columns (an engine that predates them may lack them)
#' @param PK the drug's PK, from \code{getDrugPK()}
#' @param dose the dose table the engines ran, with its route flags
#'
#' @returns a list of \code{results} without those columns and
#'   \code{compartments}, a data frame of \code{Time} and one column per
#'   compartment the drug has (\code{V2}, \code{V3}, \code{Depot}), or NULL
#'   when it has none to show
#' @keywords internal
liftCompartments <- function(results, PK, dose)
{
  columns <- c("C2", "C3", "Depot")
  present <- intersect(columns, names(results))
  lifted <- results[present]
  for (col in present) results[[col]] <- NULL
  if (length(present) == 0) return(list(results = results, compartments = NULL))

  # A drug plotted as something other than a concentration in V1 (an osmotic
  # agent's serum osmolality, warfarin's INR) has no peripheral concentration
  # to set beside it.
  if (!is.null(PK$osmotic) || !is.null(PK$inr))
    return(list(results = results, compartments = NULL))

  sets <- c(PK$PK,
            unlist(lapply(PK$parallelSystems, `[[`, "PK"), recursive = FALSE),
            unlist(PK$oralFormulations, recursive = FALSE))
  sets <- Filter(is.list, sets)
  hasV2 <- anyCompartment(sets, 2)
  hasV3 <- anyCompartment(sets, 3)
  mm <- PK$michaelisMenten
  if (!is.null(mm) && (isTRUE(PK$PK[[1]]$cl2 > 0) || !is.null(mm$k23))) hasV2 <- TRUE

  depotRoutes <- c("PO", "PO2", "IM", "IN", "SL", "RA", "RAslow", "RArate")
  dosedDepot <- any(vapply(depotRoutes, function(r) {
    x <- dose[[r]]
    !is.null(x) && any(x & dose$Dose != 0)
  }, logical(1)))
  units <- depotUnits(PK$Concentration.Units)
  hasDepot <- "Depot" %in% present && !is.null(units) &&
    (dosedDepot || any(lifted$Depot > 0))

  out <- data.frame(Time = results$Time)
  if (hasV2 && "C2" %in% present) out$V2 <- lifted$C2
  if (hasV3 && "C3" %in% present) out$V3 <- lifted$C3
  if (hasDepot) out$Depot <- lifted$Depot * units$factor
  list(
    results = results,
    compartments = if (ncol(out) > 1) out else NULL
  )
}


#' Add formed metabolite to a drug's compartments
#'
#' The peripheral concentrations superpose as the plasma does, so the formed
#' contribution is added to the receiving drug's own on the fold's time line.
#' A depot is the drug's own: metabolite is formed in the body, never in it.
#'
#' @param own the receiving drug's own compartments, or NULL
#' @param additions a list of the formed contributions' compartments (data
#'   frames of \code{Time}, \code{V2}, \code{V3})
#' @param times the fold's time line
#'
#' @returns the summed compartments on \code{times}, or NULL
#' @keywords internal
mergeCompartmentSeries <- function(own, additions, times)
{
  parts <- Filter(function(x) !is.null(x) && nrow(x) > 0, c(list(own), additions))
  if (length(parts) == 0) return(NULL)
  columns <- unique(unlist(lapply(parts, function(x) setdiff(names(x), "Time"))))
  columns <- intersect(c("V2", "V3", "Depot"), columns)
  if (length(columns) == 0) return(NULL)
  onto <- function(df, col) {
    if (is.null(df[[col]])) return(rep(0, length(times)))
    if (nrow(df) == 1) return(rep(df[[col]], length(times)))
    stats::approx(df$Time, df[[col]], times, rule = 2)$y
  }
  out <- data.frame(Time = times)
  for (col in columns)
    out[[col]] <- Reduce(`+`, lapply(parts, onto, col = col))
  out
}


#' Which drug and compartment a compartment panel's series belongs to
#'
#' @param series series names, e.g. "propofol V2" or "morphine Depot"
#' @returns a data frame of \code{drug} and \code{compartment}, NA for a series
#'   that is not a compartment's
#' @keywords internal
compartmentSeries <- function(series)
{
  pattern <- paste0("^(.*) (", paste(COMPARTMENT_PANELS, collapse = "|"), ")$")
  hit <- grepl(pattern, series)
  data.frame(
    drug        = ifelse(hit, sub(pattern, "\\1", series), NA_character_),
    compartment = ifelse(hit, sub(pattern, "\\2", series), NA_character_),
    stringsAsFactors = FALSE
  )
}


#' The strip label of a panel
#'
#' A compartment panel sits directly under its drug's own panel and is drawn
#' in the drug's colour, so its strip names only the compartment ("V2",
#' "depot") and the unit; the drug's name would not fit beside it.  The facet
#' value itself stays "<drug> <compartment>\\n(<unit>)", unique to the drug,
#' because the hover and the click find the series through it.  Every other
#' panel is labelled with its own value, as before.
#'
#' @param wrap facet values
#' @returns the labels
#' @keywords internal
compartmentPanelLabel <- function(wrap)
{
  wrap <- as.character(wrap)
  title <- sub("\n.*$", "", wrap)
  rest  <- ifelse(grepl("\n", wrap), sub("^[^\n]*", "", wrap), "")
  hit <- compartmentSeries(title)
  shown <- ifelse(hit$compartment == "Depot", "depot", hit$compartment)
  ifelse(is.na(hit$drug), wrap, paste0(shown, rest))
}
