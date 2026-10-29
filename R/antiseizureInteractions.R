# -----------------------------------------------------------------------------
# Pharmacokinetic interactions among the antiseizure drugs
# -----------------------------------------------------------------------------
# Provenance
# ----------
# Drafted by Claude Code, 2026-10-24, at the request of Steven L. Shafer, who
# asked for a model of the pharmacokinetic interactions among the epilepsy
# drugs, switched on and off from Graph Options, covering at least phenytoin,
# valproate, lamotrigine and carbamazepine.  The interactions and their
# sources are inst/extdata/antiseizureInteractions.csv, built by
# data-raw/antiseizureInteractions.R; the help page is
# inst/help/models/antiseizure-interactions.md.  Verified by
# tests/testthat/test-antiseizure-interactions.R.
#
# WHAT THE LITERATURE GIVES, AND WHAT IT DOES NOT
# ===============================================
# Almost every quantitative statement of an antiseizure interaction is a
# CATEGORICAL covariate of a population model: "clearance x 1.94 in patients
# taking phenytoin" (Rivas 2008, lamotrigine).  It says how large the change
# is in patients on ordinary maintenance doses of the perpetrator, at steady
# state.  It does not say how the change depends on the perpetrator's
# concentration, nor how fast it develops.  A simulator that adds and stops
# drugs needs both.  So each interaction is written as
#
#     fold(C) = 1 + (Fmax - 1) C / (EC50 + C)                         (1)
#
# a saturable function of the perpetrator's own plotted (total) plasma
# concentration C, calibrated so that fold(Cref) is the published factor at
# the perpetrator's reference concentration Cref (the typical value of its
# band, the concentration of a patient on an ordinary maintenance dose):
#
#     Fmax = 1 + (fold - 1) (EC50 + Cref) / Cref.
#
# Where no source gives EC50 (nearly everywhere), it is set to the LOWER edge
# of the perpetrator's reference range.  Across that range the effect then
# moves by about -25% to +10% around the published factor, which is about as
# flat as a categorical covariate implies, a sub-therapeutic level gives a
# proportionally smaller effect, and no concentration gives an unbounded
# induction or a negative clearance.  That choice is an assumption, recorded
# per row as `ec50_basis`; a row whose source does give the concentration
# dependence (valproate on lamotrigine, Gidal 2003) carries its own.
#
# TIME COURSE
# ===========
#   inhibition (and binding displacement): immediate.  The multiplier follows
#     the perpetrator's concentration, fold(C(t)), as competitive inhibition
#     at the enzyme does.
#   induction: through the turnover of the induced enzymes.  The multiplier E
#     obeys
#
#         dE/dt = kdeg (fold(C(t)) - E),     E(0) = 1                  (2)
#
#     the enzyme-turnover model (production stimulated, first-order
#     degradation) of Magnusson 2008 and Punyawudho 2009.  The degradation
#     half-life is the carbamazepine de-induction estimate, 86.1 h (kdeg
#     0.00805 /h; Punyawudho 2009, intravenous stable-label carbamazepine,
#     read in full), a hybrid of every enzyme carbamazepine induces, and it is
#     used for every inducer because no other inducer has its own estimate;
#     Magnusson's probe estimates, 70 h for CYP3A4 and 105 h for CYP1A2,
#     bracket it.  Induction therefore reaches half its new level in 3.6 days
#     and is over in about two weeks, and de-induction after the inducer is
#     stopped takes as long.  For phenytoin and phenobarbital, whose own
#     half-lives are days, the slow rise of the inducer adds to that.
#
# SEVERAL PERPETRATORS
# ====================
# Inhibitors multiply.  Inducers of one victim do not: they drive the same
# induced enzymes, so their EXCESS adds,
#
#     E_total = 1 + sum_i (E_i - 1)                                    (3)
#
# Two lamotrigine sources say so.  Rivas 2008 (600 patients) estimated two
# or more inducers together at x 2.37, against x 3.10 if phenytoin's 1.94
# and carbamazepine's 1.60 multiplied and x 2.54 if their excesses add;
# Grasela 1999 (527 patients) found a second inducer added only 13%.  The
# combined induction then multiplies any inhibition (valproate with an
# inducer on lamotrigine, as every population model writes it).
#
# CARBAMAZEPINE INDUCES ITSELF
# ============================
# A row whose perpetrator is its own victim is autoinduction.  The library's
# carbamazepine model (Graves 1998) is the INDUCED, maintenance state, so the
# source model already contains the factor at the reference concentration.
# With interactions on, the victim's clearance is
#
#     CL(t) = CL_Graves E(t) / fold(Cref)
#
# which starts from the uninduced clearance CL_Graves / fold at the first dose,
# rises with (2), and returns to Graves's clearance at a steady 8 mg/L.  A
# second inducer's published factor (phenytoin's x 1.42 in Graves) was
# measured against that induced state, so in (3) its excess is counted in
# units of the uninduced clearance, fold(Cref) (E_i - 1): at the reference
# concentrations the two give Graves's x 1.42 exactly.
#
# PHENYTOIN
# =========
# Phenytoin's elimination saturates (R/advanceMichaelisMenten.R), so a factor
# acts on Vmax (induction or inhibition of its metabolism) or on Km (on the
# total scale, Km_total = Km_unbound / fu, so a displacement from albumin, or a
# competitive inhibitor, is a factor on Km).  The engine reads the
# multipliers step by step.
#
# HOW IT RUNS
# ===========
# Every drug is first simulated on its own (processdoseTable()).  With the
# Graph Options box ticked, applyAntiseizureInteractions() then
#   1. finds the dosed victims with a dosed perpetrator (or carbamazepine,
#      which is its own);
#   2. evaluates (1) and (2) on a fine time grid from the perpetrators'
#      present concentrations, and multiplies the factors for each victim
#      parameter;
#   3. cuts each multiplier into segments within which it varies by less than
#      ASM_INTERACTION_TOLERANCE, and holds each segment at its mean;
#   4. re-simulates each victim with one PK set per segment: a linear drug
#      through the event engine (advanceClosedForm1(), exact within each
#      segment, the states carried across each change by convertState()), and
#      phenytoin through its own engine with Vmax and Km multipliers;
#   5. repeats from 2 with the new concentrations, because victims are also
#      perpetrators (phenytoin and carbamazepine induce each other; valproate
#      raises phenytoin, which induces valproate), until no concentration
#      moves by more than ASM_INTERACTION_CONVERGENCE of its peak, or
#      ASM_INTERACTION_MAX_ITERATIONS passes.
# Each re-simulated drug keeps its own simulation without interactions as the
# companion entry "<drug> (no interactions)", drawn beside it as a second line
# (R/alternativeModels.R's companion mechanism), so the size of the
# interaction is on the plot.
#
# WHAT IS NOT MODELLED
# ====================
# Concentration changes of the metabolites (carbamazepine-10,11-epoxide, which
# valproate raises by inhibiting epoxide hydrolase, N-desmethylclobazam), the
# UNBOUND concentrations (valproate displaces phenytoin, so total and free
# phenytoin part company; the library plots totals), interactions with drugs
# outside the antiseizure group (oral contraceptives, rifampicin, azoles), the
# induction of the inducers' own metabolism by each other where no source gives
# a number, and any pharmacodynamic interaction.  Interactions the literature
# describes but does not quantify, or whose victim or perpetrator is not in
# the library, are rows with status "recorded": in the registry and on the
# help page, never applied.
# -----------------------------------------------------------------------------

#' The registry file of antiseizure interactions, in inst/extdata
#' @keywords internal
ASM_INTERACTIONS_FILE <- "antiseizureInteractions.csv"

#' Graph Options input that switches the interactions on
#' @keywords internal
ASM_INTERACTIONS_INPUT <- "antiseizureInteractions"

#' Label of the companion line that shows a drug without its interactions
#' @keywords internal
ASM_INTERACTIONS_REFERENCE_LABEL <- "no interactions"

#' Help page of the interaction model
#' @keywords internal
ASM_INTERACTIONS_HELP_PAGE <- "models/antiseizure-interactions"

#' Largest relative variation of a multiplier within one PK segment
#' @keywords internal
ASM_INTERACTION_TOLERANCE <- 0.02

#' Convergence of the fixed-point iteration: the largest change in any
#' victim's concentration, as a fraction of its peak
#' @keywords internal
ASM_INTERACTION_CONVERGENCE <- 0.002

#' Most passes of the fixed-point iteration
#' @keywords internal
ASM_INTERACTION_MAX_ITERATIONS <- 8L

#' Number of intervals of the grid on which the multipliers are evaluated
#' @keywords internal
ASM_INTERACTION_GRID <- 4000L

#' Finest interval of that grid, in minutes
#' @keywords internal
ASM_INTERACTION_MIN_STEP <- 5

asmInteractionCache <- new.env(parent = emptyenv())

#' The antiseizure interaction registry
#'
#' One row per interaction and source: the perpetrator, the victim, the
#' mechanism, the parameter it changes, the published factor, the reference
#' and half-effect concentrations, the onset, the status (\code{applied} or
#' \code{recorded}) and the source with its PMID.  Built by
#' data-raw/antiseizureInteractions.R.
#'
#' @returns a data frame read from inst/extdata/antiseizureInteractions.csv
#' @export
antiseizureInteractionRegistry <- function()
{
  if (is.null(asmInteractionCache$registry))
    asmInteractionCache$registry <- utils::read.csv(
      system.file("extdata", ASM_INTERACTIONS_FILE, package = "stanpumpR"),
      stringsAsFactors = FALSE, na.strings = c("NA", "")
    )
  asmInteractionCache$registry
}

#' The interactions the simulation applies
#' @noRd
asmAppliedInteractions <- function()
{
  r <- antiseizureInteractionRegistry()
  r[r$status == "applied", , drop = FALSE]
}

#' The drugs whose clearance an applied interaction can change
#'
#' getDrugPK() keeps their covariates, which are needed to rebuild their PK
#' sets segment by segment.
#' @noRd
asmInteractionVictims <- function() unique(asmAppliedInteractions()$victim)

#' The applied interactions active for a set of dosed drugs
#'
#' An interaction is active when both its victim and its perpetrator are
#' dosed; an autoinduction row (perpetrator = victim) when its drug is.
#'
#' @param drugs names of the dosed drugs
#' @returns the active rows of the registry
#' @export
activeAntiseizureInteractions <- function(drugs)
{
  a <- asmAppliedInteractions()
  a[a$victim %in% drugs & a$perpetrator %in% drugs, , drop = FALSE]
}

#' Maximal factor of an interaction, from its published factor at the
#' reference concentration and its half-effect concentration; see (1) in the
#' header.
#' @noRd
asmFmax <- function(fold, cRef, ec50) 1 + (fold - 1) * (ec50 + cRef) / cRef

#' The factor of an interaction at the perpetrator concentration `C`; (1)
#' @noRd
asmFold <- function(C, fold, cRef, ec50)
{
  fmax <- asmFmax(fold, cRef, ec50)
  1 + (fmax - 1) * pmax(C, 0) / (ec50 + pmax(C, 0))
}

#' The multiplier one interaction applies over time
#'
#' @param time the evaluation grid, minutes, from 0
#' @param C the perpetrator's concentration on that grid
#' @param row one row of the registry
#' @returns the multiplier at each point of the grid: fold(C) for an immediate
#'   interaction, the turnover solution of (2) for an induction
#' @noRd
asmInteractionMultiplier <- function(time, C, row)
{
  target <- asmFold(C, row$fold, row$c_ref, row$ec50)
  if (row$onset == "immediate") return(target)
  # Turnover, exactly for a target held at its interval mean (trapezoid)
  kdeg <- log(2) / (row$turnover_half_life_h * 60)
  E <- numeric(length(time))
  E[1] <- 1
  if (length(time) > 1)
  {
    decay <- exp(-kdeg * diff(time))
    mid <- (target[-1] + target[-length(target)]) / 2
    for (k in seq_along(decay))
      E[k + 1] <- mid[k] + (E[k] - mid[k]) * decay[k]
  }
  E
}

#' The evaluation grid of the multipliers, minutes
#' @noRd
asmInteractionGrid <- function(maximum)
{
  step <- max(ASM_INTERACTION_MIN_STEP, maximum / ASM_INTERACTION_GRID)
  unique(c(seq(0, maximum, by = step), maximum))
}

#' Cut a multiplier into segments of nearly constant value
#'
#' Greedy: a segment grows interval by interval while the range of the
#' interval means in it stays within `tolerance` of their mean.  Each segment
#' is held at the mean of its intervals.
#'
#' @param time the grid
#' @param m the multiplier on the grid
#' @param tolerance the largest relative range within a segment
#' @returns a data frame of `start` (minutes) and `value`, one row per segment
#' @noRd
asmSegments <- function(time, m, tolerance = ASM_INTERACTION_TOLERANCE)
{
  n <- length(time)
  if (n < 2) return(data.frame(start = 0, value = m[1]))
  dt <- diff(time)
  mid <- (m[-1] + m[-n]) / 2
  starts <- integer(0)
  values <- numeric(0)
  i <- 1L
  while (i <= length(mid))
  {
    lo <- hi <- mid[i]
    w <- dt[i]
    s <- mid[i] * dt[i]
    j <- i
    while (j < length(mid))
    {
      nlo <- min(lo, mid[j + 1])
      nhi <- max(hi, mid[j + 1])
      ns <- s + mid[j + 1] * dt[j + 1]
      nw <- w + dt[j + 1]
      if ((nhi - nlo) > tolerance * ns / nw) break
      lo <- nlo; hi <- nhi; s <- ns; w <- nw
      j <- j + 1L
    }
    starts <- c(starts, i)
    values <- c(values, s / w)
    i <- j + 1L
  }
  data.frame(start = time[starts], value = values)
}

#' The multipliers of each parameter of one victim
#'
#' Inductions of one parameter add in their excess, (3); an autoinduction
#' divides the sum by its reference factor and counts the other inducers'
#' excess in units of the uninduced clearance; immediate interactions
#' multiply.  See the header.
#'
#' @param victim the victim drug
#' @param rows the active interactions of that victim
#' @param conc named list of each dosed drug's series (Time, Plasma)
#' @param time the evaluation grid
#' @returns a named list, one multiplier vector per parameter that some row
#'   changes ("CL", "Vmax", "Km")
#' @noRd
asmVictimMultipliers <- function(victim, rows, conc, time)
{
  out <- list()
  for (p in unique(rows$parameter))
  {
    these <- rows[rows$parameter == p, , drop = FALSE]
    induced <- these$onset == "turnover"
    self <- induced & these$perpetrator == these$victim
    selfFold <- if (any(self)) these$fold[self][1] else 1
    excess <- rep(0, length(time))
    immediate <- rep(1, length(time))
    for (k in seq_len(nrow(these)))
    {
      row <- these[k, ]
      series <- conc[[row$perpetrator]]
      C <- stats::approx(series$Time, series$Plasma, xout = time, rule = 2,
                         ties = mean)$y
      m <- asmInteractionMultiplier(time, C, row)
      if (induced[k]) {
        excess <- excess + (if (self[k]) 1 else selfFold) * (m - 1)
      } else {
        immediate <- immediate * m
      }
    }
    out[[p]] <- (1 + excess) / selfFold * immediate
  }
  out
}

#' A victim's PK with one PK set per clearance segment
#'
#' The drug's model is evaluated again (getDrugPK() keeps its covariates for
#' every drug an interaction can change), its default set copied once per
#' segment with the elimination clearance scaled, and the copies resolved by
#' getDrugPK(model = ...), so every set, and every further oral formulation's
#' set, is built exactly as the drug's own.
#'
#' @param PK the drug's resolved PK
#' @param segments from asmSegments()
#' @returns `PK` with the segment sets in `PK` and `oralFormulations`,
#'   `pkEvents` naming them, and `interactionEvents`, the event table that
#'   switches between them (read by simCpCe())
#' @noRd
asmSegmentPK <- function(PK, segments)
{
  if (is.null(PK$covariates))
    stop("Internal error: no covariates kept for ", PK$drug, ".")
  X <- do.call(PK$drug, PK$covariates)
  base <- X$PK[[PK_EVENT_DEFAULT]]
  n <- nrow(segments)
  keys <- c(PK_EVENT_DEFAULT, if (n > 1) sprintf("ddi%05d", seq_len(n - 1)))
  X$PK <- stats::setNames(lapply(segments$value, function(m) {
    s <- base
    s$cl1 <- s$cl1 * m
    s
  }), keys)
  rebuilt <- do.call(getDrugPK, c(list(drug = PK$drug), PK$covariates,
                                  list(model = X)))
  PK$PK <- rebuilt$PK
  PK$oralFormulations <- rebuilt$oralFormulations
  PK$pkEvents <- keys
  PK$interactionEvents <- data.frame(Time = segments$start, Event = keys,
                                     stringsAsFactors = FALSE)
  PK
}

#' Simulate one victim with its interaction multipliers
#'
#' @returns the simCpCe() result, with `interaction`: the segments of each
#'   parameter's multiplier
#' @noRd
asmSimulateVictim <- function(PK, multipliers, time, DT, ET, maximum, plotRecovery)
{
  segs <- lapply(multipliers, function(m) asmSegments(time, m))
  if (!is.null(PK$michaelisMenten))
  {
    if (!all(names(multipliers) %in% c("Vmax", "Km")))
      stop("Internal error: a clearance interaction on ", PK$drug,
           ", whose elimination is saturable.")
    one <- function(p) if (is.null(segs[[p]])) data.frame(start = 0, value = 1) else segs[[p]]
    starts <- sort(unique(c(one("Vmax")$start, one("Km")$start)))
    at <- function(s, t) s$value[findInterval(t, s$start)]
    PK$interactionMM <- list(time = starts, vmax = at(one("Vmax"), starts),
                             km = at(one("Km"), starts))
  } else {
    if (!identical(names(multipliers), "CL"))
      stop("Internal error: a Vmax or Km interaction on ", PK$drug,
           ", whose elimination is linear.")
    PK <- asmSegmentPK(PK, segs$CL)
  }
  X <- simCpCe(DT, ET, PK, maximum, plotRecovery)
  X$interaction <- segs
  X
}

#' The fields of a drug's entry that one simulation fills
#'
#' Shared by processdoseTable() and applyAntiseizureInteractions(), so that a
#' re-simulated drug is stored exactly as a first simulation is.
#' @noRd
simulationFields <- function(X, DT, ET)
{
  list(
    DT                = DT,
    ET                = ET,
    results           = X$results,
    equiSpace         = X$equiSpace,
    max               = X$max,
    # wideOwn is this drug's own simulation and nothing else.  wide is what
    # gets plotted, and may additionally carry metabolite formed from
    # another drug.  Keeping them apart is what makes folding idempotent.
    wideOwn           = X$wide,
    wide              = X$wide,
    metaboliteSeries  = X$metaboliteSeries,
    # The effect-site states behind Recovery, which foldMetabolites() needs
    # to solve the combined time until threshold.  Own and formed are kept
    # apart for the same reason wideOwn and wide are.
    recoveryStatesOwn        = X$recoveryStates,
    metaboliteRecoveryStates = X$metaboliteRecoveryStates,
    tci               = X$tci,
    scheduled         = X$scheduled,
    # The peripheral concentrations and the depot (R/compartments.R), own
    # and as plotted, kept apart as wideOwn and wide are, and the formed
    # metabolite's, which foldMetabolites() adds to the metabolite drug's.
    compartmentsOwn        = X$compartments,
    compartments           = X$compartments,
    metaboliteCompartments = X$metaboliteCompartments
  )
}

#' Apply the antiseizure interactions to simulated drugs
#'
#' Called by processdoseTable() after every drug has been simulated on its
#' own; see the header of R/antiseizureInteractions.R.  The drugs not changed
#' are returned untouched.  A changed drug's entry holds the simulation with
#' interactions in the usual fields, its own simulation without them stays in
#' `sim` (so the cache still reuses it), and `interaction` records the
#' segments of its multipliers and the rows that produced them.
#'
#' @param drugs the drug list from processdoseTable(), each simulated alone
#' @param DT the dose table
#' @param ET the event table
#' @param plotMaximum end of the simulation, minutes
#' @param plotRecovery should the time until threshold be computed?
#' @param cache the previous call's drug list: a drug whose interaction inputs
#'   (its own and its perpetrators' simulation keys) are unchanged reuses its
#'   interaction simulation
#' @returns the drug list
#' @keywords internal
applyAntiseizureInteractions <- function(drugs, DT, ET, plotMaximum, plotRecovery,
                                         cache = NULL)
{
  own <- Filter(function(d) is.null(d$companionOf) && !is.null(d$wideOwn), drugs)
  active <- activeAntiseizureInteractions(names(own))
  if (nrow(active) == 0) return(drugs)

  victims <- unique(active$victim)
  # Everything the interaction simulation of the whole group depends on.
  participants <- sort(unique(c(active$victim, active$perpetrator)))
  key <- list(
    keys = lapply(stats::setNames(participants, participants),
                  function(d) drugs[[d]]$simKey),
    active = active,
    tolerance = ASM_INTERACTION_TOLERANCE
  )
  reused <- !is.null(cache) && all(vapply(victims, function(v)
    identical(cache[[v]]$interactionKey, key) && !is.null(cache[[v]]$interactionSim),
    logical(1)))

  if (reused) {
    sims <- lapply(stats::setNames(victims, victims), function(v) cache[[v]]$interactionSim)
  } else {
    time <- asmInteractionGrid(plotMaximum)
    conc <- lapply(own, function(d) d$wideOwn[, c("Time", "Plasma")])
    sims <- list()
    for (iteration in seq_len(ASM_INTERACTION_MAX_ITERATIONS))
    {
      change <- 0
      newConc <- conc
      for (v in victims)
      {
        rows <- active[active$victim == v, , drop = FALSE]
        m <- asmVictimMultipliers(v, rows, conc, time)
        tempDT <- DT[DT$Drug == v, ]
        tempET <- drugs[[v]]$ET
        if (is.null(tempET)) tempET <- ET[0, , drop = FALSE]
        X <- asmSimulateVictim(drugs[[v]], m, time, tempDT, tempET,
                               plotMaximum, plotRecovery)
        X$interaction$rows <- rows
        sims[[v]] <- X
        newConc[[v]] <- X$wide[, c("Time", "Plasma")]
        old <- stats::approx(conc[[v]]$Time, conc[[v]]$Plasma,
                             xout = newConc[[v]]$Time, rule = 2, ties = mean)$y
        peak <- max(newConc[[v]]$Plasma, old, .Machine$double.eps)
        change <- max(change, max(abs(newConc[[v]]$Plasma - old)) / peak)
      }
      conc <- newConc
      outputComments("Antiseizure interactions: pass", iteration,
                     "largest change", signif(change, 3),
                     level = DEBUG_LEVEL_VERBOSE)
      if (change < ASM_INTERACTION_CONVERGENCE) break
    }
  }

  for (v in victims)
  {
    X <- sims[[v]]
    fields <- simulationFields(X, drugs[[v]]$DT, drugs[[v]]$ET)
    for (field in names(fields)) drugs[[v]][[field]] <- fields[[field]]
    drugs[[v]]$interaction <- X$interaction
    drugs[[v]]$interactionSim <- X
    drugs[[v]]$interactionKey <- key
  }
  drugs
}

#' Companion entries that show each changed drug without its interactions
#'
#' One per dosed victim of an active interaction: a copy of the drug's own
#' resolved PK, named "<drug> (no interactions)", which processdoseTable()
#' simulates on the drug's doses like any companion (R/alternativeModels.R)
#' and applyAntiseizureInteractions() leaves alone.
#'
#' @param drugs the drug list from recalculatePK(), resolved
#' @param dosed the names of the dosed drugs
#' @returns the drug list with the companion entries added
#' @noRd
addAntiseizureReferenceEntries <- function(drugs, dosed)
{
  for (v in unique(activeAntiseizureInteractions(dosed)$victim))
  {
    entry <- drugs[[v]]
    if (is.null(entry)) next
    name <- asmReferenceName(v)
    entry$drug           <- name
    entry$companionOf    <- v
    entry$modelLabel     <- ASM_INTERACTIONS_REFERENCE_LABEL
    entry$companionGroup <- ASM_INTERACTIONS_INPUT
    entry$helpPage       <- ASM_INTERACTIONS_HELP_PAGE
    entry$Color          <- alternativeModelColor(entry$Color)
    drugs[[name]] <- entry
  }
  drugs
}

#' The name of a drug's "without interactions" companion
#' @noRd
asmReferenceName <- function(drug)
  paste0(drug, " (", ASM_INTERACTIONS_REFERENCE_LABEL, ")")

#' One line per active interaction, for the notification and the export
#'
#' @param drugs names of the dosed drugs
#' @returns a character vector ("valproate raises lamotrigine: clearance x
#'   0.49 at 75 mcg/mL (UGT inhibition; Rivas 2008)")
#' @export
antiseizureInteractionSummary <- function(drugs)
{
  a <- activeAntiseizureInteractions(drugs)
  if (nrow(a) == 0) return(character(0))
  what <- c(CL = "clearance", Vmax = "Vmax", Km = "Km (total)")
  verb <- ifelse(a$perpetrator == a$victim, "induces its own metabolism",
                 ifelse(a$fold > 1 & a$parameter != "Km", "lowers",
                        ifelse(a$fold < 1 & a$parameter != "Km", "raises", "changes")))
  ifelse(
    a$perpetrator == a$victim,
    sprintf("%s %s: clearance x %s at %s mcg/mL once induced (%s; %s)",
            a$victim, verb, signif(a$fold, 3), a$c_ref, a$enzyme, a$source_short),
    sprintf("%s %s %s: %s x %s at %s mcg/mL (%s; %s)",
            a$perpetrator, verb, a$victim, what[a$parameter], signif(a$fold, 3),
            a$c_ref, a$enzyme, a$source_short)
  )
}

#' The applied interactions, as a Markdown table for the help page
#'
#' Generated block "antiseizure-interaction-table"
#' (inst/help/models/antiseizure-interactions.md), built from the registry so
#' that the page cannot drift from what the simulation applies.  Each row gives
#' the published factor at the perpetrator's reference concentration, the
#' maximal factor that (1) implies, and the EC50.
#' @noRd
helpAntiseizureInteractionTableMarkdown <- function(drugDefaults = getDrugDefaultsGlobal()) {
  a <- asmAppliedInteractions()
  a <- a[order(a$victim, a$perpetrator), ]
  what <- c(CL = "clearance", Vmax = "Vmax", Km = "Km (total)")
  rows <- vapply(seq_len(nrow(a)), function(i) {
    r <- a[i, ]
    perpetrator <- if (r$perpetrator == r$victim) "(itself)" else
      sprintf("[%s](help:drugs/%s)", r$perpetrator, r$perpetrator)
    sprintf("| [%s](help:drugs/%s) | %s | %s (%s) | %s x %s | %s | %s | %s | %s | [%s](https://pubmed.ncbi.nlm.nih.gov/%s/) |",
            r$victim, r$victim, perpetrator, r$mechanism, r$enzyme,
            what[[r$parameter]], helpFormatNumber(r$fold), helpFormatNumber(r$c_ref),
            helpFormatNumber(signif(asmFmax(r$fold, r$c_ref, r$ec50), 3)),
            helpFormatNumber(r$ec50),
            if (r$onset == "turnover") "induction (turnover, 86 h)" else "immediate",
            r$source_short, r$pmid)
  }, character(1))
  paste(c(paste("| Victim | Perpetrator | Mechanism | Factor at reference |",
                "Reference, mcg/mL | Maximal factor | EC50, mcg/mL | Onset | Source |"),
          "|---|---|---|---|---|---|---|---|---|", rows), collapse = "\n")
}

#' Every source in the registry, and the reading list, as Markdown
#'
#' Generated block "antiseizure-interaction-sources": one line per source
#' (applied or cross-check) with its PMID, whether its full text is in PubMed
#' Central, and what it supplies; the sources marked `to_obtain` are listed
#' first, as the papers to download.
#' @noRd
helpAntiseizureInteractionSourcesMarkdown <- function(drugDefaults = getDrugDefaultsGlobal()) {
  r <- antiseizureInteractionRegistry()
  r <- r[!duplicated(r$pmid), ]
  line <- function(x) sprintf(
    "- %s [PMID %s](https://pubmed.ncbi.nlm.nih.gov/%s/)%s. %s",
    x$source, x$pmid, x$pmid,
    if (!is.na(x$pmcid)) paste0(", full text in PubMed Central (", x$pmcid, ")") else "",
    x$finding)
  get <- r[r$to_obtain == "yes", ]
  rest <- r[r$to_obtain != "yes", ]
  paste(c(
    "**Full text to obtain** (the abstract alone was read, and the full text would give the standard errors or a number the abstract omits):",
    "",
    vapply(seq_len(nrow(get)), function(i) line(get[i, ]), character(1)),
    "",
    "**Read in full, or complete in the abstract:**",
    "",
    vapply(seq_len(nrow(rest)), function(i) line(rest[i, ]), character(1))
  ), collapse = "\n")
}
