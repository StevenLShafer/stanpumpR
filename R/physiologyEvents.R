# -----------------------------------------------------------------------------
# Physiology events: the physiology panel recorded at a time
# -----------------------------------------------------------------------------
# Steven L. Shafer, 2026-11-19: "show [and] record changes in physiology at
# certain times, and then add that to the event table.  This would allow
# physiology to change during anesthesia.  Changes would always be step
# functions at the time ... There would be a button in the physiology slider
# box that said 'create event'.  The user would enter a time, in whatever units
# matched the plot time units, and the status of all sliders would be recorded
# at that time."  And: "At time 0 the sliders are all set to 1, sea level,
# and no endobronchial intubation ... If no event is set, then the assumption
# is that they go back to time 0.  However, if an event is set, then all of
# the sliders, and presence/absence of endobronchial intubation (if a gas) is
# recorded as of the time of the event.  The PK evolve from there with the new
# physiology ... the amounts in the compartments, whatever they represent,
# stay the same.  Flows and volumes change with the new physiology."  The
# event's label is "Physiology"; a hover tip gives the settings.  Drafted by
# Claude Code, 2026-11-19.
#
# SO THE TIME LINE IS: without a physiology event, the panel's settings apply
# from time 0, as before.  With one or more, physiology is NORMAL from time 0
# (PHYSIOLOGY_STATE_NORMAL) until the first event, and each event's recorded
# settings from its time until the next; the panel's sliders then set the next
# event to be recorded.
#
# WHERE THE SETTINGS LIVE.  In the event table, as the `Settings` column of a
# row whose Event is "Physiology": one string, physiologyEventEncode(), so the
# row carries its own settings through the Edit events dialog, a bookmark and
# the workbook, and a row is never separated from what it records.  Every
# reader parses it with physiologyEventDecode(), which refuses anything out of
# range (a link is untrusted input).
#
# THE INTRAVENOUS DRUGS.  A drug the sliders change is built once per
# physiology state (physiologyEventPK()): normal, then each event's settings.
# Its PK sets are merged, keyed by the clinical event and the state
# (physiologyEventKey(): the normal state keeps the clinical names, so
# "default" is still the set in force at time 0), and simCpCe() switches
# between them on one time line that interleaves the clinical events and the
# physiology events (physiologyEventTimeline()), on the event engine
# (advanceClosedForm1()).  At each switch convertState() carries the AMOUNT in
# each compartment into the new set's states -- Shafer's conversion, in
# STANPUMP since before the repository's first commit (2019-09-21), checked
# against a matrix exponential to 2e-15 on 2026-11-19 -- so drug does not move
# between compartments at a step; ke0 is per set, and the effect site, a
# concentration, carries across unchanged.
#
# NOT CHANGED BY AN EVENT.  A drug the sliders do not change
# (physiologyUnsupported(), or no applied row) is not changed by an event
# either.  A drug with several oral formulations takes the events (each
# formulation's sets are merged the same way).  A drug whose PK sets the
# antiseizure interactions replace with their own segments keeps the
# interactions' and is held at normal physiology, said in the panel
# (physiologyStepsSkipped()).  The TCI pump plans on the set in force at time
# 0 (an event at time 0 included, physiologyEventPump()) and does not re-plan
# at a later event, so the concentration drifts from the target after it, as
# it would in a patient.  The Bayesian update fits at normal physiology
# throughout and applies the events to the patient it finds.  Both DECIDED by
# Shafer on 2026-11-19 ("Keep both"): do not make the pump re-plan at events
# or the fit read them without reopening that decision.
# -----------------------------------------------------------------------------

#' The event name of a recorded physiology
#' @keywords internal
PHYSIOLOGY_EVENT_NAME <- "Physiology"

#' The physiology state index of normal physiology, before the first event
#' @keywords internal
PHYSIOLOGY_STATE_NORMAL <- 0L

#' The most physiology events one table may hold
#' @keywords internal
PHYSIOLOGY_EVENT_MAX <- 50L

#' The settings one physiology event records, in order
#' @keywords internal
PHYSIOLOGY_EVENT_FIELDS <- c(names(PHYSIOLOGY_DEFAULT), "altitude", "endobronchial",
                             "haemoglobin")

#' Normal physiology as a physiology event's settings
#' @keywords internal
physiologyEventNormal <- function()
  c(PHYSIOLOGY_DEFAULT, altitude = ALTITUDE_DEFAULT, endobronchial = 0,
    haemoglobin = ARTERIAL_OXYGEN_HAEMOGLOBIN)

#' Check one physiology event's settings
#'
#' @param x a named numeric vector or list of PHYSIOLOGY_EVENT_FIELDS (a
#'   missing field is normal)
#' @returns the settings as a named numeric vector, endobronchial 0 or 1; an
#'   error if any is out of range
#' @keywords internal
checkPhysiologyEvent <- function(x)
{
  if (is.list(x)) x <- unlist(x)
  if (!is.numeric(x) && !is.logical(x)) stop("Invalid physiology event settings.")
  out <- physiologyEventNormal()
  if (is.null(names(x)) || !all(names(x) %in% PHYSIOLOGY_EVENT_FIELDS) ||
      anyDuplicated(names(x)))
    stop("Invalid physiology event settings: named by ",
         paste(PHYSIOLOGY_EVENT_FIELDS, collapse = ", "))
  out[names(x)] <- as.numeric(x)
  p <- checkPhysiology(out[names(PHYSIOLOGY_DEFAULT)])
  out[names(p)] <- p
  out[["altitude"]] <- checkAltitude(out[["altitude"]])
  if (!out[["endobronchial"]] %in% c(0, 1))
    stop("Invalid physiology event settings: endobronchial must be 0 or 1")
  out[["haemoglobin"]] <- checkHaemoglobin(out[["haemoglobin"]])
  out
}

#' A physiology event's settings as the event table's Settings string
#'
#' Only the settings away from normal, so that the string, which the Edit
#' events dialog shows, stays readable; "normal" when there are none.
#'
#' @param x settings, as checkPhysiologyEvent() takes them
#' @returns one string, "cardiac=0.5;temperature=34", or "normal"
#' @keywords internal
physiologyEventEncode <- function(x)
{
  x <- checkPhysiologyEvent(x)
  off <- x != physiologyEventNormal()
  if (!any(off)) return(PHYSIOLOGY_EVENT_NORMAL_TEXT)
  paste0(names(x)[off], "=", vapply(unname(x[off]), function(v) format(v, digits = 15), ""),
         collapse = ";")
}

#' The Settings string of a physiology event at normal physiology
#' @keywords internal
PHYSIOLOGY_EVENT_NORMAL_TEXT <- "normal"

#' An event table's Settings column, "" where it has none
#' @param ET an event table
#' @returns a character vector, one per row
#' @keywords internal
eventTableSettings <- function(ET)
{
  n <- if (is.data.frame(ET)) nrow(ET) else 0L
  s <- if (is.data.frame(ET) && "Settings" %in% names(ET)) ET[["Settings"]] else NULL
  if (is.null(s) || length(s) != n) return(rep("", n))
  s <- vapply(s, function(v) if (length(v) == 1 && !is.na(v)) as.character(v) else "", "",
              USE.NAMES = FALSE)
  s
}

#' An event table from its columns, Settings "" where not given
#' @param Time,Event,Settings the columns
#' @keywords internal
eventTableRows <- function(Time, Event, Settings = rep("", length(Time)))
{
  data.frame(Time = Time, Event = as.character(Event), Settings = as.character(Settings),
             stringsAsFactors = FALSE)
}

#' The problem with a Physiology row's settings, or NULL
#'
#' Every Physiology row must carry settings physiologyEventDecode() reads; the
#' other rows carry none.
#'
#' @param ET an event table
#' @returns a sentence, or NULL
#' @keywords internal
physiologyEventsProblem <- function(ET)
{
  if (!is.data.frame(ET) || nrow(ET) == 0) return(NULL)
  settings <- eventTableSettings(ET)
  physiology <- as.character(ET$Event) == PHYSIOLOGY_EVENT_NAME
  physiology[is.na(physiology)] <- FALSE
  if (any(nchar(settings) > PHYSIOLOGY_EVENT_MAX_CHARS))
    return("An event's settings are too long.")
  if (sum(physiology) > PHYSIOLOGY_EVENT_MAX)
    return(sprintf("The event table holds at most %d physiology events.", PHYSIOLOGY_EVENT_MAX))
  unreadable <- physiology & vapply(settings, function(x) is.null(physiologyEventDecode(x)), TRUE)
  if (any(unreadable))
    return(paste("A Physiology event records the physiology panel's settings: create it with",
                 "Create event in the Physiology panel."))
  NULL
}

#' The longest Settings string read
#' @keywords internal
PHYSIOLOGY_EVENT_MAX_CHARS <- 400L

#' A Settings string read back
#'
#' Untrusted (a link carries it): anything but the fields, each once, each a
#' number in range, gives NULL.
#'
#' @param s the string
#' @returns the settings, as checkPhysiologyEvent() returns them, or NULL
#' @keywords internal
physiologyEventDecode <- function(s)
{
  if (!is.character(s) || length(s) != 1 || is.na(s) || nchar(s) > PHYSIOLOGY_EVENT_MAX_CHARS)
    return(NULL)
  if (identical(trimws(s), PHYSIOLOGY_EVENT_NORMAL_TEXT)) return(physiologyEventNormal())
  # strsplit() drops an empty last field, so "cardiac=0.5;" read as valid
  # (ChatGPT's review of PR #796, finding 10)
  if (!nzchar(s) || grepl("^;|;;|;$", s)) return(NULL)
  parts <- strsplit(s, ";", fixed = TRUE)[[1]]
  kv <- strsplit(parts, "=", fixed = TRUE)
  if (length(kv) == 0 || any(lengths(kv) != 2)) return(NULL)
  keys <- vapply(kv, `[[`, character(1), 1)
  values <- suppressWarnings(as.numeric(vapply(kv, `[[`, character(1), 2)))
  if (anyNA(values) || any(!is.finite(values))) return(NULL)
  tryCatch(checkPhysiologyEvent(stats::setNames(values, keys)), error = function(e) NULL)
}

#' A physiology event's settings in words, for the hover tip
#' @param x settings
#' @param gas whether to include the altitude and the intubation, which act
#'   on the inhaled agents only
#' @returns one string
#' @keywords internal
physiologyEventSummary <- function(x, gas = TRUE)
{
  x <- checkPhysiologyEvent(x)
  mult <- function(v) if (v == 1) "x1" else paste0("x", signif(v, 3))
  parts <- c(sprintf("Liver %s", mult(x[["liver"]])),
             sprintf("Kidney %s", mult(x[["renal"]])),
             sprintf("Cardiac output %s", mult(x[["cardiac"]])),
             sprintf("GI %s", mult(x[["gi"]])),
             sprintf("Temperature %s C", format(x[["temperature"]])))
  if (gas)
    parts <- c(parts,
               if (x[["altitude"]] > 0)
                 sprintf("Altitude %s m (%.0f Torr)", format(x[["altitude"]]),
                         barometricPressure(x[["altitude"]]))
               else "Sea level",
               if (x[["endobronchial"]] == 1)
                 sprintf("Endobronchial intubation, haemoglobin %s g/dL", format(x[["haemoglobin"]]))
               else "Tracheal intubation")
  paste(parts, collapse = ", ")
}

#' The physiology events of an event table
#'
#' @param ET an event table (Time in minutes, Event, and Settings)
#' @returns a data frame of Time and the PHYSIOLOGY_EVENT_FIELDS, one row per
#'   readable Physiology row in time order (the last of a tie in force, as the
#'   engine takes clinical events), at most PHYSIOLOGY_EVENT_MAX; zero rows when
#'   there are none
#' @keywords internal
physiologyEventsFrom <- function(ET)
{
  empty <- data.frame(Time = numeric(0))
  for (f in PHYSIOLOGY_EVENT_FIELDS) empty[[f]] <- numeric(0)
  if (is.null(ET) || !is.data.frame(ET) || nrow(ET) == 0 ||
      is.null(ET$Settings) || is.null(ET$Event)) return(empty)
  rows <- which(as.character(ET$Event) == PHYSIOLOGY_EVENT_NAME)
  if (length(rows) == 0) return(empty)
  time <- suppressWarnings(as.numeric(as.character(ET$Time[rows])))
  settings <- lapply(as.character(ET$Settings[rows]), physiologyEventDecode)
  ok <- !is.na(time) & is.finite(time) & time >= 0 & !vapply(settings, is.null, logical(1))
  if (!any(ok)) return(empty)
  out <- data.frame(Time = time[ok])
  m <- do.call(rbind, settings[ok])
  for (f in PHYSIOLOGY_EVENT_FIELDS) out[[f]] <- unname(m[, f])
  out <- out[order(out$Time), , drop = FALSE]
  # A tie: the later row is in force from that time (and the earlier one for
  # no time at all), so keep only it
  out <- out[!duplicated(out$Time, fromLast = TRUE), , drop = FALSE]
  rownames(out) <- NULL
  utils::head(out, PHYSIOLOGY_EVENT_MAX)
}

#' The physiology states of a set of events: normal, then each event's
#' @param events physiologyEventsFrom()'s data frame
#' @returns a list of settings vectors, the first normal
#' @keywords internal
physiologyEventStates <- function(events)
{
  c(list(physiologyEventNormal()),
    lapply(seq_len(nrow(events)), function(i)
      unlist(events[i, PHYSIOLOGY_EVENT_FIELDS, drop = TRUE])))
}

#' The key of a clinical event's PK set in a physiology state
#'
#' The normal state keeps the clinical name, so "default" is still the set
#' in force at time 0.
#'
#' @param event clinical PK set names
#' @param state the state index, 0 for normal
#' @keywords internal
physiologyEventKey <- function(event, state)
{
  if (state == PHYSIOLOGY_STATE_NORMAL) event else paste0(event, "@physiology", state)
}

#' Why a drug's PK cannot take physiology events, or NULL if it can
#' @param PK a drug's resolved PK (getDrugPK())
#' @keywords internal
physiologyEventsBlocked <- function(PK)
{
  if (is.null(physiologyRow(pkModelDrug(PK))))
    return("the physiology sliders do not change it")
  # The event engine switches linear PK sets only (simCpCe()): a drug on
  # another engine, or one that forms a metabolite the event engine does not
  # carry, cannot switch during a run.  (A model group whose chosen candidate
  # the sliders cannot scale builds the same sets at every state, so it
  # switches to identical sets, which changes nothing.)
  if (!is.null(PK$michaelisMenten) || !is.null(PK$parallelSystems) ||
      isTRUE(PK$doseDependent) || !is.null(PK$dosePhase) || !is.null(PK$courseSteps) ||
      !is.null(PK$transitChain) || !is.null(PK$biliaryStore) || !is.null(PK$inr) ||
      !is.null(PK$act) || !is.null(PK$metaboliteName))
    return("its model cannot switch physiology during a run")
  NULL
}

#' A drug built once per physiology state, its sets merged
#'
#' @param build function(physiology) returning the drug's resolved PK at the
#'   physiology factors given (getDrugPK()'s `physiology`)
#' @param events physiologyEventsFrom()'s data frame, at least one row
#' @returns the PK at normal physiology with every state's sets merged in
#'   (physiologyEventKey()), `physiologyEvents` (Time and state, from 1), or,
#'   for a drug that cannot take them, the PK at normal physiology with
#'   `physiologyStepsSkipped` saying why
#' @keywords internal
physiologyEventPK <- function(build, events)
{
  states <- physiologyEventStates(events)
  base <- build(states[[1]][names(PHYSIOLOGY_DEFAULT)])
  blocked <- physiologyEventsBlocked(base)
  if (!is.null(blocked)) {
    base$physiologyStepsSkipped <- blocked
    return(base)
  }
  merged <- base$PK
  formulations <- base$oralFormulations
  clinical <- names(base$PK)
  for (k in seq_along(states)[-1]) {
    state <- k - 1L
    pk <- build(states[[k]][names(PHYSIOLOGY_DEFAULT)])
    if (!identical(names(pk$PK), clinical))
      stop("Internal error: ", base$drug, " built different PK sets at another physiology.")
    for (e in clinical) merged[[physiologyEventKey(e, state)]] <- pk$PK[[e]]
    for (f in names(formulations))
      for (e in clinical)
        formulations[[f]][[physiologyEventKey(e, state)]] <- pk$oralFormulations[[f]][[e]]
  }
  base$PK <- merged
  if (!is.null(formulations)) base$oralFormulations <- formulations
  base$physiologyEvents <- data.frame(Time = events$Time,
                                      State = seq_len(nrow(events)))
  base
}

#' The clinical and physiology events on one time line
#'
#' @param clinical the drug's clinical events (Time, Event; PK set names),
#'   in time order, possibly none
#' @param physiology the drug's `physiologyEvents` (Time, State)
#' @returns a data frame of Time and Event, the merged set in force from each
#'   time, in time order; a time at which both change gives one row
#' @keywords internal
physiologyEventTimeline <- function(clinical, physiology)
{
  times <- sort(unique(c(0, clinical$Time, physiology$Time)))
  clinicalAt <- function(t) {
    i <- which(clinical$Time <= t)
    if (length(i) == 0) PK_EVENT_DEFAULT else as.character(clinical$Event[max(i)])
  }
  stateAt <- function(t) {
    i <- which(physiology$Time <= t)
    if (length(i) == 0) PHYSIOLOGY_STATE_NORMAL else physiology$State[max(i)]
  }
  data.frame(Time = times,
             Event = vapply(times, function(t) physiologyEventKey(clinicalAt(t), stateAt(t)),
                            character(1)),
             stringsAsFactors = FALSE)
}

#' The Physiology event under the pointer on the Events panel
#'
#' @param ET the event table (minutes)
#' @param x the pointer's time, minutes
#' @param span the plot's width, minutes; an event within
#'   PHYSIOLOGY_EVENT_HOVER_SHARE of it is under the pointer
#' @param gas whether to describe the altitude and the intubation
#' @returns list(time, text) for the nearest such event, or NULL
#' @keywords internal
physiologyEventHover <- function(ET, x, span, gas = TRUE)
{
  events <- physiologyEventsFrom(ET)
  if (nrow(events) == 0 || length(x) != 1 || !is.finite(x) || !isTRUE(span > 0)) return(NULL)
  d <- abs(events$Time - x)
  i <- which.min(d)
  if (d[i] > PHYSIOLOGY_EVENT_HOVER_SHARE * span) return(NULL)
  list(time = events$Time[i],
       text = physiologyEventSummary(unlist(events[i, PHYSIOLOGY_EVENT_FIELDS]), gas = gas))
}

#' Share of the plot's width within which the pointer is on an event
#' @keywords internal
PHYSIOLOGY_EVENT_HOVER_SHARE <- 0.02

#' The PK a TCI pump plans on, with physiology events
#'
#' The controller plans on the first PK set (tciSchedule()).  With a
#' physiology event at time 0 the patient is in that event's physiology from
#' the start, so the pump plans on it, as it plans on the sliders' physiology
#' without events; a later event is one the pump does not know of.
#'
#' @param PK the pump's PK (the drug's, or its tciModel)
#' @returns the PK with the set in force at time 0 first
#' @keywords internal
physiologyEventPump <- function(PK)
{
  events <- PK[["physiologyEvents"]]
  if (!is.data.frame(events) || nrow(events) == 0) return(PK)
  atZero <- events$State[events$Time <= 0]
  if (length(atZero) == 0) return(PK)
  key <- physiologyEventKey(names(PK$PK)[1], max(atZero))
  if (is.null(PK$PK[[key]])) return(PK)
  PK$PK <- c(PK$PK[key], PK$PK[setdiff(names(PK$PK), key)])
  PK
}

#' A series read at a time, a repeated time read after its step
#'
#' A physiology event, a dose or a clinical event repeats its time in a
#' series, before and after the step.  Linear interpolation between the
#' last point at or before x and the next, so a point at a step's time reads
#' its value after the step, and a point just before it reads towards the
#' value before it (ChatGPT's review of PR #796, findings 5 and 6: R's approx()
#' averaged the two values, and dropping the earlier made the time before a
#' step read towards the value after it).
#'
#' @param t,y the series, in time order (ties in the order they occur)
#' @param x times to read
#' @returns the values at x, held at the ends; NA where either neighbour is NA
#' @keywords internal
seriesStepAt <- function(t, y, x)
{
  # A missing value stays missing next to it (approx(na.rm = FALSE), which
  # this replaced in seriesAt(): a time until threshold still pending)
  ok <- !is.na(t)
  t <- t[ok]
  y <- y[ok]
  n <- length(t)
  if (n == 0) return(rep(NA_real_, length(x)))
  if (n == 1) return(rep(y, length(x)))
  o <- order(t, seq_along(t))
  t <- t[o]
  y <- y[o]
  i <- findInterval(x, t)
  out <- numeric(length(x))
  lo <- i < 1
  hi <- i >= n
  out[lo] <- y[1]
  out[hi] <- y[n]
  mid <- !lo & !hi
  j <- i[mid]
  w <- (x[mid] - t[j]) / (t[j + 1] - t[j])
  # On a point, its own value, whatever the next one is
  out[mid] <- ifelse(w == 0, y[j], y[j] + w * (y[j + 1] - y[j]))
  out
}
