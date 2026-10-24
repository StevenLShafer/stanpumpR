# Scheduled (repeating) doses: qd, bid, tid and qid, and qwk (once weekly) on
# the subcutaneous units
#
# Drafted by Claude Code, 2026-10-07, at the request of Steven L. Shafer;
# tests/testthat/test-scheduled.R.
#
# A dose-table row whose unit ends in a frequency ("mg PO bid", "g qd",
# "mg/kg IM qid") gives the dose at the entered time and then repeats it every
# interval -- qd 24 h, bid 12 h, tid 8 h, qid 6 h; see SCHEDULE_INTERVALS --
# until the end of the X axis.  As with TCI, the repeats are kept out of the
# dose table the user edits, which would otherwise fill with rows that move
# whenever the X axis is lengthened; they are returned as a table of their own
# and merged back into the exported dose table.
#
# Rules from the specification (Shafer, 2026-10-07):
#   - The first dose is given at the time entered.
#   - A scheduled dose of 0 for the same route (IV, PO, SL, IM or IN), at any
#     frequency, stops the repeating sequence.  An ordinary (unscheduled) dose
#     does not.
#   - A later non-zero scheduled dose for the same route replaces the running
#     sequence from its own time, so a change of dose or frequency is entered
#     as one new row.  Two scheduled rows for one route at the same time: the
#     last one entered wins, as for TCI targets.
#   - Oral doses (Shafer, 2026-10-24): ANY later oral dose -- scheduled or not,
#     of any oral form, of any size, 0 included -- ends a running oral
#     schedule from its own time.  So a bid row entered while a tid schedule
#     runs replaces it, and an ordinary "0 mg PO" stops it: on oral dosing a
#     new dose is a new instruction to the patient.  An oral dose at the same
#     time as a schedule's first dose (a loading dose taken with it) does not
#     end it.  The other routes keep the rules above.
#   - Sublingual doses (Shafer, 2026-10-24) follow the same rule, and the two
#     routes are one for it: ANY later oral or sublingual dose ends a running
#     oral or sublingual schedule.  A tablet swallowed or under the tongue is
#     a new instruction about the tablets, so an ordinary "0 mg SL" stops a
#     sublingual schedule and an extra sublingual dose ends it.
#   - A repeat within SCHEDULE_TIME_TOLERANCE of the row that ends its
#     schedule is not given: that row replaces it.  A time written in hours or
#     days is read back from the dose table rounded to 0.001 minute, so a row
#     written at a repeat's time can read a hair after it.
#   - "Skip next dose" (the plot's dose dialogs; skipNextScheduledDose() below)
#     writes those rules into the table, on any route: a 0 dose that stops
#     the schedule at the skipped time and the schedule entered again one
#     interval later.

isScheduledUnit <- function(units) as.character(units) %in% scheduledUnits

# "mg PO bid" -> "mg PO"
scheduleBaseUnit <- function(units) {
  sub(paste0(" (", paste(names(ALL_SCHEDULE_INTERVALS), collapse = "|"), ")$"), "", units)
}

# "mg PO bid" -> 720
scheduleInterval <- function(units) {
  unname(ALL_SCHEDULE_INTERVALS[sub("^.* ", "", units)])
}

# The running schedules in one drug's rows: one row per non-zero scheduled
# row not overridden at its own time, with the time its schedule ends.
#
# dose: that drug's rows of the dose table (numeric Time in minutes).
#
# Returns list(sequences, dropped): sequences is data.frame(row, route,
# start, stop, interval), stop Inf when nothing ends it; dropped the rows
# that lose to a later-entered scheduled row for the same route and time.
scheduledSequences <- function(dose) {
  empty <- data.frame(row = integer(), route = character(), start = double(),
                      stop = double(), interval = double())
  isSched <- isScheduledUnit(dose$Units)
  if (!any(isSched)) return(list(sequences = empty, dropped = integer()))

  rows <- which(isSched)
  route <- doseRoute(dose$Units[rows])
  time <- as.numeric(dose$Time[rows])

  # Same route, same time: the last one entered wins; the others are dropped
  # entirely, first dose included.
  loser <- duplicated(data.frame(route, time), fromLast = TRUE)
  dropped <- rows[loser]
  rows <- rows[!loser]; route <- route[!loser]; time <- time[!loser]

  # What ends a schedule: the next scheduled row for its route or, on the
  # oral and sublingual routes, the next oral or sublingual dose of any kind.
  # An oral rate unit (digoxin's and amiodarone's constant-rate "mg/day PO")
  # is not a dose, and nor is a row whose time cannot be read (simCpCe()
  # keeps one).
  allTimes <- suppressWarnings(as.numeric(dose$Time))
  tablets <- setdiff(which(doseRoute(dose$Units) %in% c(ROUTE_PO, ROUTE_SL) &
                            !isRateUnit(dose$Units) & !is.na(allTimes)), dropped)

  out <- lapply(seq_along(rows), function(i) {
    row <- rows[i]
    if (dose$Dose[row] <= 0) return(NULL)
    enders <- if (route[i] %in% c(ROUTE_PO, ROUTE_SL)) allTimes[tablets] else time[route == route[i]]
    later <- enders[enders > time[i]]
    data.frame(row = row, route = route[i], start = time[i],
               stop = if (length(later)) min(later) else Inf,
               interval = scheduleInterval(dose$Units[row]))
  })
  out <- do.call(rbind, Filter(Negate(is.null), out))
  list(sequences = if (is.null(out)) empty else out, dropped = dropped)
}

# The repeat times of one schedule: every interval after its start, before
# the row that ends it and before `maximum` (no dose is given at or after it).
sequenceRepeats <- function(start, stop, interval, maximum) {
  stop <- min(stop - SCHEDULE_TIME_TOLERANCE, maximum)
  times <- start + interval * seq_len(max(0, ceiling((stop - start) / interval)))
  times[times < stop]
}

# Expand one drug's scheduled rows.
#
# dose:    that drug's rows of the dose table, as typed (user units, numeric
#          Time in minutes).
# maximum: end of the simulation (min); no dose is given at or after it.
#
# Returns a list:
#   dose      the dose table to simulate: scheduled units replaced by their
#             base unit (so the first dose is an ordinary dose) and every
#             repeat appended as an ordinary row
#   scheduled data.frame(Drug, Time, Dose, Units) of the repeats only, in the
#             user's (base) unit, or NULL if there are none
expandScheduledDoses <- function(dose, maximum) {
  if (!any(isScheduledUnit(dose$Units))) return(list(dose = dose, scheduled = NULL))

  found <- scheduledSequences(dose)
  sequences <- found$sequences
  repeats <- list()
  for (i in seq_len(nrow(sequences))) {
    s <- sequences[i, ]
    times <- sequenceRepeats(s$start, s$stop, s$interval, maximum)
    if (length(times) == 0) next
    extra <- dose[rep(s$row, length(times)), , drop = FALSE]
    extra$Time <- times
    repeats[[length(repeats) + 1]] <- extra
  }

  keep <- dose[setdiff(seq_len(nrow(dose)), found$dropped), , drop = FALSE]
  repeats <- do.call(rbind, repeats)
  out <- rbind(keep, repeats)
  out$Units <- scheduleBaseUnit(as.character(out$Units))
  rownames(out) <- NULL

  scheduled <- NULL
  if (!is.null(repeats) && nrow(repeats) > 0) {
    scheduled <- data.frame(
      Drug  = as.character(repeats$Drug),
      Time  = as.numeric(repeats$Time),
      Dose  = as.numeric(repeats$Dose),
      Units = scheduleBaseUnit(as.character(repeats$Units))
    )
    scheduled <- scheduled[order(scheduled$Time), ]
    rownames(scheduled) <- NULL
  }
  list(dose = out, scheduled = scheduled)
}

# The dose table as exported: the rows the user typed, then every drug's TCI
# infusion rows and repeated scheduled doses, so that the full dose sequence
# appears.  Time is in minutes.  With a `timeUnit` other than minutes (the
# plot's display unit) a "Time (<unit>)" column follows it with the same
# times in that unit (addDisplayTimeColumn()); the default changes nothing.
exportDoseTable <- function(DT, drugs, timeUnit = "minutes") {
  out <- tciMergeDoseTable(DT, drugs)
  if (is.null(out)) return(out)
  # A companion entry (R/alternativeModels.R) repeats its parent's scheduled
  # doses; its TCI rows, which differ from the parent's, are merged above.
  extra <- lapply(drugs, function(d) if (isAlternativeModelEntry(d)) NULL else d$scheduled)
  extra <- do.call(rbind, Filter(Negate(is.null), extra))
  if (!is.null(extra) && nrow(extra) > 0) {
    out <- out[, c("Drug", "Time", "Dose", "Units")]
    out$Time <- as.numeric(out$Time)
    out <- rbind(out, extra)
    out <- out[order(out$Drug, out$Time), ]
    rownames(out) <- NULL
  }
  addDisplayTimeColumn(out, timeUnit)
}

# "Skip next dose": the patient misses the next dose of a running schedule
# (a tablet forgotten or vomited, an injection or a spray missed), and the
# schedule then carries on as before.  Every route that has schedules: the
# intravenous bolus, PO, SL, IM, IN and SC.
#
# Requested by Steven L. Shafer, 2026-10-24, for the plot's dose dialogs:
# a patient on an antiseizure drug who misses a dose wants to know whether to
# wait for the next one or take two.  Drafted by Claude Code;
# tests/testthat/test-scheduled.R.
#
# Nothing new reaches the engine: the skip is written into the dose table by
# the rules above, where the user can see it and undo it -- a 0 dose of the
# schedule's route at the skipped time, which ends the schedule, and the
# scheduled row entered again one interval later, which restarts it.  When
# the skipped dose is the row's own first dose, that row becomes the 0 dose.
# No restart is written when the schedule would have ended before it.  The 0
# dose is an ordinary one on the oral and sublingual routes, which any later
# dose ends, and a scheduled 0 on every other route, where an ordinary dose
# of 0 does not stop a schedule.
#
# dt:        the dose table as the app holds it (Time strings in `format`,
#            blank rows allowed)
# drug:      the drug clicked
# time:      the time clicked (minutes); the dose skipped is the drug's first
#            scheduled dose at or after it, on any route
# maximum:   the end of the plot (minutes); a dose at or after it is not
#            offered
# format, reference: the dose table's time format and the procedure start
#            (minutesToEntryTime())
# units:     the units the drug offers; on the oral and sublingual routes
#            the 0 dose is written in the schedule's base unit ("mg PO",
#            "mg SL") when the drug offers it, else in the scheduled unit, as
#            it always is on the other routes ("0 mg IM qid", "0 mg SC qwk")
#
# Returns NULL when there is no such dose, else list(table, time, resumes,
# dose, units): the new table, the time of the dose skipped, the time the
# schedule resumes (NA if it does not), and the schedule's dose and unit.
skipNextScheduledDose <- function(dt, drug, time, maximum, format,
                             reference = REFERENCE_TIME_NONE, units = NULL) {
  rows <- which(as.character(dt$Drug) == drug)
  if (length(rows) == 0) return(NULL)
  d <- data.frame(
    Time  = displayTimeToMinutes(dt$Time[rows], referenceForFormat(format, reference), format[["unit"]]),
    Dose  = suppressWarnings(as.numeric(as.character(dt$Dose[rows]))),
    Units = as.character(dt$Units[rows])
  )
  ok <- !is.na(d$Time) & !is.na(d$Dose) & !is.na(d$Units) & nzchar(d$Units)
  rows <- rows[ok]; d <- d[ok, , drop = FALSE]
  sequences <- scheduledSequences(d)$sequences
  if (nrow(sequences) == 0) return(NULL)

  # The first dose of each schedule at or after the click
  nextDose <- vapply(seq_len(nrow(sequences)), function(i) {
    s <- sequences[i, ]
    times <- c(s$start, sequenceRepeats(s$start, s$stop, s$interval, maximum))
    times <- times[times >= time & times < maximum]
    if (length(times)) min(times) else NA_real_
  }, numeric(1))
  if (all(is.na(nextDose))) return(NULL)
  i <- which.min(nextDose)
  s <- sequences[i, ]
  skipped <- nextDose[i]
  r <- rows[s$row]

  scheduledUnit <- as.character(dt$Units[r])
  stopUnit <- scheduleBaseUnit(scheduledUnit)
  if (!s$route %in% c(ROUTE_PO, ROUTE_SL) || (!is.null(units) && !stopUnit %in% units)) {
    stopUnit <- scheduledUnit
  }
  resumes <- skipped + s$interval
  if (resumes >= s$stop - SCHEDULE_TIME_TOLERANCE) resumes <- NA_real_

  newRow <- function(t, dose, unit) {
    x <- dt[r, , drop = FALSE]
    x$Time <- minutesToEntryTime(t, format, reference)
    x$Dose <- dose
    x$Units <- unit
    x
  }
  dose <- as.character(dt$Dose[r])
  added <- NULL
  if (abs(skipped - s$start) < SCHEDULE_TIME_TOLERANCE) {
    dt$Dose[r] <- "0"
    dt$Units[r] <- stopUnit
  } else {
    added <- newRow(skipped, "0", stopUnit)
  }
  if (!is.na(resumes)) added <- rbind(added, newRow(resumes, dose, scheduledUnit))

  # The new rows go under the row whose schedule they change
  table <- rbind(dt[seq_len(r), , drop = FALSE], added,
                 dt[-seq_len(r), , drop = FALSE])
  rownames(table) <- NULL
  list(table = table, time = skipped, resumes = resumes,
       dose = as.numeric(dose), units = scheduledUnit)
}
