# Scheduled (repeating) doses: qd, bid, tid, qid and q4h; qam and qpm (separate
# morning/evening slots, oral and sublingual, the antiseizure drugs); and qwk
# (once weekly) on the subcutaneous units
#
# Drafted by Claude Code, 2026-10-07, at the request of Steven L. Shafer;
# tests/testthat/test-scheduled.R.
#
# A dose-table row whose unit ends in a frequency ("mg PO bid", "g qd",
# "mg/kg IM qid") gives the dose at the entered time and then repeats it every
# interval -- qd 24 h, bid 12 h, tid 8 h, qid 6 h, q4h 4 h; see SCHEDULE_INTERVALS --
# until the end of the X axis.  As with TCI, the repeats are kept out of the
# dose table the user edits, which would otherwise fill with rows that move
# whenever the X axis is lengthened; they are returned as a table of their own
# and merged back into the exported dose table.
#
# Rules (Shafer, 2026-10-07; oral rules revised 2026-10-28):
#   - The first dose is given at the time entered.
#   - qd/bid/tid/qid/q4h are WHOLE-DAY schedules.  A later whole-day scheduled
#     dose for the same route replaces the running one from its own time, so a
#     change of dose or frequency is one new row; on the oral and sublingual
#     routes (counted as one) a whole-day dose is a regimen change that ends
#     EVERY running oral schedule, the qam/qpm slots included.  A scheduled 0
#     ("0 mg PO bid", "0 mg IM qid") stops the schedule.
#   - qam and qpm are two independent once-daily slots, oral and sublingual
#     only, that DO NOT replace each other: a split regimen such as cenobamate
#     200 qpm + 50 qam is entered as two rows (the antiseizure drugs are
#     sedating and affect memory, so the morning dose is often the smaller).
#     A later qam replaces only the running qam, a later qpm only the running
#     qpm; "0 mg PO qam" stops the qam slot alone.
#   - Ordinary (unscheduled) oral and sublingual doses (Shafer, 2026-10-28):
#     an ordinary dose of 0 ("0 mg PO", "0 mg SL") stops EVERY running oral
#     schedule from its time; an ordinary dose ABOVE 0 is an extra one-off
#     dose and leaves the schedule running -- a catch-up or breakthrough dose,
#     not a new instruction.  An oral rate unit ("mg/day PO") is not a dose.
#   - On the other routes (IV bolus, IM, IN, SC) an ordinary dose beside a
#     schedule adds to it and an ordinary 0 does not stop it; only a scheduled
#     0 for that route stops it, and a later scheduled dose for that route
#     replaces it.  Two scheduled rows in one lane at the same time: the last
#     entered wins, as for TCI targets.
#   - A repeat within SCHEDULE_TIME_TOLERANCE of the row that ends its
#     schedule is not given: that row replaces it.  A time written in hours or
#     days is read back from the dose table rounded to 0.001 minute, so a row
#     written at a repeat's time can read a hair after it.
#   - "Skip next dose" (the plot's dose dialogs; skipNextScheduledDose() below)
#     writes those rules into the table: a scheduled 0 in the schedule's own
#     unit, which stops that one schedule (the qam slot, the oral regimen, or
#     the route) at the skipped time, and the schedule entered again one
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

# "mg PO qam" -> "qam"
scheduleFrequency <- function(units) sub("^.* ", "", as.character(units))

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
  freq <- scheduleFrequency(dose$Units[rows])
  slots <- names(SLOT_SCHEDULE_INTERVALS)             # c("qam", "qpm")
  oral <- route %in% c(ROUTE_PO, ROUTE_SL)

  # The "lane" a scheduled row occupies, for same-time replacement.  On the
  # oral and sublingual routes (counted as one) each slot (qam, qpm) is its
  # own lane and every whole-day frequency shares the lane "PO:regimen"; the
  # other routes keep the route as the lane.  Same lane, same time: the last
  # entered wins; the losers are dropped entirely, first dose included -- so a
  # qam and a qpm at the same time both stand.
  lane <- ifelse(oral,
                 ifelse(freq %in% slots, paste0("PO:", freq), "PO:regimen"),
                 route)
  loser <- duplicated(data.frame(lane, time), fromLast = TRUE)
  dropped <- rows[loser]
  rows <- rows[!loser]; route <- route[!loser]; time <- time[!loser]
  freq <- freq[!loser]; oral <- oral[!loser]

  # What ends an oral or sublingual schedule, from its own time onward:
  #   - any later whole-day scheduled oral/SL dose (a regimen change ends every
  #     oral schedule, the qam/qpm slots included);
  #   - any later ordinary oral/SL dose of 0 (a "0 mg PO" stops the regimen);
  #     an ordinary dose ABOVE 0 is an extra one-off and does NOT end it;
  #   - for a slot schedule, a later scheduled dose in the same slot.
  # An oral rate unit ("mg/day PO") is not a dose, and nor is a row whose time
  # cannot be read (simCpCe() keeps one).  Non-oral schedules keep the old
  # rule: the next scheduled dose for the same route ends them.
  allTimes <- suppressWarnings(as.numeric(dose$Time))
  allDose  <- suppressWarnings(as.numeric(dose$Dose))
  oralStop <- setdiff(which(doseRoute(dose$Units) %in% c(ROUTE_PO, ROUTE_SL) &
                            !isRateUnit(dose$Units) & !isScheduledUnit(dose$Units) &
                            !is.na(allTimes) & !is.na(allDose) & allDose <= 0), dropped)
  oralStopTimes <- allTimes[oralStop]
  oralWholeDayTimes <- time[oral & !(freq %in% slots)]

  out <- lapply(seq_along(rows), function(i) {
    row <- rows[i]
    if (dose$Dose[row] <= 0) return(NULL)
    if (oral[i]) {
      enders <- c(oralWholeDayTimes, oralStopTimes,
                  if (freq[i] %in% slots) time[oral & freq == freq[i]])
    } else {
      enders <- time[route == route[i]]
    }
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
# units:     the units the drug offers (kept for the caller's signature; the
#            stop is now always written in the schedule's own scheduled unit,
#            "0 mg PO qam", "0 mg PO bid", "0 mg IM qid", so it stops only the
#            one schedule's lane)
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

  # Stop this schedule in its own lane with a scheduled 0 in its own unit.  An
  # ordinary "0 mg PO" would now stop EVERY oral schedule, but a skip must stop
  # only the one being skipped (e.g. the qam slot), so the scheduled unit is
  # used on every route ("0 mg PO qam", "0 mg PO bid", "0 mg IM qid").
  scheduledUnit <- as.character(dt$Units[r])
  stopUnit <- scheduledUnit
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
