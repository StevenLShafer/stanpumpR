# Scheduled (repeating) doses: qd, bid, tid and qid
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
#   - A scheduled dose of 0 for the same route (IV, PO, IM or IN), at any
#     frequency, stops the repeating sequence.  An ordinary (unscheduled) dose
#     does not.
#   - A later non-zero scheduled dose for the same route replaces the running
#     sequence from its own time, so a change of dose or frequency is entered
#     as one new row.  Two scheduled rows for one route at the same time: the
#     last one entered wins, as for TCI targets.

isScheduledUnit <- function(units) as.character(units) %in% scheduledUnits

# "mg PO bid" -> "mg PO"
scheduleBaseUnit <- function(units) {
  sub(paste0(" (", paste(names(SCHEDULE_INTERVALS), collapse = "|"), ")$"), "", units)
}

# "mg PO bid" -> 720
scheduleInterval <- function(units) {
  unname(SCHEDULE_INTERVALS[sub("^.* ", "", units)])
}

# "mg PO bid" -> "PO"; "mg/kg bid" -> "IV"
scheduleRoute <- function(units) {
  base <- scheduleBaseUnit(units)
  route <- sub("^.* ", "", base)
  ifelse(route %in% c("PO", "IM", "IN"), route, "IV")
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
  isSched <- isScheduledUnit(dose$Units)
  if (!any(isSched)) return(list(dose = dose, scheduled = NULL))

  rows <- which(isSched)
  route <- scheduleRoute(dose$Units[rows])
  time <- as.numeric(dose$Time[rows])

  # Same route, same time: the last one entered wins; the others are dropped
  # entirely, first dose included.
  loser <- duplicated(data.frame(route, time), fromLast = TRUE)
  dropped <- rows[loser]
  rows <- rows[!loser]; route <- route[!loser]; time <- time[!loser]

  repeats <- list()
  for (r in unique(route)) {
    k <- rows[route == r]
    k <- k[order(as.numeric(dose$Time[k]))]
    for (i in seq_along(k)) {
      row <- k[i]
      if (dose$Dose[row] <= 0) next
      start <- as.numeric(dose$Time[row])
      stop  <- if (i < length(k)) as.numeric(dose$Time[k[i + 1]]) else Inf
      stop  <- min(stop, maximum)
      interval <- scheduleInterval(dose$Units[row])
      times <- start + interval * seq_len(max(0, ceiling((stop - start) / interval)))
      times <- times[times < stop]
      if (length(times) == 0) next
      extra <- dose[rep(row, length(times)), , drop = FALSE]
      extra$Time <- times
      repeats[[length(repeats) + 1]] <- extra
    }
  }

  keep <- dose[setdiff(seq_len(nrow(dose)), dropped), , drop = FALSE]
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
# appears.
exportDoseTable <- function(DT, drugs) {
  out <- tciMergeDoseTable(DT, drugs)
  if (is.null(out)) return(out)
  extra <- lapply(drugs, function(d) d$scheduled)
  extra <- do.call(rbind, Filter(Negate(is.null), extra))
  if (is.null(extra) || nrow(extra) == 0) return(out)
  out <- out[, c("Drug", "Time", "Dose", "Units")]
  out$Time <- as.numeric(out$Time)
  out <- rbind(out, extra)
  out <- out[order(out$Drug, out$Time), ]
  rownames(out) <- NULL
  out
}
