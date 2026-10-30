# "Stop": ending whatever administration of a drug is running
#
# Drafted by Claude Code, 2026-10-24, at the request of Steven L. Shafer;
# tests/testthat/test-stop-administration.R.
#
# An infusion, a TCI target, a repeating (scheduled) dose and an inhaled
# agent's dial setting all carry on until something in the dose table ends
# them, and it was not obvious what.  The plot's dose dialogs therefore offer
# a Stop button while the clicked drug has any of them running at the time
# clicked.  It writes nothing new: only the 0 row the existing rules already
# read as a stop, at the time clicked, so the simulation needs no new logic.
#
#   - An infusion (a rate unit, any route: "mcg/kg/min", "mg/day PO",
#     "U/hr SC"): the rate in force is the sum of the rate rows at the latest
#     time with any, and a 0 rate row ends it (doseLines()).
#   - A TCI target: a target of 0 stops the pump (R/tci.R).  A manual rate and
#     a target replace each other, so on the intravenous route the latest of
#     the two is what is running; at one time the target wins.
#   - A repeating dose: a scheduled dose of 0 for its route, at any frequency,
#     stops the sequence (R/scheduled.R).  The 0 is written in the schedule's
#     own unit, which stops it on every route; on the oral route any oral
#     dose would.
#   - An inhaled agent: the vaporizer (%) or flowmeter (L/min) setting holds
#     until the next row, and a 0 turns it off.

# What is running for one drug at `time`.
#
# dt:        the app's dose table (character columns, times as typed)
# drug:      the drug clicked
# time:      the time clicked, minutes
# maximum:   end of the plot, minutes; a schedule with no repeat left before
#            it has nothing to stop
# format, reference: how the table's Time strings are read (R/utils-time.R)
#
# Returns a data.frame of kind ("infusion", "tci", "schedule", "setting"),
# units (the unit the 0 row is written in) and label (what is stopped, in
# words), one row per thing running; no rows when nothing is.
ongoingAdministration <- function(dt, drug, time, maximum, format,
                                  reference = REFERENCE_TIME_NONE) {
  none <- data.frame(kind = character(), units = character(), label = character())
  rows <- which(as.character(dt$Drug) == drug)
  if (length(rows) == 0) return(none)
  d <- data.frame(
    Time  = displayTimeToMinutes(dt$Time[rows], referenceForFormat(format, reference), format[["unit"]]),
    Dose  = suppressWarnings(as.numeric(as.character(dt$Dose[rows]))),
    Units = as.character(dt$Units[rows])
  )
  ok <- !is.na(d$Time) & !is.na(d$Dose) & !is.na(d$Units) & nzchar(d$Units)
  d <- d[ok, , drop = FALSE]
  if (nrow(d) == 0 || !isTRUE(time < maximum)) return(none)

  # The rows at the latest time before `time` among `use`, in table order
  latest <- function(use) {
    use <- which(use & d$Time < time)
    if (length(use) == 0) return(NULL)
    use[d$Time[use] == max(d$Time[use])]
  }

  out <- none
  add <- function(kind, units, label) {
    out <<- rbind(out, data.frame(kind = kind, units = units, label = label))
  }

  if (isGasDrug(drug)) {
    last <- latest(rep(TRUE, nrow(d)))
    if (!is.null(last) && sum(d$Dose[last]) > 0) {
      units <- d$Units[last[length(last)]]
      add("setting", units, if (units == "%") "the vaporizer" else "the flow")
    }
    return(out)
  }

  continuous <- isRateUnit(d$Units) | isTciUnit(d$Units)
  route <- doseRoute(d$Units)
  for (r in unique(route[continuous])) {
    last <- latest(continuous & route == r)
    if (is.null(last)) next
    target <- last[isTciUnit(d$Units[last])]
    if (length(target) > 0) {
      # A target and a rate at one time: the target wins, and of two targets
      # the last one entered
      t <- target[length(target)]
      if (d$Dose[t] > 0) add("tci", d$Units[t], "the target-controlled infusion")
    } else if (sum(d$Dose[last]) > 0) {
      units <- d$Units[last[length(last)]]
      add("infusion", units, paste0("the infusion (", units, ")"))
    }
  }

  sequences <- scheduledSequences(d)$sequences
  for (i in seq_len(nrow(sequences))) {
    s <- sequences[i, ]
    if (!(s$start < time && time < s$stop)) next
    ahead <- sequenceRepeats(s$start, s$stop, s$interval, maximum)
    if (!any(ahead >= time)) next
    units <- d$Units[s$row]
    add("schedule", units, paste0("the repeated doses (", units, ")"))
  }
  out
}

# The dose table with a 0 row at `time` for everything ongoingAdministration()
# finds running, in its unit, filling the table's blank rows first, as the
# Add a dose dialog does.  Returns NULL when nothing is running, else
# list(table, stopped), stopped being ongoingAdministration()'s rows.
stopAdministration <- function(dt, drug, time, maximum, format,
                               reference = REFERENCE_TIME_NONE) {
  stopped <- ongoingAdministration(dt, drug, time, maximum, format, reference)
  if (nrow(stopped) == 0) return(NULL)
  when <- minutesToEntryTime(time, format, reference)
  for (units in unique(stopped$units)) {
    blank <- which(as.character(dt$Drug) == "")
    if (length(blank) == 0) {
      dt <- rbind(dt, doseTableNewRow)
      blank <- nrow(dt)
    }
    i <- blank[1]
    dt$Drug[i]  <- drug
    dt$Time[i]  <- when
    dt$Dose[i]  <- "0"
    dt$Units[i] <- units
  }
  if (as.character(dt$Drug[nrow(dt)]) != "") dt <- rbind(dt, doseTableNewRow)
  rownames(dt) <- NULL
  list(table = dt, stopped = stopped)
}

# "the infusion (mg/hr) and the repeated doses (mg PO bid)"
stoppedText <- function(labels) {
  if (length(labels) <= 1) return(paste(labels, collapse = ""))
  paste(paste(labels[-length(labels)], collapse = ", "), "and", labels[length(labels)])
}
