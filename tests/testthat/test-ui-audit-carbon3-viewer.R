# Carbon's (Gemini's) third UI audit of PR #730 at 4164743, 2026-11-15: the
# Visualizer's run file (R/viewerRun.R) and the debug log (R/app_globals.R).
# Each case reproduced before its fix:
#   H8  a metabolite formed only from a parent in the table (morphine from
#       codeine) was "re-solved" without the parent, which simulated nothing
#       and left the plot's own points, minutes apart, as the run; the
#       absorption was read off the fall of the Depot column where a rate
#       into the depot, or a share that passes it by, makes that fall
#       something else; and a model that switches PK sets at a clinical event
#       (the infant dexmedetomidine model on bypass) was balanced with the
#       first set's volumes throughout, drawing 23.5 mcg/min eliminated the
#       instant bypass began, while clearance had fallen 17-fold.
#   M7  a debug log written a line at a time copied the whole log once per
#       line of a table, and grew without bound.
# The viewer's side of each (tools/viewer/src) is tested in
# tools/viewer/tests/viewer-injection.test.mjs, with H2 (the pop-off valve
# drawn running backwards) and the rest of M7.
#
# Drafted by Claude Code, 2026-11-15, at the request of Steven L. Shafer.

NO_EVENTS <- data.frame(Time = double(), Event = character())

carbon3Plot <- function(doses, maximum, events = NO_EVENTS, age = 40, weight = 70, height = 170) {
  doses$Drug <- as.character(doses$Drug)
  doses$Units <- as.character(doses$Units)
  D <- recalculatePK(NULL, getDrugDefaultsGlobal(), doses,
                     age = age, weight = weight, height = height, sex = "male")
  processdoseTable(doses, events, D, maximum, FALSE)
}
carbon3Run <- function(doses, maximum, drug = as.character(doses$Drug[1]), events = NO_EVENTS,
                       minimum = 0, ...) {
  plot <- carbon3Plot(doses, maximum, events, ...)
  entry <- viewerPlotEntry(drug, plot[[drug]], doses, events, plot, minimum, maximum)
  list(entry = entry, run = viewerRun(entry, minimum, maximum), plot = plot)
}

test_that("H8: a metabolite formed only from its parent is re-solved with the parent", {
  doses <- data.frame(Drug = "codeine", Time = 0, Dose = 60, Units = "mg PO tablet")
  x <- carbon3Run(doses, 240, drug = "morphine")
  m <- x$plot$morphine
  expect_null(m$wideOwn)
  expect_true(viewerFormed(m))
  # Re-solved on the viewer's own times, not the plot's 33 points.
  times <- viewerTimes(0, 240)
  expect_true(x$entry$viewerResolved)
  expect_true(all(times %in% x$entry$wide$Time))
  expect_gt(nrow(x$entry$wide), nrow(m$wide))
  # The plot's own concentrations where it has them.
  w <- m$wide[m$wide$Time > 1 & m$wide$Time < 239, ]
  expect_equal(stats::approx(x$run$t / 60, x$run$cp, w$Time)$y, w$Plasma, tolerance = 1e-6)
  # Its input is its formation, which the dose rows do not show.
  expect_false(x$run$inputKnown)
  expect_null(x$run$given)
  # A drug with its own simulation and nothing formed is still not "formed".
  p <- carbon3Plot(data.frame(Drug = "propofol", Time = 0, Dose = 100, Units = "mg"), 30)
  expect_false(viewerFormed(p$propofol))
})

test_that("H8: absorption is drawn only where the fall of the depot is the absorption", {
  rows <- function(drug, dose, units) data.frame(Drug = drug, Time = 0, Dose = dose, Units = units)
  # Something fills the depot as it empties, or passes it by: no absorption.
  for (d in list(rows("lidocaine", c(100, 20), c("mg RA", "mg/hr RA")),
                 rows("lidocaine", 20, "mg/hr RA"),
                 rows("scopolamine", 0.333, "mg/day TD"),
                 rows("levodopaIntestinalGel", 40, "mg/hr PO"),
                 rows("insulinLispro", 10, "U SC"),
                 rows("canagliflozin", 100, "mg PO"))) {
    r <- carbon3Run(d, 600)$run
    label <- paste(d$Drug[1], paste(d$Units, collapse = " + "))
    expect_null(r$absorbed, label = label)
    expect_false(r$inputKnown, label = label)
    expect_false(is.null(r$depot), label = label)
  }
  # A dose into the depot alone: absorbed and still in the depot add up to
  # the bioavailable dose, so the arrow is drawn.
  for (d in list(rows("lidocaine", 100, "mg RA"), rows("levodopaIntestinalGel", 100, "mg PO"))) {
    r <- carbon3Run(d, 600)$run
    P <- getDrugPK(d$Drug, 70, 170, 40, "male", getDrugDefaults(d$Drug))$PK[[1]]
    F <- if (d$Drug == "lidocaine") P$bioavailability_RA else P$bioavailability_PO
    after <- r$t > 600
    expect_false(is.null(r$absorbed), label = d$Drug)
    expect_equal(r$absorbed[after] + r$depot[after], rep(F * d$Dose, sum(after)), tolerance = 1e-3,
                 label = d$Drug)
  }
})

test_that("H8: a model that switches PK sets at an event is balanced with the volumes in force", {
  # The infant bypass dexmedetomidine model (age 0.5 y, 7 kg) on bypass from 30 to 90 min.
  ev <- data.frame(Time = c(30, 90), Event = c("CPB Start", "CPB End"))
  doses <- data.frame(Drug = "dexmedetomidineInfantCPB", Time = 0, Dose = c(1, 0.5), Units = c("mcg/kg", "mcg/kg/hr"))
  x <- carbon3Run(doses, 120, events = ev, age = 0.5, weight = 7, height = 67)
  r <- x$run
  sets <- x$plot$dexmedetomidineInfantCPB$PK
  expect_false(is.null(r$pkAt))
  minutes <- r$t / 60
  inForce <- ifelse(minutes < 30, "default", ifelse(minutes < 90, "CPBStart", "CPBEnd"))
  for (k in c("v1", "v2", "cl1"))
    expect_equal(r$pkAt[[k]], vapply(inForce, function(s) sets[[s]][[k]], numeric(1), USE.NAMES = FALSE))
  expect_equal(r$pk$v1, sets$default$v1)
  expect_true(r$inputKnown)
  # Eliminated, as the viewer reads it: given less the drug in the body at the
  # volumes in force. It never falls, never jumps at an event (it fell by a
  # tenth of the body's drug at 30 min, 23.5 mcg/min), and over each second
  # is CL1 x Cp of the set in force.
  n <- length(r$t)
  body <- r$amountScale * (r$cp * r$pkAt$v1 + r$c2 * r$pkAt$v2)
  elim <- r$given - body
  rate <- diff(elim) / (diff(r$t) / 60)
  expect_gte(min(rate), 0)
  physiology <- r$amountScale * r$pkAt$cl1[-n] * (r$cp[-1] + r$cp[-n]) / 2
  # Away from the two seconds that end at an event (where the trapezoid takes
  # the next set's concentration), to 1e-3 of the largest.
  clear <- !(r$t[-1] %in% (c(30, 90) * 60))
  expect_lt(max(abs(rate - physiology)[clear]), 1e-3 * max(physiology))
  expect_lt(max(rate), 1.01 * max(physiology))
  # A window inside one set (40 to 80 min, on bypass) needs no pkAt, and
  # carries that set.
  inside <- carbon3Run(doses, 80, events = ev, minimum = 40, age = 0.5, weight = 7, height = 67)$run
  expect_null(inside$pkAt)
  expect_equal(inside$pk$v1, sets$CPBStart$v1)
  # A drug without events carries none.
  expect_null(carbon3Run(data.frame(Drug = "propofol", Time = 0, Dose = 100, Units = "mg"), 30)$run$pkAt)
})

test_that("M7: the debug log takes a table in one write, keeps its build line and is bounded", {
  lines <- sprintf("row %d of the table", seq_len(100))
  old <- function(log, lines) { for (line in lines) log <- paste0(log, "\n", line); log }
  expect_identical(appendDebugLog("\nstanpumpR build", lines), old("\nstanpumpR build", lines))
  expect_identical(appendDebugLog("", "first"), "\nfirst")
  # Over the bound: the build line, a note of what was dropped (once), and
  # the newest lines.
  log <- "\nstanpumpR build"
  for (k in 1:30) log <- appendDebugLog(log, sprintf("pass %d line %d %s", k, 1:200, strrep("x", 40)))
  expect_lte(nchar(log), DEBUG_LOG_MAX_CHARS)
  expect_true(startsWith(log, "\nstanpumpR build\n[earlier lines dropped]\npass "))
  expect_equal(lengths(regmatches(log, gregexpr("[earlier lines dropped]", log, fixed = TRUE))), 1)
  expect_true(endsWith(log, sprintf("pass 30 line 200 %s", strrep("x", 40))))

  # Through outputComments() in a session at the debug level: one write per
  # table, counted on the session's log (a reactiveVal, as app_server() makes it).
  session <- shiny::MockShinySession$new()
  session$userData$debug <- shiny::reactiveVal(DEBUG_LEVEL_VERBOSE)
  value <- shiny::reactiveVal("")
  writes <- 0
  session$userData$commentsLog <- structure(function(x) {
    if (missing(x)) return(value())
    writes <<- writes + 1
    value(x)
  }, class = class(value))
  shiny::withReactiveDomain(session, withr::with_options(list(ECHO_OUTPUT_COMMENTS = FALSE), {
    outputComments(data.frame(Drug = "propofol", Time = 1:50, Dose = 1, Units = "mg"))
  }))
  expect_equal(writes, 1)
  expect_match(shiny::isolate(value()), "propofol", fixed = TRUE)
  expect_length(strsplit(shiny::isolate(value()), "\n")[[1]], 52)   # "", the header and 50 rows
})
