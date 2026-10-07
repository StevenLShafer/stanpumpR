test_that("simulationPlot yields desired objects", {

  local_mocked_bindings(outputComments = function(...) {})

  doseTable <- data.frame(
    Drug = getDrugDefaultsGlobal(FALSE)$Drug[1],
    Time = 0,
    Dose = 1,
    Units = "mg"
  )

  eventTable <- data.frame(
    Time = 0,
    Event = "Event"
  )

  ## from server.R

  age <- 50
  weight <- 60
  height <- 66*2.54
  sex <- "female"

  plotMaximum <- 60
  plotRecovery <- TRUE
  plotEvents <- TRUE

  newDrugs <- recalculatePK(
    NULL,
    getDrugDefaultsGlobal(FALSE),
    doseTable,
    age, weight, height, sex
  )

  drugs <- processdoseTable(
    doseTable,
    eventTable,
    newDrugs,
    plotMaximum,
    plotRecovery
  )

  p <- simulationPlot(
    drugs = drugs,
    events = eventTable,
    drugDefaults = getDrugDefaultsGlobal(FALSE),
    eventDefaults = getEventDefaults(),
    plotEvents = plotEvents,
    plotRecovery = plotRecovery
  )

  expect_equal(names(p), c("plotObject","allResults","plotResults","plotHeight"))
})


# Hiding the plasma line must not hide a drug entirely.
#
# The default setting draws the effect site only.  A drug with no effect site
# -- a prodrug, or one whose potency has not been supplied -- has nothing in
# that series, so dropping its plasma rows too left it as an empty panel.
# Codeine, tramadol and desmetramadol all did this out of the box.

plotWith <- function(doseTable, plasmaLinetype, effectsiteLinetype = "solid") {
  local_mocked_bindings(outputComments = function(...) {}, .env = parent.frame())
  defaults <- getDrugDefaultsGlobal(FALSE)
  events <- data.frame(Time = numeric(0), Event = character(0))
  drugs <- processdoseTable(
    doseTable, events,
    recalculatePK(NULL, defaults, doseTable, 50, 70, 170, "male"),
    720, FALSE
  )
  simulationPlot(
    drugs = drugs, events = events,
    drugDefaults = defaults, eventDefaults = getEventDefaults(),
    plotEvents = FALSE, plotRecovery = FALSE,
    plasmaLinetype = plasmaLinetype, effectsiteLinetype = effectsiteLinetype
  )
}

drawnFor <- function(p, drug) {
  sum(as.character(p$plotResults$Drug) == drug & !is.na(p$plotResults$Y))
}


test_that("a drug with no effect site still draws when the plasma line is blank", {
  # tramadol has no effect site of its own and forms desmetramadol, which
  # has no potency supplied yet, so neither has an effect-site curve.
  DT <- data.frame(Drug = "tramadol", Time = 0, Dose = 100, Units = "mg PO")

  p <- plotWith(DT, plasmaLinetype = "blank")
  expect_gt(drawnFor(p, "tramadol"), 0)
  expect_gt(drawnFor(p, "desmetramadol"), 0)
  # and what is drawn is the plasma series, since there is nothing else
  tram <- p$plotResults[as.character(p$plotResults$Drug) == "tramadol", ]
  expect_true(all(as.character(tram$Site) == "Plasma"))
})


test_that("hiding the plasma line still hides it for drugs that have an effect site", {
  # The setting must keep working for everything else.
  DT <- data.frame(Drug = "fentanyl", Time = 0, Dose = 100, Units = "mcg")

  p <- plotWith(DT, plasmaLinetype = "blank")
  fent <- p$plotResults[as.character(p$plotResults$Drug) == "fentanyl", ]
  expect_gt(nrow(fent), 0)
  expect_false(any(as.character(fent$Site) == "Plasma"))
})


test_that("the two cases coexist in one plot", {
  DT <- data.frame(
    Drug  = c("tramadol", "fentanyl"),
    Time  = c(0, 0),
    Dose  = c(100, 100),
    Units = c("mg PO", "mcg")
  )
  p <- plotWith(DT, plasmaLinetype = "blank")

  # fentanyl shows effect site only, tramadol shows plasma only
  fent <- p$plotResults[as.character(p$plotResults$Drug) == "fentanyl", ]
  tram <- p$plotResults[as.character(p$plotResults$Drug) == "tramadol", ]
  expect_true(all(as.character(fent$Site) == "Effect Site"))
  expect_true(all(as.character(tram$Site) == "Plasma"))
  expect_gt(nrow(fent), 0)
  expect_gt(nrow(tram), 0)
})


test_that("blanking both lines returns an empty plot rather than erroring", {
  # The escape hatch has to keep working: asking for neither series draws
  # neither, rather than the prodrug rule forcing plasma back on.
  #
  # Nothing survives both filters, and simulationPlot answers "nothing to
  # plot" with NULL, as its own earlier zero-row guard does.  That guard runs
  # before the linetype filters and so never saw this case; until a drug
  # existed with only one of the two series, nothing exercised the path, and
  # the function errored on a zero-row assignment.  Reproducible on master.
  DT <- data.frame(Drug = "tramadol", Time = 0, Dose = 100, Units = "mg PO")
  expect_no_error(
    p <- plotWith(DT, plasmaLinetype = "blank", effectsiteLinetype = "blank")
  )
  expect_null(p)
})


test_that("blanking both lines is empty for an ordinary drug too", {
  # Not a prodrug problem: any single-drug table with both series blanked
  # reaches the same place.
  DT <- data.frame(Drug = "fentanyl", Time = 0, Dose = 100, Units = "mcg")
  expect_no_error(
    p <- plotWith(DT, plasmaLinetype = "blank", effectsiteLinetype = "blank")
  )
  expect_null(p)
})


# Time units (utils-time-display.R).  The plot is drawn in minutes whatever
# the display unit; the server passes the labels and the title in that unit,
# and the length of the simulation as xMaximum.  (Claude Code,
# 2026-10-07.)

simulatedFor <- function(doseTable, maximum, plotRecovery = FALSE) {
  local_mocked_bindings(outputComments = function(...) {})
  defaults <- getDrugDefaultsGlobal(FALSE)
  events <- data.frame(Time = numeric(0), Event = character(0))
  processdoseTable(
    doseTable, events,
    recalculatePK(NULL, defaults, doseTable, 50, 70, 170, "male"),
    maximum, plotRecovery
  )
}

plotIn <- function(drugs, ...) {
  local_mocked_bindings(outputComments = function(...) {}, .env = parent.frame())
  simulationPlot(
    drugs = drugs, events = data.frame(Time = numeric(0), Event = character(0)),
    drugDefaults = getDrugDefaultsGlobal(FALSE), eventDefaults = getEventDefaults(),
    plotEvents = FALSE, ...
  )
}


test_that("a weeks axis is labelled in weeks, its data still in minutes", {
  DT <- data.frame(Drug = "fentanyl", Time = 0, Dose = 100, Units = "mcg")
  maximum <- 52 * MINS_PER_WEEK                       # 524160 min
  xBreaks <- seq(0, maximum, by = 4 * MINS_PER_WEEK)  # every 4 weeks
  p <- plotIn(
    simulatedFor(DT, maximum),
    xBreaks = xBreaks,
    xLabels = axisTimeLabels(xBreaks, "weeks"),
    xAxisLabel = timeAxisTitle("weeks"),
    xMaximum = maximum
  )
  expect_no_warning(built <- ggplot2::ggplot_build(p$plotObject))
  expect_equal(p$plotObject$labels$x, "Time (weeks)")
  x <- built$layout$panel_params[[1]]$x
  expect_equal(x$breaks, xBreaks)
  expect_equal(x$get_labels(), as.character(seq(0, 52, by = 4)))
  expect_equal(built$layout$panel_params[[1]]$x.range, c(0, maximum))
  # The curve is in minutes and runs to the end of the plot
  expect_equal(max(p$plotResults$Time), maximum)
})


test_that("the axis runs to xMaximum when the tick step does not divide it", {
  # 365 days ticked every 30 days: the last tick is day 360, and the axis,
  # the typical band and everything held to the right-hand edge must still
  # reach day 365.
  DT <- data.frame(Drug = "fentanyl", Time = 0, Dose = 100, Units = "mcg")
  maximum <- 365 * MINS_PER_DAY                       # 525600 min
  xBreaks <- seq(0, maximum, by = 30 * MINS_PER_DAY)
  expect_equal(max(xBreaks), 360 * MINS_PER_DAY)
  drugs <- simulatedFor(DT, maximum)

  p <- plotIn(drugs, xBreaks = xBreaks, xLabels = axisTimeLabels(xBreaks, "days"),
              xAxisLabel = timeAxisTitle("days"), xMaximum = maximum, typical = "Mid")
  built <- ggplot2::ggplot_build(p$plotObject)
  expect_equal(built$layout$panel_params[[1]]$x.range, c(0, maximum))
  expect_equal(built$layout$panel_params[[1]]$x$get_labels(),
               as.character(seq(0, 360, by = 30)))
  band <- Filter(function(d) "xmax" %in% names(d), built$data)
  expect_gt(length(band), 0)
  expect_true(all(vapply(band, function(d) all(d$xmax == maximum), logical(1))))

  # Without xMaximum the last break is the end, as it always was
  p <- plotIn(drugs, xBreaks = xBreaks, xLabels = axisTimeLabels(xBreaks, "days"))
  built <- ggplot2::ggplot_build(p$plotObject)
  expect_equal(built$layout$panel_params[[1]]$x.range, c(0, 360 * MINS_PER_DAY))
})


test_that("the default axis is unchanged", {
  # Callers that pass nothing still get 0-60 minutes, ticked every 10
  DT <- data.frame(Drug = "fentanyl", Time = 0, Dose = 100, Units = "mcg")
  p <- plotIn(simulatedFor(DT, 60))
  built <- ggplot2::ggplot_build(p$plotObject)
  expect_equal(built$layout$panel_params[[1]]$x.range, c(0, 60))
  expect_equal(built$layout$panel_params[[1]]$x$breaks, seq(0, 60, by = 10))
  expect_equal(p$plotObject$labels$x, "Time (Minutes)")
})


test_that("each panel's time-until-threshold labels are in a unit that suits it", {
  # Three drugs whose times until threshold differ by orders of magnitude:
  # remifentanil recovers in minutes; enough fentanyl stays above its
  # threshold past the one-day horizon (labels in hours); enough vancomycin,
  # timed on its plasma, past the one-week horizon (labels in days).
  DT <- data.frame(Drug = c("remifentanil", "fentanyl", "vancomycin"), Time = 0,
                   Dose = c(1, 5000, 100000), Units = c("mcg/kg", "mcg", "mg"))
  drugs <- simulatedFor(DT, 60, plotRecovery = TRUE)
  expect_lt(drugs$remifentanil$max$Recovery, 2 * MINS_PER_HOUR)
  expect_equal(drugs$fentanyl$max$Recovery, MINS_PER_DAY)
  expect_equal(drugs$vancomycin$max$Recovery, MINS_PER_WEEK)

  p <- plotIn(drugs, plotRecovery = TRUE)
  expect_no_warning(built <- ggplot2::ggplot_build(p$plotObject))

  labelLayer <- Filter(function(l) inherits(l$geom, "GeomText") &&
                         "new" %in% names(l$data) &&
                         !any(grepl("Threshold", l$data$new)),
                       p$plotObject$layers)
  expect_length(labelLayer, 1)
  labels <- labelLayer[[1]]$data
  labelsOf <- function(drug) labels$new[labels$Drug == drug]
  valueOf  <- function(s) as.numeric(sub(" .*$", "", s))

  expect_true(all(grepl(" min$", labelsOf("remifentanil"))))
  expect_true(all(grepl(" h$",   labelsOf("fentanyl"))))
  expect_true(all(grepl(" d$",   labelsOf("vancomycin"))))

  # Whole numbers of the unit, the top one covering the horizon: 24 h, 7 d
  for (drug in c("remifentanil", "fentanyl", "vancomycin")) {
    v <- valueOf(labelsOf(drug))
    expect_equal(v, round(v))
  }
  expect_gte(max(valueOf(labelsOf("fentanyl"))), 24)
  expect_gte(max(valueOf(labelsOf("vancomycin"))), 7)

  # The line is drawn against those labels: vancomycin sits at the horizon,
  # 7 days, which is 7 / top of the way up to the top label's height.
  recoveryLine <- built$data[[length(built$data)]]
  vanco <- labels[labels$Drug == "vancomycin", ]
  top <- max(valueOf(vanco$new))
  panel <- match(unique(as.character(vanco$Wrap)), levels(p$plotResults$Wrap))
  y <- recoveryLine$y[as.integer(recoveryLine$PANEL) == panel]
  expect_gt(length(y), 0)
  expect_equal(unique(signif(y, 10)), signif(7 / top * max(vanco$y), 10))
})
