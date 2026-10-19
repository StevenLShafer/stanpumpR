# Each opioid's % MEAC on its own panel, on one shared scale (R/meacOverlay.R)

overlayPlot <- function(doseTable, maximum = 720, ...) {
  local_mocked_bindings(outputComments = function(...) {}, .env = parent.frame())
  defaults <- getDrugDefaultsGlobal(FALSE)
  events <- data.frame(Time = numeric(0), Event = character(0))
  drugs <- suppressMessages(processdoseTable(
    doseTable, events,
    recalculatePK(NULL, defaults, doseTable, 40, 70, 170, "male"),
    maximum, FALSE
  ))
  p <- simulationPlot(
    drugs = drugs, events = events,
    drugDefaults = defaults, eventDefaults = getEventDefaults(),
    xBreaks = seq(0, maximum, maximum / 6), plasmaLinetype = "blank", ...
  )
  list(drugs = drugs, plot = p)
}

# The overlay's line layer: the only layer with a Y column grouped by Drug
# and no colour mapping
overlayLines <- function(p) {
  for (layer in rev(p$plotObject$layers)) {
    d <- layer$data
    if (is.data.frame(d) && all(c("Drug", "Time", "Y", "Wrap") %in% names(d)) &&
        is.null(layer$mapping$colour)) return(d)
  }
  NULL
}

oxycodoneDose <- data.frame(Drug = "oxycodone", Time = 0, Dose = 10, Units = "mg PO")

test_that("the breaks start at zero and reach the peak", {
  expect_null(meacOverlayBreaks(0))
  expect_null(meacOverlayBreaks(NA_real_))
  expect_null(meacOverlayBreaks(numeric(0)))
  for (peak in c(4.7, 89, 103, 155, 372, 2400)) {
    b <- meacOverlayBreaks(peak)
    expect_equal(b[1], 0)
    expect_gte(max(b), peak)
    expect_true(all(diff(b) > 0))
  }
})

test_that("every opioid panel is drawn on one % MEAC scale", {
  out <- overlayPlot(oxycodoneDose, plotMEACOverlay = TRUE)
  lines <- overlayLines(out$plot)
  expect_false(is.null(lines))
  expect_setequal(unique(as.character(lines$Drug)), c("oxycodone", "oxymorphone"))

  built <- ggplot2::ggplot_build(out$plot$plotObject)
  layout <- built$layout$layout
  pct <- list()
  for (drug in c("oxycodone", "oxymorphone")) {
    entry <- out$drugs[[drug]]
    rows <- lines[lines$Drug == drug, ]
    k <- which(layout$Wrap == as.character(rows$Wrap[1]))
    limits <- built$layout$panel_scales_y[[layout$SCALE_Y[k]]]$get_limits()
    # Back to a fraction of the panel's height
    fraction <- (rows$Y - limits[1]) / diff(limits)
    expect_true(all(fraction <= 1 + 1e-9))
    pct[[drug]] <- list(
      fraction = fraction,
      pct = derivedEffectSite(entry, rows$Time) / as.numeric(entry$MEAC) * 100
    )
  }
  # The same scale: % MEAC over height fraction is one number on both panels
  ratios <- unlist(lapply(pct, function(x) {
    keep <- x$fraction > 1e-6
    x$pct[keep] / x$fraction[keep]
  }))
  expect_equal(min(ratios), max(ratios), tolerance = 1e-9)
  top <- max(meacOverlayBreaks(max(unlist(lapply(pct, `[[`, "pct")))))
  expect_equal(ratios[[1]], top, tolerance = 1e-9)
  # Oxycodone reaches the higher % MEAC, so its line rises higher
  expect_gt(max(pct$oxycodone$fraction), max(pct$oxymorphone$fraction))
})

test_that("the overlay is labelled at the right-hand edge, the top label naming MEAC", {
  out <- overlayPlot(oxycodoneDose, plotMEACOverlay = TRUE)
  labels <- NULL
  for (layer in out$plot$plotObject$layers) {
    if (is.data.frame(layer$data) && "label" %in% names(layer$data)) labels <- layer$data
  }
  expect_false(is.null(labels))
  expect_true(any(grepl("% MEAC$", labels$label)))
  expect_true(all(grepl("%", labels$label)))
})

test_that("a non-opioid panel gets no overlay", {
  dt <- data.frame(Drug = c("oxycodone", "propofol"), Time = 0, Dose = c(10, 2),
                   Units = c("mg PO", "mg/kg"))
  out <- overlayPlot(dt, maximum = 240, plotMEACOverlay = TRUE)
  lines <- overlayLines(out$plot)
  expect_false("propofol" %in% lines$Drug)
})

test_that("the time until threshold takes the right-hand edge from the overlay", {
  without <- overlayPlot(oxycodoneDose, plotRecovery = TRUE)
  both <- overlayPlot(oxycodoneDose, plotRecovery = TRUE, plotMEACOverlay = TRUE)
  expect_equal(length(both$plot$plotObject$layers), length(without$plot$plotObject$layers))
})

test_that("normalization turns the overlay off", {
  plain <- overlayPlot(oxycodoneDose, normalization = "Peak plasma")
  both <- overlayPlot(oxycodoneDose, normalization = "Peak plasma", plotMEACOverlay = TRUE)
  expect_equal(length(both$plot$plotObject$layers), length(plain$plot$plotObject$layers))
})

test_that("the overlay leaves the concentration panels' heights alone", {
  plain <- overlayPlot(oxycodoneDose)
  over  <- overlayPlot(oxycodoneDose, plotMEACOverlay = TRUE)
  rangeOf <- function(p) {
    b <- ggplot2::ggplot_build(p$plot$plotObject)
    lapply(b$layout$panel_params, function(x) x$y.range)
  }
  expect_equal(rangeOf(over), rangeOf(plain))
})

test_that("the hover's % MEAC is the overlay's value", {
  out <- overlayPlot(oxycodoneDose, plotMEACOverlay = TRUE)
  entry <- out$drugs$oxymorphone
  expect_equal(meacPercentAt(entry, 120),
               derivedEffectSite(entry, 120) / as.numeric(entry$MEAC) * 100)
  expect_null(meacPercentAt(list(MEAC = "0"), 10))
})

test_that("the scenario checker knows the option", {
  drugDefaults <- getDrugDefaultsGlobal()
  s <- helpScenario("both-right", "Both", "Opioids", "x",
                    doses = helpDoses(c("fentanyl", 0, 100, "mcg")),
                    showMEAC = TRUE, showThreshold = TRUE)
  expect_match(helpScenarioCheck(s, drugDefaults), "mutually exclusive", all = FALSE)
  s <- helpScenario("log-meac", "Log", "Opioids", "x",
                    doses = helpDoses(c("fentanyl", 0, 100, "mcg")),
                    showMEAC = TRUE, logY = TRUE)
  expect_match(helpScenarioCheck(s, drugDefaults), "logY is unavailable", all = FALSE)
  s <- helpScenario("norm-meac", "Norm", "Opioids", "x",
                    doses = helpDoses(c("fentanyl", 0, 100, "mcg")),
                    showMEAC = TRUE, normalization = "Peak plasma")
  expect_match(helpScenarioCheck(s, drugDefaults), "showMEAC needs", all = FALSE)
})
