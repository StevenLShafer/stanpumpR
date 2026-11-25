# The physiology ribbon and the stepped threshold (R/physiologyRibbon.R;
# Shafer, 2026-11-24/25, Gemini's gas-engine audit of 2026-11-23, C-01)

ribbonRun <- function(physiology = NULL) {
  dt <- data.frame(Drug = "propofol", Time = c(0, 0), Dose = c(2, 100),
                   Units = c("mg/kg", "mcg/kg/min"))
  ev <- data.frame(Time = numeric(0), Event = character(0))
  simulateDrugsWithCovariates(dt, ev, 70, 170, 40, "male", maximum = 60,
                              plotRecovery = FALSE, physiology = physiology)
}
layerOf <- function(plot, geom)
  Filter(function(l) inherits(l$geom, geom), plot$plotObject$layers)

test_that("the ribbon runs from the nominal curve to the changed one, on the drawn line", {
  d <- data.frame(Drug = "x", Time = c(0, 1, 2, 3), Wrap = "x\n(u)",
                  Site = rep(c("Plasma", "Effect Site"), each = 4),
                  Y = c(0, 2, 3, 4, 0, 1, 2, 3))
  d$Time <- rep(0:3, 2)
  n <- list(x = list(results = data.frame(Time = rep(0:3, 2),
                                          Site = rep(c("Plasma", "Effect Site"), each = 4),
                                          Y = c(0, 1, 1.5, 2, 0, 0.5, 1, 1.5))))
  r <- physiologyRibbons(d, n, "x", "x\n(u)")
  # The effect-site line, drawn, carries it
  expect_equal(r$ymax, c(0, 1, 2, 3))
  expect_equal(r$ymin, c(0, 0.5, 1, 1.5))
  # The plasma line where the effect site is not drawn
  r <- physiologyRibbons(d[d$Site == "Plasma", ], n, "x", "x\n(u)")
  expect_equal(r$ymax, c(0, 2, 3, 4))
  expect_equal(r$ymin, c(0, 1, 1.5, 2))
  # A changed curve below the nominal one: the ribbon still runs between them
  low <- d
  low$Y <- low$Y / 4
  r <- physiologyRibbons(low, n, "x", "x\n(u)")
  expect_true(all(r$ymin <= r$ymax))
  expect_equal(r$ymax[2], 0.5)
  # No ribbon where the physiology changes nothing, nor without a nominal run
  same <- list(x = list(results = d))
  expect_equal(nrow(physiologyRibbons(d, same, "x", "x\n(u)")), 0)
  expect_equal(nrow(physiologyRibbons(d, NULL, "x", "x\n(u)")), 0)
  # A step at an event's repeated time is kept, the nominal curve read through it
  step <- data.frame(Drug = "x", Wrap = "x\n(u)", Site = "Effect Site",
                     Time = c(0, 1, 1, 2), Y = c(1, 1, 3, 3))
  nom <- list(x = list(results = data.frame(Time = c(0, 2), Site = "Effect Site", Y = c(1, 1))))
  r <- physiologyRibbons(step, nom, "x", "x\n(u)")
  expect_equal(r$Time, c(0, 1, 1, 2))
  expect_equal(r$ymax, c(1, 1, 3, 3))
})

test_that("a drug the physiology changes loses its band and gains the ribbon", {
  changed <- ribbonRun(c(liver = 0.3))
  nominal <- ribbonRun()
  plot <- function(nominal, ...)
    suppressMessages(simulationPlot(changed, data.frame(Time = numeric(0), Event = character(0)),
                                    getDrugDefaultsGlobal(), getEventDefaults(),
                                    xBreaks = seq(0, 60, 10), xLabels = seq(0, 60, 10),
                                    plasmaLinetype = "blank", effectsiteLinetype = "solid",
                                    typical = "Range", nominal = nominal, ...))
  p <- plot(nominal)
  ribbon <- layerOf(p, "GeomRibbon")
  expect_length(ribbon, 1)
  expect_equal(unique(ribbon[[1]]$data$Drug), "propofol")
  expect_true(all(ribbon[[1]]$data$ymax >= ribbon[[1]]$data$ymin))
  # The band (the first rectangle layer, drawn from the panel table) is gone
  bands <- Filter(function(l) "lowerTypical" %in% names(l$data), layerOf(p, "GeomRect"))
  expect_false(any(vapply(bands, function(l) "propofol" %in% l$data$Drug, TRUE)))
  # Without a nominal run: the band, no ribbon
  p0 <- plot(NULL)
  expect_length(layerOf(p0, "GeomRibbon"), 0)
  bands0 <- Filter(function(l) "lowerTypical" %in% names(l$data), layerOf(p0, "GeomRect"))
  expect_true(any(vapply(bands0, function(l) "propofol" %in% l$data$Drug, TRUE)))
  # Not under normalization
  expect_length(layerOf(plot(nominal, normalization = "Peak plasma"), "GeomRibbon"), 0)
  # On a log axis it stays above zero
  pl <- plot(nominal, logY = TRUE)
  expect_true(all(layerOf(pl, "GeomRibbon")[[1]]$data$ymin > 0))
})

test_that("a temperature event steps a potent gas's threshold line", {
  dt <- data.frame(Drug = c("oxygen", "sevoflurane", "ventilation", "sevoflurane"),
                   Time = c(0, 0, 0, 40), Dose = c(6, 2, 6, 0))
  ev <- data.frame(Time = 20, t(physiologyEventDecode("temperature=32")))
  sim <- simulateGases(dt, weight = 70, age = 40, maximum = 60, physiologyEvents = ev)
  w <- gasWashout(sim, dt[, c("Time", "Drug", "Dose")])
  entries <- gasDrugEntries(sim, dt, getDrugDefaultsGlobal(), 60, washout = w, age = 40)
  s <- entries$sevoflurane$endCeSteps
  expect_equal(s$Start, c(0, 20))
  at40 <- gasThresholdForAge("sevoflurane",
                             getDrugDefaultsGlobal()$endCe[getDrugDefaultsGlobal()$Drug == "sevoflurane"], 40)
  expect_equal(s$endCe, c(at40, gasThresholdForTemperature("sevoflurane", at40, 32)))
  expect_lt(s$endCe[2], s$endCe[1])
  # Without an event, one value
  sim0 <- simulateGases(dt, weight = 70, age = 40, maximum = 60)
  e0 <- gasDrugEntries(sim0, dt, getDrugDefaultsGlobal(), 60,
                       washout = gasWashout(sim0, dt[, c("Time", "Drug", "Dose")]), age = 40)
  expect_null(e0$sevoflurane$endCeSteps)
  # The rectangles the plot draws: one per step, inside the window
  r <- thresholdSteps(c("sevoflurane", "MAC"), c(0.18, 0.1),
                      list(sevoflurane = s), 0, 60)
  expect_equal(r$xmin, c(0, 20, 0))
  expect_equal(r$xmax, c(20, 60, 60))
  expect_equal(r$endCe, c(s$endCe, 0.1))
})

test_that("the app simulates the nominal drugs only while the physiology is off normal", {
  shiny::testServer(app_server, {
    session$setInputs(
      timeUnits = "minutes", timeMode = "relative", referenceTime = "08:00", maximum = "60",
      plotWidth = 800, yaxisHeight = 200, plasmaLinetype = "blank", effectsiteLinetype = "solid",
      weight = 70, weightUnit = "1", height = 170, heightUnit = "1", age = 40, ageUnit = "1",
      sex = "male", showThreshold = FALSE, normalization = "none", typical = "Range", logY = FALSE,
      physiology_liver = 0, physiology_renal = 0, physiology_cardiac = 0, physiology_gi = 0,
      physiology_temperature = 37)
    dt <- doseTableInit[1, ]
    dt[1, ] <- list("propofol", "0", "2", "mg/kg")
    rownames(dt) <- NULL
    session$env$setDoseTable(dt, timeFormat("minutes", "relative"))
    session$elapse(PHYSIOLOGY_DEBOUNCE_MS + 100)
    session$flushReact()
    expect_null(nominalDrugs())
    session$setInputs(physiology_liver = log10(0.3))
    session$elapse(PHYSIOLOGY_DEBOUNCE_MS + 100)
    session$flushReact()
    n <- nominalDrugs()
    expect_true("propofol" %in% names(n))
    # Its curve is the normal one: the same as the drawn curve at normal
    session$setInputs(physiology_liver = 0)
    session$elapse(PHYSIOLOGY_DEBOUNCE_MS + 100)
    session$flushReact()
    normal <- drugs()$propofol$results
    site <- n$propofol$results$Site == "Effect Site"
    expect_equal(n$propofol$results$Y[site], normal$Y[normal$Site == "Effect Site"])
  })
})

test_that("a plot with no stepped threshold draws one per panel (no steps at all)", {
  r <- thresholdSteps(c("a", "b"), c(1, 2), list(), 0, 60)
  expect_equal(r$endCe, c(1, 2))
  expect_equal(r$xmax, c(60, 60))
})
