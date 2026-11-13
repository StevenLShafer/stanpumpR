# The toxic metabolites' panel labels are drawn in red (R/toxicMetabolites.R).
# (Claude Code, 2026-11-13, at the request of Steven L. Shafer.)

noEvents <- data.frame(Time = numeric(0), Event = character(0))

# The colour of every strip's text, by strip name, in a drawn plot.
stripColours <- function(plotObject) {
  g <- ggplot2::ggplotGrob(plotObject)
  idx <- grep("^strip-", g$layout$name)
  textOf <- function(x) {
    if (inherits(x, "text")) return(list(list(label = x$label, col = x$gp$col)))
    kids <- if (inherits(x, "gtable")) x$grobs else x$children
    unlist(lapply(kids, textOf), recursive = FALSE)
  }
  out <- lapply(idx, function(i) textOf(g$grobs[[i]]))
  data.frame(
    label = vapply(out, function(t) paste(unlist(lapply(t, `[[`, "label")), collapse = " "), ""),
    col   = vapply(out, function(t) paste(unique(unlist(lapply(t, `[[`, "col"))), collapse = ","), ""),
    stringsAsFactors = FALSE
  )
}

test_that("the toxic metabolites are MEGX and norpethidine", {
  expect_setequal(TOXIC_METABOLITES, c("MEGX", "norpethidine"))
  expect_true(all(TOXIC_METABOLITES %in% getDrugDefaultsGlobal(FALSE)$Drug))
  expect_equal(isToxicPanel(c("MEGX\n(mcg/mL)", "MEGX V2\n(mcg/mL)", "lidocaine\n(mcg/mL)",
                              "norpethidine\n(mcg/mL)", "pethidine\n(mcg/mL)")),
               c(TRUE, TRUE, FALSE, TRUE, FALSE))
})

test_that("a toxic metabolite's strip is red and the parent's is not", {
  DT <- data.frame(Drug = "lidocaine", Time = 0, Dose = 100, Units = "mg")
  dd <- getDrugDefaultsGlobal(FALSE)
  drugs <- processdoseTable(DT, noEvents, recalculatePK(NULL, dd, DT, 35, 70, 170, "male"),
                            600, FALSE)
  local_mocked_bindings(outputComments = function(...) {})
  p <- simulationPlot(drugs = drugs, events = noEvents, drugDefaults = dd,
                      eventDefaults = getEventDefaults(), plotEvents = FALSE,
                      plotRecovery = FALSE)
  s <- stripColours(p$plotObject)
  megx <- grepl("MEGX", s$label)
  lido <- grepl("lidocaine", s$label)
  expect_equal(sum(megx), 1)
  expect_equal(sum(lido), 1)
  expect_equal(s$col[megx], "red")
  expect_false(grepl("red", s$col[lido]))
  # Still an ordinary ggplot, with the facet's layout, so a click maps to a
  # panel as before
  expect_true(ggplot2::is_ggplot(p$plotObject))
  built <- ggplot2::ggplot_build(p$plotObject)
  expect_setequal(as.character(built$layout$layout$Wrap), unique(as.character(p$plotResults$Wrap)))
})

test_that("pethidine's norpethidine panel is red too, and a plot without one has none", {
  dd <- getDrugDefaultsGlobal(FALSE)
  local_mocked_bindings(outputComments = function(...) {})
  plotOf <- function(DT) {
    drugs <- processdoseTable(DT, noEvents, recalculatePK(NULL, dd, DT, 35, 70, 170, "male"),
                              600, FALSE)
    simulationPlot(drugs = drugs, events = noEvents, drugDefaults = dd,
                   eventDefaults = getEventDefaults(), plotEvents = FALSE,
                   plotRecovery = FALSE)$plotObject
  }
  s <- stripColours(plotOf(data.frame(Drug = "pethidine", Time = 0, Dose = 50, Units = "mg")))
  expect_equal(s$col[grepl("norpethidine", s$label)], "red")
  s <- stripColours(plotOf(data.frame(Drug = "fentanyl", Time = 0, Dose = 100, Units = "mcg")))
  expect_false(any(grepl("red", s$col)))
})


test_that("lidocaine + MEGX is drawn in MEGX's panel against 5 mcg/mL", {
  # Crouch 2023's convention: [lidocaine] + [MEGX] against 5 mcg/mL (GX, not
  # modelled, left out).  An infusion of 140 mg/h for 24 h at 70 kg.
  DT <- data.frame(Drug = "lidocaine", Time = c(0, 0, 1440), Dose = c(105, 140, 0),
                   Units = c("mg", "mg/hr", "mg/hr"))
  dd <- getDrugDefaultsGlobal(FALSE)
  drugs <- processdoseTable(DT, noEvents, recalculatePK(NULL, dd, DT, 35, 70, 170, "male"),
                            2880, FALSE)
  local_mocked_bindings(outputComments = function(...) {})
  p <- simulationPlot(drugs = drugs, events = noEvents, drugDefaults = dd,
                      eventDefaults = getEventDefaults(), plotEvents = FALSE,
                      plotRecovery = FALSE, xBreaks = seq(0, 2880, 360),
                      xLabels = seq(0, 48, 6), xMaximum = 2880)
  r <- p$plotResults
  sum <- r[r$Drug == "lidocaine + MEGX", ]
  expect_gt(nrow(sum), 10)
  # In MEGX's panel, as a plasma line
  megxWrap <- unique(as.character(r$Wrap[r$Drug == "MEGX"]))
  expect_equal(unique(as.character(sum$Wrap)), megxWrap)
  # It is the sum of the two drugs' plasma concentrations, read from the
  # simulation itself, at a time on both grids
  lido <- drugs$lidocaine$results; megx <- drugs$MEGX$results
  lp <- lido[lido$Site == "Plasma", ]; mp <- megx[megx$Site == "Plasma", ]
  at <- intersect(intersect(lp$Time, mp$Time), sum$Time)
  at <- at[at > 60][1:3]
  expect_equal(sum$Y[match(at, sum$Time)],
               lp$Y[match(at, lp$Time)] + mp$Y[match(at, mp$Time)], tolerance = 1e-10)
  # The panel's first series is still MEGX, so a click reads MEGX
  expect_equal(as.character(r$Drug[as.character(r$Wrap) == megxWrap])[1], "MEGX")
  # The dashed line at 5, in red, in that panel only
  built <- ggplot2::ggplot_build(p$plotObject)
  hl <- Filter(function(d) "yintercept" %in% names(d), built$data)
  expect_length(hl, 1)
  expect_equal(hl[[1]]$yintercept, 5)
  expect_equal(hl[[1]]$colour, "red")
  panel <- built$layout$layout$PANEL[as.character(built$layout$layout$Wrap) == megxWrap]
  expect_equal(as.character(hl[[1]]$PANEL), as.character(panel))
})

test_that("no sum or threshold without lidocaine, or under normalization", {
  dd <- getDrugDefaultsGlobal(FALSE)
  local_mocked_bindings(outputComments = function(...) {})
  plotOf <- function(DT, ...) {
    drugs <- processdoseTable(DT, noEvents, recalculatePK(NULL, dd, DT, 35, 70, 170, "male"),
                              600, FALSE)
    simulationPlot(drugs = drugs, events = noEvents, drugDefaults = dd,
                   eventDefaults = getEventDefaults(), plotEvents = FALSE,
                   plotRecovery = FALSE, ...)
  }
  p <- plotOf(data.frame(Drug = "fentanyl", Time = 0, Dose = 100, Units = "mcg"))
  expect_false(any(p$plotResults$Drug == "lidocaine + MEGX"))
  lido <- data.frame(Drug = "lidocaine", Time = 0, Dose = 100, Units = "mg")
  p <- plotOf(lido, normalization = "Peak plasma")
  expect_false(any(p$plotResults$Drug == "lidocaine + MEGX"))
  built <- ggplot2::ggplot_build(p$plotObject)
  expect_false(any(vapply(built$data, function(d) "yintercept" %in% names(d), logical(1))))
})
