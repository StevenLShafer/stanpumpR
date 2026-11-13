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
