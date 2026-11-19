# Dennis Fisher's comments on the app (email of 2026-10-08), one test each:
#   1. "Adjust weight to FF mass" -- you do not explain why one would do that
#   2. the same line type for plasma and effect site gave an error; better to
#      say "Plasma and effect site lines should be displayed with different
#      line types"
#   3. the default drugs included no inhaled drug, so a user might not realise
#      that those drugs are an option
#   4. "Threshold" is not defined

uiHtml <- function() {
  # app_ui() uses numericInput(updateOn = ), which needs shiny 1.10
  skip_if_not(utils::packageVersion("shiny") >= "1.10.0")
  oldConfig <- .sprglobals$config
  .sprglobals$config <- DEFAULT_CONFIG
  on.exit(.sprglobals$config <- oldConfig)
  as.character(htmltools::renderTags(app_ui()(list()))$html)
}

# The text from the end of `id`'s checkbox to the next control
afterControl <- function(html, id) {
  at <- regexpr(sprintf('id="%s"', id), html, fixed = TRUE)
  expect_gt(at, 0)
  substr(html, at, at + 4000)
}

test_that("the fat-free-mass box says why one would tick it (1)", {
  html <- uiHtml()
  tip <- afterControl(html, "adjustToFFM")
  expect_match(tip, "field-help", fixed = TRUE)
  expect_match(tip, "Why: drug clearance follows lean tissue, not fat", fixed = TRUE)
  expect_match(tip, "Doses entered per kg", fixed = TRUE)
  # and the Patient Profile help page says it too
  page <- paste(readLines(system.file("app-help", "patient-profile.md",
                                      package = "stanpumpR")), collapse = "\n")
  expect_match(page, "clearance follows lean tissue, not fat", fixed = TRUE)
})

test_that("matching line types are said, not an error (2)", {
  html <- uiHtml()
  expect_match(html, 'id="linetype-same-note"', fixed = TRUE)
  expect_match(html, LINETYPE_SAME_MESSAGE, fixed = TRUE)
  expect_match(LINETYPE_SAME_MESSAGE,
               "^Plasma and effect site lines should be displayed with different line types")
  # shown only while they match, normalization is off and neither is blank
  expect_match(html, "input.plasmaLinetype === input.effectsiteLinetype", fixed = TRUE)
  # (the page writes the condition HTML-escaped)
  expect_match(html, "input.normalization === &#39;none&#39;", fixed = TRUE)
  expect_match(html, "input.plasmaLinetype !== &#39;blank&#39;", fixed = TRUE)

  # and the plot draws for every matching choice
  dt <- data.frame(Drug = "propofol", Time = 0, Dose = 100, Units = "mg")
  ev <- data.frame(Time = numeric(0), Event = character(0))
  out <- simulateDrugsWithCovariates(dt, ev, 70, 170, 40, "male", maximum = 60,
                                     plotRecovery = FALSE)
  for (lt in PLOT_LINE_TYPES) {
    p <- suppressMessages(simulationPlot(
      drugs = out, events = ev, drugDefaults = getDrugDefaultsGlobal(),
      eventDefaults = getEventDefaults(),
      plasmaLinetype = lt, effectsiteLinetype = lt))
    if (lt == "blank") next # a deliberate request for an empty plot
    expect_silent(ggplot2::ggplotGrob(p$plotObject))
  }
})

test_that("the startup menu offers the inhaled anesthetics (3)", {
  choices <- startupDrugChoices()
  expect_true("Inhaled anesthetics" %in% names(choices))
  expect_true(all(c("sevoflurane", "desflurane", "isoflurane") %in%
                    choices[["Inhaled anesthetics"]]))
})

test_that("Time until threshold defines the threshold where it is ticked (4)", {
  html <- uiHtml()
  tip <- afterControl(html, "showThreshold")
  expect_match(tip, "field-help", fixed = TRUE)
  expect_match(tip, "The threshold is a", fixed = TRUE)
  expect_match(tip, "Drug", fixed = TRUE)
  expect_match(tip, "Thresholds", fixed = TRUE)
  page <- paste(readLines(system.file("app-help", "graph-options.md",
                                      package = "stanpumpR")), collapse = "\n")
  expect_match(page, "The **threshold** is a concentration set for each drug",
               fixed = TRUE)
})
