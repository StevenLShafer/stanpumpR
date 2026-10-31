test_that("it generates the email body", {
  recipient <- "test-name@test-domain.com"
  values <- list(
    age = 600,
    ageUnit = 1,
    weight = 150,
    weightUnit = 1,
    height = 67,
    heightUnit = 1,
    sex = "F"
  )
  ageUnit <- "months"
  weightUnit <- "pounds"
  heightUnit <- "inches"
  url <- "http://example.com"
  bodyText <- generateBodyText(recipient, values, ageUnit, weightUnit, heightUnit, url)
  expect_match(bodyText, "Dear test-name at test-domain.com:")
  expect_match(bodyText, "The simulation is for a 600 months-old F weighing 150 pounds and 67 inches tall")
  expect_match(bodyText, "file from <a href=\"http://example.com\">stanpumpR</a>")
  expect_match(bodyText, "Thank you for using stanpumpR")
  # A caller that predates time units says nothing about the plot's length
  expect_no_match(bodyText, "The plot runs for")
})

test_that("the email and the workbook say what time units the plot was in", {
  # The workbook's times stay in minutes (exportDoseTable(),
  # addDisplayTimeColumn()); the email and the Covariates sheet say how long
  # the plot ran, in the unit it was shown in, and that the minutes have that
  # unit beside them.  52 weeks = 524160 minutes.
  values <- list(age = 50, ageUnit = 1, weight = 70, weightUnit = 1,
                 height = 170, heightUnit = 1, sex = "male",
                 timeUnit = "weeks", maximum = 52 * MINS_PER_WEEK)
  bodyText <- generateBodyText("a@b.c", values, "years", "kilograms", "cms", "http://example.com")
  expect_match(bodyText, "The plot runs for 52 weeks. Times in the workbook are in minutes, each followed by the same time in weeks.", fixed = TRUE)

  expect_equal(plotTimeText(MINS_PER_DAY, "hours"),
               paste0("<p>The plot runs for 24 hours. Times in the workbook are in minutes, ",
                      "each followed by the same time in hours.</p><p>&nbsp;</p>"))
  # In minutes there is no second column to mention
  expect_equal(plotTimeText(60, "minutes"), "<p>The plot runs for 60 minutes.</p><p>&nbsp;</p>")
  expect_equal(plotTimeText(60, NULL), "<p>The plot runs for 60 minutes.</p><p>&nbsp;</p>")
  expect_equal(plotTimeText(NULL, "weeks"), "")

  expect_equal(
    plotTimeSettings(365 * MINS_PER_DAY, "days"),
    data.frame(Covariate = c("Time units", "Max time", "Max time (minutes)"),
               Value = c("days", "365 days", "525600"))
  )
  expect_equal(plotTimeSettings(60, NULL)$Value, c("minutes", "60 minutes", "60"))
  expect_null(plotTimeSettings(NULL, "days"))
})

# The PowerPoint deck: the whole plot on the first slide, then one slide per
# panel, so that a simulation with many drugs is still readable.

slideTestPlot <- function(doseTable, events = data.frame(Time = numeric(0), Event = character(0)),
                          maximum = 60, ...)
{
  local_mocked_bindings(outputComments = function(...) {}, .env = parent.frame())
  defaults <- getDrugDefaultsGlobal(FALSE)
  drugs <- processdoseTable(
    doseTable, events,
    recalculatePK(NULL, defaults, doseTable, 50, 70, 170, "male"),
    maximum, TRUE
  )
  list(
    drugs = drugs,
    plot = simulationPlot(
      drugs = drugs, events = events,
      drugDefaults = defaults, eventDefaults = getEventDefaults(),
      plotRecovery = TRUE, ...
    )
  )
}

panelsOf <- function(plotObject)
{
  as.character(ggplot2::ggplot_build(plotObject)$layout$layout$Wrap)
}

test_that("panelPlots() draws each panel of the plot alone, in the plot's order", {
  doseTable <- data.frame(
    Drug = c("propofol", "fentanyl", "remifentanil", "rocuronium"),
    Time = c(0, 0, 0, 5),
    Dose = c(3, 2, 0.2, 50),
    Units = c("Effect site target", "mcg/kg", "mcg/kg/min", "mg"),
    stringsAsFactors = FALSE
  )
  events <- data.frame(Time = c(5, 30), Event = c("Intubation", "Incision"))
  plotObject <- slideTestPlot(doseTable, events, plotEvents = TRUE,
                              plotMEAC = TRUE, plotInteraction = TRUE)$plot$plotObject

  full <- panelsOf(plotObject)
  expect_length(full, 8)  # four drugs, the TCI rate, % MEAC, p response, Events
  expect_equal(full[8], PLOT_NAME_EVENTS)

  panels <- panelPlots(plotObject)

  # One per panel but the events, in the plot's order, titled by the first
  # line of the strip (the units stay on the strip)
  expect_equal(names(panels), panelTitle(full[-8]))
  # The TCI rate panel directly under its drug's, the other drugs after it
  expect_equal(names(panels)[c(1, 2, 6, 7)],
               c("propofol", "propofol TCI", PLOT_NAME_MEAC, PLOT_NAME_INTERACTION))
  fullBuild <- ggplot2::ggplot_build(plotObject)
  yRange <- function(built, i) built$layout$panel_params[[i]]$y.range
  layerRows <- function(layer, wrap) sum(as.character(layer$data$Wrap) == wrap)
  for (i in seq_along(panels))
  {
    expect_s3_class(panels[[i]], "ggplot")
    expect_equal(panelsOf(panels[[i]]), full[i])
    # Every layer with rows on the panel is kept, cut down to those rows, so
    # the panel is drawn as it was in the full plot; a layer with none is left
    # out (below).  No fixed aspect ratio, so it fills the slide.
    drawn <- vapply(plotObject$layers, layerRows, numeric(1), wrap = full[i])
    expect_length(panels[[i]]$layers, sum(drawn > 0))
    for (layer in panels[[i]]$layers)
    {
      expect_gt(nrow(layer$data), 0)
      expect_true(all(as.character(layer$data$Wrap) == full[i]))
    }
    expect_null(panels[[i]]$theme$aspect.ratio)
    # The panel's y axis is the one it had in the full plot.  ggplot draws a
    # layer whose data is empty but whose aesthetics are constants on the
    # first panel regardless: kept, the white rectangle behind the event
    # labels (ymin 0, ymax 1) stretched the TCI rate panel (rates near 0.1)
    # to 1 and blanked the bottom of every panel.
    expect_equal(yRange(ggplot2::ggplot_build(panels[[i]]), 1), yRange(fullBuild, i))
  }
  # The TCI rate panel is the one those constants reached: its rates are well
  # below 1
  expect_lt(yRange(fullBuild, 2)[2], 0.5)

  # The original is untouched: the layers were copied, not edited in place
  expect_equal(panelsOf(plotObject), full)
  expect_equal(plotObject$theme$aspect.ratio, 0.25)
  expect_gt(nrow(plotObject$layers[[1]]$data), nrow(panels[[1]]$layers[[1]]$data))

  expect_equal(panelTitle(c("propofol\n(mcg/ml)", "% MEAC")), c("propofol", "% MEAC"))
  expect_equal(panelPlots(NULL), list())
  expect_equal(panelPlots(ggplot2::ggplot()), list())
})

test_that("the deck has the whole plot on the first slide and then one slide per panel", {
  doseTable <- data.frame(
    Drug = c("propofol", "fentanyl", "rocuronium"),
    Time = c(0, 0, 0),
    Dose = c(2, 100, 50),
    Units = c("mg/kg", "mcg", "mg"),
    stringsAsFactors = FALSE
  )
  built <- slideTestPlot(doseTable, plotEvents = FALSE)
  values <- list(
    DT = doseTable, url = "http://example.com", comments = "",
    age = 50, ageUnit = 1, weight = 70, weightUnit = 1, height = 170, heightUnit = 1,
    sex = "male", pregnant = FALSE, adjustToFFM = TRUE,
    osmolality = NULL, creatinine = NULL,
    timeUnit = "minutes", maximum = 60
  )

  # generateEmail() writes into the directory it is given (sendSlide() makes
  # a private one per send and removes it)
  dir <- tempfile("slides-")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  local_mocked_bindings(outputComments = function(...) {})

  email <- generateEmail(
    values, "a@b.c", built$plot$plotObject, built$plot$allResults, built$plot$plotResults,
    height = built$plot$plotHeight, width = 800, slide = 3,
    drugs = built$drugs, dir = dir
  )
  expect_equal(unique(dirname(c(email$pptxfileName, email$pngfileName, email$xlsxfileName))), dir)
  expect_false(dir.exists("Slides"))
  expect_true(file.exists(email$pptxfileName))
  expect_true(file.exists(email$pngfileName))
  expect_true(file.exists(email$xlsxfileName))

  deck <- officer::read_pptx(email$pptxfileName)
  expect_equal(length(deck), 4)
  titles <- officer::pptx_summary(deck)
  titles <- titles[titles$content_type == "paragraph", ]
  slideTitle <- function(i) titles$text[titles$slide_id == i][1]
  # The text in the slide-number placeholder, found by its shape id
  slideNumber <- function(i) {
    shapes <- officer::slide_summary(deck, i)
    titles$text[titles$slide_id == i & titles$id == shapes$id[shapes$type == "sldNum"]][1]
  }
  expect_match(slideTitle(1), "^stanpumpR simulation on ")
  expect_equal(c(slideTitle(2), slideTitle(3), slideTitle(4)),
               c("propofol", "fentanyl", "rocuronium"))
  # Each slide carries a drawing in the body placeholder (rvg writes the plot
  # as shapes, whose text the summary lists: the x axis title is on each) and
  # the footer fields
  for (i in 1:4)
  {
    shapes <- officer::slide_summary(deck, i)
    expect_true(all(c("title", "body", "dt", "sldNum", "ftr") %in% shapes$type))
    text <- titles$text[titles$slide_id == i]
    expect_true(any(grepl("Time (Minutes)", text, fixed = TRUE)))
    expect_true("From StanpumpR" %in% text)
  }
  # The first slide is numbered by the send, as before; the others by their
  # place in the deck
  expect_equal(c(slideNumber(1), slideNumber(2), slideNumber(4)), c("3", "2", "4"))
})
