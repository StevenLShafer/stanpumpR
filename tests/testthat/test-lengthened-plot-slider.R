# A plot lengthened past the Max time asked for moves the Max time slider to
# the length drawn (Steven L. Shafer, 2026-11-28: "I would move the slider...
# There is some dissonance for the user if the slider shows 5 while the max
# time shows 150"; Devin's QA of PR 1029 took the disagreement for a defect).
# And a link or a scenario with physiology away from normal opens the
# sidebar's Physiology section (the same day).

test_that("the plot is lengthened to show what follows the last dose", {
  # Devin's case: doses at time 0, Max time 5 minutes, a plot of 30
  expect_equal(lengthenedPlot(5, 0, "minutes")$plotMaximum, 30)
  # The emergence scenario: the last dose at 120, a plot of 150
  expect_equal(lengthenedPlot(5, 120, "minutes")$plotMaximum, 150)
  # Long enough already
  expect_null(lengthenedPlot(180, 120, "minutes"))
  expect_null(lengthenedPlot(180, -Inf, "minutes"))
  # Never past the unit's longest length
  longest <- timeSliderRange("minutes")$max
  expect_equal(lengthenedPlot(60, longest + 500, "minutes")$plotMaximum, longest)
})

test_that("the slider's new value is the length drawn, on its step and in its range", {
  expect_equal(lengthenedMaximum(30, 5, "minutes"), 30)
  expect_equal(lengthenedMaximum(150, 5, "minutes"), 150)
  # Not lengthened, or the slider already there
  expect_null(lengthenedMaximum(180, 180, "minutes"))
  expect_null(lengthenedMaximum(150, 180, "minutes"))
  # Unreadable values move nothing
  expect_null(lengthenedMaximum(NA, 5, "minutes"))
  expect_null(lengthenedMaximum(150, "x", "minutes"))
  expect_null(lengthenedMaximum(c(150, 160), 5, "minutes"))
  # Capped at the unit's longest length
  longest <- timeSliderRange("hours")$max
  expect_equal(lengthenedMaximum(longest + 1000, 60, "hours"), longest)
})

test_that("writing the lengthened end back does not lengthen the plot again", {
  # The warning from the "nlmixr2lib import" session, 2026-11-28: the slider's
  # new value comes back to plotInfo() as the Max time asked for, and the
  # ticks of the next longer Max time could push the end further while the
  # last dose is still within the margin of the new end.  From the slider's
  # new value the plot must end exactly there, in every unit, for any last
  # dose and any Max time asked for.
  for (unit in names(TIME_SLIDER_STEPS)) {
    r <- timeSliderRange(unit)
    asked <- unique(c(r$step, 2 * r$step, MAX_TIMES[[unit]]$times))
    last <- unique(c(0, r$step / 2, MAX_TIMES[[unit]]$times, MAX_TIMES[[unit]]$times - 1,
                     MAX_TIMES[[unit]]$times + TIME_EXTEND_MARGIN[[unit]] / 2,
                     seq(0, r$max, length.out = 97), r$max + 3 * r$step))
    for (requested in asked) for (maxTime in last) {
      first <- lengthenedPlot(requested, maxTime, unit)
      if (is.null(first)) next
      target <- lengthenedMaximum(first$plotMaximum, requested, unit)
      if (is.null(target)) {
        # The slider was already at or past the end: nothing to write back
        expect_lte(first$plotMaximum, requested)
        next
      }
      info <- paste(unit, requested, maxTime)
      expect_gte(target, first$plotMaximum)
      expect_lte(target, r$max)
      second <- lengthenedPlot(target, maxTime, unit)
      end <- if (is.null(second)) target else second$plotMaximum
      expect_equal(end, target, info = info)
      # ... and nothing more to write back from there
      expect_null(lengthenedMaximum(end, target, unit), info = info)
    }
  }
})

test_that("the page opens the Physiology section when the server shows the sliders", {
  js <- paste(readLines(system.file("www", "app.js", package = "stanpumpR")), collapse = "\n")
  handler <- regmatches(js, regexpr("addCustomMessageHandler\\('physiology_show'[\\s\\S]*?\\n  \\}\\);", js, perl = TRUE))
  expect_length(handler, 1)
  expect_match(handler, "showPhysiologyControls()", fixed = TRUE)
  expect_match(handler, ".accordion-collapse", fixed = TRUE)
  expect_match(handler, "bootstrap.Collapse.getOrCreateInstance", fixed = TRUE)
  # The sliders sit inside the section the handler opens
  html <- as.character(physiologyPanel())
  expect_match(html, 'id="physiology-controls"', fixed = TRUE)
  expect_match(html, "accordion-collapse", fixed = TRUE)
})

test_that("the server moves the Max time slider to the length drawn, once", {
  shiny::testServer(app_server, {
    sent <- list()
    session$sendInputMessage <- function(inputId, message) {
      sent[[length(sent) + 1]] <<- list(id = inputId, message = message)
    }
    toMaximum <- function() Filter(function(m) identical(m$id, "maximum"), sent)
    session$setInputs(
      timeUnits = "minutes", timeMode = "relative", referenceTime = "none",
      maximum = 180, minimum = 0, weight = 70, weightUnit = "1", height = 170,
      heightUnit = "1", age = 40, ageUnit = "1", sex = "male", showThreshold = FALSE,
      normalization = "none", typical = "Range", logY = FALSE)
    DT <- doseTableInit
    DT$Time[1] <- "0"
    setDoseTable(DT, timeFormat("minutes", "relative"))
    session$flushReact(); session$elapse(500)
    before <- length(toMaximum())
    # Max time dragged down to 5 minutes with every dose at 0: the plot runs
    # to 30, and the slider is sent there
    session$setInputs(maximum = 5)
    session$elapse(500)
    expect_equal(plotInfo()$plotMaximum, 30)
    moved <- toMaximum()
    expect_gt(length(moved), before)
    expect_equal(as.numeric(moved[[length(moved)]]$message$value), 30)
    # The browser reports it: the plot ends where it did, and nothing more is sent
    count <- length(moved)
    session$setInputs(maximum = 30)
    session$elapse(500)
    expect_equal(plotInfo()$plotMaximum, 30)
    expect_equal(length(toMaximum()), count)
    # A Max time the plot does not outgrow is left alone
    session$setInputs(maximum = 60)
    session$elapse(500)
    expect_equal(plotInfo()$plotMaximum, 60)
    expect_equal(length(toMaximum()), count)
    # The record of the move: the Max time chosen was 5 while the slider
    # showed the 30 the app set, and any other value since is the user's
    # own, a return to 30 included (Devin's review of PR 1036)
    expect_equal(chosenMaximum(), 60)
    session$setInputs(maximum = 5)
    session$elapse(500)
    session$setInputs(maximum = 30)
    session$elapse(500)
    expect_equal(chosenMaximum(), 5)
    session$setInputs(maximum = 60)
    session$elapse(500)
    session$setInputs(maximum = 30)
    session$elapse(500)
    expect_equal(chosenMaximum(), 30)
  })
})
