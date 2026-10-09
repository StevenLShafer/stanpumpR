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
