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
})
