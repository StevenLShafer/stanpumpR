# Carbon's (Gemini's) second repository audit, of 7e9b4f48, 2026-11-19: its
# regression check of PR #767, which addressed the first.  One test per
# finding that reproduced, each failing on the code before its fix.  The IDs
# are the audit's (A2-F1 to A2-F27).

test_that("each of fluoxetine's cohort patients receives the published clearance (A2-F1)", {
  # Han 2025's cohort: 59 kg, 17 years; 171.8 cm men, 159.7 cm women.  CL/F is
  # a women's intercept (2.91 L/h) with a men's factor (1.165), so with the
  # switch on each sex is anchored at its own typical patient.
  clh <- function(height, sex) fluoxetine(59, height, 17, sex, TRUE)$PK$default$cl1 * 60
  expect_equal(clh(159.7, "female"), 2.91, tolerance = 1e-12)
  expect_equal(clh(171.8, "male"), 2.91 * 1.165, tolerance = 1e-12)
  # V/F has no sex term and stays anchored at the pooled patient
  pooled <- cohortSizeFactors(groups = FLUOXETINE_COHORT$groups)
  expect_equal(fluoxetine(59, 159.7, 17, "female", TRUE)$PK$default$v1,
               24.9 * pkSizeFactors(59, 159.7, 17, "female")$volume / pooled$volume)
  # the switch off is the published model, unchanged
  expect_equal(fluoxetine(59, 159.7, 17, "female", FALSE)$PK$default$cl1 * 60, 2.91)
  expect_equal(fluoxetine(59, 171.8, 17, "male", FALSE)$PK$default$cl1 * 60, 2.91 * 1.165)
})

test_that("each of etoposide's cohort patients receives the published central volume (A2-F1)", {
  # Krogh-Madsen 2012: 79.4 kg, 170.1 cm, 52.3 years; V1 9.13 L in men,
  # x (1 - 0.2793) in women
  v1 <- function(sex) etoposideKroghMadsen(79.4, 170.1, 52.3, sex, TRUE)$PK$default$v1
  expect_equal(v1("male"), 9.13, tolerance = 1e-12)
  expect_equal(v1("female"), 9.13 * (1 - 0.2793), tolerance = 1e-12)
})

test_that("cohortSexSizeFactors() takes only the patient's own sex", {
  cohort <- list(weight = 80, height = 170, age = 50, men = 3, women = 7)
  expect_equal(cohortSexSizeFactors(cohort, SEX_MALE),
               cohortSizeFactors(80, 50, 1, 0, height = 170))
  expect_equal(cohortSexSizeFactors(cohort, SEX_FEMALE),
               cohortSizeFactors(80, 50, 0, 1, height = 170))
  # a cohort of men only still anchors a woman, at a woman of its demographics
  expect_equal(cohortSexSizeFactors(modifyList(cohort, list(women = 0)), SEX_FEMALE),
               cohortSizeFactors(80, 50, 0, 1, height = 170))
})

test_that("the local anaesthetics' absorption rate follows the switch (A2-F2)", {
  # Each rate was fitted with the disposition its switch position gives:
  # anchored (on) or published (off).  The anchored rate with the switch off
  # put the calibration peaks 22% and 14% low.
  expect_equal(bupivacaine(70, 170, 40, "male", TRUE)$PK$default$ka_RA, 0.011958)
  expect_equal(bupivacaine(70, 170, 40, "male", FALSE)$PK$default$ka_RA, 0.0187)
  expect_equal(ropivacaine(70, 170, 40, "male", TRUE)$PK$default$ka_RA, 0.004749)
  expect_equal(ropivacaine(70, 170, 40, "male", FALSE)$PK$default$ka_RA, 0.00612)
})

test_that("a Procedure start with am or pm is read as such, and trailing text is refused (A2-F7)", {
  expect_equal(hourMinute(c("2:30 pm", "2:30 PM", "2:30pm", "2:30 p.m.")), rep(14 * 60 + 30, 4))
  expect_equal(hourMinute(c("12:30 am", "12:30 pm", "9:00 am")), c(30, 12 * 60 + 30, 9 * 60))
  expect_true(all(is.na(hourMinute(c("12:30abc", "14:30 pm", "0:30 am", "8:75", "25:00", "")))))
  expect_false(isValidReferenceTime("12:30abc"))
  expect_true(isValidReferenceTime("2:30 pm"))
  # the forms read before are read as before
  expect_equal(hourMinute(c("08:30", "8:30", "0830", "8.30", "08h30", "8:30:15", "24:00", "23:59", "8:5")),
               c(510, 510, 510, 510, 510, 510, 0, 1439, 485))
  # an afternoon start: a 15:00 dose is 30 minutes in, not 12 h 30 min
  expect_equal(clockTimeToDelta("2:30 pm", "15:00"), 30)
})

test_that("the clock-time converters take non-finite times and an NA reference (A2-F8)", {
  expect_identical(deltaToClockTime("08:00", c(0, Inf, -Inf, NaN, NA, 59.6)),
                   c("08:00", NA, NA, NA, NA, "09:00"))
  expect_identical(deltaToClockTime(NA_character_, c(10, 20)), c(NA_character_, NA_character_))
  expect_identical(clockTimeToDelta(NA_character_, "09:00"), NA_real_)
})

test_that("an edited comment sent before the browser unticks the box is not sent (A2-F15)", {
  calls <- 0
  local_mocked_bindings(sendSlide = function(...) { calls <<- calls + 1; TRUE })
  local_mocked_bindings(outputComments = function(...) {})
  local_mocked_bindings(shinyalert = function(...) NULL, .package = "shinyalert")
  oldConfig <- .sprglobals$config
  .sprglobals$config <- DEFAULT_CONFIG
  on.exit(.sprglobals$config <- oldConfig, add = TRUE)
  dt <- doseTableInit[1, ]
  dt[1, ] <- list("propofol", "0", "2", "mg/kg")
  dt[2, ] <- list("", "", "", "")
  rownames(dt) <- NULL
  shiny::testServer(app_server, {
    session$setInputs(
      timeUnits = "minutes", timeMode = "relative", referenceTime = "08:00", maximum = "60",
      plotWidth = 800, yaxisHeight = 200, plasmaLinetype = "blank", effectsiteLinetype = "solid",
      weight = 70, weightUnit = "1", height = 170, heightUnit = "1", age = 40, ageUnit = "1",
      sex = "male", showThreshold = FALSE, normalization = "none", typical = "Range", logY = FALSE)
    session$env$setDoseTable(dt, timeFormat("minutes", "relative"))
    session$flushReact()
    session$setInputs(recipient = "user@example.com", emailComments = "teaching case")
    session$setInputs(commentSafe = TRUE)
    # the edit and Send reach the server together, the box still ticked there
    session$setInputs(emailComments = "John Doe MRN 12345", sendSlide = 1)
    expect_equal(calls, 0)
    # ticked again for the comment as it stands, it is sent
    session$setInputs(commentSafe = FALSE)
    session$setInputs(commentSafe = TRUE)
    session$setInputs(sendSlide = 2)
    expect_equal(calls, 1)
  })
})
