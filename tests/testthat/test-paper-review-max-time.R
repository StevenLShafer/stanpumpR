# Single Paper Review's Max time, from the code review of PR #682
# (2026-11-15): the Max time Load regimen sets is one the simulator accepts,
# and the limit is said once in days.

maxTimePaper <- function() {
  ex <- paperReviewExample()
  bolus <- function(day) list(route = "iv_bolus", time = day, amount = 1, mass_unit = "mg",
                              per_kg = "yes", per_time = "none", duration = NULL, time_units = "day")
  ex$regimens <- c(ex$regimens, list(list(
    id = "R9", description = "1 mg/kg on day 0 and day 200",
    doses = list(bolus(0), bolus(200)), location = "Methods")))
  ex
}

test_that("Load regimen sets a Max time within the limit the simulator holds it to", {
  ex <- maxTimePaper()
  testthat::local_mocked_bindings(paperReviewExample = function() ex)
  shiny::testServer(paperReviewServer, {
    sent <- list()
    session$sendInputMessage <- function(inputId, message)
      sent[[length(sent) + 1]] <<- list(id = inputId, message = message)
    session$setInputs(weight = 70, height = 170, age = 40, sex = "male", maximum = 600)
    session$setInputs(example = 1)
    session$setInputs(regimen = "R9", loadRegimen = 1)
    set <- Filter(function(m) identical(m$id, "maximum") && !is.null(m$message$value), sent)
    # updateNumericInput() sends the number as text
    value <- as.numeric(set[[length(set)]]$message$value)
    # Twice the regimen is 400 days, past the 365 the simulator takes: the
    # plot asked for a Max time the app had chosen itself
    minutes <- value * paperReviewMinutes(tableUnit())
    expect_lte(minutes, PAPER_REVIEW_MAX_TIME_LIMIT)
    expect_false(is.na(paperReviewMaxTimeMinutes(value, paperReviewMinutes(tableUnit()))))
    session$setInputs(maximum = value)
    expect_no_error(simulation())
  })
})

test_that("the Max time limit is said once in days", {
  ex <- maxTimePaper()
  testthat::local_mocked_bindings(paperReviewExample = function() ex)
  shiny::testServer(paperReviewServer, {
    session$setInputs(weight = 70, height = 170, age = 40, sex = "male", maximum = 600)
    session$setInputs(example = 1)
    session$setInputs(regimen = "R1", loadRegimen = 1)
    session$setInputs(timeUnit = "day")
    session$setInputs(maximum = 400)
    msg <- tryCatch({ simulation(); "" }, error = function(e) conditionMessage(e))
    expect_match(msg, "no longer than 365 days.", fixed = TRUE)
    expect_no_match(msg, "(365 days)", fixed = TRUE)
    session$setInputs(timeUnit = "h")
    session$setInputs(maximum = 1e6)
    msg <- tryCatch({ simulation(); "" }, error = function(e) conditionMessage(e))
    expect_match(msg, "8,760 hours (365 days).", fixed = TRUE)
  })
})
