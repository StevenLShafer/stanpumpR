test_that("checkNumericCovariates correctly identifies out of bounds input", {
  expect_true(checkNumericCovariates(21, 70, 170))
  expect_false(checkNumericCovariates(0, 0, 0))
  expect_false(checkNumericCovariates(MIN_AGE - 1, 70, 170))
  expect_false(checkNumericCovariates(MAX_AGE + 1, 70, 170))
  expect_false(checkNumericCovariates(5, MIN_WEIGHT - 1, 170))
  expect_false(checkNumericCovariates(5, MAX_WEIGHT + 1, 170))
  expect_false(checkNumericCovariates(5, 70, MIN_HEIGHT - 1))
  expect_false(checkNumericCovariates(5, 70, MAX_HEIGHT + 1))
})

test_that("dose tables are validated at the server trust boundary", {
  valid <- data.frame(Drug = "propofol", Time = "0", Dose = "10", Units = "mg")
  expect_true(validateDoseTableInput(valid))

  malicious <- valid
  malicious$Drug <- "<img src=x onerror=alert(1)>"
  expect_error(validateDoseTableInput(malicious), "unknown drug")

  malicious <- valid
  malicious$Time <- "0<script>"
  expect_error(validateDoseTableInput(malicious), "invalid time")

  excessive <- valid
  excessive$Dose <- Inf
  expect_error(validateDoseTableInput(excessive), "finite")

  too_many <- valid[rep(1, MAX_DOSE_ROWS + 1L), ]
  expect_error(validateDoseTableInput(too_many), "row limit")
})

test_that("event tables reject unknown and excessive input", {
  defaults <- getEventDefaults()
  valid <- data.frame(Time = "0", Event = defaults$Event[[1]])
  expect_true(validateEventTableInput(valid, defaults))

  invalid <- valid
  invalid$Event <- "<script>alert(1)</script>"
  expect_error(validateEventTableInput(invalid, defaults), "unknown event")
})

test_that("email recipients must belong to an explicit allowlist", {
  expect_true(isEmailAllowed("doctor@hospital.example", "hospital.example"))
  expect_false(isEmailAllowed("doctor@outside.example", "hospital.example"))
  expect_false(isEmailAllowed("doctor@hospital.example", character(0)))
  expect_false(isEmailAllowed("not-an-email", "hospital.example"))
})

test_that("target tables are bounded and numeric", {
  valid <- data.frame(Time = "10", Target = 2)
  expect_true(validateTargetTableInput(valid))
  invalid <- valid
  invalid$Target <- Inf
  expect_error(validateTargetTableInput(invalid), "finite")
  invalid$Target <- "not-a-number"
  expect_error(validateTargetTableInput(invalid), "finite")
  too_many <- valid[rep(1, MAX_TARGET_ROWS + 1L), ]
  expect_error(validateTargetTableInput(too_many), "row limit")
})
