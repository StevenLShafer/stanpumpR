sample_drug_defaults <- data.frame(Drug = c("propofol", "fentanyl"))
sample_event_defaults <- data.frame(Event = c("Induction", "Intubation", "CPB Start"))

test_that("dose table: full rows pass", {
  valid <- data.frame(Drug = "propofol", Time = "0", Dose = "10", Units = "mg")
  expect_true(validateDoseTableInput(valid, sample_drug_defaults))

  rows <- data.frame(
    Drug  = c("propofol", "fentanyl", "propofol"),
    Time  = c("0", "10", "08:30"),
    Dose  = c("200", "100", "50"),
    Units = c("mg", "mcg", "mcg/kg/min")
  )
  expect_true(validateDoseTableInput(rows, sample_drug_defaults))

  rows <- data.frame(
    Drug  = c("propofol", "fentanyl", "propofol", "", ""),
    Time  = c("0", "10", "08:30", "", ""),
    Dose  = c("200", "100", "50", "", ""),
    Units = c("mg", "mcg", "mcg/kg/min", "", "")
  )
  expect_true(validateDoseTableInput(rows, sample_drug_defaults))

  rows <- data.frame(
    Drug  = rep("propofol", 4),
    Time  = c("0", "5", "10", "15"),
    Dose  = c("0", "10", "150", "2.5"),
    Units = c("mg", "mg/kg", "mcg/kg/min", "mg/hr")
  )
  expect_true(validateDoseTableInput(rows, sample_drug_defaults))

  expect_true(validateDoseTableInput(doseTableInit))
})

test_that("dose table: blank placeholder rows are allowed, not rejected", {
  all_blank <- data.frame(Drug = "", Time = "", Dose = "", Units = "")
  expect_true(validateDoseTableInput(all_blank, sample_drug_defaults))

  all_blank <- data.frame(Drug = rep("", 5), Time = rep("", 5), Dose = rep("", 5), Units = rep("", 5))
  expect_true(validateDoseTableInput(all_blank, sample_drug_defaults))

  mixed <- data.frame(
    Drug  = c("propofol", ""),
    Time  = c("0", ""),
    Dose  = c("10", ""),
    Units = c("mg", "")
  )
  expect_true(validateDoseTableInput(mixed, sample_drug_defaults))
})

test_that("dose table: rejects input that isn't a data frame", {
  expect_error(validateDoseTableInput(matrix(1:4, 2, 2)), "structure")
  expect_error(validateDoseTableInput(list(Drug = "propofol")), "structure")
})

test_that("dose table: rejects a data frame missing a required column", {
  missing_units <- data.frame(Drug = "propofol", Time = "0", Dose = "10")
  expect_error(validateDoseTableInput(missing_units), "structure")
})

test_that("dose table: rejects a dose table exceeding the row limit", {
  valid <- data.frame(Drug = "propofol", Time = "0", Dose = "10", Units = "mg")
  not_too_many <- valid[rep(1, MAX_DOSE_ROWS), ]
  expect_true(validateDoseTableInput(not_too_many, sample_drug_defaults))
  too_many <- valid[rep(1, MAX_DOSE_ROWS + 1), ]
  expect_error(validateDoseTableInput(too_many, sample_drug_defaults), "row limit")
})

test_that("dose table: rejects long strings for Drug, Time, and Units", {
  overlong_drug <- data.frame(
    Drug = strrep("a", MAX_DRUGNAME_LENGTH + 1L), Time = "0", Dose = "10", Units = "mg"
  )
  expect_error(validateDoseTableInput(overlong_drug, sample_drug_defaults), "too long")

  overlong_time <- data.frame(
    Drug = "propofol", Time = strrep("1", MAX_TIME_STRING_LENGTH + 1L), Dose = "10", Units = "mg"
  )
  expect_error(validateDoseTableInput(overlong_time, sample_drug_defaults), "too long")

  overlong_units <- data.frame(
    Drug = "propofol", Time = "0", Dose = "10", Units = strrep("m", MAX_UNIT_STRING_LENGTH + 1L)
  )
  expect_error(validateDoseTableInput(overlong_units, sample_drug_defaults), "too long")
})

test_that("dose table: rejects a drug outside the allowlist", {
  bad_drug <- data.frame(Drug = "baddrug", Time = "0", Dose = "10", Units = "mg")
  expect_error(validateDoseTableInput(bad_drug, sample_drug_defaults), "unknown drug")

  bad_drug <- data.frame(Drug = "propofol2", Time = "0", Dose = "10", Units = "mg")
  expect_error(validateDoseTableInput(bad_drug, sample_drug_defaults), "unknown drug")
})

test_that("dose table: rejects dose units outside the allowlist", {
  bad_units <- data.frame(Drug = "propofol", Time = "0", Dose = "10", Units = "lightyears")
  expect_error(validateDoseTableInput(bad_units, sample_drug_defaults), "unknown dose units")
})

test_that("dose table: rejects wrong doses", {
  ok <- data.frame(Drug = "propofol", Time = "0", Dose = "0", Units = "mg")
  expect_true(validateDoseTableInput(ok, sample_drug_defaults))
  non_finite <- data.frame(Drug = "propofol", Time = "0", Dose = "Inf", Units = "mg")
  expect_error(validateDoseTableInput(non_finite, sample_drug_defaults), "finite")
  negative <- data.frame(Drug = "propofol", Time = "0", Dose = "-5", Units = "mg")
  expect_error(validateDoseTableInput(negative, sample_drug_defaults), "non-negative")
  too_big <- data.frame(Drug = "propofol", Time = "0", Dose = as.character(MAX_DOSE_VALUE + 1), Units = "mg")
  expect_error(validateDoseTableInput(too_big, sample_drug_defaults), "permitted limit")
})

test_that("dose table: rejects an invalid time value", {
  bad_time <- data.frame(Drug = "propofol", Time = "0a", Dose = "10", Units = "mg")
  expect_error(validateDoseTableInput(bad_time, sample_drug_defaults), "invalid time")
})

test_that("dose table: a bad row is caught regardless of its position in the table", {
  bad_row_at <- function(i, column, value) {
    rows <- data.frame(
      Drug  = rep("propofol", 3),
      Time  = c("0", "10", "20"),
      Dose  = c("10", "20", "30"),
      Units = rep("mg", 3)
    )
    rows[[column]][i] <- value
    rows
  }

  expect_error(validateDoseTableInput(bad_row_at(1, "Drug", "evil"), sample_drug_defaults), "unknown drug")
  expect_error(validateDoseTableInput(bad_row_at(2, "Drug", "evil"), sample_drug_defaults), "unknown drug")
  expect_error(validateDoseTableInput(bad_row_at(3, "Drug", "evil"), sample_drug_defaults), "unknown drug")

  expect_error(validateDoseTableInput(bad_row_at(3, "Units", "bogus"), sample_drug_defaults), "unknown dose units")
  expect_error(validateDoseTableInput(bad_row_at(2, "Time", "9:a"), sample_drug_defaults), "invalid time")
  expect_error(validateDoseTableInput(bad_row_at(3, "Dose", "-1"), sample_drug_defaults), "non-negative")
})

test_that("dose table: partial rows are dropped by cleanDT(), so their contents are never validated", {
  evil_but_no_dose <- data.frame(
    Drug = "system('echo pwned')", Time = "0", Dose = "", Units = "mg"
  )
  expect_true(validateDoseTableInput(evil_but_no_dose, sample_drug_defaults))

  evil_but_no_units <- data.frame(
    Drug = "system('echo pwned')", Time = "0", Dose = "10", Units = ""
  )
  expect_true(validateDoseTableInput(evil_but_no_units, sample_drug_defaults))

  bogus_units_but_no_drug <- data.frame(
    Drug = "", Time = "10", Dose = "5", Units = "bogus-units"
  )
  expect_true(validateDoseTableInput(bogus_units_but_no_drug, sample_drug_defaults))

  overlong_but_no_dose <- data.frame(
    Drug = strrep("a", MAX_DRUGNAME_LENGTH + 1L), Time = "0", Dose = "", Units = "mg"
  )
  expect_true(validateDoseTableInput(overlong_but_no_dose, sample_drug_defaults))

  over_max_dose_but_no_units <- data.frame(
    Drug = "propofol", Time = "0", Dose = as.character(MAX_DOSE_VALUE + 1), Units = ""
  )
  expect_true(validateDoseTableInput(over_max_dose_but_no_units, sample_drug_defaults))
})

test_that("dose table: a bad row still errors when it sits alongside a valid row", {
  rows <- data.frame(
    Drug  = c("propofol", "not-a-real-drug"),
    Time  = c("0", "10"),
    Dose  = c("10", "5"),
    Units = c("mg", "mg")
  )
  expect_error(validateDoseTableInput(rows, sample_drug_defaults), "unknown drug")
})

test_that("dose table: a table where every row is partial passes, since nothing survives cleaning", {
  rows <- data.frame(
    Drug  = c("evil-one", "evil-two"),
    Time  = c("0", "10"),
    Dose  = c("", ""),
    Units = c("mg", "mg")
  )
  expect_true(validateDoseTableInput(rows, sample_drug_defaults))
})

test_that("dose table: accepts Drug/Time/Units arriving as factors, not just characters", {
  factor_row <- data.frame(Drug = factor("propofol"), Time = factor("0"), Dose = "10", Units = factor("mg"))
  expect_true(validateDoseTableInput(factor_row, sample_drug_defaults))

  factor_row_bad_drug <- factor_row
  factor_row_bad_drug$Drug <- factor("not-a-real-drug")
  expect_error(validateDoseTableInput(factor_row_bad_drug, sample_drug_defaults), "unknown drug")
})

test_that("event table: full rows pass", {
  valid <- data.frame(Time = "0", Event = "Induction")
  expect_true(validateEventTableInput(valid, sample_event_defaults))

  rows <- data.frame(
    Time  = c("0", "10", "08:30"),
    Event = c("Induction", "Intubation", "CPB Start")
  )
  expect_true(validateEventTableInput(rows, sample_event_defaults))

  expect_true(validateEventTableInput(eventTableInit))
})

test_that("event table: extra columns beyond Time and Event are allowed", {
  with_fill <- data.frame(Time = "0", Event = "Induction", Fill = "green")
  expect_true(validateEventTableInput(with_fill, sample_event_defaults))
})

test_that("event table: rejects input that isn't a data frame or is missing a column", {
  expect_error(validateEventTableInput(list(Time = "0", Event = "Induction")), "structure")
  expect_error(validateEventTableInput(matrix(1:4, 2, 2)), "structure")
  expect_error(validateEventTableInput(data.frame(Time = "0")), "structure")
  expect_error(validateEventTableInput(data.frame(Event = "Induction")), "structure")
})

test_that("event table: rejects exceeding the row limit", {
  not_too_many <- data.frame(Time = rep("0", MAX_EVENT_ROWS), Event = rep("Induction", MAX_EVENT_ROWS))
  expect_true(validateEventTableInput(not_too_many, sample_event_defaults))

  too_many <- data.frame(Time = rep("0", MAX_EVENT_ROWS + 1), Event = rep("Induction", MAX_EVENT_ROWS + 1))
  expect_error(validateEventTableInput(too_many, sample_event_defaults), "row limit")
})

test_that("event table: rejects long strings for Time and Event", {
  overlong_event <- data.frame(Time = "0", Event = strrep("a", MAX_DRUGNAME_LENGTH + 1L))
  expect_error(validateEventTableInput(overlong_event, sample_event_defaults), "too long")

  overlong_time <- data.frame(Time = strrep("1", MAX_TIME_STRING_LENGTH + 1L), Event = "Induction")
  expect_error(validateEventTableInput(overlong_time, sample_event_defaults), "too long")
})

test_that("event table: rejects an event outside the allowlist", {
  bad_event <- data.frame(Time = "0", Event = "Nonsense")
  expect_error(validateEventTableInput(bad_event, sample_event_defaults), "unknown event")
})

test_that("event table: rejects an invalid time value", {
  bad_time <- data.frame(Time = "0a", Event = "Induction")
  expect_error(validateEventTableInput(bad_time, sample_event_defaults), "invalid time")
})

test_that("event table: a bad row is caught regardless of its position", {
  bad_row_at <- function(i, column, value) {
    rows <- data.frame(
      Time  = c("0", "10", "20"),
      Event = c("Induction", "Intubation", "CPB Start")
    )
    rows[[column]][i] <- value
    rows
  }

  expect_error(validateEventTableInput(bad_row_at(1, "Event", "evil"), sample_event_defaults), "unknown event")
  expect_error(validateEventTableInput(bad_row_at(2, "Event", "evil"), sample_event_defaults), "unknown event")
  expect_error(validateEventTableInput(bad_row_at(3, "Event", "evil"), sample_event_defaults), "unknown event")
  expect_error(validateEventTableInput(bad_row_at(2, "Time", "9:a"), sample_event_defaults), "invalid time")
})

test_that("event table: blank rows are rejected, unlike the dose table", {
  all_blank <- data.frame(Time = "", Event = "")
  expect_error(validateEventTableInput(all_blank, sample_event_defaults), "unknown event")

  trailing_blank <- data.frame(Time = c("0", ""), Event = c("Induction", ""))
  expect_error(validateEventTableInput(trailing_blank, sample_event_defaults), "unknown event")
})

test_that("event table: accepts Time and Event that aren't character columns", {
  numeric_time <- data.frame(Time = 0, Event = "Induction")
  expect_true(validateEventTableInput(numeric_time, sample_event_defaults))

  factor_event <- data.frame(Time = "0", Event = factor("Induction"))
  expect_true(validateEventTableInput(factor_event, sample_event_defaults))
})

test_that("target table: full rows pass", {
  expect_true(validateTargetTableInput(data.frame(Time = "10", Target = 2)))

  expect_true(validateTargetTableInput(data.frame(Time = "10", Target = "2")))

  rows <- data.frame(
    Time   = c("0", "10", "08:30"),
    Target = c("1", "2.5", "4")
  )
  expect_true(validateTargetTableInput(rows))

  expect_true(validateTargetTableInput(data.frame(Time = character(0), Target = character(0))))
})

test_that("target table: blank rows are accepted", {
  all_blank <- data.frame(Time = rep("", 6), Target = rep("", 6))
  expect_true(validateTargetTableInput(all_blank))

  partly_filled <- data.frame(
    Time   = c("10", "20", "", "", "", ""),
    Target = c("2", "3", "", "", "", "")
  )
  expect_true(validateTargetTableInput(partly_filled))
})

test_that("target table: rejects input that isn't a data frame or is missing a column", {
  expect_error(validateTargetTableInput(list(Time = "10", Target = 2)), "structure")
  expect_error(validateTargetTableInput(matrix(1:4, 2, 2)), "structure")
  expect_error(validateTargetTableInput(data.frame(Time = "10")), "structure")
  expect_error(validateTargetTableInput(data.frame(Target = 2)), "structure")
})

test_that("target table: rejects exceeding the row limit", {
  not_too_many <- data.frame(Time = rep("10", MAX_TARGET_ROWS), Target = rep("2", MAX_TARGET_ROWS))
  expect_true(validateTargetTableInput(not_too_many))

  too_many <- data.frame(Time = rep("10", MAX_TARGET_ROWS + 1), Target = rep("2", MAX_TARGET_ROWS + 1))
  expect_error(validateTargetTableInput(too_many), "row limit")
})

test_that("target table: rejects a long or invalid time", {
  ok_length <- data.frame(Time = strrep("1", MAX_TIME_STRING_LENGTH), Target = "2")
  expect_true(validateTargetTableInput(ok_length))

  overlong_time <- data.frame(Time = strrep("1", MAX_TIME_STRING_LENGTH + 1L), Target = "2")
  expect_error(validateTargetTableInput(overlong_time), "overlong time")

  bad_time <- data.frame(Time = "0a", Target = "2")
  expect_error(validateTargetTableInput(bad_time), "invalid time")
})

test_that("target table: rejects wrong target concentrations", {
  ok <- data.frame(Time = "10", Target = "0")
  expect_true(validateTargetTableInput(ok))

  at_max <- data.frame(Time = "10", Target = as.character(MAX_DOSE_VALUE))
  expect_true(validateTargetTableInput(at_max))

  non_finite <- data.frame(Time = "10", Target = Inf)
  expect_error(validateTargetTableInput(non_finite), "finite")

  not_a_number <- data.frame(Time = "10", Target = "not-a-number")
  expect_error(validateTargetTableInput(not_a_number), "finite")

  negative <- data.frame(Time = "10", Target = "-5")
  expect_error(validateTargetTableInput(negative), "permitted limit")

  too_big <- data.frame(Time = "10", Target = as.character(MAX_DOSE_VALUE + 1))
  expect_error(validateTargetTableInput(too_big), "permitted limit")
})

test_that("target table: a bad row is caught regardless of its position", {
  bad_row_at <- function(i, column, value) {
    rows <- data.frame(
      Time   = c("0", "5", "10"),
      Target = c("1", "2", "3")
    )
    rows[[column]][i] <- value
    rows
  }

  expect_error(validateTargetTableInput(bad_row_at(1, "Target", "-9")), "permitted limit")
  expect_error(validateTargetTableInput(bad_row_at(2, "Target", "-9")), "permitted limit")
  expect_error(validateTargetTableInput(bad_row_at(3, "Target", "-9")), "permitted limit")
  expect_error(validateTargetTableInput(bad_row_at(2, "Time", "9:a")), "invalid time")
})

test_that("target table: extra columns and non-character types are accepted", {
  with_extra <- data.frame(Time = "10", Target = "2", Extra = "ignored")
  expect_true(validateTargetTableInput(with_extra))

  factor_target <- data.frame(Time = "10", Target = factor("2"))
  expect_true(validateTargetTableInput(factor_target))
})
