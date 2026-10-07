sample_drug_defaults <- data.frame(Drug = c("propofol", "fentanyl"))
sample_event_defaults <- data.frame(Event = c("Induction", "Intubation", "CPB Start"))

test_that("validateDoseTableInput: full rows pass", {
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

  # The constant-rate oral unit (poRateUnits), amiodarone's
  rows <- data.frame(Drug = "amiodarone", Time = c("0", "2880"), Dose = c("1600", "0"),
                     Units = "mg/day PO")
  expect_true(validateDoseTableInput(rows, data.frame(Drug = "amiodarone")))
})

test_that("validateDoseTableInput: blank placeholder rows are allowed, not rejected", {
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

test_that("validateDoseTableInput: rejects input that isn't a data frame", {
  expect_error(validateDoseTableInput(matrix(1:4, 2, 2)), "structure")
  expect_error(validateDoseTableInput(list(Drug = "propofol")), "structure")
})

test_that("validateDoseTableInput: rejects a data frame missing a required column", {
  missing_units <- data.frame(Drug = "propofol", Time = "0", Dose = "10")
  expect_error(validateDoseTableInput(missing_units), "structure")
})

test_that("validateDoseTableInput: rejects a dose table exceeding the row limit", {
  valid <- data.frame(Drug = "propofol", Time = "0", Dose = "10", Units = "mg")
  not_too_many <- valid[rep(1, MAX_DOSE_ROWS), ]
  expect_true(validateDoseTableInput(not_too_many, sample_drug_defaults))
  too_many <- valid[rep(1, MAX_DOSE_ROWS + 1), ]
  expect_error(validateDoseTableInput(too_many, sample_drug_defaults), "row limit")
})

test_that("validateDoseTableInput: rejects long strings for Drug, Time, and Units", {
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

test_that("validateDoseTableInput: rejects a drug outside the allowlist", {
  bad_drug <- data.frame(Drug = "baddrug", Time = "0", Dose = "10", Units = "mg")
  expect_error(validateDoseTableInput(bad_drug, sample_drug_defaults), "unknown drug")

  bad_drug <- data.frame(Drug = "propofol2", Time = "0", Dose = "10", Units = "mg")
  expect_error(validateDoseTableInput(bad_drug, sample_drug_defaults), "unknown drug")
})

test_that("validateDoseTableInput: rejects dose units outside the allowlist", {
  bad_units <- data.frame(Drug = "propofol", Time = "0", Dose = "10", Units = "lightyears")
  expect_error(validateDoseTableInput(bad_units, sample_drug_defaults), "unknown dose units")
})

test_that("validateDoseTableInput: accepts the inhaled gas units", {
  gas_defaults <- data.frame(Drug = c("nitrousOxide", "sevoflurane"))
  rows <- data.frame(
    Drug  = c("nitrousOxide", "sevoflurane"),
    Time  = c("0", "0"),
    Dose  = c("4", "2"),
    Units = c("L/min", "%")
  )
  expect_true(validateDoseTableInput(rows, gas_defaults))
})

test_that("ensureGasVentilation: leaves a table with no gases alone", {
  expect_identical(ensureGasVentilation(doseTableInit, 70), doseTableInit)
  # A ventilation row on its own is not a gas being given.
  only_vent <- data.frame(Drug = c("ventilation", ""), Time = c("0", ""), Dose = c("0", ""), Units = c("L/min", ""))
  expect_identical(ensureGasVentilation(only_vent, 70), only_vent)
})

test_that("ensureGasVentilation: adds a ventilation row as soon as a gas is named", {
  # Only the drug name typed so far: no time, dose or units yet.
  DT <- data.frame(
    Drug  = c("propofol", "nitrousOxide", "", ""),
    Time  = c("0", "", "", ""),
    Dose  = c("0", "", "", ""),
    Units = c("mg", "", "", "")
  )
  out <- ensureGasVentilation(DT, weight = 60)
  expect_equal(nrow(out), nrow(DT) + 1L)
  # Inserted below the last filled row, above the trailing blanks.
  expect_equal(out$Drug, c("propofol", "nitrousOxide", "ventilation", "", ""))
  expect_equal(out[3, "Time"], "0")
  expect_equal(out[3, "Units"], "L/min")
  expect_equal(as.numeric(out[3, "Dose"]), defaultGasVentilation(60))
  expect_gt(as.numeric(out[3, "Dose"]), 0)
  expect_true(validateDoseTableInput(out))
  # Idempotent: a second pass changes nothing.
  expect_identical(ensureGasVentilation(out, weight = 60), out)
})

test_that("ensureGasVentilation: replaces a zero, blank or negative ventilation, keeps a positive one", {
  mk <- function(dose, time = "0", units = "L/min") data.frame(
    Drug = c("oxygen", "ventilation", ""), Time = c("0", time, ""),
    Dose = c("2", dose, ""), Units = c("L/min", units, "")
  )
  for (bad in c("0", "", "-3", "abc")) {
    out <- ensureGasVentilation(mk(bad), weight = 70)
    expect_equal(nrow(out), 3L)
    expect_equal(as.numeric(out$Dose[2]), defaultGasVentilation(70))
  }
  # Blank time and units are filled in too, or the row would be dropped on cleaning.
  out <- ensureGasVentilation(mk("", time = "", units = ""), weight = 70)
  expect_equal(out[2, "Time"], "0")
  expect_equal(out[2, "Units"], "L/min")

  expect_identical(ensureGasVentilation(mk("6"), weight = 70), mk("6"))
})

test_that("ensureGasVentilation: with the default in place the gases are actually delivered", {
  DT <- data.frame(
    Drug  = c("nitrousOxide", "oxygen", "air"),
    Time  = c("0", "0", "0"),
    Dose  = c("2", "1", "1"),
    Units = c("L/min", "L/min", "L/min")
  )
  sim <- simulateGases(cleanDoseTable(ensureGasVentilation(DT, weight = 60)), weight = 60, maximum = 60)
  alv <- function(g) sim$results$Y[sim$results$Drug == g & sim$results$Site == "Alveolar"]
  expect_gt(utils::tail(alv("nitrousOxide"), 1), 30)
  expect_gt(min(alv("oxygen")), 15)
})

test_that("validateDoseTableInput: rejects wrong doses", {
  ok <- data.frame(Drug = "propofol", Time = "0", Dose = "0", Units = "mg")
  expect_true(validateDoseTableInput(ok, sample_drug_defaults))
  non_finite <- data.frame(Drug = "propofol", Time = "0", Dose = "Inf", Units = "mg")
  expect_error(validateDoseTableInput(non_finite, sample_drug_defaults), "finite")
  negative <- data.frame(Drug = "propofol", Time = "0", Dose = "-5", Units = "mg")
  expect_error(validateDoseTableInput(negative, sample_drug_defaults), "non-negative")
  too_big <- data.frame(Drug = "propofol", Time = "0", Dose = as.character(MAX_DOSE_VALUE + 1), Units = "mg")
  expect_error(validateDoseTableInput(too_big, sample_drug_defaults), "permitted limit")
})

test_that("validateDoseTableInput: rejects an invalid time value", {
  bad_time <- data.frame(Drug = "propofol", Time = "0a", Dose = "10", Units = "mg")
  expect_error(validateDoseTableInput(bad_time, sample_drug_defaults), "invalid time")
})

test_that("validateDoseTableInput: a bad row is caught regardless of its position in the table", {
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

test_that("validateDoseTableInput: partial rows are dropped by cleanDoseTable(), so their contents are never validated", {
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

test_that("validateDoseTableInput: a bad row still errors when it sits alongside a valid row", {
  rows <- data.frame(
    Drug  = c("propofol", "not-a-real-drug"),
    Time  = c("0", "10"),
    Dose  = c("10", "5"),
    Units = c("mg", "mg")
  )
  expect_error(validateDoseTableInput(rows, sample_drug_defaults), "unknown drug")
})

test_that("validateDoseTableInput: a table where every row is partial passes, since nothing survives cleaning", {
  rows <- data.frame(
    Drug  = c("evil-one", "evil-two"),
    Time  = c("0", "10"),
    Dose  = c("", ""),
    Units = c("mg", "mg")
  )
  expect_true(validateDoseTableInput(rows, sample_drug_defaults))
})

test_that("validateDoseTableInput: accepts Drug/Time/Units arriving as factors, not just characters", {
  factor_row <- data.frame(Drug = factor("propofol"), Time = factor("0"), Dose = "10", Units = factor("mg"))
  expect_true(validateDoseTableInput(factor_row, sample_drug_defaults))

  factor_row_bad_drug <- factor_row
  factor_row_bad_drug$Drug <- factor("not-a-real-drug")
  expect_error(validateDoseTableInput(factor_row_bad_drug, sample_drug_defaults), "unknown drug")
})

test_that("validateEventTableInput: full rows pass", {
  valid <- data.frame(Time = "0", Event = "Induction")
  expect_true(validateEventTableInput(valid, sample_event_defaults))

  rows <- data.frame(
    Time  = c("0", "10", "510"),
    Event = c("Induction", "Intubation", "CPB Start")
  )
  expect_true(validateEventTableInput(rows, sample_event_defaults))

  expect_true(validateEventTableInput(eventTableInit))
})

test_that("validateEventTableInput: event times are minutes, checked as numbers", {
  # Stored as numbers whatever the time unit.  As text, 1e5 minutes was
  # "1e+05", not a valid time string, and an event 69 days in stopped the plot.
  long <- data.frame(Time = c(0, 100000, 200000, 524160, 1440 / 7),
                     Event = rep("Induction", 5))
  expect_true(validateEventTableInput(long, sample_event_defaults))
  expect_error(validateEventTableInput(data.frame(Time = -1, Event = "Induction"), sample_event_defaults),
               "invalid time")
  expect_error(validateEventTableInput(data.frame(Time = NA_real_, Event = "Induction"), sample_event_defaults),
               "invalid time")
  expect_error(validateEventTableInput(data.frame(Time = Inf, Event = "Induction"), sample_event_defaults),
               "invalid time")
  # a clock time is converted to minutes when the event is entered; one
  # stored as text is not a time
  expect_error(validateEventTableInput(data.frame(Time = "08:30", Event = "Induction"), sample_event_defaults),
               "invalid time")
})

test_that("validateEventTableInput: extra columns beyond Time and Event are allowed", {
  with_fill <- data.frame(Time = "0", Event = "Induction", Fill = "green")
  expect_true(validateEventTableInput(with_fill, sample_event_defaults))
})

test_that("validateEventTableInput: rejects input that isn't a data frame or is missing a column", {
  expect_error(validateEventTableInput(list(Time = "0", Event = "Induction")), "structure")
  expect_error(validateEventTableInput(matrix(1:4, 2, 2)), "structure")
  expect_error(validateEventTableInput(data.frame(Time = "0")), "structure")
  expect_error(validateEventTableInput(data.frame(Event = "Induction")), "structure")
})

test_that("validateEventTableInput: rejects exceeding the row limit", {
  not_too_many <- data.frame(Time = rep("0", MAX_EVENT_ROWS), Event = rep("Induction", MAX_EVENT_ROWS))
  expect_true(validateEventTableInput(not_too_many, sample_event_defaults))

  too_many <- data.frame(Time = rep("0", MAX_EVENT_ROWS + 1), Event = rep("Induction", MAX_EVENT_ROWS + 1))
  expect_error(validateEventTableInput(too_many, sample_event_defaults), "row limit")
})

test_that("validateEventTableInput: rejects long strings for Time and Event", {
  overlong_event <- data.frame(Time = "0", Event = strrep("a", MAX_DRUGNAME_LENGTH + 1L))
  expect_error(validateEventTableInput(overlong_event, sample_event_defaults), "too long")

  overlong_time <- data.frame(Time = strrep("1", MAX_TIME_STRING_LENGTH + 1L), Event = "Induction")
  expect_error(validateEventTableInput(overlong_time, sample_event_defaults), "too long")
})

test_that("validateEventTableInput: rejects an event outside the allowlist", {
  bad_event <- data.frame(Time = "0", Event = "Nonsense")
  expect_error(validateEventTableInput(bad_event, sample_event_defaults), "unknown event")
})

test_that("validateEventTableInput: rejects an invalid time value", {
  bad_time <- data.frame(Time = "0a", Event = "Induction")
  expect_error(validateEventTableInput(bad_time, sample_event_defaults), "invalid time")
})

test_that("validateEventTableInput: a bad row is caught regardless of its position", {
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

test_that("validateEventTableInput: blank rows are rejected, unlike the dose table", {
  all_blank <- data.frame(Time = "", Event = "")
  expect_error(validateEventTableInput(all_blank, sample_event_defaults), "unknown event")

  trailing_blank <- data.frame(Time = c("0", ""), Event = c("Induction", ""))
  expect_error(validateEventTableInput(trailing_blank, sample_event_defaults), "unknown event")
})

test_that("validateEventTableInput: accepts Time and Event that aren't character columns", {
  numeric_time <- data.frame(Time = 0, Event = "Induction")
  expect_true(validateEventTableInput(numeric_time, sample_event_defaults))

  factor_event <- data.frame(Time = "0", Event = factor("Induction"))
  expect_true(validateEventTableInput(factor_event, sample_event_defaults))
})

test_that("validateTargetTableInput: full rows pass", {
  expect_true(validateTargetTableInput(data.frame(Time = "10", Target = 2)))

  expect_true(validateTargetTableInput(data.frame(Time = "10", Target = "2")))

  rows <- data.frame(
    Time   = c("0", "10", "08:30"),
    Target = c("1", "2.5", "4")
  )
  expect_true(validateTargetTableInput(rows))

  expect_true(validateTargetTableInput(data.frame(Time = character(0), Target = character(0))))
})

test_that("validateTargetTableInput: blank rows are accepted", {
  all_blank <- data.frame(Time = rep("", 6), Target = rep("", 6))
  expect_true(validateTargetTableInput(all_blank))

  partly_filled <- data.frame(
    Time   = c("10", "20", "", "", "", ""),
    Target = c("2", "3", "", "", "", "")
  )
  expect_true(validateTargetTableInput(partly_filled))
})

test_that("validateTargetTableInput: rejects input that isn't a data frame or is missing a column", {
  expect_error(validateTargetTableInput(list(Time = "10", Target = 2)), "structure")
  expect_error(validateTargetTableInput(matrix(1:4, 2, 2)), "structure")
  expect_error(validateTargetTableInput(data.frame(Time = "10")), "structure")
  expect_error(validateTargetTableInput(data.frame(Target = 2)), "structure")
})

test_that("validateTargetTableInput: rejects exceeding the row limit", {
  not_too_many <- data.frame(Time = rep("10", MAX_TARGET_ROWS), Target = rep("2", MAX_TARGET_ROWS))
  expect_true(validateTargetTableInput(not_too_many))

  too_many <- data.frame(Time = rep("10", MAX_TARGET_ROWS + 1), Target = rep("2", MAX_TARGET_ROWS + 1))
  expect_error(validateTargetTableInput(too_many), "row limit")
})

test_that("validateTargetTableInput: rejects a long or invalid time", {
  ok_length <- data.frame(Time = strrep("1", MAX_TIME_STRING_LENGTH), Target = "2")
  expect_true(validateTargetTableInput(ok_length))

  overlong_time <- data.frame(Time = strrep("1", MAX_TIME_STRING_LENGTH + 1L), Target = "2")
  expect_error(validateTargetTableInput(overlong_time), "overlong time")

  bad_time <- data.frame(Time = "0a", Target = "2")
  expect_error(validateTargetTableInput(bad_time), "invalid time")
})

test_that("validateTargetTableInput: rejects wrong target concentrations", {
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

test_that("validateTargetTableInput: a bad row is caught regardless of its position", {
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

test_that("validateTargetTableInput: extra columns and non-character types are accepted", {
  with_extra <- data.frame(Time = "10", Target = "2", Extra = "ignored")
  expect_true(validateTargetTableInput(with_extra))

  factor_target <- data.frame(Time = "10", Target = factor("2"))
  expect_true(validateTargetTableInput(factor_target))
})


test_that("drugHasNonZeroDoses detects any non-zero dose for a drug", {
  dt <- data.frame(
    Drug = c("propofol", "propofol", "fentanyl", "fentanyl", "fentanyl", "ketamine"),
    Dose = c("100", "0", "0", "", "abc", "0")
  )
  expect_true(drugHasNonZeroDoses(dt, "propofol"))
  expect_false(drugHasNonZeroDoses(dt, "fentanyl"))
  expect_false(drugHasNonZeroDoses(dt, "ketamine"))
  expect_false(drugHasNonZeroDoses(dt, "midazolam"))
})

test_that("ensureGasOxygen: adds a blank oxygen row when nitrous oxide is named, fills it once the flow is known", {
  named <- data.frame(
    Drug  = c("propofol", "nitrousOxide", ""),
    Time  = c("0", "0", ""),
    Dose  = c("0", "0", ""),
    Units = c("mg", "L/min", "")
  )
  out <- ensureGasOxygen(named)
  expect_equal(out$Drug, c("propofol", "nitrousOxide", "oxygen", ""))
  expect_equal(out[3, "Dose"], "")          # 21% of an unknown total
  expect_equal(out[3, "Time"], "0")
  expect_equal(out[3, "Units"], "L/min")

  # The user now enters the nitrous oxide flow: the blank oxygen dose is filled.
  out$Dose[2] <- "4"
  filled <- ensureGasOxygen(out)
  # 0.21 / 0.79 * 4 = 1.063 -> 1.1 L/min
  expect_equal(filled[3, "Dose"], "1.1")
  expect_identical(ensureGasOxygen(filled), filled)
})

test_that("ensureGasOxygen: never overwrites an oxygen dose the user entered, and ignores tables without nitrous oxide", {
  mk <- function(o2) data.frame(
    Drug = c("nitrousOxide", "oxygen"), Time = c("0", "0"),
    Dose = c("4", o2), Units = c("L/min", "L/min")
  )
  expect_identical(ensureGasOxygen(mk("2")), mk("2"))
  expect_identical(ensureGasOxygen(mk("0")), mk("0"))

  sevo <- data.frame(Drug = "sevoflurane", Time = "0", Dose = "2", Units = "%")
  expect_identical(ensureGasOxygen(sevo), sevo)
  expect_identical(ensureGasOxygen(doseTableInit), doseTableInit)
})

test_that("ensureGasOxygen: counts the oxygen an air flow already carries", {
  DT <- data.frame(
    Drug = c("nitrousOxide", "air"), Time = c("0", "0"),
    Dose = c("2", "2"), Units = c("L/min", "L/min")
  )
  out <- ensureGasOxygen(DT)
  Q_O2 <- as.numeric(out$Dose[out$Drug == "oxygen"])
  # (0.21 * 4 - 0.2093 * 2) / 0.79 = 0.533 -> 0.5
  expect_equal(Q_O2, 0.5)
  # Rounding the flow to 0.1 L/min moves the fraction off 21% slightly.
  expect_equal((Q_O2 + AIR_FRACTION_O2 * 2) / (Q_O2 + 4), 0.21, tolerance = 0.05)
})

test_that("roundGasFlows: rounds L/min gas rows to 0.1 and nothing else", {
  DT <- data.frame(
    Drug  = c("propofol", "oxygen", "nitrousOxide", "ventilation", "sevoflurane", "air"),
    Time  = rep("0", 6),
    Dose  = c("1.234", "1.26", "2", "4.449", "2.15", ""),
    Units = c("mg", "L/min", "L/min", "L/min", "%", "L/min")
  )
  out <- roundGasFlows(DT)
  expect_equal(out$Dose, c("1.234", "1.3", "2", "4.4", "2.15", ""))
  expect_identical(roundGasFlows(out), out)
  expect_identical(roundGasFlows(doseTableInit), doseTableInit)
})

test_that("applyGasTableRules: nitrous oxide alone yields oxygen and ventilation rows", {
  DT <- data.frame(
    Drug = c("nitrousOxide", ""), Time = c("0", ""),
    Dose = c("2", ""), Units = c("L/min", "")
  )
  out <- applyGasTableRules(DT, weight = 60)
  expect_equal(out$Drug, c("nitrousOxide", "oxygen", "ventilation", ""))
  expect_equal(out$Dose[2], "0.5")                       # 0.21 / 0.79 * 2 = 0.53
  expect_equal(as.numeric(out$Dose[3]), defaultGasVentilation(60))
  expect_true(validateDoseTableInput(out))
  expect_identical(applyGasTableRules(out, weight = 60), out)
  expect_identical(applyGasTableRules(doseTableInit, 60), doseTableInit)
})
