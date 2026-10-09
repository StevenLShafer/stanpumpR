test_that("suggest yields a table with the appropriate columns", {

  local_mocked_bindings(outputComments = function(...) {})

  drugList <- getDrugDefaultsGlobal(FALSE)$Drug

  input <- list(referenceTime="none", targetDrug="propofol")

  doseTable <- data.frame(Drug=input$targetDrug,Time=0,Dose=0,Units="mg")

  eventTable <- data.frame(
    Time = 0,
    Event = "Event"
  )

  age <- 50
  weight <- 60
  height <- 66*2.54
  sex <- "female"

  plotMaximum <- 60
  plotRecovery <- FALSE

  newDrugs <- recalculatePK(
    NULL,
    getDrugDefaultsGlobal(FALSE),
    doseTable,
    age, weight, height, sex
  )

  drugs <- processdoseTable(
    doseTable,
    eventTable,
    newDrugs,
    plotMaximum,
    plotRecovery
  )

  targetTable <- data.frame(
      Time = c("2","20",rep("",4)),
      Target = c("2","2",rep("",4))
  )

  endTime <- 60

  testTable <- suggest(input$targetDrug,
                       targetTable,
                       endTime,
                       drugs,
                       drugList,
                       eventTable,
                       input$referenceTime)

  expect_equal(names(testTable), c("Time", "Dose", "Units", "Drug"))
})

test_that("a target at time 0 is a target, not a blank row", {
  # Blank rows were told apart by their cleaned time being 0, which dropped a
  # real target at 0, and the doses then started at the next target.  The app
  # passes suggest() minutes, so a target at the procedure start is "0" too.
  local_mocked_bindings(outputComments = function(...) {})
  defaults <- getDrugDefaultsGlobal(FALSE)
  doseTable <- data.frame(Drug = "propofol", Time = 0, Dose = 0, Units = "mg")
  eventTable <- data.frame(Time = 0, Event = "Event")
  drugs <- processdoseTable(
    doseTable, eventTable,
    recalculatePK(NULL, defaults, doseTable, 50, 60, 66 * 2.54, "female"),
    60, FALSE
  )
  targetTable <- data.frame(Time = c("0", "20", rep("", 4)), Target = c("2", "3", rep("", 4)))
  testTable <- suggest("propofol", targetTable, 60, drugs, defaults$Drug, eventTable, REFERENCE_TIME_NONE)
  expect_equal(min(testTable$Time), 0)
  # one bolus per target
  expect_equal(sum(testTable$Units == defaults$Bolus.Units[defaults$Drug == "propofol"]), 2)
})

# The audit patient: 70 kg, 170 cm, 50 y, male
suggestTestDrugs <- function(drug) {
  doseTable <- data.frame(Drug = drug, Time = 0, Dose = 0, Units = "mg")
  recalculatePK(NULL, getDrugDefaultsGlobal(FALSE), doseTable, 50, 70, 170, "male")
}

suggestTestEvents <- data.frame(Time = 0, Event = "Event")

runSuggest <- function(drug, times, targets, end) {
  suggest(
    drug,
    data.frame(Time = as.character(times), Target = as.character(targets)),
    end,
    suggestTestDrugs(drug),
    getDrugDefaultsGlobal(FALSE)$Drug,
    suggestTestEvents,
    REFERENCE_TIME_NONE
  )
}

# The infusion rate the engine runs at time t: the rows at the latest infusion
# time at or before t, added together, as advanceClosedForm0() adds them.
effectiveRate <- function(regimen, infusionUnits, t) {
  infusion <- regimen[regimen$Units == infusionUnits, ]
  started <- infusion$Time[infusion$Time <= t]
  if (length(started) == 0) return(0)
  sum(infusion$Dose[infusion$Time == max(started)])
}

test_that("no infusion runs past the requested end time", {
  # Audit F24.  A window (1 min) shorter than tPeak put the infusion's rate
  # changes after the end, and the zero went on the last sorted row, so a
  # nonzero row at the same time kept the infusion running: remifentanil
  # 69.3 mcg/kg/min, propofol 49.9 mcg/kg/min, for ever.
  local_mocked_bindings(outputComments = function(...) {})
  defaults <- getDrugDefaultsGlobal(FALSE)
  for (case in list(list(drug = "remifentanil", target = 4),
                    list(drug = "propofol",     target = 3))) {
    regimen <- runSuggest(case$drug, 1, case$target, 2)
    infusionUnits <- defaults$Infusion.Units[defaults$Drug == case$drug]
    expect_true(all(regimen$Time <= 2))
    # exactly one infusion row at the end, and it is zero
    atEnd <- regimen$Units == infusionUnits & regimen$Time == 2
    expect_equal(sum(atEnd), 1)
    expect_equal(regimen$Dose[atEnd], 0)
    for (t in c(2, 2.5, 10, 60, 1440)) {
      expect_equal(effectiveRate(regimen, infusionUnits, t), 0)
    }
    # And the engine agrees: an hour later the drug has all but washed out
    # (with the infusion left running, remifentanil Ce was 1953 ng/mL here).
    PK <- suggestTestDrugs(case$drug)[[case$drug]]
    wide <- simCpCe(regimen[, c("Time", "Dose", "Units")], suggestTestEvents, PK, 60, FALSE)$wide
    expect_lt(wide$"Effect Site"[wide$Time == 60], 0.1 * case$target)
  }
})

test_that("every rate change is inside its interval and the window", {
  PK <- list(tPeak = 1.6, Bolus.Units = "mg", Infusion.Units = "mcg/kg/min")
  # Two targets: an infusion at the target time plus tPeak and one a fifth of
  # the way from there to the end of the interval, each in whole minutes
  regimen <- suggestRegimen(data.frame(Time = c(0, 10), Target = c(3, 4)), 30, PK)
  expect_equal(regimen$Time, c(0, 2, 4, 10, 12, 16))
  expect_equal(regimen$Units, rep(c("mg", "mcg/kg/min", "mcg/kg/min"), 2))
  # A window shorter than tPeak gets its bolus alone
  regimen <- suggestRegimen(data.frame(Time = 0, Target = 4), 1, PK)
  expect_equal(regimen$Time, 0)
  # So does an interval shorter than tPeak, and the next interval's rates
  # start after its own target
  regimen <- suggestRegimen(data.frame(Time = c(0, 0.25), Target = c(3, 3)), 60, PK)
  expect_equal(regimen$Time, c(0, 0.25, 2, 14))
  expect_equal(regimen$Units, c("mg", "mg", "mcg/kg/min", "mcg/kg/min"))
  # tPeak = 0 (naloxone's model gives ke0 directly): the infusion starts with
  # the bolus, and is not duplicated
  regimen <- suggestRegimen(data.frame(Time = 0, Target = 2), 60,
                            list(tPeak = 0, Bolus.Units = "mcg", Infusion.Units = "mcg/min"))
  expect_equal(regimen$Time, c(0, 0, 12))
  expect_equal(regimen$Units, c("mcg", "mcg/min", "mcg/min"))
})

test_that("the seed reads the effect site at the dose times, not on the plot grid", {
  # Audit F23.  The proportional correction read the effect site at the last
  # point of the 100-point grid before the next dose.  With an end of 1440 the
  # grid's first step (14.5 min) passes the 2-min seed interval, and with
  # targets 0.25 min apart no grid point falls between them: either way the
  # only point found was time 0, where Ce is 0, and the doses became NaN.
  local_mocked_bindings(outputComments = function(...) {})
  for (case in list(list(times = 2,          targets = 3,      end = 1440),
                    list(times = c(2, 2.25), targets = c(3, 3), end = 60))) {
    regimen <- runSuggest("propofol", case$times, case$targets, case$end)
    expect_true(all(is.finite(regimen$Dose)))
    expect_gt(regimen$Dose[regimen$Time == 2 & regimen$Units == "mg"], 0)
    expect_equal(max(regimen$Time), case$end)
    fit <- attr(regimen, "optimizer")
    expect_true(fit$code %in% 1:2)
    expect_lt(fit$minimum, fit$seed)
  }
})

test_that("of several targets at one time, the last entered is kept", {
  # Audit F23: duplicate times failed with "replacement has length zero".
  cleaned <- suggestTargets(
    data.frame(Time = c("10", "2", "2", "5", "", "8", "60", "1"),
               Target = c("2", "3", "4", "1", "7", "0", "9", "")),
    60, REFERENCE_TIME_NONE
  )
  # The blank time, the zero and blank targets and the row at the end are
  # dropped; of the two at minute 2 the second wins; and the falling targets
  # after it are raised to 4
  expect_equal(cleaned$targets, data.frame(Time = c(2, 5, 10), Target = c(4, 4, 4)))
  expect_equal(cleaned$endTime, 60)
  expect_null(suggestTargets(data.frame(Time = c("", "70"), Target = c("2", "2")), 60,
                             REFERENCE_TIME_NONE))

  local_mocked_bindings(outputComments = function(...) {})
  regimen <- runSuggest("propofol", c(2, 2), c(3, 4), 60)
  expect_equal(sum(regimen$Units == "mg"), 1)
  expect_true(all(is.finite(regimen$Dose)))
})

test_that("only drugs with an effect site and intravenous bolus and infusion units are offered", {
  # Audit F23: mannitol and codeine were offered and failed.
  choices <- suggestDrugChoices(getDrugDefaultsGlobal())
  expect_true(all(c("propofol", "remifentanil", "fentanyl", "ketamine", "naloxone") %in% choices))
  # no effect site, oral only, or inhaled
  expect_false(any(c("mannitol", "codeine", "tramadol", "cefazolin", "vancomycin",
                     "dexamethasone", "oxycodone", "hydrocodone", "sevoflurane",
                     "oxygen") %in% choices))
  for (drug in choices) {
    expect_true(suggestSupported(getDrugPK(drug, 70, 170, 50, "male")), info = drug)
  }
  expect_true(suggestUnitsSupported("mg", "mcg/kg/min"))
  expect_false(suggestUnitsSupported("mg PO", "mg/hr"))
  expect_false(suggestUnitsSupported("mg/hr", "mg/hr"))
  expect_false(suggestUnitsSupported("mg", "mg"))
  expect_false(suggestUnitsSupported(NA, "mg/hr"))

  local_mocked_bindings(outputComments = function(...) {})
  expect_error(runSuggest("mannitol", 2, 300, 60), "effect site")
  expect_error(runSuggest("codeine", 2, 10, 60), "effect site")
})

test_that("the fit minimises the effect-site error alone", {
  # Audit F25.  The objective subtracted the target from data.frame(Time, Ce),
  # adding (Time - target)^2: for ketamine 0.12 mcg/mL that term was 112005
  # against a concentration term of 0.02, and nlm() stopped at its seed.
  local_mocked_bindings(outputComments = function(...) {})
  regimen <- runSuggest("ketamine", 2, 0.12, 60)
  fit <- attr(regimen, "optimizer")
  expect_gt(fit$iterations, 0)
  expect_lt(fit$minimum, fit$seed)

  # The objective, computed here from the returned regimen alone: the mean
  # squared effect-site error, relative to the target, over 100 evenly spaced
  # times from the first target to the end.  No time term.
  PK <- suggestTestDrugs("ketamine")$ketamine
  objective <- function(regimen) {
    wide <- simCpCe(regimen[, c("Time", "Dose", "Units")], suggestTestEvents, PK, 60, FALSE)$wide
    ce <- stats::approx(wide$Time, wide$"Effect Site", xout = seq(2, 60, length.out = RESOLUTION))$y
    mean(((ce - 0.12) / 0.12)^2)
  }
  best <- objective(regimen)
  expect_equal(best, fit$rounded, tolerance = 1e-6)
  # The doses are rounded to three significant figures, nothing more
  expect_equal(best, fit$minimum, tolerance = 1e-3)
  # And it is a minimum: moving any one dose 5% either way makes it worse
  free <- which(regimen$Time < 60)
  for (j in free) {
    for (factor in c(0.95, 1.05)) {
      moved <- regimen
      moved$Dose[j] <- moved$Dose[j] * factor
      expect_gt(objective(moved), best)
    }
  }
})
