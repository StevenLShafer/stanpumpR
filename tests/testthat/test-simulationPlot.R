test_that("simulationPlot yields desired objects", {

  local_mocked_bindings(outputComments = function(...) {})

  doseTable <- data.frame(
    Drug = getDrugDefaultsGlobal(FALSE)$Drug[1],
    Time = 0,
    Dose = 1,
    Units = "mg"
  )

  eventTable <- data.frame(
    Time = 0,
    Event = "Event"
  )

  ## from server.R

  age <- 50
  weight <- 60
  height <- 66*2.54
  sex <- "female"

  plotMaximum <- 60
  plotRecovery <- TRUE
  plotEvents <- TRUE

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

  p <- simulationPlot(
    drugs = drugs,
    events = eventTable,
    drugDefaults = getDrugDefaultsGlobal(FALSE),
    eventDefaults = getEventDefaults(),
    plotEvents = plotEvents,
    plotRecovery = plotRecovery
  )

  expect_equal(names(p), c("plotObject","allResults","plotResults","plotHeight"))
})


# Hiding the plasma line must not hide a drug entirely.
#
# The default setting draws the effect site only.  A drug with no effect site
# -- a prodrug, or one whose potency has not been supplied -- has nothing in
# that series, so dropping its plasma rows too left it as an empty panel.
# Codeine, tramadol and desmetramadol all did this out of the box.

plotWith <- function(doseTable, plasmaLinetype, effectsiteLinetype = "solid") {
  local_mocked_bindings(outputComments = function(...) {}, .env = parent.frame())
  defaults <- getDrugDefaultsGlobal(FALSE)
  events <- data.frame(Time = numeric(0), Event = character(0))
  drugs <- processdoseTable(
    doseTable, events,
    recalculatePK(NULL, defaults, doseTable, 50, 70, 170, "male"),
    720, FALSE
  )
  simulationPlot(
    drugs = drugs, events = events,
    drugDefaults = defaults, eventDefaults = getEventDefaults(),
    plotEvents = FALSE, plotRecovery = FALSE,
    plasmaLinetype = plasmaLinetype, effectsiteLinetype = effectsiteLinetype
  )
}

drawnFor <- function(p, drug) {
  sum(as.character(p$plotResults$Drug) == drug & !is.na(p$plotResults$Y))
}


test_that("a drug with no effect site still draws when the plasma line is blank", {
  # tramadol has no effect site of its own and forms desmetramadol, which
  # has no potency supplied yet, so neither has an effect-site curve.
  DT <- data.frame(Drug = "tramadol", Time = 0, Dose = 100, Units = "mg PO")

  p <- plotWith(DT, plasmaLinetype = "blank")
  expect_gt(drawnFor(p, "tramadol"), 0)
  expect_gt(drawnFor(p, "desmetramadol"), 0)
  # and what is drawn is the plasma series, since there is nothing else
  tram <- p$plotResults[as.character(p$plotResults$Drug) == "tramadol", ]
  expect_true(all(as.character(tram$Site) == "Plasma"))
})


test_that("hiding the plasma line still hides it for drugs that have an effect site", {
  # The setting must keep working for everything else.
  DT <- data.frame(Drug = "fentanyl", Time = 0, Dose = 100, Units = "mcg")

  p <- plotWith(DT, plasmaLinetype = "blank")
  fent <- p$plotResults[as.character(p$plotResults$Drug) == "fentanyl", ]
  expect_gt(nrow(fent), 0)
  expect_false(any(as.character(fent$Site) == "Plasma"))
})


test_that("the two cases coexist in one plot", {
  DT <- data.frame(
    Drug  = c("tramadol", "fentanyl"),
    Time  = c(0, 0),
    Dose  = c(100, 100),
    Units = c("mg PO", "mcg")
  )
  p <- plotWith(DT, plasmaLinetype = "blank")

  # fentanyl shows effect site only, tramadol shows plasma only
  fent <- p$plotResults[as.character(p$plotResults$Drug) == "fentanyl", ]
  tram <- p$plotResults[as.character(p$plotResults$Drug) == "tramadol", ]
  expect_true(all(as.character(fent$Site) == "Effect Site"))
  expect_true(all(as.character(tram$Site) == "Plasma"))
  expect_gt(nrow(fent), 0)
  expect_gt(nrow(tram), 0)
})


test_that("blanking both lines returns an empty plot rather than erroring", {
  # The escape hatch has to keep working: asking for neither series draws
  # neither, rather than the prodrug rule forcing plasma back on.
  #
  # Nothing survives both filters, and simulationPlot answers "nothing to
  # plot" with NULL, as its own earlier zero-row guard does.  That guard runs
  # before the linetype filters and so never saw this case; until a drug
  # existed with only one of the two series, nothing exercised the path, and
  # the function errored on a zero-row assignment.  Reproducible on master.
  DT <- data.frame(Drug = "tramadol", Time = 0, Dose = 100, Units = "mg PO")
  expect_no_error(
    p <- plotWith(DT, plasmaLinetype = "blank", effectsiteLinetype = "blank")
  )
  expect_null(p)
})


test_that("blanking both lines is empty for an ordinary drug too", {
  # Not a prodrug problem: any single-drug table with both series blanked
  # reaches the same place.
  DT <- data.frame(Drug = "fentanyl", Time = 0, Dose = 100, Units = "mcg")
  expect_no_error(
    p <- plotWith(DT, plasmaLinetype = "blank", effectsiteLinetype = "blank")
  )
  expect_null(p)
})
