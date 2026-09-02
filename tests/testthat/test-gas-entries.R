# Tests for presenting the inhaled gases as entries in the `drugs` list.
#
# The point of shaping them this way is that simulationPlot() then treats them
# like any other series, and -- critically -- a dose table containing ONLY
# inhaled agents still produces a plot instead of hitting the
# nrow(allResults) == 0 early return.

# simulationPlot() calls element_blank(), element_text() and unit() unqualified
# and NAMESPACE imports no ggplot2, so it only works when ggplot2 is attached.
# The existing suite gets away with this because test-multi-PK.R attaches it
# first; attach it here explicitly rather than depending on file order.
library(ggplot2)

gasOnlyDT <- function() data.frame(
  Drug  = c("oxygen", "sevoflurane", "ventilation"),
  Time  = c(0, 0, 0),
  Dose  = c(2, 2, 4),
  Units = c("L/min", "%", "L/min"),
  stringsAsFactors = FALSE
)


test_that("reportableGases reports results, not inputs", {
  DT <- data.frame(
    Drug = c("air", "oxygen", "nitrousOxide", "sevoflurane", "ventilation"),
    Dose = c(1, 1, 3, 2, 4),
    stringsAsFactors = FALSE
  )
  r <- reportableGases(DT)

  # Air, ventilation and nitrogen are inputs or internal state, not results
  expect_false(any(c("air", "ventilation", "nitrogen") %in% r))
  expect_setequal(r, c("oxygen", "nitrousOxide", "sevoflurane"))

  # An agent with a zero dose is not reported
  DT$Dose[DT$Drug == "nitrousOxide"] <- 0
  expect_false("nitrousOxide" %in% reportableGases(DT))

  # Oxygen is reported whenever any flow is running, even air alone
  airOnly <- data.frame(Drug = c("air", "ventilation"), Dose = c(4, 4),
                        stringsAsFactors = FALSE)
  expect_equal(reportableGases(airOnly), "oxygen")
})


test_that("gas entries carry every field simulationPlot's Step D1 reads", {
  DT  <- gasOnlyDT()
  sim <- simulateGases(DT, weight = 70, age = 40, maximum = 60)
  ent <- gasDrugEntries(sim, DT, getDrugDefaultsGlobal(FALSE), 60)

  expect_setequal(names(ent), c("oxygen", "sevoflurane", "MAC"))

  needed <- c("drug", "Color", "Concentration.Units", "typical",
              "lowerTypical", "upperTypical", "MEAC", "endCe",
              "results", "equiSpace", "max")
  for (g in names(ent)) {
    expect_true(all(needed %in% names(ent[[g]])), info = g)
    expect_true(all(c("Drug", "Time", "Site", "Y") %in% names(ent[[g]]$results)),
                info = g)
    expect_true(all(c("Drug", "Time", "Ce", "Recovery", "MEAC") %in%
                      names(ent[[g]]$equiSpace)), info = g)
    expect_true(all(c("Drug", "Recovery", "Cp", "Ce") %in% names(ent[[g]]$max)),
                info = g)
  }
})


test_that("gas Site values stay inside the plot's factor levels", {
  # simulationPlot.R:365 factors Site to exactly these levels and :388 maps
  # linetypes positionally, so any other value becomes NA and breaks the plot.
  DT  <- gasOnlyDT()
  sim <- simulateGases(DT, weight = 70, age = 40, maximum = 60)
  ent <- gasDrugEntries(sim, DT, getDrugDefaultsGlobal(FALSE), 60)

  sites <- unique(unlist(lapply(ent, function(e) unique(e$results$Site))))
  expect_true(all(sites %in% c("Plasma", "Effect Site",
                               "CpNormCp", "CeNormCp", "CpNormCe", "CeNormCe")))

  # Alveolar became Plasma and brain became Effect Site
  sevo <- ent$sevoflurane$results
  alv  <- sevo$Y[sevo$Site == "Plasma"]
  brn  <- sevo$Y[sevo$Site == "Effect Site"]
  expect_true(all(brn <= alv + 1e-9))   # brain trails end-tidal during wash-in
})


test_that("a dose table containing ONLY gases still produces a plot", {
  local_mocked_bindings(outputComments = function(...) {})

  DT  <- gasOnlyDT()
  sim <- simulateGases(DT, weight = 70, age = 40, maximum = 60)
  drugs <- gasDrugEntries(sim, DT, getDrugDefaultsGlobal(FALSE), 60)

  eventTable <- data.frame(Time = double(), Event = character(),
                           Fill = character())

  p <- simulationPlot(
    drugs         = drugs,
    events        = eventTable,
    drugDefaults  = getDrugDefaultsGlobal(FALSE),
    eventDefaults = getEventDefaults()
  )

  expect_equal(names(p), c("plotObject", "allResults", "plotResults", "plotHeight"))
  expect_gt(nrow(p$plotResults), 0)
  expect_true(all(c("oxygen", "sevoflurane", "MAC") %in% p$plotResults$Drug))
})


test_that("gas facets are labelled in % and MAC, not per millilitre", {
  local_mocked_bindings(outputComments = function(...) {})

  DT  <- gasOnlyDT()
  sim <- simulateGases(DT, weight = 70, age = 40, maximum = 60)
  drugs <- gasDrugEntries(sim, DT, getDrugDefaultsGlobal(FALSE), 60)

  p <- simulationPlot(
    drugs         = drugs,
    events        = data.frame(Time = double(), Event = character(),
                               Fill = character()),
    drugDefaults  = getDrugDefaultsGlobal(FALSE),
    eventDefaults = getEventDefaults()
  )

  wraps <- unique(p$plotResults$Wrap)
  expect_false(any(grepl("%/ml", wraps, fixed = TRUE)))
  expect_true(any(grepl("sevoflurane", wraps) & grepl("(%)", wraps, fixed = TRUE)))
  expect_true(any(grepl("age-adjusted", wraps, fixed = TRUE)))
})


test_that("gases and IV drugs plot together", {
  local_mocked_bindings(outputComments = function(...) {})

  DT <- data.frame(
    Drug  = c("propofol", "oxygen", "sevoflurane", "ventilation"),
    Time  = c(0, 0, 0, 0),
    Dose  = c(200, 2, 2, 4),
    Units = c("mg", "L/min", "%", "L/min"),
    stringsAsFactors = FALSE
  )
  eventTable <- data.frame(Time = double(), Event = character(),
                           Fill = character())

  ivDT  <- DT[!isGasDrug(DT$Drug), , drop = FALSE]
  gasDT <- DT[isGasDrug(DT$Drug), , drop = FALSE]

  ivDrugs <- recalculatePK(NULL, getDrugDefaultsGlobal(FALSE), ivDT,
                           age = 50, weight = 70, height = 170, sex = "male")
  ivDrugs <- processdoseTable(ivDT, eventTable, ivDrugs, 60, FALSE)

  sim   <- simulateGases(DT, weight = 70, age = 50, maximum = 60)
  drugs <- c(ivDrugs, gasDrugEntries(sim, gasDT, getDrugDefaultsGlobal(FALSE), 60))

  expect_setequal(names(drugs), c("propofol", "oxygen", "sevoflurane", "MAC"))

  p <- simulationPlot(
    drugs         = drugs,
    events        = eventTable,
    drugDefaults  = getDrugDefaultsGlobal(FALSE),
    eventDefaults = getEventDefaults()
  )
  expect_true(all(c("propofol", "sevoflurane", "MAC") %in% p$plotResults$Drug))
  # propofol keeps its per-millilitre label
  expect_true(any(grepl("/ml", p$plotResults$Wrap, fixed = TRUE)))
})


test_that("isGasSeries covers the gases and the synthetic MAC series", {
  expect_true(all(isGasSeries(c("oxygen", "sevoflurane", "ventilation", "MAC"))))
  expect_false(any(isGasSeries(c("propofol", "remifentanil"))))
})
