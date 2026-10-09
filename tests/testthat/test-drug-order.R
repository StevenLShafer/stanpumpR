# The order the drug pickers offer the drugs in: alphabetical (sortDrugNames()
# in R/drugAndEventDefaults.R), while the drug library keeps the CSV's order.
# Drafted by Claude Code, 2026-10-09, at the request of Steven L. Shafer.

local_mocked_bindings(outputComments = function(...) {})

test_that("sortDrugNames is alphabetical, ignoring case", {
  expect_identical(
    sortDrugNames(c("propofol", "amiodaroneIV", "Air", "amiodarone", "nitrousOxide")),
    c("Air", "amiodarone", "amiodaroneIV", "nitrousOxide", "propofol")
  )
  dd <- getDrugDefaultsGlobal()
  sorted <- sortDrugNames(dd$Drug)
  expect_setequal(sorted, dd$Drug)
  expect_identical(tolower(sorted), sort(tolower(dd$Drug), method = "radix"))
})

test_that("the dose table's Drug column lists the drugs alphabetically", {
  dd <- getDrugDefaultsGlobal()
  hot <- createHOT(doseTableInit, dd)
  drugColumn <- hot$x$columns[[which(names(doseTableInit) == "Drug")]]
  expect_identical(drugColumn$source, sortDrugNames(dd$Drug))
  expect_identical(head(drugColumn$source, 3), c("acetaminophen", "air", "alfentanil"))
})

oldConfig <- .sprglobals$config
.sprglobals$config <- DEFAULT_CONFIG

test_that("the drug pickers are alphabetical, and a click still finds its drug's units", {
  shiny::testServer(app_server, {
    session$setInputs(
      timeUnits = "minutes", timeMode = "clock", referenceTime = "08:00", maximum = "60",
      plotWidth = 800, yaxisHeight = 200, plasmaLinetype = "blank", effectsiteLinetype = "solid",
      weight = 70, weightUnit = "1", height = 170, heightUnit = "1", age = 40, ageUnit = "1",
      sex = "male", showThreshold = FALSE, normalization = "none", typical = "Range", logY = FALSE
    )
    doseTable(doseTableInit)
    session$flushReact()

    # Add a dose and Suggest Dosing offer drugList
    expect_identical(drugList, sortDrugNames(getDrugDefaultsGlobal()$Drug))

    # A click on a panel looks the drug's units up by name: drugList is no
    # longer in the library's row order, so a position in it would give
    # another drug's units
    dd <- drugDefaults()
    results <- plotResultsReactive()
    drugs <- setdiff(unique(doseTableInit$Drug), "")   # not the blank rows
    expect_length(drugs, 4)
    for (drug in drugs) {
      wrap <- as.character(results$Wrap[results$Drug == drug])[1]
      clicked <- imgDrugTime(list(x = 10, panelvar1 = wrap))
      expect_identical(clicked$drug, drug)
      expect_identical(clicked$units,
                       c(dd$Bolus.Units[dd$Drug == drug], dd$Infusion.Units[dd$Drug == drug]),
                       info = drug)
    }
  })
})

.sprglobals$config <- oldConfig
