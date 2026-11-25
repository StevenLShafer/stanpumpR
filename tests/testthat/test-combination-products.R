# Combination products drawn as their components (R/combination-products.R):
# Coartem, entered in tablets and simulated as artemether (with its
# metabolite dihydroartemisinin) and lumefantrine.  Drafted by Claude Code,
# 2026-11-24, at the request of Steven L. Shafer.

events <- data.frame(Time = numeric(0), Event = character(0), Fill = character(0))

test_that("a product row becomes one row per component, in mg of each", {
  dt <- data.frame(Drug = c("Coartem", "Coartem", "propofol"),
                   Time = c(0, 480, 10), Dose = c(4, 4, 2),
                   Units = c("tablet PO", "tablet PO bid", "mg/kg"),
                   stringsAsFactors = FALSE)
  out <- expandCombinationProducts(dt)
  expect_setequal(unique(out$Drug), c("artemether", "lumefantrine", "propofol"))
  a <- out[out$Drug == "artemether", ]
  l <- out[out$Drug == "lumefantrine", ]
  expect_equal(a$Dose, c(80, 80))
  expect_equal(l$Dose, c(480, 480))
  expect_identical(a$Units, c("mg PO", "mg PO bid"))
  expect_identical(l$Time, c(0, 480))
  # the other drugs are untouched
  expect_equal(out[out$Drug == "propofol", "Dose"], 2)
  # a 0 (Stop, Skip next dose) is a 0 for every component
  z <- expandCombinationProducts(data.frame(Drug = "Coartem", Time = 0, Dose = 0,
                                            Units = "tablet PO bid"))
  expect_equal(z$Dose, c(0, 0))
  expect_identical(z$Units, c("mg PO bid", "mg PO bid"))
  # a table without a product is returned as it is
  expect_identical(expandCombinationProducts(dt[3, ]), dt[3, ])
  expect_null(expandCombinationProducts(NULL))
})

test_that("names map both ways", {
  expect_true(isCombinationProduct("Coartem"))
  expect_false(isCombinationProduct("artemether"))
  expect_identical(combinationComponents("Coartem"), c("artemether", "lumefantrine"))
  expect_identical(combinationExpandDrugNames(c("propofol", "Coartem")),
                   c("propofol", "artemether", "lumefantrine"))
  expect_identical(combinationProductFor(c("lumefantrine", "propofol")),
                   c("Coartem", NA_character_))
  held <- data.frame(Drug = c("Coartem", "propofol"))
  expect_identical(combinationProductOf("artemether", held), "Coartem")
  expect_identical(combinationProductOf("lumefantrine", held), "Coartem")
  expect_identical(combinationProductOf("propofol", held), "propofol")
  # a component the table does not hold through a product stays itself
  expect_identical(combinationProductOf("artemether", data.frame(Drug = "propofol")),
                   "artemether")
  # dihydroartemisinin reaches Coartem through artemether, as a click does
  drugs <- list(artemether = list(metaboliteName = "dihydroartemisinin"))
  expect_identical(combinationProductOf(formedMetaboliteParent("dihydroartemisinin", drugs),
                                        held), "Coartem")
})

test_that("the units are the library's, scheduled twice a day", {
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "Coartem", ]
  expect_setequal(unlist(row$Units), c("tablet PO", "tablet PO bid"))
  expect_true(all(c("tablet PO") %in% allUnits))
  expect_true("tablet PO bid" %in% scheduledUnits)
  expect_identical(doseRoute("tablet PO"), ROUTE_PO)
  # every component has a library row and a model
  for (d in combinationComponents("Coartem")) {
    expect_true(d %in% dd$Drug, info = d)
    expect_true(exists(d, mode = "function"), info = d)
  }
})

test_that("a Coartem course simulates as the same mg of each component", {
  times <- c(0, 8, 24, 36, 48, 60) * 60
  product <- data.frame(Drug = "Coartem", Time = times, Dose = 4, Units = "tablet PO")
  direct <- data.frame(Drug = rep(c("artemether", "lumefantrine"), each = 6),
                       Time = rep(times, 2), Dose = rep(c(80, 480), each = 6),
                       Units = "mg PO")
  a <- simulateDrugsWithCovariates(product, events, 60, 165, 30, "female", 10 * 1440, FALSE)
  b <- simulateDrugsWithCovariates(direct, events, 60, 165, 30, "female", 10 * 1440, FALSE)
  expect_setequal(names(a), c("artemether", "dihydroartemisinin", "lumefantrine"))
  for (d in names(a)) expect_equal(a[[d]]$wide, b[[d]]$wide, info = d)
})

test_that("the twice-daily schedule repeats until a 0 ends it", {
  scheduled <- data.frame(Drug = "Coartem", Time = c(0, 8, 24, 72) * 60,
                          Dose = c(4, 4, 4, 0),
                          Units = c("tablet PO", "tablet PO", "tablet PO bid", "tablet PO bid"))
  rows <- data.frame(Drug = "Coartem", Time = c(0, 8, 24, 36, 48, 60) * 60, Dose = 4,
                     Units = "tablet PO")
  at <- c(30, 50, 62, 100, 150) * 60
  # each simulated at the same times, not interpolated between grids
  a <- withOutputTimes(at, simulateDrugsWithCovariates(scheduled, events, 60, 165, 30,
                                                       "male", 7 * 1440, FALSE))
  b <- withOutputTimes(at, simulateDrugsWithCovariates(rows, events, 60, 165, 30,
                                                       "male", 7 * 1440, FALSE))
  for (d in c("artemether", "lumefantrine"))
    expect_equal(a[[d]]$wide$Plasma[match(at, a[[d]]$wide$Time)],
                 b[[d]]$wide$Plasma[match(at, b[[d]]$wide$Time)],
                 tolerance = 1e-9, info = d)
  # the repeats are written in the component's mg
  expect_true(all(a$artemether$scheduled$Units == "mg PO"))
  expect_equal(unique(a$lumefantrine$scheduled$Dose), 480)
})

test_that("the export lists the doses as simulated", {
  dt <- data.frame(Drug = "Coartem", Time = c(0, 480, 1440), Dose = 4,
                   Units = c("tablet PO", "tablet PO", "tablet PO bid"))
  sim <- simulateDrugsWithCovariates(dt, events, 60, 165, 30, "male", 3 * 1440, FALSE)
  out <- exportDoseTable(dt, sim)
  expect_false("Coartem" %in% out$Drug)
  expect_setequal(unique(out$Drug), c("artemether", "lumefantrine"))
  expect_true(all(out$Units[out$Drug == "lumefantrine"] %in% c("mg PO", "mg PO bid")))
})

test_that("the Simulation API takes Coartem and returns its components", {
  res <- apiSimulate(list(
    patient = list(weight = 60, height = 165, age = 30, sex = "female"),
    doses = list(list(Drug = "Coartem", Time = 0, Dose = 4, Units = "tablet PO"),
                 list(Drug = "Coartem", Time = 480, Dose = 4, Units = "tablet PO")),
    duration = 3 * 1440
  ))
  expect_setequal(res$drugs$Drug, c("artemether", "dihydroartemisinin", "lumefantrine"))
  expect_true(all(res$drugs$Dosed[res$drugs$Drug %in% c("artemether", "lumefantrine")]))
  expect_false(res$drugs$Dosed[res$drugs$Drug == "dihydroartemisinin"])
  expect_setequal(unique(res$doses$Drug), c("artemether", "lumefantrine"))
})

test_that("the transit chain refuses a dose-occasion block that makes clearance vanish", {
  set <- list(k10 = 0.01, ka_PO = 0.03)
  ok <- list(n = 3L, ktr = 0.03, kMetabolic = 0,
             doseOccasionClearance = list(slope = 0.5, occasions = 6L))
  expect_identical(validateTransitChain(ok, set, "x"), ok)
  bad <- ok; bad$doseOccasionClearance <- list(slope = -0.3, occasions = 6L)
  expect_error(validateTransitChain(bad, set, "x"), "zero or negative")
  bad$doseOccasionClearance <- list(slope = 0.5, occasions = 2.5)
  expect_error(validateTransitChain(bad, set, "x"), "whole number of occasions")
})

test_that("a product is dosed only in its own unit word", {
  expect_true(combinationUnitOk("Coartem", "tablet PO"))
  expect_true(combinationUnitOk("Coartem", "tablet PO bid"))
  expect_false(combinationUnitOk("Coartem", "mg PO"))
  # a mass unit would give each component the whole product dose
  expect_error(expandCombinationProducts(data.frame(Drug = "Coartem", Time = 0, Dose = 80,
                                                    Units = "mg PO")),
               "dosed in tablets")
  # the Drug Library refuses one, and accepts the product's own units
  G <- getDrugDefaultsGlobal()
  edited <- drugLibraryGridData(G)
  i <- which(edited$Drug == "Coartem")
  edited$Units[i] <- paste0(edited$Units[i], ",mg PO")
  expect_match(drugLibraryUnitProblems(edited, G, maxNew = Inf),
               "a combination product is dosed in tablets", fixed = TRUE)
  current <- G
  current$Units[[which(G$Drug == "Coartem")]] <- character(0)
  expect_identical(drugLibraryUnitProblems(drugLibraryGridData(G), current, maxNew = Inf),
                   character(0))
})
