# processdoseTable() reuses a drug's previous simulation when nothing it
# depends on has changed.  Each test marks the cached copy and checks whether
# the mark survives: it does when the simulation was reused and not when the
# drug was simulated again.

noEvents <- data.frame(Time = numeric(0), Event = character(0))

runPipeline <- function(DT, cache = NULL, weight = 70, maximum = 240,
                        recovery = FALSE, ET = noEvents) {
  local_mocked_bindings(outputComments = function(...) {})
  dd <- getDrugDefaultsGlobal()
  drugs <- recalculatePK(NULL, dd, DT, 50, weight, 171, "male")
  processdoseTable(DT, ET, drugs, maximum, recovery, cache = cache)
}

markCache <- function(cache) {
  for (drug in names(cache)) {
    if (!is.null(cache[[drug]]$sim)) attr(cache[[drug]]$sim$wideOwn, "cached") <- TRUE
  }
  cache
}

reused <- function(drugs, drug) isTRUE(attr(drugs[[drug]]$sim$wideOwn, "cached"))

twoDrugs <- data.frame(Drug = c("propofol", "fentanyl"), Time = c(0, 0),
                       Dose = c(100, 50), Units = c("mg", "mcg"))

test_that("an unchanged table reuses every simulation and gives the same answer", {
  first  <- runPipeline(twoDrugs)
  second <- runPipeline(twoDrugs, cache = markCache(first))
  expect_true(reused(second, "propofol"))
  expect_true(reused(second, "fentanyl"))
  expect_equal(second$propofol$results, first$propofol$results, ignore_attr = TRUE)
  expect_equal(second$fentanyl$equiSpace, first$fentanyl$equiSpace)
})

test_that("changing one drug's doses re-simulates only that drug", {
  cache <- markCache(runPipeline(twoDrugs))
  DT <- twoDrugs
  DT$Dose[DT$Drug == "propofol"] <- 150
  out <- runPipeline(DT, cache = cache)
  expect_false(reused(out, "propofol"))
  expect_true(reused(out, "fentanyl"))
  expect_equal(out$propofol$results, runPipeline(DT)$propofol$results)
})

test_that("adding another drug's row above does not invalidate a drug", {
  cache <- markCache(runPipeline(twoDrugs))
  DT <- rbind(data.frame(Drug = "midazolam", Time = 0, Dose = 2, Units = "mg"), twoDrugs)
  out <- runPipeline(DT, cache = cache)
  expect_true(reused(out, "propofol"))
  expect_true(reused(out, "fentanyl"))
  expect_false(reused(out, "midazolam"))
})

test_that("covariates, plot length and the recovery switch re-simulate", {
  cache <- markCache(runPipeline(twoDrugs))
  for (out in list(runPipeline(twoDrugs, cache, weight = 90),
                   runPipeline(twoDrugs, cache, maximum = 480),
                   runPipeline(twoDrugs, cache, recovery = TRUE))) {
    expect_false(reused(out, "propofol"))
    expect_false(reused(out, "fentanyl"))
  }
})

test_that("a removed drug leaves no stale simulation behind", {
  cache <- markCache(runPipeline(twoDrugs))
  out <- runPipeline(twoDrugs[twoDrugs$Drug == "fentanyl", ], cache = cache)
  expect_null(out$propofol)
  expect_true(reused(out, "fentanyl"))
})

test_that("a reused metabolite drug drops a parent that is no longer given", {
  both <- data.frame(Drug = c("codeine", "morphine"), Time = c(0, 0),
                     Dose = c(60, 5), Units = c("mg PO", "mg"))
  cache <- markCache(runPipeline(both))
  expect_equal(cache$morphine$formedFrom, "codeine")

  morphineOnly <- both[both$Drug == "morphine", ]
  out   <- runPipeline(morphineOnly, cache = cache)
  fresh <- runPipeline(morphineOnly)
  expect_true(reused(out, "morphine"))
  expect_null(out$morphine$formedFrom)
  expect_equal(out$morphine$wide, fresh$morphine$wide, ignore_attr = TRUE)
  expect_equal(out$morphine$equiSpace, fresh$morphine$equiSpace)
})

test_that("a reused metabolite drug still receives its parent's contribution", {
  both <- data.frame(Drug = c("codeine", "morphine"), Time = c(0, 0),
                     Dose = c(60, 5), Units = c("mg PO", "mg"))
  first <- runPipeline(both)
  DT <- both
  DT$Dose[DT$Drug == "codeine"] <- 120
  out <- runPipeline(DT, cache = markCache(first))
  expect_true(reused(out, "morphine"))
  expect_false(reused(out, "codeine"))
  expect_equal(out$morphine$equiSpace, runPipeline(DT)$morphine$equiSpace)
})
