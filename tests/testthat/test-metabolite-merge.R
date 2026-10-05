# Tests for unit scaling between a parent and its metabolite, and for summing a
# metabolite contribution into the metabolite drug's own row.
#
# The merge works in the pipeline's own wide shape -- Time, Plasma, Effect Site
# and Recovery -- because that is what processdoseTable() holds and what
# finishDrugSeries() needs to rebuild the receiving drug's plotted series.  The
# contribution arriving from a parent is in Cp/Ce terms, which is what
# simCpCe() lifts out of advanceClosedFormMetabolite().

wideSeries <- function(Time, Plasma, Ce, Recovery = rep(0, length(Time))) {
  data.frame(Time = Time, Plasma = Plasma, `Effect Site` = Ce,
             Recovery = Recovery, check.names = FALSE)
}

# A drug entry carrying the minimum finishDrugSeries() needs
drugEntry <- function(name, wideOwn = NULL) {
  list(drug = name, MEAC = 0, wideOwn = wideOwn)
}


test_that("internal dose units follow Concentration.Units", {
  # simCpCe carries doses in mg for a drug reported in mcg/mL, and in mcg for
  # one reported in ng/mL.
  expect_equal(internalDoseScale("mcg"), 1)
  expect_equal(internalDoseScale("ng"), 1e-3)
  expect_error(internalDoseScale("kg"), "Unsupported")
})


test_that("metaboliteUnitScale catches a mismatched parent and metabolite", {
  # Same units on both sides needs no correction
  expect_equal(metaboliteUnitScale("mcg", "mcg"), 1)
  expect_equal(metaboliteUnitScale("ng", "ng"), 1)

  # A ng-reported parent carried in mcg, forming a mcg-reported metabolite
  # carried in mg, is a thousandfold conversion
  expect_equal(metaboliteUnitScale("ng", "mcg"), 1e-3)
  expect_equal(metaboliteUnitScale("mcg", "ng"), 1e3)

  # The real pairings this exists for
  dd <- getDrugDefaultsGlobal(FALSE)
  hydro <- dd$Concentration.Units[dd$Drug == "hydromorphone"]
  morph <- dd$Concentration.Units[dd$Drug == "morphine"]
  cod   <- dd$Concentration.Units[dd$Drug == "codeine"]
  expect_equal(hydro, "ng")
  expect_equal(morph, "mcg")
  expect_equal(cod,   "ng")
  expect_equal(metaboliteUnitScale(hydro, morph), 1e-3)
  expect_equal(metaboliteUnitScale(cod,   morph), 1e-3)
})


test_that("the unit scale carries straight into the coefficients", {
  parent <- getDrugPK("hydromorphone", 70, 170, 50, "male",
                      getDrugDefaults("hydromorphone"))$PK$default
  met    <- getDrugPK("morphine", 70, 170, 50, "male",
                      getDrugDefaults("morphine"))$PK$default

  plain  <- metaboliteCoefficients(parent, met, 0.01)
  scaled <- metaboliteCoefficients(parent, met, 0.01, unitScale = 1e-3)

  expect_equal(scaled$bolus, plain$bolus * 1e-3, tolerance = 1e-15)
  expect_equal(scaled$K, plain$K * 1e-3, tolerance = 1e-15)
  # Still starts at zero
  expect_equal(sum(scaled$bolus), 0, tolerance = 1e-15)

  expect_error(metaboliteCoefficients(parent, met, 0.01, unitScale = 0))
})


test_that("mergeMetaboliteSeries adds two series on a shared timeline", {
  a <- wideSeries(c(0, 10, 20), c(0, 4, 2), c(0, 2, 3))
  b <- data.frame(Time = c(0, 10, 20), Cp = c(0, 1, 1), Ce = c(0, 1, 1))

  m <- mergeMetaboliteSeries(a, b)
  expect_equal(m$Time, c(0, 10, 20))
  expect_equal(m$Plasma, c(0, 5, 3))
  expect_equal(m$"Effect Site", c(0, 3, 4))
})


test_that("mergeMetaboliteSeries interpolates onto the union of two timelines", {
  # The two drugs build their own timelines around their own dose times, so the
  # grids do not line up.
  a <- wideSeries(c(0, 20), c(0, 20), c(0, 10))
  b <- data.frame(Time = c(0, 10, 20), Cp = c(0, 5, 0), Ce = c(0, 1, 0))

  m <- mergeMetaboliteSeries(a, b)
  expect_equal(m$Time, c(0, 10, 20))
  # a is linear 0..20, so it interpolates to 10 at t = 10
  expect_equal(m$Plasma, c(0, 15, 20))
  expect_equal(m$"Effect Site", c(0, 6, 10))
})


test_that("recovery is carried from the receiving drug, not summed", {
  # Recovery is not a concentration and does not superpose.
  a <- wideSeries(c(0, 10), c(0, 4), c(0, 2), Recovery = c(0, 7))
  b <- data.frame(Time = c(0, 10), Cp = c(0, 1), Ce = c(0, 1))

  expect_equal(mergeMetaboliteSeries(a, b)$Recovery, c(0, 7))
})


test_that("a missing side of the merge is handled", {
  a <- wideSeries(c(0, 10), c(0, 4), c(0, 2))
  contribution <- data.frame(Time = c(0, 10), Cp = c(0, 4), Ce = c(0, 2))
  empty <- data.frame(Time = numeric(0), Cp = numeric(0), Ce = numeric(0))

  # Nothing to add leaves the base alone
  expect_equal(mergeMetaboliteSeries(a, NULL), a)
  expect_equal(mergeMetaboliteSeries(a, empty), a)

  # No base at all means the metabolite drug was never given directly, so the
  # contribution becomes the whole series
  made <- mergeMetaboliteSeries(NULL, contribution)
  expect_equal(made$Time, c(0, 10))
  expect_equal(made$Plasma, c(0, 4))
  expect_equal(made$"Effect Site", c(0, 2))
  expect_equal(made$Recovery, c(0, 0))
})


test_that("a prodrug's NA effect site survives the merge", {
  # Interpolating an all-NA column would error; it has to pass through.
  a <- wideSeries(c(0, 10), c(0, 4), c(NA_real_, NA_real_))
  b <- data.frame(Time = c(0, 10), Cp = c(0, 1), Ce = c(0, 1))

  m <- mergeMetaboliteSeries(a, b)
  expect_equal(m$Plasma, c(0, 5))
  expect_true(all(is.na(m$"Effect Site")))
})


test_that("foldMetabolites sums a contribution into the metabolite's own row", {
  # morphine given directly, and morphine formed from codeine
  drugs <- list(
    codeine = c(drugEntry("codeine"), list(
      metaboliteName   = "morphine",
      metaboliteSeries = data.frame(Time = c(0, 10, 20),
                                    Cp = c(0, 1, 2), Ce = c(0, 0.5, 1))
    )),
    morphine = drugEntry("morphine", wideSeries(c(0, 10, 20),
                                                c(0, 4, 3), c(0, 2, 2)))
  )

  out <- foldMetabolites(drugs, maximum = 20)

  expect_equal(out$morphine$wide$Plasma, c(0, 5, 5))
  expect_equal(out$morphine$wide$"Effect Site", c(0, 2.5, 3))
  expect_equal(out$morphine$formedFrom, "codeine")
  # The plotted series is rebuilt from the sum
  plasma <- out$morphine$results
  expect_equal(plasma$Y[plasma$Site == "Plasma"], c(0, 5, 5))
  expect_equal(out$morphine$max$Cp, 5)
  # The parent is left alone
  expect_equal(out$codeine$metaboliteSeries$Cp, c(0, 1, 2))
})


test_that("folding twice does not count the contribution twice", {
  # processdoseTable() folds on every reactive invalidation, so the operation
  # has to start from the receiving drug's own simulation each time.
  drugs <- list(
    codeine = c(drugEntry("codeine"), list(
      metaboliteName   = "morphine",
      metaboliteSeries = data.frame(Time = c(0, 10), Cp = c(0, 2), Ce = c(0, 1))
    )),
    morphine = drugEntry("morphine", wideSeries(c(0, 10), c(0, 4), c(0, 2)))
  )

  once  <- foldMetabolites(drugs, maximum = 10)
  twice <- foldMetabolites(once,  maximum = 10)

  expect_equal(once$morphine$wide$Plasma, c(0, 6))
  expect_equal(twice$morphine$wide$Plasma, c(0, 6))
})


test_that("foldMetabolites creates the row when the metabolite was not given", {
  drugs <- list(
    codeine = c(drugEntry("codeine"), list(
      metaboliteName   = "morphine",
      metaboliteSeries = data.frame(Time = c(0, 10), Cp = c(0, 2), Ce = c(0, 1))
    )),
    morphine = drugEntry("morphine")   # resolved, but never dosed
  )
  out <- foldMetabolites(drugs, maximum = 10)

  expect_equal(out$morphine$wide$Plasma, c(0, 2))
  expect_equal(out$morphine$formedFrom, "codeine")
})


test_that("two parents forming the same metabolite both contribute", {
  # Codeine and hydrocodone are both prodrugs; nothing stops a table holding
  # both once hydrocodone is added.
  drugs <- list(
    codeine = c(drugEntry("codeine"), list(
      metaboliteName   = "morphine",
      metaboliteSeries = data.frame(Time = c(0, 10), Cp = c(0, 2), Ce = c(0, 1))
    )),
    hydrocodone = c(drugEntry("hydrocodone"), list(
      metaboliteName   = "morphine",
      metaboliteSeries = data.frame(Time = c(0, 10), Cp = c(0, 3), Ce = c(0, 1))
    )),
    morphine = drugEntry("morphine")
  )
  out <- foldMetabolites(drugs, maximum = 10)

  expect_equal(out$morphine$wide$Plasma, c(0, 5))
  expect_setequal(out$morphine$formedFrom, c("codeine", "hydrocodone"))
})


test_that("a contribution with nowhere to go is skipped rather than guessed", {
  # The metabolite drug has no resolved PK, so its row cannot be finished.
  drugs <- list(
    codeine = c(drugEntry("codeine"), list(
      metaboliteName   = "morphine",
      metaboliteSeries = data.frame(Time = c(0, 10), Cp = c(0, 2), Ce = c(0, 1))
    ))
  )
  expect_equal(foldMetabolites(drugs, maximum = 10), drugs)
})


test_that("foldMetabolites leaves a drug list with no metabolites alone", {
  drugs <- list(morphine = drugEntry("morphine", wideSeries(0, 0, 0)))
  expect_equal(foldMetabolites(drugs, maximum = 10), drugs)
  expect_null(foldMetabolites(NULL, maximum = 10))
  expect_equal(foldMetabolites(list(), maximum = 10), list())
})
