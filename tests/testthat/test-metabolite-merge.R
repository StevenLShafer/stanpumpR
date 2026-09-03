# Tests for unit scaling between a parent and its metabolite, and for summing a
# metabolite contribution into the metabolite drug's own row.

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

  # The real pairing this exists for
  dd <- getDrugDefaultsGlobal(FALSE)
  hydro <- dd$Concentration.Units[dd$Drug == "hydromorphone"]
  morph <- dd$Concentration.Units[dd$Drug == "morphine"]
  expect_equal(hydro, "ng")
  expect_equal(morph, "mcg")
  expect_equal(metaboliteUnitScale(hydro, morph), 1e-3)
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
  a <- data.frame(Time = c(0, 10, 20), Cp = c(0, 4, 2), Ce = c(0, 2, 3))
  b <- data.frame(Time = c(0, 10, 20), Cp = c(0, 1, 1), Ce = c(0, 1, 1))

  m <- mergeMetaboliteSeries(a, b)
  expect_equal(m$Time, c(0, 10, 20))
  expect_equal(m$Cp, c(0, 5, 3))
  expect_equal(m$Ce, c(0, 3, 4))
})


test_that("mergeMetaboliteSeries interpolates onto the union of two timelines", {
  # The two drugs build their own timelines around their own dose times, so the
  # grids do not line up.
  a <- data.frame(Time = c(0, 20), Cp = c(0, 20), Ce = c(0, 10))
  b <- data.frame(Time = c(0, 10, 20), Cp = c(0, 5, 0), Ce = c(0, 1, 0))

  m <- mergeMetaboliteSeries(a, b)
  expect_equal(m$Time, c(0, 10, 20))
  # a is linear 0..20, so it interpolates to 10 at t = 10
  expect_equal(m$Cp, c(0, 15, 20))
  expect_equal(m$Ce, c(0, 6, 10))
})


test_that("a missing side of the merge is returned unchanged", {
  a <- data.frame(Time = c(0, 10), Cp = c(0, 4), Ce = c(0, 2))
  empty <- data.frame(Time = numeric(0), Cp = numeric(0), Ce = numeric(0))

  expect_equal(mergeMetaboliteSeries(a, NULL), a)
  expect_equal(mergeMetaboliteSeries(NULL, a), a)
  expect_equal(mergeMetaboliteSeries(a, empty), a)
  expect_equal(mergeMetaboliteSeries(empty, a), a)
})


test_that("foldMetabolites sums a contribution into the metabolite's own row", {
  # morphine given directly, and morphine formed from codeine
  drugs <- list(
    codeine = list(
      metaboliteName   = "morphine",
      metaboliteSeries = data.frame(Time = c(0, 10, 20),
                                    Cp = c(0, 1, 2), Ce = c(0, 0.5, 1))
    ),
    morphine = list(
      series = data.frame(Time = c(0, 10, 20), Cp = c(0, 4, 3), Ce = c(0, 2, 2))
    )
  )

  out <- foldMetabolites(drugs)

  expect_equal(out$morphine$series$Cp, c(0, 5, 5))
  expect_equal(out$morphine$series$Ce, c(0, 2.5, 3))
  expect_equal(out$morphine$formedFrom, "codeine")
  # The parent is left alone
  expect_equal(out$codeine$metaboliteSeries$Cp, c(0, 1, 2))
})


test_that("foldMetabolites creates the row when the metabolite was not given", {
  drugs <- list(
    codeine = list(
      metaboliteName   = "morphine",
      metaboliteSeries = data.frame(Time = c(0, 10), Cp = c(0, 2), Ce = c(0, 1))
    )
  )
  out <- foldMetabolites(drugs)

  expect_equal(out$morphine$series$Cp, c(0, 2))
  expect_equal(out$morphine$formedFrom, "codeine")
})


test_that("two parents forming the same metabolite both contribute", {
  # Codeine and tramadol are both prodrugs; nothing stops a table holding both.
  drugs <- list(
    codeine = list(
      metaboliteName   = "morphine",
      metaboliteSeries = data.frame(Time = c(0, 10), Cp = c(0, 2), Ce = c(0, 1))
    ),
    hydrocodone = list(
      metaboliteName   = "morphine",
      metaboliteSeries = data.frame(Time = c(0, 10), Cp = c(0, 3), Ce = c(0, 1))
    )
  )
  out <- foldMetabolites(drugs)

  expect_equal(out$morphine$series$Cp, c(0, 5))
  expect_setequal(out$morphine$formedFrom, c("codeine", "hydrocodone"))
})


test_that("foldMetabolites leaves a drug list with no metabolites alone", {
  drugs <- list(morphine = list(series = data.frame(Time = 0, Cp = 0, Ce = 0)))
  expect_equal(foldMetabolites(drugs), drugs)
  expect_null(foldMetabolites(NULL))
  expect_equal(foldMetabolites(list()), list())
})
