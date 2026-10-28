# Carfentanil (R/drugs_carfentanil.R): sufentanil's disposition, used on the
# judgement of Steven L. Shafer that carfentanil's pharmacokinetics are likely
# identical to sufentanil's, because no human carfentanil model exists.
# Drafted by Claude Code, 2026-10-18, at his request.
#
# What these tests guard is the *claim*: that carfentanil is sufentanil's
# disposition and nothing more.  So they check the identity holds exactly, at
# both body-size switch positions, and that none of sufentanil's
# pharmacodynamics leaked across with it.

noEvents <- data.frame(Time = numeric(0), Event = character(0))

test_that("the disposition is sufentanil's, exactly, at both switch positions", {
  for (adj in c(TRUE, FALSE)) {
    for (patient in list(list(70, 170, 35, "male"),
                         list(50, 160, 20, "female"),
                         list(120, 185, 65, "male"))) {
      carf <- do.call(carfentanil, c(patient, adjustToFFM = adj))$PK
      suf  <- do.call(sufentanil,  c(patient, adjustToFFM = adj))$PK
      expect_equal(carf, suf, info = paste(unlist(patient), adj))
    }
  }
})

test_that("the published sufentanil vector is what arrives, unaltered", {
  # Gepts 1995 at the 70 kg reference man, so that a change to either drug's
  # constants shows up here rather than silently cancelling.
  pk <- carfentanil(70, 170, 35, "male")$PK$default
  expect_equal_rounded(14.3,     pk$v1)
  expect_equal_rounded(63.1,     pk$v2)
  expect_equal_rounded(261.6,    pk$v3)
  expect_equal_rounded(0.92,     pk$cl1)
  expect_equal_rounded(1.55,     pk$cl2)
  expect_equal_rounded(0.33,     pk$cl3)
})

test_that("none of sufentanil's pharmacodynamics came across", {
  # The borrowed assumption is about disposition only.  Sufentanil has a
  # tPeak of 5.6 min and a MEAC of 0.056; carfentanil must have neither.
  carf <- carfentanil(70, 170, 35, "male")
  suf  <- sufentanil(70, 170, 35, "male")
  expect_gt(suf$tPeak, 0)
  expect_gt(suf$MEAC, 0)
  expect_equal(carf$tPeak, 0)
  expect_equal(carf$MEAC, 0)
  expect_equal(c(carf$lowerTypical, carf$typical, carf$upperTypical), c(0, 0, 0))
  # and no effect site is built from it
  expect_equal(getDrugPK("carfentanil", 70, 170, 35, "male")$PK$default$ke0, 0)
})

test_that("the reference says the parameters are assumed and whose assumption it is", {
  ref <- carfentanil(70, 170, 35, "male")$reference
  expect_match(ref, "No human pharmacokinetic model of carfentanil exists")
  expect_match(ref, "Steven L. Shafer")
  expect_match(ref, "Gepts")
  expect_match(ref, "not a fitted carfentanil model")
})

test_that("carfentanil is an opt-in illicit drug, intravenous only", {
  defaults <- getDrugDefaultsGlobal(FALSE)
  row <- defaults[defaults$Drug == "carfentanil", ]
  expect_equal(nrow(row), 1)
  expect_equal(row$Category, ILLICIT_DRUG_CATEGORY)
  expect_true(isIllicitDrug("carfentanil", defaults))
  # hidden unless the opt-in is on
  expect_false("carfentanil" %in% visibleDrugNames(FALSE, defaults))
  expect_true("carfentanil" %in% visibleDrugNames(TRUE, defaults))
  # intravenous only: every offered unit routes to IV
  units <- strsplit(row$Units, ",")[[1]]
  expect_setequal(unique(doseRoute(units)), ROUTE_IV)
})

test_that("the registry records the refused routes rather than omitting them", {
  reg <- illicitRouteRegistry()
  reg <- reg[reg$drug == "carfentanil", ]
  expect_setequal(reg$route, c("IV", "IN", "smoked"))
  expect_equal(reg$offered[reg$route == "IV"], "yes")
  expect_equal(reg$offered[reg$route == "IN"], "no")
  expect_equal(reg$offered[reg$route == "smoked"], "no")
  # the offered row must say the parameters are an approximation, not a fit
  expect_equal(reg$parameter_status[reg$route == "IV"], "derived_approximation")
  expect_match(reg$notes[reg$route == "IV"], "Shafer")
})

test_that("the simulated plasma curve is sufentanil's curve", {
  dose <- function(drug) data.frame(Drug = drug, Time = 0, Dose = 100,
                                    Units = "mcg", stringsAsFactors = FALSE)
  run <- function(drug) simulateDrugsWithCovariates(
    dose(drug), noEvents, 70, 170, 35, "male", 240, FALSE)[[drug]]$wide
  carf <- run("carfentanil")
  suf  <- run("sufentanil")

  # The two are NOT compared element by element, because they are not
  # simulated on the same time grid: simulationTimeGrid() seeds points from
  # the effect site, which sufentanil has and carfentanil deliberately does
  # not, so the grids differ even though the disposition is identical.
  expect_false(isTRUE(all.equal(carf$Time, suf$Time)))

  # The initial concentration is exact: 100 mcg into V1, in ng/mL.
  expect_equal_rounded(100 / carfentanil(70, 170, 35, "male")$PK$default$v1,
                       carf$Plasma[carf$Time == 0])

  # At matched times the curves agree to within linear interpolation error on
  # a 33 point grid through a steeply convex decay, which is a few tenths of
  # a percent; identity of the underlying model is asserted above, from the
  # parameters themselves, rather than inferred from this.
  onSuf <- stats::approx(suf$Time, suf$Plasma, carf$Time, rule = 2)$y
  expect_lt(max(abs(carf$Plasma - onSuf) / onSuf), 0.01)

  # and it is a real curve, not a flat line
  expect_gt(max(carf$Plasma), 0)
  expect_lt(carf$Plasma[nrow(carf)], carf$Plasma[1])
})
