test_that("doseRoute() reads the route from the units suffix", {
  expect_equal(doseRoute(c("mg", "mg/kg", "mcg/kg/min", "mg/hr")), rep("IV", 4))
  expect_equal(doseRoute(c("mg PO", "mg/kg PO", "mg IM", "mg/kg IM", "mg IN")),
               c("PO", "PO", "IM", "IM", "IN"))
  expect_equal(doseRoute(c(tciUnits, gasUnits)), rep("IV", 4))
  expect_equal(doseRoute(character(0)), character(0))
  expect_equal(doseRoute(factor("mg PO")), "PO")
  expect_equal(doseRoute(NA_character_), "IV")
})

test_that("doseRoute() reads the route when a qualifier follows it", {
  # Scheduled doses add a frequency after the route ("mg PO bid").
  expect_equal(doseRoute(c("mg PO bid", "mg/kg IM qid", "mg IN tid", "mg tid", "mg/kg qd")),
               c("PO", "IM", "IN", "IV", "IV"))
  expect_equal(groupUnitsByRoute(c("mg", "mg PO", "mg PO bid", "mg bid", "mg IM", "mg IM qid")),
               c("mg", "mg bid", "mg PO", "mg PO bid", "mg IM", "mg IM qid"))
})

test_that("doseRoute() agrees with every unit the app offers", {
  expect_true(all(doseRoute(c(bolusUnits, infusionUnits)) == ROUTE_IV))
  expect_true(all(doseRoute(poUnits) == ROUTE_PO))
  expect_true(all(doseRoute(imUnits) == ROUTE_IM))
  expect_true(all(doseRoute(inUnits) == ROUTE_IN))
  expect_true(all(doseRoute(slUnits) == ROUTE_SL))
  expect_true(all(doseRoute(raUnits) == ROUTE_RA))

  dd <- getDrugDefaultsGlobal()
  units <- unique(unlist(dd$Units))
  # The substring tests simCpCe() used before doseRoute(), with RA (regional
  # anesthesia, 2026-10-10) added
  old <- ifelse(grepl("PO", units), "PO",
                ifelse(grepl("IM", units), "IM",
                       ifelse(grepl("IN", units), "IN",
                              ifelse(grepl("SL", units), "SL",
                                     ifelse(grepl("RA", units), "RA", "IV")))))
  expect_equal(doseRoute(units), old)
})

test_that("the constant-rate oral unit is oral by route and a rate by kind", {
  # "mg/day PO" (poRateUnits; R/drugs_amiodarone.R).  (Claude Code,
  # 2026-10-07.)
  expect_true(all(doseRoute(poRateUnits) == ROUTE_PO))
  expect_true(all(isRateUnit(poRateUnits)))
  expect_true(all(poRateUnits %in% allUnits))
  expect_false(any(poRateUnits %in% c(infusionUnits, poUnits, scheduledUnits)))
  expect_true(all(isRateUnit(infusionUnits)))
  expect_false(any(isRateUnit(c(bolusUnits, poUnits, imUnits, inUnits, raUnits, tciUnits,
                                scheduledUnits, "%"))))
  expect_equal(isRateUnit(factor("mg/hr")), TRUE)
})

test_that("every unit offered before the oral rate keeps its exact classification", {
  # simCpCe() classified a row by substrings until 2026-10-07: a bolus was an
  # intravenous unit with neither "min" nor "hr" in it, and every PO, IM or
  # IN unit was an extravascular dose.  It now asks isRateUnit(), so that
  # "mg/day PO" can be a rate; for every other unit the answer must be the
  # one it always was.
  units <- unique(c(setdiff(allUnits, poRateUnits), gasUnits, tciUnits, scheduledUnits,
                    unlist(getDrugDefaultsGlobal()$Units)))
  units <- setdiff(units, poRateUnits)
  route <- doseRoute(units)
  oldBolus <- route == ROUTE_IV & !(grepl("min", units) | grepl("hr", units))
  newBolus <- route == ROUTE_IV & !isRateUnit(units)
  expect_equal(newBolus, oldBolus)
  expect_false(any(isRateUnit(units[route != ROUTE_IV])))
})

test_that("doseRoute() reads the route ahead of a dosing frequency", {
  expect_equal(doseRoute(c("mg bid", "mg/kg PO qd", "mg IM tid", "mcg IN qid")),
               c("IV", "PO", "IM", "IN"))
  expect_true(all(doseRoute(scheduledUnits) ==
                    doseRoute(scheduleBaseUnit(scheduledUnits))))
})

test_that("groupUnitsByRoute() orders IV, PO, IM, IN and keeps order within a route", {
  units <- c("mg", "mg PO", "mg IN", "mg/kg", "mg IM", "Plasma target", "mg/kg PO")
  expect_equal(groupUnitsByRoute(units),
               c("mg", "mg/kg", "Plasma target", "mg PO", "mg/kg PO", "mg IM", "mg IN"))
})

test_that("the drug defaults list each drug's units grouped by route, with none lost", {
  raw <- getDrugDefaultsGlobal(expand = FALSE)
  dd <- getDrugDefaultsGlobal()
  for (i in seq_len(nrow(dd))) {
    units <- dd$Units[[i]]
    expect_setequal(units, strsplit(raw$Units[i], ",")[[1]])
    expect_false(is.unsorted(match(doseRoute(units), DOSE_ROUTES)), label = dd$Drug[i])
  }
  hydromorphone <- dd$Units[[which(dd$Drug == "hydromorphone")]]
  unscheduled <- hydromorphone[!isScheduledUnit(hydromorphone)]
  expect_equal(unscheduled[doseRoute(unscheduled) != ROUTE_IV],
               c("mg PO", "mg PO liquid", "mg/kg PO liquid", "mg IM", "mg IN"))
  # Every route but sublingual and regional anesthesia, which hydromorphone
  # does not offer
  expect_equal(unique(doseRoute(hydromorphone)), setdiff(DOSE_ROUTES, c(ROUTE_SL, ROUTE_RA)))
  lidocaine <- dd$Units[[which(dd$Drug == "lidocaine")]]
  expect_equal(unique(doseRoute(lidocaine)), c(ROUTE_IV, ROUTE_RA))
  expect_true(all(c("Plasma target", "Effect site target") %in%
                    hydromorphone[doseRoute(hydromorphone) == ROUTE_IV]))
})

test_that("sublingual units read as SL and list between oral and intramuscular", {
  # The sublingual route (buprenorphine), added 2026-10-10.
  expect_equal(doseRoute(c("mg SL", "mcg/kg SL", "mg SL bid", "mcg SL qid")), rep(ROUTE_SL, 4))
  expect_false(any(isRateUnit(slUnits)))
  expect_true(all(slUnits %in% allUnits))
  expect_true(all(paste(slUnits, "bid") %in% scheduledUnits))
  expect_true(all(nchar(scheduledUnits) <= MAX_UNIT_STRING_LENGTH))
  expect_equal(groupUnitsByRoute(c("mg IN", "mg SL", "mg", "mg IM", "mg PO")),
               c("mg", "mg PO", "mg SL", "mg IM", "mg IN"))
})

test_that("oralSaturationFraction() is the inhibitory Emax of Tran 2017", {
  # F = 1 - Imax x D / (ID50 + D); Tran reports 0.688, 0.627 and 0.471.
  tran <- list(Imax = 0.906, ID50 = 571)
  expect_equal(round(oralSaturationFraction(c(300, 400, 800), tran), 3),
               c(0.688, 0.627, 0.471))
  # Complete at a vanishingly small dose, 1 - Imax at a very large one.
  expect_equal(oralSaturationFraction(0, tran), 1)
  expect_equal(oralSaturationFraction(1e12, tran), 1 - 0.906, tolerance = 1e-8)
  # The hyperbolic Dmax / (D50 + D) is the case Imax = 1, scaled by Dmax / D50.
  hyper <- list(Imax = 1, ID50 = 1120)
  expect_equal(oralSaturationFraction(300, hyper) * 823 / 1120, 823 / (1120 + 300))
  # No block: every dose is absorbed as bioavailability_PO alone says.
  expect_equal(oralSaturationFraction(c(10, 1000), NULL), c(1, 1))
})

test_that("validateOralSaturation() refuses a block that could go negative", {
  expect_null(validateOralSaturation(NULL, "x"))
  ok <- list(Imax = 0.906, ID50 = 571)
  expect_identical(validateOralSaturation(ok, "x"), ok)
  expect_error(validateOralSaturation(list(Imax = 1.2, ID50 = 571), "x"), "Imax")
  expect_error(validateOralSaturation(list(Imax = 0.5, ID50 = 0), "x"), "ID50")
  expect_error(validateOralSaturation(list(Imax = 0.5), "x"), "ID50")
  expect_error(validateOralSaturation(c(Imax = 0.5, ID50 = 10), "x"), "x")
})

test_that("only a drug that declares saturable absorption has it", {
  expect_equal(getDrugPK("gabapentin", 70, 170, 50, "male")$oralSaturation,
               list(Imax = 0.906, ID50 = 571))
  for (drug in c("oxycodone", "hydromorphone", "cefalexin"))
    expect_null(getDrugPK(drug, 70, 170, 50, "male")$oralSaturation, info = drug)
})

test_that("formulation units read as oral, with their formulation", {
  expect_true(all(doseRoute(poFormulationUnits) == ROUTE_PO))
  expect_equal(doseFormulation(c("mg PO tablet", "mg/kg PO liquid", "mg PO liquid bid",
                                 "mg PO", "mg", "mg IM")),
               c("tablet", "liquid", "liquid", NA, NA, NA))
  expect_false(any(isRateUnit(poFormulationUnits)))
  expect_true(all(poFormulationUnits %in% allUnits))
  expect_true(all(paste(poFormulationUnits, "bid") %in% scheduledUnits))
  expect_equal(scheduleBaseUnit("mg PO liquid qid"), "mg PO liquid")
})

test_that("a further oral formulation must be valid and share the default lag", {
  set <- getDrugPK("morphine", 70, 170, 40, "male")$PK$default
  # The helper reproduces getDrugPK()'s own oral coefficients.
  same <- oralCoefficients(set, set$ka_PO, set$bioavailability_PO)
  expect_equal(same, set[names(same)])
  expect_error(oralFormulationSet(set, list(ka_PO = 0.02, tlag_PO = 0), "x", "liquid"),
               "share the default oral lag")
  expect_error(oralFormulationSet(set, list(ka_PO = 0, tlag_PO = set$tlag_PO), "x", "liquid"),
               "ka_PO > 0")
  expect_error(oralFormulationSet(set, list(ka_PO = 0.02, tlag_PO = set$tlag_PO), "x", "syrup"),
               "not one of")
})
