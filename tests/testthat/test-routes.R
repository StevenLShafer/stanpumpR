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

  dd <- getDrugDefaultsGlobal()
  units <- unique(unlist(dd$Units))
  # The substring tests simCpCe() used before doseRoute()
  old <- ifelse(grepl("PO", units), "PO",
                ifelse(grepl("IM", units), "IM",
                       ifelse(grepl("IN", units), "IN", "IV")))
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
  expect_false(any(isRateUnit(c(bolusUnits, poUnits, imUnits, inUnits, tciUnits,
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
  expect_equal(unscheduled[doseRoute(unscheduled) != ROUTE_IV], c("mg PO", "mg IM", "mg IN"))
  expect_equal(unique(doseRoute(hydromorphone)), DOSE_ROUTES)
  expect_true(all(c("Plasma target", "Effect site target") %in%
                    hydromorphone[doseRoute(hydromorphone) == ROUTE_IV]))
})
